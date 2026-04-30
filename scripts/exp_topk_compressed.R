# PDD with distribution compression (R2.3 — computational cost reduction)
# Two compression levers:
#   (i)  N_sample: subsample to N rows before PDP (DALEX `N` parameter)
#   (ii) G_grid:   quantile-based grid with G points (default full grid)

# Helper: top-k important variables
get_top_k_vars_c <- function(model, model_choice, k) {
  if (model_choice == "logistic") {
    varimp <- varImp(model)
    vars <- rownames(varimp)[order(varimp$Overall, decreasing = TRUE)]
  } else if (model_choice == "rf") {
    vars <- rownames(model$importance)[order(model$importance[, 1], decreasing = TRUE)]
  } else if (model_choice == "tree") {
    vars <- names(sort(model$variable.importance, decreasing = TRUE))
  }
  head(vars, min(k, length(vars)))
}

# Helper: quantile grid for variable_splits
build_quantile_grid <- function(x, G) {
  if (G <= 1 || !is.numeric(x)) return(NULL)
  probs <- seq(0, 1, length.out = G)
  qs <- unique(as.numeric(quantile(x, probs = probs, na.rm = TRUE)))
  if (length(qs) < 2) return(NULL)
  qs
}

# Compressed profile computation: N_sample + optional quantile grid
compute_profiles_compressed <- function(explainer, vars, type = "partial",
                                        N_sample = 100, G_grid = NULL,
                                        data_for_grid = NULL) {
  profiles <- list()
  for (v in vars) {
    splits <- NULL
    if (!is.null(G_grid) && !is.null(data_for_grid)) {
      g <- build_quantile_grid(data_for_grid[[v]], G_grid)
      if (!is.null(g)) splits <- list()
      if (!is.null(g)) splits[[v]] <- g
    }
    mp <- tryCatch({
      if (!is.null(splits)) {
        model_profile(explainer, v, type, N = N_sample, variable_splits = splits)
      } else {
        model_profile(explainer, v, type, N = N_sample)
      }
    }, error = function(e) NULL)
    if (is.null(mp)) {
      profiles[[v]] <- data.frame(x = numeric(0), y = numeric(0))
    } else {
      profiles[[v]] <- data.frame(
        x = mp$agr_profiles$`_x_`,
        y = mp$agr_profiles$`_yhat_`
      )
    }
  }
  profiles
}

compute_cutoffs_c <- function(profiles_a, profiles_b) {
  cutoffs <- list()
  common_vars <- intersect(names(profiles_a), names(profiles_b))
  for (v in common_vars) {
    pa <- profiles_a[[v]]; pb <- profiles_b[[v]]
    if (length(pa$x) < 5 || length(pb$x) < 5) {
      cutoffs[[v]] <- list(PDI = 0.1, L2 = 0.1, L2Der = 0.1); next
    }
    cutoffs[[v]] <- tryCatch(metric_list_cal(pa, pb),
                             error = function(e) list(PDI = 0.1, L2 = 0.1, L2Der = 0.1))
  }
  cutoffs
}

run_experiment_topk_compressed <- function(model_choice = c("logistic", "rf", "tree"),
                                            data,
                                            target_col = "class",
                                            jj,
                                            type = "partial",
                                            pdi_coef = 1, l2_coef = 1, l2der_coef = 1,
                                            top_k_values = c(1, 2, 3),
                                            N_sample = 100,
                                            G_grid = NULL) {
  model_choice <- match.arg(model_choice)
  batch_sizes <- split_data_simple(data, jj)
  number_of_batch <- ceiling((batch_sizes$total - batch_sizes$train_set - batch_sizes$test_set) /
                               batch_sizes$test_set)

  data_sudden <- data
  set.seed(123)
  first_interval <- 1:(batch_sizes$train_set)
  test_interval  <- (max(first_interval)):(max(first_interval) + batch_sizes$test_set)

  if (model_choice == "logistic") {
    form <- as.formula(paste("as.factor(", target_col, ") ~ ."))
    rf <- glm(form, data = data_sudden[first_interval, ], family = "binomial")
    test_preds <- ifelse(predict(rf, data_sudden[test_interval, ], type = "response") > 0.5, 1, 0)
  } else if (model_choice == "rf") {
    form <- as.formula(paste(target_col, "~ ."))
    rf <- randomForest(form, data = data_sudden[first_interval, ], ntree = 50)
    test_preds <- predict(rf, data_sudden[test_interval, ], type = "class")
  } else {
    form <- as.formula(paste(target_col, "~ ."))
    rf <- rpart(form, data = data_sudden[first_interval, ])
    test_preds <- predict(rf, data_sudden[test_interval, ], type = "class")
  }
  test_accuracy <- mean(test_preds == data_sudden[test_interval, ][[target_col]])
  rf1 <- rf
  initial_top1 <- get_top_k_vars_c(rf, model_choice, 1)
  inc_size <- batch_sizes$test_set

  all_results <- data.frame()
  runtime_log <- data.frame()

  for (k in top_k_values) {
    t_start <- Sys.time()
    message(sprintf("  PDD_%d | N=%s | G=%s", k,
                    as.character(N_sample),
                    ifelse(is.null(G_grid), "full", as.character(G_grid))))
    rf_k <- rf
    data_new_k <- data_sudden[1:max(test_interval), ]
    top_vars <- get_top_k_vars_c(rf_k, model_choice, k)

    expl_tr <- DALEX::explain(model = rf_k,
                              data = data_sudden[first_interval, ],
                              y = data_sudden[first_interval, ][[target_col]],
                              verbose = FALSE, label = "tr")
    expl_te <- DALEX::explain(model = rf_k,
                              data = data_sudden[test_interval, ],
                              y = data_sudden[test_interval, ][[target_col]],
                              verbose = FALSE, label = "te")
    prof_tr <- compute_profiles_compressed(expl_tr, top_vars, type, N_sample, G_grid,
                                            data_sudden[first_interval, ])
    prof_te <- compute_profiles_compressed(expl_te, top_vars, type, N_sample, G_grid,
                                            data_sudden[test_interval, ])
    cutoffs <- compute_cutoffs_c(prof_tr, prof_te)
    prof_base <- prof_te

    acc_list <- list(); acc_idx <- 1; drift_number <- 0

    for (interval_i in seq(max(test_interval), nrow(data_sudden), by = inc_size)) {
      set.seed(123)
      new_interval <- interval_i:min(interval_i + inc_size, nrow(data_sudden))
      if (length(new_interval) <= 10) next

      if (model_choice == "logistic") {
        preds_temp <- ifelse(predict(rf_k, data_sudden[new_interval, ], type = "response") > 0.5, 1, 0)
      } else {
        preds_temp <- predict(rf_k, data_sudden[new_interval, ], type = "class")
      }
      acc_temp <- mean(preds_temp == data_sudden[new_interval, ][[target_col]], na.rm = TRUE)
      acc_list[[acc_idx]] <- acc_temp

      expl_t <- DALEX::explain(model = rf_k, data = data_sudden[new_interval, ],
                               y = data_sudden[new_interval, ][[target_col]],
                               verbose = FALSE, label = "t")
      prof_t <- compute_profiles_compressed(expl_t, top_vars, type, N_sample, G_grid,
                                             data_sudden[new_interval, ])

      triggered_by <- c()
      for (v in top_vars) {
        if (is.null(prof_base[[v]]) || is.null(prof_t[[v]]) || is.null(cutoffs[[v]])) next
        p_b <- prof_base[[v]]; p_n <- prof_t[[v]]
        if (length(p_b$x) < 5 || length(p_n$x) < 5) next
        mv <- tryCatch(metric_list_cal(p_b, p_n), error = function(e) NULL)
        if (is.null(mv)) next
        cv <- cutoffs[[v]]
        if (mv$PDI > cv$PDI * pdi_coef &&
            mv$L2 > cv$L2 * l2_coef &&
            mv$L2Der > cv$L2Der * l2der_coef) {
          triggered_by <- c(triggered_by, v)
        }
      }
      if (length(triggered_by) > 0 && length(new_interval) > 5) {
        drift_number <- drift_number + 1
        data_new_k <- rbind(data_sudden[new_interval, ], data_new_k)
        data_new_k <- na.omit(data_new_k)
        set.seed(123)
        if (model_choice == "logistic") {
          rf_k <- glm(as.formula(paste("as.factor(", target_col, ") ~ .")),
                      data = data_new_k, family = "binomial")
        } else if (model_choice == "rf") {
          rf_k <- randomForest(as.formula(paste(target_col, "~ .")),
                               data = data_new_k, ntree = 50)
        } else {
          rf_k <- rpart(as.formula(paste(target_col, "~ .")), data = data_new_k)
        }
        top_vars <- get_top_k_vars_c(rf_k, model_choice, k)
        expl_n <- DALEX::explain(model = rf_k, data = data_new_k,
                                 y = data_new_k[[target_col]],
                                 verbose = FALSE, label = "new")
        prof_base <- compute_profiles_compressed(expl_n, top_vars, type, N_sample, G_grid,
                                                  data_new_k)
        n_new <- nrow(data_new_k); split_idx <- floor(n_new * 0.8)
        expl_pt <- DALEX::explain(model = rf_k, data = data_new_k[1:split_idx, ],
                                  y = data_new_k[1:split_idx, ][[target_col]],
                                  verbose = FALSE, label = "pt")
        expl_ptest <- DALEX::explain(model = rf_k, data = data_new_k[(split_idx+1):n_new, ],
                                     y = data_new_k[(split_idx+1):n_new, ][[target_col]],
                                     verbose = FALSE, label = "pte")
        prof_pt <- compute_profiles_compressed(expl_pt, top_vars, type, N_sample, G_grid,
                                                data_new_k[1:split_idx, ])
        prof_ptest <- compute_profiles_compressed(expl_ptest, top_vars, type, N_sample, G_grid,
                                                   data_new_k[(split_idx+1):n_new, ])
        cutoffs <- compute_cutoffs_c(prof_pt, prof_ptest)
      }
      acc_idx <- acc_idx + 1
    }
    t_end <- Sys.time()
    elapsed <- as.numeric(difftime(t_end, t_start, units = "secs"))

    all_results <- rbind(all_results, data.frame(
      Model = paste0("PDD_", k),
      Mean_Accuracy = format(mean(unlist(acc_list), na.rm = TRUE), scientific = FALSE),
      Drift_Count = drift_number,
      Batch = number_of_batch,
      N_sample = N_sample,
      G_grid = ifelse(is.null(G_grid), NA, G_grid),
      Runtime_sec = round(elapsed, 2),
      model = model_choice,
      stringsAsFactors = FALSE
    ))
    message(sprintf("    -> PDD_%d done in %.1fs | drifts=%d | acc=%.4f",
                    k, elapsed, drift_number,
                    mean(unlist(acc_list), na.rm = TRUE)))
  }
  all_results
}

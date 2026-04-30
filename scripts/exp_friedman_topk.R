# Top-k variant of the Friedman (regression) drift experiment.
# Mirrors scripts/exp_topk.R structure but with RMSE / regression models.
# - PDD_C (classic): top-1 variable, single L2/L2Der cutoff (replicates exp_friedman.R logic)
# - PDD_k: monitors top-k variables; drift triggers if ANY exceeds its (var-specific) cutoffs
# - Baselines KSWIN and PageHinkley operate on |y - yhat| stream (same as exp_friedman.R)

# Helper: top-k variables (regression: linear / rf / tree)
get_top_k_vars_reg <- function(model, model_choice, k) {
  if (model_choice == "linear") {
    varimp <- caret::varImp(model)
    vars <- rownames(varimp)[order(varimp$Overall, decreasing = TRUE)]
  } else if (model_choice == "rf") {
    vars <- rownames(model$importance)[order(model$importance[, 1], decreasing = TRUE)]
  } else if (model_choice == "tree") {
    vars <- names(sort(model$variable.importance, decreasing = TRUE))
  }
  head(vars, min(k, length(vars)))
}

compute_profiles_reg <- function(explainer, vars, type = "partial") {
  profiles <- list()
  for (v in vars) {
    profiles[[v]] <- tryCatch({
      mp <- model_profile(explainer, v, type)
      data.frame(x = mp$agr_profiles$`_x_`, y = mp$agr_profiles$`_yhat_`)
    }, error = function(e) data.frame(x = numeric(0), y = numeric(0)))
  }
  profiles
}

compute_cutoffs_reg <- function(profiles_a, profiles_b) {
  cutoffs <- list()
  common_vars <- intersect(names(profiles_a), names(profiles_b))
  for (v in common_vars) {
    pa <- profiles_a[[v]]
    pb <- profiles_b[[v]]
    if (length(pa$x) < 5 || length(pb$x) < 5) {
      cutoffs[[v]] <- list(PDI = 0.1, L2 = 0.1, L2Der = 0.1)
      next
    }
    cutoffs[[v]] <- tryCatch(metric_list_cal(pa, pb),
                             error = function(e) list(PDI = 0.1, L2 = 0.1, L2Der = 0.1))
  }
  cutoffs
}

run_experiment_friedman_topk <- function(model_choice = c("linear", "rf", "tree"),
                                         data,
                                         target_col = "target",
                                         jj,
                                         experiment_result_path,
                                         type = "partial",
                                         pdi_coef = 0,
                                         l2_coef = 1,
                                         l2der_coef = 1,
                                         top_k_values = c(1, 2, 3)) {
  model_choice <- match.arg(model_choice)

  if (!dir.exists(experiment_result_path)) {
    dir.create(experiment_result_path, recursive = TRUE)
  }

  batch_sizes <- split_data_simple(data, jj)
  number_of_batch <- ceiling((batch_sizes$total - batch_sizes$train_set - batch_sizes$test_set) /
                               batch_sizes$test_set)

  data_sudden <- data
  set.seed(123)
  first_interval <- 1:(batch_sizes$train_set)
  test_interval <- (max(first_interval)):(max(first_interval) + batch_sizes$test_set)

  form <- as.formula(paste(target_col, "~ ."))

  if (model_choice == "linear") {
    rf <- lm(form, data = data_sudden[first_interval, ])
  } else if (model_choice == "rf") {
    rf <- randomForest(form, data = data_sudden[first_interval, ], ntree = 50)
  } else if (model_choice == "tree") {
    rf <- rpart(form, data = data_sudden[first_interval, ])
  }

  rf1 <- rf
  test_preds <- predict(rf, data_sudden[test_interval, ])
  train_preds <- predict(rf, data_sudden[first_interval, ])
  test_accuracy <- RMSE(test_preds, data_sudden[test_interval, ][[target_col]])
  train_accuracy <- RMSE(train_preds, data_sudden[first_interval, ][[target_col]])

  initial_most_important <- get_top_k_vars_reg(rf, model_choice, 1)
  inc_size <- batch_sizes$test_set

  # =========================================================================
  # PDD_C (classic, single-variable) — replicates exp_friedman.R drift logic
  # =========================================================================
  message(paste("=== PDD_C (Friedman) | model =", model_choice,
                "| batch =", number_of_batch,
                "| coefs L2/L2Der =", l2_coef, "/", l2der_coef, "==="))
  {
    rf_c <- rf
    data_new_c <- data_sudden[1:max(test_interval), ]
    most_important_c <- initial_most_important

    explainer_train_c <- DALEX::explain(model = rf_c,
                                        data = data_sudden[first_interval, ],
                                        y = data_sudden[first_interval, ][[target_col]],
                                        verbose = FALSE, label = "Base Train")
    mp_train_c <- model_profile(explainer_train_c, most_important_c, type)
    profile_train_c <- data.frame(x = mp_train_c$agr_profiles$`_x_`,
                                  y = mp_train_c$agr_profiles$`_yhat_`)

    explainer_base_c <- DALEX::explain(model = rf_c,
                                       data = data_sudden[test_interval, ],
                                       y = data_sudden[test_interval, ][[target_col]],
                                       verbose = FALSE, label = "Base Test")
    mp_base_c <- model_profile(explainer_base_c, most_important_c, type)
    profile_base_c <- data.frame(x = mp_base_c$agr_profiles$`_x_`,
                                 y = mp_base_c$agr_profiles$`_yhat_`)

    metrics_c <- metric_list_cal(profile_train_c, profile_base_c)
    pdi_cutoff_c <- metrics_c$PDI
    l2_cutoff_c <- metrics_c$L2
    l2_der_cutoff_c <- metrics_c$L2Der

    acc_list_c <- list()
    acc_idx_c <- 1
    drift_c <- 0

    for (interval_i in seq(max(test_interval), nrow(data_sudden), by = inc_size)) {
      set.seed(123)
      new_interval <- interval_i:min(interval_i + inc_size, nrow(data_sudden))
      if (length(new_interval) <= 5) next

      preds_temp <- predict(rf_c, na.omit(data_sudden[new_interval, ]))
      acc_temp <- RMSE(preds_temp, na.omit(data_sudden[new_interval, ])[[target_col]])
      acc_list_c[[acc_idx_c]] <- acc_temp

      explainer_t_c <- DALEX::explain(model = rf_c,
                                      data = data_sudden[new_interval, ],
                                      y = data_sudden[new_interval, ][[target_col]],
                                      verbose = FALSE, label = "temp")
      mp_t_c <- tryCatch(model_profile(explainer_t_c, most_important_c, type = type),
                         error = function(e) NULL)
      if (is.null(mp_t_c)) {
        acc_idx_c <- acc_idx_c + 1
        next
      }
      profile_t_c <- data.frame(x = mp_t_c$agr_profiles$`_x_`,
                                y = mp_t_c$agr_profiles$`_yhat_`)

      if (length(profile_t_c$x) < 5 || length(profile_base_c$x) < 5) {
        l2_t <- 999; l2_der_t <- 999
      } else {
        mt <- metric_list_cal(profile_base_c, profile_t_c)
        l2_t <- mt$L2; l2_der_t <- mt$L2Der
      }

      # Friedman uses L2 + L2Der gate (PDI dropped — see exp_friedman.R:125 / article L:426)
      if (l2_t > l2_cutoff_c * l2_coef &&
          l2_der_t > l2_der_cutoff_c * l2der_coef) {
        drift_c <- drift_c + 1
        message(paste("  PDD_C Drift #", drift_c, "at batch", acc_idx_c))

        data_new_c <- rbind(data_sudden[new_interval, ], data_new_c)
        data_new_c <- na.omit(data_new_c)
        set.seed(123)
        if (model_choice == "linear") {
          rf_c <- lm(form, data = data_new_c)
        } else if (model_choice == "rf") {
          rf_c <- randomForest(form, data = data_new_c, ntree = 50)
        } else if (model_choice == "tree") {
          rf_c <- rpart(form, data = data_new_c)
        }
        explainer_base_c <- DALEX::explain(model = rf_c,
                                           data = data_new_c,
                                           y = data_new_c[[target_col]],
                                           verbose = FALSE,
                                           label = paste("New Trained", nrow(data_new_c)))
        mp_base_c <- model_profile(explainer_base_c, most_important_c, type)
        profile_base_c <- data.frame(x = mp_base_c$agr_profiles$`_x_`,
                                     y = mp_base_c$agr_profiles$`_yhat_`)
      }
      acc_idx_c <- acc_idx_c + 1
    }

    pdd_c_result <- data.frame(
      Model = "PDD_C",
      Mean_Accuracy = format(mean(unlist(acc_list_c), na.rm = TRUE), scientific = FALSE),
      Drift_Count = drift_c,
      Batch = number_of_batch,
      most_important = initial_most_important,
      model = model_choice,
      stringsAsFactors = FALSE
    )
  }

  # =========================================================================
  # PDD_k — top-k variables, per-variable cutoffs
  # =========================================================================
  all_pdd_results <- data.frame()
  all_drift_logs <- data.frame()

  for (k in top_k_values) {
    message(paste("=== PDD top_k =", k, "| model =", model_choice,
                  "| batch =", number_of_batch,
                  "| coefs L2/L2Der =", l2_coef, "/", l2der_coef, "==="))

    rf_k <- rf
    data_new_k <- data_sudden[1:max(test_interval), ]
    top_vars <- get_top_k_vars_reg(rf_k, model_choice, k)
    initial_top_vars <- paste(top_vars, collapse = ";")

    explainer_train <- DALEX::explain(model = rf_k,
                                      data = data_sudden[first_interval, ],
                                      y = data_sudden[first_interval, ][[target_col]],
                                      verbose = FALSE, label = "Base Train")
    explainer_test  <- DALEX::explain(model = rf_k,
                                      data = data_sudden[test_interval, ],
                                      y = data_sudden[test_interval, ][[target_col]],
                                      verbose = FALSE, label = "Base Test")
    profiles_train <- compute_profiles_reg(explainer_train, top_vars, type)
    profiles_test  <- compute_profiles_reg(explainer_test, top_vars, type)
    cutoffs <- compute_cutoffs_reg(profiles_train, profiles_test)
    profiles_base <- profiles_test

    drift_log_k <- data.frame(
      model = model_choice, Batch = number_of_batch, top_k = k,
      l2_coef = l2_coef, l2der_coef = l2der_coef,
      update_no = 0, batch_index = 0,
      top_vars = initial_top_vars,
      drift_triggered_by = NA, accuracy = test_accuracy,
      stringsAsFactors = FALSE
    )

    acc_list <- list()
    acc_idx <- 1
    drift_number <- 0

    for (interval_i in seq(max(test_interval), nrow(data_sudden), by = inc_size)) {
      set.seed(123)
      new_interval <- interval_i:min(interval_i + inc_size, nrow(data_sudden))
      if (length(new_interval) <= 5) next

      preds_temp <- predict(rf_k, na.omit(data_sudden[new_interval, ]))
      acc_temp <- RMSE(preds_temp, na.omit(data_sudden[new_interval, ])[[target_col]])
      acc_list[[acc_idx]] <- acc_temp

      explainer_temp <- DALEX::explain(model = rf_k,
                                       data = data_sudden[new_interval, ],
                                       y = data_sudden[new_interval, ][[target_col]],
                                       verbose = FALSE, label = "temp")
      profiles_temp <- compute_profiles_reg(explainer_temp, top_vars, type)

      triggered_by <- c()
      for (v in top_vars) {
        if (!(v %in% names(profiles_base)) || !(v %in% names(profiles_temp))) next
        if (!(v %in% names(cutoffs))) next
        p_base <- profiles_base[[v]]
        p_new  <- profiles_temp[[v]]
        if (length(p_base$x) < 5 || length(p_new$x) < 5) next
        mv <- tryCatch(metric_list_cal(p_base, p_new), error = function(e) NULL)
        if (is.null(mv)) next
        if (mv$L2    > cutoffs[[v]]$L2    * l2_coef &&
            mv$L2Der > cutoffs[[v]]$L2Der * l2der_coef) {
          triggered_by <- c(triggered_by, v)
        }
      }

      if (length(triggered_by) > 0) {
        drift_number <- drift_number + 1
        message(paste("  Drift #", drift_number, "at batch", acc_idx,
                      "triggered by:", paste(triggered_by, collapse = ", ")))

        data_new_k <- rbind(data_sudden[new_interval, ], data_new_k)
        data_new_k <- na.omit(data_new_k)
        set.seed(123)
        if (model_choice == "linear") {
          rf_k <- lm(form, data = data_new_k)
        } else if (model_choice == "rf") {
          rf_k <- randomForest(form, data = data_new_k, ntree = 50)
        } else if (model_choice == "tree") {
          rf_k <- rpart(form, data = data_new_k)
        }

        top_vars <- get_top_k_vars_reg(rf_k, model_choice, k)

        explainer_new <- DALEX::explain(model = rf_k,
                                        data = data_new_k,
                                        y = data_new_k[[target_col]],
                                        verbose = FALSE,
                                        label = paste("Retrained", nrow(data_new_k)))
        profiles_base <- compute_profiles_reg(explainer_new, top_vars, type)

        n_new <- nrow(data_new_k)
        split_idx <- floor(n_new * 0.8)
        explainer_pt <- DALEX::explain(model = rf_k,
                                       data = data_new_k[1:split_idx, ],
                                       y = data_new_k[1:split_idx, ][[target_col]],
                                       verbose = FALSE, label = "Pseudo Train")
        explainer_ptest <- DALEX::explain(model = rf_k,
                                          data = data_new_k[(split_idx + 1):n_new, ],
                                          y = data_new_k[(split_idx + 1):n_new, ][[target_col]],
                                          verbose = FALSE, label = "Pseudo Test")
        profiles_pt    <- compute_profiles_reg(explainer_pt, top_vars, type)
        profiles_ptest <- compute_profiles_reg(explainer_ptest, top_vars, type)
        cutoffs <- compute_cutoffs_reg(profiles_pt, profiles_ptest)

        drift_log_k <- rbind(drift_log_k, data.frame(
          model = model_choice, Batch = number_of_batch, top_k = k,
          l2_coef = l2_coef, l2der_coef = l2der_coef,
          update_no = drift_number, batch_index = acc_idx,
          top_vars = paste(top_vars, collapse = ";"),
          drift_triggered_by = paste(triggered_by, collapse = ";"),
          accuracy = acc_temp, stringsAsFactors = FALSE
        ))
      }
      acc_idx <- acc_idx + 1
    }

    mean_acc <- mean(unlist(acc_list), na.rm = TRUE)
    all_pdd_results <- rbind(all_pdd_results, data.frame(
      Model = paste0("PDD_", k),
      Mean_Accuracy = format(mean_acc, scientific = FALSE),
      Drift_Count = drift_number,
      Batch = number_of_batch,
      most_important = initial_top_vars,
      model = model_choice,
      stringsAsFactors = FALSE
    ))
    all_drift_logs <- rbind(all_drift_logs, drift_log_k)
  }

  # =========================================================================
  # Baselines: KSWIN and PageHinkley on |y - yhat| stream (same as exp_friedman.R)
  # =========================================================================
  run_drift_detection_reg <- function(ddm_model, rf_initial, data_sudden, test_interval, inc_size) {
    set.seed(123)
    rf_current <- rf_initial
    ddm <- switch(ddm_model,
                  "KSWIN" = KSWIN$new(),
                  "PageHinkley" = PageHinkley$new(),
                  stop("Unknown model type"))
    acc_list <- list()
    acc_idx <- 1
    data_new_ddm <- data_sudden[1:max(test_interval), ]
    drift_count <- 0
    for (interval_i in seq(max(test_interval), nrow(data_sudden), by = inc_size)) {
      new_interval <- interval_i:min(interval_i + inc_size, nrow(data_sudden))
      set.seed(1234)
      if (length(new_interval) <= 10) next

      preds_temp <- predict(rf_current, data_sudden[new_interval, ])
      acc_temp <- RMSE(preds_temp, data_sudden[new_interval, ][[target_col]])
      acc_list[[acc_idx]] <- acc_temp
      acc_idx <- acc_idx + 1
      data_stream <- na.omit(abs(data_sudden[new_interval, ][[target_col]] - preds_temp))
      tryCatch({
        for (i in seq_along(data_stream)) {
          try(ddm$add_element(data_stream[i]), silent = TRUE)
          if (ddm$change_detected) {
            drift_count <- drift_count + 1
            data_new_ddm <- rbind(data_sudden[new_interval, ], data_new_ddm)
            data_new_ddm <- na.omit(data_new_ddm)
            set.seed(123)
            if (model_choice == "linear") {
              rf_current <- lm(form, data = data_new_ddm)
            } else if (model_choice == "rf") {
              rf_current <- randomForest(form, data = data_new_ddm, ntree = 50)
            } else if (model_choice == "tree") {
              rf_current <- rpart(form, data = data_new_ddm)
            }
            ddm$reset()
            break
          }
        }
      }, error = function(e) message("ddm error: ", e$message))
    }
    list(mean_accuracy = mean(unlist(acc_list), na.rm = TRUE),
         drift_count = drift_count)
  }

  result_kswin <- run_drift_detection_reg("KSWIN", rf1, data_sudden, test_interval, inc_size)
  result_ph    <- run_drift_detection_reg("PageHinkley", rf1, data_sudden, test_interval, inc_size)

  results_list <- list(KSWIN = result_kswin, PageHinkley = result_ph)
  baseline_df <- do.call(rbind, lapply(names(results_list), function(nm) {
    data.frame(
      Model = nm,
      Mean_Accuracy = format(results_list[[nm]]$mean_accuracy, scientific = FALSE),
      Drift_Count = results_list[[nm]]$drift_count,
      Batch = number_of_batch,
      most_important = initial_most_important,
      model = model_choice,
      stringsAsFactors = FALSE)
  }))

  extra_rows <- rbind(
    data.frame(Model = "Base Test", Mean_Accuracy = format(test_accuracy, scientific = FALSE),
               Drift_Count = "0", Batch = number_of_batch,
               most_important = initial_most_important, model = model_choice,
               stringsAsFactors = FALSE),
    data.frame(Model = "Base Train", Mean_Accuracy = format(train_accuracy, scientific = FALSE),
               Drift_Count = "0", Batch = number_of_batch,
               most_important = initial_most_important, model = model_choice,
               stringsAsFactors = FALSE),
    data.frame(Model = "batch sizes",
               Mean_Accuracy = format(as.integer(batch_sizes$train_set), scientific = FALSE),
               Drift_Count = batch_sizes$test_set, Batch = number_of_batch,
               most_important = initial_most_important, model = model_choice,
               stringsAsFactors = FALSE)
  )

  df_final <- rbind(baseline_df, pdd_c_result, all_pdd_results, extra_rows)
  list(results = df_final, drift_log = all_drift_logs)
}

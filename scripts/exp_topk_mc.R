# Multi-class extension of PDD top-k
# Key ideas:
#   - PDP is computed per (variable, class) pair via DALEX's predict_function hook
#   - Cutoffs are per (variable, class); drift triggers if ANY pair exceeds all 3 metrics
#   - Baseline detectors (DDM, HDDM-A/W, KSWIN, PH, EDDM) operate on error stream — no change

# Helper: top-k variables (reusable from binary version)
get_top_k_vars_mc <- function(model, model_choice, k) {
  if (model_choice == "rf") {
    vars <- rownames(model$importance)[order(model$importance[,1], decreasing = TRUE)]
  } else if (model_choice == "tree") {
    vars <- names(sort(model$variable.importance, decreasing = TRUE))
  }
  head(vars, min(k, length(vars)))
}

# Helper: top-k classes by frequency in training data
get_top_k_classes <- function(y, k) {
  tab <- sort(table(y), decreasing = TRUE)
  names(head(tab, min(k, length(tab))))
}

# Helper: build per-class predict_function wrapper
make_predict_class <- function(cls) {
  force(cls)
  function(model, newdata) {
    probs <- predict(model, newdata, type = "prob")
    if (is.null(colnames(probs))) return(rep(0, nrow(newdata)))
    if (!(cls %in% colnames(probs))) return(rep(0, nrow(newdata)))
    probs[, cls]
  }
}

# Compute per-(var, class) PDP profiles
compute_profiles_mc <- function(model, data, y, vars, classes, type = "partial") {
  profiles <- list()
  for (cls in classes) {
    profiles[[cls]] <- list()
    pred_fn <- make_predict_class(cls)
    explainer_c <- tryCatch(
      DALEX::explain(
        model = model,
        data = data,
        y = as.numeric(y == cls),
        predict_function = pred_fn,
        verbose = FALSE,
        label = paste0("class_", cls)
      ),
      error = function(e) NULL
    )
    if (is.null(explainer_c)) {
      for (v in vars) profiles[[cls]][[v]] <- data.frame(x = numeric(0), y = numeric(0))
      next
    }
    for (v in vars) {
      mp <- tryCatch(model_profile(explainer_c, v, type),
                     error = function(e) NULL)
      if (is.null(mp)) {
        profiles[[cls]][[v]] <- data.frame(x = numeric(0), y = numeric(0))
      } else {
        profiles[[cls]][[v]] <- data.frame(
          x = mp$agr_profiles$`_x_`,
          y = mp$agr_profiles$`_yhat_`
        )
      }
    }
  }
  profiles
}

compute_cutoffs_mc <- function(prof_a, prof_b) {
  cutoffs <- list()
  common_cls <- intersect(names(prof_a), names(prof_b))
  for (cls in common_cls) {
    cutoffs[[cls]] <- list()
    common_vars <- intersect(names(prof_a[[cls]]), names(prof_b[[cls]]))
    for (v in common_vars) {
      pa <- prof_a[[cls]][[v]]
      pb <- prof_b[[cls]][[v]]
      if (length(pa$x) < 5 || length(pb$x) < 5) {
        cutoffs[[cls]][[v]] <- list(PDI = 0.1, L2 = 0.1, L2Der = 0.1)
        next
      }
      cutoffs[[cls]][[v]] <- tryCatch(
        metric_list_cal(pa, pb),
        error = function(e) list(PDI = 0.1, L2 = 0.1, L2Der = 0.1)
      )
    }
  }
  cutoffs
}

run_experiment_topk_mc <- function(model_choice = c("rf", "tree"),
                                   data,
                                   target_col = "class",
                                   jj,
                                   experiment_result_path,
                                   type = "partial",
                                   pdi_coef = 1,
                                   l2_coef = 1,
                                   l2der_coef = 1,
                                   top_k_vars  = c(1, 2, 3),
                                   k_class     = 3) {
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
  if (model_choice == "rf") {
    rf <- randomForest(form, data = data_sudden[first_interval, ], ntree = 50)
  } else if (model_choice == "tree") {
    rf <- rpart(form, data = data_sudden[first_interval, ], method = "class")
  }
  test_preds  <- predict(rf, data_sudden[test_interval, ],  type = "class")
  train_preds <- predict(rf, data_sudden[first_interval, ], type = "class")
  test_accuracy  <- mean(test_preds  == data_sudden[test_interval, ][[target_col]])
  train_accuracy <- mean(train_preds == data_sudden[first_interval, ][[target_col]])
  rf1 <- rf

  # Top-k classes determined from training data (constant across top_k values)
  y_train <- data_sudden[first_interval, ][[target_col]]
  top_classes_init <- get_top_k_classes(y_train, k_class)

  # Top-1 variable for PDD_C (reference)
  initial_top1 <- get_top_k_vars_mc(rf, model_choice, 1)

  inc_size <- batch_sizes$test_set

  # ============ PDD_C (classic: top-1 var × top-1 class = majority) ============
  message(paste("=== PDD_C (multi-class) | model =", model_choice, "| batch =", number_of_batch, "==="))
  {
    rf_c <- rf
    data_new_c <- data_sudden[1:max(test_interval), ]
    var_c <- initial_top1
    cls_c <- top_classes_init[1]  # majority class

    explainer_train_c <- DALEX::explain(
      model = rf_c,
      data = data_sudden[first_interval, ],
      y = as.numeric(y_train == cls_c),
      predict_function = make_predict_class(cls_c),
      verbose = FALSE, label = "train"
    )
    mp_train_c <- model_profile(explainer_train_c, var_c, type)
    prof_train_c <- data.frame(x = mp_train_c$agr_profiles$`_x_`,
                               y = mp_train_c$agr_profiles$`_yhat_`)

    explainer_test_c <- DALEX::explain(
      model = rf_c,
      data = data_sudden[test_interval, ],
      y = as.numeric(data_sudden[test_interval, ][[target_col]] == cls_c),
      predict_function = make_predict_class(cls_c),
      verbose = FALSE, label = "test"
    )
    mp_test_c <- model_profile(explainer_test_c, var_c, type)
    prof_base_c <- data.frame(x = mp_test_c$agr_profiles$`_x_`,
                              y = mp_test_c$agr_profiles$`_yhat_`)

    metrics_c <- metric_list_cal(prof_train_c, prof_base_c)
    pdi_cutoff_c   <- metrics_c$PDI
    l2_cutoff_c    <- metrics_c$L2
    l2der_cutoff_c <- metrics_c$L2Der

    acc_list_c <- list()
    acc_idx_c <- 1
    drift_c <- 0

    for (interval_i in seq(max(test_interval), nrow(data_sudden), by = inc_size)) {
      set.seed(123)
      new_interval <- interval_i:min(interval_i + inc_size, nrow(data_sudden))
      if (length(new_interval) <= 10) next

      preds_temp <- predict(rf_c, data_sudden[new_interval, ], type = "class")
      acc_temp <- mean(preds_temp == data_sudden[new_interval, ][[target_col]], na.rm = TRUE)
      acc_list_c[[acc_idx_c]] <- acc_temp

      explainer_t_c <- DALEX::explain(
        model = rf_c,
        data = data_sudden[new_interval, ],
        y = as.numeric(data_sudden[new_interval, ][[target_col]] == cls_c),
        predict_function = make_predict_class(cls_c),
        verbose = FALSE, label = "temp"
      )
      mp_t_c <- tryCatch(model_profile(explainer_t_c, var_c, type),
                         error = function(e) NULL)
      if (is.null(mp_t_c)) {
        acc_idx_c <- acc_idx_c + 1
        next
      }
      prof_t_c <- data.frame(x = mp_t_c$agr_profiles$`_x_`,
                             y = mp_t_c$agr_profiles$`_yhat_`)

      if (length(prof_t_c$x) < 5 || length(prof_base_c$x) < 5) {
        pdi_t <- 1; l2_t <- 999; l2der_t <- 999
      } else {
        mt <- metric_list_cal(prof_base_c, prof_t_c)
        pdi_t <- mt$PDI; l2_t <- mt$L2; l2der_t <- mt$L2Der
      }

      if (pdi_t > pdi_cutoff_c * pdi_coef &&
          l2_t > l2_cutoff_c * l2_coef &&
          l2der_t > l2der_cutoff_c * l2der_coef) {
        drift_c <- drift_c + 1
        message(paste("  PDD_C Drift #", drift_c, "at batch", acc_idx_c))

        data_new_c <- rbind(data_sudden[new_interval, ], data_new_c)
        data_new_c <- na.omit(data_new_c)
        set.seed(123)
        if (model_choice == "rf") {
          rf_c <- randomForest(form, data = data_new_c, ntree = 50)
        } else {
          rf_c <- rpart(form, data = data_new_c, method = "class")
        }
        explainer_base_c <- DALEX::explain(
          model = rf_c,
          data = data_new_c,
          y = as.numeric(data_new_c[[target_col]] == cls_c),
          predict_function = make_predict_class(cls_c),
          verbose = FALSE, label = "new"
        )
        mp_b_c <- model_profile(explainer_base_c, var_c, type)
        prof_base_c <- data.frame(x = mp_b_c$agr_profiles$`_x_`,
                                  y = mp_b_c$agr_profiles$`_yhat_`)
      }
      acc_idx_c <- acc_idx_c + 1
    }

    pdd_c_result <- data.frame(
      Model = "PDD_C",
      Mean_Accuracy = format(mean(unlist(acc_list_c), na.rm = TRUE), scientific = FALSE),
      Drift_Count = drift_c,
      Batch = number_of_batch,
      most_important = initial_top1,
      model = model_choice,
      stringsAsFactors = FALSE
    )
  }

  # ============ PDD-mc_k (top-k variables × top-k_class classes) ============
  all_pdd_results <- data.frame()
  all_drift_logs <- data.frame()

  for (k in top_k_vars) {
    message(paste("=== PDD-mc top_k_vars =", k, "| k_class =", k_class,
                  "| model =", model_choice, "| batch =", number_of_batch, "==="))

    rf_k <- rf
    data_new_k <- data_sudden[1:max(test_interval), ]
    top_vars <- get_top_k_vars_mc(rf_k, model_choice, k)
    top_classes <- top_classes_init
    initial_top_vars <- paste(top_vars, collapse = ";")
    initial_top_classes <- paste(top_classes, collapse = ";")

    profiles_train <- compute_profiles_mc(rf_k, data_sudden[first_interval, ], y_train,
                                          top_vars, top_classes, type)
    profiles_test  <- compute_profiles_mc(rf_k, data_sudden[test_interval, ],
                                          data_sudden[test_interval, ][[target_col]],
                                          top_vars, top_classes, type)
    cutoffs <- compute_cutoffs_mc(profiles_train, profiles_test)
    profiles_base <- profiles_test

    drift_log_k <- data.frame(
      model = model_choice, Batch = number_of_batch,
      top_k_vars = k, k_class = k_class,
      update_no = 0, batch_index = 0,
      top_vars = initial_top_vars,
      top_classes = initial_top_classes,
      drift_triggered_by = NA,
      accuracy = test_accuracy,
      stringsAsFactors = FALSE
    )

    acc_list <- list()
    acc_idx <- 1
    drift_number <- 0

    for (interval_i in seq(max(test_interval), nrow(data_sudden), by = inc_size)) {
      set.seed(123)
      new_interval <- interval_i:min(interval_i + inc_size, nrow(data_sudden))
      if (length(new_interval) <= 10) next

      preds_temp <- predict(rf_k, data_sudden[new_interval, ], type = "class")
      acc_temp <- mean(preds_temp == data_sudden[new_interval, ][[target_col]], na.rm = TRUE)
      acc_list[[acc_idx]] <- acc_temp

      profiles_temp <- compute_profiles_mc(rf_k, data_sudden[new_interval, ],
                                           data_sudden[new_interval, ][[target_col]],
                                           top_vars, top_classes, type)

      triggered_by <- c()
      for (cls in top_classes) {
        for (v in top_vars) {
          if (is.null(profiles_base[[cls]][[v]]) ||
              is.null(profiles_temp[[cls]][[v]]) ||
              is.null(cutoffs[[cls]][[v]])) next
          p_base <- profiles_base[[cls]][[v]]
          p_new  <- profiles_temp[[cls]][[v]]
          if (length(p_base$x) < 5 || length(p_new$x) < 5) next
          mv <- tryCatch(metric_list_cal(p_base, p_new), error = function(e) NULL)
          if (is.null(mv)) next
          cv <- cutoffs[[cls]][[v]]
          if (mv$PDI   > cv$PDI   * pdi_coef   &&
              mv$L2    > cv$L2    * l2_coef    &&
              mv$L2Der > cv$L2Der * l2der_coef) {
            triggered_by <- c(triggered_by, paste0(v, "@", cls))
          }
        }
      }

      if (length(triggered_by) > 0 && length(new_interval) > 5) {
        drift_number <- drift_number + 1
        message(paste("  PDD-mc_", k, " Drift #", drift_number, "at batch", acc_idx,
                      "triggered by:", paste(triggered_by, collapse = ", ")))

        data_new_k <- rbind(data_sudden[new_interval, ], data_new_k)
        data_new_k <- na.omit(data_new_k)
        set.seed(123)
        if (model_choice == "rf") {
          rf_k <- randomForest(form, data = data_new_k, ntree = 50)
        } else {
          rf_k <- rpart(form, data = data_new_k, method = "class")
        }

        top_vars <- get_top_k_vars_mc(rf_k, model_choice, k)
        top_classes <- get_top_k_classes(data_new_k[[target_col]], k_class)

        profiles_base <- compute_profiles_mc(rf_k, data_new_k, data_new_k[[target_col]],
                                             top_vars, top_classes, type)
        n_new <- nrow(data_new_k)
        split_idx <- floor(n_new * 0.8)
        profiles_pt    <- compute_profiles_mc(rf_k,
                                              data_new_k[1:split_idx, ],
                                              data_new_k[1:split_idx, ][[target_col]],
                                              top_vars, top_classes, type)
        profiles_ptest <- compute_profiles_mc(rf_k,
                                              data_new_k[(split_idx+1):n_new, ],
                                              data_new_k[(split_idx+1):n_new, ][[target_col]],
                                              top_vars, top_classes, type)
        cutoffs <- compute_cutoffs_mc(profiles_pt, profiles_ptest)

        drift_log_k <- rbind(drift_log_k, data.frame(
          model = model_choice, Batch = number_of_batch,
          top_k_vars = k, k_class = k_class,
          update_no = drift_number, batch_index = acc_idx,
          top_vars = paste(top_vars, collapse = ";"),
          top_classes = paste(top_classes, collapse = ";"),
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

  # ============ Baseline detectors (error stream — works for multi-class) ============
  run_drift_detection_mc <- function(ddm_model, rf_initial, data_sudden, test_interval, inc_size) {
    set.seed(123)
    rf_current <- rf_initial
    ddm <- switch(ddm_model,
                  "HDDM_A" = HDDM_A$new(),
                  "HDDM_W" = HDDM_W$new(),
                  "KSWIN"  = KSWIN$new(),
                  "PageHinkley" = PageHinkley$new(),
                  "DDM"    = DDM$new(),
                  "EDDM"   = EDDM$new())
    acc_list <- list(); acc_idx <- 1
    data_new_ddm <- data_sudden[1:max(test_interval), ]
    drift_count <- 0
    for (interval_i in seq(max(test_interval), nrow(data_sudden), by = inc_size)) {
      new_interval <- interval_i:min(interval_i + inc_size, nrow(data_sudden))
      set.seed(1234)
      preds_temp <- predict(rf_current, data_sudden[new_interval, ], type = "class")
      acc_temp <- mean(preds_temp == data_sudden[new_interval, ][[target_col]], na.rm = TRUE)
      acc_list[[acc_idx]] <- acc_temp
      acc_idx <- acc_idx + 1
      data_stream <- na.omit(as.numeric(data_sudden[new_interval, ][[target_col]] != preds_temp))
      tryCatch({
        for (i in seq_along(data_stream)) {
          try(ddm$add_element(data_stream[i]), silent = TRUE)
          if (ddm$change_detected) {
            drift_count <- drift_count + 1
            data_new_ddm <- rbind(data_sudden[new_interval, ], data_new_ddm)
            data_new_ddm <- na.omit(data_new_ddm)
            set.seed(123)
            if (model_choice == "rf") {
              rf_current <- randomForest(form, data = data_new_ddm, ntree = 50)
            } else {
              rf_current <- rpart(form, data = data_new_ddm, method = "class")
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

  results_list <- list(
    HDDM_A      = run_drift_detection_mc("HDDM_A",      rf1, data_sudden, test_interval, inc_size),
    HDDM_W      = run_drift_detection_mc("HDDM_W",      rf1, data_sudden, test_interval, inc_size),
    KSWIN       = run_drift_detection_mc("KSWIN",       rf1, data_sudden, test_interval, inc_size),
    PageHinkley = run_drift_detection_mc("PageHinkley", rf1, data_sudden, test_interval, inc_size),
    DDM         = run_drift_detection_mc("DDM",         rf1, data_sudden, test_interval, inc_size),
    EDDM        = run_drift_detection_mc("EDDM",        rf1, data_sudden, test_interval, inc_size)
  )

  baseline_df <- do.call(rbind, lapply(names(results_list), function(nm) {
    data.frame(
      Model = nm,
      Mean_Accuracy = format(results_list[[nm]]$mean_accuracy, scientific = FALSE),
      Drift_Count = results_list[[nm]]$drift_count,
      Batch = number_of_batch,
      most_important = initial_top1,
      model = model_choice,
      stringsAsFactors = FALSE)
  }))

  extra_rows <- rbind(
    data.frame(Model = "Base Test", Mean_Accuracy = format(test_accuracy, scientific = FALSE),
               Drift_Count = "0", Batch = number_of_batch,
               most_important = initial_top1, model = model_choice,
               stringsAsFactors = FALSE),
    data.frame(Model = "Base Train", Mean_Accuracy = format(train_accuracy, scientific = FALSE),
               Drift_Count = "0", Batch = number_of_batch,
               most_important = initial_top1, model = model_choice,
               stringsAsFactors = FALSE),
    data.frame(Model = "batch sizes",
               Mean_Accuracy = format(as.integer(batch_sizes$train_set), scientific = FALSE),
               Drift_Count = batch_sizes$test_set, Batch = number_of_batch,
               most_important = initial_top1, model = model_choice,
               stringsAsFactors = FALSE)
  )

  df_final <- rbind(baseline_df, pdd_c_result, all_pdd_results, extra_rows)
  list(results = df_final, drift_log = all_drift_logs)
}

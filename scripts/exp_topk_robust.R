# Robust top-k variant: same drift logic as exp_topk.R, but model training
# is regularized to address overfit-driven cutoff inflation (Reviewer #1.5,
# NOAA/Ozone RF). Uses the helpers from exp_topk.R verbatim — original file
# is not modified.

source("scripts/exp_topk.R")

# Build a regularization preset. `strong` is what we use for NOAA/Ozone RF.
make_reg_args <- function(model_choice, regularize = c("none", "light", "strong"),
                          y_factor = NULL, classwt = NULL) {
  regularize <- match.arg(regularize)
  args <- list()
  if (model_choice == "rf") {
    args$ntree <- 50
    if (regularize == "light")  args$nodesize <- 5
    if (regularize == "strong") args$nodesize <- 20
    if (!is.null(classwt)) args$classwt <- classwt
  } else if (model_choice == "tree") {
    if (regularize == "light")  args$control <- rpart.control(minbucket = 10, cp = 0.005)
    if (regularize == "strong") args$control <- rpart.control(minbucket = 20, cp = 0.01,
                                                              maxdepth = 6)
  }
  args
}

train_robust <- function(model_choice, form, data, target_col, reg_args) {
  if (model_choice == "logistic") {
    glm(as.formula(paste("as.factor(", target_col, ") ~ .")),
        data = data, family = "binomial")
  } else if (model_choice == "rf") {
    do.call(randomForest, c(list(formula = form, data = data), reg_args))
  } else if (model_choice == "tree") {
    do.call(rpart, c(list(formula = form, data = data), reg_args))
  }
}

predict_class_robust <- function(model_choice, model, newdata) {
  if (model_choice == "logistic") {
    p <- predict(model, newdata, type = "response")
    ifelse(p > 0.50, 1, 0)
  } else {
    predict(model, newdata, type = "class")
  }
}

run_experiment_topk_robust <- function(model_choice = c("logistic", "rf", "tree"),
                                       data,
                                       target_col = "class",
                                       jj,
                                       experiment_result_path,
                                       type = "partial",
                                       pdi_coef = 1, l2_coef = 1, l2der_coef = 1,
                                       top_k_values = c(1, 2, 3),
                                       regularize = "strong",
                                       classwt = NULL) {
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
  test_interval  <- (max(first_interval)):(max(first_interval) + batch_sizes$test_set)

  form <- as.formula(paste(target_col, "~ ."))
  reg_args <- make_reg_args(model_choice, regularize, classwt = classwt)

  rf <- train_robust(model_choice, form, data_sudden[first_interval, ], target_col, reg_args)
  test_preds  <- predict_class_robust(model_choice, rf, data_sudden[test_interval, ])
  train_preds <- predict_class_robust(model_choice, rf, data_sudden[first_interval, ])
  test_accuracy  <- mean(test_preds  == data_sudden[test_interval, ][[target_col]])
  train_accuracy <- mean(train_preds == data_sudden[first_interval, ][[target_col]])
  rf1 <- rf
  initial_most_important <- get_top_k_vars(rf, model_choice, 1)
  inc_size <- batch_sizes$test_set

  message(sprintf("[robust=%s] %s | batches=%d | train_acc=%.4f | test_acc=%.4f | gap=%.4f",
                  regularize, model_choice, number_of_batch,
                  train_accuracy, test_accuracy, train_accuracy - test_accuracy))

  # ---------- PDD_C ----------
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
  pdi_cutoff_c <- metrics_c$PDI; l2_cutoff_c <- metrics_c$L2; l2_der_cutoff_c <- metrics_c$L2Der

  acc_list_c <- list(); acc_idx_c <- 1; drift_c <- 0
  for (interval_i in seq(max(test_interval), nrow(data_sudden), by = inc_size)) {
    set.seed(123)
    new_interval <- interval_i:min(interval_i + inc_size, nrow(data_sudden))
    if (length(new_interval) <= 10) next
    preds_temp <- predict_class_robust(model_choice, rf_c, data_sudden[new_interval, ])
    acc_temp <- mean(preds_temp == data_sudden[new_interval, ][[target_col]], na.rm = TRUE)
    acc_list_c[[acc_idx_c]] <- acc_temp

    explainer_t_c <- DALEX::explain(model = rf_c,
                                    data = data_sudden[new_interval, ],
                                    y = data_sudden[new_interval, ][[target_col]],
                                    verbose = FALSE, label = "temp")
    mp_t_c <- tryCatch(model_profile(explainer_t_c, most_important_c, type = type),
                       error = function(e) NULL)
    if (is.null(mp_t_c)) { acc_idx_c <- acc_idx_c + 1; next }
    profile_t_c <- data.frame(x = mp_t_c$agr_profiles$`_x_`,
                              y = mp_t_c$agr_profiles$`_yhat_`)
    if (length(profile_t_c$x) < 5 || length(profile_base_c$x) < 5) {
      pdi_t <- 1; l2_t <- 999; l2_der_t <- 999
    } else {
      mt <- metric_list_cal(profile_base_c, profile_t_c)
      pdi_t <- mt$PDI; l2_t <- mt$L2; l2_der_t <- mt$L2Der
    }

    if (pdi_t > pdi_cutoff_c * pdi_coef &&
        l2_t > l2_cutoff_c * l2_coef &&
        l2_der_t > l2_der_cutoff_c * l2der_coef &&
        length(new_interval) > 5) {
      drift_c <- drift_c + 1
      data_new_c <- rbind(data_sudden[new_interval, ], data_new_c)
      data_new_c <- na.omit(data_new_c)
      set.seed(123)
      rf_c <- train_robust(model_choice, form, data_new_c, target_col, reg_args)
      explainer_base_c <- DALEX::explain(model = rf_c,
                                         data = data_new_c, y = data_new_c[[target_col]],
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
    Mean_Accuracy = format(rowMeans(as.data.frame(acc_list_c), na.rm = TRUE), scientific = FALSE),
    Drift_Count = drift_c, Batch = number_of_batch,
    most_important = initial_most_important, model = model_choice,
    train_acc = format(train_accuracy, scientific = FALSE),
    overfit_gap = format(train_accuracy - test_accuracy, scientific = FALSE),
    regularize = regularize,
    stringsAsFactors = FALSE
  )

  # ---------- PDD_k ----------
  all_pdd_results <- data.frame()
  all_drift_logs <- data.frame()
  for (k in top_k_values) {
    rf_k <- rf
    data_new_k <- data_sudden[1:max(test_interval), ]
    top_vars <- get_top_k_vars(rf_k, model_choice, k)
    initial_top_vars <- paste(top_vars, collapse = ";")

    explainer_train <- DALEX::explain(model = rf_k,
                                      data = data_sudden[first_interval, ],
                                      y = data_sudden[first_interval, ][[target_col]],
                                      verbose = FALSE, label = "Base Train")
    explainer_test  <- DALEX::explain(model = rf_k,
                                      data = data_sudden[test_interval, ],
                                      y = data_sudden[test_interval, ][[target_col]],
                                      verbose = FALSE, label = "Base Test")
    profiles_train <- compute_profiles(explainer_train, top_vars, type)
    profiles_test  <- compute_profiles(explainer_test, top_vars, type)
    cutoffs <- compute_cutoffs(profiles_train, profiles_test)
    profiles_base <- profiles_test

    drift_log_k <- data.frame(
      model = model_choice, Batch = number_of_batch, top_k = k,
      regularize = regularize,
      update_no = 0, batch_index = 0,
      top_vars = initial_top_vars,
      drift_triggered_by = NA, accuracy = test_accuracy,
      stringsAsFactors = FALSE
    )

    acc_list <- list(); acc_idx <- 1; drift_number <- 0
    for (interval_i in seq(max(test_interval), nrow(data_sudden), by = inc_size)) {
      set.seed(123)
      new_interval <- interval_i:min(interval_i + inc_size, nrow(data_sudden))
      if (length(new_interval) <= 10) next
      preds_temp <- predict_class_robust(model_choice, rf_k, data_sudden[new_interval, ])
      acc_temp <- mean(preds_temp == data_sudden[new_interval, ][[target_col]], na.rm = TRUE)
      acc_list[[acc_idx]] <- acc_temp

      explainer_temp <- DALEX::explain(model = rf_k,
                                       data = data_sudden[new_interval, ],
                                       y = data_sudden[new_interval, ][[target_col]],
                                       verbose = FALSE, label = "temp")
      profiles_temp <- compute_profiles(explainer_temp, top_vars, type)

      triggered_by <- c()
      for (v in top_vars) {
        if (!(v %in% names(profiles_base)) || !(v %in% names(profiles_temp))) next
        if (!(v %in% names(cutoffs))) next
        p_base <- profiles_base[[v]]; p_new <- profiles_temp[[v]]
        if (length(p_base$x) < 5 || length(p_new$x) < 5) next
        mv <- tryCatch(metric_list_cal(p_base, p_new), error = function(e) NULL)
        if (is.null(mv)) next
        if (mv$PDI > cutoffs[[v]]$PDI * pdi_coef &&
            mv$L2  > cutoffs[[v]]$L2  * l2_coef &&
            mv$L2Der > cutoffs[[v]]$L2Der * l2der_coef) {
          triggered_by <- c(triggered_by, v)
        }
      }

      if (length(triggered_by) > 0 && length(new_interval) > 5) {
        drift_number <- drift_number + 1
        data_new_k <- rbind(data_sudden[new_interval, ], data_new_k)
        data_new_k <- na.omit(data_new_k)
        set.seed(123)
        rf_k <- train_robust(model_choice, form, data_new_k, target_col, reg_args)
        top_vars <- get_top_k_vars(rf_k, model_choice, k)

        explainer_new <- DALEX::explain(model = rf_k,
                                        data = data_new_k, y = data_new_k[[target_col]],
                                        verbose = FALSE,
                                        label = paste("Retrained", nrow(data_new_k)))
        profiles_base <- compute_profiles(explainer_new, top_vars, type)

        n_new <- nrow(data_new_k)
        split_idx <- floor(n_new * 0.8)
        explainer_pt    <- DALEX::explain(model = rf_k, data = data_new_k[1:split_idx, ],
                                          y = data_new_k[1:split_idx, ][[target_col]],
                                          verbose = FALSE, label = "Pseudo Train")
        explainer_ptest <- DALEX::explain(model = rf_k,
                                          data = data_new_k[(split_idx + 1):n_new, ],
                                          y = data_new_k[(split_idx + 1):n_new, ][[target_col]],
                                          verbose = FALSE, label = "Pseudo Test")
        profiles_pt    <- compute_profiles(explainer_pt,    top_vars, type)
        profiles_ptest <- compute_profiles(explainer_ptest, top_vars, type)
        cutoffs <- compute_cutoffs(profiles_pt, profiles_ptest)

        drift_log_k <- rbind(drift_log_k, data.frame(
          model = model_choice, Batch = number_of_batch, top_k = k,
          regularize = regularize,
          update_no = drift_number, batch_index = acc_idx,
          top_vars = paste(top_vars, collapse = ";"),
          drift_triggered_by = paste(triggered_by, collapse = ";"),
          accuracy = acc_temp, stringsAsFactors = FALSE
        ))
      }
      acc_idx <- acc_idx + 1
    }

    mean_acc <- rowMeans(as.data.frame(acc_list), na.rm = TRUE)
    all_pdd_results <- rbind(all_pdd_results, data.frame(
      Model = paste0("PDD_", k),
      Mean_Accuracy = format(mean_acc, scientific = FALSE),
      Drift_Count = drift_number, Batch = number_of_batch,
      most_important = initial_top_vars, model = model_choice,
      train_acc = format(train_accuracy, scientific = FALSE),
      overfit_gap = format(train_accuracy - test_accuracy, scientific = FALSE),
      regularize = regularize,
      stringsAsFactors = FALSE
    ))
    all_drift_logs <- rbind(all_drift_logs, drift_log_k)
  }

  # ---------- Baselines ----------
  run_drift_detection <- function(ddm_model, rf_initial) {
    set.seed(123)
    rf_current <- rf_initial
    ddm <- switch(ddm_model,
      "HDDM_A" = HDDM_A$new(), "HDDM_W" = HDDM_W$new(),
      "KSWIN"  = KSWIN$new(),  "PageHinkley" = PageHinkley$new(),
      "DDM"    = DDM$new(),    "EDDM"   = EDDM$new())
    acc_list <- list(); acc_idx <- 1
    data_new_ddm <- data_sudden[1:max(test_interval), ]; drift_count <- 0
    for (interval_i in seq(max(test_interval), nrow(data_sudden), by = inc_size)) {
      new_interval <- interval_i:min(interval_i + inc_size, nrow(data_sudden))
      set.seed(1234)
      preds_temp <- predict_class_robust(model_choice, rf_current, data_sudden[new_interval, ])
      acc_temp <- mean(preds_temp == data_sudden[new_interval, ][[target_col]], na.rm = TRUE)
      acc_list[[acc_idx]] <- acc_temp; acc_idx <- acc_idx + 1
      data_stream <- na.omit(1 - (data_sudden[new_interval, ][[target_col]] == preds_temp))
      tryCatch({
        for (i in seq_along(data_stream)) {
          try(ddm$add_element(data_stream[i]), silent = TRUE)
          if (ddm$change_detected) {
            drift_count <- drift_count + 1
            data_new_ddm <- rbind(data_sudden[new_interval, ], data_new_ddm)
            data_new_ddm <- na.omit(data_new_ddm)
            set.seed(123)
            rf_current <- train_robust(model_choice, form, data_new_ddm, target_col, reg_args)
            ddm$reset(); break
          }
        }
      }, error = function(e) message("ddm error: ", e$message))
    }
    list(mean_accuracy = mean(unlist(acc_list), na.rm = TRUE), drift_count = drift_count)
  }

  baselines <- list(
    HDDM_A = run_drift_detection("HDDM_A", rf1),
    HDDM_W = run_drift_detection("HDDM_W", rf1),
    KSWIN  = run_drift_detection("KSWIN",  rf1),
    PageHinkley = run_drift_detection("PageHinkley", rf1),
    DDM = run_drift_detection("DDM", rf1),
    EDDM = run_drift_detection("EDDM", rf1)
  )

  baseline_df <- do.call(rbind, lapply(names(baselines), function(nm) {
    data.frame(Model = nm,
               Mean_Accuracy = format(baselines[[nm]]$mean_accuracy, scientific = FALSE),
               Drift_Count = baselines[[nm]]$drift_count, Batch = number_of_batch,
               most_important = initial_most_important, model = model_choice,
               train_acc = format(train_accuracy, scientific = FALSE),
               overfit_gap = format(train_accuracy - test_accuracy, scientific = FALSE),
               regularize = regularize,
               stringsAsFactors = FALSE)
  }))

  extra_rows <- rbind(
    data.frame(Model = "Base Test",  Mean_Accuracy = format(test_accuracy, scientific = FALSE),
               Drift_Count = "0", Batch = number_of_batch,
               most_important = initial_most_important, model = model_choice,
               train_acc = format(train_accuracy, scientific = FALSE),
               overfit_gap = format(train_accuracy - test_accuracy, scientific = FALSE),
               regularize = regularize, stringsAsFactors = FALSE),
    data.frame(Model = "Base Train", Mean_Accuracy = format(train_accuracy, scientific = FALSE),
               Drift_Count = "0", Batch = number_of_batch,
               most_important = initial_most_important, model = model_choice,
               train_acc = format(train_accuracy, scientific = FALSE),
               overfit_gap = format(train_accuracy - test_accuracy, scientific = FALSE),
               regularize = regularize, stringsAsFactors = FALSE),
    data.frame(Model = "batch sizes",
               Mean_Accuracy = format(as.integer(batch_sizes$train_set), scientific = FALSE),
               Drift_Count = batch_sizes$test_set, Batch = number_of_batch,
               most_important = initial_most_important, model = model_choice,
               train_acc = format(train_accuracy, scientific = FALSE),
               overfit_gap = format(train_accuracy - test_accuracy, scientific = FALSE),
               regularize = regularize, stringsAsFactors = FALSE)
  )

  df_final <- rbind(baseline_df, pdd_c_result, all_pdd_results, extra_rows)
  list(results = df_final, drift_log = all_drift_logs)
}

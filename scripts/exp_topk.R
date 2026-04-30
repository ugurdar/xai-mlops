# Helper: get top-k important variables from a trained model
get_top_k_vars <- function(model, model_choice, k) {
  if (model_choice == "logistic") {
    varimp <- varImp(model)
    vars <- rownames(varimp)[order(varimp$Overall, decreasing = TRUE)]
  } else if (model_choice == "rf") {
    vars <- rownames(model$importance)[order(model$importance[,1], decreasing = TRUE)]
  } else if (model_choice == "tree") {
    vars <- names(sort(model$variable.importance, decreasing = TRUE))
  }
  head(vars, min(k, length(vars)))
}

# Helper: compute PDP profiles for multiple variables
compute_profiles <- function(explainer, vars, type = "partial") {
  profiles <- list()
  for (v in vars) {
    tryCatch({
      mp <- model_profile(explainer, v, type)
      profiles[[v]] <- data.frame(x = mp$agr_profiles$`_x_`, y = mp$agr_profiles$`_yhat_`)
    }, error = function(e) {
      message(paste("  Profile computation failed for", v))
      profiles[[v]] <<- data.frame(x = numeric(0), y = numeric(0))
    })
  }
  profiles
}

# Helper: compute cutoffs for multiple variables
compute_cutoffs <- function(profiles_a, profiles_b) {
  cutoffs <- list()
  common_vars <- intersect(names(profiles_a), names(profiles_b))
  for (v in common_vars) {
    pa <- profiles_a[[v]]
    pb <- profiles_b[[v]]
    if (length(pa$x) < 5 || length(pb$x) < 5) {
      cutoffs[[v]] <- list(PDI = 0.1, L2 = 0.1, L2Der = 0.1)
      next
    }
    tryCatch({
      cutoffs[[v]] <- metric_list_cal(pa, pb)
    }, error = function(e) {
      message(paste("  Cutoff computation failed for", v, "- using defaults"))
      cutoffs[[v]] <<- list(PDI = 0.1, L2 = 0.1, L2Der = 0.1)
    })
  }
  cutoffs
}

run_experiment_topk <- function(model_choice = c("logistic", "rf", "tree"),
                                data,
                                target_col = "class",
                                jj,
                                experiment_result_path,
                                type = "partial",
                                pdi_coef = 1,
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

  # === Train initial model ===
  if (model_choice == "logistic") {
    form <- as.formula(paste("as.factor(", target_col, ") ~ ."))
    rf <- glm(form, data = data_sudden[first_interval, ], family = "binomial")
    test_preds <- predict(rf, data_sudden[test_interval, ], type = "response")
    test_preds <- ifelse(test_preds > 0.50, 1, 0)
    train_preds <- predict(rf, data_sudden[first_interval, ], type = "response")
    train_preds <- ifelse(train_preds > 0.50, 1, 0)
  } else if (model_choice == "rf") {
    form <- as.formula(paste(target_col, "~ ."))
    rf <- randomForest(form, data = data_sudden[first_interval, ], ntree = 50)
    test_preds <- predict(rf, data_sudden[test_interval, ], type = "class")
    train_preds <- predict(rf, data_sudden[first_interval, ], type = "class")
  } else if (model_choice == "tree") {
    form <- as.formula(paste(target_col, "~ ."))
    rf <- rpart(form, data = data_sudden[first_interval, ])
    test_preds <- predict(rf, data_sudden[test_interval, ], type = "class")
    train_preds <- predict(rf, data_sudden[first_interval, ], type = "class")
  }

  test_accuracy <- mean(test_preds == data_sudden[test_interval, ][[target_col]])
  train_accuracy <- mean(train_preds == data_sudden[first_interval, ][[target_col]])
  rf1 <- rf
  initial_most_important <- get_top_k_vars(rf, model_choice, 1)

  inc_size <- batch_sizes$test_set

  # === Run PDD_C (classic: fixed importance, fixed cutoffs) ===
  # Replicates original exp.R exactly, including all plot operations for RNG consistency
  message(paste("=== PDD_C (classic) | model =", model_choice, "| batch =", number_of_batch, "==="))
  {
    rf_c <- rf
    data_new_c <- data_sudden[1:max(test_interval), ]
    most_important_c <- initial_most_important

    # Exact same order as original exp.R
    explainer_base_train_c <- DALEX::explain(model = rf_c,
      data = data_sudden[first_interval, ],
      y = data_sudden[first_interval, ][[target_col]],
      verbose = FALSE, label = "Base Train")
    model_profile_base_train_c <- model_profile(explainer_base_train_c, most_important_c, type)
    profile_train_c <- data.frame(x = model_profile_base_train_c$agr_profiles$`_x_`,
      y = model_profile_base_train_c$agr_profiles$`_yhat_`)

    explainer_base_c <- DALEX::explain(model = rf_c,
      data = data_sudden[test_interval, ],
      y = data_sudden[test_interval, ][[target_col]],
      verbose = FALSE, label = "Base Test")
    model_profile_base_c <- model_profile(explainer_base_c, most_important_c, type)
    profile_base_c <- data.frame(x = model_profile_base_c$agr_profiles$`_x_`,
      y = model_profile_base_c$agr_profiles$`_yhat_`)

    metrics_c <- metric_list_cal(profile_train_c, profile_base_c)
    pdi_cutoff_c <- metrics_c$PDI
    l2_cutoff_c <- metrics_c$L2
    l2_der_cutoff_c <- metrics_c$L2Der

    # Plot operations matching original exp.R (lines 79-90)
    tryCatch({
      print(plot(model_profile_base_train_c, model_profile_base_c))
    }, error = function(e) NULL)
    p1_c <- plot1(profile_train_c, "Train", profile_base_c, "Test", most_important_c) +
      ggtitle(paste("Train/Test", "-",
        "Accuracy:", round(test_accuracy, 2), "-",
        "PDI:", round(pdi_cutoff_c, 2), "-",
        "L2Der:", round(l2_der_cutoff_c, 2), "-",
        "L2:", round(l2_cutoff_c, 2)))
    plot_list_c <- list()
    plot_list_c[[1]] <- p1_c
    tryCatch({
      save_plot(experiment_result_path, number_of_batch,
        test_accuracy, pdi_cutoff_c, l2_der_cutoff_c, l2_cutoff_c, p1_c)
    }, error = function(e) NULL)

    acc_list_c <- list()
    acc_list_index_c <- 1
    drift_number_c <- 0

    for (interval_i in seq(max(test_interval), nrow(data_sudden), by = inc_size)) {
      set.seed(123)
      new_interval <- interval_i:min(interval_i + inc_size, nrow(data_sudden))
      if(length(new_interval) > 10){

        if (model_choice == "logistic") {
          preds_temp <- predict(rf_c, data_sudden[new_interval, ], type = "response")
          preds_temp <- ifelse(preds_temp > 0.50, 1, 0)
        } else {
          preds_temp <- predict(rf_c, data_sudden[new_interval, ], type = "class")
        }
        acc_temp <- mean(preds_temp == data_sudden[new_interval, ][[target_col]], na.rm = TRUE)
        acc_list_c[[acc_list_index_c]] <- acc_temp

        explainer_temp_c <- DALEX::explain(model = rf_c,
          data = data_sudden[new_interval, ],
          y = data_sudden[new_interval, ][[target_col]],
          verbose = FALSE, label = "temp")
        model_profile_temp_c <- model_profile(explainer_temp_c, most_important_c, type = type)
        profile_temp_c <- data.frame(x = model_profile_temp_c$agr_profiles$`_x_`,
          y = model_profile_temp_c$agr_profiles$`_yhat_`)

        if (length(profile_temp_c$x) < 5 || length(profile_base_c$x) < 5) {
          pdi_temp <- 1
          l2_temp <- 999
          l2_der_temp <- 999
        } else {
          metrics_temp <- metric_list_cal(profile_base_c, profile_temp_c)
          pdi_temp <- metrics_temp$PDI
          l2_temp <- metrics_temp$L2
          l2_der_temp <- metrics_temp$L2Der
        }

        # Plot operations matching original exp.R (lines 133-135)
        message(title(acc_temp, pdi_temp, l2_der_temp, l2_temp))
        tryCatch({
          print(plot(model_profile_base_c, model_profile_temp_c) +
            ggtitle(title(acc_temp, pdi_temp, l2_der_temp, l2_temp)))
        }, error = function(e) NULL)

        if (pdi_temp > pdi_cutoff_c * pdi_coef &&
            l2_temp > l2_cutoff_c * l2_coef &&
            l2_der_temp > l2_der_cutoff_c * l2der_coef &&
            length(new_interval) > 5) {

          message(title("Cutoff", pdi_cutoff_c, l2_der_cutoff_c, l2_cutoff_c))
          drift_number_c <- drift_number_c + 1
          message(paste("  PDD_C Drift #", drift_number_c, "at batch", acc_list_index_c))

          # Plot operations matching original exp.R (lines 145-154)
          p2_c <- plot1(profile_temp_c, "New Batch", profile_base_c, "Old Batch", most_important_c) +
            ggtitle(paste("Index:", acc_list_index_c, "-",
              "Accuracy:", round(acc_temp, 2), "-",
              "PDI:", round(pdi_temp, 2), "-",
              "L2Der:", round(l2_der_temp, 2), "-",
              "L2:", round(l2_temp, 2)))
          plot_list_c[[drift_number_c + 1]] <- p2_c
          tryCatch({
            save_plot(experiment_result_path, number_of_batch,
              acc_temp, pdi_temp, l2_der_temp, l2_temp, p2_c)
          }, error = function(e) NULL)
          message("new model training")

          data_new_c <- rbind(data_sudden[new_interval, ], data_new_c)
          data_new_c <- na.omit(data_new_c)
          set.seed(123)
          if (model_choice == "logistic") {
            rf_c <- glm(as.formula(paste("as.factor(", target_col, ") ~ .")),
              data = data_new_c, family = "binomial")
          } else if (model_choice == "rf") {
            rf_c <- randomForest(as.formula(paste(target_col, "~ .")),
              data = data_new_c, ntree = 50)
          } else if (model_choice == "tree") {
            rf_c <- rpart(as.formula(paste(target_col, "~ .")), data = data_new_c)
          }
          explainer_base_c <- DALEX::explain(model = rf_c,
            data = data_new_c, y = data_new_c[[target_col]],
            verbose = FALSE, label = paste("New Trained", nrow(data_new_c)))
          model_profile_base_c <- model_profile(explainer_base_c, most_important_c, type)
          profile_base_c <- data.frame(x = model_profile_base_c$agr_profiles$`_x_`,
            y = model_profile_base_c$agr_profiles$`_yhat_`)
        }
        acc_list_index_c <- acc_list_index_c + 1
      }
    }

    # Save combined plots matching original exp.R (lines 182-183)
    tryCatch({
      save_plots(plot_list_c, experiment_result_path, number_of_batch,
        round(mean(unlist(acc_list_c), na.rm = TRUE), 2))
    }, error = function(e) NULL)

    pdd_c_result <- data.frame(
      Model = "PDD_C",
      Mean_Accuracy = format(rowMeans(as.data.frame(acc_list_c), na.rm = TRUE), scientific = FALSE),
      Drift_Count = drift_number_c,
      Batch = number_of_batch,
      most_important = initial_most_important,
      model = model_choice,
      stringsAsFactors = FALSE
    )
  }

  # === Run PDD for each top_k ===
  all_pdd_results <- data.frame()
  all_drift_logs <- data.frame()

  for (k in top_k_values) {
    message(paste("=== PDD top_k =", k, "| model =", model_choice, "| batch =", number_of_batch, "==="))

    rf_k <- rf
    data_new_k <- data_sudden[1:max(test_interval), ]

    top_vars <- get_top_k_vars(rf_k, model_choice, k)
    initial_top_vars <- paste(top_vars, collapse = ";")

    # Compute initial baseline profiles (train & test)
    explainer_train <- DALEX::explain(model = rf_k,
      data = data_sudden[first_interval, ],
      y = data_sudden[first_interval, ][[target_col]],
      verbose = FALSE, label = "Base Train")
    explainer_test <- DALEX::explain(model = rf_k,
      data = data_sudden[test_interval, ],
      y = data_sudden[test_interval, ][[target_col]],
      verbose = FALSE, label = "Base Test")

    profiles_train <- compute_profiles(explainer_train, top_vars, type)
    profiles_test <- compute_profiles(explainer_test, top_vars, type)
    cutoffs <- compute_cutoffs(profiles_train, profiles_test)
    profiles_base <- profiles_test

    # Drift log: initial entry
    drift_log_k <- data.frame(
      model = model_choice, Batch = number_of_batch, top_k = k,
      update_no = 0, batch_index = 0,
      top_vars = initial_top_vars,
      drift_triggered_by = NA, accuracy = test_accuracy,
      stringsAsFactors = FALSE
    )

    acc_list <- list()
    acc_list_index <- 1
    drift_number <- 0

    for (interval_i in seq(max(test_interval), nrow(data_sudden), by = inc_size)) {
      set.seed(123)
      new_interval <- interval_i:min(interval_i + inc_size, nrow(data_sudden))
      if (length(new_interval) <= 10) next

      if (model_choice == "logistic") {
        preds_temp <- predict(rf_k, data_sudden[new_interval, ], type = "response")
        preds_temp <- ifelse(preds_temp > 0.50, 1, 0)
      } else {
        preds_temp <- predict(rf_k, data_sudden[new_interval, ], type = "class")
      }
      acc_temp <- mean(preds_temp == data_sudden[new_interval, ][[target_col]], na.rm = TRUE)
      acc_list[[acc_list_index]] <- acc_temp

      # Compute profiles for current batch
      explainer_temp <- DALEX::explain(model = rf_k,
        data = data_sudden[new_interval, ],
        y = data_sudden[new_interval, ][[target_col]],
        verbose = FALSE, label = "temp")
      profiles_temp <- compute_profiles(explainer_temp, top_vars, type)

      # Check drift: ANY variable exceeds its cutoffs
      triggered_by <- c()
      for (v in top_vars) {
        if (!(v %in% names(profiles_base)) || !(v %in% names(profiles_temp))) next
        if (!(v %in% names(cutoffs))) next
        p_base <- profiles_base[[v]]
        p_new <- profiles_temp[[v]]
        if (length(p_base$x) < 5 || length(p_new$x) < 5) next
        metrics_v <- tryCatch(metric_list_cal(p_base, p_new),
          error = function(e) NULL)
        if (is.null(metrics_v)) next
        if (metrics_v$PDI > cutoffs[[v]]$PDI * pdi_coef &&
            metrics_v$L2 > cutoffs[[v]]$L2 * l2_coef &&
            metrics_v$L2Der > cutoffs[[v]]$L2Der * l2der_coef) {
          triggered_by <- c(triggered_by, v)
        }
      }

      if (length(triggered_by) > 0 && length(new_interval) > 5) {
        drift_number <- drift_number + 1
        message(paste("  Drift #", drift_number, "at batch", acc_list_index,
                      "triggered by:", paste(triggered_by, collapse = ", ")))

        # Retrain model
        data_new_k <- rbind(data_sudden[new_interval, ], data_new_k)
        data_new_k <- na.omit(data_new_k)
        set.seed(123)
        if (model_choice == "logistic") {
          rf_k <- glm(as.formula(paste("as.factor(", target_col, ") ~ .")),
            data = data_new_k, family = "binomial")
        } else if (model_choice == "rf") {
          rf_k <- randomForest(as.formula(paste(target_col, "~ .")),
            data = data_new_k, ntree = 50)
        } else if (model_choice == "tree") {
          rf_k <- rpart(as.formula(paste(target_col, "~ .")), data = data_new_k)
        }

        # Recalculate top-k variables from retrained model
        top_vars <- get_top_k_vars(rf_k, model_choice, k)

        # Update baseline profiles
        explainer_new <- DALEX::explain(model = rf_k,
          data = data_new_k,
          y = data_new_k[[target_col]],
          verbose = FALSE,
          label = paste("Retrained", nrow(data_new_k)))
        profiles_base <- compute_profiles(explainer_new, top_vars, type)

        # Recompute cutoffs for (possibly new) variables
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
        profiles_pt <- compute_profiles(explainer_pt, top_vars, type)
        profiles_ptest <- compute_profiles(explainer_ptest, top_vars, type)
        cutoffs <- compute_cutoffs(profiles_pt, profiles_ptest)

        # Log drift event
        drift_log_k <- rbind(drift_log_k, data.frame(
          model = model_choice, Batch = number_of_batch, top_k = k,
          update_no = drift_number, batch_index = acc_list_index,
          top_vars = paste(top_vars, collapse = ";"),
          drift_triggered_by = paste(triggered_by, collapse = ";"),
          accuracy = acc_temp, stringsAsFactors = FALSE
        ))
      }

      acc_list_index <- acc_list_index + 1
    }

    # Store PDD result for this k
    mean_acc <- rowMeans(as.data.frame(acc_list), na.rm = TRUE)
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

  # === Run baseline drift detectors (once) ===
  run_drift_detection <- function(ddm_model, rf_initial, data_sudden, test_interval, inc_size) {
    set.seed(123)
    rf_current <- rf_initial
    ddm <- switch(ddm_model,
      "HDDM_A" = HDDM_A$new(),
      "HDDM_W" = HDDM_W$new(),
      "KSWIN"  = KSWIN$new(),
      "PageHinkley" = PageHinkley$new(),
      "DDM"    = DDM$new(),
      "EDDM"   = EDDM$new(),
      stop("Unknown model type"))
    acc_list <- list()
    acc_list_index <- 1
    data_new_ddm <- data_sudden[1:max(test_interval), ]
    drift_count <- 0
    for (interval_i in seq(max(test_interval), nrow(data_sudden), by = inc_size)) {
      new_interval <- interval_i:min(interval_i + inc_size, nrow(data_sudden))
      set.seed(1234)
      if (model_choice == "logistic") {
        preds_temp <- predict(rf_current, data_sudden[new_interval, ], type = "response")
        preds_temp <- ifelse(preds_temp > 0.50, 1, 0)
      } else {
        preds_temp <- predict(rf_current, data_sudden[new_interval, ], type = "class")
      }
      acc_temp <- mean(preds_temp == data_sudden[new_interval, ][[target_col]], na.rm = TRUE)
      acc_list[[acc_list_index]] <- acc_temp
      acc_list_index <- acc_list_index + 1
      data_stream <- na.omit(1 - (data_sudden[new_interval, ][[target_col]] == preds_temp))
      tryCatch({
        for (i in seq_along(data_stream)) {
          try(ddm$add_element(data_stream[i]))
          if (ddm$change_detected) {
            drift_count <- drift_count + 1
            data_new_ddm <- rbind(data_sudden[new_interval, ], data_new_ddm)
            data_new_ddm <- na.omit(data_new_ddm)
            set.seed(123)
            if (model_choice == "logistic") {
              rf_current <- glm(as.formula(paste("as.factor(", target_col, ") ~ .")),
                data = data_new_ddm, family = "binomial")
            } else if (model_choice == "rf") {
              rf_current <- randomForest(as.formula(paste(target_col, "~ .")),
                data = data_new_ddm, ntree = 50)
            } else if (model_choice == "tree") {
              rf_current <- rpart(as.formula(paste(target_col, "~ .")), data = data_new_ddm)
            }
            ddm$reset()
            break
          }
        }
      }, error = function(e) {
        message("Error detected: ", e$message)
      })
    }
    mean_acc <- rowMeans(as.data.frame(acc_list), na.rm = TRUE)
    list(mean_accuracy = mean_acc, drift_count = drift_count)
  }

  result_hddm_a <- run_drift_detection("HDDM_A", rf1, data_sudden, test_interval, inc_size)
  result_hddm_w <- run_drift_detection("HDDM_W", rf1, data_sudden, test_interval, inc_size)
  result_kswin  <- run_drift_detection("KSWIN", rf1, data_sudden, test_interval, inc_size)
  result_page_hinkley <- run_drift_detection("PageHinkley", rf1, data_sudden, test_interval, inc_size)
  result_ddm    <- run_drift_detection("DDM", rf1, data_sudden, test_interval, inc_size)
  result_eddm   <- run_drift_detection("EDDM", rf1, data_sudden, test_interval, inc_size)

  results_list <- list(
    HDDM_A = result_hddm_a, HDDM_W = result_hddm_w,
    KSWIN = result_kswin, PageHinkley = result_page_hinkley,
    DDM = result_ddm, EDDM = result_eddm
  )

  baseline_df <- do.call(rbind, lapply(names(results_list), function(name) {
    data.frame(
      Model = name,
      Mean_Accuracy = format(results_list[[name]]$mean_accuracy, scientific = FALSE),
      Drift_Count = results_list[[name]]$drift_count,
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

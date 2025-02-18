run_experiment_friedman <- function(model_choice = c("linear", "rf", "tree"),
                           data,
                           target_col = "target",
                           jj,
                           experiment_result_path,
                           type = "partial",
                           pdi_coef = 0,
                           l2_coef = 1,
                           l2der_coef = 1) {
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
  
  if(model_choice == "linear"){
    rf <- lm(form, data = data_sudden[first_interval,])
  } else if(model_choice == "rf"){
    rf <- randomForest(form, data = data_sudden[first_interval,], ntree = 50)
  } else if(model_choice == "tree"){
    rf <- rpart(form, data = data_sudden[first_interval,])
  }
  
  rf1 <- rf
  test_preds <- predict(rf, data_sudden[test_interval,])
  train_preds <- predict(rf, data_sudden[first_interval,])
  
  test_accuracy <- RMSE(test_preds, data_sudden[test_interval,][[target_col]])
  train_accuracy <- RMSE(train_preds, data_sudden[first_interval,][[target_col]])
  
  # Önemli değişkenin seçimi
  if(model_choice == "linear"){
    varimp <- varImp(rf)
    most_important <- rownames(varimp)[which.max(varimp$Overall)]
  } else if(model_choice == "rf"){
    most_important <- rownames(rf$importance)[which.max(rf$importance)]
  } else if(model_choice == "tree"){
    most_important <- names(which.max(rf$variable.importance))
  }
  
  explainer_base_train <- DALEX::explain(model = rf,
                                         data = data_sudden[first_interval,],
                                         y = data_sudden[first_interval,][[target_col]],
                                         verbose = FALSE,
                                         label = "Base Train")
  model_profile_base_train <- model_profile(explainer_base_train, most_important, type)
  profile1_train <- data.frame(x = model_profile_base_train$agr_profiles$`_x_`,
                               y = model_profile_base_train$agr_profiles$`_yhat_`)
  
  explainer_base <- DALEX::explain(model = rf,
                                   data = data_sudden[test_interval,],
                                   y = data_sudden[test_interval,][[target_col]],
                                   verbose = FALSE,
                                   label = "Base Test")
  model_profile_base <- model_profile(explainer_base, most_important, type)
  profile1 <- data.frame(x = model_profile_base$agr_profiles$`_x_`,
                         y = model_profile_base$agr_profiles$`_yhat_`)
  
  metrics <- metric_list_cal(profile1_train, profile1)
  pdi_cutoff <- metrics$PDI
  l2_cutoff <- metrics$L2
  l2_der_cutoff <- metrics$L2Der
  
  print(plot(model_profile_base_train, model_profile_base))
  p1 <- plot1(profile1_train, "Train", profile1, "Test", most_important) +
    ggtitle(paste("Train/Test - RMSE:", round(test_accuracy, 2),
                  "- PDI:", round(pdi_cutoff, 2),
                  "- L2Der:", round(l2_der_cutoff, 2),
                  "- L2:", round(l2_cutoff, 2)))
  plot_list <- list()
  plot_list[[1]] <- p1
  save_plot(experiment_result_path, number_of_batch,
            test_accuracy, pdi_cutoff, l2_der_cutoff, l2_cutoff, p1)
  
  inc_size <- batch_sizes$test_set
  acc_list_pdi <- list()
  acc_list_index <- 1
  data_new <- data_sudden[1:max(test_interval),]
  drift_number <- 0
  start <- Sys.time()
  
  for(interval_i in seq(max(test_interval), nrow(data_sudden), by = inc_size)) {
    set.seed(123)
    new_interval <- interval_i:(interval_i + inc_size)
    if(length(new_interval) > 5){
    preds_temp <- predict(rf, na.omit(data_sudden[new_interval,]))
    acc_temp <- RMSE(preds_temp, na.omit(data_sudden[new_interval,])[[target_col]])
    acc_list_pdi[[acc_list_index]] <- acc_temp
    
    explainer_temp <- DALEX::explain(model = rf,
                                     data = data_sudden[new_interval,],
                                     y = data_sudden[new_interval,][[target_col]],
                                     verbose = FALSE,
                                     label = "temp")
    model_profile_temp <- model_profile(explainer_temp, most_important, type = type)
    profile2 <- data.frame(x = model_profile_temp$agr_profiles$`_x_`,
                           y = model_profile_temp$agr_profiles$`_yhat_`)
    
    if(length(profile2$x) < 5 || length(profile1$x) < 5){
      pdi_temp <- 1
      l2_temp <- 999
      l2_der_temp <- 999
    } else {
      metrics <- metric_list_cal(profile1, profile2)
      pdi_temp <- metrics$PDI
      l2_temp <- metrics$L2
      l2_der_temp <- metrics$L2Der
    }
    
    message(title(acc_temp, pdi_temp, l2_der_temp, l2_temp))
    print(plot(model_profile_base, model_profile_temp) +
            ggtitle(title(acc_temp, pdi_temp, l2_der_temp, l2_temp)))
    
    if(l2_temp > l2_cutoff * l2_coef && l2_der_temp > l2_der_cutoff * l2der_coef && length(new_interval) > 5){
      message(title("Cutoff", pdi_cutoff, l2_der_cutoff, l2_cutoff))
      drift_number <- drift_number + 1
      p2 <- plot1(profile2, "New Batch", profile1, "Old Batch", most_important) +
        ggtitle(paste("Index:", acc_list_index, "- RMSE:", round(acc_temp, 2),
                      "- PDI:", round(pdi_temp, 2),
                      "- L2Der:", round(l2_der_temp, 2),
                      "- L2:", round(l2_temp, 2)))
      plot_list[[drift_number+1]] <- p2
      save_plot(experiment_result_path, number_of_batch, acc_temp,
                pdi_temp, l2_der_temp, l2_temp, p2)
      message("new model training")
      
      data_new <- rbind(data_sudden[new_interval,], data_new)
      data_new <- na.omit(data_new)
      print(nrow(data_new))
      set.seed(123)
      if(model_choice=="linear"){
        rf <- lm(form, data = data_new)
      } else if(model_choice=="rf"){
        rf <- randomForest(form, data = data_new, ntree = 50)
      } else if(model_choice=="tree"){
        rf <- rpart(form, data = data_new)
      }
      explainer_base <- DALEX::explain(model = rf,
                                       data = data_new,
                                       y = data_new[[target_col]],
                                       verbose = FALSE,
                                       label = paste("New Trained", nrow(data_new)))
      model_profile_base <- model_profile(explainer_base, most_important, type)
      profile1 <- data.frame(x = model_profile_base$agr_profiles$`_x_`,
                             y = model_profile_base$agr_profiles$`_yhat_`)
    }
    acc_list_index <- acc_list_index + 1
   }
  }
  save_plots(plot_list, experiment_result_path, number_of_batch,
             round(mean(unlist(acc_list_pdi), na.rm = TRUE), 2))
  
  run_drift_detection <- function(ddm_model, rf_initial, data_sudden, test_interval, inc_size) {
    set.seed(123)
    rf <- rf_initial
    ddm <- switch(ddm_model,
                  "HDDM_A" = HDDM_A$new(),
                  "HDDM_W" = HDDM_W$new(),
                  "KSWIN" = KSWIN$new(),
                  "PageHinkley" = PageHinkley$new(),
                  "DDM" = DDM$new(),
                  "EDDM" = EDDM$new(),
                  stop("Unknown model type"))
    acc_list <- list()
    acc_list_index <- 1
    data_new_ddm <- data_sudden[1:max(test_interval),]
    drift_count <- 0
    start_time <- Sys.time()
    for(interval_i in seq(max(test_interval), nrow(data_sudden), by = inc_size)){
      new_interval <- interval_i:(interval_i + inc_size)
      set.seed(1234)
      if(length(new_interval) > 10){
      preds_temp <- predict(rf, data_sudden[new_interval,])
      acc_temp <- RMSE(preds_temp, data_sudden[new_interval,][[target_col]])
      acc_list[[acc_list_index]] <- acc_temp
      acc_list_index <- acc_list_index + 1
      data_stream <- abs(data_sudden[new_interval,][[target_col]] - preds_temp)
      data_stream <- na.omit(data_stream)
        tryCatch({
          for(i in seq_along(data_stream)){
            try(ddm$add_element(data_stream[i]))
            if(ddm$change_detected){
              drift_count <- drift_count + 1
              message(paste("Drift detected!", i, interval_i, round(acc_temp, 2)))
              message("new model training")
              data_new_ddm <- rbind(data_sudden[new_interval,], data_new_ddm)
              data_new_ddm <- na.omit(data_new_ddm)
              set.seed(123)
              if(model_choice=="linear"){
                rf <- lm(form, data = data_new_ddm)
              } else if(model_choice=="rf"){
                rf <- randomForest(form, data = data_new_ddm, ntree = 50)
              } else if(model_choice=="tree"){
                rf <- rpart(form, data = data_new_ddm)
              }
              ddm$reset()
              break
            }
          }
        }, error = function(e) {
          message("Error detected: ", e$message)
        })
        
      }
      
    }
    runtime <- Sys.time() - start_time
    mean_acc <- rowMeans(as.data.frame(acc_list), na.rm = TRUE)
    list(mean_accuracy = mean_acc, drift_count = drift_count, runtime = runtime)
  }

  result_kswin   <- run_drift_detection("KSWIN", rf1, data_sudden, test_interval, inc_size)
  result_page_hinkley <- run_drift_detection("PageHinkley", rf1, data_sudden, test_interval, inc_size)

  results_list <- list(
    KSWIN       = result_kswin,
    PageHinkley = result_page_hinkley
  )
  results_df <- do.call(rbind, lapply(names(results_list), function(name) {
    data.frame(Model = name,
               Mean_Accuracy = results_list[[name]]$mean_accuracy,
               Drift_Count = results_list[[name]]$drift_count)
  }))
  df2 <- rbind(results_df,
               data.frame(Model = "PDD", Mean_Accuracy = rowMeans(as.data.frame(acc_list_pdi), na.rm = TRUE), Drift_Count = drift_number),
               data.frame(Model = "Base Test", Mean_Accuracy = test_accuracy, Drift_Count = "0"),
               data.frame(Model = "Base Train", Mean_Accuracy = train_accuracy, Drift_Count = "0"),
               data.frame(Model = "batch sizes", Mean_Accuracy = as.integer(batch_sizes$train_set), Drift_Count = batch_sizes$test_set)
  )
  df2$Mean_Accuracy <- format(df2$Mean_Accuracy, scientific = FALSE)
  df2 <- cbind(df2, Batch = number_of_batch, most_important, model = model_choice)
  
  return(df2)
}

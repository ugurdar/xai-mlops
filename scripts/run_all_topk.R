source("scripts/2_utils.R")
source("scripts/exp_topk.R")

splits <- c(3, 5, 7)
batches <- c(10, 20, 30)
models <- c("rf", "logistic", "tree")

#############################################
##### Elec2 - Top-K Experiment ##############
message("========== Starting Elec2 ==========")
set.seed(1234)
data <- read.arff(paste0("data/", "elec", ".arff"))
data$class <- ifelse(data$class == "UP", 1, 0)
data$class <- as.factor(data$class)
data <- data %>% dplyr::select(nswprice, nswdemand, vicprice, vicdemand, class)

df_results <- data.frame()
df_drift_log <- data.frame()
count <- 1

for (split in splits) {
  results_model <- data.frame()
  for (model in models) {
    experiment_result_path <- paste0("results/plots/Elec2_topk/", model, "/", batches[count], "/")
    result <- run_experiment_topk(model, data, target_col = "class", split, experiment_result_path)
    results_model <- rbind(results_model, result$results)
    df_drift_log <- rbind(df_drift_log, result$drift_log)
  }
  df_results <- rbind(df_results, results_model)
  count <- count + 1
}

write.csv(df_results, file = "results/tables/Elec2_topk.csv", row.names = FALSE)
write.csv(df_drift_log, file = "results/tables/Elec2_topk_drift_log.csv", row.names = FALSE)
message("Done! Elec2 results saved.")

#############################################
##### Hyperplane - Top-K Experiment #########
message("========== Starting Hyperplane ==========")
set.seed(1234)
data <- read.csv("data/hyperplane07.csv", header = TRUE)
data$target <- as.factor(data$target)

df_results <- data.frame()
df_drift_log <- data.frame()
count <- 1

for (split in splits) {
  results_model <- data.frame()
  for (model in models) {
    experiment_result_path <- paste0("results/plots/hyperplane_topk/", model, "/", batches[count], "/")
    result <- run_experiment_topk(model, data, target_col = "target", split, experiment_result_path)
    results_model <- rbind(results_model, result$results)
    df_drift_log <- rbind(df_drift_log, result$drift_log)
  }
  df_results <- rbind(df_results, results_model)
  count <- count + 1
}

write.csv(df_results, file = "results/tables/hyperplane_topk.csv", row.names = FALSE)
write.csv(df_drift_log, file = "results/tables/hyperplane_topk_drift_log.csv", row.names = FALSE)
message("Done! Hyperplane results saved.")

#############################################
##### NOAA - Top-K Experiment ###############
message("========== Starting NOAA ==========")
set.seed(1234)
data <- read.arff(paste0("data/", "NOAA", ".arff"))

df_results <- data.frame()
df_drift_log <- data.frame()
count <- 1

for (split in splits) {
  results_model <- data.frame()
  for (model in models) {
    experiment_result_path <- paste0("results/plots/NOAA_topk/", model, "/", batches[count], "/")
    result <- run_experiment_topk(model, data, target_col = "class", split, experiment_result_path)
    results_model <- rbind(results_model, result$results)
    df_drift_log <- rbind(df_drift_log, result$drift_log)
  }
  df_results <- rbind(df_results, results_model)
  count <- count + 1
}

write.csv(df_results, file = "results/tables/NOAA_topk.csv", row.names = FALSE)
write.csv(df_drift_log, file = "results/tables/NOAA_topk_drift_log.csv", row.names = FALSE)
message("Done! NOAA results saved.")

#############################################
##### Ozone - Top-K Experiment ##############
message("========== Starting Ozone ==========")
set.seed(1234)
data <- read.arff(paste0("data/", "ozone", ".arff"))

df_results <- data.frame()
df_drift_log <- data.frame()
count <- 1

for (split in splits) {
  results_model <- data.frame()
  for (model in models) {
    experiment_result_path <- paste0("results/plots/ozone_topk/", model, "/", batches[count], "/")
    result <- run_experiment_topk(model, data, target_col = "Class", split, experiment_result_path)
    results_model <- rbind(results_model, result$results)
    df_drift_log <- rbind(df_drift_log, result$drift_log)
  }
  df_results <- rbind(df_results, results_model)
  count <- count + 1
}

write.csv(df_results, file = "results/tables/ozone_topk.csv", row.names = FALSE)
write.csv(df_drift_log, file = "results/tables/ozone_topk_drift_log.csv", row.names = FALSE)
message("Done! Ozone results saved.")

#############################################
##### SEA - Top-K Experiment ################
message("========== Starting SEA ==========")
set.seed(1234)
xx <- read.csv("data/SEA_training_data.csv", header = FALSE)
yy <- read.csv("data/SEA_training_class.csv", header = FALSE)
data <- data.frame(xx, class = as.factor(yy$V1))

df_results <- data.frame()
df_drift_log <- data.frame()
count <- 1

for (split in splits) {
  results_model <- data.frame()
  for (model in models) {
    experiment_result_path <- paste0("results/plots/SEA_topk/", model, "/", batches[count], "/")
    result <- run_experiment_topk(model, data, target_col = "class", split, experiment_result_path)
    results_model <- rbind(results_model, result$results)
    df_drift_log <- rbind(df_drift_log, result$drift_log)
  }
  df_results <- rbind(df_results, results_model)
  count <- count + 1
}

write.csv(df_results, file = "results/tables/SEA_topk.csv", row.names = FALSE)
write.csv(df_drift_log, file = "results/tables/SEA_topk_drift_log.csv", row.names = FALSE)
message("Done! SEA results saved.")

message("========== ALL EXPERIMENTS COMPLETE ==========")

# Friedman top-k driver
# 1) Friedman_topk.csv         — primary table; uses 5x L2/L2Der relaxation (matches the
#                                 published Friedman setup described in article.tex L:426)
# 2) Friedman_topk_thr_sweep.csv — extra: sweeps coef in {1,2,3,4,5} as a sensitivity study
#                                 over the threshold relaxation called out by Reviewer #1.

source("scripts/2_utils.R")
source("scripts/exp_friedman_topk.R")

splits  <- c(3, 5, 7)
batches <- c(10, 20, 30)
models  <- c("rf", "linear", "tree")

set.seed(1234)
data <- read.csv("data/friedman_drift_dataset.csv", header = TRUE)

# ------------------------------------------------------------------
# 1) Primary top-k table (default 5x L2/L2Der, matching original article)
# ------------------------------------------------------------------
message("========== Friedman top-k (5x L2/L2Der — primary table) ==========")
df_results  <- data.frame()
df_drift    <- data.frame()
count <- 1
for (split in splits) {
  results_model <- data.frame()
  for (model in models) {
    message(model)
    out_path <- paste0("results/plots/Friedman_topk/", model, "/", batches[count], "/")
    res <- run_experiment_friedman_topk(
      model_choice = model, data = data, target_col = "target",
      jj = split, experiment_result_path = out_path,
      l2_coef = 5, l2der_coef = 5
    )
    results_model <- rbind(results_model, res$results)
    df_drift <- rbind(df_drift, res$drift_log)
  }
  df_results <- rbind(df_results, results_model)
  count <- count + 1
}
write.csv(df_results, file = "results/tables/Friedman_topk.csv", row.names = FALSE)
write.csv(df_drift,   file = "results/tables/Friedman_topk_drift_log.csv", row.names = FALSE)
message("Saved: results/tables/Friedman_topk.csv")

# ------------------------------------------------------------------
# 2) Threshold-sweep companion table (coef in {1..5})
# ------------------------------------------------------------------
message("========== Friedman top-k threshold sweep (1x..5x) ==========")
sweep_coefs <- 1:5
df_sweep       <- data.frame()
df_sweep_drift <- data.frame()
for (coef in sweep_coefs) {
  count <- 1
  for (split in splits) {
    for (model in models) {
      message(paste("coef =", coef, "model =", model, "split =", split))
      out_path <- paste0("results/plots/Friedman_topk_sweep/coef_", coef, "/",
                         model, "/", batches[count], "/")
      res <- run_experiment_friedman_topk(
        model_choice = model, data = data, target_col = "target",
        jj = split, experiment_result_path = out_path,
        l2_coef = coef, l2der_coef = coef
      )
      res_with_coef <- cbind(res$results, l2_coef = coef, l2der_coef = coef)
      df_sweep <- rbind(df_sweep, res_with_coef)
      df_sweep_drift <- rbind(df_sweep_drift, res$drift_log)
    }
    count <- count + 1
  }
}
write.csv(df_sweep,       file = "results/tables/Friedman_topk_thr_sweep.csv",       row.names = FALSE)
write.csv(df_sweep_drift, file = "results/tables/Friedman_topk_thr_sweep_drift_log.csv", row.names = FALSE)
message("Saved: results/tables/Friedman_topk_thr_sweep.csv")

message("========== Friedman top-k experiments complete ==========")

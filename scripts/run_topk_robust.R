# Robust top-k driver — NOAA & Ozone only, RF + tree (regularized).
# Logistic kept for comparison even though it doesn't overfit on these.

source("scripts/2_utils.R")
source("scripts/exp_topk_robust.R")

splits  <- c(3, 5, 7)
batches <- c(10, 20, 30)
models  <- c("rf", "logistic", "tree")

# ---------- NOAA ----------
message("========== NOAA (robust=strong) ==========")
set.seed(1234)
data_noaa <- read.arff("data/NOAA.arff")

df_results <- data.frame(); df_drift <- data.frame(); count <- 1
for (split in splits) {
  for (model in models) {
    message(sprintf("NOAA | split=%d batch=%d model=%s", split, batches[count], model))
    out_path <- paste0("results/plots/NOAA_topk_robust/", model, "/", batches[count], "/")
    res <- run_experiment_topk_robust(
      model_choice = model, data = data_noaa, target_col = "class",
      jj = split, experiment_result_path = out_path,
      regularize = "strong"
    )
    df_results <- rbind(df_results, res$results)
    df_drift   <- rbind(df_drift,   res$drift_log)
  }
  count <- count + 1
}
write.csv(df_results, file = "results/tables/NOAA_topk_robust.csv", row.names = FALSE)
write.csv(df_drift,   file = "results/tables/NOAA_topk_robust_drift_log.csv", row.names = FALSE)
message("Saved NOAA robust tables.")

# ---------- Ozone ----------
message("========== Ozone (robust=strong, classwt for RF) ==========")
set.seed(1234)
data_ozone <- read.arff("data/ozone.arff")

# Severe imbalance (~6% positive). Class factor levels are "1"/"2" — minority=2.
ozone_classwt <- c("1" = 1, "2" = 15)

df_results <- data.frame(); df_drift <- data.frame(); count <- 1
for (split in splits) {
  for (model in models) {
    message(sprintf("Ozone | split=%d batch=%d model=%s", split, batches[count], model))
    out_path <- paste0("results/plots/ozone_topk_robust/", model, "/", batches[count], "/")
    cw <- if (model == "rf") ozone_classwt else NULL
    res <- run_experiment_topk_robust(
      model_choice = model, data = data_ozone, target_col = "Class",
      jj = split, experiment_result_path = out_path,
      regularize = "strong", classwt = cw
    )
    df_results <- rbind(df_results, res$results)
    df_drift   <- rbind(df_drift,   res$drift_log)
  }
  count <- count + 1
}
write.csv(df_results, file = "results/tables/ozone_topk_robust.csv", row.names = FALSE)
write.csv(df_drift,   file = "results/tables/ozone_topk_robust_drift_log.csv", row.names = FALSE)
message("Saved Ozone robust tables.")

message("========== Robust top-k experiments complete ==========")

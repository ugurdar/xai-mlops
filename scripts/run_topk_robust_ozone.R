# Ozone-only robust top-k driver (rerun after classwt fix).
source("scripts/2_utils.R")
source("scripts/exp_topk_robust.R")

splits  <- c(3, 5, 7)
batches <- c(10, 20, 30)
models  <- c("rf", "logistic", "tree")

set.seed(1234)
data_ozone <- read.arff("data/ozone.arff")

# Class levels are "1"/"2"; minority = "2" (~6.3%)
ozone_classwt <- c("1" = 1, "2" = 15)

df_results <- data.frame(); df_drift <- data.frame(); count <- 1
for (split in splits) {
  for (model in models) {
    message(sprintf("Ozone | split=%d batch=%d model=%s", split, batches[count], model))
    out_path <- paste0("results/plots/ozone_topk_robust/", model, "/", batches[count], "/")
    cw <- if (model == "rf") ozone_classwt else NULL
    res <- tryCatch(
      run_experiment_topk_robust(
        model_choice = model, data = data_ozone, target_col = "Class",
        jj = split, experiment_result_path = out_path,
        regularize = "strong", classwt = cw
      ),
      error = function(e) {
        message(sprintf("  SKIPPED (error): split=%d model=%s -- %s",
                        split, model, conditionMessage(e)))
        NULL
      }
    )
    if (!is.null(res)) {
      df_results <- rbind(df_results, res$results)
      df_drift   <- rbind(df_drift,   res$drift_log)
    }
    # Save after every iteration so partial progress isn't lost
    write.csv(df_results, file = "results/tables/ozone_topk_robust.csv", row.names = FALSE)
    write.csv(df_drift,   file = "results/tables/ozone_topk_robust_drift_log.csv", row.names = FALSE)
  }
  count <- count + 1
}
write.csv(df_results, file = "results/tables/ozone_topk_robust.csv", row.names = FALSE)
write.csv(df_drift,   file = "results/tables/ozone_topk_robust_drift_log.csv", row.names = FALSE)
message("Ozone robust complete.")

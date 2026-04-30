# Re-runs NOAA + Ozone with class encoded 0/1 (matching elec2 pattern).
# Background: glm with family=binomial expects 0/1 (or 2-level factor where the
# second level is the "positive"). Original NOAA/Ozone class factor levels were
# "1"/"2", so ifelse(p>0.5, 1, 0) compared numeric 0/1 to factor("1","2") and
# nearly always failed → LR accuracy ~0.07. RF/DT use predict(type="class")
# which returns factor levels directly so they were unaffected — and we verify
# this by also re-running RF/DT and confirming numbers don't change.
#
# Original exp_topk.R is untouched. Only the data-encoding step before each
# experiment is changed (driver-level fix, mirroring how elec2 was already set up).

source("scripts/2_utils.R")
source("scripts/exp_topk.R")

splits  <- c(3, 5, 7)
batches <- c(10, 20, 30)
models  <- c("rf", "logistic", "tree")

# Save under "_lrfix" first so we can diff RF/DT against original before overwriting
NOAA_OUT  <- "results/tables/NOAA_topk_lrfix.csv"
NOAA_LOG  <- "results/tables/NOAA_topk_lrfix_drift_log.csv"
OZONE_OUT <- "results/tables/ozone_topk_lrfix.csv"
OZONE_LOG <- "results/tables/ozone_topk_lrfix_drift_log.csv"

# ---------- NOAA ----------
message("========== NOAA (class encoded 0/1) ==========")
set.seed(1234)
data_noaa <- read.arff("data/NOAA.arff")
# levels were "1" (majority, 68.6%) and "2" (minority); recode minority="2" -> 1
data_noaa$class <- ifelse(data_noaa$class == "1", 0, 1)
data_noaa$class <- as.factor(data_noaa$class)
message("NOAA class table after recode:"); print(table(data_noaa$class))

df_results <- data.frame(); df_drift <- data.frame(); count <- 1
for (split in splits) {
  for (model in models) {
    message(sprintf("NOAA | split=%d batch=%d model=%s", split, batches[count], model))
    out_path <- paste0("results/plots/NOAA_topk_lrfix/", model, "/", batches[count], "/")
    res <- tryCatch(
      run_experiment_topk(model, data_noaa, target_col = "class",
                          jj = split, experiment_result_path = out_path),
      error = function(e) {
        message(sprintf("  SKIPPED: %s", conditionMessage(e))); NULL
      }
    )
    if (!is.null(res)) {
      df_results <- rbind(df_results, res$results)
      df_drift   <- rbind(df_drift,   res$drift_log)
    }
    write.csv(df_results, file = NOAA_OUT, row.names = FALSE)
    write.csv(df_drift,   file = NOAA_LOG, row.names = FALSE)
  }
  count <- count + 1
}
message("Saved NOAA_topk_lrfix.csv")

# ---------- Ozone ----------
message("========== Ozone (Class encoded 0/1) ==========")
set.seed(1234)
data_ozone <- read.arff("data/ozone.arff")
# levels were "1" (negative, 93.7%) and "2" (positive ozone day, 6.3%)
data_ozone$Class <- ifelse(data_ozone$Class == "1", 0, 1)
data_ozone$Class <- as.factor(data_ozone$Class)
message("Ozone Class table after recode:"); print(table(data_ozone$Class))

df_results <- data.frame(); df_drift <- data.frame(); count <- 1
for (split in splits) {
  for (model in models) {
    message(sprintf("Ozone | split=%d batch=%d model=%s", split, batches[count], model))
    out_path <- paste0("results/plots/ozone_topk_lrfix/", model, "/", batches[count], "/")
    res <- tryCatch(
      run_experiment_topk(model, data_ozone, target_col = "Class",
                          jj = split, experiment_result_path = out_path),
      error = function(e) {
        message(sprintf("  SKIPPED: %s", conditionMessage(e))); NULL
      }
    )
    if (!is.null(res)) {
      df_results <- rbind(df_results, res$results)
      df_drift   <- rbind(df_drift,   res$drift_log)
    }
    write.csv(df_results, file = OZONE_OUT, row.names = FALSE)
    write.csv(df_drift,   file = OZONE_LOG, row.names = FALSE)
  }
  count <- count + 1
}
message("Saved ozone_topk_lrfix.csv")
message("========== LR-fix re-run complete ==========")

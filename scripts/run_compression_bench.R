# Compression benchmark: run Elec2 with varying N_sample and G_grid
# Trade-off: runtime vs accuracy vs drift count
source("scripts/2_utils.R")
source("scripts/exp_topk_compressed.R")

message("========== Compression benchmark (Elec2 RF, split=3, batch=10) ==========")
set.seed(1234)
data <- read.arff("data/elec.arff")
data$class <- ifelse(data$class == "UP", 1, 0)
data$class <- as.factor(data$class)
data <- data %>% dplyr::select(nswprice, nswdemand, vicprice, vicdemand, class)

# Grid of compression settings:
#   N_sample: subsample size for PDP (DALEX default = 100)
#   G_grid:   quantile grid resolution (NULL = full grid of ~G_max points)
configs <- list(
  list(N = 30,  G = NULL, tag = "N30"),
  list(N = 50,  G = NULL, tag = "N50"),
  list(N = 100, G = NULL, tag = "N100_baseline"),
  list(N = 200, G = NULL, tag = "N200"),
  list(N = 100, G = 10,   tag = "N100_G10"),
  list(N = 100, G = 20,   tag = "N100_G20"),
  list(N = 30,  G = 10,   tag = "N30_G10_maxcompress")
)

all_df <- data.frame()
for (cfg in configs) {
  message(sprintf("\n>>> Config: N=%s G=%s  (%s)",
                  cfg$N, ifelse(is.null(cfg$G), "full", cfg$G), cfg$tag))
  res <- run_experiment_topk_compressed(
    model_choice = "rf",
    data         = data,
    target_col   = "class",
    jj           = 3,
    top_k_values = c(1, 2, 3),
    N_sample     = cfg$N,
    G_grid       = cfg$G
  )
  res$config <- cfg$tag
  all_df <- rbind(all_df, res)
}

write.csv(all_df, "results/tables/compression_bench_elec2.csv", row.names = FALSE)
message("\n========== Summary ==========")
print(all_df[, c("config", "Model", "Mean_Accuracy", "Drift_Count",
                 "N_sample", "G_grid", "Runtime_sec")])

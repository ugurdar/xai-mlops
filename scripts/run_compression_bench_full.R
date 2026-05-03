# Full compression benchmark: Elec2
# Compares PDD (default N=100) vs compressed variants vs baseline methods
# Measures wall-clock time for all methods

source("scripts/2_utils.R")
source("scripts/exp_topk_compressed.R")
source("scripts/exp_topk.R")

message("========== Full Compression Benchmark (Elec2) ==========")
set.seed(1234)
data <- read.arff("data/elec.arff")
data$class <- ifelse(data$class == "UP", 1, 0)
data$class <- as.factor(data$class)
data <- data %>% dplyr::select(nswprice, nswdemand, vicprice, vicdemand, class)

split_val <- 3  # batch=10

# ---------------------------------------------------------------
# 1) Compressed PDD configs (N < 100 and/or G_grid)
# ---------------------------------------------------------------
configs <- list(
  # Default PDD (N=100, no grid compression)
  list(N = 100, G = NULL, tag = "PDD_default"),
  # Compression variants
  list(N = 75,  G = NULL, tag = "N75"),
  list(N = 50,  G = NULL, tag = "N50"),
  list(N = 30,  G = NULL, tag = "N30"),
  # Grid compression (with N=100)
  list(N = 100, G = 20,   tag = "N100_G20"),
  list(N = 100, G = 10,   tag = "N100_G10"),
  # Combined compression
  list(N = 50,  G = 20,   tag = "N50_G20"),
  list(N = 50,  G = 10,   tag = "N50_G10"),
  list(N = 30,  G = 10,   tag = "N30_G10")
)

models_to_test <- c("rf", "logistic", "tree")

all_compressed <- data.frame()

for (model in models_to_test) {
  for (cfg in configs) {
    message(sprintf("\n[%s] Config: N=%s G=%s (%s)",
                    model, cfg$N, ifelse(is.null(cfg$G), "full", cfg$G), cfg$tag))
    res <- tryCatch(
      run_experiment_topk_compressed(
        model_choice = model,
        data         = data,
        target_col   = "class",
        jj           = split_val,
        top_k_values = c(1, 2, 3),
        N_sample     = cfg$N,
        G_grid       = cfg$G
      ),
      error = function(e) {
        message(sprintf("  ERROR: %s", e$message))
        NULL
      }
    )
    if (!is.null(res)) {
      res$config <- cfg$tag
      res$model_type <- model
      all_compressed <- rbind(all_compressed, res)
    }
  }
}

# ---------------------------------------------------------------
# 2) Baseline methods with runtime
# ---------------------------------------------------------------
message("\n========== Baseline Methods Runtime ==========")

run_baseline_timed <- function(ddm_model, rf_initial, model_choice, data_sudden,
                               test_interval, inc_size, target_col) {
  t_start <- Sys.time()
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
    if (length(new_interval) <= 10) next
    set.seed(1234)
    if (model_choice == "logistic") {
      preds_temp <- predict(rf_current, data_sudden[new_interval, ], type = "response")
      preds_temp <- ifelse(preds_temp > 0.50, 1, 0)
    } else {
      preds_temp <- predict(rf_current, data_sudden[new_interval, ], type = "class")
    }
    acc_temp <- mean(preds_temp == data_sudden[new_interval, ][[target_col]], na.rm = TRUE)
    acc_list[[acc_idx]] <- acc_temp
    acc_idx <- acc_idx + 1
    data_stream <- na.omit(1 - (data_sudden[new_interval, ][[target_col]] == preds_temp))
    tryCatch({
      for (i in seq_along(data_stream)) {
        try(ddm$add_element(data_stream[i]), silent = TRUE)
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
          } else {
            rf_current <- rpart(as.formula(paste(target_col, "~ .")), data = data_new_ddm)
          }
          ddm$reset()
          break
        }
      }
    }, error = function(e) message("ddm error: ", e$message))
  }
  t_end <- Sys.time()
  elapsed <- as.numeric(difftime(t_end, t_start, units = "secs"))
  list(mean_accuracy = mean(unlist(acc_list), na.rm = TRUE),
       drift_count = drift_count,
       runtime = round(elapsed, 2))
}

baseline_methods <- c("HDDM_A", "HDDM_W", "KSWIN", "PageHinkley", "DDM", "EDDM")
all_baselines <- data.frame()

for (model in models_to_test) {
  message(sprintf("\n--- Baselines for model: %s ---", model))

  # Train initial model
  batch_sizes <- split_data_simple(data, split_val)
  first_interval <- 1:(batch_sizes$train_set)
  test_interval <- (max(first_interval)):(max(first_interval) + batch_sizes$test_set)
  inc_size <- batch_sizes$test_set

  set.seed(123)
  if (model == "logistic") {
    rf_init <- glm(as.factor(class) ~ ., data = data[first_interval, ], family = "binomial")
  } else if (model == "rf") {
    rf_init <- randomForest(class ~ ., data = data[first_interval, ], ntree = 50)
  } else {
    rf_init <- rpart(class ~ ., data = data[first_interval, ])
  }

  for (bl in baseline_methods) {
    message(sprintf("  %s...", bl))
    res <- tryCatch(
      run_baseline_timed(bl, rf_init, model, data, test_interval, inc_size, "class"),
      error = function(e) { message(e$message); list(mean_accuracy=NA, drift_count=0, runtime=NA) }
    )
    all_baselines <- rbind(all_baselines, data.frame(
      Model = bl,
      Mean_Accuracy = format(res$mean_accuracy, scientific = FALSE),
      Drift_Count = res$drift_count,
      Batch = 10,
      N_sample = NA,
      G_grid = NA,
      Runtime_sec = res$runtime,
      model = model,
      config = bl,
      model_type = model,
      stringsAsFactors = FALSE
    ))
  }
}

# ---------------------------------------------------------------
# 3) Combine and save
# ---------------------------------------------------------------
# Rename columns to match
all_compressed$model_type <- all_compressed$model_type
final_df <- rbind(all_compressed, all_baselines)

write.csv(final_df, "results/tables/compression_bench_full.csv", row.names = FALSE)

# ---------------------------------------------------------------
# 4) Summary table
# ---------------------------------------------------------------
message("\n========== RESULTS ==========\n")

# Reference runtime: PDD_default, PDD_3 (most expensive)
ref_row <- final_df[final_df$config == "PDD_default" & final_df$Model == "PDD_3" &
                    final_df$model_type == "rf", ]
ref_time <- if (nrow(ref_row) > 0) ref_row$Runtime_sec[1] else NA

summary_df <- final_df[, c("model_type", "config", "Model", "Mean_Accuracy",
                           "Drift_Count", "Runtime_sec")]
summary_df$Speedup <- if (!is.na(ref_time)) round(ref_time / summary_df$Runtime_sec, 1) else NA

print(summary_df[order(summary_df$model_type, summary_df$config, summary_df$Model), ])

message(sprintf("\nReference time (PDD_default, PDD_3, RF): %.2fs", ref_time))
message("Results saved to: results/tables/compression_bench_full.csv")

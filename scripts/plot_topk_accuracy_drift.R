suppressPackageStartupMessages({
  library(dplyr)
  library(tidyr)
  library(ggplot2)
})

methods_keep <- c("PDD_C", "PDD_1", "PDD_2", "PDD_3")
model_label  <- c(logistic = "LR", tree = "DT", rf = "RF")

read_one <- function(csv_path, dataset_name) {
  df <- read.csv(csv_path, stringsAsFactors = FALSE)
  df$Mean_Accuracy <- as.numeric(df$Mean_Accuracy)
  df$Drift_Count   <- suppressWarnings(as.integer(df$Drift_Count))
  df$Batch         <- as.integer(df$Batch)
  df <- df[df$Model %in% methods_keep, ]
  df$Method  <- factor(df$Model, levels = methods_keep)
  df$Model_  <- factor(model_label[df$model], levels = c("LR", "DT", "RF"))
  df$Dataset <- dataset_name
  df
}

datasets <- list(
  c("results/tables/Elec2_topk.csv",      "Elec2"),
  c("results/tables/hyperplane_topk.csv", "Hyperplane"),
  c("results/tables/NOAA_topk.csv",       "NOAA"),
  c("results/tables/ozone_topk.csv",      "Ozone"),
  c("results/tables/SEA_topk.csv",        "SEA")
)

all_df <- do.call(rbind, lapply(datasets, function(d) read_one(d[1], d[2])))
all_df$Dataset <- factor(all_df$Dataset, levels = sapply(datasets, `[`, 2))

plot_one_dataset <- function(df_ds, ds_name) {
  # dual axis needs shared scale: map drifts to [0,1] range of accuracy
  max_dr <- max(df_ds$Drift_Count, na.rm = TRUE)
  if (max_dr <= 0) max_dr <- 1
  scale_factor <- 1 / max_dr

  df_long_acc <- df_ds %>%
    select(Method, Model_, Batch, Mean_Accuracy) %>%
    mutate(metric = "Accuracy", value = Mean_Accuracy)

  df_long_dr <- df_ds %>%
    select(Method, Model_, Batch, Drift_Count) %>%
    mutate(metric = "Drifts", value = Drift_Count * scale_factor)

  ggplot() +
    geom_line(data = df_long_acc,
              aes(x = Batch, y = value, color = Method, linetype = "Accuracy"),
              size = 0.8) +
    geom_point(data = df_long_acc,
               aes(x = Batch, y = value, color = Method),
               size = 2) +
    geom_line(data = df_long_dr,
              aes(x = Batch, y = value, color = Method, linetype = "Drifts"),
              size = 0.6, alpha = 0.7) +
    geom_point(data = df_long_dr,
               aes(x = Batch, y = value, color = Method),
               shape = 17, size = 1.8, alpha = 0.7) +
    facet_wrap(~ Model_, nrow = 1) +
    scale_y_continuous(
      name = "Accuracy (solid)",
      limits = c(0, 1),
      sec.axis = sec_axis(~ . / scale_factor, name = "Number of drifts (dashed)")
    ) +
    scale_x_continuous(breaks = c(10, 20, 30)) +
    scale_color_manual(values = c(PDD_C = "#1f77b4", PDD_1 = "#2ca02c",
                                  PDD_2 = "#ff7f0e", PDD_3 = "#d62728")) +
    scale_linetype_manual(values = c(Accuracy = "solid", Drifts = "dashed"),
                          name = "Metric") +
    labs(title = ds_name, x = "Batch configuration") +
    theme_bw(base_size = 11) +
    theme(
      legend.position = "bottom",
      strip.background = element_rect(fill = "grey90", color = NA),
      panel.grid.minor = element_blank(),
      plot.title = element_text(face = "bold")
    )
}

out_dir <- "results/plots/topk_summary"
dir.create(out_dir, showWarnings = FALSE, recursive = TRUE)

# Per-dataset PDFs
for (ds in levels(all_df$Dataset)) {
  p <- plot_one_dataset(all_df %>% filter(Dataset == ds), ds)
  ggsave(file.path(out_dir, paste0(ds, "_acc_drift.pdf")),
         p, width = 10, height = 4.2)
  ggsave(file.path(out_dir, paste0(ds, "_acc_drift.png")),
         p, width = 10, height = 4.2, dpi = 150)
  message("Wrote ", ds)
}

# Combined PDF (one page per dataset)
pdf(file.path(out_dir, "ALL_acc_drift.pdf"), width = 10, height = 4.2)
for (ds in levels(all_df$Dataset)) {
  print(plot_one_dataset(all_df %>% filter(Dataset == ds), ds))
}
invisible(dev.off())
message("Wrote combined PDF: ", file.path(out_dir, "ALL_acc_drift.pdf"))

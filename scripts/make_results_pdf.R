suppressPackageStartupMessages(library(dplyr))

methods_order_cls <- c("HDDM_A", "HDDM_W", "KSWIN", "PageHinkley", "DDM", "EDDM",
                       "PDD_C", "PDD_1", "PDD_2", "PDD_3")
methods_order_reg <- c("KSWIN", "PageHinkley", "PDD_C", "PDD_1", "PDD_2", "PDD_3")
method_label  <- c(HDDM_A = "HDDM_A", HDDM_W = "HDDM_W", KSWIN = "KSWIN",
                   PageHinkley = "PH", DDM = "DDM", EDDM = "EDDM",
                   PDD_C = "**PDD_C**", PDD_1 = "**PDD_1**",
                   PDD_2 = "**PDD_2**", PDD_3 = "**PDD_3**")
model_label   <- c(logistic = "LR", linear = "LR", tree = "DT", rf = "RF")
model_order_cls <- c("logistic", "tree", "rf")
model_order_reg <- c("linear", "tree", "rf")
batches       <- c(10, 20, 30)

fmt_md_table <- function(csv_path, dataset_name,
                         methods_order = methods_order_cls,
                         model_order   = model_order_cls,
                         metric_label  = "Acc") {
  df <- read.csv(csv_path, stringsAsFactors = FALSE)
  df$Mean_Accuracy <- as.numeric(df$Mean_Accuracy)
  df$Drift_Count   <- suppressWarnings(as.integer(df$Drift_Count))
  df$Batch         <- as.integer(df$Batch)
  df <- df[df$Model %in% methods_order & df$model %in% model_order, ]

  lookup <- function(method, mdl, b) {
    row <- df[df$Model == method & df$model == mdl & df$Batch == b, ]
    if (nrow(row) == 0) return(c("--", "--"))
    c(sprintf("%.4f", row$Mean_Accuracy[1]), as.character(row$Drift_Count[1]))
  }

  lines <- c(
    sprintf("## %s", dataset_name),
    "",
    sprintf("| Method | Model | B10 %s | B10 #dr | B20 %s | B20 #dr | B30 %s | B30 #dr |",
            metric_label, metric_label, metric_label),
    "|---|---|---|---|---|---|---|---|"
  )

  for (m in methods_order) {
    for (j in seq_along(model_order)) {
      mdl <- model_order[j]
      vals <- c()
      for (b in batches) vals <- c(vals, lookup(m, mdl, b))
      first_col <- if (j == 1) method_label[[m]] else ""
      lines <- c(lines, sprintf("| %s | %s | %s | %s | %s | %s | %s | %s |",
                                first_col, model_label[[mdl]],
                                vals[1], vals[2], vals[3], vals[4], vals[5], vals[6]))
    }
  }
  lines <- c(lines, "")
  paste(lines, collapse = "\n")
}

# Long-form sweep (one row per Method × Model × Batch, columns per coef)
fmt_md_sweep <- function(csv_path, dataset_name,
                         methods_order = methods_order_reg,
                         model_order   = model_order_reg,
                         metric_label  = "RMSE") {
  df <- read.csv(csv_path, stringsAsFactors = FALSE)
  df$Mean_Accuracy <- as.numeric(df$Mean_Accuracy)
  df$Drift_Count   <- suppressWarnings(as.integer(df$Drift_Count))
  df$Batch         <- as.integer(df$Batch)
  df$l2_coef       <- as.integer(df$l2_coef)
  df <- df[df$Model %in% methods_order & df$model %in% model_order, ]
  coefs <- sort(unique(df$l2_coef))

  lookup <- function(method, mdl, b, coef) {
    row <- df[df$Model == method & df$model == mdl & df$Batch == b & df$l2_coef == coef, ]
    if (nrow(row) == 0) return(c("--", "--"))
    c(sprintf("%.4f", row$Mean_Accuracy[1]), as.character(row$Drift_Count[1]))
  }

  header_top <- paste0("| Method | Model | Batch | ",
                       paste(sprintf("%dx %s | %dx #dr", coefs, metric_label, coefs), collapse = " | "),
                       " |")
  sep_cells <- paste(rep("---", 3 + 2 * length(coefs)), collapse = "|")
  sep_row <- paste0("|", sep_cells, "|")

  lines <- c(
    sprintf("## %s — Threshold Sweep (l2_coef = l2der_coef in {1..5})", dataset_name),
    "",
    header_top,
    sep_row
  )
  for (m in methods_order) {
    first_method <- TRUE
    for (mdl in model_order) {
      first_model <- TRUE
      for (b in batches) {
        cells <- c()
        for (coef in coefs) cells <- c(cells, lookup(m, mdl, b, coef))
        method_col <- if (first_method) method_label[[m]] else ""
        model_col  <- if (first_model)  model_label[[mdl]] else ""
        first_method <- FALSE
        first_model  <- FALSE
        lines <- c(lines,
                   sprintf("| %s | %s | %d | %s |",
                           method_col, model_col, b, paste(cells, collapse = " | ")))
      }
    }
  }
  lines <- c(lines, "")
  paste(lines, collapse = "\n")
}

datasets <- list(
  c("results/tables/Elec2_topk.csv",      "Elec2"),
  c("results/tables/hyperplane_topk.csv", "Hyperplane"),
  c("results/tables/NOAA_topk.csv",       "NOAA"),
  c("results/tables/ozone_topk.csv",      "Ozone"),
  c("results/tables/SEA_topk.csv",        "SEA")
)

header <- c(
  "---",
  "title: \"PDD Top-K Experiment Results\"",
  "author: \"datadriftR DMKD\"",
  sprintf("date: \"%s\"", format(Sys.Date(), "%Y-%m-%d")),
  "geometry: margin=2cm",
  "fontsize: 9pt",
  "---",
  "",
  "Comparison of drift detection methods across five datasets. Each method is run with three models (LR = logistic regression, DT = decision tree, RF = random forest) and three batch configurations (B10 / B20 / B30). **PDD_C** replicates the original single-variable PDD; **PDD_1/2/3** monitor the top-k important variables dynamically.",
  ""
)

out <- header

# ---------- Consolidated test/train accuracy table (article Table tab:acc_batch) ----------
fmt_acc_batch <- function(dataset_specs) {
  lines <- c(
    "## Test/Train Accuracy per Batch (article Table acc\\_batch equivalent)",
    "",
    "Initial-fit accuracies extracted from `Base Test` / `Base Train` rows in each dataset CSV.",
    "",
    "| Dataset | Model | B10 test | B10 train | B20 test | B20 train | B30 test | B30 train |",
    "|---|---|---|---|---|---|---|---|"
  )
  for (spec in dataset_specs) {
    csv_path <- spec$csv
    name     <- spec$name
    mord     <- spec$model_order
    if (!file.exists(csv_path)) next
    df <- read.csv(csv_path, stringsAsFactors = FALSE)
    df$Mean_Accuracy <- as.numeric(df$Mean_Accuracy)
    df$Batch <- as.integer(df$Batch)
    first_row <- TRUE
    for (mdl in mord) {
      get <- function(method, b) {
        row <- df[df$Model == method & df$model == mdl & df$Batch == b, ]
        if (nrow(row) == 0) return("--")
        sprintf("%.4f", row$Mean_Accuracy[1])
      }
      vals <- c()
      for (b in batches) vals <- c(vals, get("Base Test", b), get("Base Train", b))
      ds_col <- if (first_row) name else ""
      first_row <- FALSE
      lines <- c(lines, sprintf("| %s | %s | %s | %s | %s | %s | %s | %s |",
                                ds_col, model_label[[mdl]],
                                vals[1], vals[2], vals[3], vals[4], vals[5], vals[6]))
    }
  }
  lines <- c(lines, "")
  paste(lines, collapse = "\n")
}

acc_batch_specs <- list(
  list(csv = "results/tables/SEA_topk.csv",        name = "SEA",        model_order = model_order_cls),
  list(csv = "results/tables/hyperplane_topk.csv", name = "Hyperplane", model_order = model_order_cls),
  list(csv = "results/tables/NOAA_topk.csv",       name = "NOAA",       model_order = model_order_cls),
  list(csv = "results/tables/ozone_topk.csv",      name = "Ozone",      model_order = model_order_cls),
  list(csv = "results/tables/Elec2_topk.csv",      name = "Elec2",      model_order = model_order_cls),
  list(csv = "results/tables/Friedman_topk.csv",   name = "Friedman (RMSE)", model_order = model_order_reg)
)
out <- c(out, fmt_acc_batch(acc_batch_specs))

for (d in datasets) {
  if (!file.exists(d[1])) next
  out <- c(out, fmt_md_table(d[1], d[2]))
}

friedman_csv <- "results/tables/Friedman_topk.csv"
if (file.exists(friedman_csv)) {
  out <- c(out, fmt_md_table(friedman_csv, "Friedman (5x L2/L2Der)",
                             methods_order = methods_order_reg,
                             model_order   = model_order_reg,
                             metric_label  = "RMSE"))
}

friedman_sweep <- "results/tables/Friedman_topk_thr_sweep.csv"
if (file.exists(friedman_sweep)) {
  out <- c(out, fmt_md_sweep(friedman_sweep, "Friedman"))
}

# ---------- Robust (regularized) tables for NOAA & Ozone ----------
# Side-by-side comparison highlighting reviewer #1.5 (PDD reliability under overfit).
fmt_robust_compare <- function(orig_path, robust_path, dataset_name) {
  o <- read.csv(orig_path, stringsAsFactors = FALSE)
  r <- read.csv(robust_path, stringsAsFactors = FALSE)
  o$Mean_Accuracy <- as.numeric(o$Mean_Accuracy)
  r$Mean_Accuracy <- as.numeric(r$Mean_Accuracy)
  o$Drift_Count   <- suppressWarnings(as.integer(o$Drift_Count))
  r$Drift_Count   <- suppressWarnings(as.integer(r$Drift_Count))

  # train_acc / gap from robust file
  gaps <- unique(r[r$Model == "Base Train" & r$model %in% c("rf","tree","logistic"),
                   c("model","Batch","train_acc","overfit_gap")])
  gaps$train_acc   <- as.numeric(gaps$train_acc)
  gaps$overfit_gap <- as.numeric(gaps$overfit_gap)

  # Original train acc for comparison
  o_train <- o[o$Model == "Base Train", c("model","Batch","Mean_Accuracy")]
  colnames(o_train)[3] <- "orig_train_acc"

  lookup <- function(df, method, mdl, b) {
    row <- df[df$Model == method & df$model == mdl & df$Batch == b, ]
    if (nrow(row) == 0) return(c("--", "--"))
    c(sprintf("%.4f", row$Mean_Accuracy[1]), as.character(row$Drift_Count[1]))
  }

  lines <- c(
    sprintf("## %s — Robust (regularize=strong) vs Original", dataset_name),
    "",
    "Regularization closes the train-test overfit gap and restores PDD drift sensitivity. Reviewer #1.5 specifically flagged NOAA RF / Batch 20 where original PDD fired 0 drifts despite accuracy collapse — that silence is broken under the regularized model.",
    "",
    "### Train accuracy / overfit gap (Base Train comparison)",
    "",
    "| Model | Batch | Orig train acc | Robust train acc | Robust gap (train-test) |",
    "|---|---|---|---|---|"
  )

  for (mdl in c("rf","tree","logistic")) {
    for (b in c(10,20,30)) {
      ot <- o_train[o_train$model == mdl & o_train$Batch == b, ]
      gp <- gaps[gaps$model == mdl & gaps$Batch == b, ]
      if (nrow(ot) == 0 || nrow(gp) == 0) next
      lines <- c(lines, sprintf("| %s | %d | %.4f | %.4f | %.4f |",
                                model_label[[mdl]], b,
                                ot$orig_train_acc[1], gp$train_acc[1], gp$overfit_gap[1]))
    }
  }

  lines <- c(lines, "",
             "### Drift counts and accuracy (orig → robust)",
             "",
             "| Method | Model | B10 orig | B10 robust | B20 orig | B20 robust | B30 orig | B30 robust |",
             "|---|---|---|---|---|---|---|---|")

  for (m in methods_order_cls) {
    for (mdl in c("rf","tree","logistic")) {
      first_col <- if (mdl == "rf") method_label[[m]] else ""
      cells <- c()
      for (b in c(10,20,30)) {
        oa <- lookup(o, m, mdl, b)
        rg <- lookup(r, m, mdl, b)
        cells <- c(cells,
                   sprintf("%s/%s", oa[1], oa[2]),
                   sprintf("%s/%s", rg[1], rg[2]))
      }
      lines <- c(lines, sprintf("| %s | %s | %s |",
                                first_col, model_label[[mdl]],
                                paste(cells, collapse = " | ")))
    }
  }
  lines <- c(lines, "")
  paste(lines, collapse = "\n")
}

noaa_robust  <- "results/tables/NOAA_topk_robust.csv"
ozone_robust <- "results/tables/ozone_topk_robust.csv"
if (file.exists(noaa_robust)) {
  out <- c(out, fmt_robust_compare("results/tables/NOAA_topk.csv",
                                   noaa_robust, "NOAA"))
}
if (file.exists(ozone_robust)) {
  out <- c(out, fmt_robust_compare("results/tables/ozone_topk.csv",
                                   ozone_robust, "Ozone"))
}

writeLines(out, "results/tables/results_topk.md")
cat("Wrote results/tables/results_topk.md\n")

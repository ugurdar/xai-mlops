suppressPackageStartupMessages(library(dplyr))

methods_order_cls <- c("HDDM_A", "HDDM_W", "KSWIN", "PageHinkley", "DDM", "EDDM",
                       "PDD_C", "PDD_1", "PDD_2", "PDD_3")
methods_order_reg <- c("KSWIN", "PageHinkley", "PDD_C", "PDD_1", "PDD_2", "PDD_3")
method_label  <- c(HDDM_A = "HDDM\\_A", HDDM_W = "HDDM\\_W", KSWIN = "KSWIN",
                   PageHinkley = "PH", DDM = "DDM", EDDM = "EDDM",
                   PDD_C = "PDD\\_C", PDD_1 = "PDD\\_1",
                   PDD_2 = "PDD\\_2", PDD_3 = "PDD\\_3")
model_label   <- c(logistic = "LR", linear = "LR", tree = "DT", rf = "RF")
model_order_cls <- c("logistic", "tree", "rf")
model_order_reg <- c("linear", "tree", "rf")
batches       <- c(10, 20, 30)

make_table <- function(csv_path, dataset_tex, label_tag,
                       methods_order = methods_order_cls,
                       model_order   = model_order_cls,
                       metric_label  = "Acc.") {
  df <- read.csv(csv_path, stringsAsFactors = FALSE)
  df$Mean_Accuracy <- as.numeric(df$Mean_Accuracy)
  df$Drift_Count   <- suppressWarnings(as.integer(df$Drift_Count))
  df$Batch         <- as.integer(df$Batch)
  df <- df[df$Model %in% methods_order & df$model %in% model_order, ]

  fmt <- function(method, mdl, b) {
    row <- df[df$Model == method & df$model == mdl & df$Batch == b, ]
    if (nrow(row) == 0) return(c("--", "--"))
    c(sprintf("%.4f", row$Mean_Accuracy[1]), as.character(row$Drift_Count[1]))
  }

  lines <- c(
    "\\begin{table}[h]",
    "    \\centering",
    "    \\scriptsize",
    sprintf("    \\caption{The results of experiments on the \\texttt{%s} dataset}", dataset_tex),
    sprintf("    \\label{tab:exp_%s}", label_tag),
    "    \\begin{tabular}{llcccccc}\\toprule",
    "         & & \\multicolumn{2}{c}{Batch 10} & \\multicolumn{2}{c}{Batch 20} & \\multicolumn{2}{c}{Batch 30} \\\\\\cmidrule(lr){3-4}\\cmidrule(lr){5-6}\\cmidrule(lr){7-8}",
    sprintf("         \\textbf{Method} & \\textbf{Model} & \\textbf{%s} & \\textbf{\\#drifts} & \\textbf{%s} & \\textbf{\\#drifts} & \\textbf{%s} & \\textbf{\\#drifts} \\\\\\midrule",
            metric_label, metric_label, metric_label)
  )

  n_methods <- length(methods_order)
  for (i in seq_along(methods_order)) {
    m <- methods_order[i]
    lines <- c(lines, sprintf("    %% %s", m))
    for (j in seq_along(model_order)) {
      mdl <- model_order[j]
      row_vals <- c()
      for (b in batches) row_vals <- c(row_vals, fmt(m, mdl, b))
      first_col <- if (j == 1) method_label[[m]] else ""
      lines <- c(lines, sprintf("    %-8s & %s & %s & %s & %s & %s & %s & %s \\\\",
                                first_col, model_label[[mdl]],
                                row_vals[1], row_vals[2],
                                row_vals[3], row_vals[4],
                                row_vals[5], row_vals[6]))
    }
    if (i < n_methods) lines <- c(lines, "    \\midrule")
  }

  lines <- c(lines,
             "    \\bottomrule",
             "    \\end{tabular}",
             "\\end{table}")
  paste(lines, collapse = "\n")
}

# Threshold-sweep companion table (long format: rows = method × model × batch × coef)
make_thr_sweep_table <- function(csv_path, dataset_tex, label_tag,
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
  fmt <- function(method, mdl, b, coef) {
    row <- df[df$Model == method & df$model == mdl & df$Batch == b & df$l2_coef == coef, ]
    if (nrow(row) == 0) return(c("--", "--"))
    c(sprintf("%.4f", row$Mean_Accuracy[1]), as.character(row$Drift_Count[1]))
  }

  header_main <- paste0("        & & & ",
                        paste(sprintf("\\multicolumn{2}{c}{$%dx$}", coefs), collapse = " & "),
                        " \\\\")
  cmid_parts <- sapply(seq_along(coefs), function(i) {
    start <- 4 + (i - 1) * 2
    sprintf("\\cmidrule(lr){%d-%d}", start, start + 1)
  })
  cmid_line <- paste0("        ", paste(cmid_parts, collapse = ""))
  header_sub <- paste0("        \\textbf{Method} & \\textbf{Model} & \\textbf{Batch} & ",
                       paste(rep(sprintf("\\textbf{%s} & \\textbf{\\#d}", metric_label), length(coefs)),
                             collapse = " & "),
                       " \\\\\\midrule")
  col_spec <- paste0("ll l", paste(rep("cc", length(coefs)), collapse = ""))

  lines <- c(
    "\\begin{table}[h]",
    "    \\centering",
    "    \\scriptsize",
    sprintf("    \\caption{Threshold sweep ($l2\\_coef = l2der\\_coef \\in \\{1,\\ldots,5\\}$) on the \\texttt{%s} dataset}", dataset_tex),
    sprintf("    \\label{tab:thr_sweep_%s}", label_tag),
    sprintf("    \\begin{tabular}{%s}\\toprule", col_spec),
    header_main,
    cmid_line,
    header_sub
  )

  for (m in methods_order) {
    lines <- c(lines, sprintf("    %% %s", m))
    first_method <- TRUE
    for (mdl in model_order) {
      first_model <- TRUE
      for (b in batches) {
        row_vals <- c()
        for (coef in coefs) row_vals <- c(row_vals, fmt(m, mdl, b, coef))
        method_col <- if (first_method) method_label[[m]] else ""
        model_col  <- if (first_model)  model_label[[mdl]] else ""
        first_method <- FALSE
        first_model  <- FALSE
        cells <- paste(row_vals, collapse = " & ")
        lines <- c(lines, sprintf("    %s & %s & %d & %s \\\\",
                                  method_col, model_col, b, cells))
      }
    }
    lines <- c(lines, "    \\midrule")
  }
  lines[length(lines)] <- "    \\bottomrule"  # replace trailing midrule
  lines <- c(lines, "    \\end{tabular}", "\\end{table}")
  paste(lines, collapse = "\n")
}

datasets <- list(
  list(csv = "results/tables/Elec2_topk.csv",      tex = "Elec2",       label = "Elec2"),
  list(csv = "results/tables/hyperplane_topk.csv", tex = "Hyperplane",  label = "Hyperplane"),
  list(csv = "results/tables/NOAA_topk.csv",       tex = "NOAA",        label = "NOAA"),
  list(csv = "results/tables/ozone_topk.csv",      tex = "Ozone",       label = "Ozone"),
  list(csv = "results/tables/SEA_topk.csv",        tex = "SEA",         label = "SEA")
)

out <- character(0)
for (d in datasets) {
  if (!file.exists(d$csv)) next
  out <- c(out, make_table(d$csv, d$tex, d$label), "")
}

# Friedman top-k (regression metric: RMSE)
friedman_csv  <- "results/tables/Friedman_topk.csv"
if (file.exists(friedman_csv)) {
  out <- c(out, make_table(friedman_csv, "Friedman", "Friedman_topk",
                           methods_order = methods_order_reg,
                           model_order   = model_order_reg,
                           metric_label  = "RMSE"), "")
}

# Friedman threshold sweep (1x..5x)
friedman_sweep <- "results/tables/Friedman_topk_thr_sweep.csv"
if (file.exists(friedman_sweep)) {
  out <- c(out, make_thr_sweep_table(friedman_sweep, "Friedman", "Friedman"), "")
}

# ----------------------------------------------------------------------
# tab:acc_batch — initial test/train accuracy per (dataset, model, batch)
# Replicates the table layout in the article verbatim.
# ----------------------------------------------------------------------
make_acc_batch_latex <- function(specs) {
  hdr <- c(
    "\\begin{table}[H]",
    "    \\centering",
    "    \\scriptsize",
    "    \\caption{The accuracies of the models in the batches}",
    "    \\label{tab:acc_batch}",
    "    \\resizebox{\\linewidth}{!}{",
    "    \\begin{tabular}{llcccccc}\\toprule",
    "    & & \\multicolumn{6}{c}{\\textbf{\\#batch}}\\\\",
    "    \\cmidrule(lr){3-8}",
    "    & & \\multicolumn{2}{c}{\\textbf{10}} & \\multicolumn{2}{c}{\\textbf{20}} & \\multicolumn{2}{c}{\\textbf{30}} \\\\",
    "    \\cmidrule(lr){3-8}",
    "    \\textbf{Dataset} & \\textbf{Model} & \\textbf{test} & \\textbf{train} & \\textbf{test} & \\textbf{train} & \\textbf{test} & \\textbf{train} \\\\",
    "    \\midrule"
  )
  body <- character(0)
  for (i in seq_along(specs)) {
    spec <- specs[[i]]
    csv_path <- spec$csv
    if (!file.exists(csv_path)) next
    df <- read.csv(csv_path, stringsAsFactors = FALSE)
    df$Mean_Accuracy <- as.numeric(df$Mean_Accuracy)
    df$Batch <- as.integer(df$Batch)
    first <- TRUE
    for (mdl in spec$model_order) {
      vals <- c()
      for (b in c(10, 20, 30)) {
        get <- function(method) {
          row <- df[df$Model == method & df$model == mdl & df$Batch == b, ]
          if (nrow(row) == 0) return("--")
          sprintf("%.4f", row$Mean_Accuracy[1])
        }
        vals <- c(vals, get("Base Test"), get("Base Train"))
      }
      ds_col <- if (first) sprintf("\\texttt{%s}", spec$tex) else ""
      mdl_col <- spec$model_label_map[[mdl]]
      first <- FALSE
      body <- c(body,
                sprintf("    %s & %s & %s & %s & %s & %s & %s & %s \\\\",
                        ds_col, mdl_col,
                        vals[1], vals[2], vals[3], vals[4], vals[5], vals[6]))
    }
    if (i < length(specs)) body <- c(body, "    \\midrule")
  }
  ftr <- c("    \\bottomrule",
           "    \\end{tabular}}",
           "\\end{table}")
  paste(c(hdr, body, ftr), collapse = "\n")
}

# Article order: SEA, Hyperplane, NOAA, Ozone, Elec2, Friedman.
# Article order within dataset: RF, LR, DT (Friedman uses "linear" for LR slot).
acc_batch_specs <- list(
  list(csv = "results/tables/SEA_topk.csv",        tex = "SEA",
       model_order = c("rf", "logistic", "tree"),
       model_label_map = list(rf = "RF", logistic = "LR", tree = "DT")),
  list(csv = "results/tables/hyperplane_topk.csv", tex = "Hyperplane",
       model_order = c("rf", "logistic", "tree"),
       model_label_map = list(rf = "RF", logistic = "LR", tree = "DT")),
  list(csv = "results/tables/NOAA_topk.csv",       tex = "NOAA",
       model_order = c("rf", "logistic", "tree"),
       model_label_map = list(rf = "RF", logistic = "LR", tree = "DT")),
  list(csv = "results/tables/ozone_topk.csv",      tex = "Ozone",
       model_order = c("rf", "logistic", "tree"),
       model_label_map = list(rf = "RF", logistic = "LR", tree = "DT")),
  list(csv = "results/tables/Elec2_topk.csv",      tex = "Elec2",
       model_order = c("rf", "logistic", "tree"),
       model_label_map = list(rf = "RF", logistic = "LR", tree = "DT")),
  list(csv = "results/tables/Friedman_topk.csv",   tex = "Friedman",
       model_order = c("rf", "linear", "tree"),
       model_label_map = list(rf = "RF", linear = "linear", tree = "DT"))
)
acc_batch_tex <- make_acc_batch_latex(acc_batch_specs)
out <- c(out, acc_batch_tex, "")
writeLines(acc_batch_tex, "results/tables/acc_batch.tex")

# ----------------------------------------------------------------------
# Per-dataset method table — model-grouped (\multirow), Method second column.
# Format requested by user — matches article tab:exp_<dataset> style but
# updated with PDD_C/PDD_1/PDD_2/PDD_3 (10 methods total instead of 7).
# ----------------------------------------------------------------------
method_label_dash <- c(HDDM_A = "HDDM-A", HDDM_W = "HDDM-W", KSWIN = "KSWIN",
                       PageHinkley = "PH", DDM = "DDM", EDDM = "EDDM",
                       PDD_C = "PDD\\_C", PDD_1 = "PDD\\_1",
                       PDD_2 = "PDD\\_2", PDD_3 = "PDD\\_3")
methods_v2_order <- c("HDDM_A", "HDDM_W", "KSWIN", "PageHinkley",
                      "DDM", "EDDM", "PDD_C", "PDD_1", "PDD_2", "PDD_3")

make_per_method_v2 <- function(csv_path, dataset_tex, label_tag,
                               model_order   = c("logistic", "tree", "rf"),
                               methods_order = methods_v2_order,
                               metric_label  = "Acc.") {
  df <- read.csv(csv_path, stringsAsFactors = FALSE)
  df$Mean_Accuracy <- as.numeric(df$Mean_Accuracy)
  df$Drift_Count   <- suppressWarnings(as.integer(df$Drift_Count))
  df$Batch         <- as.integer(df$Batch)

  fmt <- function(method, mdl, b) {
    row <- df[df$Model == method & df$model == mdl & df$Batch == b, ]
    if (nrow(row) == 0) return(c("--", "--"))
    c(sprintf("%.4f", row$Mean_Accuracy[1]), as.character(row$Drift_Count[1]))
  }

  n_methods <- length(methods_order)
  lines <- c(
    "\\begin{table}[h]",
    "    \\centering",
    "    \\scriptsize",
    sprintf("    \\caption{The results of experiments on the \\texttt{%s} dataset}", dataset_tex),
    sprintf("    \\label{tab:exp_%s}", label_tag),
    "    \\resizebox{\\linewidth}{!}{",
    "    \\color{red}",
    "    \\begin{tabular}{llcrcrcr}\\toprule",
    "        & & \\multicolumn{6}{c}{\\textbf{\\#batch}} \\\\",
    "        \\cmidrule(lr){3-8}",
    "        & & \\multicolumn{2}{c}{\\textbf{10}} & \\multicolumn{2}{c}{\\textbf{20}} & \\multicolumn{2}{c}{\\textbf{30}} \\\\",
    "        \\cmidrule(lr){3-4} \\cmidrule(lr){5-6} \\cmidrule(lr){7-8}",
    sprintf("        \\textbf{Model} & \\textbf{Method} & {\\textbf{%s}} & \\textbf{\\#drifts} & {\\textbf{%s}} & \\textbf{\\#drifts} & {\\textbf{%s}} & \\textbf{\\#drifts} \\\\ \\midrule",
            metric_label, metric_label, metric_label)
  )

  for (mi in seq_along(model_order)) {
    mdl <- model_order[mi]
    for (i in seq_along(methods_order)) {
      m <- methods_order[i]
      first_col <- if (i == 1) sprintf("\\multirow{%d}{*}{%s}", n_methods, model_label[[mdl]]) else ""
      cells <- c()
      for (b in batches) cells <- c(cells, fmt(m, mdl, b))
      sep <- if (i == n_methods) " \\\\ \\midrule" else " \\\\"
      lines <- c(lines, sprintf("        %s & %s & %s & %s & %s & %s & %s & %s%s",
                                first_col, method_label_dash[[m]],
                                cells[1], cells[2], cells[3], cells[4],
                                cells[5], cells[6], sep))
    }
  }
  # replace last "\\ \midrule" with "\\\bottomrule"
  lines[length(lines)] <- sub("\\\\\\\\ \\\\midrule$", "\\\\\\\\\\\\bottomrule", lines[length(lines)])
  lines <- c(lines, "    \\end{tabular}}", "\\end{table}")
  paste(lines, collapse = "\n")
}

# Generate per-method-v2 tables for all datasets (write each to its own file).
v2_specs <- list(
  list(csv = "results/tables/SEA_topk.csv",        tex = "SEA",        label = "SEA",
       model_order = c("logistic","tree","rf")),
  list(csv = "results/tables/hyperplane_topk.csv", tex = "Hyperplane", label = "Hyperplane",
       model_order = c("logistic","tree","rf")),
  list(csv = "results/tables/NOAA_topk.csv",       tex = "NOAA",       label = "NOAA",
       model_order = c("logistic","tree","rf")),
  list(csv = "results/tables/ozone_topk.csv",      tex = "Ozone",      label = "Ozone",
       model_order = c("logistic","tree","rf")),
  list(csv = "results/tables/Elec2_topk.csv",      tex = "Elec2",      label = "Elec2",
       model_order = c("logistic","tree","rf"))
)
for (sp in v2_specs) {
  if (!file.exists(sp$csv)) next
  tex <- make_per_method_v2(sp$csv, sp$tex, sp$label, sp$model_order)
  fname <- sprintf("results/tables/exp_%s_v2.tex", sp$label)
  writeLines(tex, fname)
}

# Friedman main table — pull coef=1 slice from the sweep CSV so numbers match
# the article's tab:exp_Friedman exactly (PDD_C/LR/B10 = 2.8332/3, etc.).
# The 5x version stays available in Friedman_topk.csv as a separate artifact.
methods_v2_reg <- c("KSWIN", "PageHinkley", "PDD_C", "PDD_1", "PDD_2", "PDD_3")
if (file.exists("results/tables/Friedman_topk_thr_sweep.csv")) {
  sweep <- read.csv("results/tables/Friedman_topk_thr_sweep.csv", stringsAsFactors = FALSE)
  sweep$l2_coef <- as.integer(sweep$l2_coef)
  coef1 <- sweep[sweep$l2_coef == 1, ]
  # Also include rows that are coef-independent (Base Test, Base Train, batch sizes)
  # so make_per_method_v2 still finds them if it ever does. We don't need them here.
  coef1_path <- "results/tables/Friedman_topk_coef1.csv"
  write.csv(coef1, coef1_path, row.names = FALSE)
  tex <- make_per_method_v2(coef1_path,
                            "Friedman", "Friedman_topk",
                            model_order   = c("linear","tree","rf"),
                            methods_order = methods_v2_reg,
                            metric_label  = "RMSE")
  writeLines(tex, "results/tables/exp_Friedman_v2.tex")
}

# Friedman threshold sweep (1x..5x) — multirow on Model, Method second col,
# rows enumerate Batch × Coef-block. Output is one table per Batch to keep
# width sane (5 coef columns each with RMSE + #drifts = 10 sub-cols + 2 label cols).
make_friedman_sweep_v2 <- function(csv_path, dataset_tex = "Friedman",
                                   model_order   = c("linear","tree","rf"),
                                   methods_order = methods_v2_reg) {
  df <- read.csv(csv_path, stringsAsFactors = FALSE)
  df$Mean_Accuracy <- as.numeric(df$Mean_Accuracy)
  df$Drift_Count   <- suppressWarnings(as.integer(df$Drift_Count))
  df$Batch         <- as.integer(df$Batch)
  df$l2_coef       <- as.integer(df$l2_coef)
  coefs <- sort(unique(df$l2_coef))
  batches_local <- sort(unique(df$Batch))

  fmt <- function(method, mdl, b, coef) {
    row <- df[df$Model == method & df$model == mdl & df$Batch == b & df$l2_coef == coef, ]
    if (nrow(row) == 0) return(c("--", "--"))
    c(sprintf("%.4f", row$Mean_Accuracy[1]), as.character(row$Drift_Count[1]))
  }

  out_lines <- character(0)
  for (b in batches_local) {
    n_methods <- length(methods_order)
    col_spec <- paste0("ll", paste(rep("cr", length(coefs)), collapse = ""))
    header_top <- paste0("        & & \\multicolumn{", 2 * length(coefs),
                         "}{c}{\\textbf{threshold relaxation}} \\\\")
    cmid_top <- sprintf("        \\cmidrule(lr){3-%d}", 2 + 2 * length(coefs))
    coef_cols <- paste(sprintf("\\multicolumn{2}{c}{\\textbf{$%dx$}}", coefs), collapse = " & ")
    cmid_parts <- sapply(seq_along(coefs), function(i) {
      s <- 3 + (i - 1) * 2
      sprintf("\\cmidrule(lr){%d-%d}", s, s + 1)
    })
    cmid_line <- paste0("        ", paste(cmid_parts, collapse = " "))
    header_metric <- paste0("        \\textbf{Model} & \\textbf{Method} & ",
                            paste(rep("{\\textbf{RMSE}} & \\textbf{\\#d}", length(coefs)),
                                  collapse = " & "), " \\\\ \\midrule")
    lines <- c(
      "\\begin{table}[h]",
      "    \\centering",
      "    \\scriptsize",
      sprintf("    \\caption{Threshold relaxation sweep on \\texttt{%s} dataset, batch %d}",
              dataset_tex, b),
      sprintf("    \\label{tab:thr_sweep_%s_b%d}", dataset_tex, b),
      "    \\resizebox{\\linewidth}{!}{",
      "    \\color{red}",
      sprintf("    \\begin{tabular}{%s}\\toprule", col_spec),
      header_top,
      cmid_top,
      paste0("        & & ", coef_cols, " \\\\"),
      cmid_line,
      header_metric
    )
    for (mi in seq_along(model_order)) {
      mdl <- model_order[mi]
      for (i in seq_along(methods_order)) {
        m <- methods_order[i]
        first_col <- if (i == 1) sprintf("\\multirow{%d}{*}{%s}", n_methods, model_label[[mdl]]) else ""
        cells <- c()
        for (coef in coefs) cells <- c(cells, fmt(m, mdl, b, coef))
        cells_str <- paste(cells, collapse = " & ")
        sep <- if (i == n_methods) " \\\\ \\midrule" else " \\\\"
        lines <- c(lines, sprintf("        %s & %s & %s%s",
                                  first_col, method_label_dash[[m]], cells_str, sep))
      }
    }
    lines[length(lines)] <- sub("\\\\\\\\ \\\\midrule$", "\\\\\\\\\\\\bottomrule",
                                lines[length(lines)])
    lines <- c(lines, "    \\end{tabular}}", "\\end{table}", "")
    out_lines <- c(out_lines, lines)
  }
  paste(out_lines, collapse = "\n")
}

if (file.exists("results/tables/Friedman_topk_thr_sweep.csv")) {
  tex <- make_friedman_sweep_v2("results/tables/Friedman_topk_thr_sweep.csv")
  writeLines(tex, "results/tables/exp_Friedman_sweep_v2.tex")
}

writeLines(out, "results/tables/latex_tables_topk.tex")
cat(paste(out, collapse = "\n"))

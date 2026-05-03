#!/usr/bin/env Rscript
# reproduce.R — Master script to reproduce all PDD_k experiments
#
# Usage:
#   Rscript reproduce.R   # run all experiments

dir.create("results/tables", recursive = TRUE, showWarnings = FALSE)
dir.create("results/plots",  recursive = TRUE, showWarnings = FALSE)

# Phase 1: Classification experiments (5 datasets)
message("\n====== Phase 1/3: Classification experiments ======\n")
source("scripts/run_all_topk.R")

# Phase 2: Friedman regression + threshold sweep
message("\n====== Phase 2/3: Friedman experiments ======\n")
source("scripts/run_friedman_topk.R")

# Phase 3: Compression benchmark
message("\n====== Phase 3/3: Compression benchmark ======\n")
source("scripts/run_compression_bench_full.R")

message("\nAll done. Results in results/tables/ and results/plots/")

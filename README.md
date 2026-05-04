This repository contains the reproducible materials for the paper "From XAI to MLOps: Explainable Concept Drift Detection with the Partial Dependence Profiles-Based Approach".

R implementation of the PDD_k method for concept drift detection using partial dependence profiles. PDD_k monitors the top-k most important features via PDP-based L2 and L2-derivative statistics, providing interpretable drift detection for streaming data.

## Repository Structure

```
├── scripts/
│   ├── 2_utils.R                    # Core PDD_k functions and helpers
│   ├── exp_topk.R                   # Classification experiment runner
│   ├── exp_friedman_topk.R          # Regression experiment runner (Friedman)
│   ├── exp_topk_compressed.R        # Compressed PDD variant runner
│   ├── run_all_topk.R               # Driver: 5 classification datasets
│   ├── run_friedman_topk.R          # Driver: Friedman + threshold sweep
│   └── run_compression_bench_full.R # Driver: compression benchmark
├── data/                            # Benchmark datasets (~10 MB)
├── results/
│   ├── tables/                      # CSV results
│   └── plots/                       # Per-dataset accuracy/drift plots
├── reproduce.R                      # Master script (runs everything)
├── Dockerfile                       # Reproducible container build
├── renv.lock                        # R package versions
└── README.md
```

## Quick Start

### Docker (recommended)

```bash
docker build -t pdd-experiments .

# Full reproduction (all experiments — several hours)
docker run -v $(pwd)/results:/app/results pdd-experiments
```

### Local R

Requires R >= 4.4.1.

```bash
# Restore packages
Rscript -e "renv::restore()"

# Full run
Rscript reproduce.R
```

## Experiments

| Dataset | Type | Instances | Script | Models |
|---------|------|-----------|--------|--------|
| Elec2 | Classification | 45,312 | `run_all_topk.R` | RF, LR, DT |
| Hyperplane | Classification | 10,000 | `run_all_topk.R` | RF, LR, DT |
| NOAA | Classification | 18,159 | `run_all_topk.R` | RF, LR, DT |
| Ozone | Classification | 2,534 | `run_all_topk.R` | RF, LR, DT |
| SEA | Classification | 50,000 | `run_all_topk.R` | RF, LR, DT |
| Friedman | Regression | 10,000 | `run_friedman_topk.R` | RF, Linear, DT |

Additional experiments:
- **Threshold sensitivity sweep** (Friedman, coef 1-5): `run_friedman_topk.R`
- **Compression benchmark** (Elec2, N/G grid): `run_compression_bench_full.R`

Each dataset is tested with batch sizes 10, 20, and 30 and compared against baseline drift detectors (HDDM-A, HDDM-W, KSWIN, PageHinkley, DDM, EDDM).

## Key Dependencies

- `randomForest`, `rpart` — learners
- `DALEX` — partial dependence profiles
- `frbs` — ARFF reader
- `dplyr`, `ggplot2` — data wrangling and plots
- `caret` — resampling utilities

All versions are pinned in `renv.lock`.

## Outputs

- `results/tables/*_topk.csv` — per-dataset accuracy and drift count results
- `results/tables/*_drift_log.csv` — per-batch drift detection logs
- `results/tables/compression_bench_full.csv` — compression benchmark
- `results/plots/*_topk/` — accuracy and drift visualizations

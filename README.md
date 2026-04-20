# The Inner Beauty of Firms

**Replication code for [*The Inner Beauty of Firms*](https://www.jkohlhepp.com/files/kohlhepp_jmp.pdf)**
by Jake Kohlhepp

---

## Executive Summary

How do firms organize workers internally, and why does it matter? This paper develops and estimates an **entropy-regularized assignment model** of worker specialization within firms. Using high-frequency transaction data from hairdressing salons across three U.S. markets (Cook County IL, New York County NY, and Los Angeles County CA, 2018--2021), the model captures how firms allocate heterogeneous workers across tasks based on comparative advantage.

The key insight is that **within-firm specialization**---measured by the entropy of the worker-task assignment matrix---is a first-order determinant of firm productivity and pricing. The estimation strategy combines:

1. **Demand-side IV estimation** of price sensitivity, skill requirements, and material costs.
2. **Supply-side GMM** that matches observed worker-type distributions to those predicted by the assignment equilibrium, using fixed-point iteration (SQUAREM) and bisection over the entropy-regularization parameter.

## Repository Structure

```
refactor_estimation/
|
|-- config.R                           # Centralized configuration (model dimensions, tolerances, paths)
|-- preamble.R                         # Core model functions (equilibrium solver, GMM objective)
|
|-- Data Preparation
|   |-- run_prep_data.R                # Orchestrates all prep scripts
|   |-- prep_00_compile_transactions.R # Merge raw transaction pulls
|   |-- prep_01_cosmo_classify.R       # Classify services into 5 task types
|   |-- prep_02_censuspop.R            # Census population controls
|   |-- prep_03_county_msa_xwalk.R     # County-to-MSA crosswalk
|   |-- prep_04_consumerexpenditure.R  # Outside option from Consumer Expenditure Survey
|   |-- prep_05_ppi.R                  # Producer Price Index (material cost instrument)
|   |-- prep_06_qcew.R                 # QCEW county-level wage data
|
|-- Main Pipeline
|   |-- run_all.R                      # Master script: runs full pipeline end-to-end
|   |-- 00_mk_tasks_cosmo.R           # Aggregate transactions to task-worker-firm-quarter level
|   |-- 01_build_data.R               # Hierarchical clustering + skill matrix construction
|   |-- 02_estimation.R               # Two-stage GMM estimation
|   |-- cluster.R                      # Clustering helper functions
|
|-- Analysis
|   |-- 01_01_stylized_facts.R         # Descriptive tables and figures
|   |-- 01_02_spatial_corr.R           # Spatial correlation analysis
|   |-- 01_03_summary_stats_esample.R  # Estimation sample summary statistics
|
|-- tests/testthat/                    # Unit tests (bisection, fixed point, GMM, config, etc.)
|-- utils/logging.R                    # Timestamp-based conditional execution
|-- renv.lock                          # Reproducible R package snapshot
```

## Workflow

The pipeline has two phases, each orchestrated by a master script:

### Phase 1 --- Data Preparation

Run once (or when raw data changes):

```r
source("run_prep_data.R")
```

| Step | Script | Output | Description |
|:----:|--------|--------|-------------|
| 0 | `prep_00_compile_transactions.R` | `compiled_trxns.rds` | Merges raw POS transaction pulls |
| 1 | `prep_01_cosmo_classify.R` | `classified_descriptions.rds` | Keyword-based service classification into 5 task types |
| 2 | `prep_02_censuspop.R` | `county_census_pop.rds` | 2020 Census population data |
| 3 | `prep_03_county_msa_xwalk.R` | `county_msa_xwalk.rds` | Geographic crosswalk |
| 4 | `prep_04_consumerexpenditure.R` | `cex_outside.rds` | Consumer Expenditure Survey (outside option) |
| 5 | `prep_05_ppi.R` | `ppi.rds` | Producer Price Index for beauty inputs |
| 6 | `prep_06_qcew.R` | `qcew_county.rds` | County average weekly wages |

### Phase 2 --- Estimation

Run the full pipeline:

```r
source("run_all.R")
```

| Step | Script | Output | Description |
|:----:|--------|--------|-------------|
| 0 | `00_mk_tasks_cosmo.R` | `00_tasks_cosmo.rds` | Task-level aggregation with Lidstone smoothing; drops legacy-import rows (see `docs/legacy_import_data_issue.md`) |
| 1 | `01_build_data.R` | `01_working.rds` | Hierarchical clustering of workers into 5 types; constructs skill matrix **B** and worker distribution **E** |
| 2 | `02_estimation.R` | `02_parameters.rds` | **Stage 1**: IV demand estimation. **Stage 2**: GMM on assignment equilibrium moments |

Both master scripts support **conditional execution**---they check file timestamps and skip steps whose inputs have not changed, logging status to `logs/`.

### Analysis Scripts

After estimation, run analysis scripts individually for descriptive output:

```r
source("01_01_stylized_facts.R")    # Tables and figures
source("01_02_spatial_corr.R")      # Spatial patterns
source("01_03_summary_stats_esample.R")  # Sample summary statistics
```

## `01_build_data.R` Pipeline

`01_build_data.R` (which sources `cluster.R` at the end) transforms raw task-level data into the estimation-ready dataset `01_working.rds`. The full pipeline has nine stages:

| Stage | Location | Description |
|-------|----------|-------------|
| 1 | `01_build_data.R:31–54` | **Task merge + duration imputation.** Merge task cluster 3 into 4 (blowdry/style/treatment/extension), re-rank cluster IDs, impute zero-duration services (~2%) with the quarter–task-cluster mean. |
| 2 | `01_build_data.R:57–69` | **ZIP → county mapping.** GEOCORR crosswalk with >50% area-fraction threshold; merge county population data. |
| 3 | `01_build_data.R:76–83` | **Drop zero-revenue firm-quarters.** |
| 4 | `01_build_data.R:107–162` | **Staff-task matrix.** Aggregate transactions to worker × firm × quarter × task; wide-pivot so each row is one worker-quarter with duration columns per task. Compute firm-level task mix, `service_mix_id` (binary string of active tasks), and the **specialization index `s_index`** (a KL-divergence-type measure of how much a worker's task allocation deviates from the firm average). Save `01_staff_task_full.rds`. |
| 5 | `01_build_data.R:164–175` | **Lidstone smoothing + sample filter.** Restrict to target quarters (2018 Q1–2021 Q2, excluding 2020 Q2/Q3) and the three study counties. Add the mean service duration as a pseudocount to regularize zero-duration cells; recompute B shares on smoothed totals. Save `01_staff_task_full_smoothed.rds` and `01_staff_task.rds`. |
| 6 | `cluster.R:1–38` | **Within-firm hierarchical clustering.** For each county, find the minimum dendrogram cut height that guarantees ≤ `n_worker_types` groups per firm (complete linkage, Euclidean distance on `Btilde_raw`). Apply cut to assign `type_within_firm`. |
| 7 | `cluster.R:44–113` | **Cross-firm type matching.** Select a reference firm per county (largest firm with all 5 types). For each comparable firm-quarter, exhaustively enumerate permutations of local type labels and pick the one minimizing L1 distance on normalized log-ratio Btilde vectors. Produces globally consistent `worker_type` labels. |
| 8 | `cluster.R:115–226` | **Expand to full type matrix.** Collapse to merged type × firm × quarter; expand so every firm always has all 5 type rows (zeros for absent types); cast wide to `verywide_expanded` (one row per firm-quarter). Also build `forgamma_verywide` (pairwise log-ratios of `BdivE` across type pairs, used for gamma estimation). |
| 9 | `cluster.R:229–424` | **Gamma estimation + auxiliary merges.** Build a firm-similarity graph (two firms are linked if they share ≥2 comparable type pairs). Traverse shortest paths from each firm to the reference firm to estimate `gamma_normalized` (relative productivity/entropy parameter). Merge CEX outside-good shares and QCEW county wages. |

### Key Variables

| Variable | Definition |
|----------|-----------|
| `Btilde_raw` | Worker's raw (unsmoothed) share of each task, normalized by total worker time. Used for clustering. |
| `Btilde` | Smoothed version of `Btilde_raw`. |
| `B` | Worker type's share of total firm time by task (smoothed). |
| `BdivE` | `B` divided by the worker type's employment share `E`. Core input for gamma estimation. |
| `s_index` | Firm-level specialization index: sum over workers and tasks of `B_raw * log(B_raw / task_mix / e_frac)`. |
| `gamma_normalized` | Estimated entropy-regularization parameter per firm-quarter, recovered from skill-ratio paths. |
| `e_frac` / `E` | Worker type's share of total firm labor time. |

### Validation

Among workers observed across multiple quarters, approximately 90% are classified as the same type in every quarter, providing evidence that the inferred types capture stable worker characteristics rather than transient variation.

## Numerical Methods

The estimation relies on three core algorithms defined in `preamble.R`:

- **SQUAREM** (Squared Polynomial Extrapolation): Accelerated fixed-point iteration to solve for the equilibrium worker distribution **E** and assignment matrix **B**, given a cost matrix and entropy parameter.
- **Bisection**: Finds the entropy-regularization parameter *gamma* that rationalizes the observed specialization index for each firm-quarter.
- **Barzilai-Borwein (BBsolve)**: Spectral method for the outer GMM loop, minimizing the distance between model-predicted and observed worker-type distributions.

All computations use **log-space arithmetic** with floor/ceiling safeguards to maintain numerical stability.

## Getting Started

### Prerequisites

- **R** >= 4.3.0
- Raw transaction data (not included; placed in path specified by `.Renviron`)

### Setup

```r
source("setup_renv.R")   # Install pinned package versions via renv
```

Create a `.Renviron` file in the project root with your local data path:

```
RAW_DATA_BASE=/path/to/raw/data
```

### Run

```r
source("run_prep_data.R")   # Phase 1: data preparation
source("run_all.R")          # Phase 2: estimation
```

### Tests

```r
source("run_tests.R")
```

## Citation

If you use this code, please cite:

> Kohlhepp, Jake. "The Inner Beauty of Firms." Job Market Paper. [https://www.jkohlhepp.com/files/kohlhepp_jmp.pdf](https://www.jkohlhepp.com/files/kohlhepp_jmp.pdf)

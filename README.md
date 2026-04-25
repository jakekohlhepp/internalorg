# The Inner Beauty of Firms

Replication code for *The Inner Beauty of Firms* by Jake Kohlhepp.

## Current Workflow

### Phase 1: Data Preparation

Run the prep pipeline when raw inputs change:

```r
source("run_prep_data.R")
```

| Step | Script | Output | Purpose |
|------|--------|--------|---------|
| 00 | `prep_00_compile_transactions.R` | `<raw_data_base>/compiled_trxns.rds` | Compile confidential raw transaction pulls into one file. |
| 01 | `prep_01_cosmo_classify.R` | `mkdata/data/classified_descriptions.rds` | Map service descriptions to task categories. |
| 02 | `prep_02_censuspop.R` | `mkdata/data/county_census_pop.rds` | Build county population controls. |
| 03 | `prep_03_county_msa_xwalk.R` | `mkdata/data/county_msa_xwalk.rds` | Build county-to-MSA crosswalk. |
| 04 | `prep_04_consumerexpenditure.R` | `mkdata/data/cex_outside.rds` | Build outside-option inputs from CEX. |
| 05 | `prep_05_ppi.R` | `mkdata/data/ppi.rds` | Build quarterly price-index inputs. |
| 06 | `prep_06_qcew.R` | `mkdata/data/qcew_county.rds` | Build county wage inputs from QCEW. |

### Phase 2: Main Pipeline

Run the managed estimation workflow:

```r
source("run_all.R")
```

| Step | Component | Output | Purpose |
|------|-----------|--------|---------|
| 0 | `00_mk_tasks_cosmo.R` | `mkdata/data/00_tasks_cosmo.rds` | Convert compiled transactions into task-level data. |
| 1 | package restore/setup inside `run_all.R` | local `renv` library state | Restore pinned package versions when needed. |
| 2 | `01_build_data.R` + `cluster.R` | `mkdata/data/01_working.rds`, `mkdata/data/01_staff_task_full.rds` | Build worker-task objects, cluster worker types, and assemble the pre-filter firm-quarter panel. |
| 3 | `04_estimation_sample.R` | `mkdata/data/04_estimation_sample.rds`, `results/out/tables/04_summary_stats_structural.tex` | Augment the working sample with PPI, minimum wage, instruments, and the GMM `estim_matrix`; write the estimation-sample summary-stats table. |
| 4 | `06_estimation.R` + `preamble.R` | `results/data/06_parameters.rds` | Estimate the structural model from `04_estimation_sample.rds`. |
| 5 | `05_iv_spec_comparison.R` + `preamble.R` | `results/out/tables/05_standard_iv_comparison.tex`, `results/out/tables/05_standard_hausman_fe_comparison.tex`, `results/out/tables/05_nested_fe_comparison.tex` | Produce demand-side IV specification comparison tables. |
| 6 | `run_counterfactuals.R` + `utils/counterfactuals_core.R` | `results/data/counterfactuals/*.rds`, `results/out/tables/05_06_*.tex`, `results/out/figures/05_07_*.png` | Run the post-estimation counterfactual series using a shared setup/output layer. |

### Manual Analysis Scripts

These scripts are still standalone and are not called by `run_all.R` yet:

```r
source("01_01_stylized_facts.R")
source("01_02_spatial_corr.R")
```

## Repository Layout

```text
refactor_estimation/
|-- config.R
|-- utils/logging.R
|-- run_prep_data.R
|-- prep_00_compile_transactions.R
|-- prep_01_cosmo_classify.R
|-- prep_02_censuspop.R
|-- prep_03_county_msa_xwalk.R
|-- prep_04_consumerexpenditure.R
|-- prep_05_ppi.R
|-- prep_06_qcew.R
|-- run_all.R
|-- 00_mk_tasks_cosmo.R
|-- 01_build_data.R
|-- cluster.R
|-- preamble.R
|-- 04_estimation_sample.R
|-- 05_iv_spec_comparison.R
|-- 06_estimation.R
|-- run_counterfactuals.R
|-- utils/counterfactuals_core.R
|-- 01_01_stylized_facts.R
|-- 01_02_spatial_corr.R
|-- archive/experimental/        # archived exploratory scripts, not in pipeline
|-- docs/                        # active docs and reference materials
|   |-- historical/             # archived review and refactor notes
|-- tests/testthat/
|-- mkdata/data/
|-- results/data/
|-- results/out/
```

## Setup

Requirements:

- R >= 4.3.0
- a `.Renviron` entry for `raw_data_base` if you need confidential raw transaction inputs
- optional `JMP_RAW_DATA_PATH` override if your local raw/prep source folder is not `mkdata/raw`

Set up the package environment with:

```r
source("setup_renv.R")
```

## Run

```r
source("run_prep_data.R")
source("run_all.R")
source("run_tests.R")
```

`run_all.R` and `run_prep_data.R` both write logs to `logs/` and skip steps that
are already up to date unless `CONFIG$force_rerun` is set to `TRUE`.

Counterfactual artifacts are written under `results/data/counterfactuals/`.
For compatibility during the refactor, the `05_*` scripts still mirror outputs to
the legacy `analysis_final/` locations when they run.

## Documentation

- Active workflow docs are indexed in `docs/README.md`.
- Historical process notes live in `docs/historical/`.
- Archived exploratory scripts live in `archive/experimental/`.

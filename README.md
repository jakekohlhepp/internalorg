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
| 2 | `01_build_data.R` + `cluster.R` | `mkdata/data/01_working.rds`, `mkdata/data/01_staff_task_full.rds`, `mkdata/data/01_worker_type_lookup.rds` | Build worker-task objects, cluster worker types (cut-height rule documented in `docs/worker_type_clustering.md`), and assemble the pre-filter firm-quarter panel. |
| 3 | `04_estimation_sample.R` | `mkdata/data/04_estimation_sample.rds`, `results/out/tables/04_summary_stats_structural.tex` | Augment the working sample with PPI, minimum wage, the `dye_instrument`, the constrained-IV inputs, and the GMM `estim_matrix`; write the estimation-sample summary-stats table. |
| 4 | `05_iv_spec_comparison.R` + `preamble.R` | `results/out/tables/05_standard_iv_comparison.tex`, `results/out/tables/05_standard_hausman_fe_comparison.tex`, `results/out/tables/05_nested_fe_comparison.tex` | Produce demand-side IV specification comparison tables. |
| 5 | `06_estimation.R` + `preamble.R` | `results/data/06_parameters.rds` | Estimate the structural model from `04_estimation_sample.rds`. |
| 5c | `06c_wage_identification.R` | `results/data/06c_wage_identification.rds`, `results/out/tables/06c_wage_eigenvalues.tex`, `results/out/tables/06c_wage_perturbation.tex` | Wage-stage Hessian eigenvalues and perturbation diagnostic. |
| 6 | `07_vcov.R` | `results/data/07_first_stage_vcov.rds`, `results/data/07_murphy_topel_vcov.rds` | Standard errors: analytical clustered 2SLS demand vcov + Murphy-Topel two-step sandwich for the structural (wage/price) SEs (see `docs/murphy_topel_proposal.md`). Replaces the retired Petrin-Train bootstrap (`legacy/07_bootstrap.R`). Murphy-Topel is the default structural SE source. |
| 7 | `08_display_estimates.R` | `results/out/tables/08_org_price.tex`, `08_time_effects.tex`, `08_model_fit.tex`, `08_wages_skills_<county>.tex` | Render the estimation tables for the JMP draft. |
| 8 | `09_invert_gammas.R` | `results/data/09_withgammas.rds`, `results/out/figures/09_gamma_dist.png` | Invert gammas back onto the full panel. |
| 9 | `12_validate.R` | `results/data/12_data_for_counterfactuals.rds`, `results/out/tables/12_validate_corr.tex` | Model validation; produces the counterfactual-pipeline hand-off file. |
| 10 | `run_counterfactuals.R` + `utils/counterfactuals_core.R` | `results/data/counterfactuals/13_*.rds`–`17_*.rds`, `results/out/tables/18_tot_counterfactuals.tex`, `18_bytype_counterfactuals.tex`, `results/out/figures/19_*.png` | Run the `13_*`–`19_*` counterfactual scenarios using a shared setup/output layer. |
| 11 | `20_substitution.R` | `results/out/tables/20_substitute.tex`, `results/out/figures/20_*.png` | Wage-substitution patterns at the cleared equilibrium wages from `13_initial_wages.rds`. |
| 12 | `21_substitution_prod.R` | `results/out/tables/21_substitute_prod.tex` | Productivity-substitution patterns at the cleared equilibrium wages. |

### Manual Analysis Scripts

These scripts are still standalone and are not called by `run_all.R` yet:

```r
source("02_stylized_facts.R")
source("03_spatial_corr.R")
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
|-- 06b_estimation_monotone.R
|-- 06c_wage_identification.R
|-- 07_vcov.R
|-- 08_display_estimates.R
|-- 09_invert_gammas.R
|-- 12_validate.R
|-- 13_counterfactual_prep.R
|-- 14_counterfactual_diffusion.R
|-- 15_counterfactual_sales_tax.R
|-- 16_counterfactual_immigration.R
|-- 17_counterfactual_merger.R
|-- 18_counterfactual_summary.R
|-- 19_counterfactual_figures.R
|-- 20_substitution.R
|-- 21_substitution_prod.R
|-- run_counterfactuals.R
|-- compile_warm_start_wages.R
|-- utils/constrained_demand_iv.R
|-- utils/counterfactuals_core.R
|-- utils/estimation_pipeline.R
|-- utils/structural_solver.R
|-- 02_stylized_facts.R
|-- 03_spatial_corr.R
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

Counterfactual artifacts are written under `results/data/counterfactuals/`
with the `13_*`–`17_*` filename scheme. For compatibility during the
refactor, the `13_*`–`19_*` scripts still mirror outputs to the legacy
`analysis_final/` locations when they run. Figure basenames produced by
`19_counterfactual_figures.R` now use the `19_*` prefix to match the
script number (the JMP draft must reference the new names).

## Documentation

- Active workflow docs are indexed in `docs/README.md`.
- Historical process notes live in `docs/historical/`.
- Archived exploratory scripts live in `archive/experimental/`.

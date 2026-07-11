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
| 2b | `02_stylized_facts.R` | `results/data/02_stylized_facts_data.rds`, `results/out/tables/02_*.tex`, `results/out/figures/02_*.png` | Industry-wide stylized facts on the full national sample (dispersion, productivity-specialization correlations, customer behavior). Skipped when the confidential raw pulls under `raw_data_base` are unavailable. |
| 2c | `03_spatial_corr.R` | `results/out/figures/03_spatial_cor_*.png`, `results/out/figures/03_coverage.png` | ZIP-level choropleths of productivity / specialization / return rate for the three focal counties plus a sample-coverage map. Consumes 02's panel and Census/GEOCORR geo files; skipped when those are unavailable. |
| 3 | `04_estimation_sample.R` | `mkdata/data/04_estimation_sample.rds`, `results/out/tables/04_summary_stats_structural.tex` | Augment the working sample with PPI, minimum wage, the `dye_instrument`, the constrained-IV inputs, and the GMM `estim_matrix`; write the estimation-sample summary-stats table. |
| 4 | `05_iv_spec_comparison.R` + `preamble.R` | `results/out/tables/05_standard_iv_comparison.tex`, `results/out/tables/05_standard_hausman_fe_comparison.tex`, `results/out/tables/05_standard_dye_fe_comparison.tex`, `results/out/tables/05_nested_fe_comparison.tex`, `results/out/tables/05_nested_sigma_eq_one.tex`, `results/out/tables/05_standard_iv_comparison_first_stage.tex`, `results/out/tables/05_standard_hausman_fe_comparison_first_stage.tex`, `results/out/tables/05_standard_dye_fe_comparison_first_stage.tex` | Produce demand-side IV specification comparison tables (standard + nested logit, instrument/FE comparisons, and first-stage companions). |
| 5 | `06_estimation.R` + `preamble.R` | `results/data/06_parameters.rds` | Estimate the structural model from `04_estimation_sample.rds`. |
| 5b | `06b_estimation_monotone.R` + `utils/constrained_demand_iv.R` | `results/data/06b_parameters_monotone.rds`, `results/data/06b_perms.rds`, `results/data/06b_qp_diagnostics.rds`, `mkdata/data/06b_seeit_bb.rds` | Robustness variant of step 5 imposing workers-as-rows monotonicity on each county's skill matrix (constrained-2SLS QP over worker permutations). Feeds the 06b comparison diagnostics. |
| 5c | `06c_wage_identification.R` | `results/data/06c_wage_identification.rds`, `results/out/tables/06c_wage_eigenvalues.tex`, `results/out/tables/06c_wage_perturbation.tex` | Wage-stage Hessian eigenvalues and perturbation diagnostic. |
| 6 | `07_vcov.R` | `results/data/07_first_stage_vcov.rds`, `results/data/07_murphy_topel_vcov.rds` | Standard errors: analytical clustered 2SLS demand vcov + Murphy-Topel two-step sandwich for the structural (wage/price) SEs (see `docs/murphy_topel_proposal.md`). Replaces the retired Petrin-Train bootstrap (`legacy/07_bootstrap.R`). Murphy-Topel is the default structural SE source. |
| 7 | `08_display_estimates.R` | `results/out/tables/08_org_price.tex`, `results/out/tables/08_time_effects.tex`, `results/out/tables/08_model_fit.tex`, `results/out/tables/08_wages_skills_<county>.tex` | Render the estimation tables for the JMP draft. |
| 7b | `08b_model_fit_monotone.R` | `results/out/tables/08b_model_fit_monotone.tex`, `results/out/tables/08b_qp_objective_monotone.tex`, `results/data/08b_model_fit_monotone.rds` | Robustness companion to step 7: the 08 moment-fit R2 summary recomputed under both the main (06) and monotone (06b) estimates, side by side, plus the per-county constrained-2SLS QP objectives at the chosen permutation. |
| 8 | `09_invert_gammas.R` | `results/data/09_withgammas.rds`, `results/out/figures/09_gamma_dist.png` | Invert gammas back onto the full panel. |
| 9 | `12_validate.R` | `results/data/12_data_for_counterfactuals.rds`, `results/out/tables/12_validate_corr.tex`, `results/out/figures/12_marginal_{cut,color,other3}.png`, `results/out/figures/12_bivariate_{cut_color,cut_other3,color_other3}.png` | Model validation; produces the counterfactual-pipeline hand-off file plus marginal/bivariate fit figures. |
| 10 | `run_counterfactuals.R` + `utils/counterfactuals_core.R` | `results/data/counterfactuals/13_*.rds`–`17_*.rds`, `results/out/tables/18_tot_counterfactuals.tex`, `18_bytype_counterfactuals.tex`, `results/out/figures/19_*.png` | Run the `13_*`–`19_*` counterfactual scenarios using a shared setup/output layer. |
| 11 | `20_substitution.R` | `results/out/tables/20_substitute.tex`, `results/out/figures/20_*.png` | Wage-substitution patterns at the cleared equilibrium wages from `13_initial_wages.rds`. |
| 12 | `21_substitution_prod.R` | `results/out/tables/21_substitute_prod.tex` | Productivity-substitution patterns at the cleared equilibrium wages. |
| 13 | `22_skill_parameter_units.R` | `results/out/tables/22_skill_units.tex`, `results/data/22_skill_units.csv` | Skill parameters in interpretable units. |

### 06b Monotone Comparison Diagnostic

After step 5b (`06b_estimation_monotone.R`) has produced
`results/data/06b_parameters_monotone.rds`, `06b_perms.rds`, and
`06b_qp_diagnostics.rds`, a single standalone helper compares the unconstrained
(06) and monotone (06b) point estimates. It is not called by `run_all.R` because
it is a presentation/diagnostic helper:

```r
source("06b_compare.R")
```

`06b_compare.R` reads the saved 06/06b parameter files (a single source of
truth — no QP recomputation) and emits both the console comparison tables
(price/org cost, wage premiums, per-county skill matrices with the monotone
check and QP permutation gap, time-varying parameters, headline Frobenius
moves) and the paired skill-matrix heatmaps
(`results/out/figures/06b_skillmatrix_{percounty,sharedlog,diff}.{png,pdf}`).
It supersedes the former `compare_06_vs_06b.R`, `compare_06_vs_06b_figures.R`,
and `display_06b_skills.R`.

## Requirements

- **R.** The cluster runs R 4.4.0, the version recorded in `renv.lock`;
  `run_all.R` enforces R >= 4.3.0, and local runs on 4.3.0 reproduce the
  tracked outputs bit-identically. Use 4.4.0 where possible.
- **Packages.** Pinned in `renv.lock` (built against the Posit Package Manager
  CRAN snapshot of 2024-01-15). The project `.Rprofile` activates renv
  automatically, and `run_all.R` STEP 1 runs `renv::restore()`.

## Data Availability

### Confidential data (not included)

The raw salon transaction data (appointment-level exports from the
booking-software provider, including the chair-renter, tip, and product pulls)
are confidential and are **not** part of this repository. `config.R` locates
them through the `raw_data_base` entry in a project `.Renviron` file. The
stages that need them
— `prep_00_compile_transactions.R`, `00_mk_tasks_cosmo.R`, `01_build_data.R`
(via `00_tasks_cosmo.rds`), and `02_stylized_facts.R` — run only on a machine
with access and are skipped gracefully elsewhere; everything from
`04_estimation_sample.R` onward runs from derived inputs (see
`docs/slurm_longleaf.md` §8 for the local-vs-cluster execution map).

### Derived intermediate data (not included)

Several intermediate files are pseudonymized firm/staff-level records derived
from the confidential transactions, so they are also not distributed in this
public repository even though downstream pipeline stages consume them:
`mkdata/data/00_tasks_cosmo.rds`, `01_working.rds`, `01_staff_task.rds`,
`01_staff_task_full.rds`, `01_staff_task_full_smoothed.rds`,
`01_worker_type_lookup.rds`, `01_xwalk.rds`, `04_estimation_sample.rds`,
`classified_descriptions.rds`, plus `results/data/02_stylized_facts_data.rds`,
`09_withgammas.rds`, and `12_data_for_counterfactuals.rds`. The estimation and
counterfactual stages (`04_estimation_sample.R` → `22_skill_parameter_units.R`)
require a subset of these. None of these files carries information beyond the
confidential raw data: each is fully reproducible by running the committed code
(`run_prep_data.R`, then pipeline steps 00–12) against the raw inputs.

Everything else the pipeline needs **is** committed: `seeit_bb.rds` (the
estimation starting vector), `minwage.xlsx`, `01_keytask.rds`, the
public-source prep outputs (`cex_outside`, `county_census_pop`,
`county_msa_xwalk`, `ppi`, `qcew_county`), the estimated parameters
(`results/data/06_parameters.rds`), the counterfactual warm-start seeds, and
the paper's tables and figures under `results/out/`.

### Public data (download sources)

`mkdata/raw/` is gitignored (large external files). To rebuild the prep layer,
populate it as follows; several folders carry a `readme.txt` recording the
original retrieval details.

| Folder / file | Contents | Source |
|---|---|---|
| `202403101_morecspops/` | County population estimates (`co-est2019-alldata.csv`, `co-est2022-alldata.csv`) | US Census Bureau county population totals: [2010s](https://www.census.gov/data/datasets/time-series/demo/popest/2010s-counties-total.html), [2020s](https://www.census.gov/data/tables/time-series/demo/popest/2020s-counties-total.html) |
| `20220711_cex/` | Consumer Expenditure Survey interview PUMD, 2013–2021 (`intrvw13/`…`intrvw21/`) | [BLS CEX public-use microdata](https://www.bls.gov/cex/pumd_data.htm) |
| `20220727_countypop/` | GEOCORR ZCTA-to-county crosswalks (`geocorr2022_*.csv`) | [Missouri Census Data Center GEOCORR](https://mcdc.missouri.edu/applications/geocorr.html) |
| `20231023_county_msa_crosswalk/` | QCEW county–MSA–CSA crosswalk + CEX PSU codebook | [BLS crosswalk](https://www.bls.gov/cew/classifications/areas/county-msa-csa-crosswalk.htm) and [CEX PUMD documentation](https://www.bls.gov/cex/pumd_doc.htm); the folder `readme.txt` documents the matching methodology |
| `20231227_ppi_cost/file.csv` | Producer Price Index series WPU06140282 | [BLS data viewer](https://beta.bls.gov/dataViewer/view/timeseries/WPU06140282) |
| `20220427_qcew_code/` | QCEW county wages cache (NAICS 812112, 2014–2021) | [BLS QCEW](https://www.bls.gov/cew/) open-data API; `prep_06_qcew.R` refreshes the cache from the API when it is absent |
| `20240415_census_zcta_shapefiles/` | ZCTA cartographic boundary shapefile (`cb_2018_us_zcta510_500k.*`) | [US Census cartographic boundary files](https://www.census.gov/geographies/mapping-files/time-series/geo/cartographic-boundary.html) |
| `20220526_cosmo_classify/`, `20260310_manual_review/` | Cosmetologist task classification of service descriptions + follow-up manual review | Author-constructed (manual classification); the `20260310` folder `readme.txt` documents the workflow |
| `mkdata/data/minwage.xlsx` | County-quarter minimum wage schedule | Author-compiled |
| `mkdata/manual_rules/*.csv` | Manual price-unit and service-classification overrides | Author-constructed; **tracked in this repository** |

## License

The code in this repository is released under the MIT License (see
[LICENSE](LICENSE)).

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
|-- 08b_model_fit_monotone.R
|-- 09_invert_gammas.R
|-- 12_validate.R
|-- 12b_validate_monotone.R
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

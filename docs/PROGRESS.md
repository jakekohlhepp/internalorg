# Project Status

Last updated: 2026-04-30

## 2026-04-30 Counterfactual Refactor

`12_validate.R` and `13_*.R` through `19_*.R` are point-estimate post-estimation
programs. They read `results/data/06_parameters.rds` plus their upstream point
estimate artifacts and do not consume bootstrap draws, standard errors, or any
`07_bootstrap.R` output.

The counterfactual scripts now share assignment, entropy, wage-adjusted skill
matrix, and best-response price helpers from `utils/counterfactuals_core.R`.
Those helpers call the same `solve_equilibrium()` implementation used earlier
in the structural pipeline, so the core counterfactual allocation logic is
defined once.

## Current State

The structural pipeline has an explicit three-stage handoff, and the
counterfactual pipeline is now wired into `run_all.R`:

1. `01_build_data.R` writes:
   - `01_staff_task_full.rds` for full-sample descriptive work
   - `01_working.rds` for the estimation-base branch
2. `04_estimation_sample.R` enriches the estimation-base branch with PPI,
   minimum wages, and instruments, then writes
   `mkdata/data/04_estimation_sample.rds`
3. `05_iv_spec_comparison.R` and `06_estimation.R` consume the saved 04
   artifact. `preamble.R` exposes `build_estimation_setup()` and no longer
   mutates ambient session state when sourced.
4. `run_counterfactuals.R` loads `results/data/06_parameters.rds` via
   `utils/counterfactuals_core.R` helpers and orchestrates the
   `05_00`–`05_07` scenario scripts.

This keeps the stylized-facts architecture clear:

- descriptive scripts stay on the full-sample branch
- structural scripts stay on the estimation-only branch
- counterfactual scripts consume only post-estimation outputs

## Current Automated Workflow

### Prep runner

`run_prep_data.R` manages:

1. `prep_00_compile_transactions.R`
2. `prep_01_cosmo_classify.R`
3. `prep_02_censuspop.R`
4. `prep_03_county_msa_xwalk.R`
5. `prep_04_consumerexpenditure.R`
6. `prep_05_ppi.R`
7. `prep_06_qcew.R`
8. `prep_07_nyc_rent.R`

### Main runner

`run_all.R` manages:

1. `00_mk_tasks_cosmo.R`
2. package restore/setup through `renv`
3. `01_build_data.R`
4. `04_estimation_sample.R`
5. `05_iv_spec_comparison.R`
6. `06_estimation.R`
7. `07_bootstrap.R`
8. `08_display_estimates.R`
9. `09_invert_gammas.R`
10. `10_substitution.R`
11. `11_substitution_prod.R`
12. `12_validate.R`
13. `run_counterfactuals.R`

### Counterfactual runner

`run_counterfactuals.R` manages:

1. `05_00_prep_data.R` (baseline wages and working data)
2. `05_01_automation.R`, `05_01_no_variance.R`
3. `05_02_diffusion.R`
4. `05_03_sales_tax.R`
5. `05_04_immigration.R`
6. `05_05_no_frictions.R`
7. `05_06_merger.R`, `05_06_disp_counterfactuals.R`
8. `05_07_beautiful_display.R`

Shared utilities live in `utils/counterfactuals_core.R`.

## Active Pipeline Outputs

The automated workflow writes:

- `mkdata/data/04_estimation_sample.rds`
- `results/out/tables/04_summary_stats_structural.tex`
- `results/data/06_parameters.rds`
- `results/out/tables/05_standard_iv_comparison.tex`
- `results/out/tables/05_standard_hausman_fe_comparison.tex`
- `results/out/tables/05_nested_fe_comparison.tex`
- `counterfactuals/05_00_*.rds` and `counterfactuals/05_0[1-6]_*.rds`
- `results/out/tables/05_06_tot_counterfactuals.tex`
- `results/out/tables/05_06_bytype_counterfactuals.tex`
- `results/out/figures/05_07_*.png`

## Standalone Analysis Scripts

The following scripts remain standalone on purpose:

- `02_stylized_facts.R`
- `03_spatial_corr.R`
- `03_00_bootstrap.R` (Bayesian bootstrap; tolerances in `boot_settings.R`)

They stay outside `run_all.R` because they operate on the descriptive,
full-sample branch or produce exploratory robustness output.

## Active Workflow Docs

Use these as the current source of truth:

- `README.md`
- `docs/README.md`
- `docs/prep_data_pipeline.md`
- `docs/data_dependencies.md`
- `docs/pipeline_schematic.html`
- `docs/mk_tasks_cosmo_summary.md`
- `docs/legacy_import_data_issue.md`
- `docs/02_estimation_walkthrough.md`

## Next Planned Work

- review `03_00_bootstrap.R` to confirm it reads from the current
  `mkdata/data/01_working.rds` path (it still references a legacy
  `analysis_final/data/01_01_working.rds` input)
- keep the active docs aligned with the runner outputs and script numbering

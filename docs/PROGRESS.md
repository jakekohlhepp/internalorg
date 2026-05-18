# Project Status

Last updated: 2026-05-18

## Current State

The structural pipeline has an explicit three-stage handoff, and the
counterfactual pipeline (now numbered `13_*` through `19_*`) is wired into
`run_all.R`:

1. `01_build_data.R` writes:
   - `mkdata/data/01_staff_task_full.rds` for full-sample descriptive work
   - `mkdata/data/01_working.rds` for the estimation-base branch
   - `mkdata/data/01_worker_type_lookup.rds`, which now also carries
     `effective_cutlevels` and the `cutlevel_quantile` in effect (see
     `docs/worker_type_clustering.md`)
2. `04_estimation_sample.R` enriches the estimation-base branch with PPI,
   minimum wages, the `dye_instrument`, and the constrained-IV inputs, then
   writes `mkdata/data/04_estimation_sample.rds`
3. `05_iv_spec_comparison.R` and `06_estimation.R` consume the saved 04
   artifact. `preamble.R` exposes `build_estimation_setup()` and no longer
   mutates ambient session state when sourced.
4. `06b_wage_identification.R` is a lightweight wage-stage Hessian +
   perturbation diagnostic that runs after `06_estimation.R` (also has a
   parallel-track companion, `06b_estimation_monotone.R`, plus
   `compare_06_vs_06b.R` / `display_06b_skills.R` outside the runner).
5. `07_bootstrap.R` produces the Bayesian bootstrap. It now warm-starts from
   `results/data/06_parameters.rds`, supports SLURM array deployment via
   `run_bootstrap_array.sl`, and combines per-rep files into
   `results/data/07_bootstrap.rds` on a combine-only pass.
6. `08_display_estimates.R` → `12_validate.R` produce the post-estimation
   tables, figures, inverted gammas, and the counterfactual hand-off
   `results/data/12_data_for_counterfactuals.rds`.
7. `run_counterfactuals.R` loads `results/data/06_parameters.rds` and
   `results/data/12_data_for_counterfactuals.rds` via
   `utils/counterfactuals_core.R` helpers and orchestrates the
   `13_*`–`19_*` scenario scripts.

This keeps the stylized-facts architecture clear:

- descriptive scripts stay on the full-sample branch
- structural scripts stay on the estimation-only branch
- counterfactual scripts consume only post-estimation outputs

## Counterfactual Pipeline Renumbering (2026-05-13/14)

The old `05_00`–`05_07` counterfactual scheme was renamed to the contiguous
`13_*`–`19_*` block on master. The script names, in-script artifact names
under `results/data/counterfactuals/`, and the summary table names now all
share the new prefix.

| Old script | New script | Old artifact prefix | New artifact prefix |
|---|---|---|---|
| `05_00_prep_data.R` | `13_counterfactual_prep.R` | `05_00_*` | `13_*` |
| `05_02_diffusion.R` | `14_counterfactual_diffusion.R` | `05_02_*` | `14_*` |
| `05_03_sales_tax.R` | `15_counterfactual_sales_tax.R` | `05_03_*` | `15_*` |
| `05_04_immigration.R` | `16_counterfactual_immigration.R` | `05_04_*` | `16_*` |
| `05_06_merger.R` | `17_counterfactual_merger.R` | `05_06_*` | `17_*` |
| `05_06_disp_counterfactuals.R` | `18_counterfactual_summary.R` | `05_06_tot/bytype_counterfactuals.tex` | `18_tot/bytype_counterfactuals.tex` |
| `05_07_beautiful_display.R` | `19_counterfactual_figures.R` | figure basenames unchanged (`05_07_*.png`) | figure basenames unchanged (`05_07_*.png`) |

The figure file basenames produced by `19_counterfactual_figures.R` still
carry the `05_07_` prefix (see `utils/counterfactuals_core.R` /
`19_counterfactual_figures.R`); the figure-folder output names were not
renamed even though the script and data artifacts were.

The `05_01_automation.R`, `05_01_no_variance.R`, and `05_05_no_frictions.R`
scenarios were retired during this consolidation (see
`45b23aa Remove obsolete counterfactual scripts`).

## Recent Pipeline Changes (since 2026-04-23)

### Estimation
- Demand IV switched from the Hausman instrument to the `dye_instrument`
  (`e86e7d2`, with construction inside `04_estimation_sample.R`)
- Added `utils/constrained_demand_iv.R` and a dedicated unit-test file
  (`1ec7c50`)
- Rank-aware solve hardened against silent rank truncation; diagnostics
  saved (`fc7720a`, `23c08d0`)
- Wage solver: new `wage_optimizer_mode = "pso"` (PSO global + Nelder-Mead
  double-polish), nleqslv variant retained; obj-tol gating unified under
  `JMP_OBJ_TOL` (`350d973`, `1152d36`, `bb9f125`)
- Tight tolerances (1e-10 / 1e-8 / 1e-6) promoted to config defaults
  (`6e57cd0`)
- 01_build_data.R now allows disconnected gamma in the baseline cluster
  (`67845ad`)
- `cluster.R` cut-height rule replaced with `CONFIG$cutlevel_quantile`
  (default 0.9), documented in `docs/worker_type_clustering.md`

### Bootstrap
- Wired for SLURM array deployment (`330f4b7`); `run_bootstrap_array.sl`
  bumped to 2-day walltime, `--mem=4g`, 1100 reps default
- 06 parameters used as warm start; per-rep `boot_res_<i>.rds` files
  combined into `07_bootstrap.rds` (`63025e9`)
- Soft-revert reps kept; only `status = "error"` rows rejected (`0b34ce1`)
- See `docs/bootstrap_slurm.md` for the full wage-gate semantics and
  resource tuning rationale

### Counterfactuals
- Wage solver inside the counterfactual loop replaced with the shared
  `pso_solve` helper plus log-wage BBsolve fallback and a full-5D best-effort
  retry (`23a027e`, `9d38e0d`, `085d1d8`)
- Tier-2 wage solve in `13_counterfactual_prep.R` reduced from WLS to a
  4-equation system, then swapped to the full-5D best-effort path
  (`839e584`, `085d1d8`)
- Immigration shock now targets the lowest-wage worker type per county and
  is scaled to 10% of total labor (`a312241`, `f9116c4`)
- `run_counterfactual_check.R` added as a quick smoke driver (`d49eb7a`)

### Tests and tooling
- `tests/testthat/` expanded: `test_counterfactual_data_table.R`,
  `test_counterfactual_helpers.R`, `test_cluster_refactor.R`,
  `test_rank_aware_estimation.R`, plus standalone drills under `tests/`
- `.gitignore` now ignores all `*.sl` SLURM wrappers and any file with
  `smoke` in the name (`e60703f`, `e1c55d8`, `a643363`)
- `renv.lock` refreshed against cloud.r-project.org (`78300d7`)

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
7. `06b_wage_identification.R` (wage-stage Hessian + perturbation diagnostic)
8. `07_bootstrap.R`
9. `08_display_estimates.R`
10. `09_invert_gammas.R`
11. `10_substitution.R`
12. `11_substitution_prod.R`
13. `12_validate.R`
14. `run_counterfactuals.R`

### Counterfactual runner

`run_counterfactuals.R` manages:

1. `13_counterfactual_prep.R` (baseline wages, working data, total labor,
   `prod_initial`)
2. `14_counterfactual_diffusion.R`
3. `15_counterfactual_sales_tax.R`
4. `16_counterfactual_immigration.R`
5. `17_counterfactual_merger.R`
6. `18_counterfactual_summary.R`
7. `19_counterfactual_figures.R`

Shared utilities live in `utils/counterfactuals_core.R`.

## Active Pipeline Outputs

The automated workflow writes:

- `mkdata/data/04_estimation_sample.rds`
- `results/out/tables/04_summary_stats_structural.tex`
- `results/out/tables/05_standard_iv_comparison.tex`
- `results/out/tables/05_standard_hausman_fe_comparison.tex`
- `results/out/tables/05_nested_fe_comparison.tex`
- `results/data/06_parameters.rds`
- `results/data/06b_wage_identification.rds`
- `results/out/tables/06b_wage_eigenvalues.tex`, `06b_wage_perturbation.tex`
- `results/data/07_boot_weights.rds`, `07_bootstrap.rds`,
  `bootstrap_reps/boot_res_<i>.rds`
- `results/out/tables/08_org_price.tex`, `08_time_effects.tex`,
  `08_model_fit.tex`
- `results/data/09_withgammas.rds`, `results/out/figures/09_gamma_dist.png`
- `results/out/tables/10_substitute.tex`
- `results/out/tables/11_substitute_prod.tex`
- `results/data/12_data_for_counterfactuals.rds`,
  `results/out/tables/12_validate_corr.tex`
- `results/data/counterfactuals/13_initial_wages.rds`,
  `13_working_data.rds`, `13_total_labor.rds`, `13_prod_initial.rds`
- `results/data/counterfactuals/14_wages_diffusion.rds`,
  `14_prod_diffusion.rds`
- `results/data/counterfactuals/15_wages_salestax.rds`,
  `15_prod_salestax.rds`
- `results/data/counterfactuals/16_wages_immigration.rds`,
  `16_prod_immigration.rds`
- `results/data/counterfactuals/17_wages_merger.rds`, `17_prod_merger.rds`
- `results/out/tables/18_tot_counterfactuals.tex`,
  `18_bytype_counterfactuals.tex`
- `results/out/figures/05_07_realloc_*.png`, `05_07_reorg_*.png` (basenames
  retained for back-compat with the JMP draft)

## Standalone Analysis Scripts

The following scripts remain standalone on purpose:

- `02_stylized_facts.R`
- `03_spatial_corr.R`
- `03_00_bootstrap.R` (Bayesian bootstrap; tolerances in `boot_settings.R`)
- `06b_estimation_monotone.R`, `compare_06_vs_06b.R`, `display_06b_skills.R`
  (workers-as-rows monotone diagnostic track; see `6176a9b`)
- `peek_skill_matrix.R`, `skill_matrix_dominance.R` (skill-matrix
  diagnostics, see `b392cef`)
- `run_counterfactual_check.R` (single-call smoke through the counterfactual
  loop)

They stay outside `run_all.R` because they operate on the descriptive,
full-sample branch or produce exploratory diagnostic output.

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
- `docs/worker_type_clustering.md`
- `docs/bootstrap_slurm.md`

## Next Planned Work

- review `03_00_bootstrap.R` to confirm it reads from the current
  `mkdata/data/01_working.rds` path (it still references a legacy
  `analysis_final/data/01_01_working.rds` input)
- rename the `05_07_*` counterfactual figure basenames produced by
  `19_counterfactual_figures.R` to match the new `19_*` prefix, or
  explicitly document the basename freeze in
  `docs/data_dependencies.md`
- add the post-hoc bootstrap filter in `08_display_estimates.R` flagged
  in `docs/bootstrap_slurm.md` (drop reps whose NYC ssq is in the wrong
  basin or whose `status` / `wage_convergence` is non-zero)
- keep the active docs aligned with the runner outputs and script numbering

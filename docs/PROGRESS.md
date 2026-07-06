# Project Status

Last updated: 2026-07-01

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
4. `06c_wage_identification.R` is a lightweight wage-stage Hessian +
   perturbation diagnostic that runs after `06_estimation.R`. The monotone
   parallel track `06b_estimation_monotone.R` now runs in `run_all.R` as
   STEP 5b; its comparison helper `06b_compare.R` (tables + heatmaps) is run
   standalone.
5. `07_vcov.R` produces the standard errors: the analytical clustered 2SLS
   demand vcov (`07_first_stage_vcov.rds`) and the Murphy-Topel two-step
   structural sandwich (`07_murphy_topel_vcov.rds`); see
   `docs/murphy_topel_proposal.md`. This replaced the Petrin-Train bootstrap
   (`07_bootstrap.R`), now retired to `legacy/` (2026-06-13). Murphy-Topel is
   the default structural SE source in `08_display_estimates.R`.
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
| `05_07_beautiful_display.R` | `19_counterfactual_figures.R` | `05_07_*.png` | `19_*.png` |

The figure file basenames produced by `19_counterfactual_figures.R` were
renamed from `05_07_*.png` to `19_*.png` on 2026-05-20 so the figure
outputs match the script number. Any external references to the legacy
`05_07_*.png` names (e.g. JMP draft) must be updated.

The `05_01_automation.R`, `05_01_no_variance.R`, and `05_05_no_frictions.R`
scenarios were retired during this consolidation (see
`45b23aa Remove obsolete counterfactual scripts`).

## Recent Pipeline Changes (since 2026-04-23)

### 2026-06-29 – 2026-07-01: replication-package pass

- **Prep reproducibility restored** — `prep_00/02/04` reverted to their
  pre-refactor (Mar-9) versions and `prep_03`'s premature column assertion
  fixed; every prep output now regenerates **bit-identically** via
  `run_prep_data.R`, as do `00_mk_tasks_cosmo` → `01_build_data` →
  `02_stylized_facts` via `run_all.R` (22/22 hash check). `prep_07_nyc_rent.R`
  removed (its output had no consumer).
- **Licensing + data availability** — MIT `LICENSE` added; README gains
  Requirements and Data Availability sections (confidential vs derived vs
  public inputs, with download sources for everything under `mkdata/raw/`).
- **Data shipping model** — `.gitignore` restructured as default-deny with an
  explicit allowlist; non-confidential replication data committed (public-source
  prep outputs, `seeit_bb.rds`, `minwage.xlsx`, `01_keytask.rds`,
  `06_parameters.rds`, warm-start seeds, all paper tables/figures). Derived
  firm/staff-level microdata stays untracked and transfers to the cluster
  out-of-band (`slurm_longleaf.md` §8).
- **`06_estimation.R` no longer overwrites `seeit_bb.rds`** — the committed
  starting vector is a stable replication anchor; the post-run vector goes to
  `06_seeit_bb_next.rds` (untracked).
- **Environment hardening** — `maps` and `minpack.lm` recorded in `renv.lock`
  (both were silently missing); `run_all.R` STEP 1 now treats `renv.lock` as
  authoritative (the drifted fresh-install list is gone); STEP 0/2 dependency
  lists completed so upstream input edits trigger downstream re-runs.

### 2026-05-25/26 updates

- **Wage-fallback ladder** (`utils/wage_fallbacks.R`) -- new 5-layer post-solve
  escalation invoked after the primary wage solve in `06_estimation.R`:
  L1 per-county polish, L2 slice-Hessian probe, L3 per-county multistart,
  L4 re-PSO from improved joint vector, L5 (opt-in) joint multistart.
  See `docs/wage_identification.md` postscript and
  `docs/02_estimation_walkthrough.md` section 4a.
- **L1-L4 enabled in bootstrap by default** (`ff6f6a0`, 2026-05-26).
  Bootstrap reps now run the same ladder as solo `06_estimation.R`;
  override via `JMP_WAGE_FALLBACK_SKIP_IN_BOOTSTRAP=TRUE`.
- **Counterfactual `full_5d_retry`** -- `counterfactual_full_5d_retry`
  (PSO + polish) now triggers automatically on non-convergence of
  `solve_wage_market` (`4ffa5aa`), gated on `use_homotopy=TRUE` (`5545670`).
- **Counterfactual solver knobs** -- nine env-var-tunable knobs added to
  `config.R` for the L4 fallback (multistart count, perturbation SD, LHS/dfsane
  parameters, PSO halfwidth/particles/iter, coord-descent sweeps).
- **Bootstrap SLURM spec** -- walltime bumped to 3 days (`%200` concurrency,
  `--mem=4g`), see `docs/bootstrap_slurm.md`. Job `52526344` is the current
  1100-rep array.
- **`22_skill_parameter_units.R`** added (STEP 13 in `run_all.R`) -- re-presents
  the estimated skill matrix in willingness-to-pay (USD/customer at 1% reassign)
  and percentage-point market-share units. Wired as the final pipeline step.
- **`compile_warm_start_wages.R`** -- standalone manual tool (not a `run_all.R`
  step): compiles per-scenario warm-start wages from the most recent
  `14_*-17_*_wages_*.rds` outputs into
  `counterfactuals/13_warm_start_wages_<scenario>.rds`, consumed by 14-17.
- **Logging fix** (`ac08310`, 2026-05-27) -- `CONFIG$log_dir` resolved to an
  absolute path so subscripts that `setwd()` no longer create stray `logs/`
  subdirectories.

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

### Main runner

`run_all.R` manages:

1. `00_mk_tasks_cosmo.R`
2. package restore through `renv` (renv.lock is authoritative)
3. `01_build_data.R`
4. `02_stylized_facts.R` (STEP 2b; needs the confidential raw pulls, skipped elsewhere)
5. `03_spatial_corr.R` (STEP 2c; needs the Census/ZCTA geo files, skipped elsewhere)
6. `04_estimation_sample.R`
7. `05_iv_spec_comparison.R`
8. `06_estimation.R`
9. `06b_estimation_monotone.R` (STEP 5b; monotone-restricted robustness variant)
10. `06c_wage_identification.R` (wage-stage Hessian + perturbation diagnostic)
11. `07_vcov.R` (first-stage 2SLS + Murphy-Topel structural SEs)
12. `08_display_estimates.R`
13. `09_invert_gammas.R`
14. `12_validate.R`
15. `run_counterfactuals.R`
16. `20_substitution.R` (wage-substitution patterns at the cleared equilibrium
    wages from `13_initial_wages.rds`)
17. `21_substitution_prod.R` (productivity-substitution patterns at the
    cleared equilibrium wages)
18. `22_skill_parameter_units.R` (skill parameters in interpretable units)

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
- `results/out/tables/05_standard_dye_fe_comparison.tex`
- `results/out/tables/05_nested_fe_comparison.tex`
- `results/out/tables/05_nested_sigma_eq_one.tex`
- `results/out/tables/05_standard_iv_comparison_first_stage.tex`
- `results/out/tables/05_standard_hausman_fe_comparison_first_stage.tex`
- `results/out/tables/05_standard_dye_fe_comparison_first_stage.tex`
- `results/data/06_parameters.rds`
- `results/data/06c_wage_identification.rds`
- `results/out/tables/06c_wage_eigenvalues.tex`, `06c_wage_perturbation.tex`
- `results/data/07_first_stage_vcov.rds`, `07_murphy_topel_vcov.rds`
- `results/out/tables/08_org_price.tex`, `08_time_effects.tex`,
  `08_model_fit.tex`
- `results/data/09_withgammas.rds`, `results/out/figures/09_gamma_dist.png`
- `results/out/tables/20_substitute.tex`
- `results/out/tables/21_substitute_prod.tex`
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
- `results/out/figures/19_immigration_*.png`, `19_merger_*.png`,
  `19_prod_reversal_dumbbell.png`
- `results/out/tables/22_skill_units.tex`, `results/data/22_skill_units.csv`

## Pipeline Integration Notes

Several formerly-standalone scripts are now wired into `run_all.R`:

- `02_stylized_facts.R` (STEP 2b) and `03_spatial_corr.R` (STEP 2c) run after
  the build-data step; both skip gracefully when their confidential raw inputs
  are unavailable.
- `06b_estimation_monotone.R` (STEP 5b) runs the workers-as-rows monotone
  estimation track.
- `22_skill_parameter_units.R` (STEP 13) renders the skill matrix in
  interpretable units (`results/out/tables/22_skill_units.tex`,
  `results/data/22_skill_units.csv`) as the final pipeline step.

The following remain standalone on purpose:

- `06b_compare.R` — unified 06-vs-06b comparison (console tables + paired
  skill-matrix heatmaps to
  `results/out/figures/06b_skillmatrix_{percounty,sharedlog,diff}.{png,pdf}`).
  Reads the saved 06/06b parameter files; supersedes the former
  `compare_06_vs_06b.R`, `compare_06_vs_06b_figures.R`, and
  `display_06b_skills.R` (consolidated 2026-06-17). The ad-hoc
  `peek_skill_matrix.R` / `skill_matrix_dominance.R` inspectors were deleted in
  the same pass.
- `run_counterfactual_check.R` (single-call smoke through the counterfactual
  loop)
- `compile_warm_start_wages.R` (baseline) and `build_la_warm_start_*.R`
  (per-counterfactual) — manual tools, run by hand to refresh the committed
  warm-start seeds (`counterfactuals/13_warm_start_wages.rds` and
  `14_/15_/16_/17_warm_start_wages_*.rds`) when a better-clearing wage vector is
  found. Those `.rds` seeds are committed inputs read directly by `13_`–`17_`;
  the tools are not a `run_all.R` step.

They stay outside `run_all.R` because they operate on the descriptive,
full-sample branch or produce exploratory diagnostic output.

## Active Workflow Docs

Use these as the current source of truth:

- `README.md`
- `docs/README.md`
- `docs/prep_data_pipeline.md`
- `docs/data_dependencies.md` (includes the current mermaid diagram)
- `docs/mk_tasks_cosmo_summary.md`
- `docs/legacy_import_data_issue.md`
- `docs/02_estimation_walkthrough.md`
- `docs/worker_type_clustering.md`
- `docs/bootstrap_slurm.md`
- `docs/wage_identification.md`, `docs/wage_solver_stability.md`,
  `docs/wage_basin_retrospective.md` (wage-stage retrospectives, still
  cited by live code)
- `docs/counterfactual_solver_assessment.md` (status table tracks open
  vs. implemented recommendations)

## Bootstrap History

The standalone `03_00_bootstrap.R` Bayesian-bootstrap script was deleted on
2026-04-24 in commit `1e79630` ("Integrate post-estimation and
counterfactual scripts; swap 05/06 order"). It was replaced by
`07_bootstrap.R` (Petrin-Train bootstrap), which was in turn retired to
`legacy/` on 2026-06-13 and replaced as step 6 by `07_vcov.R`, the analytical
clustered 2SLS + Murphy-Topel standard-error producer (see
`docs/murphy_topel_proposal.md`).

## Next Planned Work

- (DONE / SUPERSEDED 2026-06-13) the bootstrap-SE work — post-hoc rep filtering
  and the 1100-rep array monitoring — is obsolete: the Petrin-Train bootstrap
  was retired in favor of the analytical Murphy-Topel sandwich (`07_vcov.R`),
  which has no reps to filter or monitor.
- implement the open counterfactual recommendations in
  `docs/counterfactual_solver_assessment.md` (Jacobian-condition diagnostic
  and `residual` surfaced alongside `converged` in summary tables)
- keep the active docs aligned with the runner outputs and script numbering

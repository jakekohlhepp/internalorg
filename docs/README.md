# Documentation Guide

This folder separates active workflow documentation from wage-stage
retrospectives, reference materials, and archived process notes.

## Active Workflow Docs

Source-of-truth references kept aligned with the runners and current code.

- `PROGRESS.md` -- current project status, recent changes, and next steps
- `prep_data_pipeline.md` -- prep-stage runner, inputs, outputs, and rebuild rules
- `data_dependencies.md` -- end-to-end dependency map (mermaid + tables),
  including the counterfactual runner
- `02_estimation_walkthrough.md` -- structural-estimation walkthrough for
  `06_estimation.R` (includes the L1-L5 fallback ladder)
- `worker_type_clustering.md` -- `cluster.R` cut-height rule and the
  `cutlevel_quantile` config knob
- `mk_tasks_cosmo_summary.md` -- stage-0 task construction note
- `legacy_import_data_issue.md` -- post-mortem on the legacy-duration drop
  (kept as a permanent reference)
- `bootstrap_slurm.md` -- SLURM array spec, wage-gate semantics, and resource
  tuning rationale for `run_bootstrap_array.sl`
- `STYLE_GUIDE.md` -- coding conventions

## Wage-Stage Retrospectives

Empirical records of the wage-solver investigation. These are historical but
still cited by live code (`run_bootstrap_array.sl`, `06_estimation.R`).

- `wage_solver_stability.md` -- multi-seed PSO and polisher experiments;
  basis for the PSO+dual-polish production path
- `wage_identification.md` -- per-county Hessian + perturbation diagnostic
  (`06c_wage_identification.R`); 2026-05-26 postscript notes the manual loop
  is now automated in `utils/wage_fallbacks.R`
- `wage_basin_retrospective.md` -- narrative retrospective on why the lower
  NYC basin was missed and how the L2/L3 layers found it
- `counterfactual_solver_assessment.md` -- audit of the counterfactual wage
  solver against the 06-stage lessons; status table tracks which
  recommendations are implemented

## Reference Materials

- `draft_jmp_context.pdf`
- `slides_kohlhepp_jmp.pdf`

## Historical Docs

Superseded review notes, refactor plans, and dated session logs live in
`docs/historical/`.

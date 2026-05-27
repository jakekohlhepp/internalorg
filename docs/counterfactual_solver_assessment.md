# Counterfactual solver assessment in light of the wage-stage findings

This document audits the counterfactual wage solver against what we learned
fixing the 06_estimation wage stage (see [wage_solver_stability.md](wage_solver_stability.md),
[wage_identification.md](wage_identification.md)).

## Status of recommendations (2026-05-26)

| # | Recommendation | Status |
|---|---|---|
| 1 | Per-result Jacobian/Hessian diagnostic | **open** |
| 2 | Surface `residual` alongside `converged` in summary tables | **open** |
| 3 | Wide-search global PSO fallback | **implemented** -- `counterfactual_full_5d_retry` in [utils/counterfactuals_core.R](../utils/counterfactuals_core.R) runs `pso_solve` as part of the L4 escalation when the primary chain leaves the residual above tol. Gated on `use_homotopy=TRUE` (`5545670`); triggered automatically by `solve_wage_market` on non-convergence (`4ffa5aa`). |
| 4 | Regenerate `13_initial_wages.rds` after re-run 06 | **mechanical, done as needed** -- the counterfactual baseline is rebuilt each time `13_counterfactual_prep.R` runs against the current `06_parameters.rds`. |
| 5 | Replicate dual-polish PSO inside counterfactual solver | **declined** -- existing multistart + homotopy + BBsolve already cover the same escape paths. |
| 6 | Hard-stop per-county on non-convergence | **declined** -- counterfactuals continue to log + soft-revert. |

The remaining open items (1) and (2) are still worth doing: a per-result
Jacobian-condition diagnostic and surfaced `residual` give the same kind of
identification audit that `06c_wage_identification.R` provides for
estimation.

## What the counterfactual solver currently does

The wage solver chain in [counterfactual_solve_wage_market](../utils/counterfactuals_core.R)
([counterfactuals_core.R:838-1021](../utils/counterfactuals_core.R#L838-L1021))
is materially more sophisticated than the bare estimation solver:

| layer | purpose | parameters |
|---|---|---|
| `counterfactual_safe_wage_fn` | wraps `fn`, returns 1e6 if any wage ≤ 0 | enforces strict positivity |
| `counterfactual_multistarts` | builds 5+ starts per call: seed, ±25% scaling, +small additive perturbations, plus user `additional_starts` | structured neighbourhood |
| `counterfactual_protected_worker_starts` | for each "saturated" worker type (target labor ≤ floor), generates starts where that type is given a high wage | handles corner solutions |
| nleqslv (Broyden + dbldog) on `log(w)` | per-start root-finder; positivity is automatic via the log transform | xtol=1e−10, ftol=1e−8, allowSingular=TRUE, maxit=`counterfactual_nleqslv_maxit` |
| best-of-multistart | tracks the lowest residual across all starts | — |
| homotopy continuation (λ ∈ {0.25, 0.5, 0.75, 1}) | blends `fn(w)` with `log(w) − log(anchor)` regulariser, then peels back | activates only when best residual > `target_tol` |
| `BB::BBsolve` (with `NM=TRUE`) | additional fallback if homotopy chain still hasn't reached tol | maxit=`counterfactual_bbsolve_maxit`, tol=min(target_tol, 1e−8) |
| `pracma::broyden` | final polish | maxiter=`counterfactual_broyden_maxit` |
| `target_tol` | gate for "converged" flag | `CONFIG$counterfactual_wage_tol` (default **0.01**) |

This is essentially a 5-level fallback ladder. Compared to the estimation
solver as it was at the start of this exercise (single nleqslv call), it
already handles many of the failure modes we hit there.

## Mapping the 06 lessons onto the counterfactual problem

| Lesson from 06 | Translates to counterfactual? | Reason |
|---|---|---|
| **Strict-exit guard** when residual exceeds tol | partial — already present via `converged` flag, but not stop()-style | The `converged` field is set but does not abort the run; calling code can choose to fail. Acceptable as-is for counterfactuals: a non-converged county-quarter is logged but not fatal. |
| **Per-county parscale** for NM | not directly applicable | The counterfactual chain uses nleqslv on log-wages and BBsolve, neither of which exposes parscale in the same way. Log-transform partially handles scale-invariance. |
| **Cold-start + dual polish** (PSO + polish_seed) | conceptually applies, but probably not needed | The existing multistart already includes the seed start. Homotopy continuation provides an alternative escape route from local basins. |
| **Hessian diagnostic** at the converged point | **does not exist; would be valuable** | There is currently no audit of whether each counterfactual wage equilibrium is at a strict local solution. With multiple shallow minima possible (NYC empirical case), the counterfactual could land on the wrong side of a saddle without anyone noticing. |
| **Seed-anchor pathology** when seed is at a bad basin | applies via `initial_guess` | `initial_guess` is reconstructed from `06_parameters.rds` (`market_parms`); if estimation found a saddle, the counterfactual inherits that as its starting point. The patched `seeit_bb.rds` + dual-polish PSO that's now in flight (job 49633616) addresses this upstream. |
| **PSO global search** | not necessary as default; could be a final fallback | The counterfactual problem is a root-finding exercise (find wages where labor demand = supply), not a least-squares minimisation. Root-finders like nleqslv with multistart + homotopy + BBsolve are the right tools. PSO would only help if the labor-clearing equation has *multiple* roots — possible at corner-solution boundaries but not the typical case. |

## Concrete differences between the two problems

The estimation wage stage minimises `‖g_wage(θ)‖²` — a sum-of-squared-moments
objective in parameter space. Local minima exist at any stationary point of
this scalar function. NYC has multiple shallow local minima because the
moment system is rank-deficient along the E_5 axis at the relevant scale.

The counterfactual wage stage finds zeros of `g_wage_clearing(w)` — a
labor-clearing equation in wage space. At a fixed (θ, γ, ω) and a given
counterfactual structure, an equilibrium exists at any wage vector where
labor demand equals labor supply. Multiple equilibria *can* exist (e.g.,
corner solutions where some worker types are priced out), but the
mathematical structure is different from a saddle of an ssq surface.

In short: the failure modes that matter for counterfactuals are
- (a) corner solutions (some types saturated to zero employment), and
- (b) numerical convergence of the inner equilibrium — neither of which is the multi-basin
issue we were chasing in 06.

The counterfactual solver already addresses (a) via
`counterfactual_protected_worker_starts`.

## Recommendations, ranked

### High value — implement

**1. Add a per-result Hessian/residual diagnostic.** After
`counterfactual_solve_wage_market` returns, compute the Jacobian of `fn` at
the solution numerically and check its condition number / smallest singular
value. A near-singular Jacobian flags a weakly-identified equilibrium —
the counterfactual analogue of NYC's flat ridge. Output a per-county-quarter
`solver_diagnostic` field alongside `converged`, recording the smallest
singular value and the conditioning. Bootstrap counterfactuals with bad
diagnostics should be flagged in the summary tables.

This is a small extension (~30 lines) and provides the same kind of insight
that `06c_wage_identification.R` provides for estimation.

**2. Tighten the "converged" definition.** Currently `converged = TRUE` if
`residual ≤ target_tol`. With `target_tol = 0.01`, a counterfactual that
clears labor markets to 1% accuracy is reported as converged. Worth
reporting `residual` itself prominently in summary tables so consumers know
how tight each county-quarter's clearing is, not just a binary flag.

### Medium value — consider

**3. Add a wide-search global fallback.** If the existing chain (multistart
→ nleqslv → homotopy → BBsolve → broyden) still leaves residual above
`target_tol`, optionally invoke a single PSO sweep with a wide search box
in log-wage space, then re-polish with nleqslv. Symmetric with the
estimation pipeline; fires only on persistent failure. Most county-quarters
will never trigger it.

**4. Re-run the counterfactual baseline (`13_counterfactual_prep.R`) after
the in-flight 06 finishes.** `initial_wages` (saved as
`13_initial_wages.rds`) was last produced from a different `06_parameters.rds`.
The patched seeit_bb / new estimation will give different `initial_guess`
values; the counterfactual baseline should be regenerated so all four
counterfactual scripts (14–17) start from the new equilibrium. This is a
mechanical re-run, not a code change.

### Low value or not recommended

**5. Replicating the dual-polish PSO design (cold + polish_seed) inside
`counterfactual_solve_wage_market`.** Skip. The counterfactual already has
`counterfactual_multistarts` which seeds the existing fixed point, plus
homotopy from the seed, plus BBsolve from the best-so-far — the structural
equivalent is already there.

**6. Per-county strict-exit (stop on non-convergence).** Skip. A failed
county-quarter currently logs a warning and continues; downstream code can
then decide what to do with the partial results. Adding a hard stop would
make a single bad market kill the entire counterfactual run, which is more
brittle.

## Practical next steps

If the user wants implementation, the natural sequence is:

1. (Wait for the in-flight 06 / 06b chain to finish; confirm new seeit_bb
   stabilises at a strict local min for NYC.)
2. Implement recommendation **(1)** — add the Jacobian-condition diagnostic
   to `counterfactual_solve_wage_market`. Surface in summary outputs (table
   in 18_counterfactual_summary.R).
3. Implement recommendation **(2)** — surface `residual` in summary tables
   alongside `converged`.
4. Re-run **(4)** — regenerate `13_initial_wages.rds` and the four
   counterfactual scripts.
5. Optionally implement **(3)** if the diagnostic from (1) flags any
   county-quarters as weakly identified post-rerun.

Steps 1–4 are mechanical; step 5 only fires if needed. The Jacobian
diagnostic alone closes the most important gap relative to what the 06b
diagnostic does for estimation.

# Counterfactual & 13_ solver tolerances (canonical recipe)

**Why this file exists.** The SLURM launch wrappers (`run_*.sl`) are gitignored
(see `.gitignore`; PROGRESS.md "Tests and tooling"), so the *actual* solver
tolerances used for the production 13_/14_–17_ runs were historically not
captured in version control by the launch scripts themselves. **As of 2026-06-29
the hardened recipe below is baked into the `config.R` defaults**, so running the
master script (`run_all.R`) — or a bare `Rscript 14_*.R` — reproduces the
production counterfactuals with **no env vars required**. The
`JMP_COUNTERFACTUAL_*` env vars remain as overrides (e.g. loosen them for fast
smoke runs); this file documents the canonical values now encoded in config.R.

## Canonical hardened recipe (13_counterfactual_prep + 14–17)

Set these before sourcing any of `13_counterfactual_prep.R`,
`14_counterfactual_diffusion.R`, `15_counterfactual_sales_tax.R`,
`16_counterfactual_immigration.R`, `17_counterfactual_merger.R`:

| Env var | Value | config.R default | Meaning |
|---|---|---|---|
| `JMP_COUNTERFACTUAL_FIXEDPOINT_MAX_ITER` | **5000000** (5e6) | 5e6 (now default) | **SQUAREM assignment** inner cap. THE critical anti-artifact knob. **v5 used 1e6, which was INSUFFICIENT** — it capped 1 immigration LA firm that needs 1.10M iters (see verification below). Use ≥5e6 (covers the observed worst with margin; converging firms stop early so the margin is ~free). |
| `JMP_FIXEDPOINT_MAX_ITER` (get_demands) | **100000** (1e5) | 1e5 | 13_'s setup/demand loop cap. **Decoupled** from the assignment cap. |
| `JMP_COUNTERFACTUAL_FIXEDPOINT_MAX_ITER_CHEAP` | **1000** | 1000 | Adaptive cheap cap: firm runs cheap first, accepted only if SQUAREM `convergence==TRUE`, else warm-restart at the full 1e6. |
| `JMP_COUNTERFACTUAL_INNERTOL` | **1e-10** | 1e-10 (now default) | Org SQUAREM fixed-point tol. (verify_cfcap recorded 1e-12 for the first hardened config; 1e-10 is the 52877088 canonical. Both tight; the cap — not innertol — is the binding correctness parameter.) |
| `JMP_COUNTERFACTUAL_OUTERTOL` | **1e-8** | 1e-8 (now default) | Price best-response contraction tol. |
| `JMP_COUNTERFACTUAL_WAGE_TOL` | **0.01** | 0.01 | Acceptance gate on the labor-clearing residual (stored per-cell as `target_tol`). |
| `JMP_COUNTERFACTUAL_CONVERGENCE_METRIC` | **labor_weighted** | labor_weighted (now default) | Cell clears if labor-share-weighted mean\|r\| ≤ tol (vs strict max\|r\|). The one override the v5 wrappers definitely set. |
| `JMP_COUNTERFACTUAL_COORD_DESCENT_FIRST` | **false** (v5) | false | Routing: root-finders → gate → coord_descent fallback. (Setting it `true` seeds coord_descent from the warm start; result-neutral, faster — safe only under the max gate, NOT labor_weighted, per the config comment.) |

Single-thread BLAS/data.table so the 8 cores go to per-firm `mclapply`:
`OMP_NUM_THREADS=1 OPENBLAS_NUM_THREADS=1 MKL_NUM_THREADS=1 R_DATATABLE_NUM_THREADS=1`.
Module: `r/4.4.0`; activate renv (`source("renv/activate.R")`).

## Why the cap must be large (the false-convergence / cap-artifact history)

LA (county 6037) has near-corner firms (gamma≈0.898) whose **assignment SQUAREM
fixed point needs a very large number of iterations** to converge — and the
requirement is wage-dependent, so it varies by counterfactual: ~51k (merger),
~161k (sales_tax), **~1.10M (immigration)**. When a firm needs more iterations
than the cap allows, it hits the cap *without converging*, so the implied labor
demand — and therefore the labor-clearing residual — is wrong: a small residual
that looks converged but isn't ("false labor-market clearing"). Symptom: LA-reorg
floored at ~0.15 in BBsolve/minpack/L-BFGS-B at the old 1e5 cap.

History of the cap:
- **1e5 (original):** too low for *every* LA cell (even sales_tax needs 161k).
- **1e6 (v5 hardening, ~2026-05-28–30):** decoupled from get_demands (1e5), with
  the adaptive cheap-then-full scheme + coord_descent-first fallback. Verified
  decoupling: `logs/verify_cfcap_52671433.out` ("DECOUPLED OK: TRUE"). Sufficient
  for sales_tax/merger/diffusion-baseline but **NOT immigration** (1.10M > 1e6).
- **5e6 (recommended, 2026-06-05):** covers the observed worst (1.10M) with ~4.5x
  margin. A converging firm stops as soon as it hits `innertol`, so a generous
  cap costs nothing extra unless a firm genuinely needs it.

Diagnostics: `diagnostics/verify_la_017_cap_artifact.R`,
`probe_coord_descent_cap_artifact.R`, `probe_cap_sensitivity_proxy.R`.

## Verification that current results are NOT cap artifacts

`smoke_verify_cap_artifact.R` re-runs each completed result's saved LA-reorg
wages through the per-firm assignment SQUAREM at cap 1e6 vs 1e7. Verdict = REAL
if no firm hits the cap and max fpevals is stable across caps.

| Counterfactual | saved fval | max fpevals (1e6 / 1e7) | firms at cap @1e6 | verdict |
|---|---|---|---|---|
| sales_tax (15) | 0.00751 | 160,659 / 160,659 | 0 | **REAL** |
| merger (17) | 0.0150 | 51,193 / 51,193 | 0 | **REAL** |
| immigration (16) | 0.0199 | 1,000,001 / 1,104,018 | **1** | **CAP ARTIFACT @1e6** — re-run clean at cap 5e6 → residual **0.0197** (vs 0.0199), labor-wtd 0.00617 unchanged, still clears. Artifact was **materially immaterial** (a convergence-flag issue, not a real error). |
| diffusion (14) | re-running cap-1e6 search (53774441), verify at 5e6 | — | — | not a clearing cell (0.031 max / 0.0125 lw, fails both gates) — cap affects only the exact residual value, not the verdict |

(Verification job 53698727, `smoke_verify_cap_artifact.R`.)

**Reading the table:** sales_tax and merger fully converge well under 1e6 and
are identical at 1e6 vs 1e7 → REAL. **Immigration's worst LA firm hits the 1e6
cap** (max_fpevals pinned at 1,000,001 = capped) and only converges at 1,104,018
under cap 1e7 → its v5 result was computed with that firm not fully converged.
The firm is ~10% over the 1e6 cap, so the *material* error in the aggregate
residual is small (its labor-weighted clearance, 0.0062, is robust), but it is
technically a false-convergence case and should be re-run at cap ≥5e6 for a
clean result.

**Lesson:** the old 1e5 cap was clearly too low (sales_tax alone needs 160k);
1e6 covered 3 of 4 cells but NOT immigration (1.10M). The cap must exceed the
hardest firm's requirement across *all* counterfactuals — hence the **5e6**
recommendation above.

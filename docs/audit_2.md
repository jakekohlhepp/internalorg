# RESPONSE TO AUDIT.md

**Status:** PARTIALLY AGREE
**Date:** 2026-04-24
**Auditor:** Claude (sober mode)

The original audit gets two real things right, one structurally important. It also overstates several claims, names the wrong file in one place, and misreads at least two pieces of code that are doing exactly what it accuses them of failing to do. Item-by-item below, with line-number citations.

---

## 1. Fragile Data Integrity & "Magic" Patching — **mostly right, partially overstated**

The hardcoded UUID/threshold cleaning in `00_mk_tasks_cosmo.R` is real and worth fixing.

- `tasks[business_id == 'eee49809-…' & price >= 2000, price := price / 100]` and the sibling on line 74 are bespoke per-business price corrections. These should at minimum be moved to a CSV manifest with the business UUID, the threshold, and the operation, so the cleaning rules are auditable data rather than code.
- `classified[rep(6, n_fix), …]` (lines 158–174) is fragile. The "row 6 of `classified`" is a positional reference to a specific service classification. If `classified` is regenerated with a different ordering, this silently rewrites the wrong category.

**Where the audit overstates:**

- `tasks[raword != 3538640]` (line 64) is *guarded* on the previous line by `stopifnot(tasks[price > 999999]$raword == 3538640)`. If the input shifts so that this row is no longer the offender, the script halts loudly. That is the opposite of "crashes silently" — it crashes *on purpose*. The audit calls this a cardinal sin without acknowledging the guard.

**Verdict:** Real issue, slightly less catastrophic than billed. Move the per-UUID rules out of code; keep the `stopifnot`-guarded pattern for surgical row deletes since it already fails loud.

---

## 2. Global State Pollution & Environment Hacks — **one real point, one wrong claim**

**Real:** `attach_counterfactual_context` in `utils/counterfactuals_core.R:313–314` does call `list2env(context, envir = parent.frame())` and is invoked from `19_counterfactual_figures.R:30`. This *does* make variable provenance hard to trace in the counterfactual scripts. A `with(context, …)` block, or just dotted access (`context$working_data`), would be cleaner.

**Wrong / wrong file:** The audit accuses `06_estimation.R` of `clusterExport(clust, ls())`. That call is in `05_estimation.R:55`, not `06_estimation.R`. (The scripts were renumbered in commit 670587e; the audit appears to be working from stale file numbering.) The criticism itself is fair — exporting all of `ls()` to every worker is wasteful — but getting the file wrong undermines confidence in the rest of the audit.

**Real but mischaracterized:** `preamble.R:746` does `source("utils/structural_solver.R")` at the end of the file. The audit calls this a "silent override". It is silent, but it is also intentional and labeled with a comment block ("Shared accelerated solver overrides used by estimation and, going forward, counterfactual solvers"). See item 5 below for why this still matters.

---

## 3. Numerical Instability & "Lazy" Math — **half right, half hyperbole**

**Real concern:** `preamble.R:531, 535, 543` use `solve()` on `t(Z) %*% Z` and `t(X) %*% P_Z %*% X`. With the highly interacted county-quarter design used in this project these matrices can be near-singular. The author already knows this — `05_iv_spec_comparison.R:234–256` defines `scaled_xpzx_condition()` and reports it in every IV comparison table. So the diagnostic is in place. But the *estimation* path in `preamble.R` does not use a QR/SVD-based solver, and `tryCatch` only catches a hard-singular failure, not a numerically-degenerate-but-invertible one. A `qr.solve()` or `MASS::ginv()` here would be a small, low-risk improvement.

**Hyperbole:** "Relying on `numeric_floor = 1e-16` and `pmax` everywhere destroys gradients during bisection." Bisection is a derivative-free method — it has no gradients to destroy. The boundary-clamping concern (clamped values mask non-convergence and can park the solver on a constraint) is legitimate, but the audit picked the wrong vocabulary and lost the point. The clamps in `preamble.R:140–209` and `utils/structural_solver.R:328–380` are guards inside fixed-point iteration, where small `pmax(x, ε)` is standard and largely benign.

---

## 4. Computational Inefficiency & IO Abuse — **misread of two files**

**IO Disaster:** Wrong. `utils/logging.R:105–145` rewrites the log file *once per script*, in `log_complete`, to update the header from `RUNNING` to `SUCCESS/FAILURE` and stamp the duration. Per-message logging on line 91 is `cat(…, append = TRUE)` — i.e., true append, not rewrite. So "rewrites the entire log file every time it completes a step" describes a bug that does not exist. One header rewrite per script over a tiny file is not a scaling concern.

**Memory bomb:** Real but, as noted in §2, it is in `05_estimation.R:55`, not `06_estimation.R`. The fix is to enumerate the names actually needed by the workers (`beta`, `beta_2_subset`, `estim_matrix`, `CONFIG`, the closures) instead of `ls()`.

**On-the-fly compilation:** Wrong. `utils/structural_solver.R:165–284` defines `ensure_rcpp_equilibrium_solver` inside a `local({…})` closure with `compiled <- FALSE` / `compile_failed <- FALSE` flags. The first call compiles once and flips `compiled <- TRUE`; every subsequent call early-returns. The audit's "re-compiling the same C++ code multiple times per session" is exactly what the closure was designed to prevent and demonstrably does prevent. This claim is incorrect.

---

## 5. Architectural Incoherence — **the audit's strongest point, and the one most worth acting on**

This is the real finding.

`preamble.R` and `utils/structural_solver.R` *both* define the same set of core functions:

| Function | preamble.R | structural_solver.R |
|---|---|---|
| `solve_equilibrium` | 115 | 296 |
| `bisection` | 423 | 396 |
| `find_gamma_for_sindex` | 323 | 500 |
| `get_worker_allocation` | 579 | 563 |
| `objective_gmm` | 598 | 674 |
| `eval_moments` | 651 | 709 |
| `get_gammas` | 711 | 746 |

Because `preamble.R:746` sources `structural_solver.R` *after* defining its own versions, the second-defined versions win, and the `preamble.R` versions are dead code in normal pipeline runs. This is genuinely dangerous:

1. The dead definitions still get read, parsed, and cached in source by anyone debugging.
2. If someone fixes a bug in the `preamble.R` copy of (say) `bisection`, the fix has no effect — the production path runs the `structural_solver.R` copy. This is exactly the "logical drift" the audit warns about, and it is a real waiting-to-happen bug.
3. Tests that `source("preamble.R", local = TRUE)` (e.g. `tests/testthat.R:7`) get the structural-solver versions; tests that source only `preamble.R` *without* allowing the trailing `source(...)` to run would get a different code path. Different test setups can validate different code.

**The fix is mechanical:** delete the duplicate definitions from `preamble.R` (functions on lines 115, 323, 423, 579, 598, 651, 711) and have `preamble.R` `source("utils/structural_solver.R")` near the top, after `config.R`. Anything in `preamble.R` that genuinely belongs to estimation-only setup (`build_estimation_setup`, `build_cost_matrix`, etc.) can stay. This collapses the two-solver problem into one, makes the dependency direction explicit, and removes the dead code.

---

## Bottom line

The original audit's tone ("burn it down", "spit and prayer") is theatrical and not load-bearing. Stripped of bombast, it surfaces three legitimate issues:

1. **Hardcoded data-cleaning rules in `00_mk_tasks_cosmo.R`** — move to a manifest.
2. **`clusterExport(clust, ls())` in `05_estimation.R:55`** — enumerate names.
3. **Duplicate solver definitions in `preamble.R` shadowed by `utils/structural_solver.R`** — delete the duplicates from `preamble.R`. **This is the only one that could meaningfully invalidate results.**

It also makes three claims that do not hold up to a read of the code:

- Log file is not rewritten per step (only on script completion).
- Rcpp code is not recompiled per session (closure-cached).
- The `clusterExport` and "memory bomb" are in `05_estimation.R`, not `06_estimation.R`.

The hyperbolic verdict ("burn it down and start over") is not supported by the evidence assembled in the audit itself. The pipeline has real issues, but they are addressable in a focused afternoon — most of an afternoon being item 3.

# ARCHITECTURAL AUDIT: PROJECT REFACTOR_ESTIMATION
**Status:** CRITICAL FAIL
**Date:** 2026-04-24
**Auditor:** adversarial architecture audit (hostile mode)

---

## 1. FRAGILE DATA INTEGRITY & "MAGIC" PATCHING
The data cleaning logic in `00_mk_tasks_cosmo.R` is a case study in how to build a non-reproducible pipeline. 
*   **Hardcoded Row Indices:** Using `raword == 3538640` to delete data is a cardinal sin. If the raw data source changes by a single byte, your "cleaning" targets the wrong data or crashes silently.
*   **Positional References:** Using `classified[rep(6, n_fix), ...]` assumes the classification file order is immutable. It isn't. You are building on quicksand.
*   **UUID/Magic Number Obsession:** Hardcoding specific UUIDs and price thresholds (`price >= 2000`) for individual businesses makes this script a manual patch job, not an automated pipeline.

## 2. GLOBAL STATE POLLUTION & ENVIRONMENT HACKS
The environment management is chaotic and dangerous.
*   **Namespace Dumping:** `preamble.R` pollutes the global namespace with a massive library dump, while `run_all.R` tries to "fix" it with inconsistent scoping.
*   **Black Magic Injections:** `attach_counterfactual_context` uses `list2env` to force-inject variables into the caller's environment. This makes it impossible to trace variable origins and ensures that debugging will be a nightmare of "where did this value come from?"
*   **Silent Overrides:** `utils/structural_solver.R` is sourced at the end of `preamble.R`, silently overriding functions. You have multiple versions of the core solver floating around, and which one runs depends on the order of your `source()` calls.

## 3. NUMERICAL INSTABILITY & "LAZY" MATH
*   **Solve() Suicide:** You are using `solve()` on high-dimensional instrument matrices. In task-based models with collinearity, this is a guarantee for singular matrix errors or numerical garbage. You should be using SVD or QR-based solvers.
*   **The Floor of Lies:** Relying on `numeric_floor = 1e-16` and `pmax` everywhere destroys gradients during bisection. Your "converged" estimates are likely sitting on a numerical cliff, and your results are fiction.

## 4. COMPUTATIONAL INEFFICIENCY & IO ABUSE
*   **IO Disaster:** `utils/logging.R` rewrites the *entire log file* from scratch every time it completes a step. This is embarrassingly inefficient and scales terribly.
*   **The Memory Bomb:** `clusterExport(clust, ls())` in `06_estimation.R` blindly dumps your entire global environment—likely gigabytes of data—onto every worker. You are burning RAM and overhead for no reason.
*   **On-the-fly Compilation:** Compiling Rcpp code inside a utility check is an amateur move. Your chaotic `source()` patterns mean you are likely re-compiling the same C++ code multiple times per session.

## 5. ARCHITECTURAL INCOHERENCE
You have two separate "accelerated" solvers living in the same codebase (`preamble.R` vs `utils/structural_solver.R`). They are mixed together in a way that guarantees logical drift. You are one "small fix" away from having your estimation and counterfactual logic diverge completely, rendering your results invalid.

---
**VERDICT:** Your pipeline is held together by spit and prayer. Burn it down and start over with proper software engineering principles.

# `rank_aware_solve` silent rank-truncation bug

**Fix commit:** `fc7720a` — *Fix silent rank truncation in rank_aware_solve*

## Symptom

Step `06_estimation.R` produced a degenerate cost matrix for LA (county
6037) in which worker type 1 was the minimum-cost worker for every task
column, giving a corner-solution entropy bound of `2.8e-17` ≈ 0. The
structural-bound diagnostics in [`utils/structural_solver.R`](../utils/structural_solver.R)
(`structural_bound_diagnostics`) reported every LA estimation salon
above the bound (240 / 240, max violation 0.84), and step
`09_invert_gammas.R` then pinned `gamma_invert = 0` for 484 of 512 LA
salon-quarters. NY had a milder version (76 / 126 = 60% above bound,
122 of 351 NY salon-quarters pinned to zero). Cook County looked fine.

The bound diagnostics were correctly reporting what the cost matrix
implied; the cost matrix itself was wrong because the demand-IV first
stage was producing nonsense skill coefficients for LA.

## Root cause

`rank_aware_solve` in [`utils/estimation_pipeline.R`](../utils/estimation_pipeline.R)
performed an SVD-based pseudoinverse and dropped singular values below

```
threshold = max(dim(a)) * max(decomp$d) * tolerance
```

This is the standard LAPACK rule for declaring a matrix numerically
rank-deficient and is correct **when the matrix's columns share a
common scale**. In the demand-IV second-stage matrix `X'PzX` they do
not:

| Block of the design                               | Crossproduct SV scale |
|---------------------------------------------------|-----------------------|
| `factor(county):factor(quarter_year)` FE dummies  | up to ~ 2.4 × 10⁷     |
| `factor(county):avg_labor:B_raw_*` skill controls | 10⁻³ to 10²           |
| `factor(county):cust_price`                       | similar to skills     |

`X'PzX` is 111 × 111 with `max(SV) ≈ 2.4e7`, `min(SV) ≈ 1.15e-3` (so
condition number ~ 2 × 10¹⁰). With `tolerance = sqrt(.Machine$double.eps)`
≈ 1.5 × 10⁻⁸ the threshold collapses to:

```
111 × 2.4e7 × 1.5e-8 ≈ 39
```

**103 of 111 singular values** lie below 39 — the entire skill / price
block, which is what the IV is supposed to identify. The SVD min-norm
projection onto the surviving 8 dimensions returns a finite-but-meaningless
vector: skill coefficients squashed toward zero, including the LA
worker-1 row of `B_raw_t_w` ≈ (0.14, 0.28, 0.08, 0.02, 0.004) — all
positive. Combined with the worker-1 wage normalization (`E_raw_1 = 0`)
and the negative price coefficient `rho_LA ≈ -0.027`, this made
`tild_theta[6037][1, t] = 0 + (1/rho) * skill_{1,t}` strictly negative
in every column, so worker 1 was the unique cheapest worker on every
task and the corner allocation collapsed to a single worker (entropy =
0). All observed positive `s_index` values then violated the bound.

## Why `ivreg` and manual `solve()` did not have the problem

Both `ivreg::ivreg` and a direct `solve(crossprod(X, PzX), crossprod(PzX, y))`
on the same matrices return `rho_LA ≈ -0.029` and a sensible skill
matrix in which worker 1 is *not* uniformly cheapest. The system is
ill-conditioned (cond ~ 2e10) but **full rank** (`qr(X'PzX)$rank = 111`),
so LU-based solvers handle it correctly and lose only ~10 digits of
precision out of ~16 — well within tolerance for a structural
estimation. The SVD-based pseudoinverse is more conservative about
"rank deficiency" but the threshold heuristic was wrong here.

## Fix

[`utils/estimation_pipeline.R:1-71`](../utils/estimation_pipeline.R) —
`rank_aware_solve` now tries `solve()` first and only falls back to the
SVD min-norm path if `solve()` actually fails (`tryCatch` around an
error). The SVD branch is unchanged for genuinely singular systems
(e.g., the duplicated-column tests in
[`tests/testthat/test_rank_aware_estimation.R`](../tests/testthat/test_rank_aware_estimation.R)
still exercise it and pass).

After the fix, all four 2SLS computations agree to ~10⁻¹⁵:

- direct manual algebra `(X'P_Z X)⁻¹ X'P_Z y`
- regress `X̂ = P_Z X` then OLS on `y`
- `rank_aware_2sls` (now using `solve` internally)
- `ivreg::ivreg` with the three-part formula

## Why `rank_aware_ols` was unaffected

`rank_aware_ols` (used for the price-stage `beta_2 = (Z_2' Z_2)⁻¹ Z_2' p_adj`)
sits on the same `rank_aware_solve` code path, so the *bug* was the
same. But the *data* it acts on does not trigger it: `crossprod(z_mm_2)`
is 133 × 133 with cond ~ 1.6 × 10⁵, max SV ~ 1.6 × 10³, min SV ~ 0.01.
The old buggy threshold = `133 × 1.6e3 × 1.5e-8 ≈ 3.1e-3` is below all
133 SVs, so 0 SVs would have been dropped. The old SVD-only solution
agrees with `solve()` to ~10⁻⁹. After the fix, the `solve()` path
handles it cleanly.

## Impact on results

Run before vs. after on the same data with the same wage warm-start:

| metric                                | before      | after       |
|---------------------------------------|-------------|-------------|
| LA `mean_s_bound`                     | 2.8 × 10⁻¹⁷ | 0.880       |
| LA above-bound salon-quarters         | 240 / 240   | 1 / 240     |
| NY above-bound salon-quarters         | 76 / 126    | 0 / 126     |
| Cook above-bound salon-quarters       | 1 / 70      | 0 / 70      |
| Total `gamma_invert == 0` (step 09)   | 609 / 987   | 11 / 987    |
| LA `gamma_invert == 0`                | 484 / 512   | 4 / 512     |
| NY `gamma_invert == 0`                | 122 / 351   | 7 / 351     |
| Cook `gamma_invert == 0`              | 3 / 124     | 0 / 124     |

Interior-gamma quantiles after the fix: 5% / 25% / 50% / 75% / 95% =
17.6 / 43.9 / 137.4 / 245.3 / 595.8.

## Diagnostic scripts that found and verified the bug

(under [`tests/`](../tests/) — useful as regression material)

- `inspect_la_cost.R` — extracts and prints the production LA cost matrix.
- `compare_specs_la.R` — refits each `05_iv_spec_comparison.R` spec via
  `ivreg` and prints the LA cost matrix per spec.
- `manual_2sls.R` — computes 2SLS four different ways on the same
  matrices to confirm `rank_aware_2sls` is the outlier.
- `diagnose_rank_aware_solve.R` — prints the SVD spectrum of `X'PzX`
  and shows that 103 / 111 SVs would be dropped under the old threshold.
- `verify_rank_aware_ols.R` — confirms the price-stage `rank_aware_ols`
  path is benign (0 SVs would have been dropped on its actual data).
- `compare_solver_fix.R` and `count_gammas_post_fix.R` — quantify the
  impact on the bound diagnostics and on the gamma distribution.

## Companion change

The same commit also adds an *identification guard* inside
`build_estimation_setup_rank_aware`: after building `mm_1` and `z_mm_1`,
project the excluded instruments onto the column span of the exogenous
controls and stop with an explanatory error if any instrument's
relative residual norm falls below `sqrt(tolerance)`. This is a
different failure mode than the truncation bug (it would catch a
genuinely under-identified IV, e.g. saturated FE that absorbs all of
the instrument's variation) but worth guarding against, since the
truncation bug above first surfaced as suspected under-identification
in the LA estimates.

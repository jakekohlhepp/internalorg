# Murphy–Topel Standard Errors for the Structural Stage

Status: IMPLEMENTED (2026-06-10) in `07_vcov.R` (renamed 2026-06-13 from
`07c_murphy_topel.R` when it became the sole SE producer -- it now also writes
`results/data/07_first_stage_vcov.rds`, replacing the retired `07_bootstrap.R`),
with the sandwich
assembly in [`assemble_triangular_mt_vcov` and
`cluster_vcov_2sls(return_scores = TRUE)` in
utils/estimation_pipeline.R](../utils/estimation_pipeline.R), unit-tested
against a brute-force joint sandwich in
`tests/testthat/test_murphy_topel.R`. Output:
`results/data/07_murphy_topel_vcov.rds`. Report these SEs for the structural
rows via `JMP_STRUCTURAL_SE_SOURCE=murphy_topel` (08_display_estimates.R);
the default remains `draws`.

Implementation conventions (decisions made at implementation time):

- **Small-sample factor.** The joint sandwich uses
  `c = G/(G-1) * (N-1)/(N-K_total)` with `K_total` = all estimated
  parameters (111 + 12 + free price coords) — the CR1 convention applied
  once, consistently, to the whole system. The saved object also carries
  `vcov_cluster_only` (`c = G/(G-1)`). Consequence: the beta block of the MT
  vcov equals the shipped analytical first-stage vcov times the *known*
  ratio of adjustments (it uses K1 = 111); `07c` asserts that identity at
  1e-8 relative. Reported demand SEs in 08 stay the analytical
  first-stage ones either way.
- **Penalty-aware wage FOC.** When `JMP_WAGE_INTERIOR_PENALTY=true` (must
  match the 06 estimation run), the wage estimating equations are the
  penalized score `h = J_2w'[gbar2 + (lambda/2)u]` with the analytic penalty
  curvature entering the scores and Jacobians
  (`D = (lambda/2) diag(pen''(share)(N/n_c)^2)`); observed-share sampling
  noise enters through the share-recovery channel. With the penalty slack
  this reduces to a fixed nonsingular transform of the raw moment system —
  sandwich unchanged (unit-tested). 07c reports the active penalty set and
  warns near the one-sided hinge kink.
- **Demeaning.** The g2/g3 per-row contributions are demeaned before
  cluster-summing (the wage FOC sits at a basin floor, not exactly zero);
  the g1 scores stay raw so the beta block is exact.
- **Price block uses the KKT/gradient system, not the raw moments.** The
  bounded L-BFGS-B step minimizes `||gbar3||^2` subject to min-wage lower
  bounds, so what it actually solves is `[(M'M) gbar3]_free = 0` — and when a
  bound binds (production data: Cook 2018Q3's `avg_labor` coefficient),
  `gbar3` itself is NOT zero (observed `max|mean g3| ~ 6`, leaking from the
  binding coordinate through M'M). 07c therefore uses
  `h_i = [(M'M) g3_i]_free` as the price-stage estimating equations, with
  Jacobian `-[(M'M)(M'M)]_{free,free}` in `p_free`. When nothing binds this
  is a fixed nonsingular transform of the moment system and the sandwich is
  exactly invariant (unit-tested). Binding coordinates are treated as fixed
  at their bound and get NA SEs (08 keeps the draw-based SD for them); the
  remaining nonstandardness — the parameter-on-boundary distribution of the
  binding coordinate itself and the sampling variability of M'M interacting
  with `gbar3* != 0` — is documented, not corrected.
- **Step sizes.** beta: `0.01 * analytical SE_j`; wage:
  `1e-3 * max(|w_j|, county parscale)`. Both scale with
  `JMP_MT_STEP_SCALE`; `JMP_MT_STEP_CHECK=true` re-runs the first columns at
  `2h` and reports the disagreement. `JMP_MT_SMOKE=true` runs the base
  evaluation + diagnostics + one column per block and exits without writing.
- The original proposal text follows. Companion to
[legacy/bootstrap_petrin_train.md](../legacy/bootstrap_petrin_train.md), which
documents the draw-based procedure formerly shipped in `07_bootstrap.R` (now
retired to `legacy/`). That procedure captured only the first-stage propagation
term; Murphy & Topel (1985) /
Newey & McFadden (1994, §6) give the full asymptotic variance of a two-step
estimator, including the second stage's own conditional sampling variance and
the cross-stage covariance that arises because both stages are estimated on
the SAME sample. Karaca-Mandic & Train (2003) is the nested-samples version;
identical samples is its special case.

## 1. The estimator is a triangular stacked GMM

Per observation i (rows of `estim_matrix`; cluster g = `location_id`), the
three estimation stages solve the stacked estimating equations:

```
g1_i(beta)        = Xhat_i (y_i - X_i' beta)                     (111: 2SLS normal equations, Xhat = Pz X)
g2_i(w; beta)     = E_mat row i                                  (12: wage moments, objective_gmm())
g3_i(p; w, beta)  = (p_adj_i(beta, w) - M_i' p) * M_i            (120: price moments, obj_final_reg's residual_moments)
```

with sum-FOCs `sum_i g1_i = 0` (exact, by 2SLS), `sum_i g2_i ~= 0` (wage
solve; 12 moments / 12 params, exactly identified), `sum_i g3_i ~= 0`
(interior coordinates of the L-BFGS-B price step; 120/120, OLS form). The
joint parameter is (beta, w, p), dim 111 + 12 + 120 = 243.

The joint Jacobian of the summed moments is **block lower-triangular**
(g1 doesn't involve (w, p); g2 doesn't involve p):

```
J = [ -A      0      0     ]        A    = X' Pz X                (analytic, = cluster_vcov_2sls bread)
    [ J_2b    J_2w   0     ]        J_2w = d sum(g2) / dw'        (12 x 12,  numerical)
    [ J_3b    J_3w  -M'M   ]        J_2b = d sum(g2) / dbeta'     (12 x 111, numerical; see §3 sparsity)
                                    J_3w = d sum(g3) / dw'        (120 x 12, numerical)
                                    J_3b = d sum(g3) / dbeta'     (120 x 111, numerical; sparse)
                                    M'M  analytic (price model matrix crossprod)
```

Joint clustered meat: `S = sum_g s_g s_g'` with
`s_g = sum_{i in g} (g1_i, g2_i, g3_i)` (243-vector per cluster). Then

```
V_joint = c * J^{-1} S J^{-1}'     with CR small-sample factor c (G/(G-1)-style).
```

Murphy–Topel for (w, p) is exactly the lower-right block of `V_joint`. The
upper-left block reproduces the analytical clustered 2SLS vcov already
shipped in `07_first_stage_vcov.rds` **identically** (the system is
triangular, so beta's marginal variance is unaffected by the later stages) —
this is both how the proposal "exploits the analytical first stage" and a
built-in consistency check.

### Influence-function form (what to actually compute)

No 243×243 inversion. Blockwise, per cluster g:

```
psi_b,g = A^{-1} s1_g                                  (already computable from cluster_vcov_2sls internals)
psi_w,g = -J_2w^{-1} (s2_g + J_2b psi_b,g)             (12x12 solve)
psi_p,g = (M'M)^{-1} (s3_g + J_3b psi_b,g + J_3w psi_w,g)
V       = c * sum_g psi_g psi_g',   psi_g = (psi_b,g, psi_w,g, psi_p,g)
```

Reading the decomposition: `-J_2w^{-1} s2_g` is the conditional ("traditional
formulas") term, `-J_2w^{-1} J_2b psi_b,g` is the first-stage propagation term
(the population analogue of what the Petrin-Train draws estimate, since
`dw/dbeta' = -J_2w^{-1} J_2b`), and summing them *within* a cluster before the
outer product automatically delivers the Murphy-Topel cross-covariance terms
that same-sample estimation requires.

Bonus: `V_joint` also yields `Cov(beta_hat, theta_hat)` — needed later for
delta-method SEs on counterfactual objects, which the draw procedure does not
provide.

## 2. Where each piece comes from in this codebase

| Piece | Source | Cost |
|---|---|---|
| `A`, `s1_g`, `psi_b,g` | extend `cluster_vcov_2sls()` to also return per-cluster score sums (it already forms `rowsum(x_hat * residuals, cluster)`) | free |
| `g2_i` rows at point estimates | `objective_gmm(theta = w_hat, x = estim_matrix, beta = beta_hat, ...)` returns the N×12 per-row matrix directly | 1 moment pass |
| `g3_i` rows | rebuild `p_adj` via `add_price_adjustment()` + `M = model.matrix(xnam)` exactly as `fit_price_parameters()` does; `g3 = (p_adj - M %*% p_hat) * M` | 1 gamma pass |
| `M'M` | analytic | free |
| `J_2w, J_3w` | central differences in the 12 wage coords; each perturbation evaluates **both** `objective_gmm` (g2 sums) and the `p_adj`/`g3` sums in one pass | 24 passes |
| `J_2b, J_3b` | central differences in the **78 relevant** beta coords (75 `cnty:avg_labor:B_*` + 3 `cnty:cust_price`); the 33 county×quarter demand FEs never enter stage 2 (`build_cost_matrix` greps only `:avg_labor:B` and `:cust_price$`; `p_adj` uses beta only through 1/rho), so those columns are identically zero — assert with one spot-check perturbation | 156 passes |

Total: ~181 moment-evaluation passes. From the smoke run, one pass (equilibrium
solves + gamma inversion over the full sample, warm-started cache) costs
~1–2 min single-threaded → **3–6 h serial** in one Slurm job
(`sbatch --wrap`, 1 core / 4 GB / `-t 12:00:00`), or trivially parallel over
perturbation columns with `mclapply` if wanted. No array needed.

### Numerical-derivative hygiene

- Step size `h_j = 0.01 * SE_j` (analytic SE for beta coords; a preliminary
  draw-based SD for wage coords): far above the solver noise floor
  (innertol 1e-10 / outertol 1e-8 → moment noise ~1e-7 on O(1) moments), far
  below the scale of nonlinearity. Re-run a column at `2h` and compare
  (Richardson check); flag columns where the two disagree by >5%.
- Run evaluation passes with `JMP_USE_STAGED_SOLVER_TOLERANCES=false` and the
  tight default tolerances so the moment function is as smooth as the solver
  allows; keep the solver cache ON (perturbations of this size are far above
  the 12-digit cache rounding).

## 3. Caveats — read before trusting the output

1. **Local linearization vs. NYC's multi-basin wage surface.** Murphy-Topel
   is a delta method: it linearizes theta(beta) at beta_hat. The draw-based
   reps show theta(beta) is violently nonlinear over the ±2 SE range of NYC's
   weakly identified rho (sign flips ~1 draw in 6, basin switching — see the
   NYC weak-ID memory). MT will be smooth and likely *smaller* than the
   draw-based SD exactly where the draws blow up. That is not a bug in either
   number: MT = all variation under the local asymptotic approximation;
   draws = global first-stage propagation without the conditional term.
   **Report both.** Where they agree (Cook, most LA params), MT's added
   conditional term is the better, more complete number; where they diverge
   (NYC rho-adjacent params), neither asymptotic story is trustworthy and the
   divergence itself is the finding.
2. **Wage FOC is not exactly zero.** The wage solver exits at a basin floor
   (NYC ssq ~1.3e-3), so `sum_i g2_i != 0` exactly. The omitted
   second-order term in J is O(||gbar2|| * ||d2g||). Mitigation: demean the
   per-row g2 contributions before cluster-summing (standard
   misspecification-robust meat), and report ||gbar2|| alongside the SEs.
3. **Binding lower bounds in the price step.** L-BFGS-B enforces min-wage
   lower bounds; a coordinate of p_hat sitting ON its bound has no interior
   FOC and must be dropped from the p block (treated as fixed) with a note.
   Check `p_hat - lower_bound` at the point estimates first; if many bind,
   the p block needs inequality-constrained theory (out of scope).
4. **G = 84 clusters again.** The joint meat S has rank ≤ 84 < 243. Diagonal
   SEs are fine; joint Wald tests across many parameters are not. Use
   t(G-1 = 83) critical values.
5. **Bound-guard scoring.** The wage solver's accept/revert gates use a
   bound-guarded score (moments + 10×violations²), but the estimating
   equations are the 12 moments; at an accepted interior solution the guard
   contributes nothing. If a county's solution sits where the entropy-bound
   penalty is active, the FOC is not the plain moment condition — emit a
   diagnostic (reuse `structural_bound_diagnostics`) and flag that county.

## 4. Proposed wiring

- Script `07_vcov.R` (originally `07c_murphy_topel.R`):
  reads `04_estimation_sample.rds`, `06_parameters.rds`,
  `07_first_stage_vcov.rds`; writes `results/data/07_murphy_topel_vcov.rds`
  with `list(V_joint, se, blocks = list(A, J_2w, J_2b, J_3w, J_3b),
  diagnostics = ...)`.
- `cluster_vcov_2sls()` gains a `return_scores = FALSE` arg (per-cluster
  score sums + influence rows) — additive, no behavior change.
- `08_display_estimates.R`: `JMP_STRUCTURAL_SE_SOURCE = draws | murphy_topel`
  (default `draws` until validated); when `murphy_topel`, structural rows take
  MT SEs and the message reports the max relative gap vs the draw-based SD.
- Validation gates before flipping the default:
  (a) V(beta) block == `07_first_stage_vcov.rds` to fp tolerance;
  (b) propagation-only term `J_2w^{-1} J_2b V1 J_2b' J_2w^{-1}'` vs the
      empirical covariance of the draw reps (agreement away from NYC);
  (c) Richardson step-size checks pass;
  (d) no (or explicitly handled) binding price bounds.

## 5. Verified: demand-block SE vs standalone 2SLS (run 54207749, 2026-06-13)

Confirms validation gate (a) numerically on the production run
(`results/data/07_murphy_topel_vcov.rds`, created 2026-06-12 22:12;
`07_first_stage_vcov.rds`). Sample: N = 468, G = 84 clusters
(`location_id`), K_total = 243.

- **Raw sandwich identical.** Stripping each routine's CR1 factor, the demand
  `cust_price` SEs agree to ~1e-14 relative (Cook/NYC/LA un-adjusted SE
  0.009621 / 0.038101 / 0.006378 from both MT and 2SLS). The beta block of the
  MT vcov *is* the analytical first-stage vcov — no estimation difference.
- **Reported MT SEs differ by a known constant 1.260×.** This is purely the
  CR1 dof factor `c = G/(G-1)*(N-1)/(N-K)`: 2SLS uses K1 = 111
  (`c = 1.32388`), MT uses K_total = 243 (`c = 2.10056`); the SE ratio is
  `sqrt(2.10056/1.32388) = 1.2596`. So MT reports Cook/NYC/LA price SEs
  0.013944 / 0.055221 / 0.009244 vs 2SLS 0.011070 / 0.043839 / 0.007339.
- **No effect on published numbers.** Per the small-sample-factor convention
  above, 08 reports the analytical first-stage (2SLS) demand SEs regardless of
  `JMP_STRUCTURAL_SE_SOURCE`; the 1.26× inflation lives only in the raw joint
  vcov object and never reaches the demand rows. (Caveat for anyone reading SEs
  straight off `07_murphy_topel_vcov.rds$se`: the demand entries carry the
  K_total dof factor, so they run ~1.26× above a standalone 2SLS report — e.g.
  Cook t = -2.08 there vs -2.62 under the demand block's own K1 dof.)

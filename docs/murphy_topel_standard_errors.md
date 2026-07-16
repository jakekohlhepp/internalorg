---
title: "Murphy--Topel Standard Errors for the Structural Stage"
author: "The Inner Beauty of Firms --- replication pipeline (`07_vcov.R`)"
date: "Compiled 2026-07-01"
geometry: margin=0.85in
fontsize: 11pt
colorlinks: true
linkcolor: MidnightBlue
urlcolor: MidnightBlue
toc: true
toc-depth: 2
header-includes:
  - \usepackage{longtable}
  - \usepackage{booktabs}
  - \usepackage{etoolbox}
  - \AtBeginEnvironment{longtable}{\footnotesize}
  - \AtBeginEnvironment{verbatim}{\footnotesize}
  - \setlength{\emergencystretch}{6em}
  - \AtBeginDocument{\sloppy}
---

# 0. About this document

This is the consolidated design-and-implementation reference for the Murphy--Topel
(MT) standard errors used for the structural (wage / price) stage of the pipeline.
It reproduces the full contents of `docs/murphy_topel_proposal.md` and adds a set of
implementation details drawn directly from the current code
(`07_vcov.R`, `utils/estimation_pipeline.R`, `tests/testthat/test_murphy_topel.R`,
`config.R`, `08_display_estimates.R`).

**Verification status (2026-06-30).** Every *algorithmic* claim in the proposal was
checked line-by-line against `07_vcov.R` and its supporting functions and found
**consistent**: the triangular stacked-GMM structure, the block Jacobian identities,
the blockwise influence-function assembly (unit-tested against a brute-force joint
sandwich to `1e-10`), the CR1 small-sample factor, the beta-block consistency gate
(relative deviation `3.2e-14`), the penalty-aware wage FOC, the KKT price-block
transform, the demeaning convention, the binding-bound handling, and the entire
Section 8 (formerly Section 5) arithmetic all reproduce exactly.

A small number of **documentation-lag** items were found --- none of them
algorithmic errors. They are collected and corrected in
[Section 9.1 (Current-state reconciliation)](#91-current-state-reconciliation-corrections-to-the-proposal-text).
The most consequential is that the default structural-SE source has flipped from
`draws` to `murphy_topel`; the proposal text below still says `draws` in two places,
and those statements are stale. Where the proposal's original text is retained
verbatim for completeness, the reconciliation table is the authority on current
behavior.

**Added 2026-07-01.** [Section 7](#7-verified-demand-block-se-equals-classical-clustered-2sls)
now carries (i) an *independent external* replication of the demand-block SEs against
`ivreg` + `sandwich::vcovCL(type = "HC1")` --- a code path disjoint from the pipeline,
agreeing to $\sim10^{-13}$ ([§7.2](#72-independent-external-replication-ivreg--sandwich-2026-07-01))
--- and (ii) a side-by-side derivation showing that the classical clustered 2SLS variance
is exactly the $\beta$ block of the Murphy--Topel sandwich, and that the structural block
is the textbook Murphy--Topel $V_1/V_2$ formula
([§7.3](#73-the-classical-2sls-and-murphytopel-formulas-side-by-side)).

---

# 1. Overview and status

**Status: IMPLEMENTED** (2026-06-10) in `07_vcov.R`. The script was renamed 2026-06-13
from `07c_murphy_topel.R` when it became the sole SE producer: it now also writes
`07_first_stage_vcov.rds`, replacing the retired `07_bootstrap.R`. The sandwich
assembly lives in `assemble_triangular_mt_vcov` and
`cluster_vcov_2sls(return_scores = TRUE)` in `utils/estimation_pipeline.R`, and is
unit-tested against a brute-force joint sandwich in
`tests/testthat/test_murphy_topel.R`.

- **Output:** `results/data/07_murphy_topel_vcov.rds`.
- **Reporting:** the structural rows take these SEs via
  `JMP_STRUCTURAL_SE_SOURCE=murphy_topel` (`08_display_estimates.R`).
  *Current default:* `murphy_topel` (see reconciliation item R1 --- the proposal's
  original "the default remains `draws`" is stale).

## Relationship to the retired draw-based procedure

Companion to `legacy/bootstrap_petrin_train.md`, which documents the draw-based
procedure formerly shipped in `07_bootstrap.R` (now retired to `legacy/`). That
procedure captured only the first-stage propagation term. Murphy \& Topel (1985) /
Newey \& McFadden (1994, Section 6) give the full asymptotic variance of a two-step
estimator, including the second stage's own conditional sampling variance and the
cross-stage covariance that arises because both stages are estimated on the SAME
sample. Karaca-Mandic \& Train (2003) is the nested-samples version; identical
samples is its special case.

---

# 2. Implementation conventions

Decisions made at implementation time. These override any conflicting text in the
original proposal sections that follow.

## 2.1 Small-sample factor

The joint sandwich uses
$$c = \frac{G}{G-1}\cdot\frac{N-1}{N-K_{\text{total}}}$$
with $K_{\text{total}}$ = all estimated parameters ($111 + 12 +$ free price coords)
--- the CR1 convention applied once, consistently, to the whole system. The saved
object also carries `vcov_cluster_only` ($c = G/(G-1)$). Consequence: the beta block
of the MT vcov equals the shipped analytical first-stage vcov times the *known* ratio
of adjustments (it uses $K_1 = 111$); `07_vcov.R` asserts that identity at `1e-8`
relative. Reported demand SEs in `08` stay the analytical first-stage ones either way.

*Code:* `07_vcov.R:543-545` (`k_total`, `adjustment`, `adjustment_cluster_only`),
`:601` (`vcov_cluster_only`), `:555-560` (beta-block assertion).

## 2.2 Penalty-aware wage FOC

When `JMP_WAGE_INTERIOR_PENALTY=true` (must match the 06 estimation run), the wage
estimating equations are the penalized score
$$h = J_{2w}'\,[\,\bar g_2 + (\lambda/2)\,u\,],$$
with the analytic penalty curvature entering the scores and Jacobians through
$$D = (\lambda/2)\,\operatorname{diag}\!\big(\text{pen}''(\text{share})\,(N/n_c)^2\big).$$
On the active branch ($\text{gap} = \log(\text{min\_share}) - \log(s) > 0$),
$$\text{pen}'(s) = -2\,\text{gap}/s, \qquad \text{pen}''(s) = 2\,(1+\text{gap})/s^2,$$
and both are zero when slack. Observed-share sampling noise enters through the
share-recovery
channel. With the penalty slack this reduces to a fixed nonsingular transform of the
raw moment system --- sandwich unchanged (unit-tested). `07_vcov.R` reports the active
penalty set and warns near the one-sided hinge kink.

*Code:* `07_vcov.R:471-524` (penalty branch), `:492` ($h$), `:488-490`
($\text{pen}''$, $u$, $D$), `:506-510` (transformed scores / Jacobians),
`:499-504` (near-kink warning). *Test:* `test_murphy_topel.R:109-134`.

## 2.3 Demeaning

The $g_2$ / $g_3$ per-row contributions are demeaned before cluster-summing (the wage
FOC sits at a basin floor, not exactly zero); the $g_1$ scores stay raw so the beta
block is exact.

*Code:* `07_vcov.R:438` (`s2` demeaned by `g2_mean`), `:441` (`s3` demeaned by
`colMeans(g3h)`), `:446` (`psi_b` raw from `first_stage$influence`).

## 2.4 Price block uses the KKT/gradient system, not the raw moments

The bounded L-BFGS-B step minimizes $\lVert\bar g_3\rVert^2$ subject to min-wage lower
bounds, so what it actually solves is $[(M'M)\,\bar g_3]_{\text{free}} = 0$ --- and
when a bound binds, $\bar g_3$ itself is NOT zero (its moment leaks from the binding
coordinate through $M'M$). `07_vcov.R` therefore uses
$$h_i = [(M'M)\,g_{3,i}]_{\text{free}}$$
as the price-stage estimating equations, with Jacobian
$-[(M'M)(M'M)]_{\text{free,free}}$ in $p_{\text{free}}$. When nothing binds this is a
fixed nonsingular transform of the moment system and the sandwich is exactly invariant
(unit-tested). Binding coordinates are treated as fixed at their bound and get NA SEs
(`08` keeps the draw-based SD for them); the remaining nonstandardness --- the
parameter-on-boundary distribution of the binding coordinate itself and the sampling
variability of $M'M$ interacting with $\bar g_3^{*}\neq 0$ --- is documented, not
corrected.

*Code:* `07_vcov.R:271-278` (binding detection, `free_p`), `:288-291`
(`MtM_full`, `C_free`, `MtM_h`, KKT gradient), `:439-441` (per-row $h$),
`:536-537` (`J_3w_h`, `J_3b_h`). *Test:* `test_murphy_topel.R:80-107`.
*(See reconciliation item R4: the proposal's "Cook 2018Q3 binds, max|mean g3| ~ 6"
illustration does not match the shipped run, where nothing binds and
max|mean g3| = 1.393.)*

## 2.5 Step sizes

- beta: $0.01\times$ analytical $\text{SE}_j$
- wage: $10^{-3}\times\max(|w_j|,\ \text{county parscale})$

Both scale with `JMP_MT_STEP_SCALE`. `JMP_MT_STEP_CHECK=true` re-runs the first columns
of each block at $2h$ and reports the disagreement. `JMP_MT_SMOKE=true` runs the base
evaluation + diagnostics + one column per block and exits without writing.

*Code:* `07_vcov.R:308-317` (steps), `:420-430` (2h probe), `:367-377` (smoke).

---

# 3. The estimator is a triangular stacked GMM

Per observation $i$ (rows of `estim_matrix`; cluster $g$ = `location_id`), the three
estimation stages solve the stacked estimating equations:

```
g1_i(beta)        = Xhat_i (y_i - X_i' beta)     (111: 2SLS normal eqns, Xhat = Pz X)
g2_i(w; beta)     = E_mat row i                  (12: wage moments, objective_gmm())
g3_i(p; w, beta)  = (p_adj_i(beta, w) - M_i' p) * M_i
                                                 (120: price moments, residual_moments)
```

with sum-FOCs $\sum_i g_{1,i} = 0$ (exact, by 2SLS), $\sum_i g_{2,i} \approx 0$ (wage
solve; 12 moments / 12 params, exactly identified), $\sum_i g_{3,i} \approx 0$
(interior coordinates of the L-BFGS-B price step; 120/120, OLS form). The joint
parameter is $(\beta, w, p)$, dimension $111 + 12 + 120 = 243$.

The joint Jacobian of the summed moments is **block lower-triangular** ($g_1$ doesn't
involve $(w, p)$; $g_2$ doesn't involve $p$):

```
J = [ -A      0      0     ]   A    = X' Pz X          (analytic, cluster_vcov_2sls bread)
    [ J_2b    J_2w   0     ]   J_2w = d sum(g2) / dw'   (12 x 12,  numerical)
    [ J_3b    J_3w  -M'M   ]   J_2b = d sum(g2) / dbeta'(12 x 111, numerical; sparse)
                              J_3w = d sum(g3) / dw'    (120 x 12, numerical)
                              J_3b = d sum(g3) / dbeta' (120 x 111, numerical; sparse)
                              M'M  analytic (price model-matrix crossprod)
```

Joint clustered meat: $S = \sum_g s_g s_g'$ with
$s_g = \sum_{i\in g}(g_{1,i}, g_{2,i}, g_{3,i})$ (243-vector per cluster). Then
$$V_{\text{joint}} = c\cdot J^{-1} S\, J^{-1\prime}$$
with the CR small-sample factor $c$. Murphy--Topel for $(w, p)$ is exactly the
lower-right block of $V_{\text{joint}}$. The upper-left block reproduces the analytical
clustered 2SLS vcov already shipped in `07_first_stage_vcov.rds` **identically** (the
system is triangular, so beta's marginal variance is unaffected by the later stages)
--- this is both how the proposal "exploits the analytical first stage" and a built-in
consistency check.

## 3.1 Influence-function form (what is actually computed)

No $243\times243$ inversion. Blockwise, per cluster $g$:

```
psi_b,g = A^{-1} s1_g
psi_w,g = -J_2w^{-1} (s2_g + J_2b psi_b,g)                 (12 x 12 solve)
psi_p,g = (M'M)^{-1} (s3_g + J_3b psi_b,g + J_3w psi_w,g)
V       = c * sum_g psi_g psi_g',   psi_g = (psi_b,g, psi_w,g, psi_p,g)
```

Reading the decomposition: $-J_{2w}^{-1} s_{2,g}$ is the conditional ("traditional
formulas") term, $-J_{2w}^{-1} J_{2b}\,\psi_{b,g}$ is the first-stage propagation term
(the population analogue of what the Petrin--Train draws estimate, since
$dw/d\beta' = -J_{2w}^{-1} J_{2b}$), and summing them *within* a cluster before the
outer product automatically delivers the Murphy--Topel cross-covariance terms that
same-sample estimation requires.

Bonus: $V_{\text{joint}}$ also yields $\operatorname{Cov}(\hat\beta,\hat\theta)$ ---
needed later for delta-method SEs on counterfactual objects, which the draw procedure
does not provide.

*Code:* `assemble_triangular_mt_vcov` at `utils/estimation_pipeline.R:258-269`.

---

# 4. Where each piece comes from in this codebase

| Piece | Source | Cost |
|:----------|:------------------------------------------------|:----------|
| `A`, `s1_g`, `psi_b,g` | extend `cluster_vcov_2sls()` to also return per-cluster score sums (already forms `rowsum(x_hat * residuals, cluster)`) | free |
| `g2_i` rows at point estimates | `objective_gmm(theta = w_hat, x = estim_matrix, beta = beta_hat, ...)` returns the $N\times12$ per-row matrix directly | 1 moment pass |
| `g3_i` rows | rebuild `p_adj` via `add_price_adjustment()` + `M = model.matrix(xnam)` exactly as `fit_price_parameters()` does; `g3 = (p_adj - M %*% p_hat) * M` | 1 gamma pass |
| `M'M` | analytic | free |
| `J_2w, J_3w` | central differences in the 12 wage coords; each perturbation evaluates **both** `objective_gmm` ($g_2$ sums) and the `p_adj` / $g_3$ sums in one pass | 24 passes |
| `J_2b, J_3b` | central differences in the **78 relevant** beta coords (75 `cnty:avg_labor:B_*` + 3 `cnty:cust_price`); the 33 county$\times$quarter demand FEs never enter stage 2, so those columns are identically zero --- assert with one spot-check perturbation | 156 passes |

Total: **~181 moment-evaluation passes.** From the smoke run, one pass (equilibrium
solves + gamma inversion over the full sample, warm-started cache) costs ~1--2 min
single-threaded, i.e. ~3--6 h serial in one Slurm job, or trivially parallel over
perturbation columns with `mclapply` (`JMP_MT_WORKERS`). No array needed.

*Zero-column argument:* `build_cost_matrix` greps only `:avg_labor:B` and
`:cust_price$`, and `p_adj` uses beta only through $1/\rho$, so the demand FE columns
of $J_{2b}$ / $J_{3b}$ are identically zero. `07_vcov.R:339-346` asserts this with one
spot-check perturbation (`stopifnot(spot_max < 1e-6)`).

*(See reconciliation item R3: the `07_vcov.R` header comment's "~367 passes" is ~2x
too high; the ~181 here is correct, and the script's own progress message prints
"180 passes".)*

## 4.1 Numerical-derivative hygiene

- Step size $h_j = 0.01\times\text{SE}_j$ (analytic SE for beta coords; for wage coords
  the actual rule is $10^{-3}\times\max(|w_j|,\text{parscale})$ --- see 2.5): far above
  the solver noise floor (innertol `1e-10` / outertol `1e-8` gives moment noise ~`1e-7`
  on $O(1)$ moments), far below the scale of nonlinearity. Re-run a column at $2h$ and
  compare (Richardson check); the code prints the relative disagreement for the first
  three columns of each block (it reports the number rather than auto-flagging a 5%
  threshold).
- Run evaluation passes with `JMP_USE_STAGED_SOLVER_TOLERANCES=false` and the tight
  default tolerances so the moment function is as smooth as the solver allows
  (`07_vcov.R` hardcodes this in `mt_config`, `:69`). Keep the solver **cache ON**
  (perturbations of this size are far above the 12-digit cache rounding); solver
  **warm starts are deliberately OFF** in `07_vcov.R` (`:70`) so that $+h$ / $-h$
  re-solves are symmetric and the $g_2$-vs-`objective_gmm` faithfulness check holds
  bit-for-bit.

---

# 5. Caveats --- read before trusting the output

1. **Local linearization vs. NYC's multi-basin wage surface.** MT is a delta method:
   it linearizes $\theta(\beta)$ at $\hat\beta$. The draw-based reps show
   $\theta(\beta)$ is violently nonlinear over the $\pm2$ SE range of NYC's weakly
   identified $\rho$ (sign flips ~1 draw in 6, basin switching --- see the NYC weak-ID
   notes). MT will be smooth and likely *smaller* than the draw-based SD exactly where
   the draws blow up. That is not a bug in either number: MT = all variation under the
   local asymptotic approximation; draws = global first-stage propagation without the
   conditional term. **Report both.** Where they agree (Cook, most LA params), MT's
   added conditional term is the better, more complete number; where they diverge (NYC
   $\rho$-adjacent params), neither asymptotic story is trustworthy and the divergence
   itself is the finding. *The propagation-only term is computed at `07_vcov.R:565-567`
   and saved per wage parameter as `wage_summary$se_first_stage_propagation`.*
2. **Wage FOC is not exactly zero.** The wage solver exits at a basin floor (NYC ssq
   ~`1.3e-3`), so $\sum_i g_{2,i}\neq 0$ exactly. The omitted second-order term in $J$
   is $O(\lVert\bar g_2\rVert\cdot\lVert d^2 g\rVert)$. Mitigation: demean the per-row
   $g_2$ contributions before cluster-summing (standard misspecification-robust meat),
   and report $\lVert\bar g_2\rVert$ alongside the SEs (`07_vcov.R:224`).
3. **Binding lower bounds in the price step.** L-BFGS-B enforces min-wage lower bounds;
   a coordinate of $\hat p$ sitting ON its bound has no interior FOC and must be dropped
   from the $p$ block (treated as fixed) with a note. `07_vcov.R:271` checks
   $\hat p - \text{lower\_bound}$ at the point estimates first; if many bind, the $p$
   block needs inequality-constrained theory (out of scope).
4. **$G$ clusters (84 in the production run).** The joint meat $S$ has rank
   $\le G < 243$. Diagonal SEs are fine; joint Wald tests across many parameters are
   not. Use $t(G-1=83)$ critical values. ($G$ is data-derived from `location_id`, not
   hardcoded.)
5. **Bound-guard scoring.** The wage solver's accept/revert gates use a bound-guarded
   score (moments + $10\times$violations$^2$), but the estimating equations are the 12
   moments; at an accepted interior solution the guard contributes nothing. The current
   code does **not** reuse `structural_bound_diagnostics` for this (that was the
   original proposal's suggestion); instead it implements the more complete
   penalty-aware wage FOC of Section 2.2 for the *interior-share* penalty, emitting its
   own active-set and near-kink diagnostics (`07_vcov.R:493-504`). See reconciliation
   item R5.

---

# 6. Wiring

- Script `07_vcov.R` (originally `07c_murphy_topel.R`): reads
  `04_estimation_sample.rds`, `06_parameters.rds`, `07_first_stage_vcov.rds`
  (cross-checked if present); writes `results/data/07_first_stage_vcov.rds` (it is now
  the producer) and `results/data/07_murphy_topel_vcov.rds`. *(The exact saved-object
  schema is in [Section 9.2](#92-exact-saved-object-schema); it is richer than the
  original proposal's `list(V_joint, se, blocks, diagnostics)` sketch --- see
  reconciliation item R2.)*
- `cluster_vcov_2sls()` gains a `return_scores = FALSE` arg (per-cluster score sums +
  influence rows) --- additive, no behavior change.
- `08_display_estimates.R`: `JMP_STRUCTURAL_SE_SOURCE = draws | murphy_topel`; when
  `murphy_topel`, structural rows take MT SEs and the message reports the max relative
  gap vs the draw-based SD (only over parameters that have both a valid MT SE and a
  valid draw SD; with the draws legacy retired this set is typically empty). *Current
  default: `murphy_topel` (item R1).*
- Validation gates:
  (a) $V(\beta)$ block $==$ `07_first_stage_vcov.rds` to fp tolerance --- asserted in
      `07_vcov.R:555-560` (after dividing by the known adjustment ratio);
  (b) propagation-only term $(dw/d\beta')\,V_1\,(dw/d\beta')'$ with
      $dw/d\beta' = -J_{2w}^{-1} J_{2b}$ --- computed at `:565-567` (compared to the
      draw reps externally, not by a `stopifnot`);
  (c) Richardson step-size checks --- opt-in via `JMP_MT_STEP_CHECK`;
  (d) no (or explicitly handled) binding price bounds --- handled at `:271-278`.

---

# 7. Verified: demand-block SE equals classical clustered 2SLS

The demand ($\beta$) block of the MT vcov *is*, by construction, the classical
cluster-robust 2SLS variance clustered at `location_id`. Three things establish this:
an internal consistency gate against the pipeline's own analytical first stage
([§7.1](#71-internal-consistency-gate-run-54207749-2026-06-13)), an **independent
external** replication against `ivreg` + `sandwich::vcovCL` that shares no code with
the pipeline ([§7.2](#72-independent-external-replication-ivreg--sandwich-2026-07-01)),
and the exact mathematical relationship between the two variance formulas
([§7.3](#73-the-classical-2sls-and-murphytopel-formulas-side-by-side)).

## 7.1 Internal consistency gate (run 54207749, 2026-06-13)

Confirms validation gate (a) numerically on the production run
(`results/data/07_murphy_topel_vcov.rds`, created 2026-06-12 22:12;
`07_first_stage_vcov.rds`). Sample: $N = 468$, $G = 84$ clusters (`location_id`),
$K_{\text{total}} = 243$. *(This section is an explicitly dated historical snapshot;
its raw numbers describe that run, not necessarily the current one.)*

- **Raw sandwich identical.** Stripping each routine's CR1 factor, the demand
  `cust_price` SEs agree to ~`1e-14` relative (Cook / NYC / LA un-adjusted SE
  `0.009621 / 0.038101 / 0.006378` from both MT and 2SLS). The beta block of the MT
  vcov *is* the analytical first-stage vcov --- no estimation difference.
- **Reported MT SEs differ by a known constant $1.260\times$.** This is purely the CR1
  dof factor $c = \tfrac{G}{G-1}\tfrac{N-1}{N-K}$: 2SLS uses $K_1 = 111$
  ($c = 1.32388$), MT uses $K_{\text{total}} = 243$ ($c = 2.10056$); the SE ratio is
  $\sqrt{2.10056/1.32388} = 1.2596$. So MT reports Cook / NYC / LA price SEs
  `0.013944 / 0.055221 / 0.009244` vs 2SLS `0.011070 / 0.043839 / 0.007339`.
- **No effect on published numbers.** Per the small-sample-factor convention (2.1),
  `08` reports the analytical first-stage (2SLS) demand SEs regardless of
  `JMP_STRUCTURAL_SE_SOURCE`; the $1.26\times$ inflation lives only in the raw joint
  vcov object and never reaches the demand rows. *Caveat for anyone reading SEs straight
  off `07_murphy_topel_vcov.rds$se`:* the demand entries carry the $K_{\text{total}}$
  dof factor, so they run ~$1.26\times$ above a standalone 2SLS report --- e.g. Cook
  $t = -2.08$ there vs $-2.62$ under the demand block's own $K_1$ dof.

## 7.2 Independent external replication (`ivreg` + `sandwich`, 2026-07-01)

§7.1 checks the MT $\beta$ block against the pipeline's *own* analytical first stage
(`cluster_vcov_2sls`). That is an internal identity: both sides run the same code. This
section closes the loop with a **fully independent** implementation of clustered 2SLS
that shares no code with the pipeline --- `ivreg::ivreg` for the point estimates and
`sandwich::vcovCL(fit, cluster = location_id, type = "HC1")` for the CR1 cluster-robust
variance --- and compares it to the **saved** outputs of `07_vcov.R` on the current run
($N = 468$, $G = 84$, $K_1 = 111$ all identified, $K_{\text{total}} = 243$).

| Comparison | Metric | Result |
|:--|:--|:--|
| Point estimates $\hat\beta$: `ivreg` vs `07` | max abs diff | $1.6\times10^{-11}$ |
| **`07_first_stage_vcov.rds` SE** vs `vcovCL(HC1)` | max **rel** SE diff | $\mathbf{7.9\times10^{-13}}$ |
| same, full vcov block | max rel diff | $2.6\times10^{-13}$ |
| same, SE | median rel diff | $3.0\times10^{-14}$ |
| `07_murphy_topel_vcov.rds` $\beta$ block, raw | max rel SE diff | $0.260$ (the dof factor) |
| same, after $\div\sqrt{c/c_1}$ | max rel SE diff | $9.0\times10^{-13}$ |

So the shipped `07_first_stage_vcov.rds` (self-labeled `type = "analytical_clustered_2sls"`,
`estimator = "2sls"`) reproduces textbook CR1 clustered 2SLS SEs **to floating-point
precision** against an outside implementation. The MT-file $\beta$ block reproduces the
*same* reference once its deliberately-more-conservative $K_{\text{total}}$ dof factor is
removed --- the raw $26\%$ gap is entirely $\sqrt{c/c_1}=1.2596$ (§7.1), not an
estimation difference. The wage (12) and price (120) blocks have no classical-2SLS
analog and are excluded from this comparison; they are the genuinely two-step object of
§7.3. *(Reproduce with `pandoc`-independent script; the check imports nothing from
`utils/estimation_pipeline.R`.)*

## 7.3 The classical 2SLS and Murphy--Topel formulas, side by side

Both are cluster-robust sandwiches on the same $G=84$ `location_id` clusters; they differ
only in *how many estimating equations are stacked*.

**Classical clustered 2SLS (CR1).** For $y_i = X_i'\beta + u_i$ instrumented by $Z_i$,
with projection $\hat X = P_Z X$, $P_Z = Z(Z'Z)^{-1}Z'$,
$$\hat\beta = A^{-1}\hat X'y,\qquad A \equiv \hat X'X = X'P_Z X,\qquad u_i = y_i - X_i'\hat\beta.$$
Per-cluster score sums $s_{1,g}=\sum_{i\in g}\hat X_i\,u_i$ and influence
$\psi_{b,g}=A^{-1}s_{1,g}$ give
$$V^{\text{2SLS}}_\beta \;=\; c_1\,A^{-1}\Big(\sum_g s_{1,g}s_{1,g}'\Big)A^{-1\prime}
\;=\; c_1\sum_g \psi_{b,g}\psi_{b,g}',\qquad
c_1=\frac{G}{G-1}\cdot\frac{N-1}{N-K_1}.$$
This is exactly `cluster_vcov_2sls()`, `sandwich::vcovCL(., type="HC1")`, and
`07_first_stage_vcov.rds`.

**Murphy--Topel (this pipeline).** Stack the three stages and form the joint per-cluster
influence $\psi_g=(\psi_{b,g},\psi_{w,g},\psi_{p,g})$:
$$\psi_{b,g}=A^{-1}s_{1,g},\quad
\psi_{w,g}=-J_{2w}^{-1}\!\big(s_{2,g}+J_{2b}\,\psi_{b,g}\big),\quad
\psi_{p,g}=(M'M)^{-1}\!\big(s_{3,g}+J_{3b}\,\psi_{b,g}+J_{3w}\,\psi_{w,g}\big),$$
$$V^{\text{MT}}=c\sum_g\psi_g\psi_g',\qquad c=\frac{G}{G-1}\cdot\frac{N-1}{N-K_{\text{total}}}.$$

**How they line up.**

- *The $\beta$ block is the same object.* Triangularity makes $\psi_{b,g}$ independent of
  $(w,p)$ --- it is the standalone-2SLS influence, unchanged. Hence
  $$V^{\text{MT}}_{\beta\beta}=c\sum_g\psi_{b,g}\psi_{b,g}'=\frac{c}{c_1}\,V^{\text{2SLS}}_\beta,
  \qquad \frac{c}{c_1}=\frac{N-K_1}{N-K_{\text{total}}}=\frac{468-111}{468-243}=1.5867.$$
  The two matrices are identical up to that scalar; strip $c,c_1$ and they agree to
  $\sim10^{-13}$ (§7.1, §7.2). This is why MT "exploits the analytical first stage" and
  why gate (a) can be an exact `stopifnot`.

- *2SLS is MT with no second stage.* Delete the $g_2,g_3$ rows and $V^{\text{MT}}$
  collapses to $V^{\text{2SLS}}_\beta$; MT is the strict generalization that appends the
  structural blocks.

- *The structural block is the textbook Murphy--Topel $V_1/V_2$ formula.* Split the wage
  influence into a conditional and a propagation piece,
  $$\psi_{w,g}=\underbrace{-J_{2w}^{-1}s_{2,g}}_{\text{conditional (}\beta\text{ known)}}
  \;\underbrace{-\,J_{2w}^{-1}J_{2b}\,\psi_{b,g}}_{\text{first-stage propagation}},
  \qquad \frac{dw}{d\beta'}=-J_{2w}^{-1}J_{2b}.$$
  With $V_1=\sum_g\psi_{b,g}\psi_{b,g}'$ and
  $V_2=J_{2w}^{-1}\big(\sum_g s_{2,g}s_{2,g}'\big)J_{2w}^{-1\prime}$, the clustered outer
  product expands to
  $$\sum_g\psi_{w,g}\psi_{w,g}'=
  \underbrace{V_2}_{\text{conditional}}
  +\underbrace{\frac{dw}{d\beta'}\,V_1\,\frac{dw}{d\beta'}^{\!\prime}}_{\text{propagation}}
  -\,2\,\operatorname{sym}\!\Big(J_{2w}^{-1}\big(\textstyle\sum_g s_{2,g}\psi_{b,g}'\big)
  \tfrac{dw}{d\beta'}^{\!\prime}\Big),$$
  where $\operatorname{sym}(B)=\tfrac12(B+B')$. This is Murphy \& Topel's (1985)
  $$\operatorname{Var}(\hat w)=V_2+V_2\big[\,C\,V_1\,C'-C\,V_1\,R'-R\,V_1\,C'\,\big]V_2,$$
  with $C\leftrightarrow$ the $\beta$-derivative of the wage score ($J_{2b}$) and
  $R\leftrightarrow$ the within-cluster covariance of the wage and demand scores (the
  third term above). The propagation term $\tfrac{dw}{d\beta'}V_1\tfrac{dw}{d\beta'}'$
  *alone* is what the retired draw-based procedure estimated (saved as
  `wage_summary$se_first_stage_propagation`); the cross-covariance $R$ --- the piece that
  vanishes only under *independent* samples (Karaca-Mandic \& Train's nested case) ---
  is delivered for free by summing $s_{2,g}$ and $\psi_{b,g}$ *within a cluster before*
  the outer product. That single same-sample correction is the entire content of
  Murphy--Topel over the naive "draw $\beta$, re-estimate" variance, and $\psi_{p,g}$
  carries the identical decomposition with propagation from **both** $\beta$ and $w$.

---

# 8. (reserved)

*This numbering gap is intentional: Sections 3--7 above correspond to the original
proposal's Sections 1--5. The additional material follows.*

---

# 9. Additional details (from the current code)

## 9.1 Current-state reconciliation (corrections to the proposal text)

Ranked by severity. "Doc" = `docs/murphy_topel_proposal.md`.

| ID | Severity | Item | Correct current behavior |
|:--|:--------|:--------------------------|:--------------------------------|
| R1 | substantive | Doc (lines 14, 206) says the default SE source "remains `draws`". | `config.R:507` defaults `JMP_STRUCTURAL_SE_SOURCE` to `murphy_topel`; with the env var unset, `08_display_estimates.R:183` takes the MT branch. Default is **`murphy_topel`**. |
| R2 | substantive | Doc (Section 4) sketches the output as `list(V_joint, se, blocks=list(A, ...), diagnostics=...)`. | No field `V_joint` (it is `vcov`); `blocks` does **not** contain `A`; there is no `diagnostics` field (diagnostics live in `foc`, `wage_penalty`, `binding_price_coords`, `steps`, `J_2w_condition`, ...). Full schema in 9.2. |
| R3 | minor | `07_vcov.R:28-30` header comment estimates "~367 cold passes". | The design evaluates both $g_2$ and $g_3$ in one equilibrium pass, so $90$ coords $\times 2 = 180$ Jacobian passes (+ a few base/faithfulness). The script's own message prints "180 passes"; the proposal's ~181 is correct. |
| R4 | minor | Price-block convention cites "Cook 2018Q3's `avg_labor` binds; observed max\|mean g3\| ~ 6". | The shipped run has **0 binding** price coords and **max\|mean g3\| = 1.393** (on a task-mix coefficient). The KKT machinery is generic and correct; only the illustrative production fact is stale. |
| R5 | superseded | Doc caveat 5 proposes reusing `structural_bound_diagnostics`; doc Section 2 (hygiene) proposes a "draw-based SD" wage step. | Both are original-proposal text overridden by the Implementation Conventions: the wage step is $10^{-3}\max(|w|,\text{parscale})$ (2.5), and the interior-share penalty is handled by the penalty-aware FOC (2.2), not `structural_bound_diagnostics`. |
| R6 | cosmetic | Internal runtime/submit drift: header says "~3--4 min/pass" and a 12-core/16 GB/24 h `sbatch`; Section 4 says "~1--2 min/pass" and 1-core/4 GB/12 h. `07_vcov.R:54` still names the script `07c_murphy_topel.R` in a `stop()` string. | Use the parallel spec (`JMP_MT_WORKERS`, multi-core) for production; timings are hardware-dependent. Cosmetic only. |

## 9.2 Exact saved-object schema

`readRDS("results/data/07_murphy_topel_vcov.rds")` returns a list with:

| Field | Contents |
|:----------------------|:----------------------------------------------------------|
| `vcov` | $243\times243$ joint MT vcov (CR1 factor with $K_{\text{total}}$). |
| `vcov_cluster_only` | same, rescaled to $c = G/(G-1)$. |
| `se` | full named SE vector (demand + wage + price), binding price coords = NA. Assembled **positionally** (demand FEs share names with their price/B twins). |
| `se_structural` | wage + price SEs only (unique names; `08` merges on this). |
| `wage_summary` | `data.table(parm_name, estimate, se_mt_total, se_first_stage_propagation)`. |
| `blocks` | `list(J_2w, J_2b, J_3w, J_3b, J_2w_use, J_2b_use, C_free, MtM_h, MtM_full, dw_dbeta)`. |
| `wage_penalty` | penalty record: `enabled`, and when on `lambda, min_share, share, gap, active, u, Dvec, h_norm`. |
| `foc` | `list(g2_mean, g3_mean, price_kkt)`. |
| `binding_price_coords` | names of price coords on their bound (NA SE). |
| `steps` | `list(h_wage, h_beta)`. |
| `cluster_variable`, `n_obs`, `n_clusters`, `k_total` | `"location_id"`, $N$, $G$, $K_{\text{total}}$. |
| `adjustment`, `adjustment_cluster_only` | the two CR1 factors. |
| `beta_block_rel_dev` | relative deviation of the beta block from the analytical vcov (~`3.2e-14`). |
| `J_2w_condition`, `J_2w_use_condition` | condition numbers of the raw and as-used wage-block Jacobians. |
| `created_at` | timestamp. |

`readRDS("results/data/07_first_stage_vcov.rds")` returns (field names mirror the
retired bootstrap output for drop-in compatibility): `beta`, `vcov`, `se`,
`cluster_variable`, `n_clusters`, `n_obs`, `rank`, `small_sample_adjustment`,
`type = "analytical_clustered_2sls"`, `estimator = "2sls"`.

A checkpoint of the expensive Jacobian is also written (under `results/data/`) to
`07_murphy_topel_jacobian_checkpoint.rds`
(`list(tasks, cols, h_wage, h_beta, free_p, binding)`) so a failure in assembly/save
never costs a recompute (`07_vcov.R:389-391`).

## 9.3 Configuration knobs

Columns: environment variable, default, and effect (with the `CONFIG` field name in
italics).

| Env var | Default | Effect (`CONFIG` field) |
|:--------------------------------|:-----------|:-------------------------------------|
| `JMP_STRUCTURAL_SE_SOURCE` | `murphy_topel` | *structural_se_source* --- which structural SEs `08` reports (`draws` uses retired reps). |
| `JMP_MT_WORKERS` | `1` | *mt_workers* --- `mclapply` fork count over Jacobian columns. |
| `JMP_MT_STEP_SCALE` | `1` | *mt_step_scale* --- multiplier on both finite-difference steps. |
| `JMP_MT_STEP_CHECK` | `false` | *mt_step_check* --- re-run first 3 columns/block at $2h$; print rel disagreement. |
| `JMP_MT_SMOKE` | `false` | *mt_smoke* --- base + diagnostics + one column/block, then exit without writing. |
| `JMP_WAGE_INTERIOR_PENALTY` | `false` | *wage_interior_penalty_enabled* --- must match the 06 run; turns on the penalized wage FOC. |
| `JMP_WAGE_INTERIOR_PENALTY_WEIGHT` | `1` | *wage_interior_penalty_weight* --- $\lambda$. |
| `JMP_WAGE_INTERIOR_PENALTY_MIN_SHARE` | `1e-3` | *wage_interior_penalty_min_share* --- share floor. |
| `JMP_MIN_OPTIM_PARSCALE_WAGE` | `20` | *min_optim_parscale_wage* --- default wage parscale (county overrides in `min_optim_parscale_wage_by_county`). |

`07_vcov.R` additionally forces, in a local `mt_config` copy (`:68-71`),
`use_staged_solver_tolerances = FALSE`, `use_solver_warm_starts = FALSE`,
`pl_on = FALSE` (row-level parallelism off; parallelize across columns instead). It
also stops unless `skill_monotone_orientation == "none"` (`:53-56`): the 2SLS sandwich
does not apply to the constrained demand estimator.

## 9.4 Function signatures (`utils/estimation_pipeline.R`)

```r
cluster_vcov_2sls(x, z, y, beta, cluster,
                  tolerance = sqrt(.Machine$double.eps),
                  context = "2SLS clustered vcov",
                  return_scores = FALSE)
# -> list(vcov, se, n_obs, n_clusters, rank, adjustment)
#    + scores, influence  when return_scores = TRUE
#    scores   = rowsum(x_hat * residuals, cluster)   (G x K, per-cluster s1_g)
#    influence= A^{-1} s1_g rows,  A = crossprod(x, x_hat) = X' Pz X

assemble_triangular_mt_vcov(psi_b, s2, s3,
                            J_2w, J_2b, J_3w, J_3b, MtM,
                            adjustment = 1,
                            tolerance = sqrt(.Machine$double.eps))
# -> list(vcov, se, psi)
#    psi_w = -(J_2w)^{-1}(s2 + J_2b psi_b);  psi_p = (MtM)^{-1}(s3 + J_3b psi_b + J_3w psi_w)
#    vcov  = adjustment * crossprod(cbind(psi_b, psi_w, psi_p));  MtM is the NEGATIVE of dg3/dp
```

## 9.5 Unit-test inventory (`tests/testthat/test_murphy_topel.R`)

1. **`return_scores` influence rows reproduce the clustered vcov exactly** ---
   `vcov == adjustment * crossprod(influence)` to `1e-10`; 2SLS FOC scores sum to ~0;
   default call returns no scores.
2. **Triangular assembly equals the brute-force joint sandwich** --- builds the explicit
   $J$, $S = \operatorname{crossprod}(\text{cbind}(s_1,s_2,s_3))$,
   $V_{\text{ref}} = c\,J^{-1} S J^{-1\prime}$, and checks `mt$vcov == V_ref` and the
   beta block $== c\,\operatorname{crossprod}(\psi_b)$, all to `1e-10`.
3. **Third-block sandwich invariant to a fixed nonsingular transform (KKT)** --- with
   nothing binding, $C = M'M$ gives $s_3\to s_3 C'$, $J_{3*}\to C J_{3*}$,
   $-M'M\to -C\,M'M$, and $V$ is unchanged to `1e-9`.
4. **Wage-block score transform with slack penalty ($D=0$) leaves the sandwich
   unchanged** --- $s_2\to s_2 J_{2w}$, $J_{2w}\to J_{2w}'J_{2w}$,
   $J_{2b}\to J_{2w}'J_{2b}$; $V$ unchanged to `1e-9`.
5. **Assembly validates cluster alignment and dimensions** --- misaligned cluster
   rownames raise an `"identical"` error.

## 9.6 Execution and determinism

- **Solver settings** (per 4.1 / 9.3): staged tolerances OFF, warm starts OFF, cache
  ON, single-thread BLAS. Cold solves are deterministic in their inputs, so the only
  finite-difference error left is truncation, and $+h$ / $-h$ passes are symmetric.
- **Parallelism:** columns are independent; set `JMP_MT_WORKERS` (`mclapply`,
  `mc.preschedule = FALSE`). Row-level parallelism is disabled inside a pass.
- **Faithfulness checks run every time:** `base$g2` vs `objective_gmm` (`< 1e-6`),
  reconstructed `p_adj` vs `add_price_adjustment` (`< 1e-6`), recomputed $\beta$ vs the
  06 file (`< 1e-6`), the zero-column spot check (`< 1e-6`), and the on-disk first-stage
  cross-check (`< 1e-8`).
- **Suggested submission (parallel):**
  ```bash
  sbatch -p general -n 1 --cpus-per-task=12 --mem=16g -t 1-00:00:00 \
    -J 07_vcov -o logs/mt_%j.out \
    --wrap 'module load r/4.4.0; export JMP_MT_WORKERS=12 OMP_NUM_THREADS=1 \
            OPENBLAS_NUM_THREADS=1 MKL_NUM_THREADS=1; \
            Rscript -e "source(\"renv/activate.R\"); source(\"07_vcov.R\")"'
  ```

## 9.7 Current FOC / diagnostics snapshot

**Snapshot date: 2026-07-14, post-relabel point estimates** (`06_parameters.rds` from the
worker-type relabel re-run, commit `69281bd`; FOC norms from the `07_vcov.R` base moment
evaluation, run `58869073`, single-thread, tight tolerances). **Status:** these are the FOC
norms at the current point estimates; the Murphy-Topel vcov itself has **not** been
recomputed post-relabel --- run `58869073` was cancelled during the Jacobian pass, so the
shipped `results/data/07_murphy_topel_vcov.rds` (2026-07-03) is a **stale, pre-relabel**
object and must be regenerated before the SEs are quoted. The `07_first_stage_vcov.rds`
demand block (2026-07-14) is current.

- $\lVert\overline{g_2}\rVert = 0.0359$ (the wage moment floor --- the wage solver exits at
  a basin floor, not exactly zero, as caveat 2 anticipates; this floor is looser than the
  pre-relabel $3.05\times10^{-5}$, consistent with the current point estimates not clearing
  every county's wage block to machine zero).
- $\max|\overline{g_3}| = 1.393 \to 6.116$; **four price coordinates now bind** their lower
  bound and are excluded from the $p$ block (NA SE):
  `avg_labor:factor(county)17031:factor(quarter_year)2018.1`,
  `...36061:...2018.1`, `...36061:...2018.2`, `...17031:...2018.3`. The retained $p$ block
  is therefore $120-4 = 116$ and $K_{\text{total}} = 111+12+116 = 239$ (down from $243$ when
  nothing bound pre-relabel).
- The beta-block cross-check against the analytical stage-1 vcov (`beta_block_rel_dev`) is
  computed during MT assembly and is **pending** the completed post-relabel Jacobian run.
- Nonzero raw price moments are expected once a bound binds: the price KKT gradient on the
  free coordinates is reported alongside `max|mean g3|` for context (here max $|\cdot| =
  78.31$ on the free coords vs raw $\max|\overline{g_3}| = 6.116$).

**Default SE sources (current pipeline):** the structural (wage/price) SEs consumed by
`08_display_estimates.R` default to the **Murphy-Topel sandwich** (`JMP_STRUCTURAL_SE_SOURCE`
unset $\Rightarrow$ `murphy_topel`, config.R); the legacy Petrin-Train across-rep draws
(`=draws`) are retained only as a fallback. The demand block defaults to the analytical
clustered 2SLS vcov from `07_vcov.R` (`07_first_stage_vcov.rds`).

*These are diagnostic FOC norms, not SEs; they document how close the point estimates
sit to the exact estimating-equation zeros and confirm the assumptions behind the
demeaning and KKT conventions.*

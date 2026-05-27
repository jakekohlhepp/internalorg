# LA wage-moment floor: not under-identification, structural model misfit

> **Update (after the joint-estimation-experiment branch).** The "structural
> floor" diagnosis below was wrong. The floor is an artifact of using
> `nleqslv` (a root-finder) as the wage-stage solver. With `optim`
> Nelder-Mead minimizing `||g||^2` directly, the LA wage block can reach
> ssq = 0.002 from a (0,0,0,0) cold start with sensible wages
> (72, 8, 34, 163), versus nleqslv's ssq = 0.147 with extreme wages of
> magnitude 10^7. See `tests/wage_stage_minimizer_check.R` and the
> "Wage-stage solver should be a minimizer, not a root-finder" addendum
> at the bottom of this note. The body below records the diagnosis as
> originally written; treat it as a worked example of how a poor
> optimizer choice masquerades as model failure.

---

**Companion to** `rank_aware_solve_truncation_bug.md`. After the
`rank_aware_solve` fix, the wage-stage convergence diagnostics showed
that the LA wage solve still stalled or got rejected from any starting
point we tried (warm-start, cold-start zeros, cold-start with various
perturbations). This note documents what's actually happening: it is
not a bug or under-identification, but a structural floor on the LA
wage moment objective coming from the worker-3 share in particular.

## Numerical evidence

Sweeping LA wage-3 (`factor(county)6037:avg_labor:E_raw_3`) while
holding the other LA wages at modest values, the LA worker-3 wage
moment (`mean over LA salons of model E_3 - observed E_3`) behaves as
follows:

| LA wage_3   | E_3 moment | Mean model E_3 (per LA salon) |
|-------------|------------|-------------------------------|
| -10000      | +0.362     | 0.977 (worker 3 dominant)     |
| -1000       | +0.349     | 0.977                         |
| -100        | +0.277     | active region                 |
| 0           | -0.090     | 0.178                         |
| **50**      | **-0.188** | ~0                             |
| 100, 500, 1000, 1e4, 1e5, 1e6 | **-0.188 (locked)** | 0 (one unique value across salons) |

For wage_3 ≳ 50, worker 3 is priced out of the LA cost matrix entirely
(the model assigns zero share to worker 3 at every LA salon), so the
moment becomes
```
(0 - mean observed E_3) = -0.188
```
and stays there forever. Both nleqslv warm-start (LA wage_3 ≈ 234) and
some cold-start perturbations placed wage_3 in this saturated
region; from there nleqslv sees a numerically zero gradient on wage_3
and gives up, often after taking a step that worsened the score and
getting rejected by the bound-guarded acceptance gate.

There IS a finite optimum near wage_3 ∈ [-100, 0] where the moment
crosses zero, but in the joint (wage_2, wage_3, wage_4, wage_5) ssq
the best basin sits at roughly (200, 0, 100, 0) with ssq ≈ 0.053.
Multiple parameter configurations achieve essentially the same value
(warm-start at (8603, 234, 12784, 92) gave ssq ≈ 0.057), so the
basin is wide and flat.

## Where the 0.05 floor comes from

At any LA wage_3 in the saturated region, the E_3 moment alone
contributes `(-0.188)² ≈ 0.0354` to ssq. The remaining ~0.018 comes
from the other three workers. The model-implied LA worker-3 share
saturates at zero (or near-zero per salon) once wage_3 exceeds ~50,
while the **observed** mean LA worker-3 share is **0.342**. The model
cannot reach that observed level under the entropy-regularised
allocation given the LA cost matrix produced by the corrected
demand-IV stage. So:

- Even at the global ssq minimum, the LA wage stage cannot zero its
  moment vector. There is an irreducible misfit of ~0.19 on the
  worker-3 moment.
- This is **not** a numerical or identification bug; it is a
  model-vs-data tension that the wage stage cannot reconcile.

## Why this didn't show up before

Before the `rank_aware_solve` truncation bug was fixed, the demand-IV
beta for LA produced a degenerate cost matrix in which worker 1 was
cheapest at every task (corner entropy = 0). Under that buggy cost
matrix the wage stage fit one specific multiplicity that drove most
moments roughly to zero by giving wages whatever values the
near-singular system produced. After the fix, the cost matrix is
sensible (mean s_bound ≈ 0.88 in LA, only 1/240 above-bound), but the
worker-3 share predicted by the model can no longer be inflated to
match the data without re-introducing the very pathology that the fix
removed.

## What this means for the estimation

- The wage-stage objective for LA has a global minimum that is
  meaningful (not at infinity, not at degeneracy) but at a non-zero
  moment value. nleqslv's "did not converge" status code on LA is
  honest: the moment system has no zero in the feasible region.
- The structural-bound diagnostics in 06 are clean (LA: 1/240
  above bound, mean violation 0.0007) and the gamma-inversion in 09
  produces 4 zeros out of 512 LA salon-quarters (down from 484/512
  pre-fix). So the misfit on E_3 is local to that one moment, not a
  return of the broader degeneracy.
- The downstream substitution and counterfactual scripts use the
  fitted parameters as-is. They should be re-run after the fix
  (already done for 08-12; counterfactuals 13-19 still pending).
- If the LA E_3 misfit is unacceptable, possible remedies (in order
  of invasiveness):
  1. **Different starting point** for the wage stage that places LA
     wage_3 in the active gradient region. We checked (200, 0, 100, 0)
     and warm-start (8603, 234, 12784, 92) — both give ssq ≈ 0.05-0.06,
     no significant improvement available from start variation alone.
  2. **Add wage-direction regularization** to the wage stage objective
     to prefer parameter configurations of modest magnitude when the
     moment system is locally flat. Would change interpretation but
     improve numerical behavior.
  3. **Examine the LA worker-3 cluster** (in the upstream
     `cluster.R` clustering) to see if it pools dissimilar workers
     whose wage premium can't be summarized by a single parameter.
  4. **Revisit the entropy-regularised allocation** — a parametric
     family that admits more concentration on a single non-argmin
     worker (e.g., allowing salon-specific gamma in addition to the
     market-wide one) might be required.

## Cautionary tale: starting from the local optimum makes things worse

Seeding the wage stage with the grid-optim minimum (LA wages
(210, 10, 110, 10)) produced a *lower* per-county LA objective
(0.021 vs the cold-start's 0.484, vs the saturated-region floor of
~0.05) but it did so by escaping the saturated region in pathological
ways:

- LA worker 2 wage drove to **4.05 × 10⁶** ($4 million/hr offset).
- The downstream price-stage L-BFGS-B objective then exploded to
  **2.63 × 10⁷** (vs ~3 for the warm- and cold-start fits).

In other words, the wage-moment landscape has a flat valley with
non-zero floor, and any move that escapes the floor by changing
saturated-region wages produces extreme parameter values that wreck
the downstream price stage. The wage system is "identified" only in a
degenerate sense: many wage configurations sit on the same flat
valley with similar moment values, and the global GMM objective
prefers the one with the smallest combined wage-plus-price penalty,
which appears to be the cold-start fit.

**The cold-start v2 result (Cook + NY fitted, LA wages = 0) is the
recommended baseline going forward**, with the understanding that the
LA wage parameters are not pinned by the moment system and the LA E_3
moment cannot be zeroed.

## Diagnostic scripts

(under [`tests/`](../tests/))

- `la_wage_moment_landscape.R` — computes LA wage moments and a
  numerical Jacobian at LA = (50, 50, 50, 50). Initially looked like
  Jacobian rank-deficiency (E_3 row identically zero, wage_3 column
  identically zero) but that was the **saturated-region** signature,
  not true under-identification.
- `la_w3_sweep.R` — sweeps wage_3 across [-1e4, 1e6]; shows the active
  region (wage_3 ∈ [-1000, 0]) and the saturated region (wage_3 ≥ 50).
- `la_optim_grid.R` — grid + Nelder-Mead search for the joint LA wage
  ssq minimum. Best found: (210, 10, 110, 10) at ssq = 0.0530.

---

## Addendum: wage-stage solver should be a minimizer, not a root-finder

`tests/wage_stage_minimizer_check.R` runs three optimizers on the LA
wage block (4 params, all other wages and demand `beta` held fixed at
the cold-start v2 sequential solution; starting point = (0,0,0,0)):

| Optimizer                 | Type        | Final ssq | LA wages found              | Time |
|---------------------------|-------------|-----------|------------------------------|------|
| `nleqslv`                 | root-finder | 0.147     | (172k, -13.5M, -676k, -298k) | 9 s  |
| `BB::BBoptim`             | minimizer   | 0.074     | (74, -217, 400, -225)        | 13 m |
| `optim` Nelder-Mead       | minimizer   | **0.0021**| **(72, 8, 34, 163)**         | 12 m |

The minimizers find configurations with sensible wage magnitudes that
push ssq ~70x lower than the root-finder. nleqslv chases extreme
parameter values (millions in magnitude) trying to satisfy individual
moment equations, gets stuck at termcd=6 ("function values converged
but x has not"), and returns a poor fit. The minimizers see ssq
decreasing continuously even when no exact zero exists, and find clean
interior solutions.

Implication: the production `wage_optimizer_mode = nleqslv` (and the
joint/county BBsolve modes, which are also root-finders) should be
replaced by a minimizer over `||g||^2`. The targeted joint-GMM
prototype `06b_joint_estimation.R` confirms this independently --
within that prototype the demand-side betas never moved, all gain came
from the wage-side variables, but the gain only materialised because
optim was the outer solver.

The recommended fix is to add a new wage_optimizer_mode (e.g.
`"min_optim"`) that calls `optim(par, sum(g^2)^, method = "Nelder-Mead",
parscale = ...)` per county or jointly. The `parscale` argument is
essential: without it, default Nelder-Mead step sizes generate NaN
configurations. Reasonable defaults: 20 for wage parameters, 0.005 for
rho-style coefficients, 2 for skill coefficients.

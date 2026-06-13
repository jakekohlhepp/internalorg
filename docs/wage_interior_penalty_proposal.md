# Proposal: Interior-Share Penalty for the Wage Stage

Status: MOCKED UP, OFF BY DEFAULT (2026-06-10). Code is in place behind
`JMP_WAGE_INTERIOR_PENALTY` (config `wage_interior_penalty_*`); enabling it is
a re-estimation decision (full 06 re-run + downstream refresh). Companion to
the priced-out-coordinate findings in
[murphy_topel_proposal.md](murphy_topel_proposal.md) and the project memory
"priced-out wage coords".

## Why

At the 2026-06 estimates two worker types are *numerically extinct*: the model
assigns ~0 share to Cook type 3 (observed 8.2%) and LA type 5 (observed
3.4%). The share response to the type's wage coefficient is a cliff (LA:
county-mean share 19.4% at w5=200 vs ~1e-12 at w5=300; Cook: 9.1% at w3=50 vs
~1e-12 at w3=100). The demo scan below shows the two cases differ: for LA the
wage objective `||gbar||^2` genuinely prefers extinction (interiority costs
fit), while for Cook the extinct solution is a *wrong-basin artifact* — an
interior basin at w3 ≈ 60 fits 30× better. Consequences of the extinct
solutions:

1. **Local unidentification.** On the extinct plateau the objective is exactly
   flat in the coefficient; the Murphy-Topel wage Jacobian is singular there,
   and warm-started re-runs walk the coefficient outward arbitrarily
   (`seeit_bb` history: LA w5 0→163→544→591→754; Cook w3 351→500→526). No
   SE method gives these coordinates meaning.
2. **Saturated moments = the wage FOC floor.** The two pinned moments
   (−0.030, −0.072 county-mean scale) account for essentially all of
   `||mean g2|| ≈ 0.02`.
3. **Degenerate counterfactuals.** An extinct type can never re-enter in the
   counterfactual equilibria (its cost is arbitrarily far above competitive),
   which contaminates any experiment that shifts labor composition — notably
   16_counterfactual_immigration.

## The option

Add to the wage minimizers' objective a smooth one-sided penalty on the
county-mean model shares:

```
penalty = weight * sum_{c, k>=2} max(0, log(min_share) - log(model_ck))^2
```

- **Interiority only.** The floor `min_share` (default 1e-3) is an absolute
  share level, the same for every county and type, with no reference to the
  observed shares (those enter only in recovering model shares from the
  moment means the solver already computes). The penalty never pushes a
  share toward its observed level — it only forbids `model_ck -> 0`. Types
  2..n_w are covered; type 1 is the omitted reference type.
- **Exactly zero in the admissible region** — wherever every type's model
  share is ≥ min_share, the objective (and therefore the estimates) is
  unchanged. NYC and all currently-identified coordinates are unaffected by
  construction.
- **Diverges like log(share)² toward extinction** — at the numeric floor the
  penalty is ≈ log(1e-3/1e-16)² ≈ 900, which dwarfs the ~5e-4 fit advantage
  of the plateau, so the minimizer is forced to the edge of the cliff window
  where the type holds ≈ min_share. The previously-flat coordinate is
  thereby pinned at "the largest wage consistent with the type existing at
  min_share" — `min_share` is the disclosed tuning constant that locates it
  (robustness: 1e-4 / 1e-3 / 1e-2).
- C¹-smooth (squared hinge), county-separable (fits the per-county solver
  architecture), share-scale-free (log).

### Implementation map

| Piece | Where |
|---|---|
| Knobs | [config.R](../config.R): `wage_interior_penalty_enabled` / `_weight` / `_min_share` (env `JMP_WAGE_INTERIOR_PENALTY*`) |
| Penalty math | `wage_interior_penalty_county()`, `wage_interior_obs_means()`, `wage_interior_penalty_terms()` in [utils/structural_solver.R](../utils/structural_solver.R) |
| Minimizer objectives | wired into both `objective_county_ssq` closures (min_optim / min_optim_warm path and pso path) |
| Accept/revert gates | wired into `structural_wage_objective_score`, so gates 2/3 and the L1–L4 fallback ladder rank candidates by the same criterion |
| Not applicable | the `nleqslv` root-finding mode (solves `g = 0` directly; no objective to penalize) |
| Tests | `tests/testthat/test_wage_interior_penalty.R` (zero-in-admissible-region, extinction magnitude, county-dilution recovery, gate integration) |
| Demo scan | `diagnostics/wage_interior_penalty_scan.R` → `diagnostics/out/wage_interior_penalty_scan.rds` |

### Demo scan (1-D, all other coordinates at the 06 estimates)

`diagnostics/out/wage_interior_penalty_scan.rds`, weight = 1, min_share = 1e-3.
County-level raw moment ssq vs. penalized total, with county-mean model
shares (LA types ordered 2/3/4/5; observed LA = .31/.22/.21/.034, observed
Cook type 3 = .08):

**LA, w5 scan** (estimate 754.1):

| w5 | raw ssq | penalty | total | share_3 | share_5 |
|---|---|---|---|---|---|
| 754.1 | 0.00093 | 896 | 896 | 0.208 | 1e-16 |
| 300 | 0.00093 | 383 | 383 | 0.208 | 3e-12 |
| 275 | 0.0325 | **0** | **0.033** | 0.062 | 0.127 |
| 250 | 0.0688 | 0 | 0.069 | 0.0074 | 0.177 |
| 230 | 0.0767 | 0 | 0.077 | 0.0027 | 0.185 |
| 220 | 0.0803 | 0.19 | 0.274 | 6e-4 | 0.188 |
| 210 | 0.0830 | 358 | 358 | 6e-12 | 0.191 |

**Cook, w3 scan** (estimate 525.9):

| w3 | raw ssq | penalty | total | share_3 |
|---|---|---|---|---|
| 525.9 | 0.00517 | 436 | 436 | 8e-13 |
| 80 | 0.00517 | 332 | 332 | 1e-11 |
| 70 | 0.00186 | **0** | 0.00186 | 0.029 |
| 60 | **0.00017** | 0 | **0.00017** | 0.085 |
| 55 | 0.00022 | 0 | 0.00022 | 0.087 |
| 45 | 0.00101 | 0 | 0.00101 | 0.103 |

Two different stories:

1. **Cook's plateau is a wrong-basin artifact, not a fit-optimal corner.**
   At w3 ≈ 55–60 the county fit is **30× better** than the plateau
   (ssq 0.00017 vs 0.00517), type 3 sits at 8.5% ≈ its observed 8%, and the
   other types' shares are *unchanged* — no displacement. The current
   estimate (526) is a local-search artifact: the historical warm start
   (351) was already on the plateau, every warm-started re-run stayed there,
   and the flat surface let it drift outward. **Cook should be re-seeded
   near w3 ≈ 60 regardless of whether the penalty is adopted.**
2. **LA is a true trade-off: types 3 and 5 are substitutes.** Below
   w5 ≈ 275, type 5 enters at 13%+ but *displaces type 3* (0.208 → 0.062 →
   extinct by w5 = 210) — the all-types-interior window is a narrow band
   w5 ∈ ~(225, 290), and inside it the raw fit is 35–80× worse than the
   plateau (type 3's moment goes badly negative; consistent with the known
   near-collinearity of LA's type-1/2/3 skill rows). The penalty selects
   w5 ≈ 275 (both types interior: 6.2% and 12.7%) and makes the fit cost —
   county ssq 0.0009 → 0.033 — explicit. The model cannot hold both LA
   types at their observed shares simultaneously; the penalty buys
   coexistence, not fit.

## What enabling it would mean

1. **It is a different estimator.** The penalty acts as a soft constraint
   encoding "no worker type is driven to zero." Where it binds (the two flat
   coordinates — and through equilibrium spillovers, potentially the other
   coordinates of those two counties), point estimates change; the penalized
   optimum sits at the cliff edge where the model share ≈ min_share. Expect
   the *raw* moment fit to be marginally worse than the plateau fit (that is
   the explicit trade: fit vs. interiority). The honest reporting frame:
   estimates are conditional on the interiority restriction, with
   `min_share` a disclosed tuning constant (robustness: 1e-4 / 1e-3 / 1e-2).
2. **Identification is restored mechanically but locally fragile.** In the
   cliff window the objective is extremely steep (shares move ~10 orders of
   magnitude over Δw ≈ 100), so the penalized optimum is well-pinned in
   location but the Jacobian there is large and fast-varying — finite-
   difference steps for Murphy-Topel must shrink accordingly (`JMP_MT_STEP_SCALE`),
   and solver tolerances matter more. Expect tight SEs for the previously
   flat coordinates, reflecting the penalty's pinning, not rich data
   information. The honest SE framing under the penalty: Murphy-Topel on the
   penalized FOC — IMPLEMENTED in `07c_murphy_topel.R` (2026-06-10): the
   wage estimating equations become the penalized score, with the analytic
   penalty curvature in the scores/Jacobians and exact reduction to the raw
   system when the penalty is slack. Run 07c with the SAME
   `JMP_WAGE_INTERIOR_PENALTY` setting as the 06 run.
3. **Counterfactuals regain a margin.** With types interior at baseline, the
   immigration/diffusion experiments can move labor into or out of these
   types; today they cannot.
4. **Cost.** Full 06 re-estimation with the penalty on (hours), then 07
   (draws), 07c (Murphy-Topel with penalty-aware FOC), 09+, and the
   counterfactual chain. Warm-start hygiene: reset the two walked
   coordinates in `seeit_bb.rds` into the cliff window first (scan table
   gives the window), or the solver starts on the plateau where the penalty
   gradient is zero in the focal coordinate (the penalty is flat there too —
   the *hinge gap* is constant at its maximum, so NM/PSO still feels the
   pull only via the global search; a warm start inside the window avoids
   relying on PSO to find it).

## Alternatives considered

- **Log-share moments** (`log E_model − log E_obs`): more principled
  (extinction infinitely penalized automatically; small types get scale-
  appropriate weight) but a bigger change — it reweights ALL moments,
  shifting every wage estimate and the moment-fit displays; bigger lift to
  re-validate. The penalty is the surgical version: identical estimator
  wherever types are already interior.
- **Inverse-share moment weighting**: rebalances but does not bound shares
  away from zero — the optimizer can still choose extinction if the model
  prefers it; doesn't fix identification.
- **Hard box constraints on wage coefficients** (cap w below the cliff):
  simplest, but the cap value is arbitrary, binds exactly (boundary
  inference again), and the admissible window depends on beta — it would
  move with first-stage sampling variation.
- **Status quo + set-identification reporting** (current path): fix the two
  coordinates, NA SEs, footnote. Zero re-estimation cost; the model keeps
  its "cannot fit small interior shares" property and counterfactual
  degeneracy.

## Recommendation

Two separate decisions:

1. **Cook (do regardless of the penalty):** the current w3 = 526 estimate is
   in the wrong basin. Re-seed `seeit_bb` with w3 ≈ 60 and re-run 06; the
   county fit improves 30×, type 3 becomes interior at its observed share,
   and the coordinate becomes locally identified — no penalty needed to hold
   it there (the basin is a genuine interior minimum).
2. **LA (the actual penalty decision):** adopt the penalty if the
   counterfactual margin (type 5 able to respond) or interiority per se
   matters for the paper's claims, accepting the disclosed fit cost (county
   ssq 0.0009 → 0.033, driven by type 3's share dropping to 6.2% vs 21.6%
   observed — types 3 and 5 are substitutes in LA). Otherwise the status-quo
   set-identification treatment for w5 (fix it, NA SE, footnote) is cheaper
   and equally honest.

If adopted: enable with defaults (`weight = 1`, `min_share = 1e-3`), reseed
LA w5 ≈ 275 (and Cook w3 ≈ 60 per point 1), re-run 06, check which
coordinates moved materially, then re-run 07/07c with the penalty-aware FOC
and report a min_share robustness column.

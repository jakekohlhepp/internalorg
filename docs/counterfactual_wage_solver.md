# Counterfactual Wage Solver: Diagnosis and Recommended Replacement

This note documents the outer wage‑clearing solver in
[14_counterfactual_diffusion.R](../14_counterfactual_diffusion.R), explains
why its current `BBsolve` tolerance ladder + `broyden` polish is fragile, and
proposes a concrete replacement built on tools already present in the repo.

## Provenance and attribution

**User‑authored (Jake Kohlhepp, JMP work).**
- The counterfactual scripts and the existing solver scaffolding —
  `solve_wages`, `solve_reloc`, `get_everything`, the `BBsolve` ladder, the
  `pracma::broyden` polishing, and the inner organization / price contractions
  on which the outer wage problem depends.
- The decision about whether and when to adopt anything proposed here.

**Code‑review notes (not user‑authored).** Everything below: the diagnosis of
why the ladder exists, the recommended `nleqslv` replacement, the rescaling and
log‑wage suggestions, the homotopy fallback, and the multiple‑equilibria
caveat.

Motivating request: "14_*.R uses a certain combo of solvers
because it is often hard to find the solution. do you have suggestions for a
better way?", followed by "write as a .md in docs/".

## Where the wage solver lives

Each `(county, quarter_year)` market is solved independently inside two
loops. The structure is:

- `solve_wages(wage_guess)` (and its sibling `solve_reloc`) takes a
  candidate 5‑vector of wages, recomputes the cost matrix `tild_theta`,
  runs the entropy‑regularized organization fixed point row‑by‑row via
  SQUAREM, computes equilibrium prices via the Lambert‑W contraction (see
  [docs/price_solver.md](price_solver.md)), aggregates implied labor demand,
  and returns the demand‑minus‑supply residual per worker type.
- The outer driver searches over `wage_guess` for a zero of that residual.

For NYC and Chicago at quarter `2021.2` the driver looks like
[14_counterfactual_diffusion.R:379-386](../14_counterfactual_diffusion.R) —
two `BBsolve` calls bracketing one `pracma::broyden` polish. For LA the
driver expands to **eleven** stages
([14_counterfactual_diffusion.R:391-417](../14_counterfactual_diffusion.R)),
ratcheting `tol` from `500000` down to `1` and ending with another `broyden`
polish followed by a final `BBsolve`.

## Diagnosis

The eleven‑stage ladder is symptomatic of three fixable issues, not of an
intrinsically hard problem.

### 1. The residual is not dimensionless

`solve_wages` returns `new_total_labor - target_total_labor` in raw worker
counts. LA's labor totals dominate the smaller counties by orders of
magnitude, so a residual of `500000` is a "loose" tolerance there but
absurd elsewhere. The ladder is chasing the same *relative* residual at
wildly different absolute scales — that is the entire reason it has eleven
rungs for LA and three for NYC.

### 2. `BBsolve` is dfsane, the wrong tool for a 5×5 smooth system

`BB::BBsolve` is a derivative‑free spectral algorithm
(Barzilai‑Borwein step). It earns its keep on very high‑dimensional or
non‑smooth systems where assembling a Jacobian is infeasible. The
counterfactual problem is 5‑dimensional, smooth in wages, and inexpensive
to differentiate by finite differences. Quasi‑Newton (Broyden secant
updates with trust‑region globalization) converges in a handful of
iterations on this kind of problem; dfsane spends thousands. The fact that
the script chases each `BBsolve` with `pracma::broyden` is the algorithm
telling you it should have been Broyden from the start.

### 3. There is no globalization

`dfsane`'s `noimp=200` parameter accepts a stalled residual if it cannot
improve for 200 steps. That is not convergence — that is surrender. A
trust‑region or line‑search globalization is required for any serious
nonlinear root‑finding on a problem with non‑monotone residual norm, which
is what wages produce here (raising $w_k$ raises costs and prices, which
lowers labor demand directly, but also reallocates worker shares away from
type $k$, which lowers labor demand of type $k$ specifically — the two
effects can have opposite signs).

## Recommended replacement

The structural estimator already imported the right tool — see
[utils/structural_solver.R:888-958](../utils/structural_solver.R) where
`estimate_wage_parameters_nleqslv` uses `nleqslv` with `Broyden + dbldog`
and the docstring at line 960 reports a wall‑time drop from "hours" to
"minutes" relative to the BBsolve baseline at the GMM step. The same
prescription applies one level out.

The proposed change is three local edits, ordered by leverage.

### Edit 1 — Make the residual dimensionless

Replace the raw labor difference with a log relative gap, anchored at the
strictly positive supply target:

```r
labor_clearing <- log(pmax(new_total_labor, 1e-12)) -
                  log(pmax(target_total_labor, 1e-12))
```

A tolerance of `1e-6` then means "wages within $10^{-4}\%$ of clearing"
everywhere, with no county‑specific tuning. This single edit eliminates the
reason the ladder exists.

### Edit 2 — Solve in log wages

```r
solve_wages_log <- function(log_w) solve_wages(exp(log_w))
```

Two benefits: the solver cannot wander into negative wages (which break the
inner cost matrix), and shares respond multiplicatively to wage changes, so
the residual is closer to linear in $\log w$ than in $w$. Iteration counts
typically drop by half.

### Edit 3 — Replace the BBsolve ladder with `nleqslv`

```r
nleqslv_res <- nleqslv::nleqslv(
  x       = log(as.numeric(initial_wages[county==cnty & quarter_year==qy,
                                          c("w1","w2","w3","w4","w5")])),
  fn      = solve_wages_log,
  jac     = NULL,                       # finite-difference Jacobian
  method  = "Broyden",                  # secant updates after initial FD
  global  = "dbldog",                   # double-dogleg trust region
  control = list(xtol = 1e-10, ftol = 1e-8, maxit = 200,
                 cndtol = 1e-12, allowSingular = TRUE,
                 trace = 1)
)
quad_wages <- list(par      = exp(nleqslv_res$x),
                   residual = max(abs(nleqslv_res$fvec)),
                   message  = nleqslv_res$message,
                   termcd   = nleqslv_res$termcd)
```

`Broyden + dbldog` is robust precisely where dfsane stalls: a trust region
shrinks the step on bad iterations and the Jacobian is rebuilt via finite
differences when the secant approximation goes stale. With $n = 5$ a
Jacobian rebuild costs five inner `solve_wages` evaluations — cheap
insurance.

### Edit 4 (optional) — Continuation in `gamma_invert`

Worth adding only if Edits 1–3 still fail in some corner. The baseline at
`gamma_baseline` is already a fixed point at `initial_wages`, so a homotopy
in the counterfactual perturbation gives a sequence of small, well‑warmed
problems:

```r
lambdas <- c(0, 0.25, 0.5, 0.75, 1)
warm <- log(initial_wages_vec)
for (lam in lambdas) {
  working_data[, gamma_active :=
                 (1 - lam) * gamma_baseline + lam * gamma_invert]
  res <- nleqslv::nleqslv(warm, solve_wages_log,
                          method = "Broyden", global = "dbldog",
                          control = list(xtol = 1e-10, ftol = 1e-8,
                                         maxit = 200))
  warm <- res$x
}
```

This is the principled version of what the eleven‑stage `BBsolve` ladder is
doing heuristically — moving along a path of nearby problems with each
solution warm‑starting the next.

## Why Edits 1–3 first

Edits 1 and 2 are local rewrites with no algorithmic risk and remove the
scale dependence that makes the ladder necessary. Edit 3 is then a one‑shot
drop‑in. The existing terminal `pracma::broyden` polish at
[14_counterfactual_diffusion.R:413](../14_counterfactual_diffusion.R)
already shows that Broyden converges quickly once the iterate is in the
right basin — the ladder's job is to reach that basin, and Edit 1's
rescaling plus Edit 3's globalization put us there directly.

Edit 4 is engineering effort that should only be spent if a concrete LA‑style
counterfactual still fails after the first three edits.

## What not to change

- **The inner Lambert‑W price step.** Verified to machine precision in
  [docs/price_solver.md](price_solver.md); it is already a contraction in
  the relevant regimes and is faster than any Newton‑class alternative
  would be at the inner level.
- **The `SQUAREM` entropy organization fixed point.** Same reasoning —
  contraction with quadratic acceleration on a problem with a guaranteed
  unique fixed point. Replacing it would buy nothing.

## Caveat: multiple equilibria

The wage problem can in principle have multiple equilibria — high‑wage and
low‑wage solutions with self‑consistent labor demand. None of the above
guarantees finding the "right" one. `nleqslv` converges to the basin of its
starting point, exactly as `BBsolve` does today; the difference is that
`nleqslv` gets there in seconds rather than hours, which leaves budget to
**start from several initial points** and check that they coincide.

If they don't coincide, that is an economic finding (and an interesting one
for the JMP), not a solver bug. The current eleven‑stage ladder hides this
possibility, because every restart begins from the previous stage's output
and so cannot escape its initial basin.

A practical pattern would be to solve from at least two starts per
`(county, quarter)`:

1. `initial_wages` (the baseline equilibrium — the natural warm start),
2. a perturbed start, e.g. `initial_wages * runif(5, 0.7, 1.3)`,

and assert that the two converge to the same wage vector to within
`xtol`. The assertion either passes (in which case the answer is
robust within that ball) or fails (in which case the multiplicity is
real and worth reporting).

## Migration footprint

The same `BBsolve` ladder pattern is repeated in:

- [14_counterfactual_diffusion.R:240-247](../14_counterfactual_diffusion.R)
  (`solve_reloc`, four stages)
- [14_counterfactual_diffusion.R:379-386](../14_counterfactual_diffusion.R)
  (`solve_wages`, NYC + Chicago, three stages)
- [14_counterfactual_diffusion.R:391-417](../14_counterfactual_diffusion.R)
  (`solve_wages`, LA, eleven stages)

If only one stage migrates first, do the LA `solve_wages` block — it is the
worst offender and the clearest test of whether `nleqslv` actually wins on
the hardest case in the pipeline. If it does, the other blocks follow with
the same recipe. The same pattern recurs in
[15_counterfactual_sales_tax.R](../15_counterfactual_sales_tax.R),
[16_counterfactual_immigration.R](../16_counterfactual_immigration.R), and
[17_counterfactual_merger.R](../17_counterfactual_merger.R), so a clean
helper in [utils/counterfactuals_core.R](../utils/counterfactuals_core.R)
that wraps Edits 1–3 would amortize the change across all four scripts and
match the way `estimate_wage_parameters` already centralizes the same
choice for the GMM stage.

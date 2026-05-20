# Why we couldn't find the moment-zeroing NYC wages earlier

A retrospective on the wage-stage debugging campaign. Companion to
[wage_solver_stability.md](wage_solver_stability.md) (multi-seed PSO and
multi-method polish stability) and [wage_identification.md](wage_identification.md)
(Hessian + perturbation diagnostic). Records the four NYC wage basins we
discovered and explains why each method we tried saw only some of them.

## The four NYC basins

| label | NYC `(E_2, E_3, E_4, E_5)` | ssq | first discovered by |
|---|---|---|---|
| all-large-negative plateau | (−1059, −948, −869, −675) | 0.0248 | nleqslv termcd=5 (initial production); reproduced as the original min_optim seed |
| PSO v1 ("ridge") | (−22, +498, +151, **+1826**) | 0.00128 | cold PSO with halfwidth=2000 in `smoke_pso_nyc` (job 48962785), reproducibly hit by 4 different RNG seeds |
| PSO v2 (perturbation polish) | (−24, +496, +152, **+343**) | 0.00119 | `06c_wage_identification` perturbation step `+10` along the smallest-eigenvalue direction |
| **moment-zeroing** | **(−21, +494, +152, +122)** | **1.36e−12** | `polish_seed` in 06_estimation 49633616 after patching `seeit_bb.rds` to the v2 point |

Three of these four are local minima of the per-county wage moment ssq
`‖g_wage‖²`. Only the last one zeros the moment system to numerical
precision.

## Why each method we tried missed the moment-zeroing point

### 1. Local solvers are local

`nleqslv`, `optim` (NM, BFGS, L-BFGS-B, CG), `nlminb`, `SANN` and
`BB::BBsolve` all converge to the basin nearest the starting point.
- From the original seed inside the all-negative plateau, every local
  solver stopped there or in an adjacent shallow minimum.
- From PSO v1's `(-22, +498, +151, +1826)` start, every polisher in the
  `polish_all` smoke (job 48993924, seed 2718281) stopped at PSO v1 to
  6+ significant figures.

This was not surprising — local solvers cannot hop between basins by
design — but it framed the problem incorrectly during the early
debugging.

### 2. The +1826 basin has a wide basin of attraction; the +122 basin is much smaller

PSO is global but coarse. 40 particles in a 4D `[-2000, +2000]^4` box
gives a particle density of ~40 / (4000⁴) ≈ 6×10⁻¹³ per unit volume.
Velocity updates pull all particles toward the running global best, so
PSO ends up sampling whichever basin first dominates.

The +1826 basin captures most of the action. We saw this empirically:
- 4 PSO seeds (smoke jobs 48993921–24) all converged to +1826 to 6 sig figs.
- Halfwidth 2000, 5000, 10000 (job 49008699) all converged to +1826.

The +122 basin is narrow enough that 40 particles in this box almost
never land near it, and once gbest is at +1826, no particle is pulled
toward +122 by the velocity update.

### 3. The "flat ridge" intuition was incomplete

The cross-seed E_5 spread (1825–1826) and the post-polish E_5 drift
(1826 → 2009 → 2234) led us to label this region a "flat ridge". With
the Hessian diagnostic we now see it is a chain of shallow local minima
at similar ssq, separated by very shallow saddles:

```
... +122 [min] ... saddle ... +343 [min] ... saddle ... +1826 [min] ...
```

Within a single local-solver call the surface looks flat-ridge-like
because the local gradient is essentially zero. Across solver calls
from different starts, the same RNG seed reliably picks the same
basin because the velocity dynamics are deterministic.

### 4. The jitter in our restart heuristics was too small

The minima along the chain are spaced by O(100–1000) units in E_5.
The restart-on-code-10 jitter we used was 5% of `max(|par|, parscale)` —
which translates to a displacement of ~90 units at par = +1826. That
keeps the perturbed particle within the same basin's width, just shifted
slightly.

To cross from +1826 to +343 requires a perturbation of magnitude ~1500.
The pre-Hessian heuristics (cold-PSO, multistart NM, dual-polish) never
tried displacements that big from the global best.

### 5. The Hessian's smallest-eigenvalue eigenvector turned out to point across basins

At the +1826 basin, the smallest-eigenvalue eigenvector decomposed as
roughly `0.033 · e_2 + 0.575 · e_3 + 0.184 · e_4 + 0.796 · e_5`. The
06b perturbation tested `x* + s · v` for `s ∈ {±1, ±10, ±50, ±100}`.

At step `+10`, the displacement in E_5 alone is `10 × 0.796 ≈ +8`, which
is small. But the *new* point's E_5 = 1826 + 8 = 1834, and the polish
that follows that perturbation — running NM from a point already
slightly off the +1826 basin's bottom — descends *along the chain* into
the +343 basin, not along the 1834 → 1826 axis. (This is essentially the
"slow downhill direction was missed because gradient was too flat to
detect" mechanism.) Step +50 jumps even farther, into a different basin
above 0.02. Step +10 happened to land exactly in the +343 basin's
attraction region.

So the Hessian diagnostic incidentally acted as a **direction-finder
across basins**, not just a local curvature probe. That's the
constructive path that no method above could provide on its own.

### 6. Patching `seeit_bb.rds` to the +343 basin let `polish_seed` finish the job

Once the seed was inside the +343 basin's region of attraction, NM polish
from there descended cleanly to +122 at ssq=1.36e−12. This is just
standard local search succeeding when the start is right. The dual-polish
PSO design (cold PSO + polish_pso + polish_seed, take min) made this
"just work" without a code change beyond the seed patch.

## Could we have found it earlier?

Three retrospective options, each with trade-offs:

| approach | what it would have done | trade-off |
|---|---|---|
| **(a) Larger jitter from the start** | 100% jitter on restart instead of 5% | NYC would have crossed basins; Cook and LA, already at the global min, would have been knocked out of optimum before re-converging. A single jitter scale doesn't work for all counties. |
| **(b) Hessian-guided multistart inside the wage solver** | At every NM stopping point, compute Hessian; if any eigenvalue is small/negative, add `x* + s · v_min` as additional starts; recurse | Right design. Currently sits *between* 06 and 06b in the pipeline rather than *inside* `min_optim`/`pso`. Folding it inside the solver would automate this loop. |
| **(c) PSO with ~10× more particles** | 400 particles × 100 iterations | The +122 basin would more likely have captured some particles and become gbest. Cost: 10× PSO wall time per county. |

The path we actually took — PSO finds *a* good basin, Hessian flags a
weak direction, perturbation step finds *the* good basin — turned out to
be the cheapest of the three. Each step was inexpensive on its own, and
the diagnostic produced both the verdict on the current solution and the
direction needed to find the next one.

## Practical recommendation for future estimations

The strongest version of this design is **(b)**: fold the
Hessian-and-perturb step into the inner wage solver loop. After PSO +
dual polish completes for a county, run a small Hessian check; if any
eigenvalue is below a threshold, perturb along that eigenvector and
re-polish; keep iterating until either the Hessian is positive definite
or the perturbation no longer improves the ssq. This automates the
constructive path we took by hand and would catch this kind of multi-basin
landscape on first pass.

Implementation sketch:

1. Add a config flag `pso_iterate_on_hessian = TRUE` (default off until
   tested).
2. After the existing dual-polish step returns the best per-county
   solution, compute the 4×4 Hessian numerically (one extra ~30s per
   county at NYC scale).
3. If the smallest eigenvalue is below a configurable threshold (e.g.
   `pso_hessian_min_lambda = 1e-6 * |max_lambda|`), perturb along that
   eigenvector at a small set of step sizes, polish each, take the
   minimum.
4. Loop up to `pso_max_hessian_iterations` (default 3–5).
5. Expose telemetry (number of Hessian iterations, final eigenvalue
   spectrum) in the wage-solver result.

Most counties (Cook, LA in our data) would never enter the loop because
their Hessians are positive definite from the start. NYC-like counties
would converge in 2–3 iterations to the deepest basin without manual
intervention.

## Cross-references

- [wage_solver_stability.md](wage_solver_stability.md) — multi-seed PSO
  and multi-polisher convergence at PSO v1.
- [wage_identification.md](wage_identification.md) — the Hessian +
  perturbation diagnostic that exposed PSO v1 as a saddle.
- [parameter_comparison.md](parameter_comparison.md) — comparison of
  current point estimates with the manuscript draft.
- [counterfactual_solver_assessment.md](counterfactual_solver_assessment.md) —
  whether these lessons apply to the counterfactual wage solver.

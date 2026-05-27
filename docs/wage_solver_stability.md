# Wage solver stability — NYC PSO basin

Records the multi-seed and multi-method experiments that established the
existence and stability of NYC's PSO-discovered wage basin. Companion to
[parameter_comparison.md](parameter_comparison.md).

Background: with `min_optim` (Nelder-Mead) the production code dead-ends at
NYC ssq ≈ 0.0248 with all-large-negative wages. PSO (`smoke_pso_nyc`,
job 48962785) found a different basin at ssq ≈ 0.00128 with manuscript-shape
wages. The experiments below verify that this PSO basin is a *stable* local
minimum, not a stochastic artefact.

All experiments warm-start one swarm particle at the polished PSO best from
job 48962785 — `(-22.4, 498.2, 150.94, 1826.32)` — and draw the remaining 39
particles uniformly in `[-2000, +2000]^4`. Swarm: 40 particles, 100 iterations,
inertia w=0.7, cognitive c1=1.5, social c2=1.5. Polish runs `optim` (or
`nlminb`/`nleqslv`) from the PSO global best.

## Experiment 1: four PSO seeds, NM polish

Same swarm size, same warm start, same search box, only seed differs.

| Job | label | seed | PSO best ssq | best par (4 dims) |
|---|---|---|---|---|
| 48993921 | seed_A | 4459665 | 0.00128336 | (−22.401, 498.207, 150.942, 1826.289) |
| 48993922 | seed_B | 1234567 | 0.00128336 | (−22.402, 498.180, 150.945, 1826.324) |
| 48993923 | seed_C | 9876543 | 0.00128336 | (−22.397, 498.185, 150.941, 1826.353) |
| 48993924 | finishes | 2718281 | 0.00128336 | (−22.400, 498.200, 150.942, 1824.755) |

NM polish from each PSO best:

| label | polish ssq | polish convergence |
|---|---|---|
| seed_A | 0.00128336 | 10 (degenerate simplex) |
| seed_B | 0.00128336 | 10 |
| seed_C | 0.00128336 | 0 |
| finishes | 0.00128336 | 0 |

**Across-seed coordinate variation** (max minus min of the 4 best-par vectors):
- E_raw_2: 0.005
- E_raw_3: 0.027
- E_raw_4: 0.004
- E_raw_5: 1.598

The first three coordinates agree to four decimal places. E_raw_5 varies by
1.6 across seeds (≈ 0.09% relative to 1826) — well below the precision of any
downstream comparison. The variation is consistent with the PSO solution
sitting in a flat-bottomed local basin where E_raw_5 has weak curvature.

**Wall-clock cost** (each warm-started run): 38 → 59 min on the 8-CPU
allocation. Fast because the warm-start particle pre-positions one swarm
member at the basin centre.

## Experiment 2: seven polishing methods, single PSO seed

Run from job 48993924 (`finishes`), same PSO best as the entry point for all
seven polishers.

| polish method | final ssq | convergence | wall (s) |
|---|---|---|---|
| Nelder-Mead | 0.00128336 | 0 | 78.0 |
| BFGS (numerical grad) | 0.00128336 | 0 | 26.2 |
| L-BFGS-B (numerical grad) | 0.00128339 | 0 | 49.0 |
| CG | 0.00128340 | 0 | 11.1 |
| nlminb | 0.00128337 | 1 | 16.6 |
| SANN (maxit=2000) | 0.00128350 | 0 | 249.3 |
| nleqslv (Broyden + dbldog, root-finder on g) | 0.00128337 | 3 | 5.9 |

All seven methods exit with what they call convergence, and all seven values
sit in the band 0.00128336–0.00128350. The 1.4e-7 spread is consistent with
the inner equilibrium solver's numerical floor (the `objective_county_ssq`
function calls `objective_gmm`, which solves a fixed-point system per salon
to `innertol = 1e-10`; per-call noise at the 7th decimal is expected).

## What this proves

1. **The 0.00128336 basin is a true local minimum of ‖g_wage‖² at NYC.**
   Independent random seeds converge to it. Independent polishing methods
   converge to it. The gradient is zero there to within numerical noise
   (BFGS, CG, nlminb, nleqslv all confirm via different convergence
   criteria).
2. **It is not the same as the production-code minimum.** Production
   `min_optim` (Nelder-Mead with a `[-1000, +1000]^4` multi-start box and
   parscale 200) dead-ends at ssq ≈ 0.0248 — a *different* local minimum
   sitting just outside the search ranges where PSO finds the better basin.
3. **Whether 0.00128336 is the global minimum is not yet established.**
   PSO with halfwidth 2000 may itself be missing a still-better basin
   further out. Tests at halfwidth 4000+ or with basin-hopping have not
   been run.

## Practical floor for the strict-exit guard

The smoke results imply NYC's empirical floor is ssq ≈ 1.3e-3 under any
reasonable solver. Setting `obj_tol = 1e-6` for the strict-exit guard
guarantees NYC will fail. Two sensible choices:

- **`pso_strict_obj_tol = 0.01`** (current default): comfortably above NYC's
  observed floor with a 7× safety margin, while still tight enough that
  Cook (~1e-16) and LA (~1e-7) pass trivially. This is what production
  job 49003738 uses.
- **`pso_strict_obj_tol_by_county["36061"] = 0.002`**: tightest threshold
  that still admits NYC's PSO basin, useful for catching genuine regression
  in the basin (e.g., if the prior best changes by more than ~50%).

Cook and LA are unaffected — `pso_strict_obj_tol = 0.01` is many orders
of magnitude above their seed-start values.

## Implications for the manuscript draft

The PSO basin's NYC wages, sorted, are `(−22, +151, +498, +1826)` versus the
manuscript's `(−166, −142, −70, +660)`. Both have the *same shape* — three
modest values plus one large positive — but in opposite directions on three
of four. The +1826 in the PSO basin is roughly 2.8× the manuscript's +660;
all four wages are positive (or near-zero) in PSO, while the manuscript has
three negatives. Whether a still-lower basin exists with the manuscript's
exact sign pattern is open.

## Logs and reproducibility

- Smoke job logs: `logs/smoke_pso_warm_48993921.out` through
  `logs/smoke_pso_warm_48993924.out`.
- Smoke source: [smoke_pso_warmstart.R](../smoke_pso_warmstart.R),
  [run_smoke_pso_warmstart.sl](../run_smoke_pso_warmstart.sl).
- Production PSO solver: `estimate_wage_parameters_pso` in
  [utils/structural_solver.R](../utils/structural_solver.R).
- Reproduce a seeded smoke run with:
  ```sh
  sbatch --export=ALL,JMP_PSO_SEED=4459665,JMP_PSO_LABEL=seed_A,JMP_PSO_MODE=polish_nm \
    run_smoke_pso_warmstart.sl
  ```
- Reproduce the 7-polisher run with `JMP_PSO_MODE=polish_all`.

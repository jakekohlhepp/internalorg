# Structural Solver Speedups

This note documents the reusable numerical solver layer in
`utils/structural_solver.R`. The wage estimator and counterfactual solvers both
need the same inner object: for a county cost matrix, task mix, and target
specialization index, solve for `gamma`, worker shares `E`, and assignment
matrix `B`.

## Why the Original Step Is Expensive

One outer wage objective evaluation does the following:

1. Build county cost matrices from candidate wage parameters.
2. For each estimation row, find `gamma` by bisection.
3. For each bisection gamma, solve the entropy-regularized assignment fixed
   point.
4. Compare predicted worker shares to observed `E_raw_*` shares.

This nests many fixed-point solves inside every outer optimizer call.

## Implemented Speedups

In concise form, the reusable layer now:

- Precomputes row-level task shares, counties, `s_index`, and observed worker
  shares before the hot loop.
- Memoizes bisection function values so each `gamma` is evaluated once per root
  search.
- Stores row-level `gamma` warm starts across objective evaluations.
- Keeps optional fixed-point `E` warm starts behind
  `JMP_USE_FIXEDPOINT_WARM_STARTS`; this defaults to `false` because it can
  change SQUAREM/fallback numerical paths.
- Exposes row-level caching for repeated `(county, task mix, s_index)` states.
- Exposes an optional Rcpp plain fixed-point path; this is not default because
  it benchmarks slower than SQUAREM for the current estimation dimensions.
- Exposes a county-separable wage-solver mode for future tuning. The default
  remains `joint` until that mode is made reliably faster.

### Precomputed row inputs

`prepare_equilibrium_rows()` converts the estimation data into matrices and
vectors once per objective call:

- `alpha`: task-mix matrix
- `county`: character vector for cost-matrix lookup
- `s_index`: specialization target
- `observed_E`: observed worker shares excluding type 1

This removes repeated data-frame/data-table extraction from the hot row loop.

### Bisection value memoization

`bisection()` now stores `f(gamma)` values inside each root search. The old
routine called the same expensive equilibrium solve multiple times at the same
gamma, especially for midpoint and lower-bound sign checks.

### Warm gamma starts

`solve_worker_rows()` stores the previous `gamma` for each row key in a solver
state environment. On the next objective evaluation, `find_gamma_for_sindex()`
uses that value to build a tighter initial bracket.

This is the biggest measured win for wage optimization because `BBsolve`
evaluates many nearby candidate wage vectors.

### Optional fixed-point warm starts

`use_fixedpoint_warm_starts` is available but defaults to `FALSE`.

Warm-starting worker shares `E` can reduce fixed-point iterations, but it can
also change numerical paths when SQUAREM stops early or falls back. Keep this
off for baseline-compatible runs; enable it only after validating tolerances for
a specific counterfactual workflow.

### Row-level cache

`use_solver_cache` collapses repeated `(county, task mix, s_index)` rows within
an objective call. It is useful when rows repeat or when counterfactual grids
reuse states. In the current 436-row estimation sample, rows are mostly unique,
so this does not improve the single-call benchmark.

### Staged tolerances

`use_staged_solver_tolerances` allows looser inner/outer tolerances while the
previous objective norm is far from the target. This is intended for exploratory
or early-stage optimizer passes. For final estimates, validate with tight
tolerances.

### County-separable wage solves

`estimate_wage_parameters()` exposes `wage_optimizer_mode = "county"`. Because
a county's worker-share moments depend only on that county's four wage
parameters, the 12-parameter wage system should be decomposable into three
4-parameter systems.

The default remains `joint` because the first smoke test of the county path was
not yet faster. Treat county mode as implemented but experimental until we tune
its outer-solver controls.

### Optional Rcpp path

`use_rcpp_equilibrium` compiles a plain fixed-point solver via Rcpp. In the
current estimation benchmark it is slower than SQUAREM, so it defaults to
`FALSE`. It remains useful as a documented fallback and as a starting point for
a future compiled SQUAREM-like implementation.

## Current Benchmark

Serial objective evaluation on `mkdata/data/04_estimation_sample.rds`, using
the saved `seeit_bb.rds` wage vector:

| Mode | Elapsed seconds | Moment squared norm |
| --- | ---: | ---: |
| Exact, no cache/warm/staged | 12.03 | 0.06227157 |
| Warm gamma, first call | 11.93 | 0.06227157 |
| Warm gamma, second call | 2.41 | 0.06227190 |
| Rcpp plain fixed point | 22.32 | 0.06227157 |

The main speed gain comes after the first objective call, which is the relevant
case inside an optimizer, and specifically inside `BBsolve`.

## Wage-Stage Scheduling Parallelism (2026-07)

The PSO wage stage's wall-clock cost is dominated by sequential scheduling of
work that is provably independent. Three changes address this; the first two
are bit-identical to the sequential code and default ON, the third changes
low-order bits and defaults OFF.

### County-parallel PSO (`JMP_WAGE_PSO_COUNTY_PARALLEL`, default true)

`estimate_wage_parameters_pso()` forks one child per county (Linux only,
needs >1 core). Bit-identity with the sequential loop rests on four facts:

1. County objectives are exactly separable: `build_cost_matrix()` selects
   wage parameters by county pattern, and each county's moments are computed
   on its own rows (other counties' moment columns are exact zeros).
2. Each county reseeds its own RNG (`set.seed(pso_seed)`) before consuming
   any randomness.
3. Warm-start cache keys are county-prefixed, so the children's cache writes
   are disjoint; the parent merges them per county after the forks return.
4. The single genuine sequential coupling — `get_solver_tolerances()`'
   coarse/fine branch at a county's *first* objective call reads the previous
   county's final moment norm — is checked after the forks return, and any
   county whose branch would have differed is transparently re-run in-process
   with the exact serial incoming state (worst case: sequential speed).

The parent also restores the post-loop `.Random.seed` and the global cache
norm exactly as the sequential loop would have left them (the price stage
consumes the cache via `get_gammas()`).

### Parallel L5 joint multistart (`JMP_WAGE_FB_L5_PARALLEL`, default true)

Each L5 start is a pure function of its start vector: the dispatcher resets
the global solver cache on entry and every RNG consumer reseeds
deterministically. The K starts therefore fork cleanly; the parent re-emits
each start's warnings in order, applies the same best-selection rule, and
installs start K's final cache and RNG state (what a serial loop leaves
behind). Only active when `solver_state` is NULL — a caller-supplied live
state is not reset by the dispatcher and couples the starts.

### L4 polish-only (`JMP_WAGE_FB_L4_POLISH_ONLY`, default false)

A full L4 re-dispatch re-runs the cold-start swarm with the same per-county
seed, so the swarm and its polish reproduce round 1 (verified in
`p06_58869070.out`); only the polish from the L1/L3-improved seed adds
information. This flag makes L4 rounds skip the swarm and polish from the
improved seed only. NOT bit-identical (the polish starts from a different
warm-gamma cache history), so it stays opt-in; enable it to roughly halve
L4 cost when exact replication is not required.

### Verification

`diagnostics/verify_wage_stage_bitident.R` runs the full wage stage
(dispatcher + L1–L5 ladder) on a row-subset of the real estimation sample
and snapshots the result object, the global solver cache, and the final
`.Random.seed`. Old-code (HEAD overlay in `diagnostics/baseline_src_head/`)
vs new-code and serial vs parallel payloads are compared byte-for-byte via
`serialize()`.

### Sizing note

With county-parallel on, 06's PSO phase uses ~3 concurrent county solves,
each of which row-parallelizes across `SLURM_CPUS_PER_TASK / 3` cores; with
L5 K>1 the multiplier grows to `K x 3`. Request 24 cores for a K=2 run
instead of the historical 8 to realize the full speedup (cores are split
automatically via `config$core_count`).

## Main Configuration Switches

- `JMP_WAGE_OPTIMIZER_MODE=joint|county`
- `JMP_USE_SOLVER_WARM_STARTS=true|false`
- `JMP_USE_FIXEDPOINT_WARM_STARTS=true|false`
- `JMP_USE_SOLVER_CACHE=true|false`
- `JMP_USE_STAGED_SOLVER_TOLERANCES=true|false`
- `JMP_USE_RCPP_EQUILIBRIUM=true|false`
- `JMP_SOLVER_CACHE_ROUND_DIGITS=12`
- `JMP_WAGE_PSO_COUNTY_PARALLEL=true|false`
- `JMP_WAGE_FB_L5_PARALLEL=true|false`
- `JMP_WAGE_FB_L4_POLISH_ONLY=true|false`

## Counterfactual Reuse

Counterfactual solvers should call the reusable layer directly:

1. Build or update county cost matrices.
2. Prepare state rows with `prepare_equilibrium_rows()` or an analogous
   counterfactual row object.
3. Reuse a `make_solver_state()` environment across nearby counterfactual
   states so gamma warm starts carry over.
4. Keep `use_fixedpoint_warm_starts = FALSE` until a workflow-specific
   validation confirms it does not perturb results beyond tolerance.

In short, counterfactual solvers should reuse this layer by keeping a
`make_solver_state()` environment alive across nearby counterfactual states.
Gamma warm starts are the safe default. Fixed-point `E` warm starts should stay
disabled until a workflow-specific validation confirms they do not perturb
results beyond the chosen tolerance.

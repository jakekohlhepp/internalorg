# Bootstrap SLURM Deployment

How `run_bootstrap_array.sl` is sized, and why. Update this file when the
specs in the script change.

## Submission

```bash
# Pre-flight (once, on a login node):
module load r/4.4.0
Rscript -e 'source("renv/activate.R"); renv::restore(prompt = FALSE)'

# Submit the array (1100 reps, 100 concurrent):
sbatch run_bootstrap_array.sl

# After the array finishes, combine per-rep RDS files into 07_bootstrap.rds:
JMP_BOOTSTRAP_COMBINE_ONLY=true Rscript -e \
  'source("renv/activate.R"); source("07_bootstrap.R")'
```

The bootstrap reads `mkdata/data/04_estimation_sample.rds` and
`results/data/06_parameters.rds` (wage warm start is pulled from
`parameter_table` rows whose `parm_name` matches `:avg_labor:E_raw_*`,
reordered to match `rownames(beta_2)[wage_idx]`). It writes one
`results/data/bootstrap_reps/boot_res_<i>.rds` per array task, and combines
them into `results/data/07_bootstrap.rds` on the combine-only pass.

## Spec table

| Directive | Value | Rationale |
|---|---|---|
| `-p general` | general partition | Memory fits well under the 232 GB/node cap; no need for `general_big` or `bigmem`. |
| `-n 1` | one task per array element | Each rep is one process. |
| `--cpus-per-task=2` | 2 CPUs | Each rep is single-threaded at the R level (see "Why serial" below). One core would suffice; the second is headroom for `data.table` internal threads and any OpenMP region not covered by the env-var pinning. |
| `--mem=4g` | 4 GB | Tuned 2026-05-09 from `sacct MaxRSS`. Bench job 49320283 (full PSO+warm-start, single rep) peaked at 698 MB. 4 GB is ~5.7x headroom. |
| `-t 10:00:00` | 10 h | Tuned 2026-05-09 from bench job 49320283 wall = 5:22:16 (full PSO wage solve across Cook/NYC/LA + price step). 10 h is ~1.9x headroom for tail reps where NYC PSO crawls. The previous 1:30:00 budget (extrapolated from a smoke run with `JMP_SKIP_STRUCTURAL_OPTIMIZER=true`) caused job 49194150 to TIMEOUT 476/600 elapsed tasks. |
| `--array=1-1100%100` | 1100 reps, 100 concurrent | `CONFIG$bootstrap_reps` is 1100 in [config.R](../config.R). At ~5h per rep (bench job 49320283 wall = 5:22:16) with %100 concurrency, the array finishes in ~11 waves ≈ ~2.5 days wall. |
| OMP/OpenBLAS/MKL/vecLib `_NUM_THREADS=1` | env vars | Force BLAS/OpenMP single-threaded so we don't silently oversubscribe the 2 allocated cores. Required because the R-level backend is serial (next section). |
| `JMP_PSO_STRICT_OBJ_TOL=0.1` | env var | Loosened from 0.01 default for bootstrap mode. The 0.01 gate was sized for the uniform-weight surface (NYC basin floor ~1.3e-3 + safety margin per [docs/wage_solver_stability.md](wage_solver_stability.md)); bootstrap reweighting shifts that floor by ~1 order of magnitude per draw. Cancelled job 49194150 had 38 strict-tol failures (worst Cook 0.023, worst NYC 0.032), and bench rep 1 (job 49320283) hit NYC polish_seed=0.0497 — only 0.7% under a 0.05 gate. 0.1 absorbs that tail with ~2x headroom over the bench. **Caveat:** NYC's "wrong basin" floor (~0.025) sits below 0.1, so the strict guard alone will not catch a rep that lands there — pair with a post-hoc filter in 08 that drops reps whose NYC ssq exceeds (e.g.) 0.1 or whose `wage_convergence` is non-zero. |
| `JMP_OBJ_TOL=1e-4` | env var | Loosened from 1e-6 default. The polish step's `reltol` was making Nelder-Mead burn 160+ evals stuck at the same value chasing precision the reweighted moment surface doesn't have. 1e-4 lets polish exit when relevant progress stops; cuts wall time materially. |

## Wage-stage convergence and fallback semantics

Each bootstrap rep runs PSO mode in the wage solver
([`estimate_wage_parameters_pso` in utils/structural_solver.R](../utils/structural_solver.R)).
There are three gates with different behaviors when they fail. The rep's
final `status` field in `boot_res_<i>.rds` reflects which gate (if any) fired.

### Per-county objective

For each county the solver computes
`best = min(raw_PSO, polish_from_PSO, polish_from_seed)` where:

- **raw PSO**: `pso_n_particles=40` × `pso_n_iter=100`, cold start (no
  particle pinned at the seed), search box ± `pso_search_halfwidth=2000`.
- **polish_from_PSO**: Nelder-Mead from the PSO global best.
- **polish_from_seed**: Nelder-Mead from the 06 warm-start vector.

Both polishes use `reltol = JMP_OBJ_TOL = 1e-4`. The "objective" is the
squared L2 norm of the weighted per-county wage moments
(model E – observed E across worker types 2..n_worker_types).

### Gate 1 — per-county strict objective (hard error)

If `best > JMP_PSO_STRICT_OBJ_TOL = 0.1` for any county, the solver calls
`stop(...)` with "PSO+polish did not reach pso_strict_obj_tol for county
&lt;cnty&gt;". 07_bootstrap.R's `tryCatch` writes a diagnostic
`boot_res_<i>.rds` with `status = "error"` and no parameter columns. **The
rep is lost** — it cannot be silently replaced by the 06 warm start, because
substituting uniform-weight estimates into the bootstrap distribution would
bias the standard errors. To recover these reps, either tighten PSO
settings (more particles, wider search box) and resubmit just those
iterations via `JMP_BOOTSTRAP_ITERATION`, or accept them as legitimate
non-convergence failures and exclude from the bootstrap distribution.

### Gate 2 — per-county accept (soft revert)

For each county where the candidate's **bound-guarded score**
(moment ssq + `structural_bound_guard_weight=10` × sum of bound violations²)
is worse than the seed's score, the county's parameters revert to the 06
warm-start values. `full_par[par_idx] <- result_par` simply does not fire;
the gate-2 branch keeps `start_par` for that county.

This is a numerical sanity check: PSO+polish must not produce a worse
candidate than starting from 06. Empirically gate 2 rarely fires because
the polish_from_seed branch keeps things at least as good as the warm
start by construction.

### Gate 3 — full-objective accept (soft revert, all counties)

After per-county updates, the full bound-guarded score across all three
counties is recomputed. If it exceeds the starting full score, the entire
`full_par` reverts to `initial_full_par` (06's wage values for every
county). This catches cases where per-county updates each look fine in
isolation but worsen the joint moment surface (e.g., gamma/E warm-start
cache interactions).

### Final return code → status

[`estimate_wage_parameters_pso` returns](../utils/structural_solver.R):

```r
convergence = if (global_accepted && all(county_converged)) 0L else 1L
```

where `county_converged[c]` requires that county c was accepted (gate 2 OK)
AND `final_objective_county[c] ≤ strict_tol`. 07_bootstrap.R reads this
return value and maps it to `status`:

| `boot_res$status` | Wage outcome | Parameter columns present? |
|---|---|---|
| `ok` | All counties' new candidates passed gate 1, gate 2, and the joint gate 3 | Yes (new estimates) |
| `wage_nonconverged` | Reached gate-1 strict-tol (best ≤ 0.1 for every county) BUT at least one county failed gate 2, or the joint check failed gate 3 → at least one county fell back to 06 | Yes (mix of new + 06 fallback, or all-06 in the gate-3 case) |
| `price_nonconverged` | Wage gates all passed; L-BFGS-B price step returned non-zero convergence | Yes (wage as above; price coeffs are whatever L-BFGS-B returned at termination) |
| `wage+price_nonconverged` | Both above | Yes |
| `error` | Gate 1 fired (best > 0.1 for some county) → solver threw, rep aborted | **No** (diagnostic row only with `error_message`) |

The price step always runs after wage (whether the wage vector is the new
candidate, a mix, or all-06 fallback). Bootstrap reps where the wage
vector is partially or fully 06-substituted will *still* re-estimate
prices under bootstrap weights using those wage values.

### Post-hoc filtering in 08

`pso_strict_obj_tol = 0.1` is loose enough that the gate-1 check cannot
distinguish a "right basin, reweighting-shifted floor" rep (e.g. NYC
polish_seed = 0.05) from a "wrong basin" rep (NYC basin floor ≈ 0.025
under uniform weights — see [wage_solver_stability.md](wage_solver_stability.md)).
The natural defense is a downstream filter in
`08_display_estimates.R` that drops or flags reps whose NYC moment ssq
exceeds some threshold, or whose `status` is non-`ok`. This filter has not
yet been added; without it, the combined `07_bootstrap.rds` validator
([`validate_bootstrap_results` in 07_bootstrap.R](../07_bootstrap.R)) will
reject any non-`ok` status outright when the combine pass runs.

## Why serial

In array mode, each task runs exactly **one** bootstrap iteration:

- [`resolve_bootstrap_iterations` in 07_bootstrap.R](../07_bootstrap.R) returns a single integer (`config$slurm_array_task_id`) when the env var is set.
- [`resolve_bootstrap_backend` in 07_bootstrap.R](../07_bootstrap.R) returns `"serial"` whenever a SLURM array ID is set OR there is only one iteration. So `mclapply` / `parLapply` paths are not taken.
- [`make_windows_solver_cluster` in utils/estimation_pipeline.R](../utils/estimation_pipeline.R) returns `NULL` on Linux, so there is no per-iteration solver cluster either.

That means a single rep is one R process doing `estimate_structural_parameters`
end to end, with BLAS pinned to one thread by the env vars above. Allocating 4
or 8 CPUs to each task wastes scheduling weight without speeding anything up.

## Renv guard in the script

`run_bootstrap_array.sl` runs an `Rscript --vanilla` pre-flight that sources
`renv/activate.R` and verifies that the direct runtime packages loaded by
`07_bootstrap.R`, `preamble.R`, and the shared estimation helpers are loadable
before launching the actual bootstrap. This turns "renv was not restored" into
an immediate, identifiable failure rather than a silent abort at the first
`library()` call.

Reference incident: SLURM job `48457046` (06_estimation) failed in 2 seconds
on `library("lubridate")` because the renv library had not been restored.
The guard makes that failure mode self-describing in the array log.

## When to revisit the specs

After the first successful array wave, run:

```bash
sacct -j <ARRAY_JOB_ID> --format=JobID,Elapsed,MaxRSS,State
```

Then:

- If `MaxRSS` is consistently below ~2 GB, drop `--mem` to 2g.
- If the slowest `Elapsed` is under ~45 min, drop `-t` to 1:00:00.
- If `--cpus-per-task=2` looks fully unused (it likely will), drop to 1.
- If queue throttling is not biting at %100, raise `%N` further.

Re-record any change in the spec table above and in the `#SBATCH` block of
`run_bootstrap_array.sl`.

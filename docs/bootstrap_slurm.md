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
| `--array=1-1100%100` | 1100 reps, 100 concurrent | `CONFIG$bootstrap_reps` is 1100 in [config.R](../config.R). At ~20-25 min/rep with %100 concurrency, the array finishes in ~11 waves (~4-5 h wall). |
| OMP/OpenBLAS/MKL/vecLib `_NUM_THREADS=1` | env vars | Force BLAS/OpenMP single-threaded so we don't silently oversubscribe the 2 allocated cores. Required because the R-level backend is serial (next section). |
| `JMP_PSO_STRICT_OBJ_TOL=0.05` | env var | Loosened from 0.01 default for bootstrap mode. The 0.01 gate was sized for the uniform-weight surface (NYC basin floor ~1.3e-3 + safety margin per [docs/wage_solver_stability.md](wage_solver_stability.md)); bootstrap reweighting shifts that floor by ~1 order of magnitude per draw. Cancelled job 49194150 had 38 strict-tol failures: worst Cook (17031) was 0.023 (n=30); worst NYC (36061) was 0.032 (n=8). 0.05 covers both with ~50% margin. **Caveat:** NYC's "wrong basin" floor (~0.025) sits below 0.05, so the strict guard alone won't catch a rep that lands there — pair with a post-hoc filter in 08. |
| `JMP_OBJ_TOL=1e-4` | env var | Loosened from 1e-6 default. The polish step's `reltol` was making Nelder-Mead burn 160+ evals stuck at the same value chasing precision the reweighted moment surface doesn't have. 1e-4 lets polish exit when relevant progress stops; cuts wall time materially. |

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

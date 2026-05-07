# Bootstrap SLURM Deployment

How `run_bootstrap_array.sl` is sized, and why. Update this file when the
specs in the script change.

## Submission

```bash
# Pre-flight (once, on a login node):
module load r/4.4.0
Rscript -e 'source("renv/activate.R"); renv::restore(prompt = FALSE)'

# Submit the array (200 reps, 50 concurrent):
sbatch run_bootstrap_array.sl

# After the array finishes, combine per-rep RDS files into 07_bootstrap.rds:
JMP_BOOTSTRAP_COMBINE_ONLY=true Rscript -e \
  'source("renv/activate.R"); source("07_bootstrap.R")'
```

The bootstrap reads `mkdata/data/04_estimation_sample.rds` and
`results/data/06_parameters.rds`, writes one
`results/data/bootstrap_reps/boot_res_<i>.rds` per array task, and combines
them into `results/data/07_bootstrap.rds` on the combine-only pass.

## Spec table

| Directive | Value | Rationale |
|---|---|---|
| `-p general` | general partition | Memory fits well under the 232 GB/node cap; no need for `general_big` or `bigmem`. |
| `-n 1` | one task per array element | Each rep is one process. |
| `--cpus-per-task=2` | 2 CPUs | Each rep is single-threaded (see "Why serial" below). One core would suffice; the second is headroom for `data.table` internal threads and any OpenMP region not covered by the env-var pinning. |
| `--mem=16g` | 16 GB | No measured peak yet. `04_estimation_sample.rds` is 272 KB on disk; `working_data` + `estim_matrix` + warm-start vector should peak well under 8 GB. 16 GB is conservative until we have `sacct MaxRSS` data; tighten after the first wave. |
| `-t 12:00:00` | 12 h | No measured full-rep wall time yet. The closest data points are: `06_estimation` cold (full wage + price, uniform weights) took 5:47 in job 48532114; smoke reps with `JMP_SKIP_STRUCTURAL_OPTIMIZER=true` (price L-BFGS-B only, bootstrap weights) took 15:54 and 21:13 in job 48608353. The reweighted price step is markedly slower than the uniform-weight one, so a full rep (wage + price under bootstrap weights) is plausibly longer than 06's 5:47, not faster. 12 h is a placeholder until we run a single full rep — see "When to revisit". |
| `--array=1-200%50` | 200 reps, 50 concurrent | `bootstrap_reps = 200` ([config.R:203](../config.R#L203)). Reps are independent, each uses 1 CPU, so %50 finishes the array in ~4 waves on the general partition. |
| OMP/OpenBLAS/MKL/vecLib `_NUM_THREADS=1` | env vars | Force BLAS/OpenMP single-threaded so we don't silently oversubscribe the 2 allocated cores. Required because the R-level backend is serial (next section). |

## Why serial

In array mode, each task runs exactly **one** bootstrap iteration:

- [07_bootstrap.R:39-52](../07_bootstrap.R#L39-L52) — `resolve_bootstrap_iterations` returns a single integer (`config$slurm_array_task_id`) when the env var is set.
- [07_bootstrap.R:152-164](../07_bootstrap.R#L152-L164) — `resolve_bootstrap_backend` returns `"serial"` whenever a SLURM array ID is set OR there is only one iteration. So `mclapply` / `parLapply` paths are not taken.
- [utils/estimation_pipeline.R:570-574](../utils/estimation_pipeline.R#L570-L574) — `make_windows_solver_cluster` returns `NULL` on Linux, so there is no per-iteration solver cluster either.

That means a single rep is one R process doing `estimate_structural_parameters`
end to end, with BLAS pinned to one thread by the env vars above. Allocating 4
or 8 CPUs to each task wastes scheduling weight without speeding anything up.

## Renv guard in the script

`run_bootstrap_array.sl` runs an `Rscript --vanilla` pre-flight that sources
`renv/activate.R` and verifies that `lubridate`, `data.table`, `BB`, `SQUAREM`,
and `nleqslv` are loadable before launching the actual bootstrap. This turns
"renv was not restored" into an immediate, identifiable failure rather than a
silent abort at the first `library()` call.

Reference incident: SLURM job `48457046` (06_estimation) failed in 2 seconds
on `library("lubridate")` because the renv library had not been restored.
The guard makes that failure mode self-describing in the array log.

## When to revisit the specs

After the first successful array wave, run:

```bash
sacct -j <ARRAY_JOB_ID> --format=JobID,Elapsed,MaxRSS,State
```

Then:

- If `MaxRSS` is consistently below ~6 GB, drop `--mem` to 8g.
- If the slowest `Elapsed` is under 6 h, drop `-t` to 8h or below.
- If `--cpus-per-task=2` looks fully unused (it likely will), drop to 1.

Re-record any change in the spec table above and in the `#SBATCH` block of
`run_bootstrap_array.sl`.

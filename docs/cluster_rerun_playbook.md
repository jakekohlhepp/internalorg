# Cluster Re-run Playbook (steps 04 → 22)

How to re-run the cluster half of the pipeline after the local front half has
been re-run and its derived files uploaded. Everything here is derived from
`sacct` history and from reading the scripts; each number cites the job it came
from so it can be recomputed.

Companion docs: `slurm_longleaf.md` §8 (local-vs-cluster split),
`data_dependencies.md` (full input map).

---

## 1. Pre-flight

The front half runs locally (it needs the confidential raw pull); the back half
runs here. Before submitting anything, check all four:

| Check | Command | Why |
|---|---|---|
| Code matches the uploaded data | `git fetch && git status -sb` | The local run may have used a commit this clone lacks. Fast-forward before running, or you compute new data with old code. |
| `mkdata/data/` is intact | `git status --short mkdata/data/` | An out-of-band sync that *replaces* the directory silently deletes the eight tracked inputs. Restore with `git checkout -- mkdata/data/`. |
| No stale self-check files | `ls results/data/07_first_stage_vcov.rds` | See §4. |
| renv is usable | `Rscript -e 'renv::status()'` | Compute nodes have no internet. `maps` and `minpack.lm` show as recorded-but-not-installed; both are `used = n`, so they are benign. |

`mkdata/data/` must hold **twelve** files before step 04 can run:

- Uploaded out-of-band (gitignored, from the local run):
  `01_working.rds`, `01_staff_task.rds`, `01_staff_task_full.rds`,
  `01_worker_type_lookup.rds`
- Committed (restorable from HEAD):
  `01_keytask.rds`, `cex_outside.rds`, `county_census_pop.rds`,
  `county_msa_xwalk.rds`, `minwage.xlsx`, `ppi.rds`, `qcew_county.rds`,
  `seeit_bb.rds`

`04_estimation_sample.rds` is **not** in git and does **not** need to be
uploaded — step 04 regenerates it deterministically (no RNG) from
`01_working.rds` + `01_staff_task_full.rds` + `ppi.rds` + `minwage.xlsx`.

---

## 2. What actually parallelizes

Measured as `TotalCPU / Elapsed` (effective cores), not assumed. This is the
single biggest source of wasted allocation in past runs — most steps are serial,
and requesting eight cores for them only lengthens the queue wait.

| Parallel primitive | Reached from |
|---|---|
| `mclapply(mc.cores = get_core_count())` in `utils/structural_solver.R:667-672` (`solve_worker_rows`) | 06, 06b, 06c, 08, 08b |
| `mclapply` in `09_invert_gammas.R:221`, `12b_validate_monotone.R:224` | 09, 12b |
| `counterfactual_apply_per_firm`, `utils/counterfactuals_core.R:729-736` | 13 |
| `mclapply(mc.cores = mt_config$mt_workers)`, `07_vcov.R:363` | 07 — **only if `JMP_MT_WORKERS` is exported** |

**Strictly serial — give them 1 CPU:** 04, 05, 12, 14, 15, 16, 17, 18, 19, 20,
21, 22. Steps 14–17 solve firms serially via `data.table by = location_id`;
historical 8- and 16-core requests measured 1.00–1.03 effective cores.

BLAS is pinned to one thread by convention, so cores only ever help via
`mclapply`.

---

## 3. Resource sizing

Every row cites the job it was measured from. `MaxRSS` comes from the `.batch`
row of `sacct` (it is absent on the `-X` summary row).

| Job | Script | CPUs | Mem | Time | Basis |
|---|---|---|---|---|---|
| `p04` | `04_estimation_sample.R` | 1 | 8g | 20m | 5.3 s, 0.20 GB (measured; never on Slurm) |
| `p05` | `05_iv_spec_comparison.R` | 1 | 8g | 20m | 10.7 s, 0.24 GB; serial `ivreg`/`lm` |
| `p06` | `06_estimation.R` | 8 | 24g | 2d | 57566182 = 14:41:33, MaxRSS 1.92 GB (family peak 2.11 GB). Since the 2026-07 county-parallel PSO + parallel-L5 changes (bit-identical, on by default; docs/structural_solver_speedups.md), the wage stage forks 3 county solves (x K with `JMP_WAGE_FB_L5_K>1`): request **24 CPUs** (mem unchanged; forks are copy-on-write) to realize the ~3x wall-clock cut |
| `p06b` | `06b_estimation_monotone.R` | 8 | 24g | 2d | 57566183 = 04:11:21, MaxRSS 2.33 GB. Same county-parallel note as `p06` |
| `p06c` | `06c_wage_identification.R` | 8 | 16g | 2d | 8c: 18 m–1:58. A 4-core attempt (58314149) **TIMED OUT at 4 h** |
| `p07` | `07_vcov.R` | 12 | 16g | 2d | 57652896 = 02:55:35, MaxRSS 3.18 GB, 9.63 effective cores |
| `p08` | `08_display_estimates.R` | 4 | 8g | 1h | 57669512 = 07:45, 2.24 effective cores |
| `p08b` | `08b_model_fit_monotone.R` | 4 | 8g | 1h | 58678664 = 13:45, 2.8 effective cores |
| `p09` | `09_invert_gammas.R` | 8 | 16g | 4h | 57669513 = 12:18, 2.99 effective cores |
| `p12` | `12_validate.R` | 1 | 48g | 2h | **OOM at 16g** (57347919, 51037613, 52343317). Completed at 32g with **MaxRSS 19.16 GB**; 0.98 effective cores |
| `p12b` | `12b_validate_monotone.R` | 8 | 48g | 4h | 58234555 = 41:28, MaxRSS 19.24 GB, 3.4 effective cores |
| `p13` | `13_counterfactual_prep.R` | 8 | 16g | 2d | 57775219 = **09:40:20**, MaxRSS 1.42 GB, 3.88 effective cores. A 16c/64g attempt gained nothing (1.16 cores over 18 h) |
| `p14`–`p17` | counterfactual scenarios | 1 | 16g | 2d | Single-threaded. Warm: ~2 min each. **Cold: 16 took 09:31:24** |
| `p18` | `18_counterfactual_summary.R` | 1 | 8g | 30m | 11 s |
| `p19` | `19_counterfactual_figures.R` | 1 | 8g | 30m | 40 s |
| `p20` | `20_substitution.R` | 1 | 8g | 1h | 08:14, plain `for` loops |
| `p21` | `21_substitution_prod.R` | 1 | 8g | 30m | 32 s |
| `p22` | `22_skill_parameter_units.R` | 1 | 8g | 30m | 44 s, 2 s TotalCPU |
| `pcheck` | `diagnostics/check_outputs_refreshed.R` | 1 | 8g | 20m | mtime manifest + CF convergence audit |

Memory is the one place not to economize: 12 and 12b peak at ~19.2 GB, so 32g
gives only 1.7× headroom. 48g is the safe number.

---

## 4. Traps

**`07_vcov.R` reads its own previous output back.** Lines 97–102 load
`results/data/07_first_stage_vcov.rds` if present and assert
`stopifnot(dev < 1e-8)` against the freshly computed vcov. After any change that
moves the estimates, the old file **will abort step 07**. Delete it first:

```bash
rm -f results/data/07_first_stage_vcov.rds
```

**`JMP_MT_WORKERS` is mandatory for 07.** `pl_on = FALSE` at `07_vcov.R:70`
means `mt_workers` defaults to 1, and the job runs serially for roughly 28 h
instead of ~3 h. Export it inside the wrap body:
`export JMP_MT_WORKERS=$SLURM_CPUS_PER_TASK`.

**Warm starts are indexed by worker type.** After anything that relabels worker
types (e.g. `cluster.R`'s `assign_worker_types_baseline`), the counterfactual
warm-start wage vectors in `results/data/counterfactuals/*_warm_start_wages*.rds`
are old-label equilibria. The reads are guarded (`utils/counterfactuals_core.R:434`
returns `NULL` when absent), so clearing them is safe, and a stale warm start has
already caused a real failure: job 57981008 sat for 18 h stuck in the
nleqslv/coord-descent fallback ladder on county 36061. The NYC wage problem is
documented as multi-basin, so a wrong-label seed can converge to a wrong
equilibrium, not merely a slow one. Clear them after a relabel.

`mkdata/data/seeit_bb.rds` (step 06's start vector) has the same provenance
issue, but 06 searches far more broadly than the wage root-finders, so keeping it
is the usual call. Cold-starting 06 has historically taken 44:53:29 — which fits
a 2-day wall by only ~3 h.

**Every numbered script runs bare.** `Rscript NN_name.R` from the repo root works
for all 21 of them (verified by executing each in an isolated tree); none needs
to be sourced from `run_all.R`. The cwd must be the repo root — the scripts call
`source("config.R")` on a relative path, and `.Rprofile` activates renv.

---

## 5. The DAG

Verified against the source: every parallel edge below was checked for
read-after-write and write-after-write conflicts, and none exists.

```
04 → 05 → 06 → 06b → 06c → 07 → 08 → 08b → 09 → 12 → 12b → 13 ─┬→ 14 ─┐
                                                                ├→ 15 ─┤
                                                                ├→ 16 ─┼→ 18 → 19 → 20 → 21 → 22 → pcheck
                                                                └→ 17 ─┘
```

14/15/16/17 are mutually independent: each reads only 13's outputs plus the
estimation parameters, and each writes only its own
`results/data/counterfactuals/NN_*` files. 18 summarizes all four, so it joins
on all of them.

More parallelism is available than the chain above uses, if you want it: 06c
depends only on **04** (not on 06 — it re-optimizes from `seeit_bb.rds`), and
05, 07, 09, and 22 have no dependency on each other. The chain is kept mostly
sequential because the tail steps cost minutes and a linear chain is far easier
to reason about when a step fails.

---

## 6. Submitting

Per project convention, use `sbatch --wrap` — do **not** add per-script `.sl` /
`.sbatch` files (they are gitignored boilerplate). Write a start stamp *before*
submitting so the refresh check has a threshold, chain with
`--dependency=afterok`, and end with an `afterany` check so it still reports when
an upstream step fails.

```bash
ENVPIN='set -euo pipefail; cd "$SLURM_SUBMIT_DIR"; module load r/4.4.0; export OMP_NUM_THREADS=1 OPENBLAS_NUM_THREADS=1 MKL_NUM_THREADS=1 VECLIB_MAXIMUM_THREADS=1 R_DATATABLE_NUM_THREADS=1 LC_ALL=C;'

date +"%Y-%m-%d %H:%M:%S %z" > logs/pipeline_start.stamp

sbatch --parsable -p general -n 1 --cpus-per-task=8 --mem=24g -t 2-00:00:00 \
  -J p06 -o logs/p06_%j.out --dependency=afterok:$J05 \
  --wrap="$ENVPIN Rscript 06_estimation.R"
```

`$SLURM_SUBMIT_DIR` and `$SLURM_CPUS_PER_TASK` must be single-quoted at
definition so the compute node expands them, not the submitting shell.

The final job closes the loop:

```bash
JMP_REFRESH_SINCE=logs/pipeline_start.stamp Rscript diagnostics/check_outputs_refreshed.R
```

It exits 0 only if every output in its manifest exists, post-dates the stamp, and
every counterfactual wage cell converged at the focus quarter — which is what
catches a step that silently skipped or died and left a stale mtime behind.

---

## 7. Comparing a re-run against the previous vintage

All 59 back-half table/figure outputs in the `check_outputs_refreshed` manifest
are git-tracked, so the previous vintage is the git baseline. Commit the on-disk
outputs *before* launching, then after the run `git diff --stat results/out/`
shows exactly what moved. Anything untracked is invisible to that diff, which is
why the manifest and the tracked set need to stay in sync.

Expected end-to-end wall on the current vintage: roughly 35 h, dominated by 06
(~15 h) and 13 (~10 h).

*Last updated: 2026-07-13.*

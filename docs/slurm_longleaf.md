# UNC Longleaf Slurm Documentation (V3 - Post-Audit)

This document provides procedures for submitting jobs to the UNC Longleaf cluster.

## 1. Environment Preparation (Login Node Only)
**CRITICAL:** Longleaf compute nodes do **not** have internet access. You must prepare your `renv` environment on a login node before submitting a batch job.

```bash
# On a login node (longleaf.its.unc.edu):
module load r/4.4.0
Rscript -e "renv::restore()"
```
If you fail to do this, your batch job will hang or crash when it attempts to download packages.

## 2. Core Submission Command: `sbatch`
Always submit from the project root.

### Common SBATCH Directives
| Directive | Description | Example |
| :--- | :--- | :--- |
| `-n 1` | Tasks per job. Always use 1 for this pipeline. | `#SBATCH -n 1` |
| `--cpus-per-task` | Number of cores. Matches `get_core_count()` logic. | `#SBATCH --cpus-per-task=16` |
| `--mem` | Memory per node. Request a 10-20% buffer. | `#SBATCH --mem=64g` |
| `-t <time>` | Time limit (D-HH:MM:SS). | `#SBATCH -t 1-00:00:00` |

## 3. Parallelism Logic
The project's `config.R` now automatically detects Slurm allocations.
- It prioritizes `SLURM_CPUS_PER_TASK`.
- If no Slurm variable is found, it falls back to **1 core** on Linux to prevent accidental "core-bombing" of shared login nodes.
- **Manual Override:** You can still set `export MC_CORES=X` in your shell script to force a specific core count.

## 4. Partitions & 2026 Resource Caps
| Partition | Purpose | Resource Caps (Enforced) |
| :--- | :--- | :--- |
| **`general`** | CPU jobs | Max 11 days. |
| **`interact`** | Debugging | Max 8 cores / 64GB RAM. |
| **`a100-gpu`** | GPU | **Cap:** 16 CPUs and 128GB RAM per GPU requested. |

## 5. Slurm Job Arrays
Standard errors no longer use a job array: they come from the analytical
Murphy-Topel sandwich computed in a single `07_vcov.R` job (the Petrin-Train
bootstrap that used arrays is retired to `legacy/`; its array notes live in
`legacy/bootstrap_slurm.md`). Job arrays are still useful for other embarrassingly
parallel sweeps (e.g. SE-validation batches, counterfactual scenarios). Generic
template:

```bash
#!/bin/bash
#SBATCH -p general
#SBATCH -n 1
#SBATCH --cpus-per-task=4
#SBATCH --mem=16g
#SBATCH -t 08:00:00
#SBATCH --array=1-200%20   # Run 200 tasks, 20 at a time
#SBATCH -o logs/task_%a.out

cd $SLURM_SUBMIT_DIR
module load r/4.4.0

# The script detects $SLURM_ARRAY_TASK_ID and runs only that task
Rscript <your_array_script>.R
```

## 6. Storage & I/O Best Practices
- **Home:** Do **not** write large intermediate results or temp files here.
- **Scratch (`/pine/scr/...`):** Use for large intermediate files. Remember the **36-day deletion policy**.
- **Project (`/proj/...`):** Store final `results/` and raw data here.

## 7. Troubleshooting
- **Job OOM:** If you see `Out Of Memory` errors, increase `--mem`. R's `mclapply` can have high memory overhead.
- **Immediate Failure:** Check `logs/` for "package not found" errors. Ensure you ran `renv::restore()` on a login node.
- **Pending Forever:** Check if you violated GPU caps (requesting >16 CPUs for a single GPU job).

## 8. Pipeline stages: where each runs (local vs cluster)

The pipeline splits at the **raw-data boundary**. The front-half prep needs the
confidential raw transaction pull plus external geo/Census files that live only
on the author's local machine, so it runs **locally**. Everything downstream
executes on the **cluster** (all heavy compute goes through Slurm — never the
login node) from two kinds of inputs: **committed** files (aggregate parameters,
warm starts, and public-source-derived prep outputs tracked in the repo) and
**derived microdata transferred out-of-band** (see "Getting the derived inputs
onto the cluster" below).

**Local only — raw inputs, not reproducible on the cluster:**
- `prep_00_compile_transactions.R` … `prep_06_qcew.R` — raw → `mkdata/data/*`
  (the public-source-derived outputs are committed; the rest stay local)
- `00_mk_tasks_cosmo.R` — raw `compiled_trxns.rds` + geocorr
- `01_build_data.R` — needs `00_tasks_cosmo.rds`
- `02_stylized_facts.R`, `03_spatial_corr.R` — raw pulls + Census/ZCTA shapefiles

**Cluster — the Slurm workloads:**
- Estimation block: `04_estimation_sample.R` → `08_display_estimates.R`
  (`05`, `06`, `06b`, `06c`, `07`). The structural estimation `06` is the heavy job here.
- Counterfactual back-half: `09_invert_gammas.R`, `12_validate.R`,
  `run_counterfactuals.R` (`13`–`19`),
  `20_substitution.R`, `21_substitution_prod.R`, `22_skill_parameter_units.R`.
  The counterfactual wage solves (`13`–`17`) are the heavy jobs; they use the
  hardened tolerances now baked into `config.R` (see `counterfactual_tolerances.md`).

The warm-start seeds (`13_warm_start_wages.rds` and
`14_/15_/16_/17_warm_start_wages_*.rds`) are committed inputs;
`compile_warm_start_wages.R` (baseline) and `build_la_warm_start_*.R`
(per-counterfactual) are manual refresh tools run by hand, not part of
`run_all.R`.

### Getting the derived inputs onto the cluster

The firm/staff-level derived files the cluster half reads are pseudonymized
microdata from the confidential source, so they are **not committed** to the
public repo (see the data-availability section of the root README). Copy them
into the cluster clone out-of-band:

```bash
scp mkdata/data/{01_working.rds,01_staff_task.rds,01_staff_task_full.rds,01_worker_type_lookup.rds,04_estimation_sample.rds} \
    <user>@longleaf.its.unc.edu:<project>/mkdata/data/
# Only needed when skipping straight to the counterfactual back-half
# (a full cluster run regenerates these from 04_estimation_sample.rds):
scp results/data/{09_withgammas.rds,12_data_for_counterfactuals.rds} \
    <user>@longleaf.its.unc.edu:<project>/results/data/
```

Everything else the cluster half needs is committed: `seeit_bb.rds`,
`minwage.xlsx`, `ppi.rds`, `cex_outside.rds`, `county_msa_xwalk.rds`,
`01_keytask.rds`, `results/data/06_parameters.rds`, and the warm-start seeds.

### Running the cluster portion reproducibly

To re-run the cluster portion from the committed estimation outputs, run the
master script with the front-half disabled and determinism factors pinned:

```bash
# In run_all.R, set RUN_* for steps 00-08 to FALSE (06c has no RUN_ flag and
# runs by default via RUN_WAGE_IDENTIFICATION being unset -- leave it on).
export OMP_NUM_THREADS=1 OPENBLAS_NUM_THREADS=1 MKL_NUM_THREADS=1 \
       VECLIB_MAXIMUM_THREADS=1 R_DATATABLE_NUM_THREADS=1 LC_ALL=C
Rscript -e 'source("renv/activate.R"); source("run_all.R")'
```

Single-thread BLAS + a fixed locale are required for bit-identical outputs across
runs and core counts (the solver RNG is already deterministic via
`counterfactual_stable_seed`). Exclude figures (`*.png`) from bit-identity checks
(device/timestamp noise).

---
*Last Updated: 2026-06-29 — added §8 (local vs cluster execution map). Prior audit: April 2026.*

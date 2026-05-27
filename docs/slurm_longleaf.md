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

## 5. Slurm Job Arrays (Bootstrap)
The bootstrap script (`07_bootstrap.R`) supports Slurm Job Arrays. This is highly recommended to avoid exceeding wall-time limits.

### Example Array Script (`run_bootstrap_array.sl`)
```bash
#!/bin/bash
#SBATCH -p general
#SBATCH -n 1
#SBATCH --cpus-per-task=4
#SBATCH --mem=16g
#SBATCH -t 08:00:00
#SBATCH --array=1-200%20   # Run 200 reps, 20 at a time
#SBATCH -o logs/boot_%a.out

cd $SLURM_SUBMIT_DIR
module load r/4.4.0

# The script detects $SLURM_ARRAY_TASK_ID and runs only that rep
Rscript 07_bootstrap.R
```

## 6. Storage & I/O Best Practices
- **Home:** Do **not** write large intermediate results or temp files here.
- **Scratch (`/pine/scr/...`):** Use for large intermediate files. Remember the **36-day deletion policy**.
- **Project (`/proj/...`):** Store final `results/` and raw data here.

## 7. Troubleshooting
- **Job OOM:** If you see `Out Of Memory` errors, increase `--mem`. R's `mclapply` can have high memory overhead.
- **Immediate Failure:** Check `logs/` for "package not found" errors. Ensure you ran `renv::restore()` on a login node.
- **Pending Forever:** Check if you violated GPU caps (requesting >16 CPUs for a single GPU job).

---
*Last Updated: April 2026 (Audited by Grouchy Programmer)*

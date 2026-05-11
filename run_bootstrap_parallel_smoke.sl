#!/bin/bash
#SBATCH -p general
#SBATCH -n 1
#SBATCH --cpus-per-task=4
#SBATCH --mem=12g
#SBATCH -t 00:30:00
#SBATCH -J 07_boot_par_smoke
#SBATCH -o logs/boot_par_smoke_%j.out

# Single-job parallel smoke for 07_bootstrap.R. No --array, so
# resolve_bootstrap_backend() picks the multicore backend and 4 reps run
# concurrently inside this one job. JMP_SKIP_STRUCTURAL_OPTIMIZER=true keeps
# each rep cheap so we only verify that the 06_parameters.rds warm-start
# path (parm_name filtered on :avg_labor:E_raw_*), the weighted estimation
# pipeline, and per-rep RDS writes complete without crash.

set -euo pipefail

cd "$SLURM_SUBMIT_DIR"
module load r/4.4.0

export OMP_NUM_THREADS=1
export OPENBLAS_NUM_THREADS=1
export MKL_NUM_THREADS=1
export VECLIB_MAXIMUM_THREADS=1

export JMP_SKIP_STRUCTURAL_OPTIMIZER=true
export JMP_BOOTSTRAP_REPS=4
export JMP_BOOTSTRAP_RESULTS_DIR=results/data/bootstrap_reps_par_smoke
export JMP_BOOTSTRAP_WORKERS=4

mkdir -p "$JMP_BOOTSTRAP_RESULTS_DIR"

Rscript 07_bootstrap.R

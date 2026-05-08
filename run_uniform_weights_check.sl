#!/bin/bash
#SBATCH -p general
#SBATCH -n 1
#SBATCH --cpus-per-task=2
#SBATCH --mem=8g
#SBATCH -t 02:00:00
#SBATCH -J 07_uniform_check
#SBATCH -o logs/uniform_weights_check_%j.out

# Sanity check that the bootstrap path with per-row weights = 1/N reproduces
# the cold 06_estimation result. Drives tests/bootstrap_uniform_weights_check.R.

set -euo pipefail

cd "$SLURM_SUBMIT_DIR"
module load r/4.4.0

export OMP_NUM_THREADS=1
export OPENBLAS_NUM_THREADS=1
export MKL_NUM_THREADS=1
export VECLIB_MAXIMUM_THREADS=1

Rscript tests/bootstrap_uniform_weights_check.R

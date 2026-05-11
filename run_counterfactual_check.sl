#!/bin/bash
#SBATCH -p general
#SBATCH -n 1
#SBATCH --cpus-per-task=8
#SBATCH --mem=32g
#SBATCH -t 24:00:00
#SBATCH -J cf_check
#SBATCH -o logs/counterfactual_check_%j.out

set -euo pipefail

cd "$SLURM_SUBMIT_DIR"
module load r/4.4.0

export OMP_NUM_THREADS=1
export OPENBLAS_NUM_THREADS=1
export MKL_NUM_THREADS=1
export VECLIB_MAXIMUM_THREADS=1

Rscript run_counterfactual_check.R

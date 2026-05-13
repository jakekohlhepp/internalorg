#!/bin/bash
#SBATCH -p general
#SBATCH -n 1
#SBATCH --cpus-per-task=4
#SBATCH --mem=16g
#SBATCH -t 0:30:00
#SBATCH -J 18_summary
#SBATCH -o logs/18_counterfactual_summary_%j.out

set -euo pipefail

cd "$SLURM_SUBMIT_DIR"
module load r/4.4.0

export OMP_NUM_THREADS=1
export OPENBLAS_NUM_THREADS=1
export MKL_NUM_THREADS=1
export VECLIB_MAXIMUM_THREADS=1

Rscript 18_counterfactual_summary.R

#!/bin/bash
#SBATCH -p general
#SBATCH -n 1
#SBATCH --cpus-per-task=1
#SBATCH --mem=4g
#SBATCH -t 00:05:00
#SBATCH -J peek_skill_matrix
#SBATCH -o logs/peek_skill_matrix_%j.out

set -euo pipefail

cd "$SLURM_SUBMIT_DIR"
module load r/4.4.0

export OMP_NUM_THREADS=1
export OPENBLAS_NUM_THREADS=1
export MKL_NUM_THREADS=1
export VECLIB_MAXIMUM_THREADS=1

Rscript peek_skill_matrix.R

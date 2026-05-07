#!/bin/bash
#SBATCH -p general
#SBATCH -n 1
#SBATCH --cpus-per-task=8
#SBATCH --mem=16g
#SBATCH -t 06:00:00
#SBATCH -J 06_estimation
#SBATCH -o logs/06_estimation_%j.out

set -euo pipefail

cd "$SLURM_SUBMIT_DIR"
module load r/4.4.0

export OMP_NUM_THREADS=1
export OPENBLAS_NUM_THREADS=1
export MKL_NUM_THREADS=1
export VECLIB_MAXIMUM_THREADS=1

export JMP_WAGE_OPTIMIZER_MODE=min_optim
export JMP_MIN_OPTIM_TRACE=1

Rscript 06_estimation.R

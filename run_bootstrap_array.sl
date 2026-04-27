#!/bin/bash
#SBATCH -p general
#SBATCH -n 1
#SBATCH --cpus-per-task=4
#SBATCH --mem=16g
#SBATCH -t 08:00:00
#SBATCH --array=1-200%20
#SBATCH -o logs/boot_%a.out

set -euo pipefail

cd "$SLURM_SUBMIT_DIR"
module load r/4.4.0

export OMP_NUM_THREADS=1
export OPENBLAS_NUM_THREADS=1
export MKL_NUM_THREADS=1
export VECLIB_MAXIMUM_THREADS=1

# Prepare renv on a Longleaf login node before submitting this job:
# module load r/4.4.0
# Rscript -e "renv::restore()"
#
# After the array finishes, combine per-rep files with:
# JMP_BOOTSTRAP_COMBINE_ONLY=true Rscript 07_bootstrap.R

Rscript 07_bootstrap.R

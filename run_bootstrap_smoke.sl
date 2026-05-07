#!/bin/bash
#SBATCH -p general
#SBATCH -n 1
#SBATCH --cpus-per-task=2
#SBATCH --mem=8g
#SBATCH -t 01:00:00
#SBATCH --array=1-2
#SBATCH -J 07_boot_smoke
#SBATCH -o logs/boot_smoke_%A_%a.out

# Smoke deployment of the bootstrap array. Two-rep test that
#   1. confirms 07_bootstrap.R runs end-to-end on Longleaf,
#   2. exercises the per-rep RDS write at results/data/bootstrap_reps_smoke/,
#   3. shakes out path / IO / module errors before the real array.
#
# JMP_SKIP_STRUCTURAL_OPTIMIZER=true makes each rep skip the BBsolve wage
# solve (the expensive step), so a smoke rep finishes in seconds rather than
# hours. Each rep still does the demand regressions and price L-BFGS-B.
#
# This script intentionally does NOT source renv/activate.R: the project
# renv library is currently empty (only renv itself is linked), and the
# user-level library at ~/R/x86_64-pc-linux-gnu-library/4.4/ has all the
# packages 06_estimation has been running against. Once renv::restore()
# fully populates the project library, the production run_bootstrap_array.sl
# (which goes through renv) becomes the canonical entry point.

set -euo pipefail

cd "$SLURM_SUBMIT_DIR"
module load r/4.4.0

export OMP_NUM_THREADS=1
export OPENBLAS_NUM_THREADS=1
export MKL_NUM_THREADS=1
export VECLIB_MAXIMUM_THREADS=1

export JMP_SKIP_STRUCTURAL_OPTIMIZER=true
export JMP_BOOTSTRAP_RESULTS_DIR=results/data/bootstrap_reps_smoke

mkdir -p "$JMP_BOOTSTRAP_RESULTS_DIR"

Rscript 07_bootstrap.R

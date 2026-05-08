#!/bin/bash
#SBATCH -p general
#SBATCH -n 1
#SBATCH --cpus-per-task=2
#SBATCH --mem=4g
#SBATCH -t 00:15:00
#SBATCH -J gamma_cache_drill
#SBATCH -o logs/gamma_cache_drill_%j.out

# Drill-down: confirm whether get_gammas output depends on solver_state
# (specifically state$last_moment_norm, which flips coarse vs fine tols
# for the inner equilibrium solves).

set -euo pipefail

cd "$SLURM_SUBMIT_DIR"
module load r/4.4.0

export OMP_NUM_THREADS=1
export OPENBLAS_NUM_THREADS=1
export MKL_NUM_THREADS=1
export VECLIB_MAXIMUM_THREADS=1

Rscript tests/gamma_cache_drill.R

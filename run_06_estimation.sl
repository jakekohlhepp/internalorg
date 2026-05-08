#!/bin/bash
#SBATCH -p general
#SBATCH -n 1
#SBATCH --cpus-per-task=8
#SBATCH --mem=16g
#SBATCH -t 12:00:00
#SBATCH -J 06_estimation
#SBATCH -o logs/06_estimation_%j.out

set -euo pipefail

cd "$SLURM_SUBMIT_DIR"
module load r/4.4.0

export OMP_NUM_THREADS=1
export OPENBLAS_NUM_THREADS=1
export MKL_NUM_THREADS=1
export VECLIB_MAXIMUM_THREADS=1

# Force PSO for the wage stage. docs/wage_solver_stability.md establishes that
# nleqslv and min_optim dead-end at NYC's worse basin (ssq ~0.025); PSO is the
# only solver that finds the manuscript-shape basin (ssq ~1.3e-3).
export JMP_WAGE_OPTIMIZER_MODE=pso
export JMP_MIN_OPTIM_TRACE=1

echo "JMP_WAGE_OPTIMIZER_MODE=${JMP_WAGE_OPTIMIZER_MODE}"
echo "JMP_MIN_OPTIM_TRACE=${JMP_MIN_OPTIM_TRACE}"

Rscript 06_estimation.R

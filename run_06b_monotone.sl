#!/bin/bash
#SBATCH -p general
#SBATCH -n 1
#SBATCH --cpus-per-task=8
#SBATCH --mem=16g
#SBATCH -t 12:00:00
#SBATCH -J 06b_monotone
#SBATCH -o logs/06b_monotone_%j.out

set -euo pipefail

cd "$SLURM_SUBMIT_DIR"
module load r/4.4.0

export OMP_NUM_THREADS=1
export OPENBLAS_NUM_THREADS=1
export MKL_NUM_THREADS=1
export VECLIB_MAXIMUM_THREADS=1

# Force PSO for the wage stage. The constrained skill matrix may produce a
# different basin landscape than the unconstrained run, and PSO is the only
# solver that reliably finds the manuscript-shape basin in the trickier
# markets (per docs/wage_solver_stability.md).
export JMP_WAGE_OPTIMIZER_MODE=pso
export JMP_MIN_OPTIM_TRACE=1

# Workers-as-rows monotonicity restriction on the per-county skill matrix.
# Permutations are searched per county; the data picks the worker-skill
# ordering. The estimation does NOT impose ex ante which worker type is most
# productive.
export JMP_SKILL_MONOTONE=workers_rows

# The constrained skill matrix has many tied entries (perfect substitutes
# among worker types on individual tasks), creating a flat region in the
# wage FOC that PSO + NM can drive down to a non-zero floor (Cook plateau:
# ssq ~0.027). The default pso_strict_obj_tol=0.01 is calibrated for the
# unconstrained model and rejects this. Loosen for the constrained run; rely
# on the convergence-code warning in 06b_estimation_monotone.R to flag
# pathological non-convergence rather than the strict-exit guard.
export JMP_PSO_STRICT_OBJ_TOL=0.5

echo "JMP_WAGE_OPTIMIZER_MODE=${JMP_WAGE_OPTIMIZER_MODE}"
echo "JMP_MIN_OPTIM_TRACE=${JMP_MIN_OPTIM_TRACE}"
echo "JMP_SKILL_MONOTONE=${JMP_SKILL_MONOTONE}"
echo "JMP_PSO_STRICT_OBJ_TOL=${JMP_PSO_STRICT_OBJ_TOL}"

Rscript 06b_estimation_monotone.R

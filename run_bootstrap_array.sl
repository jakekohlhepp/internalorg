#!/bin/bash
#SBATCH -p general
#SBATCH -n 1
#SBATCH --cpus-per-task=2
#SBATCH --mem=4g
#SBATCH -t 10:00:00
#SBATCH --array=1-1100%100
#SBATCH -J 07_bootstrap
#SBATCH -o logs/boot_%A_%a.out

# Spec rationale lives in docs/bootstrap_slurm.md. Tuned 2026-05-09 against
# bench (jobs 48614866-70: ~1:34-1:41h, MaxRSS ~650 MB, no-PSO defaults) and
# the most recent PSO+warm-start parallel smoke (job 49173662: 13-23 min/rep
# under 4-way multicore contention, MaxRSS 1.07 GB). Array-mode reps run solo
# on their CPUs, so per-rep wall should sit well under the new -t budget.

set -euo pipefail

cd "$SLURM_SUBMIT_DIR"
module load r/4.4.0

# Each rep is single-threaded at the R level (07_bootstrap.R picks the "serial"
# backend whenever SLURM_ARRAY_TASK_ID is set). Pin BLAS/OpenMP to 1 so we don't
# silently oversubscribe the 2 allocated cores.
export OMP_NUM_THREADS=1
export OPENBLAS_NUM_THREADS=1
export MKL_NUM_THREADS=1
export VECLIB_MAXIMUM_THREADS=1

# Force PSO for the wage stage so bootstrap reps land in the same basin as
# 06_estimation. docs/wage_solver_stability.md establishes that nleqslv and
# min_optim dead-end at NYC's worse basin (ssq ~0.025); PSO is the only solver
# that finds the manuscript-shape basin (ssq ~1.3e-3).
export JMP_WAGE_OPTIMIZER_MODE=pso
export JMP_MIN_OPTIM_TRACE=1

# Bootstrap-mode tolerance loosenings (rationale in docs/bootstrap_slurm.md):
#   - PSO_STRICT_OBJ_TOL: the 0.01 default was sized for the uniform-weight
#     surface (NYC basin floor ~1.3e-3 + ~8x margin). Bootstrap reweighting
#     shifts that floor materially; cancelled job 49194150 saw 38 strict-tol
#     failures (worst Cook 0.023, worst NYC 0.032), and bench rep 1 (job
#     49320283) hit NYC polish_seed=0.0497 -- only 0.7% under a 0.05 gate.
#     0.1 absorbs that tail with ~2x headroom over the bench.
#   - OBJ_TOL: the 1e-6 polish reltol made Nelder-Mead burn 160+ evals stuck
#     at the same value chasing precision the reweighted moment surface
#     doesn't have. 1e-4 lets polish exit when relevant progress stops.
# Caveat: NYC's "wrong basin" floor (~0.025) sits below 0.1, so the strict
# guard alone will not catch a rep that lands there. Pair with a post-hoc
# filter in 08_display_estimates.R that drops reps whose NYC ssq exceeds
# (e.g.) 0.1 or whose wage_convergence is non-zero.
export JMP_PSO_STRICT_OBJ_TOL=0.1
export JMP_OBJ_TOL=1e-4

# Fail fast if the renv project library is missing or direct runtime packages
# are not loadable. Job 48457046 (06_estimation) burned a slot exiting at the
# library(lubridate) call because renv had not been restored yet; this guard
# turns that failure mode into an immediate, obvious one.
if [[ ! -f renv/activate.R ]]; then
  echo "ERROR: renv/activate.R not found. Run setup_renv.R on a login node first." >&2
  exit 2
fi
Rscript --vanilla -e '
  source("renv/activate.R")
  required <- c(
    "data.table", "lubridate", "stringr", "zoo", "lessR", "xtable",
    "gmm", "readxl", "nloptr", "BB", "SQUAREM", "nleqslv", "ivreg"
  )
  missing <- required[!vapply(required, requireNamespace, logical(1), quietly = TRUE)]
  if (length(missing)) {
    stop("renv library is missing required packages: ",
         paste(missing, collapse = ", "),
         ". Run renv::restore() on a login node before submitting.")
  }
' || exit 3

# After the array finishes, combine per-rep files with:
#   JMP_BOOTSTRAP_COMBINE_ONLY=true Rscript -e 'source("renv/activate.R"); source("07_bootstrap.R")'

Rscript -e 'source("renv/activate.R"); source("07_bootstrap.R")'

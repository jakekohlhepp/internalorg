#!/bin/bash
#SBATCH -p general
#SBATCH -n 1
#SBATCH --cpus-per-task=2
#SBATCH --mem=16g
#SBATCH -t 12:00:00
#SBATCH --array=1-200%50
#SBATCH -J 07_bootstrap
#SBATCH -o logs/boot_%A_%a.out

# Spec rationale lives in docs/bootstrap_slurm.md.

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

# Fail fast if the renv project library is missing or a representative package
# is not loadable. Job 48457046 (06_estimation) burned a slot exiting at the
# library(lubridate) call because renv had not been restored yet; this guard
# turns that failure mode into an immediate, obvious one.
if [[ ! -f renv/activate.R ]]; then
  echo "ERROR: renv/activate.R not found. Run setup_renv.R on a login node first." >&2
  exit 2
fi
Rscript --vanilla -e '
  source("renv/activate.R")
  required <- c("lubridate", "data.table", "BB", "SQUAREM", "nleqslv")
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

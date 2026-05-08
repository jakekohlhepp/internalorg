#!/bin/bash
#SBATCH -p general
#SBATCH -n 1
#SBATCH --cpus-per-task=2
#SBATCH --mem=8g
#SBATCH -t 02:00:00
#SBATCH -J p_adj_drill
#SBATCH -o logs/p_adj_path_drill_%j.out

# Run estimate_structural_parameters two ways back-to-back in one R session:
# (A) 06-style with explicit beta + beta_2_subset, (B) bootstrap-style with
# uniform weights. Compare parameter_table row-by-row to localize the divergence.

set -euo pipefail

cd "$SLURM_SUBMIT_DIR"
module load r/4.4.0

export OMP_NUM_THREADS=1
export OPENBLAS_NUM_THREADS=1
export MKL_NUM_THREADS=1
export VECLIB_MAXIMUM_THREADS=1

Rscript tests/p_adj_path_drill.R

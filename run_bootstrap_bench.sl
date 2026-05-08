#!/bin/bash
#SBATCH -p general
#SBATCH -n 1
#SBATCH --cpus-per-task=2
#SBATCH --mem=12g
#SBATCH -t 03:00:00
#SBATCH --array=1
#SBATCH -J 07_boot_bench
#SBATCH -o logs/boot_bench_%A_%a.out

# Single-rep benchmark harness for 07_bootstrap.R. The configuration to test
# is supplied via env vars at submit time:
#   sbatch --export=ALL,JMP_BOOTSTRAP_RESULTS_DIR=results/data/bench_x,JMP_FOO=bar \
#          run_bootstrap_bench.sl
# Each invocation runs rep 1 (same bootstrap weights given the seed) and
# writes boot_res_1.rds to JMP_BOOTSTRAP_RESULTS_DIR, so multiple configs
# can be compared side-by-side.

set -euo pipefail

cd "$SLURM_SUBMIT_DIR"
module load r/4.4.0

: "${OMP_NUM_THREADS:=1}"
: "${OPENBLAS_NUM_THREADS:=1}"
: "${MKL_NUM_THREADS:=1}"
: "${VECLIB_MAXIMUM_THREADS:=1}"
: "${JMP_BOOTSTRAP_RESULTS_DIR:=results/data/bench_default}"
export OMP_NUM_THREADS OPENBLAS_NUM_THREADS MKL_NUM_THREADS VECLIB_MAXIMUM_THREADS JMP_BOOTSTRAP_RESULTS_DIR

mkdir -p "$JMP_BOOTSTRAP_RESULTS_DIR"

echo "==== BENCH CONFIG ===="
echo "  JMP_BOOTSTRAP_RESULTS_DIR=$JMP_BOOTSTRAP_RESULTS_DIR"
echo "  JMP_SKIP_STRUCTURAL_OPTIMIZER=${JMP_SKIP_STRUCTURAL_OPTIMIZER:-unset}"
echo "  JMP_USE_RCPP_EQUILIBRIUM=${JMP_USE_RCPP_EQUILIBRIUM:-unset}"
echo "  JMP_WAGE_OPTIMIZER_MODE=${JMP_WAGE_OPTIMIZER_MODE:-unset}"
echo "  JMP_COARSE_INNERTOL=${JMP_COARSE_INNERTOL:-unset}"
echo "  JMP_COARSE_OUTERTOL=${JMP_COARSE_OUTERTOL:-unset}"
echo "  cpus-per-task=$SLURM_CPUS_PER_TASK"
echo "  OMP_NUM_THREADS=$OMP_NUM_THREADS"
echo "  array_task_id=$SLURM_ARRAY_TASK_ID"
echo "===================="

Rscript 07_bootstrap.R

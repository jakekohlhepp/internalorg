## Re-run iter 26 with full diagnostic to dump county_results values and
## convergence codes. The bootstrap rep .rds only saves the parameter_table,
## so to inspect the underlying r$accepted / r$polish_*_value / r$pso_value
## fields that gate the convergence test we replay just the wage stage and
## dump everything.
##
## Output: smoke_iter26_wage_diagnostic.rds
suppressPackageStartupMessages(library(data.table))
source("config.R")
source("preamble.R")

CONFIG$bootstrap_combine_only <- FALSE
Sys.setenv(SLURM_ARRAY_TASK_ID = "26")
CONFIG$slurm_array_task_id <- 26L

source("legacy/07_bootstrap.R")
EOF

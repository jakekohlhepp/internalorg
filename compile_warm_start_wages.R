## compile_warm_start_wages.R
##
## Persist the best-known cleared wage vectors per (county, quarter_year) so
## 13_counterfactual_prep.R can warm-start its labor-clearing solver from
## somewhere already close to a market-clearing equilibrium. Re-run this
## script whenever a better-clearing wage vector is found.

source("config.R")
source("utils/counterfactuals_core.R")
suppressPackageStartupMessages(library(data.table))

ensure_counterfactual_dirs()

warm <- data.table(
  county       = c("17031", "36061", "6037"),
  quarter_year = c(2021.2,  2021.2,  2021.2),
  ## 17031: BBsolve converged, |residual|_max = 1.402e-09.
  w1 = c(40.293,  81.9985,  61.085),
  w2 = c(114.810, 65.7976,  79.494),
  w3 = c(109.010, 319.5385, 78.719),
  w4 = c(52.413,  159.225,  54.673),
  w5 = c(93.154,  211.708,  315.928),
  ## 36061: PSO big new best |residual|_max = 0.0976189 (job timed out mid-CRS2_LM).
  ## 6037:  BBsolve converged, |residual|_max = 1.826e-04.
  source = c(
    "BBsolve, |r|_max=1.402e-09",
    "PSO big, |r|_max=0.0976189",
    "BBsolve, |r|_max=1.826e-04"
  )
)
setkey(warm, county, quarter_year)

save_counterfactual_rds(
  warm,
  "13_warm_start_wages.rds",
  legacy_filename = NULL
)

cat("Wrote 13_warm_start_wages.rds:\n")
print(warm)

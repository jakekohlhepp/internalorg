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
  ## 36061: PSO big new best |residual|_max = 0.0976189 (job timed out mid-CRS2_LM).
  ## 6037:  13_ run (job 52395966) — coord_descent_sweep8_i5_uniroot_hw0.015
  ##        in the production full-5D fallback ladder, warm-started from the
  ##        prior v5 coord_descent_extended point. |residual|_max = 0.01839859.
  ##        Verified live against the current compute_counterfactual_total_labor
  ##        / market_parms.
  w1 = c(40.293,  81.9985,  77.02585826265),
  w2 = c(114.810, 65.7976,  92.9773102427984),
  w3 = c(109.010, 319.5385, 113.733599763972),
  w4 = c(52.413,  159.225,  67.9728633798002),
  w5 = c(93.154,  211.708,  350.959461733367),
  source = c(
    "BBsolve, |r|_max=1.402e-09",
    "PSO big, |r|_max=0.0976189",
    "13_ coord_descent_sweep8, |r|_max=0.01839859"
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

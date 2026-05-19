## compile_warm_start_wages.R
##
## Persist the best-known cleared wage vectors per (county, quarter_year) so
## 13_counterfactual_prep.R can warm-start its labor-clearing solver from
## somewhere already close to a market-clearing equilibrium. Re-run this
## script whenever a smoke produces a better-clearing wage vector.
##
## Provenance of each row is captured in the `source` column; see the inline
## comments for the smoke-log line numbers that yielded each vector.

source("config.R")
source("utils/counterfactuals_core.R")
suppressPackageStartupMessages(library(data.table))

ensure_counterfactual_dirs()

warm <- data.table(
  county       = c("17031", "36061", "6037"),
  quarter_year = c(2021.2,  2021.2,  2021.2),
  ## 17031: smoke_13_with_model_exo (smoke13_model_exo_51482391.out:8) — BBsolve
  ## converged with |residual|_max = 1.402e-09.
  w1 = c(40.293,  81.9985,  61.085),
  w2 = c(114.810, 65.7976,  79.494),
  w3 = c(109.010, 319.5385, 78.719),
  w4 = c(52.413,  159.225,  54.673),
  w5 = c(93.154,  211.708,  315.928),
  ## 36061: smoke_13_warmstart_nyc_extra (smoke13_nyc_extra_51507154.out:168)
  ##   PSO_big new best |residual|_max = 0.0976189 (job timed out mid-CRS2_LM).
  ## 6037:  smoke_13_warmstart_county for LA (smoke13_warm_LA_51495953.out:7)
  ##   BBsolve converged, |residual|_max = 1.826e-04.
  source = c(
    "smoke13_model_exo_51482391 (line 8)",
    "smoke13_nyc_extra_51507154 (line 168, PSO big)",
    "smoke13_warm_LA_51495953 (line 7)"
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

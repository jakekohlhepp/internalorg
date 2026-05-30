## SMOKE: 1-D slice of the LA reorg labor-clearing residual on log(w_3),
## with w_1, w_2, w_4, w_5 pinned at the canonical reorg anchor.
##
## Diagnoses whether the worker-3 residual floor (~0.016-0.029) under
## diffusion / merger / immigration is a smooth local minimum (system has no
## solution) or a kink (assignment discontinuity).
##
## For each scenario (diffusion = 14, merger = 17), this script:
##   1. Loads the LA reorg anchor wages from the canonical wage file.
##   2. Applies the scenario shock to LA's working_data:
##        diffusion: gamma_invert := improve_it(gamma_invert)
##        merger:    weight := weight / 2
##   3. Sweeps log(w_3) on a fine grid around log(anchor_w3) and records the
##      full 5-d clearing residual at each grid point.
##   4. Adds a "tight" sweep with innertol=1e-12 and outertol=1e-8 around the
##      minimum to test sensitivity to inner-solver precision.
##
## Output:
##   smoke_la_reorg_w3_slice.rds  (gitignored)
suppressPackageStartupMessages({
  library("data.table")
})

source("config.R")
source("utils/counterfactuals_core.R")

n_w <- CONFIG$n_worker_types
n_t <- CONFIG$n_task_types
task_mix_cols   <- get_task_mix_cols(CONFIG)
e_field_names   <- counterfactual_e_field_names(CONFIG)
tot_field_names <- counterfactual_tot_labor_field_names(CONFIG)

LA <- "6037"
QY <- 2021.2

market_input_cols <- c(
  "location_id", "county", "quarter_year", "gamma_invert", "avg_labor",
  task_mix_cols, "qual_exo", "cost_exo", "weight", "cust_price", "CSPOP"
)

improve_it <- function(x) {
  rankhold <- frank(x, ties.method = "first")
  sort(x)[pmax(rankhold - 1, 1)]
}

read_anchor <- function(path, stype) {
  if (!file.exists(path)) stop("missing wages file: ", path)
  tab <- as.data.table(readRDS(path))
  row <- tab[county == LA & quarter_year == QY & sol_type == stype]
  stopifnot(nrow(row) == 1)
  as.numeric(unlist(row[, paste0("w", seq_len(n_w)), with = FALSE]))
}

scenarios <- list(
  diffusion = list(
    wages_path = counterfactual_data_path("14_wages_diffusion.rds", CONFIG),
    apply_shock = function(wd) {
      wd[county == LA & quarter_year == QY,
         gamma_invert := improve_it(gamma_invert)]
      wd
    }
  ),
  merger = list(
    wages_path = counterfactual_data_path("17_wages_merger.rds", CONFIG),
    apply_shock = function(wd) {
      wd[county == LA & quarter_year == QY, weight := weight / 2]
      wd
    }
  )
)

## ------------------------------------------------------------------ context
## Reload counterfactual context fresh for each scenario so the shocks don't
## stack on top of each other.
build_solver <- function(scenario, innertol_override = NULL, outertol_override = NULL) {
  ctx <- load_counterfactual_context()
  wd  <- copy(ctx$working_data)
  wd  <- scenario$apply_shock(wd)
  total_labor <- copy(ctx$total_labor)
  market_parms <- ctx$market_parms
  rho <- ctx$rho

  innertol <- if (is.null(innertol_override)) CONFIG$counterfactual_innertol else innertol_override
  outertol <- if (is.null(outertol_override)) CONFIG$counterfactual_outertol else outertol_override

  la_data_template <- copy(wd[county == LA & quarter_year == QY, ..market_input_cols])

  build_mats <- function(wage_guess) {
    new_theta <- matrix(
      market_parms[grep(paste0(LA, ":avg_labor:B"), names(market_parms))],
      ncol = n_t, nrow = n_w, byrow = FALSE
    )
    w_mat <- matrix(wage_guess, ncol = n_t, nrow = n_w, byrow = FALSE)
    new_tild_theta <- w_mat + (rho[LA])^(-1) * new_theta
    new_tild_theta <- sweep(new_tild_theta, 2, apply(new_tild_theta, 2, min))
    list(new_theta = new_theta, new_tild_theta = new_tild_theta)
  }

  function(wage_guess) {
    cr <- copy(la_data_template)
    mats <- build_mats(wage_guess)
    out_fields <- c("c_endog", "q_endog", e_field_names)
    cr[, (out_fields) := counterfactual_org_outputs(
        cost_matrix  = mats$new_tild_theta,
        alpha        = as.numeric(.SD),
        gamma        = gamma_invert,
        wage_guess   = wage_guess,
        new_theta    = mats$new_theta,
        innertol     = innertol,
        config       = CONFIG
      )[c("c_endog", "q_endog", e_field_names)],
      by = c("location_id"),
      .SDcols = task_mix_cols
    ]
    cr[, Q := q_endog * avg_labor + qual_exo]
    cr[, C := pmax(c_endog * avg_labor + cost_exo, 0)]
    cr[, newprice := counterfactual_best_response_prices(
      cust_price, Q, C, weight, rho[LA], outertol, paste(LA, QY)
    )]
    cr[, new_share := counterfactual_logit_shares(Q, newprice, weight, rho[LA])]
    new_total_labor <- cr[, setNames(
      lapply(seq_len(n_w), function(idx) {
        sum(weight * new_share * CSPOP * get(e_field_names[idx]) * avg_labor)
      }),
      tot_field_names
    )]
    stopifnot(nrow(new_total_labor) == 1)
    counterfactual_labor_gap(new_total_labor, total_labor, LA, QY)
  }
}

## ------------------------------------------------------------------ slice
slice_one <- function(label, scn, default_grid, tight_grid) {
  cat("============================================================\n")
  cat("Scenario: ", label, "\n", sep = "")
  anchor <- read_anchor(scn$wages_path, "reorg")
  cat("Anchor wages: ", paste(sprintf("%.4f", anchor), collapse = ", "), "\n", sep = "")
  log_w3_anchor <- log(anchor[3])

  ## default-tol sweep
  solver_def <- build_solver(scn)
  cat("Default-tol sweep (", length(default_grid), " points)\n", sep = "")
  t0 <- Sys.time()
  res_def <- vector("list", length(default_grid))
  for (i in seq_along(default_grid)) {
    w <- anchor
    w[3] <- exp(log_w3_anchor + default_grid[i])
    r <- tryCatch(solver_def(w), error = function(e) rep(NA_real_, n_w))
    res_def[[i]] <- list(d_logw3 = default_grid[i], w3 = w[3], r = r,
                         ssq = if (all(is.finite(r))) sum(r^2) else NA_real_,
                         max_abs = if (all(is.finite(r))) max(abs(r)) else NA_real_)
  }
  cat("  done in ", round(as.numeric(difftime(Sys.time(), t0, units = "secs"))), "s\n", sep = "")

  ## tight-tol sweep near min
  solver_tight <- build_solver(scn,
                               innertol_override = 1e-12,
                               outertol_override = 1e-8)
  cat("Tight-tol sweep (", length(tight_grid), " points, innertol=1e-12, outertol=1e-8)\n", sep = "")
  t0 <- Sys.time()
  res_tight <- vector("list", length(tight_grid))
  for (i in seq_along(tight_grid)) {
    w <- anchor
    w[3] <- exp(log_w3_anchor + tight_grid[i])
    r <- tryCatch(solver_tight(w), error = function(e) rep(NA_real_, n_w))
    res_tight[[i]] <- list(d_logw3 = tight_grid[i], w3 = w[3], r = r,
                           ssq = if (all(is.finite(r))) sum(r^2) else NA_real_,
                           max_abs = if (all(is.finite(r))) max(abs(r)) else NA_real_)
  }
  cat("  done in ", round(as.numeric(difftime(Sys.time(), t0, units = "secs"))), "s\n", sep = "")

  to_dt <- function(lst) {
    rbindlist(lapply(lst, function(x) {
      data.table(
        d_logw3 = x$d_logw3, w3 = x$w3,
        r1 = x$r[1], r2 = x$r[2], r3 = x$r[3], r4 = x$r[4], r5 = x$r[5],
        ssq = x$ssq, max_abs = x$max_abs
      )
    }))
  }

  list(anchor = anchor,
       default = to_dt(res_def),
       tight   = to_dt(res_tight))
}

## ------------------------------------------------------------------ grids
## Default-tol wide sweep: log-deviation from anchor on log(w_3) from -1.5 to +1.5
##   (i.e. w_3 in ~22% to ~450% of anchor) at 121 points.
default_grid <- seq(-1.5, 1.5, length.out = 121)

## Tight-tol fine sweep at ±0.05 in log around anchor.
tight_grid   <- seq(-0.05, 0.05, length.out = 41)

out <- list()
for (label in names(scenarios)) {
  out[[label]] <- slice_one(label, scenarios[[label]], default_grid, tight_grid)
}

## ------------------------------------------------------------------ summary
for (label in names(out)) {
  cat("\n=== ", label, " summary ===\n", sep = "")
  d <- out[[label]]$default[order(ssq)]
  cat("Default-tol top 5 by SSQ:\n")
  print(head(d[, .(d_logw3, w3, r1, r2, r3, r4, r5, ssq, max_abs)], 5))

  t <- out[[label]]$tight[order(ssq)]
  cat("Tight-tol top 5 by SSQ:\n")
  print(head(t[, .(d_logw3, w3, r1, r2, r3, r4, r5, ssq, max_abs)], 5))

  cat("Default min(max_abs) = ", min(d$max_abs, na.rm = TRUE), "\n", sep = "")
  cat("Tight   min(max_abs) = ", min(t$max_abs, na.rm = TRUE), "\n", sep = "")
}

saveRDS(out, "smoke_la_reorg_w3_slice.rds")
cat("\nSaved: smoke_la_reorg_w3_slice.rds\n")

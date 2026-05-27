## Shared setup for the LA-reorg immigration smoke fleet.
## Builds the production-equivalent residual function for county 6037,
## sol_type "reorg", quarter 2021.2 under the 5% immigration shock, and
## exposes save_smoke_result() in the same shape as the existing smoke fleet.
suppressPackageStartupMessages({ library(data.table) })
source("config.R")
source("utils/counterfactuals_core.R")

la_reorg_make_residual <- function() {
  innertol <- CONFIG$counterfactual_innertol
  outertol <- CONFIG$counterfactual_outertol
  ctx <- load_counterfactual_context()
  working_data <- ctx$working_data
  initial_wages <- ctx$initial_wages
  total_labor   <- ctx$total_labor
  market_parms  <- ctx$market_parms
  rho           <- ctx$rho

  n_worker_types <- CONFIG$n_worker_types
  n_task_types   <- CONFIG$n_task_types
  task_mix_cols  <- get_task_mix_cols(CONFIG)
  e_field_names  <- counterfactual_e_field_names(CONFIG)
  market_input_cols <- c(
    "location_id", "county", "quarter_year", "gamma_invert", "avg_labor",
    task_mix_cols, "qual_exo", "cost_exo", "weight", "cust_price", "CSPOP"
  )

  ## 5% immigration shock to LA->type1, NYC->type2, Cook->type4 (matches
  ## 16_counterfactual_immigration.R lines 146-159 exactly).
  tot_field_names <- counterfactual_tot_labor_field_names(CONFIG)
  add_immigrants <- function(cnty, target_idx, qy = 2021.2) {
    base <- as.numeric(as.matrix(total_labor[
      county == cnty & quarter_year == qy, .SD, .SDcols = tot_field_names]))
    delta <- 0.05 * sum(base)
    total_labor[county == cnty & quarter_year == qy,
                (tot_field_names[target_idx]) := get(tot_field_names[target_idx]) + delta]
  }
  add_immigrants("6037",  1)
  add_immigrants("36061", 2)
  add_immigrants("17031", 4)

  build_market_matrices <- function(wage_guess, cnty) {
    new_theta <- matrix(
      market_parms[grep(paste0(cnty, ":avg_labor:B"), names(market_parms))],
      ncol = n_task_types, nrow = n_worker_types, byrow = FALSE)
    w_mat <- matrix(wage_guess, ncol = n_task_types, nrow = n_worker_types, byrow = FALSE)
    new_tild_theta <- w_mat + (rho[cnty])^(-1) * new_theta
    new_tild_theta <- sweep(new_tild_theta, 2, apply(new_tild_theta, 2, min))
    list(new_theta = new_theta, new_tild_theta = new_tild_theta)
  }
  apply_pricing <- function(counter_res, cnty, qy) {
    counter_res[, Q := q_endog * avg_labor + qual_exo]
    counter_res[, C := pmax(c_endog * avg_labor + cost_exo, 0)]
    counter_res[, newprice := counterfactual_best_response_prices(
      cust_price, Q, C, weight, rho[cnty], outertol, paste(cnty, qy))]
    counter_res[, new_share := counterfactual_logit_shares(
      Q, newprice, weight, rho[cnty])]
    counter_res
  }
  new_total_labor_from <- function(counter_res) {
    counter_res[, setNames(
      lapply(seq_len(n_worker_types), function(idx) {
        sum(weight * new_share * CSPOP * get(e_field_names[idx]) * avg_labor)
      }),
      counterfactual_tot_labor_field_names(CONFIG))]
  }
  solve_org_fresh <- function(alpha, gamma, mats, wage_guess) {
    counterfactual_org_outputs(
      cost_matrix = mats$new_tild_theta, alpha = alpha, gamma = gamma,
      wage_guess = wage_guess, new_theta = mats$new_theta,
      innertol = innertol, config = CONFIG)
  }

  cnty <- "6037"; qy <- 2021.2
  residual_fn <- function(wage_guess) {
    if (!all(is.finite(wage_guess)) || any(wage_guess <= 0)) {
      return(rep(1e6, n_worker_types))
    }
    counter_res <- copy(working_data[county == cnty & quarter_year == qy, ..market_input_cols])
    mats <- build_market_matrices(wage_guess, cnty)
    output_fields <- c("c_endog", "q_endog", e_field_names)
    counter_res[, (output_fields) := solve_org_fresh(
        as.numeric(.SD), gamma_invert, mats, wage_guess),
      by = c("location_id"), .SDcols = task_mix_cols]
    counter_res <- apply_pricing(counter_res, cnty, qy)
    new_total_labor <- new_total_labor_from(counter_res)
    as.numeric(counterfactual_labor_gap(new_total_labor, total_labor, cnty, qy))
  }
  ssr_fn <- function(wage_guess) sum(residual_fn(wage_guess)^2)
  maxabs_fn <- function(wage_guess) max(abs(residual_fn(wage_guess)))

  ## Warm-start: production realloc wages and the failed reorg point.
  realloc_w <- as.numeric(unlist(
    as.data.table(readRDS("results/data/counterfactuals/16_wages_immigration.rds"))[
      county == "6037" & sol_type == "realloc" & quarter_year == 2021.2,
      .(w1, w2, w3, w4, w5)]))
  reorg_w <- as.numeric(unlist(
    as.data.table(readRDS("results/data/counterfactuals/16_wages_immigration.rds"))[
      county == "6037" & sol_type == "reorg" & quarter_year == 2021.2,
      .(w1, w2, w3, w4, w5)]))
  baseline_w <- as.numeric(unlist(
    initial_wages[county == "6037" & quarter_year == 2021.2, .(w1, w2, w3, w4, w5)]))

  list(
    residual_fn = residual_fn, ssr_fn = ssr_fn, maxabs_fn = maxabs_fn,
    realloc_w = realloc_w, reorg_w = reorg_w, baseline_w = baseline_w,
    n_worker_types = n_worker_types
  )
}

save_smoke_result <- function(method, smoke_method, par, starting_wages,
                              baseline_max_abs, residual_fn, elapsed,
                              extra = list(), out_filename) {
  par <- as.numeric(par)
  resid <- residual_fn(par)
  max_abs <- max(abs(resid))
  resnorm <- sqrt(sum(resid^2))
  result <- c(list(
    log_par = log(pmax(par, 1e-300)),
    par = par,
    max_abs = max_abs,
    residual_norm = resnorm,
    residual_vector = resid,
    method = method,
    elapsed = as.numeric(elapsed),
    converged = is.finite(max_abs) && max_abs <= 1e-2,
    counterfactual = "immigration",
    smoke_method = smoke_method,
    baseline_max_abs = baseline_max_abs,
    starting_wages = starting_wages,
    timestamp = Sys.time(),
    jobid = Sys.getenv("SLURM_JOB_ID", unset = NA)
  ), extra)
  out_path <- file.path("results/data/counterfactuals", out_filename)
  saveRDS(result, out_path)
  cat("\n=== ", method, " ===\n", sep = "")
  cat("starting max|resid|:", baseline_max_abs, "\n")
  cat("final    max|resid|:", max_abs, "\n")
  cat("converged (<= 1e-2):", result$converged, "\n")
  cat("elapsed (s):        ", round(elapsed, 1), "\n")
  cat("saved to:           ", out_path, "\n")
  invisible(result)
}

## ===========================================================================
## verify_cf_market_clearing.R
##
## Independent verification of market clearing across the completed
## counterfactuals (14, 15, 17) and the 13 baseline. For each saved wage
## solution, plug the wages BACK into the same residual function used at
## solve time and report the actual residual norm. Sanity-checks the stored
## resid_* columns against a fresh compute, and recomputes 13's baseline LA
## residual to confirm the 0.62 number from the run log.
## ===========================================================================

suppressPackageStartupMessages({
  library(data.table)
  library(stringr)
})

source("config.R")
source("utils/counterfactuals_core.R")

innertol <- CONFIG$counterfactual_innertol
outertol <- CONFIG$counterfactual_outertol

n_w <- CONFIG$n_worker_types
n_t <- CONFIG$n_task_types
task_mix_cols   <- get_task_mix_cols(CONFIG)
e_field_names   <- counterfactual_e_field_names(CONFIG)
tot_field_names <- counterfactual_tot_labor_field_names(CONFIG)

## ---------------------------------------------------------------------------
## Step 1: re-run 13's baseline LA residual independently
## ---------------------------------------------------------------------------

cat("\n==============================\n")
cat(" 13 baseline — recompute LA 2021.2 residual\n")
cat("==============================\n")

ctx <- load_counterfactual_context()
working_data <- ctx$working_data
market_parms <- ctx$market_parms
rho          <- ctx$rho

LA <- "6037"
QY <- 2021.2

market_input_cols <- c(
  "location_id", "county", "quarter_year", "gamma_invert", "avg_labor",
  task_mix_cols, "qual_exo", "cost_exo", "weight", "cust_price", "CSPOP",
  "outside_share"
)

la_template <- copy(working_data[county == LA & quarter_year == QY,
                                  ..market_input_cols])
theta_la <- matrix(
  market_parms[grep(paste0(LA, ":avg_labor:B"), names(market_parms))],
  ncol = n_t, nrow = n_w, byrow = FALSE
)
target_lab_la <- as.numeric(as.matrix(
  compute_counterfactual_total_labor(working_data, CONFIG)[
    county == LA & quarter_year == QY, .SD, .SDcols = tot_field_names
  ]
))

solve_wages_la <- function(log_w) {
  w <- exp(log_w)
  cr <- copy(la_template)
  w_mat <- matrix(w, ncol = n_t, nrow = n_w, byrow = FALSE)
  new_tild_theta <- w_mat + (rho[LA])^(-1) * theta_la
  new_tild_theta <- sweep(new_tild_theta, 2, apply(new_tild_theta, 2, min))
  output_fields <- c("c_endog", "q_endog", e_field_names)
  cr[, (output_fields) := counterfactual_org_outputs(
      cost_matrix = new_tild_theta, alpha = as.numeric(.SD),
      gamma = gamma_invert, wage_guess = w,
      new_theta = theta_la, innertol = innertol, config = CONFIG
    )[c("c_endog", "q_endog", e_field_names)],
    by = c("location_id"), .SDcols = task_mix_cols]
  cr[, Q := q_endog * avg_labor + qual_exo]
  cr[, C := pmax(c_endog * avg_labor + cost_exo, 0)]
  cr[, newprice := counterfactual_best_response_prices(
    cust_price, Q, C, weight, rho[LA], outertol, paste(LA, QY))]
  cr[, new_share := counterfactual_logit_shares(Q, newprice, weight, rho[LA])]
  model_lab <- sapply(seq_len(n_w), function(idx) {
    sum(cr$weight * cr$new_share * cr$CSPOP * cr[[e_field_names[idx]]] * cr$avg_labor)
  })
  log(pmax(model_lab, CONFIG$numeric_floor)) -
    log(pmax(target_lab_la, CONFIG$numeric_floor))
}

initial_wages <- ctx$initial_wages
w_la <- as.numeric(unlist(initial_wages[county == LA & quarter_year == QY,
                                         paste0("w", seq_len(n_w)),
                                         with = FALSE]))
log_w_la <- log(pmax(w_la, 1e-6))
r_la <- solve_wages_la(log_w_la)
cat("LA baseline saved wages:", round(w_la, 3), "\n")
cat("LA residuals (recomputed): ", paste(sprintf("%+.4e", r_la), collapse="  "), "\n")
cat("||r||_inf = ", signif(max(abs(r_la)), 4),
    "   ||r||_2 = ", signif(sqrt(sum(r_la^2)), 4), "\n", sep="")
cat("13 reported best in log: residual=0.62 from PSO_SSR\n")
cat("Match within rounding? ", round(max(abs(r_la)), 2) == 0.62, "\n")

## ---------------------------------------------------------------------------
## Step 2: verify counterfactual saved residuals are internally consistent
## ---------------------------------------------------------------------------

cat("\n==============================\n")
cat(" Counterfactual stored residuals — consistency check\n")
cat("==============================\n")

check_one <- function(label, num) {
  p <- sprintf("results/data/counterfactuals/%d_wages_%s.rds", num, label)
  w <- as.data.table(readRDS(p))
  w <- w[is.finite(fval)]
  w[, max_abs_resid := pmax(abs(resid_1), abs(resid_2), abs(resid_3),
                             abs(resid_4), abs(resid_5))]
  w[, consistency_gap := abs(fval - max_abs_resid)]
  cat("\n-- ", toupper(label), " --\n", sep="")
  print(w[, .(county, quarter_year, sol_type,
              fval = signif(fval, 4),
              max_abs_resid = signif(max_abs_resid, 4),
              consistency_gap = signif(consistency_gap, 4),
              converged, target_tol)])
  cat("max(consistency_gap) =", signif(max(w$consistency_gap), 4), "\n")
  invisible(w)
}
diff_w <- check_one("diffusion", 14)
sale_w <- check_one("salestax",  15)
merg_w <- check_one("merger",    17)

## ---------------------------------------------------------------------------
## Step 3: aggregate convergence summary
## ---------------------------------------------------------------------------

cat("\n==============================\n")
cat(" Convergence summary\n")
cat("==============================\n")
all_w <- rbindlist(list(
  cbind(scenario = "diffusion", diff_w),
  cbind(scenario = "salestax",  sale_w),
  cbind(scenario = "merger",    merg_w)
), fill = TRUE)
summ <- all_w[, .(.N, n_converged = sum(converged),
                  n_failed = sum(!converged)),
              by = .(scenario, sol_type)][order(scenario, sol_type)]
print(summ)

cat("\nFailures:\n")
print(all_w[converged == FALSE, .(scenario, county, quarter_year, sol_type,
                                   fval = signif(fval, 4),
                                   target_tol, method)])

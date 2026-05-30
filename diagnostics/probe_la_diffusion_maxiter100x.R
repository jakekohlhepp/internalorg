## SMOKE: re-solve the LA reorg assignment under the diffusion shock at the
## canonical 14_ reorg anchor wages, with fixedpoint_max_iter bumped 100x
## (100000 -> 10000000) and innertol = 1e-12. Tests whether the two near-corner
## firms (gamma ~ 0.898) actually converge with a much larger iteration budget,
## and what the resulting 5-d labor-clearing residual is.
##
## Output:
##   smoke_la_diffusion_maxiter100x.rds (gitignored)
suppressPackageStartupMessages({
  library("data.table")
  library("SQUAREM")
})

source("config.R")
source("utils/counterfactuals_core.R")

MAXITER_100X <- 100L * CONFIG$fixedpoint_max_iter   # 100000 -> 10,000,000
INNERTOL     <- 1e-12

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

ctx <- load_counterfactual_context()
wd  <- copy(ctx$working_data)
wd[county == LA & quarter_year == QY,
   gamma_invert := improve_it(gamma_invert)]
market_parms <- ctx$market_parms
rho <- ctx$rho
total_labor <- copy(ctx$total_labor)

la_data <- copy(wd[county == LA & quarter_year == QY, ..market_input_cols])

path <- counterfactual_data_path("14_wages_diffusion.rds", CONFIG)
tab  <- as.data.table(readRDS(path))
anchor <- as.numeric(unlist(
  tab[county == LA & quarter_year == QY & sol_type == "reorg",
      paste0("w", seq_len(n_w)), with = FALSE]
))
cat("Diffusion reorg anchor (w_1..w_5): ",
    paste(sprintf("%.4f", anchor), collapse = ", "), "\n", sep = "")
cat("fixedpoint_max_iter = ", MAXITER_100X, " (100x), innertol = ", INNERTOL, "\n", sep = "")

new_theta <- matrix(
  market_parms[grep(paste0(LA, ":avg_labor:B"), names(market_parms))],
  ncol = n_t, nrow = n_w, byrow = FALSE
)
w_mat <- matrix(anchor, ncol = n_t, nrow = n_w, byrow = FALSE)
new_tild_theta <- w_mat + (rho[LA])^(-1) * new_theta
new_tild_theta <- sweep(new_tild_theta, 2, apply(new_tild_theta, 2, min))

## per-firm assignment, instrumented, with the 100x maxiter budget.
probe_assignment <- function(cost_matrix, alpha, gamma, tol, maxiter) {
  gamma_eff <- counterfactual_effective_gamma(gamma, CONFIG)
  if (!(is.finite(gamma_eff) && gamma_eff > 0)) {
    return(list(B = NULL, E = NULL, c_endog = NA, q_endog = NA,
                iter = NA_integer_, fpevals = NA_integer_,
                convergence = NA_integer_, regime = "corner"))
  }
  A <- exp(-cost_matrix / gamma_eff)
  A[A >= Inf] <- CONFIG$numeric_ceiling
  A[A <= 0]   <- CONFIG$numeric_floor
  fxpt <- function(p) {
    C <- colSums(t(A) * alpha / colSums(A * p))
    p * C
  }
  res <- SQUAREM::squarem(
    rep(1 / n_w, n_w),
    fixptfn = fxpt,
    control = list(maxiter = maxiter, tol = tol)
  )
  E <- res$par
  B <- t(t(A) * alpha / colSums(A * E)) * E
  B[abs(B) < CONFIG$B_zero_threshold] <- 0
  list(B = B, E = rowSums(B),
       iter = res$iter, fpevals = res$fpevals,
       convergence = as.integer(res$convergence), regime = "interior")
}

cat("Solving ", nrow(la_data), " LA firms at 100x maxiter...\n", sep = "")
recs <- list()
E_mat <- matrix(NA_real_, nrow = nrow(la_data), ncol = n_w)
for (i in seq_len(nrow(la_data))) {
  alpha <- as.numeric(la_data[i, ..task_mix_cols])
  gamma <- la_data$gamma_invert[i]
  t0 <- Sys.time()
  r  <- probe_assignment(new_tild_theta, alpha, gamma, INNERTOL, MAXITER_100X)
  el <- as.numeric(difftime(Sys.time(), t0, units = "secs"))
  if (r$regime == "interior") {
    E_mat[i, ] <- r$E
    ## c_endog / q_endog from B and new_theta (mirror counterfactual_org_outputs)
    s_cost <- sum(r$B * new_tild_theta)
    q_val  <- sum(r$B * new_theta)
  } else {
    s_cost <- NA_real_; q_val <- NA_real_
  }
  recs[[i]] <- data.table(
    location_id = la_data$location_id[i],
    gamma = gamma,
    iter = r$iter, fpevals = r$fpevals,
    convergence = r$convergence,
    elapsed_sec = el
  )
}
rec_dt <- rbindlist(recs)

cat("\n=== Per-firm convergence at 100x maxiter (innertol=1e-12) ===\n")
cat("convergence != 0 (FAILED) count: ",
    sum(rec_dt$convergence != 0, na.rm = TRUE), " / ",
    sum(!is.na(rec_dt$convergence)), "\n", sep = "")
cat("Slowest 6 firms:\n")
print(rec_dt[order(-iter)][1:6])
cat("Total assignment wall time: ", round(sum(rec_dt$elapsed_sec)), "s\n", sep = "")

## --- now compute the labor-clearing residual at the anchor using these E ---
## Use the package solver but force the 100x maxiter via a temporary config.
CFG2 <- CONFIG
CFG2$fixedpoint_max_iter <- MAXITER_100X
out_fields <- c("c_endog", "q_endog", e_field_names)
la_solve <- copy(la_data)
la_solve[, (out_fields) := counterfactual_org_outputs(
    cost_matrix  = new_tild_theta,
    alpha        = as.numeric(.SD),
    gamma        = gamma_invert,
    wage_guess   = anchor,
    new_theta    = new_theta,
    innertol     = INNERTOL,
    config       = CFG2
  )[c("c_endog", "q_endog", e_field_names)],
  by = c("location_id"),
  .SDcols = task_mix_cols
]
la_solve[, Q := q_endog * avg_labor + qual_exo]
la_solve[, C := pmax(c_endog * avg_labor + cost_exo, 0)]
la_solve[, newprice := counterfactual_best_response_prices(
  cust_price, Q, C, weight, rho[LA], 1e-8, paste(LA, QY))]
la_solve[, new_share := counterfactual_logit_shares(Q, newprice, weight, rho[LA])]
new_total_labor <- la_solve[, setNames(
  lapply(seq_len(n_w), function(idx) {
    sum(weight * new_share * CSPOP * get(e_field_names[idx]) * avg_labor)
  }),
  tot_field_names
)]
resid <- counterfactual_labor_gap(new_total_labor, total_labor, LA, QY)

cat("\n=== Labor-clearing residual at anchor, 100x maxiter + innertol=1e-12 ===\n")
for (k in seq_len(n_w)) {
  cat(sprintf("  worker %d: resid = %-14.8g\n", k, resid[k]))
}
cat(sprintf("max|resid| = %.6g   SSQ = %.6g   (outer tol = %.4g)\n",
            max(abs(resid)), sum(resid^2), CONFIG$counterfactual_wage_tol))
cat("\nFor reference:\n")
cat("  default tol (1e-8 / 1e-4)         : max|resid| = 0.02943, worker 3 binds\n")
cat("  tight tol, default maxiter (1e-12): max|resid| = 0.30867, worker 5 binds\n")

saveRDS(list(rec_dt = rec_dt, resid = resid, anchor = anchor,
             maxiter = MAXITER_100X, innertol = INNERTOL),
        "smoke_la_diffusion_maxiter100x.rds")
cat("\nSaved: smoke_la_diffusion_maxiter100x.rds\n")

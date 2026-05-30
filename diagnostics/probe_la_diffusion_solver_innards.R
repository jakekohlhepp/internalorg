## SMOKE: dissect the inner SQUAREM assignment loop and the outer price loop
## for the LA reorg solve under the diffusion counterfactual, evaluated at the
## canonical 14_ reorg anchor wages. Compare default (innertol=1e-8 /
## outertol=1e-4) vs tight (innertol=1e-12 / outertol=1e-8).
##
## Reports per-firm:
##   - SQUAREM iter count + fpevals + convergence flag at both tols
##   - max-abs change in E between default and tight final E
##   - max-abs change in B between default and tight final B
## Reports for the price loop (one call covering all LA firms simultaneously):
##   - iteration count, converged flag, max abs |p_new - p_old| at exit
##
## Output:
##   smoke_la_diffusion_solver_innards.rds (gitignored)
suppressPackageStartupMessages({
  library("data.table")
  library("SQUAREM")
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

ctx <- load_counterfactual_context()
wd  <- copy(ctx$working_data)
wd[county == LA & quarter_year == QY,
   gamma_invert := improve_it(gamma_invert)]

market_parms <- ctx$market_parms
rho <- ctx$rho

la_data <- copy(wd[county == LA & quarter_year == QY, ..market_input_cols])

## ---------------------------------------------------------------- anchor
path <- counterfactual_data_path("14_wages_diffusion.rds", CONFIG)
tab  <- as.data.table(readRDS(path))
anchor <- as.numeric(unlist(
  tab[county == LA & quarter_year == QY & sol_type == "reorg",
      paste0("w", seq_len(n_w)), with = FALSE]
))
cat("Diffusion reorg anchor (w_1..w_5): ",
    paste(sprintf("%.4f", anchor), collapse = ", "), "\n", sep = "")

## ---------------------------------------------------------------- new_theta + cost mat
new_theta <- matrix(
  market_parms[grep(paste0(LA, ":avg_labor:B"), names(market_parms))],
  ncol = n_t, nrow = n_w, byrow = FALSE
)
w_mat <- matrix(anchor, ncol = n_t, nrow = n_w, byrow = FALSE)
new_tild_theta <- w_mat + (rho[LA])^(-1) * new_theta
new_tild_theta <- sweep(new_tild_theta, 2, apply(new_tild_theta, 2, min))

## ---------------------------------------------------------------- per-firm squarem
probe_assignment <- function(cost_matrix, alpha, gamma, tol) {
  gamma_eff <- counterfactual_effective_gamma(gamma, CONFIG)
  if (!(is.finite(gamma_eff) && gamma_eff > 0)) {
    return(list(B = NA, E = NA, iter = NA_integer_, fpevals = NA_integer_,
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
    control = list(maxiter = CONFIG$fixedpoint_max_iter, tol = tol)
  )
  E <- res$par
  B <- t(t(A) * alpha / colSums(A * E)) * E
  B[abs(B) < CONFIG$B_zero_threshold] <- 0
  list(B = B, E = rowSums(B), iter = res$iter, fpevals = res$fpevals,
       convergence = as.integer(res$convergence), regime = "interior")
}

cat("Probing per-firm assignment for ", nrow(la_data), " LA firms...\n", sep = "")
firm_res <- list()
for (i in seq_len(nrow(la_data))) {
  alpha <- as.numeric(la_data[i, ..task_mix_cols])
  gamma <- la_data$gamma_invert[i]
  def_  <- probe_assignment(new_tild_theta, alpha, gamma, 1e-8)
  tight <- probe_assignment(new_tild_theta, alpha, gamma, 1e-12)
  if (def_$regime == "interior" && tight$regime == "interior") {
    dE <- max(abs(def_$E - tight$E))
    dB <- max(abs(def_$B - tight$B))
  } else {
    dE <- NA_real_; dB <- NA_real_
  }
  firm_res[[i]] <- data.table(
    location_id = la_data$location_id[i],
    gamma = gamma,
    regime_default = def_$regime,
    regime_tight   = tight$regime,
    iter_default   = def_$iter,
    iter_tight     = tight$iter,
    fpevals_default = def_$fpevals,
    fpevals_tight   = tight$fpevals,
    conv_default   = def_$convergence,
    conv_tight     = tight$convergence,
    maxabs_dE      = dE,
    maxabs_dB      = dB
  )
}
firm_dt <- rbindlist(firm_res)

cat("\n=== Assignment-loop summary across ", nrow(firm_dt), " LA firms ===\n", sep = "")
cat("iter_default   quantiles (5/50/95): ",
    paste(sprintf("%.0f", quantile(firm_dt$iter_default,
                                   probs = c(.05, .5, .95), na.rm = TRUE)),
          collapse = " / "), "\n", sep = "")
cat("iter_tight     quantiles (5/50/95): ",
    paste(sprintf("%.0f", quantile(firm_dt$iter_tight,
                                   probs = c(.05, .5, .95), na.rm = TRUE)),
          collapse = " / "), "\n", sep = "")
cat("fpevals_default median: ", median(firm_dt$fpevals_default, na.rm = TRUE), "\n", sep = "")
cat("fpevals_tight   median: ", median(firm_dt$fpevals_tight,   na.rm = TRUE), "\n", sep = "")
cat("conv_default   non-zero count: ", sum(firm_dt$conv_default != 0, na.rm = TRUE),
    " / ", sum(!is.na(firm_dt$conv_default)), "\n", sep = "")
cat("conv_tight     non-zero count: ", sum(firm_dt$conv_tight != 0, na.rm = TRUE),
    " / ", sum(!is.na(firm_dt$conv_tight)), "\n", sep = "")
cat("max(maxabs_dE): ", max(firm_dt$maxabs_dE, na.rm = TRUE), "\n", sep = "")
cat("max(maxabs_dB): ", max(firm_dt$maxabs_dB, na.rm = TRUE), "\n", sep = "")
cat("Firms with maxabs_dE > 1e-3:\n")
print(firm_dt[order(-maxabs_dE)][1:10,
              .(location_id, gamma,
                iter_default, iter_tight,
                fpevals_default, fpevals_tight,
                conv_default, conv_tight,
                maxabs_dE, maxabs_dB)])

## ---------------------------------------------------------------- price-loop probe
## Use the tight inner solve to construct Q, C, then run the price loop at
## default (1e-4) and tight (1e-8) outertol.
out_fields <- c("c_endog", "q_endog", e_field_names)
la_data[, (out_fields) := counterfactual_org_outputs(
    cost_matrix  = new_tild_theta,
    alpha        = as.numeric(.SD),
    gamma        = gamma_invert,
    wage_guess   = anchor,
    new_theta    = new_theta,
    innertol     = 1e-12,
    config       = CONFIG
  )[c("c_endog", "q_endog", e_field_names)],
  by = c("location_id"),
  .SDcols = task_mix_cols
]
la_data[, Q := q_endog * avg_labor + qual_exo]
la_data[, C := pmax(c_endog * avg_labor + cost_exo, 0)]

probe_price <- function(outertol) {
  p <- tryCatch(
    counterfactual_best_response_prices(
      la_data$cust_price, la_data$Q, la_data$C, la_data$weight, rho[LA],
      outertol, paste("probe", LA, QY, outertol)
    ),
    warning = function(w) {
      ## return the same call's result; status is already stored
      suppressWarnings(counterfactual_best_response_prices(
        la_data$cust_price, la_data$Q, la_data$C, la_data$weight, rho[LA],
        outertol, paste("probe", LA, QY, outertol)
      ))
    }
  )
  st <- counterfactual_last_price_status$status
  ## one extra iteration to estimate residual change at exit
  demand_index <- counterfactual_bounded_exp(la_data$Q + rho[LA] * p, CONFIG)
  denom <- pmax(1 + sum(la_data$weight * demand_index) - demand_index,
                CONFIG$numeric_floor)
  log_lw <- -1 + la_data$Q + rho[LA] * la_data$C - log(denom)
  p_next <- -1 / rho[LA] + la_data$C -
    counterfactual_lambertW0_exp(log_lw, CONFIG) / rho[LA]
  list(p = p,
       iter = st$iterations,
       converged = st$converged,
       max_step = max(abs(p_next - p)))
}

cat("\n=== Price-loop summary (single LA call) ===\n")
def_pr  <- probe_price(1e-4)
tight_pr <- probe_price(1e-8)
cat("Default outertol=1e-4: iter=", def_pr$iter,
    " converged=", def_pr$converged,
    " max|p_next - p_exit|=", sprintf("%.4g", def_pr$max_step), "\n", sep = "")
cat("Tight   outertol=1e-8: iter=", tight_pr$iter,
    " converged=", tight_pr$converged,
    " max|p_next - p_exit|=", sprintf("%.4g", tight_pr$max_step), "\n", sep = "")
cat("Max-abs price change default vs tight (across LA firms): ",
    sprintf("%.4g", max(abs(def_pr$p - tight_pr$p))), "\n", sep = "")

saveRDS(list(firm_dt = firm_dt,
             default_price = def_pr,
             tight_price   = tight_pr,
             anchor        = anchor),
        "smoke_la_diffusion_solver_innards.rds")
cat("\nSaved: smoke_la_diffusion_solver_innards.rds\n")

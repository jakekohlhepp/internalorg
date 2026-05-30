## CONVERGENCE STUDY: at the LA diffusion reorg anchor wages, sweep the inner
## SQUAREM precision (innertol x fixedpoint_max_iter) and record the labor-
## clearing objective (max|resid| and SSQ). Goal: find the (tol, maxiter) point
## beyond which the objective stops changing -- i.e. where tightening further is
## wasted because the inner fixed point is already resolved below the objective's
## sensitivity.
##
## Evaluated at a FIXED wage vector (the canonical 14_ diffusion reorg anchor),
## so the only thing varying is inner-solve precision.
##
## Output: smoke_la_reorg_tol_maxiter_convergence.rds (gitignored)
suppressPackageStartupMessages({
  library("data.table")
  library("parallel")
  library("SQUAREM")
})

source("config.R")
source("utils/counterfactuals_core.R")

n_w <- CONFIG$n_worker_types
n_t <- CONFIG$n_task_types
task_mix_cols   <- get_task_mix_cols(CONFIG)
e_field_names   <- counterfactual_e_field_names(CONFIG)
tot_field_names <- counterfactual_tot_labor_field_names(CONFIG)

LA <- "6037"; QY <- 2021.2
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
wd[county == LA & quarter_year == QY, gamma_invert := improve_it(gamma_invert)]  # diffusion shock
market_parms <- ctx$market_parms
rho <- ctx$rho
total_labor <- copy(ctx$total_labor)

la_data <- copy(wd[county == LA & quarter_year == QY, ..market_input_cols])

## anchor (canonical 14_ diffusion reorg wages)
tab <- as.data.table(readRDS(counterfactual_data_path("14_wages_diffusion.rds", CONFIG)))
anchor <- as.numeric(unlist(tab[county == LA & quarter_year == QY & sol_type == "reorg",
                                paste0("w", seq_len(n_w)), with = FALSE]))
cat("Diffusion reorg anchor (w_1..w_5): ",
    paste(sprintf("%.4f", anchor), collapse = ", "), "\n\n", sep = "")

new_theta <- matrix(
  market_parms[grep(paste0(LA, ":avg_labor:B"), names(market_parms))],
  ncol = n_t, nrow = n_w, byrow = FALSE)
w_mat <- matrix(anchor, ncol = n_t, nrow = n_w, byrow = FALSE)
new_tild_theta <- w_mat + (rho[LA])^(-1) * new_theta
new_tild_theta <- sweep(new_tild_theta, 2, apply(new_tild_theta, 2, min))

## inner assignment with explicit (tol, maxiter); returns E + slow-firm diag.
inner_assignment <- function(alpha, gamma, tol, maxiter) {
  ge <- counterfactual_effective_gamma(gamma, CONFIG)
  if (!(is.finite(ge) && ge > 0)) {
    B <- matrix(0, n_w, n_t)
    if (identical(ge, 0) || (!is.na(ge) && ge == 0)) {
      for (col in seq_len(n_t)) B[which.min(new_tild_theta[, col]), col] <- alpha[col]
    } else {
      B[which.min(rowSums(t(t(new_tild_theta) * alpha))), ] <- alpha
    }
    return(list(E = rowSums(B), fpevals = 0L, conv = NA_integer_))
  }
  A <- exp(-new_tild_theta / ge)
  A[A >= Inf] <- CONFIG$numeric_ceiling
  A[A <= 0]   <- CONFIG$numeric_floor
  fxpt <- function(p) {
    C <- colSums(t(A) * alpha / colSums(A * p))
    p * C
  }
  res <- squarem(rep(1 / n_w, n_w), fixptfn = fxpt,
                 control = list(maxiter = maxiter, tol = tol))
  E <- res$par
  B <- t(t(A) * alpha / colSums(A * E)) * E
  B[abs(B) < CONFIG$B_zero_threshold] <- 0
  list(E = rowSums(B), fpevals = res$fpevals, conv = as.integer(res$convergence))
}

## c_endog / q_endog from B-consistent E (mirror counterfactual_org_outputs path
## via the package solver to keep pricing identical).
eval_residual <- function(tol, maxiter) {
  cr <- copy(la_data)
  ## per-firm E + endogenous c/q using the package solver but with our tol/maxiter
  CFG <- CONFIG; CFG$fixedpoint_max_iter <- as.integer(maxiter)
  out_fields <- c("c_endog", "q_endog", e_field_names)
  cr[, (out_fields) := counterfactual_org_outputs(
      cost_matrix = new_tild_theta, alpha = as.numeric(.SD), gamma = gamma_invert,
      wage_guess = anchor, new_theta = new_theta, innertol = tol, config = CFG
    )[c("c_endog", "q_endog", e_field_names)],
    by = c("location_id"), .SDcols = task_mix_cols]
  cr[, Q := q_endog * avg_labor + qual_exo]
  cr[, C := pmax(c_endog * avg_labor + cost_exo, 0)]
  cr[, newprice := counterfactual_best_response_prices(
    cust_price, Q, C, weight, rho[LA], 1e-8, paste(LA, QY))]
  cr[, new_share := counterfactual_logit_shares(Q, newprice, weight, rho[LA])]
  ntl <- cr[, setNames(lapply(seq_len(n_w), function(idx)
    sum(weight * new_share * CSPOP * get(e_field_names[idx]) * avg_labor)), tot_field_names)]
  resid <- as.numeric(counterfactual_labor_gap(ntl, total_labor, LA, QY))

  ## diagnostic: slow-firm fpevals/conv at this (tol, maxiter)
  slow_idx <- which.min(la_data$gamma_invert)
  slow <- inner_assignment(as.numeric(la_data[slow_idx, ..task_mix_cols]),
                           la_data$gamma_invert[slow_idx], tol, maxiter)
  list(resid = resid, max_abs = max(abs(resid)), ssq = sum(resid^2),
       slow_fpevals = slow$fpevals, slow_conv = slow$conv)
}

## ---------------------------------------------------------------- grid
innertols <- c(1e-6, 1e-7, 1e-8, 1e-9, 1e-10, 1e-11, 1e-12, 1e-13, 1e-14)
maxiters  <- c(1e4, 1e5, 1e6, 1e7)
grid <- CJ(innertol = innertols, maxiter = maxiters, sorted = FALSE)

n_cores <- as.integer(Sys.getenv("SLURM_CPUS_PER_TASK", unset = "8"))
cat("Sweeping ", nrow(grid), " (tol, maxiter) combos on ", n_cores, " cores...\n", sep = "")

t0 <- Sys.time()
rows <- mclapply(seq_len(nrow(grid)), function(i) {
  tol <- grid$innertol[i]; mit <- grid$maxiter[i]
  ti <- Sys.time()
  r <- tryCatch(eval_residual(tol, mit), error = function(e) NULL)
  el <- as.numeric(difftime(Sys.time(), ti, units = "secs"))
  if (is.null(r)) return(data.table(innertol = tol, maxiter = mit,
                                     max_abs = NA, ssq = NA, slow_fpevals = NA,
                                     slow_conv = NA, elapsed = el))
  data.table(innertol = tol, maxiter = mit, max_abs = r$max_abs, ssq = r$ssq,
             r1 = r$resid[1], r2 = r$resid[2], r3 = r$resid[3],
             r4 = r$resid[4], r5 = r$resid[5],
             slow_fpevals = r$slow_fpevals, slow_conv = r$slow_conv, elapsed = el)
}, mc.cores = n_cores, mc.preschedule = FALSE)
out <- rbindlist(rows, fill = TRUE)
cat("Done in ", round(as.numeric(difftime(Sys.time(), t0, units = "secs"))), "s\n\n", sep = "")

setorder(out, innertol, maxiter)
cat("=== Objective vs (innertol, maxiter) at fixed anchor ===\n")
print(out[, .(innertol, maxiter, max_abs, ssq, slow_fpevals, slow_conv, elapsed)])

## reference = tightest setting that converged the slow firm
ref <- out[slow_conv == 1][order(-maxiter, innertol)][1]
if (nrow(ref) == 1 && is.finite(ref$max_abs)) {
  out[, d_max_abs_vs_ref := abs(max_abs - ref$max_abs)]
  cat(sprintf("\nReference (converged): innertol=%.0e maxiter=%.0e -> max|resid|=%.8g\n",
              ref$innertol, ref$maxiter, ref$max_abs))
  cat("\n=== |max_abs - reference| (plateau search) ===\n")
  print(out[, .(innertol, maxiter, max_abs, d_max_abs_vs_ref, slow_conv)])
  cat("\nCombos within 1e-4 of reference objective:\n")
  print(out[d_max_abs_vs_ref <= 1e-4, .(innertol, maxiter, max_abs, slow_fpevals, slow_conv)])
}

saveRDS(list(grid = out, anchor = anchor, reference = ref),
        "smoke_la_reorg_tol_maxiter_convergence.rds")
cat("\nSaved: smoke_la_reorg_tol_maxiter_convergence.rds\n")

## Is the LA baseline "0.0184" point (found by 13's NM-SSR phase under maxiter=2e5)
## a real near-clearing wage vector, or a maxiter-cap artifact?
##
## Method:
##   1. Rebuild the LA baseline residual (fresh assignment, no shock, target =
##      compute_counterfactual_total_labor, exactly as 13_'s solve_wages).
##   2. Under maxiter=2e5 / innertol=1e-12, run NM on the SSR from several starts
##      to rediscover a low-residual point w* (the ~0.0184 the running 13 hit).
##   3. Re-evaluate residual(w*) at maxiter in {2e5, 1e6, 1e7} (innertol=1e-12).
##      Stable ~0.018 => REAL.  Jumps to ~0.15 => 2e5-CAP ARTIFACT.
##   4. Report per-firm fpevals/convergence at w* for each maxiter to expose any
##      capping.
##
## Independent of the running 13-17 chain (read-only on saved 13_working_data).
## Output: smoke_la_baseline_0184_artifact.rds (gitignored)
suppressPackageStartupMessages({ library(data.table); library(parallel); library(SQUAREM) })

source("config.R")
source("utils/counterfactuals_core.R")

n_w <- CONFIG$n_worker_types
n_t <- CONFIG$n_task_types
task_mix_cols   <- get_task_mix_cols(CONFIG)
e_field_names   <- counterfactual_e_field_names(CONFIG)
tot_field_names <- counterfactual_tot_labor_field_names(CONFIG)
LA <- "6037"; QY <- get_counterfactual_focus_quarter()

ctx <- load_counterfactual_context()
working_data <- ctx$working_data
market_parms <- ctx$market_parms
rho <- ctx$rho

market_input_cols <- c(
  "location_id", "county", "quarter_year", "gamma_invert", "avg_labor",
  task_mix_cols, "qual_exo", "cost_exo", "weight", "cust_price", "CSPOP")

## data-anchored baseline target (exactly as 13_ line 183)
total_labor <- compute_counterfactual_total_labor(working_data, CONFIG)

la_data <- copy(working_data[county == LA & quarter_year == QY, ..market_input_cols])

build_mats <- function(wage_guess) {
  new_theta <- matrix(market_parms[grep(paste0(LA, ":avg_labor:B"), names(market_parms))],
                      ncol = n_t, nrow = n_w, byrow = FALSE)
  w_mat <- matrix(wage_guess, ncol = n_t, nrow = n_w, byrow = FALSE)
  ntt <- w_mat + (rho[LA])^(-1) * new_theta
  ntt <- sweep(ntt, 2, apply(ntt, 2, min))
  list(new_theta = new_theta, new_tild_theta = ntt)
}

## residual at given inner maxiter (innertol fixed 1e-12, price outertol 1e-8)
residual_fn <- function(wage_guess, maxiter) {
  if (!all(is.finite(wage_guess)) || any(wage_guess <= 0)) return(rep(1e6, n_w))
  CFG <- CONFIG; CFG$fixedpoint_max_iter <- as.integer(maxiter)
  cr <- copy(la_data); mats <- build_mats(wage_guess)
  of <- c("c_endog", "q_endog", e_field_names)
  cr[, (of) := counterfactual_org_outputs(
      cost_matrix = mats$new_tild_theta, alpha = as.numeric(.SD), gamma = gamma_invert,
      wage_guess = wage_guess, new_theta = mats$new_theta, innertol = 1e-12,
      config = CFG)[c("c_endog", "q_endog", e_field_names)],
    by = c("location_id"), .SDcols = task_mix_cols]
  cr[, Q := q_endog * avg_labor + qual_exo]
  cr[, C := pmax(c_endog * avg_labor + cost_exo, 0)]
  cr[, newprice := counterfactual_best_response_prices(
    cust_price, Q, C, weight, rho[LA], 1e-8, paste(LA, QY))]
  cr[, new_share := counterfactual_logit_shares(Q, newprice, weight, rho[LA])]
  ntl <- cr[, setNames(lapply(seq_len(n_w), function(idx)
    sum(weight * new_share * CSPOP * get(e_field_names[idx]) * avg_labor)), tot_field_names)]
  as.numeric(counterfactual_labor_gap(ntl, total_labor, LA, QY))
}

## per-firm fpevals/conv diag at a wage vector + maxiter
firm_caps <- function(wage_guess, maxiter) {
  mats <- build_mats(wage_guess)
  caps <- 0L; nonconv <- 0L; maxfp <- 0L
  for (i in seq_len(nrow(la_data))) {
    alpha <- as.numeric(la_data[i, ..task_mix_cols]); gamma <- la_data$gamma_invert[i]
    ge <- counterfactual_effective_gamma(gamma, CONFIG)
    if (!(is.finite(ge) && ge > 0)) next
    A <- exp(-mats$new_tild_theta / ge); A[A >= Inf] <- CONFIG$numeric_ceiling; A[A <= 0] <- CONFIG$numeric_floor
    fx <- function(p){ C <- colSums(t(A) * alpha / colSums(A * p)); p * C }
    r <- squarem(rep(1/n_w, n_w), fixptfn = fx, control = list(maxiter = maxiter, tol = 1e-12))
    if (as.integer(r$convergence) != 1) nonconv <- nonconv + 1L
    if (r$fpevals >= maxiter) caps <- caps + 1L
    maxfp <- max(maxfp, r$fpevals)
  }
  list(caps = caps, nonconv = nonconv, maxfp = maxfp)
}

## ---- starts: OLD baseline wages + warm start + perturbations
bw <- as.numeric(unlist(ctx$initial_wages[county == LA & quarter_year == QY,
                                          paste0("w", seq_len(n_w)), with = FALSE]))
cat("OLD 13 baseline LA wages: ", paste(sprintf("%.4f", bw), collapse = ", "), "\n", sep = "")
cat("residual at OLD baseline (maxiter=1e6): ",
    paste(sprintf("%.5g", residual_fn(bw, 1e6)), collapse = ", "), "\n\n", sep = "")

set.seed(20260528)
starts <- c(list(bw), lapply(1:11, function(i) bw * exp(rnorm(n_w, 0, 0.15))))

ssr2e5 <- function(logw) sum(residual_fn(exp(logw), 2e5)^2)
cat("Running NM multistart on SSR at maxiter=2e5 to rediscover the low point...\n")
res <- mclapply(starts, function(s) {
  o <- tryCatch(optim(log(s), ssr2e5, method = "Nelder-Mead",
                      control = list(maxit = 4000, reltol = 1e-12)),
                error = function(e) NULL)
  if (is.null(o)) return(NULL)
  w <- exp(o$par); r <- residual_fn(w, 2e5)
  list(w = w, max_abs = max(abs(r)), r = r)
}, mc.cores = as.integer(Sys.getenv("SLURM_CPUS_PER_TASK", "8")))
res <- Filter(Negate(is.null), res)
best <- res[[which.min(sapply(res, function(x) x$max_abs))]]

cat(sprintf("\nBest point under maxiter=2e5: max|r|=%.6g\n", best$max_abs))
cat("  wages: ", paste(sprintf("%.4f", best$w), collapse = ", "), "\n", sep = "")
cat("  resid: ", paste(sprintf("%.6g", best$r), collapse = ", "), "\n", sep = "")

## ---- the verdict: re-evaluate this exact w* at increasing maxiter
cat("\n=== residual(w*) vs maxiter (innertol=1e-12) ===\n")
ver <- list()
for (mit in c(2e5, 1e6, 1e7)) {
  r <- residual_fn(best$w, mit)
  fc <- firm_caps(best$w, mit)
  ver[[as.character(mit)]] <- list(maxiter = mit, r = r, max_abs = max(abs(r)),
                                   caps = fc$caps, nonconv = fc$nonconv, maxfp = fc$maxfp)
  cat(sprintf("maxiter=%-8.0g  max|r|=%-10.6g  bind=w%d  caps=%d nonconv=%d maxfp=%d\n",
              mit, max(abs(r)), which.max(abs(r)), fc$caps, fc$nonconv, fc$maxfp))
  cat("           resid: ", paste(sprintf("%.6g", r), collapse = ", "), "\n", sep = "")
}

r2e5 <- ver[["2e+05"]]$max_abs; r1e6 <- ver[["1e+06"]]$max_abs
verdict <- if (abs(r1e6 - r2e5) > 1e-2) "CAP ARTIFACT (jumps at 1e6)" else "REAL (stable across maxiter)"
cat("\n=========================================================\n")
cat("VERDICT: ", verdict, "\n", sep = "")
cat(sprintf("  max|r| at 2e5 = %.6g ; at 1e6 = %.6g ; delta = %.6g\n",
            r2e5, r1e6, abs(r1e6 - r2e5)))

saveRDS(list(best = best, verification = ver, old_baseline = bw, verdict = verdict),
        "smoke_la_baseline_0184_artifact.rds")
cat("\nSaved: smoke_la_baseline_0184_artifact.rds\n")

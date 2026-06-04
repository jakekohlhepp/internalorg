## Targeted cap-sensitivity proxy check. Loads the sweep-3 best wages saved
## by the timed-out verify job (smoke_la_017_cap_artifact.rds) and evaluates
## the LA labor-clearing residual at TWO caps (1e6 and 1e7) plus per-firm
## SQUAREM convergence/cap counts. If the residual is stable across caps
## AND no firm hits cap, the sweep-3 point is honest -- and by extension
## the canonical's tighter sweep-9 point is even less likely to be artifact.
suppressPackageStartupMessages({ library(data.table); library(SQUAREM) })
source("config.R"); source("utils/counterfactuals_core.R")

base <- CONFIG
base$counterfactual_innertol <- 1e-10
base$counterfactual_fixedpoint_max_iter <- 1000000L

n_w <- base$n_worker_types; n_t <- base$n_task_types
task_mix_cols <- get_task_mix_cols(base)
e_field_names <- counterfactual_e_field_names(base)
tot_field_names <- counterfactual_tot_labor_field_names(base)
LA <- "6037"; QY <- get_counterfactual_focus_quarter()

saved <- readRDS("smoke_la_017_cap_artifact.rds")
best_wages <- saved$best_wages
cat("sweep checkpoint:", saved$sweep, "\n")
cat("checkpoint best_max_1e6:", signif(saved$best_max_1e6, 6), "\n")
cat("best_wages: ", paste(sprintf("%.4f", best_wages), collapse=", "), "\n\n", sep="")

ctx <- load_counterfactual_context(config = base)
wd <- copy(ctx$working_data); market_parms <- ctx$market_parms; rho <- ctx$rho
total_labor <- compute_counterfactual_total_labor(wd, base)
mic <- c("location_id","county","quarter_year","gamma_invert","avg_labor",
         task_mix_cols,"qual_exo","cost_exo","weight","cust_price","CSPOP")
la <- copy(wd[county==LA & quarter_year==QY, ..mic])
new_theta <- matrix(market_parms[grep(paste0(LA,":avg_labor:B"), names(market_parms))],
                    ncol=n_t, nrow=n_w, byrow=FALSE)

resid_fn <- function(wages, maxiter) {
  CFG <- base; CFG$counterfactual_fixedpoint_max_iter <- as.integer(maxiter)
  w_mat <- matrix(wages, ncol=n_t, nrow=n_w, byrow=FALSE)
  ntt <- sweep(w_mat + (rho[LA])^(-1)*new_theta, 2,
               apply(w_mat + (rho[LA])^(-1)*new_theta, 2, min))
  cr <- copy(la); of <- c("c_endog","q_endog", e_field_names)
  cr[, (of) := counterfactual_org_outputs(
        cost_matrix=ntt, alpha=as.numeric(.SD), gamma=gamma_invert,
        wage_guess=wages, new_theta=new_theta,
        innertol=1e-10, config=CFG)[c("c_endog","q_endog",e_field_names)],
     by=c("location_id"), .SDcols=task_mix_cols]
  cr[, Q := q_endog*avg_labor + qual_exo]
  cr[, C := pmax(c_endog*avg_labor + cost_exo, 0)]
  cr[, newprice := counterfactual_best_response_prices(
        cust_price, Q, C, weight, rho[LA], 1e-8, paste(LA, QY))]
  cr[, new_share := counterfactual_logit_shares(Q, newprice, weight, rho[LA])]
  ntl <- cr[, setNames(lapply(seq_len(n_w),
        function(k) sum(weight*new_share*CSPOP*get(e_field_names[k])*avg_labor)),
        tot_field_names)]
  as.numeric(counterfactual_labor_gap(ntl, total_labor, LA, QY))
}

## Per-firm SQUAREM cap counter
firm_caps <- function(wages, maxiter) {
  w_mat <- matrix(wages, ncol=n_t, nrow=n_w, byrow=FALSE)
  ntt <- sweep(w_mat + (rho[LA])^(-1)*new_theta, 2,
               apply(w_mat + (rho[LA])^(-1)*new_theta, 2, min))
  caps <- 0L; nonconv <- 0L; maxfp <- 0L
  for (i in seq_len(nrow(la))) {
    alpha <- as.numeric(la[i, ..task_mix_cols])
    g <- la$gamma_invert[i]
    ge <- counterfactual_effective_gamma(g, base)
    if (!(is.finite(ge) && ge > 0)) next
    A <- exp(-ntt/ge)
    A[A >= Inf] <- base$numeric_ceiling
    A[A <= 0]  <- base$numeric_floor
    fx <- function(p) {
      pp <- pmax(p, base$numeric_floor)
      pp * colSums(t(A) * alpha / pmax(colSums(A*pp), base$numeric_floor))
    }
    r <- squarem(rep(1/n_w, n_w), fixptfn=fx,
                 control=list(maxiter=maxiter, tol=1e-10))
    if (!isTRUE(r$convergence)) nonconv <- nonconv + 1L
    if (r$fpevals >= maxiter)   caps    <- caps + 1L
    maxfp <- max(maxfp, r$fpevals)
  }
  list(caps=caps, nonconv=nonconv, maxfp=maxfp, n_firms=nrow(la))
}

cat("=== cap-sensitivity at sweep-3 best wages ===\n")
for (mit in c(1e6, 1e7)) {
  r  <- resid_fn(best_wages, mit)
  fc <- firm_caps(best_wages, mit)
  cat(sprintf("cap=%-9.0g  max|r|=%-10.6g  bind=w%d  caps=%d/%d  nonconv=%d  maxfp=%d\n",
              mit, max(abs(r)), which.max(abs(r)), fc$caps, fc$n_firms, fc$nonconv, fc$maxfp))
  cat("  resid: ", paste(sprintf("%.5g", r), collapse=", "), "\n", sep="")
}

r1 <- resid_fn(best_wages, 1e6); m1 <- max(abs(r1))
r2 <- resid_fn(best_wages, 1e7); m2 <- max(abs(r2))
fc <- firm_caps(best_wages, 1e6)
d <- abs(m1 - m2)
cat(sprintf("\n|max|r| at 1e7 - at 1e6| = %.4g\n", d))
cat(sprintf("firms hitting cap at 1e6: %d / %d\n", fc$caps, fc$n_firms))
if (d < 1e-3 && fc$caps == 0) {
  cat("VERDICT: HONEST -> sweep-3 point at 0.0154 is REAL; canonical's 0.01086 at the same tolerances is likely also honest.\n")
} else if (fc$caps > 0) {
  cat(sprintf("VERDICT: %d firm(s) hit cap=1e6 -> cap-artifact risk REMAINS; need cap=1e7 or tighter innertol for these firms.\n", fc$caps))
} else {
  cat(sprintf("VERDICT: residual moves %.4g across caps -> cap-sensitive but not cap-bound; canonical's result may be artifact-prone.\n", d))
}

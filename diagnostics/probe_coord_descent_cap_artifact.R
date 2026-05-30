## Is the coord-descent ~0.024 LA baseline residual real, or a maxiter-cap
## artifact (incomplete inner SQUAREM convergence at cap=2e5)?
##
## 1. Run coord descent (cap=2e5, innertol=1e-12) to a ~0.024 point, SAVING the
##    best wages each sweep (so a timeout still leaves the point on disk).
## 2. Re-evaluate that exact wage vector's 5-d clearing residual at cap in
##    {2e5, 1e6, 1e7}, and count per-firm inner solves that hit the cap.
##    Stable across caps -> real. Jumps -> cap artifact.
suppressPackageStartupMessages({ library(data.table); library(SQUAREM) })
source("config.R"); source("utils/counterfactuals_core.R")

n_w <- CONFIG$n_worker_types; n_t <- CONFIG$n_task_types
task_mix_cols   <- get_task_mix_cols(CONFIG)
e_field_names   <- counterfactual_e_field_names(CONFIG)
tot_field_names <- counterfactual_tot_labor_field_names(CONFIG)
LA <- "6037"; QY <- get_counterfactual_focus_quarter()
SAVE <- "smoke_coord_descent_cap_artifact.rds"

base <- CONFIG; base$counterfactual_innertol <- 1e-12
ctx <- load_counterfactual_context(config = base)
working_data <- ctx$working_data; market_parms <- ctx$market_parms; rho <- ctx$rho
total_labor <- compute_counterfactual_total_labor(working_data, base)
mic <- c("location_id","county","quarter_year","gamma_invert","avg_labor",
         task_mix_cols,"qual_exo","cost_exo","weight","cust_price","CSPOP")
la <- copy(working_data[county==LA & quarter_year==QY, ..mic])
new_theta <- matrix(market_parms[grep(paste0(LA,":avg_labor:B"),names(market_parms))],ncol=n_t,nrow=n_w,byrow=FALSE)

## residual at a given inner cap (innertol fixed 1e-12, price outertol 1e-8)
resid_fn <- function(wages, maxiter) {
  if (any(!is.finite(wages))||any(wages<=0)) return(rep(1e6,n_w))
  CFG <- base; CFG$counterfactual_fixedpoint_max_iter <- as.integer(maxiter)
  w_mat <- matrix(wages,ncol=n_t,nrow=n_w,byrow=FALSE)
  ntt <- sweep(w_mat + (rho[LA])^(-1)*new_theta, 2, apply(w_mat + (rho[LA])^(-1)*new_theta,2,min))
  cr <- copy(la); of <- c("c_endog","q_endog",e_field_names)
  cr[, (of) := counterfactual_org_outputs(cost_matrix=ntt, alpha=as.numeric(.SD), gamma=gamma_invert,
        wage_guess=wages, new_theta=new_theta, innertol=1e-12, config=CFG)[c("c_endog","q_endog",e_field_names)],
     by=c("location_id"), .SDcols=task_mix_cols]
  cr[, Q := q_endog*avg_labor+qual_exo]; cr[, C := pmax(c_endog*avg_labor+cost_exo,0)]
  cr[, newprice := counterfactual_best_response_prices(cust_price,Q,C,weight,rho[LA],1e-8,paste(LA,QY))]
  cr[, new_share := counterfactual_logit_shares(Q,newprice,weight,rho[LA])]
  ntl <- cr[, setNames(lapply(seq_len(n_w),function(k) sum(weight*new_share*CSPOP*get(e_field_names[k])*avg_labor)),tot_field_names)]
  as.numeric(counterfactual_labor_gap(ntl, total_labor, LA, QY))
}
## per-firm inner cap/convergence count at given wages + cap
firm_caps <- function(wages, maxiter) {
  w_mat <- matrix(wages,ncol=n_t,nrow=n_w,byrow=FALSE)
  ntt <- sweep(w_mat + (rho[LA])^(-1)*new_theta, 2, apply(w_mat + (rho[LA])^(-1)*new_theta,2,min))
  caps <- 0L; nonconv <- 0L; maxfp <- 0L
  for (i in seq_len(nrow(la))) {
    alpha <- as.numeric(la[i,..task_mix_cols]); g <- la$gamma_invert[i]
    ge <- counterfactual_effective_gamma(g, base); if(!(is.finite(ge)&&ge>0)) next
    A <- exp(-ntt/ge); A[A>=Inf]<-base$numeric_ceiling; A[A<=0]<-base$numeric_floor
    fx <- function(p){ pp<-pmax(p,base$numeric_floor); pp*colSums(t(A)*alpha/pmax(colSums(A*pp),base$numeric_floor)) }
    r <- squarem(rep(1/n_w,n_w), fixptfn=fx, control=list(maxiter=maxiter, tol=1e-12))
    if (as.integer(r$convergence)!=1) nonconv <- nonconv+1L
    if (r$fpevals>=maxiter) caps <- caps+1L
    maxfp <- max(maxfp, r$fpevals)
  }
  list(caps=caps, nonconv=nonconv, maxfp=maxfp)
}

## --- coord descent at cap=2e5, saving best each sweep ---
w1 <- market_parms[paste0("avg_labor:factor(county)",LA,":factor(quarter_year)",QY)]
diffs <- market_parms[grep(paste0(LA,":avg_labor:E"),names(market_parms))]
w <- as.numeric(c(w1, w1+diffs))
best <- w; best_max <- max(abs(resid_fn(w, 2e5)))
cat(sprintf("start max|r|=%.5g\n", best_max))
for (s in seq_len(6L)) {
  for (k in seq_len(n_w)) {
    rk <- function(wk){ ww<-w; ww[k]<-wk; resid_fn(ww,2e5)[k] }
    flo<-tryCatch(rk(1),error=function(e)NA); fhi<-tryCatch(rk(2000),error=function(e)NA)
    if (is.finite(flo)&&is.finite(fhi)&&sign(flo)!=sign(fhi))
      w[k] <- tryCatch(stats::uniroot(rk,lower=1,upper=2000,tol=1e-8,maxiter=200)$root, error=function(e) w[k])
  }
  mr <- max(abs(resid_fn(w,2e5)))
  if (mr<best_max){ best<-w; best_max<-mr }
  cat(sprintf("sweep %d: max|r|=%.6g (best=%.6g)\n", s, mr, best_max))
  saveRDS(list(best_wages=best, best_max_2e5=best_max, sweep=s), SAVE)  # checkpoint
  if (best_max<0.02) break
}

## --- cap-sensitivity check on the best point ---
cat("\n=== cap-sensitivity of the coord-descent best point ===\n")
cat("best wages: ", paste(sprintf("%.3f",best),collapse=", "), "\n", sep="")
verify <- list()
for (mit in c(2e5,1e6,1e7)) {
  r <- resid_fn(best, mit); fc <- firm_caps(best, mit)
  verify[[as.character(mit)]] <- list(maxiter=mit, max_abs=max(abs(r)), resid=r,
                                      caps=fc$caps, nonconv=fc$nonconv, maxfp=fc$maxfp)
  cat(sprintf("maxiter=%-8.0g  max|r|=%-10.6g  bind=w%d  caps=%d nonconv=%d maxfp=%d\n",
              mit, max(abs(r)), which.max(abs(r)), fc$caps, fc$nonconv, fc$maxfp))
  cat("   resid: ", paste(sprintf("%.5g",r),collapse=", "), "\n", sep="")
}
d <- abs(verify[["1e+07"]]$max_abs - verify[["2e+05"]]$max_abs)
cat(sprintf("\n|max|r| at 1e7 - at 2e5| = %.4g\n", d))
cat(if (d < 1e-3) "VERDICT: STABLE across caps -> 0.024 is REAL (inner converged), not a cap artifact\n"
    else "VERDICT: residual MOVES with cap -> the 2e5 result was a CAP ARTIFACT\n")
saveRDS(list(best_wages=best, best_max_2e5=best_max, verify=verify), SAVE)
cat("Saved: ", SAVE, "\n", sep="")

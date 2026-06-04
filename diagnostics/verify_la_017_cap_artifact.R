## Is 13 canonical's ~0.017 coord-descent point real, or a cap=1e6 artifact?
##
## Reproduces production coord_descent on the LA baseline at the SAME settings
## the running 13 canonical (52877088) uses: innertol=1e-10, cap=1e6,
## per-coordinate uniroot with cd_widths starting at 2.0. Starts from the
## production warm start (where 13's BBsolve floored at ~0.15). Saves best
## wages each sweep. Then re-evaluates the best point at cap in {1e6, 1e7}
## with per-firm cap counts to settle real-vs-artifact.
##
## Output: smoke_la_017_cap_artifact.rds
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
SAVE <- "smoke_la_017_cap_artifact.rds"

ctx <- load_counterfactual_context(config = base)
wd <- copy(ctx$working_data); market_parms <- ctx$market_parms; rho <- ctx$rho
init <- ctx$initial_wages
total_labor <- compute_counterfactual_total_labor(wd, base)   # baseline DATA target
mic <- c("location_id","county","quarter_year","gamma_invert","avg_labor",
         task_mix_cols,"qual_exo","cost_exo","weight","cust_price","CSPOP")
la <- copy(wd[county==LA & quarter_year==QY, ..mic])
new_theta <- matrix(market_parms[grep(paste0(LA,":avg_labor:B"),names(market_parms))],ncol=n_t,nrow=n_w,byrow=FALSE)

## residual evaluator at given cap (innertol fixed 1e-10, price outertol 1e-8)
resid_fn <- function(wages, maxiter) {
  if (any(!is.finite(wages)) || any(wages<=0)) return(rep(1e6,n_w))
  CFG <- base; CFG$counterfactual_fixedpoint_max_iter <- as.integer(maxiter)
  w_mat <- matrix(wages,ncol=n_t,nrow=n_w,byrow=FALSE)
  ntt <- sweep(w_mat + (rho[LA])^(-1)*new_theta, 2, apply(w_mat + (rho[LA])^(-1)*new_theta,2,min))
  cr <- copy(la); of <- c("c_endog","q_endog",e_field_names)
  cr[, (of) := counterfactual_org_outputs(cost_matrix=ntt, alpha=as.numeric(.SD), gamma=gamma_invert,
        wage_guess=wages, new_theta=new_theta, innertol=1e-10, config=CFG)[c("c_endog","q_endog",e_field_names)],
     by=c("location_id"), .SDcols=task_mix_cols]
  cr[, Q := q_endog*avg_labor + qual_exo]; cr[, C := pmax(c_endog*avg_labor + cost_exo,0)]
  cr[, newprice := counterfactual_best_response_prices(cust_price,Q,C,weight,rho[LA],1e-8,paste(LA,QY))]
  cr[, new_share := counterfactual_logit_shares(Q,newprice,weight,rho[LA])]
  ntl <- cr[, setNames(lapply(seq_len(n_w),function(k) sum(weight*new_share*CSPOP*get(e_field_names[k])*avg_labor)),tot_field_names)]
  as.numeric(counterfactual_labor_gap(ntl, total_labor, LA, QY))
}
firm_caps <- function(wages, maxiter) {
  w_mat <- matrix(wages,ncol=n_t,nrow=n_w,byrow=FALSE)
  ntt <- sweep(w_mat + (rho[LA])^(-1)*new_theta, 2, apply(w_mat + (rho[LA])^(-1)*new_theta,2,min))
  caps <- 0L; nonconv <- 0L; maxfp <- 0L
  for (i in seq_len(nrow(la))) {
    alpha <- as.numeric(la[i,..task_mix_cols]); g <- la$gamma_invert[i]
    ge <- counterfactual_effective_gamma(g, base); if(!(is.finite(ge)&&ge>0)) next
    A <- exp(-ntt/ge); A[A>=Inf]<-base$numeric_ceiling; A[A<=0]<-base$numeric_floor
    fx <- function(p){ pp<-pmax(p,base$numeric_floor); pp*colSums(t(A)*alpha/pmax(colSums(A*pp),base$numeric_floor)) }
    r <- squarem(rep(1/n_w,n_w), fixptfn=fx, control=list(maxiter=maxiter, tol=1e-10))
    if (!isTRUE(r$convergence)) nonconv <- nonconv+1L
    if (r$fpevals>=maxiter) caps <- caps+1L
    maxfp <- max(maxfp, r$fpevals)
  }
  list(caps=caps, nonconv=nonconv, maxfp=maxfp)
}

## --- coord descent mirroring production: starts from 13 warm start, shrinking
## cd_widths starting at 2.0 in log space, innertol=1e-10, cap=1e6. ---
ws_tab <- as.data.table(readRDS(counterfactual_data_path("13_warm_start_wages.rds", base)))
w <- as.numeric(unlist(ws_tab[county==LA, paste0("w",seq_len(n_w)), with=FALSE]))
cat("Start wages (13_warm_start LA): ", paste(sprintf("%.3f",w),collapse=","), "\n", sep="")
best <- w; best_max <- max(abs(resid_fn(w, 1e6)))
cat(sprintf("start max|r|=%.5g\n", best_max))
cd_widths <- c(2.0, 1.0, 0.5, 0.25, 0.12, 0.06)   # 6 sweeps is enough to hit 0.017 if it's there
cur <- log(pmax(best, base$numeric_floor))
for (s in seq_along(cd_widths)) {
  hw_k <- cd_widths[s]
  for (i in seq_len(n_w)) {
    ri <- function(z){ x<-cur; x[i]<-z; resid_fn(exp(x),1e6)[i] }
    lo <- cur[i]-hw_k; hi <- cur[i]+hw_k
    flo <- tryCatch(ri(lo),error=function(e)NA); fhi <- tryCatch(ri(hi),error=function(e)NA)
    if (is.finite(flo) && is.finite(fhi) && sign(flo)!=sign(fhi)) {
      sol <- tryCatch(stats::uniroot(ri,lower=lo,upper=hi,tol=1e-12,maxiter=300)$root, error=function(e) cur[i])
      cur[i] <- sol
    }
  }
  curw <- exp(cur); mr <- max(abs(resid_fn(curw,1e6)))
  if (mr < best_max) { best <- curw; best_max <- mr }
  cat(sprintf("sweep %d (hw=%.3g): max|r|=%.6g (best=%.6g)\n", s, hw_k, mr, best_max))
  saveRDS(list(best_wages=best, best_max_1e6=best_max, sweep=s), SAVE)
  if (best_max < 0.012) break
}

cat("\n=== cap-sensitivity on best point ===\n")
cat("best wages: ", paste(sprintf("%.3f",best),collapse=", "), "\n", sep="")
verify <- list()
for (mit in c(1e6, 1e7)) {
  r <- resid_fn(best, mit); fc <- firm_caps(best, mit)
  verify[[as.character(mit)]] <- list(maxiter=mit, max_abs=max(abs(r)), resid=r,
                                      caps=fc$caps, nonconv=fc$nonconv, maxfp=fc$maxfp)
  cat(sprintf("maxiter=%-8.0g  max|r|=%-10.6g  bind=w%d  caps=%d nonconv=%d maxfp=%d\n",
              mit, max(abs(r)), which.max(abs(r)), fc$caps, fc$nonconv, fc$maxfp))
  cat("   resid: ", paste(sprintf("%.5g",r),collapse=", "), "\n", sep="")
}
d <- abs(verify[["1e+07"]]$max_abs - verify[["1e+06"]]$max_abs)
cat(sprintf("\n|max|r| at 1e7 - at 1e6| = %.4g\n", d))
cat(if (d < 1e-3) "VERDICT: STABLE -> ~0.017 is REAL clearing under honest tols, NOT a cap artifact.\n"
    else "VERDICT: residual MOVES with cap -> cap=1e6 at innertol=1e-10 is STILL artifact-prone.\n")
saveRDS(list(best=best, best_max=best_max, verify=verify), SAVE)
cat("Saved.\n")

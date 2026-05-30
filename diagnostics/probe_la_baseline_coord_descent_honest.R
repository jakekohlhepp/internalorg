## Does coordinate descent clear the LA BASELINE labor market at HONEST inner
## tol (innertol=1e-12, SQUAREM cap=1e6)? The full-5D ladder (nleqslv/NM/...)
## floors at max|r|=0.1505 (worker 5) because it stalls near the estimated-wage
## region where worker-5 demand ~0 (singular Jacobian). Coordinate descent with
## per-coordinate uniroot searches the wide wage range and should find the w5
## that generates worker-5 demand = target -- IF a joint clearing solution
## exists. Prior coord-descent "wins" (~0.02) were at LOOSE tol (false basins);
## this tests it at honest tol.
##
## Residual = 13_'s baseline solve_wages: fresh per-firm assignment + best-
## response pricing, target = compute_counterfactual_total_labor (data anchor),
## clearing gap in log-labor.
##
## Output: smoke_la_baseline_coord_descent_honest.rds
suppressPackageStartupMessages({ library(data.table) })
source("config.R"); source("utils/counterfactuals_core.R")

CFG <- CONFIG
## innertol 1e-12 keeps the solve honest (no false basin); cap 2e5 is enough
## per the convergence study (2e5 vs 1e6 differ by ~3e-5 in the aggregate
## residual) and ~5x faster than 1e6 during the uniroot search.
CFG$counterfactual_innertol <- 1e-12
CFG$counterfactual_fixedpoint_max_iter <- 200000L
innertol <- CFG$counterfactual_innertol
outertol <- 1e-8

n_w <- CFG$n_worker_types; n_t <- CFG$n_task_types
task_mix_cols   <- get_task_mix_cols(CFG)
e_field_names   <- counterfactual_e_field_names(CFG)
tot_field_names <- counterfactual_tot_labor_field_names(CFG)
LA <- "6037"; QY <- get_counterfactual_focus_quarter()

ctx <- load_counterfactual_context(config = CFG)
working_data <- ctx$working_data; market_parms <- ctx$market_parms; rho <- ctx$rho
total_labor <- compute_counterfactual_total_labor(working_data, CFG)   # DATA anchor (13_ baseline target)

mic <- c("location_id","county","quarter_year","gamma_invert","avg_labor",
         task_mix_cols,"qual_exo","cost_exo","weight","cust_price","CSPOP")
la <- copy(working_data[county==LA & quarter_year==QY, ..mic])
new_theta <- matrix(market_parms[grep(paste0(LA,":avg_labor:B"),names(market_parms))],ncol=n_t,nrow=n_w,byrow=FALSE)

resid_fn <- function(wages) {
  if (any(!is.finite(wages)) || any(wages<=0)) return(rep(1e6,n_w))
  w_mat <- matrix(wages, ncol=n_t, nrow=n_w, byrow=FALSE)
  ntt <- sweep(w_mat + (rho[LA])^(-1)*new_theta, 2, apply(w_mat + (rho[LA])^(-1)*new_theta, 2, min))
  cr <- copy(la); of <- c("c_endog","q_endog", e_field_names)
  cr[, (of) := counterfactual_org_outputs(cost_matrix=ntt, alpha=as.numeric(.SD), gamma=gamma_invert,
        wage_guess=wages, new_theta=new_theta, innertol=innertol, config=CFG)[c("c_endog","q_endog",e_field_names)],
     by=c("location_id"), .SDcols=task_mix_cols]
  cr[, Q := q_endog*avg_labor + qual_exo]; cr[, C := pmax(c_endog*avg_labor + cost_exo,0)]
  cr[, newprice := counterfactual_best_response_prices(cust_price,Q,C,weight,rho[LA],outertol,paste(LA,QY))]
  cr[, new_share := counterfactual_logit_shares(Q,newprice,weight,rho[LA])]
  ntl <- cr[, setNames(lapply(seq_len(n_w), function(k) sum(weight*new_share*CSPOP*get(e_field_names[k])*avg_labor)), tot_field_names)]
  as.numeric(counterfactual_labor_gap(ntl, total_labor, LA, QY))
}

## starts: estimated wages (parm_wage_vec) and the 13_ warm-start
w1 <- market_parms[paste0("avg_labor:factor(county)",LA,":factor(quarter_year)",QY)]
diffs <- market_parms[grep(paste0(LA,":avg_labor:E"),names(market_parms))]
est_w <- as.numeric(c(w1, w1+diffs))
ws_tab <- tryCatch(as.data.table(readRDS(counterfactual_data_path("13_warm_start_wages.rds",CFG))), error=function(e) NULL)
warm_w <- if(!is.null(ws_tab)) as.numeric(unlist(ws_tab[county==LA, paste0("w",seq_len(n_w)), with=FALSE])) else est_w

## coordinate descent: per coordinate, uniroot the k-th clearing residual on w_k.
coord_descent <- function(start, sweeps=10L, lo=1, hi=2000) {
  w <- start
  best <- w; best_max <- max(abs(resid_fn(w)))
  cat(sprintf("  start max|r|=%.5g  wages=%s\n", best_max, paste(sprintf("%.2f",w),collapse=",")))
  for (s in seq_len(sweeps)) {
    for (k in seq_len(n_w)) {
      rk <- function(wk){ ww <- w; ww[k] <- wk; resid_fn(ww)[k] }
      flo <- tryCatch(rk(lo), error=function(e) NA); fhi <- tryCatch(rk(hi), error=function(e) NA)
      if (is.finite(flo) && is.finite(fhi) && sign(flo)!=sign(fhi)) {
        rt <- tryCatch(stats::uniroot(rk, lower=lo, upper=hi, tol=1e-8, maxiter=200)$root, error=function(e) w[k])
        w[k] <- rt
      }
    }
    mr <- max(abs(resid_fn(w)))
    if (mr < best_max) { best <- w; best_max <- mr }
    cat(sprintf("  sweep %2d: max|r|=%.6g\n", s, mr))
    if (best_max < 1e-3) break
  }
  list(w=best, max_abs=best_max, resid=resid_fn(best))
}

cat("=== coord descent on LA baseline at HONEST tol, start = ESTIMATED wages ===\n")
r_est <- coord_descent(est_w)
cat("\n=== coord descent on LA baseline at HONEST tol, start = 13_ warm-start ===\n")
r_warm <- coord_descent(warm_w)

best <- if (r_est$max_abs <= r_warm$max_abs) r_est else r_warm
cat("\n================= RESULT =================\n")
cat(sprintf("best max|r| = %.6g  (ladder floor was 0.1505)\n", best$max_abs))
cat("residual by worker: ", paste(sprintf("%.5g", best$resid), collapse=", "), "\n", sep="")
cat("clearing wages:     ", paste(sprintf("%.3f", best$w), collapse=", "), "\n", sep="")
cat(if (best$max_abs < 0.01) "VERDICT: CLEARS -> worker-5 floor was a SOLVER limitation, NOT structural\n"
    else if (best$max_abs < 0.1505*0.9) "VERDICT: improves on ladder but still > tol\n"
    else "VERDICT: floors at ~0.15 too -> structural (coord-descent prior wins were loose-tol artifacts)\n")

saveRDS(list(est=r_est, warm=r_warm, best=best), "smoke_la_baseline_coord_descent_honest.rds")
cat("Saved.\n")

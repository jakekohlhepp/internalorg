## PROTOTYPE: daarem (damped Anderson accel w/ restarts + epsilon-monotonicity)
## for the inner assignment fixed point, vs SQUAREM, on LA firms.
##
## Hand-rolled plain AA was ~3000x faster on stiff firms but jumped basins with
## the wrong window. daarem's damping/monotonicity should keep the speedup AND
## land in the SQUAREM basin for every firm. This is the production-candidate test.
##
## Tests ALL LA interior firms (not just a sample) so we catch any firm where
## daarem disagrees with SQUAREM-1e6. PASS = max dE over ALL firms < 1e-5.
##
## Output: smoke_prototype_daarem_inner.rds (gitignored)
suppressPackageStartupMessages({ library(data.table); library(SQUAREM); library(daarem) })

source("config.R")
source("utils/counterfactuals_core.R")

n_w <- CONFIG$n_worker_types; n_t <- CONFIG$n_task_types
task_mix_cols <- get_task_mix_cols(CONFIG)
LA <- "6037"; QY <- get_counterfactual_focus_quarter()

ctx <- load_counterfactual_context()
wd <- copy(ctx$working_data); market_parms <- ctx$market_parms; rho <- ctx$rho
improve_it <- function(x){ r <- frank(x, ties.method="first"); sort(x)[pmax(r-1,1)] }
wd[county==LA & quarter_year==QY, gamma_invert := improve_it(gamma_invert)]
la <- wd[county==LA & quarter_year==QY]

anchor <- as.numeric(unlist(as.data.table(readRDS(counterfactual_data_path("14_wages_diffusion.rds", CONFIG)))[
  county==LA & quarter_year==QY & sol_type=="reorg", paste0("w", seq_len(n_w)), with=FALSE]))
new_theta <- matrix(market_parms[grep(paste0(LA,":avg_labor:B"), names(market_parms))], ncol=n_t, nrow=n_w, byrow=FALSE)
w_mat <- matrix(anchor, ncol=n_t, nrow=n_w, byrow=FALSE)
tmp <- w_mat + (rho[LA])^(-1)*new_theta
cost_matrix <- sweep(tmp, 2, apply(tmp,2,min))

make_A <- function(g){ A<-exp(-cost_matrix/g); A[A>=Inf]<-CONFIG$numeric_ceiling; A[A<=0]<-CONFIG$numeric_floor; A }
B_from <- function(A,alpha,E){ B<-t(t(A)*alpha/colSums(A*E))*E; B[abs(B)<CONFIG$B_zero_threshold]<-0; B }
make_fxpt <- function(A,alpha) function(p){ pp<-pmax(p,CONFIG$numeric_floor); d<-pmax(colSums(A*pp),CONFIG$numeric_floor); pp*colSums(t(A)*alpha/d) }
make_obj  <- function(A,alpha,fx) function(p){ r<-fx(p)-p; -sum(r*r) }   # maximize -> 0 residual

run_sq <- function(A,alpha){ fx<-make_fxpt(A,alpha); t0<-proc.time()[[3]]
  r<-squarem(rep(1/n_w,n_w),fixptfn=fx,control=list(maxiter=1e6,tol=1e-12))
  list(E=rowSums(B_from(A,alpha,r$par)), fpevals=r$fpevals, sec=proc.time()[[3]]-t0) }
run_da <- function(A,alpha){ fx<-make_fxpt(A,alpha); ob<-make_obj(A,alpha,fx); t0<-proc.time()[[3]]
  r<-tryCatch(daarem(rep(1/n_w,n_w), fixptfn=fx, objfn=ob, control=list(maxiter=2000, tol=1e-12)),
              error=function(e) NULL)
  if(is.null(r)) return(list(E=rep(NA_real_,n_w),fpevals=NA,conv=NA,sec=proc.time()[[3]]-t0))
  list(E=rowSums(B_from(A,alpha,r$par)), fpevals=r$fpevals, conv=as.integer(r$convergence), sec=proc.time()[[3]]-t0) }

res <- list()
for (i in seq_len(nrow(la))){
  alpha<-as.numeric(la[i,..task_mix_cols]); gamma<-la$gamma_invert[i]
  ge<-counterfactual_effective_gamma(gamma,CONFIG); if(!(is.finite(ge)&&ge>0)) next
  A<-make_A(ge)
  sq<-run_sq(A,alpha); da<-run_da(A,alpha)
  res[[length(res)+1]]<-data.table(loc=substr(la$location_id[i],1,8), gamma=round(gamma,3),
    sq_fpevals=sq$fpevals, sq_sec=round(sq$sec,3),
    da_fpevals=da$fpevals, da_sec=round(da$sec,3), da_conv=da$conv,
    dE=max(abs(da$E-sq$E)))
}
out<-rbindlist(res,fill=TRUE); setorder(out,-dE)

cat("=== daarem vs SQUAREM-1e6 over ALL ", nrow(out), " LA interior firms ===\n", sep="")
cat("Worst 8 by dE:\n"); print(head(out,8))
cat("\nfirms with dE>1e-5: ", sum(out$dE>1e-5,na.rm=TRUE), " / ", nrow(out), "\n", sep="")
cat("max dE over all firms: ", signif(max(out$dE,na.rm=TRUE),3), "\n", sep="")
cat("daarem non-converged (conv!=0): ", sum(out$da_conv!=0,na.rm=TRUE), "\n", sep="")
cat("\ntotal fpevals  SQUAREM: ", sum(out$sq_fpevals,na.rm=TRUE),
    "  daarem: ", sum(out$da_fpevals,na.rm=TRUE),
    "  (", round(sum(out$sq_fpevals,na.rm=TRUE)/max(sum(out$da_fpevals,na.rm=TRUE),1),1), "x fewer)\n", sep="")
cat("total wall sec SQUAREM: ", round(sum(out$sq_sec,na.rm=TRUE),2),
    "  daarem: ", round(sum(out$da_sec,na.rm=TRUE),2), "\n", sep="")

verdict <- if (max(out$dE,na.rm=TRUE) < 1e-5 && sum(out$dE>1e-5,na.rm=TRUE)==0)
  "PASS: daarem matches SQUAREM-1e6 on ALL firms, far fewer evals (production-viable)" else
  "FAIL: daarem disagrees with SQUAREM on some firm(s); not safe as drop-in"
cat("\nVERDICT: ", verdict, "\n", sep="")

saveRDS(list(out=out, verdict=verdict), "smoke_prototype_daarem_inner.rds")
cat("Saved: smoke_prototype_daarem_inner.rds\n")

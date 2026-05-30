## PROTOTYPE: daarem + residual gate + SQUAREM fallback (production-candidate
## inner assignment solver). For each firm: run daarem (fast); if its true
## fixed-point residual ||fxpt(E)-E|| exceeds a tight gate, fall back to SQUAREM.
## Goal: match SQUAREM-1e6 on ALL 53 LA firms while keeping most of the speedup.
##
## Output: smoke_prototype_daarem_gated_inner.rds (gitignored)
suppressPackageStartupMessages({ library(data.table); library(SQUAREM); library(daarem) })
source("config.R"); source("utils/counterfactuals_core.R")

n_w <- CONFIG$n_worker_types; n_t <- CONFIG$n_task_types
task_mix_cols <- get_task_mix_cols(CONFIG)
LA <- "6037"; QY <- get_counterfactual_focus_quarter()
GATE <- 1e-9   # max allowed ||fxpt(E)-E|| to trust the daarem result

ctx <- load_counterfactual_context()
wd <- copy(ctx$working_data); market_parms <- ctx$market_parms; rho <- ctx$rho
improve_it <- function(x){ r<-frank(x,ties.method="first"); sort(x)[pmax(r-1,1)] }
wd[county==LA & quarter_year==QY, gamma_invert := improve_it(gamma_invert)]
la <- wd[county==LA & quarter_year==QY]
anchor <- as.numeric(unlist(as.data.table(readRDS(counterfactual_data_path("14_wages_diffusion.rds",CONFIG)))[
  county==LA & quarter_year==QY & sol_type=="reorg", paste0("w",seq_len(n_w)), with=FALSE]))
new_theta <- matrix(market_parms[grep(paste0(LA,":avg_labor:B"),names(market_parms))],ncol=n_t,nrow=n_w,byrow=FALSE)
w_mat <- matrix(anchor,ncol=n_t,nrow=n_w,byrow=FALSE); tmp <- w_mat+(rho[LA])^(-1)*new_theta
cost_matrix <- sweep(tmp,2,apply(tmp,2,min))
make_A <- function(g){ A<-exp(-cost_matrix/g); A[A>=Inf]<-CONFIG$numeric_ceiling; A[A<=0]<-CONFIG$numeric_floor; A }
B_from <- function(A,alpha,E){ B<-t(t(A)*alpha/colSums(A*E))*E; B[abs(B)<CONFIG$B_zero_threshold]<-0; B }
mk_fx <- function(A,alpha) function(p){ pp<-pmax(p,CONFIG$numeric_floor); d<-pmax(colSums(A*pp),CONFIG$numeric_floor); pp*colSums(t(A)*alpha/d) }
mk_ob <- function(fx) function(p){ r<-fx(p)-p; -sum(r*r) }
resid_of <- function(fx,E){ r<-fx(E)-E; sqrt(sum(r*r)) }

sq <- function(A,alpha,maxit=1e6){ fx<-mk_fx(A,alpha); r<-squarem(rep(1/n_w,n_w),fixptfn=fx,control=list(maxiter=maxit,tol=1e-12))
  list(E=rowSums(B_from(A,alpha,r$par)), fpevals=r$fpevals, par=r$par) }

## gated solver: daarem first, fall back to SQUAREM if residual gate fails
gated <- function(A,alpha){
  fx<-mk_fx(A,alpha); ob<-mk_ob(fx)
  da<-tryCatch(daarem(rep(1/n_w,n_w),fixptfn=fx,objfn=ob,control=list(maxiter=2000,tol=1e-12)),error=function(e) NULL)
  if(!is.null(da)){
    rd<-resid_of(fx, da$par)
    if(is.finite(rd) && rd < GATE) return(list(E=rowSums(B_from(A,alpha,da$par)), fpevals=da$fpevals, used="daarem", resid=rd))
  }
  s<-sq(A,alpha)  # fallback
  list(E=s$E, fpevals=da_fpevals_safe(da)+s$fpevals, used="squarem_fallback", resid=resid_of(fx,s$par))
}
da_fpevals_safe <- function(da) if(is.null(da)||is.null(da$fpevals)) 0L else da$fpevals

res<-list()
for(i in seq_len(nrow(la))){
  alpha<-as.numeric(la[i,..task_mix_cols]); gamma<-la$gamma_invert[i]
  ge<-counterfactual_effective_gamma(gamma,CONFIG); if(!(is.finite(ge)&&ge>0)) next
  A<-make_A(ge)
  ref<-sq(A,alpha); g<-gated(A,alpha)
  res[[length(res)+1]]<-data.table(loc=substr(la$location_id[i],1,8), gamma=round(gamma,3),
    ref_fpevals=ref$fpevals, gated_fpevals=g$fpevals, used=g$used,
    daarem_resid=signif(g$resid,3), dE=max(abs(g$E-ref$E)))
}
out<-rbindlist(res,fill=TRUE); setorder(out,-dE)
cat("=== gated daarem (gate=",GATE,") vs SQUAREM-1e6, all ",nrow(out)," LA firms ===\n",sep="")
cat("Worst 6 by dE:\n"); print(head(out,6))
cat("\nused daarem: ",sum(out$used=="daarem"),"  fell back to SQUAREM: ",sum(out$used=="squarem_fallback"),"\n",sep="")
cat("firms with dE>1e-5: ",sum(out$dE>1e-5,na.rm=TRUE)," / ",nrow(out),"\n",sep="")
cat("max dE: ",signif(max(out$dE,na.rm=TRUE),3),"\n",sep="")
cat("total fpevals  SQUAREM-only: ",sum(out$ref_fpevals)," gated: ",sum(out$gated_fpevals),
    " (",round(sum(out$ref_fpevals)/max(sum(out$gated_fpevals),1),1),"x fewer)\n",sep="")
verdict<-if(max(out$dE,na.rm=TRUE)<1e-5) "PASS: gated solver matches SQUAREM-1e6 on ALL firms, with speedup" else "FAIL: still mismatches"
cat("\nVERDICT: ",verdict,"\n",sep="")
saveRDS(list(out=out,verdict=verdict,gate=GATE),"smoke_prototype_daarem_gated_inner.rds")
cat("Saved: smoke_prototype_daarem_gated_inner.rds\n")

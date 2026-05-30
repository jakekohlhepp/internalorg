## PROTOTYPE (final): daarem warm-started from a SHORT SQUAREM, to keep it in the
## interior basin (the assignment map has spurious corner fixed points that cold
## daarem jumps to). Backstop: if daarem's result has a near-zero share or its
## residual fails a gate, fall back to full SQUAREM. Must match SQUAREM-1e6 on
## ALL 53 LA firms.
##
## Output: smoke_prototype_daarem_warmstart_inner.rds (gitignored)
suppressPackageStartupMessages({ library(data.table); library(SQUAREM); library(daarem) })
source("config.R"); source("utils/counterfactuals_core.R")
n_w<-CONFIG$n_worker_types; n_t<-CONFIG$n_task_types; task_mix_cols<-get_task_mix_cols(CONFIG)
LA<-"6037"; QY<-get_counterfactual_focus_quarter()
GATE<-1e-9; EMIN<-1e-8   # residual gate; min share to call a solution "interior"
WARM<-300L               # short SQUAREM steps before daarem

ctx<-load_counterfactual_context(); wd<-copy(ctx$working_data); market_parms<-ctx$market_parms; rho<-ctx$rho
improve_it<-function(x){ r<-frank(x,ties.method="first"); sort(x)[pmax(r-1,1)] }
wd[county==LA & quarter_year==QY, gamma_invert:=improve_it(gamma_invert)]; la<-wd[county==LA & quarter_year==QY]
anchor<-as.numeric(unlist(as.data.table(readRDS(counterfactual_data_path("14_wages_diffusion.rds",CONFIG)))[
  county==LA & quarter_year==QY & sol_type=="reorg", paste0("w",seq_len(n_w)), with=FALSE]))
new_theta<-matrix(market_parms[grep(paste0(LA,":avg_labor:B"),names(market_parms))],ncol=n_t,nrow=n_w,byrow=FALSE)
w_mat<-matrix(anchor,ncol=n_t,nrow=n_w,byrow=FALSE); tmp<-w_mat+(rho[LA])^(-1)*new_theta; cost_matrix<-sweep(tmp,2,apply(tmp,2,min))
make_A<-function(g){ A<-exp(-cost_matrix/g); A[A>=Inf]<-CONFIG$numeric_ceiling; A[A<=0]<-CONFIG$numeric_floor; A }
B_from<-function(A,alpha,E){ B<-t(t(A)*alpha/colSums(A*E))*E; B[abs(B)<CONFIG$B_zero_threshold]<-0; B }
mk_fx<-function(A,alpha) function(p){ pp<-pmax(p,CONFIG$numeric_floor); d<-pmax(colSums(A*pp),CONFIG$numeric_floor); pp*colSums(t(A)*alpha/d) }
mk_ob<-function(fx) function(p){ r<-fx(p)-p; -sum(r*r) }
resid_of<-function(fx,E){ r<-fx(E)-E; sqrt(sum(r*r)) }
sq<-function(A,alpha,maxit){ fx<-mk_fx(A,alpha); squarem(rep(1/n_w,n_w),fixptfn=fx,control=list(maxiter=maxit,tol=1e-12)) }

solve_ref<-function(A,alpha){ r<-sq(A,alpha,1e6); list(E=rowSums(B_from(A,alpha,r$par)), fpevals=r$fpevals) }
solve_warm_daarem<-function(A,alpha){
  fx<-mk_fx(A,alpha); ob<-mk_ob(fx)
  ws<-sq(A,alpha,WARM)                       # short SQUAREM -> interior basin
  ev<-ws$fpevals
  da<-tryCatch(daarem(ws$par,fixptfn=fx,objfn=ob,control=list(maxiter=2000,tol=1e-12)),error=function(e) NULL)
  if(!is.null(da)){
    E<-da$par; rd<-resid_of(fx,E); Ef<-rowSums(B_from(A,alpha,E))
    if(is.finite(rd) && rd<GATE && all(Ef>EMIN))
      return(list(E=Ef, fpevals=ev+da$fpevals, used="warm+daarem"))
    ev<-ev+da$fpevals
  }
  s<-sq(A,alpha,1e6); list(E=rowSums(B_from(A,alpha,s$par)), fpevals=ev+s$fpevals, used="squarem_fallback")
}

res<-list()
for(i in seq_len(nrow(la))){
  alpha<-as.numeric(la[i,..task_mix_cols]); gamma<-la$gamma_invert[i]
  ge<-counterfactual_effective_gamma(gamma,CONFIG); if(!(is.finite(ge)&&ge>0)) next
  A<-make_A(ge); ref<-solve_ref(A,alpha); g<-solve_warm_daarem(A,alpha)
  res[[length(res)+1]]<-data.table(loc=substr(la$location_id[i],1,8),gamma=round(gamma,3),
    ref_fpevals=ref$fpevals, new_fpevals=g$fpevals, used=g$used, dE=max(abs(g$E-ref$E)))
}
out<-rbindlist(res,fill=TRUE); setorder(out,-dE)
cat("=== warm-start daarem (WARM=",WARM,") vs SQUAREM-1e6, all ",nrow(out)," firms ===\n",sep="")
cat("Worst 6 by dE:\n"); print(head(out,6))
cat("\nused warm+daarem: ",sum(out$used=="warm+daarem"),"  fell back: ",sum(out$used=="squarem_fallback"),"\n",sep="")
cat("firms dE>1e-5: ",sum(out$dE>1e-5,na.rm=TRUE)," / ",nrow(out),"   max dE: ",signif(max(out$dE,na.rm=TRUE),3),"\n",sep="")
cat("total fpevals SQUAREM: ",sum(out$ref_fpevals)," new: ",sum(out$new_fpevals),
    " (",round(sum(out$ref_fpevals)/max(sum(out$new_fpevals),1),1),"x fewer)\n",sep="")
verdict<-if(max(out$dE,na.rm=TRUE)<1e-5) "PASS: warm-start daarem matches SQUAREM-1e6 on ALL firms, with speedup" else "FAIL"
cat("\nVERDICT: ",verdict,"\n",sep="")
saveRDS(list(out=out,verdict=verdict,warm=WARM,gate=GATE,emin=EMIN),"smoke_prototype_daarem_warmstart_inner.rds")
cat("Saved.\n")

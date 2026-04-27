## compute counterfactuals
## Store only the essentials of the equilibrium:
### 1. Equilibrium wages (to get equilibrium again)
### 2. org Structures (because these require contraction to recover)
## 

## Compute the following objects
## 1. Consumer surplus
## 2. specialization (s-index)
## 3. Specialization (max fraction of time spent on one task)
## 4. sum of theta*B at each firm
## 5. marginal cost of each firm.

## Structure:
### First loop: wages
### Second loop: best-response pricing (contract on lambert's w). 
#### convergence not for sure. but if we end up converging, this is a best response to best responses.
### Third loop: best-response org structure. convergence guaranteed.
innertol<-1e-08
outertol<-1e-04

source("config.R")
source("utils/counterfactuals_core.R")

counterfactual_context <- load_counterfactual_context()
working_data <- counterfactual_context$working_data
initial_wages <- counterfactual_context$initial_wages
all_results <- counterfactual_context$all_results
market_parms <- counterfactual_context$market_parms
total_labor <- counterfactual_context$total_labor
total_labor_orig <- counterfactual_context$total_labor_orig
initial_guess <- counterfactual_context$initial_guess
rho <- counterfactual_context$rho
tild_theta <- counterfactual_context$tild_theta
spec_log<-function(x)  ifelse(x==0 | is.nan(x),0,log(x))

get_everything<-function(wage_guess, cnty, qy){
  
  counter_res<-copy(working_data[county==cnty & quarter_year==qy, c("location_id","county","quarter_year","gamma_invert","avg_labor", "task_mix_1",
                                                                    "task_mix_2", "task_mix_3", "task_mix_4", "task_mix_5",
                                                                    "qual_exo", "cost_exo","weight", "cust_price",
                                                                    "CSPOP")])
  
  ## create the skill sets matrix (theta), wage vectors (wage_guess), and wage-adjusted skills (new_tild_theta)
  new_theta<-matrix(market_parms[grep(paste0(cnty,":avg_labor:B"),names(market_parms))], ncol=5, nrow=5, byrow=FALSE)
  w_mat<-matrix(wage_guess, ncol=5, nrow=5, byrow=FALSE)
  new_tild_theta<-w_mat+(rho[cnty])^(-1)*new_theta
  new_tild_theta<-sweep(new_tild_theta,2,apply(new_tild_theta,2,min))
  
  ## solve internal org
  
  
  solve_org<-Vectorize(function(a1, a2, a3, a4, a5,gamma){
    alpha<-c(a1, a2, a3, a4,a5)
    if (is.finite(gamma) & gamma>0){
      
      alpha<-c(a1, a2, a3, a4,a5)
      ## this function will return matrix given gamma
      A<-exp(-1/gamma*(new_tild_theta) )
      E<-rep(0.2, 5)
      A[A>=Inf]<-1e16
      A[A<=0]<-1e-16
      fxpt<-function(p){
        C<-colSums(t(A)*alpha/colSums(A*p))
        return(p*C)
      }
      #for (i in 1:1000000){
      # E_old<-E
      # E<-fxpt(E_old)
      #if (all(abs(E-E_old)<innertol)) break
      #}
      E<-squarem(E,fixptfn = fxpt, control=list(maxiter=100000,tol=innertol) )$par
      
      B<-t(t(A)*alpha/colSums(A*E))*E
    } else if (gamma==0){
      # no frictions
      B<-matrix(0, ncol=5, nrow=5)
      for (col in 1:5){
        B[which.min(new_tild_theta[,col]),col]<-alpha[col]
      }
    } else{
      # max frictions
      B<-matrix(0, ncol=5, nrow=5)
      B[which.min(rowSums(t(t(new_tild_theta)*alpha ))),]<-alpha
    }
    E<-rowSums(B)
    B[abs(B)<1e-16]<-0
    Brel<-t(t(B/E)/alpha)
    ## compute endogenous cost and quality components.
    # cost is wages plus org cost.
    cendog<-sum(E*wage_guess)+ifelse(is.finite(gamma),gamma*sum(B*spec_log(Brel)),
                                     0)
    qendog<-sum(B*new_theta)
    return(list(c_endog=cendog,q_endog=qendog,s_index=sum(B*spec_log(Brel)),
                B_1_1=B[1,1], B_1_2=B[2,1], B_1_3=B[3,1], B_1_4=B[4,1], B_1_5=B[5,1],
                B_2_1=B[1,2], B_2_2=B[2,2], B_2_3=B[3,2], B_2_4=B[4,2], B_2_5=B[5,2],
                B_3_1=B[1,3], B_3_2=B[2,3], B_3_3=B[3,3], B_3_4=B[4,3], B_3_5=B[5,3],
                B_4_1=B[1,4], B_4_2=B[2,4], B_4_3=B[3,4], B_4_4=B[4,4], B_4_5=B[5,4],
                B_5_1=B[1,5], B_5_2=B[2,5], B_5_3=B[3,5], B_5_4=B[4,5], B_5_5=B[5,5]))
  })
  
  counter_res[, c("c_endog", "q_endog","s_index",
                  "B_1_1", "B_1_2", "B_1_3", "B_1_4", "B_1_5",
                  "B_2_1", "B_2_2", "B_2_3", "B_2_4", "B_2_5",
                  "B_3_1", "B_3_2", "B_3_3", "B_3_4", "B_3_5",
                  "B_4_1", "B_4_2", "B_4_3", "B_4_4", "B_4_5",
                  "B_5_1", "B_5_2", "B_5_3", "B_5_4", "B_5_5"):= (solve_org(task_mix_1, task_mix_2, task_mix_3, task_mix_4,task_mix_5,
                                                                            gamma_invert)),by=c("location_id")]
  counter_res[, Q:=q_endog*avg_labor+qual_exo]
  counter_res[, C:=c_endog*avg_labor+cost_exo]
  # do not allow negative costs.
  counter_res[C<0, C:=0]
  
  # be careful about sign
  # in original draft, rho is positive.
  # in new draft, rho is negative.
  best_respond<-function(p0, Q,C, wgt){
    old_p<-p0
    for (i in 1:10000000){
      new_p<- -1/rho[cnty]+C-lambertW0(exp(-1+Q+rho[cnty]*C)/(1+sum(wgt*exp(Q+rho[cnty]*old_p))-exp(Q+rho[cnty]*old_p)) )/rho[cnty]
      if (all(abs(new_p-old_p)<outertol)) break
      old_p<-new_p
    }
    return(new_p)
  }
  
  
  counter_res[, newprice:=best_respond(cust_price, Q,C,weight)]
  counter_res[, new_share:=exp(Q+rho[cnty]*newprice)]
  counter_res[,new_share:=new_share/(sum(weight*new_share)+1)]
  
  return(counter_res)
  
}

orig_struct <- build_counterfactual_structure_snapshot(get_everything, initial_wages)


### single diffusion improvement
improve_it<-function(x){
  frank(x, ties.method="first")->rankhold
  c(sort(x)[pmax(rankhold-1,1)])
}
orig_data<-copy(working_data)
res_wages<-new_counterfactual_wages_grid(unique(total_labor_orig$quarter_year))

## single improvement in management
working_data[, gamma_invert:=improve_it(gamma_invert), by=c("county", "quarter_year")]

solve_reloc<-function(wage_guess){
  
  counter_res<-copy(working_data[county==cnty & quarter_year==qy, c("location_id","county","quarter_year","gamma_invert","avg_labor", "task_mix_1",
                                                                    "task_mix_2", "task_mix_3", "task_mix_4", "task_mix_5",
                                                                    "qual_exo", "cost_exo","weight", "cust_price",
                                                                    "CSPOP", "E_1", "E_2", "E_3", "E_4", "E_5")])
  
  ## create the skill sets matrix (theta), wage vectors (wage_guess), and wage-adjusted skills (new_tild_theta)
  ## swepe ere, because it improve snumericla performance. undo when calcing quality.
  new_theta<-matrix(market_parms[grep(paste0(cnty,":avg_labor:B"),names(market_parms))], ncol=5, nrow=5, byrow=FALSE)
  w_mat<-matrix(wage_guess, ncol=5, nrow=5, byrow=FALSE)
  new_tild_theta<-w_mat+(rho[cnty])^(-1)*new_theta
  new_tild_theta<-sweep(new_tild_theta,2,apply(new_tild_theta,2,min))
  
  ## solve internal org
  
  
  solve_org<-Vectorize(function(loc, gamma){
    B<-matrix(as.matrix(orig_struct[[cnty]][location_id==loc,.SD, .SDcols=grep("^B_", colnames(orig_struct[[cnty]]))]), byrow=FALSE,nrow=5, ncol=5)
    E<-rowSums(B)
    Brel<-t(t(B/E)/colSums(B))
    ## compute endogenous cost and quality components.
    # cost is wages plus org cost.
    cendog<-sum(E*wage_guess)+ifelse(is.finite(gamma),gamma*sum(B*spec_log(Brel)),
                                     0)
    qendog<-sum(B*new_theta)
    return(list(c_endog=cendog,q_endog=qendog,E_1=E[1],
                E_2=E[2], E_3=E[3], E_4=E[4], E_5=E[5]))
  })
  
  counter_res[, c("c_endog", "q_endog",
                  "E_1", "E_2", "E_3", "E_4", "E_5"):= (solve_org(location_id, gamma_invert)),by=c("location_id")]
  counter_res[, Q:=q_endog*avg_labor+qual_exo]
  counter_res[, C:=c_endog*avg_labor+cost_exo]
  # do not allow negative costs.
  counter_res[C<0, C:=0]
  # be careful about sign
  # in original draft, rho is positive.
  # in new draft, rho is negative.
  best_respond<-function(p0, Q,C, wgt){
    old_p<-p0
    for (i in 1:10000000){
      new_p<- -1/rho[cnty]+C-lambertW0(exp(-1+Q+rho[cnty]*C)/(1+sum(wgt*exp(Q+rho[cnty]*old_p))-exp(Q+rho[cnty]*old_p)) )/rho[cnty]
      if (all(abs(new_p-old_p)<outertol)) break
      old_p<-new_p
    }
    return(new_p)
  }
  
  ## from conlon
  old_p<-counter_res$cust_price
  #for (i in 1:10000000){
  #  temp_shares<- exp(counter_res$Q+rho[cnty]*old_p)
  # temp_shares<-temp_shares/sum(counter_res$weight*temp_shares)
  # big_G<- rho[cnty]*matrix(temp_shares, ncol=length(temp_shares), nrow=length(temp_shares), byrow=FALSE)*matrix(temp_shares, ncol=length(temp_shares), nrow=length(temp_shares), byrow=TRUE)
  #big_Tri<-rho[cnty]*diag(temp_shares)
  #new_p<-as.numeric(counter_res$C + solve(big_Tri)%*%t(diag(x=1, nrow=nrow(counter_res), ncol=nrow(counter_res))*big_G)%*%(old_p-counter_res$C)-solve(big_Tri)%*%temp_shares)
  
  # if (all(abs(new_p-old_p)<innertol)) break
  #old_p<-new_p
  #}
  #counter_res[, newprice:=new_p]
  counter_res[, newprice:=best_respond(cust_price, Q,C,weight)]
  
  
  counter_res[, new_share:=exp(Q+rho[cnty]*newprice)]
  counter_res[,new_share:=new_share/(sum(weight*new_share)+1)]
  
  new_total_labor<-counter_res[, .(tot_1=sum(weight*new_share*CSPOP*E_1*avg_labor),tot_2=sum(weight*new_share*CSPOP*E_2*avg_labor),
                                   tot_3=sum(weight*new_share*CSPOP*E_3*avg_labor),tot_4=sum(weight*new_share*CSPOP*E_4*avg_labor),
                                   tot_5=sum(weight*new_share*CSPOP*E_5*avg_labor))]
  
  stopifnot(nrow(new_total_labor)==1)
  names_mat<-copy(new_total_labor)
  names_mat[, tot_1:=paste0(cnty,"-", qy, "-", "1")]
  names_mat[, tot_2:=paste0(cnty,"-", qy, "-", "2")]
  names_mat[, tot_3:=paste0(cnty,"-", qy, "-", "3")]
  names_mat[, tot_4:=paste0(cnty,"-", qy, "-", "4")]
  names_mat[, tot_5:=paste0(cnty,"-", qy, "-", "5")]
  labor_clearing<-as.numeric(as.matrix(new_total_labor-total_labor[county==cnty & quarter_year==qy ,-c("county", "quarter_year")]))
  names_clearing<-as.character(as.matrix(names_mat))
  #labor_clearing<-sum(as.matrix(new_total_labor-total_labor[county==cnty & quarter_year==qy ,-c("county", "quarter_year")])^2)
  
  names(labor_clearing)<-names_clearing
  
  return(labor_clearing)
}
for (cnty in c('36061','17031', '6037') ){
  for (qy in 2021.2 ){
    print(paste("*******",cnty, qy, " -  First Try"))
    quad_wages<-BBsolve(as.numeric(initial_wages[county==cnty & quarter_year==qy, c("w1", "w2", "w3", "w4", "w5")]),
                        solve_reloc, control=list(maxit=10000,trace=TRUE, NM=TRUE, noimp=200, tol=300000, M=c(10,20,5,50)))
    quad_wages<-BBsolve(quad_wages$par,
                        solve_reloc, control=list(maxit=10000,trace=TRUE, NM=TRUE, noimp=200, tol=100000, M=c(10,2,200,100,15)))
    quad_wages<-BBsolve(quad_wages$par,
                        solve_reloc, control=list(maxit=10000,trace=TRUE, NM=TRUE, noimp=200, tol=1000, M=c(10,2,200,100,15)))
    quad_wages<-BBsolve(quad_wages$par,
                        solve_reloc, control=list(maxit=10000,trace=TRUE, NM=TRUE, noimp=200, tol=1, M=c(10,2,200,100,15)))
    res_wages[quarter_year==qy & county==cnty & sol_type=="realloc", c("w1", "w2", "w3", "w4", "w5"):= as.list(quad_wages$par)]
    res_wages[quarter_year==qy & county==cnty& sol_type=="realloc", fval:=quad_wages$residual]
    
  }
}

## one function for full solution
solve_wages<-function(wage_guess){
  
  counter_res<-copy(working_data[county==cnty & quarter_year==qy, c("location_id","county","quarter_year","gamma_invert","avg_labor", "task_mix_1",
                                                                    "task_mix_2", "task_mix_3", "task_mix_4", "task_mix_5",
                                                                    "qual_exo", "cost_exo","weight", "cust_price",
                                                                    "CSPOP")])
  
  ## create the skill sets matrix (theta), wage vectors (wage_guess), and wage-adjusted skills (new_tild_theta)
  ## swepe ere, because it improve snumericla performance. undo when calcing quality.
  new_theta<-matrix(market_parms[grep(paste0(cnty,":avg_labor:B"),names(market_parms))], ncol=5, nrow=5, byrow=FALSE)
  w_mat<-matrix(wage_guess, ncol=5, nrow=5, byrow=FALSE)
  new_tild_theta<-w_mat+(rho[cnty])^(-1)*new_theta
  new_tild_theta<-sweep(new_tild_theta,2,apply(new_tild_theta,2,min))
  
  ## solve internal org
  
  
  solve_org<-Vectorize(function(a1, a2, a3, a4, a5,gamma){
    alpha<-c(a1, a2, a3, a4,a5)
    if (is.finite(gamma) & gamma>0){
      
      alpha<-c(a1, a2, a3, a4,a5)
      ## this function will return matrix given gamma
      A<-exp(-1/gamma*(new_tild_theta) )
      E<-rep(0.2, 5)
      A[A>=Inf]<-1e16
      A[A<=0]<-1e-16
      fxpt<-function(p){
        C<-colSums(t(A)*alpha/colSums(A*p))
        return(p*C)
      }
      #for (i in 1:1000000){
      #E_old<-E
      #E<-fxpt(E_old)
      #if (all(abs(E-E_old)<innertol)) break
      #}
      E<-squarem(E,fixptfn = fxpt, control=list(maxiter=100000,tol=innertol) )$par
      
      B<-t(t(A)*alpha/colSums(A*E))*E
    } else if (gamma==0){
      # no frictions
      B<-matrix(0, ncol=5, nrow=5)
      for (col in 1:5){
        B[which.min(new_tild_theta[,col]),col]<-alpha[col]
      }
    } else{
      # max frictions
      B<-matrix(0, ncol=5, nrow=5)
      B[which.min(rowSums(t(t(new_tild_theta)*alpha ))),]<-alpha
    }
    E<-rowSums(B)
    B[abs(B)<1e-16]<-0
    Brel<-t(t(B/E)/alpha)
    ## compute endogenous cost and quality components.
    # cost is wages plus org cost.
    cendog<-sum(E*wage_guess)+ifelse(is.finite(gamma),gamma*sum(B*spec_log(Brel)),
                                     0)
    qendog<-sum(B*new_theta)
    return(list(c_endog=cendog,q_endog=qendog,E_1=E[1],
                E_2=E[2], E_3=E[3], E_4=E[4], E_5=E[5]))
  })
  
  counter_res[, c("c_endog", "q_endog",
                  "E_1", "E_2", "E_3", "E_4", "E_5"):= (solve_org(task_mix_1, task_mix_2, task_mix_3, task_mix_4,task_mix_5,
                                                                  gamma_invert)),by=c("location_id")]
  counter_res[, Q:=q_endog*avg_labor+qual_exo]
  counter_res[, C:=c_endog*avg_labor+cost_exo]
  # do not allow negative costs.
  counter_res[C<0, C:=0]
  # be careful about sign
  # in original draft, rho is positive.
  # in new draft, rho is negative.
  best_respond<-function(p0, Q,C, wgt){
    old_p<-p0
    for (i in 1:10000000){
      new_p<- -1/rho[cnty]+C-lambertW0(exp(-1+Q+rho[cnty]*C)/(1+sum(wgt*exp(Q+rho[cnty]*old_p))-exp(Q+rho[cnty]*old_p)) )/rho[cnty]
      if (all(abs(new_p-old_p)<outertol)) break
      old_p<-new_p
    }
    return(new_p)
  }
  
  ## from conlon
  old_p<-counter_res$cust_price
  #for (i in 1:10000000){
  #  temp_shares<- exp(counter_res$Q+rho[cnty]*old_p)
  # temp_shares<-temp_shares/sum(counter_res$weight*temp_shares)
  # big_G<- rho[cnty]*matrix(temp_shares, ncol=length(temp_shares), nrow=length(temp_shares), byrow=FALSE)*matrix(temp_shares, ncol=length(temp_shares), nrow=length(temp_shares), byrow=TRUE)
  #big_Tri<-rho[cnty]*diag(temp_shares)
  #new_p<-as.numeric(counter_res$C + solve(big_Tri)%*%t(diag(x=1, nrow=nrow(counter_res), ncol=nrow(counter_res))*big_G)%*%(old_p-counter_res$C)-solve(big_Tri)%*%temp_shares)
  
  # if (all(abs(new_p-old_p)<innertol)) break
  #old_p<-new_p
  #}
  #counter_res[, newprice:=new_p]
  counter_res[, newprice:=best_respond(cust_price, Q,C,weight)]
  
  
  counter_res[, new_share:=exp(Q+rho[cnty]*newprice)]
  counter_res[,new_share:=new_share/(sum(weight*new_share)+1)]
  
  new_total_labor<-counter_res[, .(tot_1=sum(weight*new_share*CSPOP*E_1*avg_labor),tot_2=sum(weight*new_share*CSPOP*E_2*avg_labor),
                                   tot_3=sum(weight*new_share*CSPOP*E_3*avg_labor),tot_4=sum(weight*new_share*CSPOP*E_4*avg_labor),
                                   tot_5=sum(weight*new_share*CSPOP*E_5*avg_labor))]
  
  stopifnot(nrow(new_total_labor)==1)
  names_mat<-copy(new_total_labor)
  names_mat[, tot_1:=paste0(cnty,"-", qy, "-", "1")]
  names_mat[, tot_2:=paste0(cnty,"-", qy, "-", "2")]
  names_mat[, tot_3:=paste0(cnty,"-", qy, "-", "3")]
  names_mat[, tot_4:=paste0(cnty,"-", qy, "-", "4")]
  names_mat[, tot_5:=paste0(cnty,"-", qy, "-", "5")]
  labor_clearing<-as.numeric(as.matrix(new_total_labor-total_labor[county==cnty & quarter_year==qy ,-c("county", "quarter_year")]))
  names_clearing<-as.character(as.matrix(names_mat))
  #labor_clearing<-sum(as.matrix(new_total_labor-total_labor[county==cnty & quarter_year==qy ,-c("county", "quarter_year")])^2)
  
  names(labor_clearing)<-names_clearing
  
  return(labor_clearing)
}

for (cnty in c('36061','17031') ){
  for (qy in 2021.2 ){
    print(paste("*******DIFFUSION***",cnty, qy, " -  First Try"))
    quad_wages<-BBsolve(as.numeric(initial_wages[county==cnty & quarter_year==qy, c("w1", "w2", "w3", "w4", "w5")]),
                        solve_wages, control=list(maxit=10000,trace=TRUE, NM=FALSE, noimp=200, tol=1000, M=c(100,10,20,5,50)))
    quad_wages<-broyden(solve_wages, quad_wages$par,maxiter=1000 )
    quad_wages<-BBsolve(quad_wages$zero,
                        solve_wages, control=list(maxit=10000,trace=TRUE, NM=FALSE, noimp=200, tol=1, M=c(10,2,200,100,15)))
    
    res_wages[quarter_year==qy & county==cnty & sol_type=="reorg", c("w1", "w2", "w3", "w4", "w5"):= as.list(quad_wages$par)]
    res_wages[quarter_year==qy & county==cnty& sol_type=="reorg", fval:=quad_wages$residual]
    
  }
}

for (cnty in c('6037') ){
  for (qy in 2021.2 ){
    print(paste("*******DIFFUSION***",cnty, qy, " -  First Try"))
    quad_wages<-BBsolve(as.numeric(res_wages[county==cnty & quarter_year==qy &sol_type=="realloc", c("w1", "w2", "w3", "w4", "w5")]),
                        solve_wages, control=list(maxit=10000,trace=TRUE, NM=FALSE, noimp=200, tol=500000, M=c(10,2,200,100,15)))
    quad_wages<-BBsolve(quad_wages$par,
                        solve_wages, control=list(maxit=10000,trace=TRUE, NM=FALSE, noimp=200, tol=200000, M=c(10,2,200,100,15)))
    
    quad_wages<-BBsolve(quad_wages$par,
                        solve_wages, control=list(maxit=10000,trace=TRUE, NM=TRUE, noimp=200, tol=100000, M=c(10,2,15,5,20)))
    quad_wages<-BBsolve(quad_wages$par,
                        solve_wages, control=list(maxit=10000,trace=TRUE, NM=TRUE, noimp=200, tol=50000, M=c(10,2,15,5,20)))
    quad_wages<-BBsolve(quad_wages$par,
                        solve_wages, control=list(maxit=10000,trace=TRUE, NM=TRUE, noimp=200, tol=20000, M=c(10,2,15,5,20)))
    quad_wages<-BBsolve(quad_wages$par,
                        solve_wages, control=list(maxit=10000,trace=TRUE, NM=TRUE, noimp=200, tol=10000, M=c(10,2,15,5,20)))
    quad_wages<-BBsolve(quad_wages$par,
                        solve_wages, control=list(maxit=10000,trace=TRUE, NM=TRUE, noimp=200, tol=5000, M=c(10,2,15,5,20)))
    quad_wages<-BBsolve(quad_wages$par,
                        solve_wages, control=list(maxit=10000,trace=TRUE, NM=TRUE, noimp=200, tol=100, M=c(10,2,15,5,20)))
    quad_wages<-BBsolve(quad_wages$par,
                        solve_wages, control=list(maxit=10000,trace=TRUE, NM=TRUE, noimp=200, tol=1, M=c(10,2,15,5,20)))
    quad_wages<-broyden(solve_wages, quad_wages$par,maxiter=10000 )
    quad_wages<-BBsolve(quad_wages$zero,
                        solve_wages, control=list(maxit=10000,trace=TRUE, NM=TRUE, noimp=200, tol=1, M=c(100,2,15,5,20)))
    res_wages[quarter_year==qy & county==cnty & sol_type=="reorg", c("w1", "w2", "w3", "w4", "w5"):= as.list(quad_wages$par)]
    res_wages[quarter_year==qy & county==cnty& sol_type=="reorg", fval:=quad_wages$residual]
    
  }
}

save_counterfactual_rds(
  res_wages,
  "05_02_wages_diffusion.rds",
  legacy_filename = "05_02_wages_diffusion.rds"
)


get_prod<-function(wage_guess, cnty, qy,stype){
  
  counter_res<-copy(working_data[county==cnty & quarter_year==qy, c("location_id","county","quarter_year","gamma_invert","avg_labor", "task_mix_1",
                                                                    "task_mix_2", "task_mix_3", "task_mix_4", "task_mix_5",
                                                                    "qual_exo", "cost_exo","weight", "cust_price",
                                                                    "CSPOP")])
  
  ## create the skill sets matrix (theta), wage vectors (wage_guess), and wage-adjusted skills (new_tild_theta)
  new_theta<-matrix(market_parms[grep(paste0(cnty,":avg_labor:B"),names(market_parms))], ncol=5, nrow=5, byrow=FALSE)
  w_mat<-matrix(wage_guess, ncol=5, nrow=5, byrow=FALSE)
  new_tild_theta<-w_mat+(rho[cnty])^(-1)*new_theta
  new_tild_theta<-sweep(new_tild_theta,2,apply(new_tild_theta,2,min))
  
  ## solve internal org
  
  if (stype=="reorg"){
    solve_org<-Vectorize(function(a1, a2, a3, a4, a5,gamma){
      alpha<-c(a1, a2, a3, a4,a5)
      if (is.finite(gamma) & gamma>0){
        
        alpha<-c(a1, a2, a3, a4,a5)
        ## this function will return matrix given gamma
        A<-exp(-1/gamma*(new_tild_theta) )
        E<-rep(0.2, 5)
        A[A>=Inf]<-1e16
        A[A<=0]<-1e-16
        fxpt<-function(p){
          C<-colSums(t(A)*alpha/colSums(A*p))
          return(p*C)
        }
        #for (i in 1:1000000){
        # E_old<-E
        # E<-fxpt(E_old)
        #if (all(abs(E-E_old)<innertol)) break
        #}
        E<-squarem(E,fixptfn = fxpt, control=list(maxiter=100000,tol=innertol) )$par
        
        B<-t(t(A)*alpha/colSums(A*E))*E
      } else if (gamma==0){
        # no frictions
        B<-matrix(0, ncol=5, nrow=5)
        for (col in 1:5){
          B[which.min(new_tild_theta[,col]),col]<-alpha[col]
        }
      } else{
        # max frictions
        B<-matrix(0, ncol=5, nrow=5)
        B[which.min(rowSums(t(t(new_tild_theta)*alpha ))),]<-alpha
      }
      E<-rowSums(B)
      B[abs(B)<1e-16]<-0
      Brel<-t(t(B/E)/alpha)
      ## compute endogenous cost and quality components.
      # cost is wages plus org cost.
      cendog<-sum(E*wage_guess)+ifelse(is.finite(gamma),gamma*sum(B*spec_log(Brel)),
                                       0)
      qendog<-sum(B*new_theta)
      s_hold<-sum(B*spec_log(Brel))
      ## for this only
      swept_theta<-sweep(new_theta,2,apply(new_theta,2,min))
      B<- B*swept_theta
      return(list(c_endog=cendog,q_endog=qendog,s_index=s_hold,
                  B_1_1=B[1,1], B_1_2=B[2,1], B_1_3=B[3,1], B_1_4=B[4,1], B_1_5=B[5,1],
                  B_2_1=B[1,2], B_2_2=B[2,2], B_2_3=B[3,2], B_2_4=B[4,2], B_2_5=B[5,2],
                  B_3_1=B[1,3], B_3_2=B[2,3], B_3_3=B[3,3], B_3_4=B[4,3], B_3_5=B[5,3],
                  B_4_1=B[1,4], B_4_2=B[2,4], B_4_3=B[3,4], B_4_4=B[4,4], B_4_5=B[5,4],
                  B_5_1=B[1,5], B_5_2=B[2,5], B_5_3=B[3,5], B_5_4=B[4,5], B_5_5=B[5,5]))
    })
    
    counter_res[, c("c_endog", "q_endog","s_index",
                    "B_1_1", "B_1_2", "B_1_3", "B_1_4", "B_1_5",
                    "B_2_1", "B_2_2", "B_2_3", "B_2_4", "B_2_5",
                    "B_3_1", "B_3_2", "B_3_3", "B_3_4", "B_3_5",
                    "B_4_1", "B_4_2", "B_4_3", "B_4_4", "B_4_5",
                    "B_5_1", "B_5_2", "B_5_3", "B_5_4", "B_5_5"):= (solve_org(task_mix_1, task_mix_2, task_mix_3, task_mix_4,task_mix_5,
                                                                              gamma_invert)),by=c("location_id")]
    
    
  } else if (stype=="realloc"){
    solve_org<-Vectorize(function(loc, gamma){
      B<-matrix(as.matrix(orig_struct[[cnty]][location_id==loc,.SD, .SDcols=grep("^B_", colnames(orig_struct[[cnty]]))]), byrow=FALSE,nrow=5, ncol=5)
      E<-rowSums(B)
      Brel<-t(t(B/E)/colSums(B))
      ## compute endogenous cost and quality components.
      # cost is wages plus org cost.
      cendog<-sum(E*wage_guess)+ifelse(is.finite(gamma),gamma*sum(B*spec_log(Brel)),
                                       0)
      qendog<-sum(B*new_theta)
      s_hold<-sum(B*spec_log(Brel))
      swept_theta<-sweep(new_theta,2,apply(new_theta,2,min))
      
      B<- B*swept_theta
      
      return(list(c_endog=cendog,q_endog=qendog,s_index=s_hold,
                  B_1_1=B[1,1], B_1_2=B[2,1], B_1_3=B[3,1], B_1_4=B[4,1], B_1_5=B[5,1],
                  B_2_1=B[1,2], B_2_2=B[2,2], B_2_3=B[3,2], B_2_4=B[4,2], B_2_5=B[5,2],
                  B_3_1=B[1,3], B_3_2=B[2,3], B_3_3=B[3,3], B_3_4=B[4,3], B_3_5=B[5,3],
                  B_4_1=B[1,4], B_4_2=B[2,4], B_4_3=B[3,4], B_4_4=B[4,4], B_4_5=B[5,4],
                  B_5_1=B[1,5], B_5_2=B[2,5], B_5_3=B[3,5], B_5_4=B[4,5], B_5_5=B[5,5]))
    })
    
    counter_res[, c("c_endog", "q_endog","s_index",
                    "B_1_1", "B_1_2", "B_1_3", "B_1_4", "B_1_5",
                    "B_2_1", "B_2_2", "B_2_3", "B_2_4", "B_2_5",
                    "B_3_1", "B_3_2", "B_3_3", "B_3_4", "B_3_5",
                    "B_4_1", "B_4_2", "B_4_3", "B_4_4", "B_4_5",
                    "B_5_1", "B_5_2", "B_5_3", "B_5_4", "B_5_5"):= (solve_org(location_id, gamma_invert)),by=c("location_id")]
    
  } else{
    stopifnot(FALSE)
  }
  
  
  
  
  counter_res[, Q:=q_endog*avg_labor+qual_exo]
  counter_res[, C:=c_endog*avg_labor+cost_exo]
  # do not allow negative costs.
  counter_res[C<0, C:=0]
  
  # be careful about sign
  # in original draft, rho is positive.
  # in new draft, rho is negative.
  best_respond<-function(p0, Q,C, wgt){
    old_p<-p0
    for (i in 1:10000000){
      new_p<- -1/rho[cnty]+C-lambertW0(exp(-1+Q+rho[cnty]*C)/(1+sum(wgt*exp(Q+rho[cnty]*old_p))-exp(Q+rho[cnty]*old_p)) )/rho[cnty]
      if (all(abs(new_p-old_p)<outertol)) break
      old_p<-new_p
    }
    return(new_p)
  }
  
  
  counter_res[, newprice:=best_respond(cust_price, Q,C,weight)]
  counter_res[, new_share:=exp(Q+rho[cnty]*newprice)]
  counter_res[,new_share:=new_share/(sum(weight*new_share)+1)]
  
  return(counter_res)
  
}


prod_data_new<-data.table()
for (cnty in unique(res_wages[is.finite(w1),]$county)){
  for (qy in unique(res_wages[is.finite(w1),]$quarter_year)){
    for (sol in unique(res_wages[is.finite(w1),]$sol_type)){
      prod_data_new<-rbind(prod_data_new, data.table(county=cnty, quarter_year=qy, sol_type=sol,get_prod(as.numeric(res_wages[county==cnty & quarter_year==qy & sol_type==sol , c("w1", "w2", "w3", "w4", "w5")]),
                                                                                                         cnty, qy,sol) ))
    }
  }
}
save_counterfactual_rds(
  prod_data_new,
  "05_02_prod_diffusion.rds",
  legacy_filename = "05_02_prod_diffusion.rds"
)

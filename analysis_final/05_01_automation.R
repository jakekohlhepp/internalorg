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


library('data.table')
library('lubridate')
library('rootSolve')
library('stringr')
library('lamW')
library('pracma')
library('BB')
library('SQUAREM')
spec_log<-function(x)  ifelse(x==0 | is.nan(x),0,log(x))


## market parameters
working_data<-readRDS("analysis_final/data/05_00_working_data.rds")
initial_wages<-readRDS("analysis_final/data/05_00_initial_wages.rds")


all_results<-readRDS('analysis_final/data/02_00_parameters.rds')

market_parms<-all_results$coefficients
names(market_parms)<-all_results$parm_name

# labor demand is then weights times market share times avg_labor times cspop
total_labor<-working_data[, .(tot_1=sum(weight*salon_share_subdiv*CSPOP*E_1*avg_labor),tot_2=sum(weight*salon_share_subdiv*CSPOP*E_2*avg_labor),
                              tot_3=sum(weight*salon_share_subdiv*CSPOP*E_3*avg_labor),tot_4=sum(weight*salon_share_subdiv*CSPOP*E_4*avg_labor),
                              tot_5=sum(weight*salon_share_subdiv*CSPOP*E_5*avg_labor)), by=c("county", "quarter_year")]
total_labor_orig<-copy(total_labor)


initial_guess<-vector(mode='numeric', length=uniqueN(working_data[,c("quarter_year", "county")]))
lab_wages<-vector(mode='character', length=uniqueN(working_data[,c("quarter_year", "county")])) 
rho<-vector(mode='numeric',3)
names(rho)<-c("17031", "36061", "6037")
i=1
for (cnty in c("17031", "36061", "6037")){
  rho[cnty]<-market_parms[grep(paste0(cnty,":cust_price$"),names(market_parms))]
  for (qy in as.character(unique(working_data$quarter_year))){
    initial_guess[i:(i+4)]<-c(0,market_parms[grep(paste0(cnty,":avg_labor:E"),names(market_parms))])+
      market_parms[paste0("avg_labor:","factor(county)", cnty,":factor(quarter_year)", qy)]
    lab_wages[i:(i+4)]<-paste0(cnty,"-", qy, "-",i:(i+4)-i+1)
    i<-i+5
  }
}
names(initial_guess)<-lab_wages

## wage adjusted skills


tild_theta<-vector(mode='list', length=3)
names(tild_theta)<-list("17031", "36061", "6037")
for (cnty in names(tild_theta)){
  tild_theta[[cnty]]<-vector(mode='list', length=12)
  names(tild_theta[[cnty]])<-unique(working_data$quarter_year)
}
## be careful - the sweep command helps numerically but needs to be undone for some outcomes
for (cnty in names(tild_theta)){
  for (qy in names(tild_theta[[cnty]])){
    w_mat<-matrix(c(0,market_parms[grep(paste0(cnty,":avg_labor:E"),names(market_parms))]), ncol=5, nrow=5, byrow=FALSE)
    ## add in base wage
    w_mat<-w_mat+market_parms[paste0("avg_labor:factor(county)",cnty,":factor(quarter_year)",qy)]
    skills<-matrix(market_parms[grep(paste0(cnty,":avg_labor:B"),names(market_parms))], ncol=5, nrow=5, byrow=FALSE)
    tild_theta[[cnty]][[qy]]<-w_mat+(rho[cnty])^(-1)*skills
    tild_theta[[cnty]][[qy]]<-sweep(tild_theta[[cnty]][[qy]],2,apply(tild_theta[[cnty]][[qy]],2,min))
    
  }
}

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

orig_struct<-vector(mode='list', length=3)
names(orig_struct)<-list("17031", "36061", "6037")

orig_struct[['36061']]<-get_everything(as.numeric(initial_wages[county=='36061' & quarter_year==2021.2, c("w1", "w2", "w3", "w4", "w5")]),
                                       '36061', 2021.2)
orig_struct[['6037']]<-get_everything(as.numeric(initial_wages[county=='6037' & quarter_year==2021.2, c("w1", "w2", "w3", "w4", "w5")]),
                                      '6037', 2021.2)
orig_struct[['17031']]<-get_everything(as.numeric(initial_wages[county=='17031' & quarter_year==2021.2, c("w1", "w2", "w3", "w4", "w5")]),
                                       '17031', 2021.2)

### automation
## automation is as good as the best worker.
## automation is free. so it just adds to quality but does not ad to cost.
orig_data<-copy(working_data)
res_wages<-data.table(expand.grid(county=c('6037', '17031', '36061'), quarter_year=unique(total_labor_orig$quarter_year), 
                                  sol_type=c("reorg", "realloc")))
res_wages[, fval:=Inf]
res_wages[, w1:=Inf][, w2:=Inf][, w3:=Inf][, w4:=Inf][, w5:=Inf]

## adjust task mix and qualities
for (cnty in names(tild_theta)){
  for (qy in 2021.2){
    skills<-matrix(market_parms[grep(paste0(cnty,":avg_labor:B"),names(market_parms))], ncol=5, nrow=5, byrow=FALSE)
    working_data[county==cnty & quarter_year==qy, admin_auto_qual:=task_mix_4*max(skills[,4])]
  }
}


working_data[, qual_exo:=qual_exo+admin_auto_qual]
working_data[, task_mix_4:=0]
working_data[, task_mix_1:=task_mix_1/(task_mix_1+task_mix_2+task_mix_3+task_mix_5)]
working_data[, task_mix_2:=task_mix_2/(task_mix_1+task_mix_2+task_mix_3+task_mix_5)]
working_data[, task_mix_3:=task_mix_3/(task_mix_1+task_mix_2+task_mix_3+task_mix_5)]
working_data[, task_mix_5:=task_mix_5/(task_mix_1+task_mix_2+task_mix_3+task_mix_5)]

## adjust internal org: eliminate task 4, redistribute across all evenly.
orig_struct[['36061']][,B_1_1:=0][,B_1_2:=0][,B_1_3:=0][,B_1_4:=0][,B_1_5:=0]
orig_struct[['6037']][,B_1_1:=0][,B_1_2:=0][,B_1_3:=0][,B_1_4:=0][,B_1_5:=0]
orig_struct[['17031']][,B_1_1:=0][,B_1_2:=0][,B_1_3:=0][,B_1_4:=0][,B_1_5:=0]

cols<-colnames(orig_struct[['17031']])[grep("B_",colnames(orig_struct[['17031']]))]
orig_struct[['17031']][ , (cols) := lapply(.SD, '/', 1-task_mix_4), .SDcols = cols]
cols<-colnames(orig_struct[['6037']])[grep("B_",colnames(orig_struct[['17031']]))]
orig_struct[['6037']][ , (cols) := lapply(.SD, '/', 1-task_mix_4), .SDcols = cols]
cols<-colnames(orig_struct[['36061']])[grep("B_",colnames(orig_struct[['17031']]))]
orig_struct[['36061']][ , (cols) := lapply(.SD, '/', 1-task_mix_4), .SDcols = cols]


res_wages<-data.table(expand.grid(county=c('6037', '17031', '36061'), quarter_year=unique(total_labor_orig$quarter_year), 
                                  sol_type=c("reorg", "realloc")))
res_wages[, fval:=Inf]
res_wages[, w1:=Inf][, w2:=Inf][, w3:=Inf][, w4:=Inf][, w5:=Inf]


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
    if (orig_struct[[cnty]][location_id==loc,]$task_mix_4==1){
      return(list(c_endog=0,q_endog=0,E_1=0,
                  E_2=0, E_3=0, E_4=0, E_5=0))
    } else{
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
    }
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
    print(paste("*******automation*****",cnty, qy, " -  First Try"))
    quad_wages<-BBsolve(as.numeric(initial_wages[county==cnty & quarter_year==qy, c("w1", "w2", "w3", "w4", "w5")]),
                        solve_reloc, control=list(maxit=10000,trace=TRUE, NM=TRUE, noimp=50, tol=200000, M=seq(from=2, to=100, by=5)))
    quad_wages<-BBsolve(quad_wages$par,
                        solve_reloc, control=list(maxit=10000,trace=TRUE, NM=TRUE, noimp=50, tol=100000, M=seq(from=2, to=100, by=5)))
    quad_wages<-BBsolve(quad_wages$par,
                        solve_reloc, control=list(maxit=10000,trace=TRUE, NM=TRUE, noimp=50, tol=50000, M=seq(from=2, to=100, by=5)))
    quad_wages<-BBsolve(quad_wages$par,
                        solve_reloc, control=list(maxit=10000,trace=TRUE, NM=TRUE, noimp=50, tol=40000, M=seq(from=2, to=100, by=5)))
    quad_wages<-BBsolve(quad_wages$par,
                        solve_reloc, control=list(maxit=10000,trace=TRUE, NM=TRUE, noimp=50, tol=30000, M=seq(from=2, to=100, by=5)))
    quad_wages<-BBsolve(quad_wages$par,
                        solve_reloc, control=list(maxit=10000,trace=TRUE, NM=TRUE, noimp=50, tol=20000, M=seq(from=2, to=100, by=5)))
    quad_wages<-BBsolve(quad_wages$par,
                        solve_reloc, control=list(maxit=10000,trace=TRUE, NM=TRUE, noimp=50, tol=10000, M=seq(from=2, to=100, by=5)))
    quad_wages<-BBsolve(quad_wages$par,
                        solve_reloc, control=list(maxit=10000,trace=TRUE, NM=TRUE, noimp=50, tol=5000, M=seq(from=2, to=100, by=5)))
    quad_wages<-BBsolve(quad_wages$par,
                        solve_reloc, control=list(maxit=10000,trace=TRUE, NM=TRUE, noimp=50, tol=3000, M=seq(from=2, to=100, by=5)))
    quad_wages<-BBsolve(quad_wages$par,
                        solve_reloc, control=list(maxit=10000,trace=TRUE, NM=TRUE, noimp=50, tol=2000, M=seq(from=2, to=100, by=5)))
    quad_wages<-BBsolve(quad_wages$par,
                        solve_reloc, control=list(maxit=10000,trace=TRUE, NM=TRUE, noimp=50, tol=1, M=seq(from=2, to=100, by=5)))
    
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
solve_wages_abs<-function(wage_guess){
  
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
  
  return(sum(abs(labor_clearing)))
}


quad_wages<-optim(as.numeric(initial_wages[county==cnty & quarter_year==qy, c("w1", "w2", "w3", "w4", "w5")]), solve_wages_abs)

for (cnty in c('6037', '17031', '36061') ){
  for (qy in 2021.2 ){
    print(paste("*******automation*****",cnty, qy, " -  First Try"))
    quad_wages<-BBsolve(as.numeric(initial_wages[county==cnty & quarter_year==qy, c("w1", "w2", "w3", "w4", "w5")]),
                        solve_wages, control=list(maxit=10000,trace=TRUE, NM=TRUE, noimp=50, tol=200000, M=c(100,50,30,20,10)))
    quad_wages<-BBsolve(quad_wages$par,
                        solve_wages, control=list(maxit=10000,trace=TRUE, NM=TRUE, noimp=50, tol=100000, M=c(100,50,30,20,10)))
    quad_wages<-BBsolve(quad_wages$par,
                        solve_wages, control=list(maxit=10000,trace=TRUE, NM=TRUE, noimp=50, tol=50000, M=seq(from=2, to=100, by=5)))
    quad_wages<-BBsolve(quad_wages$par,
                        solve_wages, control=list(maxit=10000,trace=TRUE, NM=TRUE, noimp=50, tol=40000, M=seq(from=2, to=100, by=5)))
    quad_wages<-BBsolve(quad_wages$par,
                        solve_wages, control=list(maxit=10000,trace=TRUE, NM=TRUE, noimp=50, tol=30000, M=seq(from=2, to=100, by=5)))
    quad_wages<-BBsolve(quad_wages$par,
                        solve_wages, control=list(maxit=10000,trace=TRUE, NM=TRUE, noimp=50, tol=20000, M=seq(from=2, to=100, by=5)))
    quad_wages<-BBsolve(quad_wages$par,
                        solve_wages, control=list(maxit=10000,trace=TRUE, NM=TRUE, noimp=50, tol=10000, M=seq(from=2, to=100, by=5)))
    quad_wages<-BBsolve(quad_wages$par,
                        solve_wages, control=list(maxit=10000,trace=TRUE, NM=TRUE, noimp=50, tol=5000, M=seq(from=2, to=100, by=5)))
    quad_wages<-BBsolve(quad_wages$par,
                        solve_wages, control=list(maxit=10000,trace=TRUE, NM=TRUE, noimp=50, tol=3000, M=seq(from=2, to=100, by=5)))
    quad_wages<-BBsolve(quad_wages$par,
                        solve_wages, control=list(maxit=10000,trace=TRUE, NM=TRUE, noimp=50, tol=2000, M=seq(from=2, to=100, by=5)))
    quad_wages<-BBsolve(quad_wages$par,
                        solve_wages, control=list(maxit=10000,trace=TRUE, NM=TRUE, noimp=50, tol=1, M=seq(from=2, to=100, by=5)))
    
    res_wages[quarter_year==qy & county==cnty & sol_type=="reorg", c("w1", "w2", "w3", "w4", "w5"):= as.list(quad_wages$par)]
    res_wages[quarter_year==qy & county==cnty& sol_type=="reorg", fval:=quad_wages$residual]
    
  }
}




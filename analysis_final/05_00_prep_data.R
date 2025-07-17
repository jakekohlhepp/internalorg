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


## need to get back employee count
staff_task<-data.table(readRDS('analysis_final/data/01_00_staff_task.rds'))
has_emps<-unique(staff_task[, c("emps", "location_id", "quarter_year")])


## market parameters
working_data<-readRDS('analysis_final/data/02_06_data_for_counterfactuals.rds')
working_data<-merge(working_data, has_emps, by=c("location_id", "quarter_year"), all.x=TRUE)
stopifnot(nrow(working_data[is.na(emps)])==0)
rm(has_emps)
rm(staff_task)



## wage adjusted skills

all_results<-readRDS('analysis_final/data/02_00_parameters.rds')

market_parms<-all_results$coefficients
names(market_parms)<-all_results$parm_name

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
    tild_theta[[cnty]][[qy]]<-w_mat+(market_parms[grep(paste0(cnty,":cust_price$"),names(market_parms))])^(-1)*skills
    tild_theta[[cnty]][[qy]]<-sweep(tild_theta[[cnty]][[qy]],2,apply(tild_theta[[cnty]][[qy]],2,min))
    
  }
}


##### Initial State: Compute Total Labor
cex<-readRDS('mkdata/data/cex_outside.rds')
cex[, outside_share:=nohc_count /count_sample ]
cex<-cex[str_detect(PSU,"S")>0,]
cex<-unique(cex)
stopifnot(nrow(cex)==uniqueN(cex[,c("quarter_year", "PSU")]))

# get psu to county xwalk
cex<-merge(cex,data.table(readRDS("mkdata/data/county_msa_xwalk.rds")),by="PSU", all.x=TRUE, allow.cartesian = TRUE)
stopifnot(nrow(cex)==uniqueN(cex[,c("quarter_year", "county")]))
cex[, county:=as.character(county)]

### some firms were not in the estimation sample originally
## for these we need to plug in B_raw as the computed B

for (col in gsub("^B_raw_","",names(working_data)[grep("^B_raw_", names(working_data))])) working_data[,(paste0("B_raw_",col))  := ifelse(is.na(get(paste0("B_raw_",col))),get(paste0("B_",col)),get(paste0("B_raw_",col))) ]




addition_info<-readRDS("analysis_final/data/01_00_staff_task_full.rds")
addition_info<-unique(addition_info[,.SD, .SDcols=c("location_id", "quarter_year", "cust_count", "CSPOP", "revenue")])
working_data<-merge(working_data, addition_info, by=c("location_id", "quarter_year"), all.x=TRUE)
working_data<-merge(working_data,cex[,c("county", "quarter_year", "outside_share") ], by=c("county", "quarter_year"),all.x=TRUE )
working_data[, salon_share_subdiv:=cust_count/CSPOP]
# weight is balanced by qcew firm size distributions for 2021Q2
size_classes<-c(50398,14240,6663,2270+29+44+1+1)
working_data[emps<5, size_class:=1]
working_data[emps>=5 & emps<=9, size_class:=2]
working_data[emps>9 & emps<=19, size_class:=3]
working_data[emps>19, size_class:=4]
stopifnot(nrow(working_data[emps>99,])==0)

working_data[, tot_inclass:= uniqueN(location_id) , by=c("quarter_year", "county", "size_class") ]
working_data[, size_mult:=size_classes[size_class]/tot_inclass]


#working_data[, weight:=size_mult*(1-outside_share)/(sum(size_mult*salon_share_subdiv)), by=c("county","quarter_year")]
working_data[, weight:=(1-outside_share)/(sum(salon_share_subdiv)), by=c("county","quarter_year")]
stopifnot(nrow(working_data[is.na(weight) | is.infinite(weight)])==0)

# check weight
checkit<-working_data[, .(tot=sum(weight*salon_share_subdiv), outside_share=unique(outside_share)), by=c("county", "quarter_year")]
checkit[,shouldadd:=tot+outside_share]
stopifnot(nrow(checkit[round(shouldadd,15)!=1,])==0)
rm(checkit)

# other misc variables - need to make to get quality-cost residuals
working_data[, avg_labor:=tot_duration/cust_count/60]
working_data[, log_rel_mkt:=log(salon_share_subdiv/outside_share)]
working_data[, cust_price:=revenue/cust_count]
working_data[, mk_piece:=1/(1-salon_share_subdiv)]


## for this portion only, we obtain org_cost by 
working_data[, org_cost:=ifelse(is.finite(gamma_invert),gamma_invert*s_index*avg_labor,0)]

## Compute amount of each type.
get_demands<-function(a1, a2, a3, a4, a5, county,quarter_year,gamma){
  alpha<-c(a1, a2, a3, a4,a5)
  if (is.finite(gamma) & gamma>0 & !is.na(gamma)){
    
    alpha<-c(a1, a2, a3, a4,a5)
    ## this function will return matrix given gamma
    A<-exp(-1/gamma*(tild_theta[[county]][[as.character(quarter_year)]]) )
    E<-rep(0.2, 5)
    A[A>=Inf]<-1e16
    A[A<=0]<-1e-16
    fxpt<-function(p){
      C<-colSums(t(A)*alpha/colSums(A*p))
      return(p*C)
    }
    for (i in 1:1000000){
      E_old<-E
      E<-fxpt(E_old)
      if (all(abs(E-E_old)<innertol)) break
    }
    B<-t(t(A)*alpha/colSums(A*E))*E
  } else if (is.infinite(gamma) | is.na(gamma) | is.nan(gamma)){
    # max frictions
    B<-matrix(0, ncol=5, nrow=5)
    B[which.min(rowSums(t(t(tild_theta[[county]][[as.character(quarter_year)]])*alpha ))),]<-alpha
  } else{
    # no frictions
    B<-matrix(0, ncol=5, nrow=5)
    for (col in 1:5){
      B[which.min(tild_theta[[county]][[as.character(quarter_year)]][,col]),col]<-alpha[col]
    }
  }
  
  E<-rowSums(B)
  B[abs(B)<1e-16]<-0
  Brel<-t(t(B/E)/alpha)
  return(c(E,sum(B*spec_log(Brel))) )
}
for (i in 1:nrow(working_data)){
  working_data[i,c("E_1", "E_2", "E_3", "E_4", "E_5", "s_pred") := as.list(get_demands(task_mix_1, task_mix_2, task_mix_3, task_mix_4, task_mix_5, county,quarter_year, gamma_invert)) ]
}
# fill in labor demands for firms not in regression estimation sample.
# to get labor demand we use raw shares for firms in estimation sample and we use model shares for others.

working_data[is.na(E_raw_1), E_raw_1:=E_1]
working_data[is.na(E_raw_2), E_raw_2:=E_2]
working_data[is.na(E_raw_3), E_raw_3:=E_3]
working_data[is.na(E_raw_4), E_raw_4:=E_4]
working_data[is.na(E_raw_5), E_raw_5:=E_5]


# labor demand is then weights times market share times avg_labor times cspop
total_labor<-working_data[, .(tot_1=sum(weight*salon_share_subdiv*CSPOP*E_1*avg_labor),tot_2=sum(weight*salon_share_subdiv*CSPOP*E_2*avg_labor),
                              tot_3=sum(weight*salon_share_subdiv*CSPOP*E_3*avg_labor),tot_4=sum(weight*salon_share_subdiv*CSPOP*E_4*avg_labor),
                              tot_5=sum(weight*salon_share_subdiv*CSPOP*E_5*avg_labor)), by=c("county", "quarter_year")]
total_labor_orig<-copy(total_labor)

# get quality and marginal cost heterogeneity (residual)
# instead of writing out the entire multiplication I rebuild 
# the feature matrix

# only missing should be infinite gammas



parm_demand<-all_results[demand==TRUE,]$coefficients
parm_supply<-all_results[demand==FALSE & !str_detect(parm_name, "E_raw_[0-9]$"),]$coefficients
working_data[, wb_2:=market_parms[paste0("factor(county)", county, ":avg_labor:E_raw_2")]*E_raw_2*avg_labor]
working_data[, wb_3:=market_parms[paste0("factor(county)", county, ":avg_labor:E_raw_3")]*E_raw_3*avg_labor]
working_data[, wb_4:=market_parms[paste0("factor(county)", county, ":avg_labor:E_raw_4")]*E_raw_4*avg_labor]
working_data[, wb_5:=market_parms[paste0("factor(county)", county, ":avg_labor:E_raw_5")]*E_raw_5*avg_labor]

## get exogenous cost and demand shifters
## these are everything net of org cost, markups, 
xnam<-c("factor(county):cust_price","factor(county):factor(quarter_year)",paste0("factor(county):avg_labor:",names(working_data)[grep("^B_raw_[0-9]_", names(working_data))]))
xnam <- as.formula(paste0("~",paste0(xnam, collapse="+"),"-1"))
mm_1<-model.matrix(xnam, data=working_data)
mm_1[,-c(grep(":cust_price$",colnames(mm_1)),grep("B_raw_",colnames(mm_1)))]<-0
working_data[,qual_exo:=log_rel_mkt- mm_1%*%parm_demand]
working_data[, mean_qual_exo:=sum(qual_exo*(service_mix_id=="11111"))/sum(service_mix_id=="11111"), by=c("county", "quarter_year")]


xnam <- as.formula("~avg_labor:factor(county):factor(quarter_year)+factor(quarter_year):factor(county)+factor(quarter_year):(task_mix_2+task_mix_3+task_mix_4+task_mix_5)-1")
mod_mm_2<-model.matrix(xnam, data=working_data)
mod_mm_2[,-grep("^avg_labor:",colnames(mod_mm_2))]<-0
working_data[, cost_exo:=cust_price-wb_2-wb_3-wb_4-wb_5-org_cost+mk_piece/market_parms[paste0("factor(county)",county, ":cust_price")] - mod_mm_2%*%parm_supply ]
working_data[, mean_cost_exo:=sum(cost_exo*(service_mix_id=="11111"))/sum(service_mix_id=="11111"), by=c("county", "quarter_year")]


## trim

#### solve counterfactual

# start guess at estimated wages.
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

solve_wage_abs<-function(x){
  return(sum(abs(solve_wages(x))))
}


res_wages<-data.table(expand.grid(county=c('6037', '17031', '36061'), quarter_year=unique(total_labor_orig$quarter_year)))
res_wages[, fval:=Inf]
res_wages[, w1:=Inf][, w2:=Inf][, w3:=Inf][, w4:=Inf][, w5:=Inf]



for (cnty in c('36061') ){
  for (qy in 2021.2 ){
    print(paste("*******",cnty, qy, " -  First Try"))
    quad_wages<-BBsolve(initial_guess[grep(paste0("^",cnty,"-",qy), names(initial_guess))],
                        solve_wages, control=list(maxit=10000,trace=TRUE, NM=FALSE, noimp=200, tol=10000, M=c(10,20,5,50)))
    if (all(quad_wages$par>0)){
      res_wages[quarter_year==qy & county==cnty, c("w1", "w2", "w3", "w4", "w5"):= as.list(quad_wages$par)]
      res_wages[quarter_year==qy & county==cnty, fval:=quad_wages$residual]
    } else{
      res_wages[quarter_year==qy & county==cnty, c("w1", "w2", "w3", "w4", "w5"):= as.list(initial_guess[grep(paste0("^",cnty,"-",qy), names(initial_guess))])]
      res_wages[quarter_year==qy & county==cnty, fval:=sqrt(sum((solve_wages(initial_guess[grep(paste0("^",cnty,"-",qy), names(initial_guess))]))^2))]
    }
    
  }
}

for (cnty in c('36061')){
  for (qy in 2021.2 ){
    print(paste("*******",cnty, qy, " -  Refine"))
    quad_wages<-broyden(solve_wages, as.numeric(res_wages[quarter_year==qy & county==cnty, c("w1", "w2", "w3", "w4","w5")]),maxiter=1000 )
    if (all(quad_wages$zero>0)){
      print("Successful final refine first try.")
      res_wages[quarter_year==qy & county==cnty, c("w1", "w2", "w3", "w4", "w5"):= as.list(quad_wages$zero)]
      res_wages[quarter_year==qy & county==cnty, fval:=quad_wages$fnorm]
    } else{
      
    }
    
    
    
    
  }
}

for (cnty in c('36061') ){
  for (qy in 2021.2 ){
    print(paste("*******",cnty, qy, " -  First Try"))
    quad_wages<-BBsolve(as.numeric(res_wages[quarter_year==qy & county==cnty, c("w1", "w2", "w3", "w4","w5")]),
                        solve_wages, control=list(maxit=10000,trace=TRUE, NM=TRUE, noimp=200, tol=1, M=c(10,20,5,50)))
    if (all(quad_wages$par>0)){
      res_wages[quarter_year==qy & county==cnty, c("w1", "w2", "w3", "w4", "w5"):= as.list(quad_wages$par)]
      res_wages[quarter_year==qy & county==cnty, fval:=quad_wages$residual]
    } else{
      res_wages[quarter_year==qy & county==cnty, c("w1", "w2", "w3", "w4", "w5"):= as.list(initial_guess[grep(paste0("^",cnty,"-",qy), names(initial_guess))])]
      res_wages[quarter_year==qy & county==cnty, fval:=sqrt(sum((solve_wages(initial_guess[grep(paste0("^",cnty,"-",qy), names(initial_guess))]))^2))]
    }
    
  }
}
## for los angeles, use higher threshold



for (cnty in c('6037') ){
  for (qy in 2021.2 ){
    print(paste("*******",cnty, qy, " -  First Try"))
    quad_wages<-BBsolve(initial_guess[grep(paste0("^",cnty,"-",qy), names(initial_guess))],
                        solve_wages, control=list(maxit=10000,trace=TRUE, NM=TRUE, noimp=200, tol=500000, M=c(5,20,5,50)))
    if (all(quad_wages$par>0)){
      res_wages[quarter_year==qy & county==cnty, c("w1", "w2", "w3", "w4", "w5"):= as.list(quad_wages$par)]
      res_wages[quarter_year==qy & county==cnty, fval:=quad_wages$residual]
    } else{
      res_wages[quarter_year==qy & county==cnty, c("w1", "w2", "w3", "w4", "w5"):= as.list(initial_guess[grep(paste0("^",cnty,"-",qy), names(initial_guess))])]
      res_wages[quarter_year==qy & county==cnty, fval:=sqrt(sum((solve_wages(initial_guess[grep(paste0("^",cnty,"-",qy), names(initial_guess))]))^2))]
    }
    
  }
}

for (cnty in c('6037') ){
  for (qy in 2021.2 ){
    print(paste("*******",cnty, qy, " -  First Try"))
    quad_wages<-BBsolve(as.numeric(res_wages[quarter_year==qy & county==cnty, c("w1", "w2", "w3", "w4","w5")]),
                        solve_wages, control=list(maxit=10000,trace=TRUE, NM=TRUE, noimp=200, tol=100000, M=c(10,20,5,50)))
    if (all(quad_wages$par>0)){
      res_wages[quarter_year==qy & county==cnty, c("w1", "w2", "w3", "w4", "w5"):= as.list(quad_wages$par)]
      res_wages[quarter_year==qy & county==cnty, fval:=quad_wages$residual]
    } else{
      res_wages[quarter_year==qy & county==cnty, c("w1", "w2", "w3", "w4", "w5"):= as.list(initial_guess[grep(paste0("^",cnty,"-",qy), names(initial_guess))])]
      res_wages[quarter_year==qy & county==cnty, fval:=sqrt(sum((solve_wages(initial_guess[grep(paste0("^",cnty,"-",qy), names(initial_guess))]))^2))]
    }
    
  }
}


for (cnty in c('6037') ){
  for (qy in 2021.2 ){
    print(paste("*******",cnty, qy, " -  First Try"))
    quad_wages<-BBsolve(as.numeric(res_wages[quarter_year==qy & county==cnty, c("w1", "w2", "w3", "w4","w5")]),
                        solve_wages, control=list(maxit=10000,trace=TRUE, NM=TRUE, noimp=50, tol=50000, M=c(50,5,10,20,100)))
    if (all(quad_wages$par>0)){
      res_wages[quarter_year==qy & county==cnty, c("w1", "w2", "w3", "w4", "w5"):= as.list(quad_wages$par)]
      res_wages[quarter_year==qy & county==cnty, fval:=quad_wages$residual]
    } else{
      res_wages[quarter_year==qy & county==cnty, c("w1", "w2", "w3", "w4", "w5"):= as.list(initial_guess[grep(paste0("^",cnty,"-",qy), names(initial_guess))])]
      res_wages[quarter_year==qy & county==cnty, fval:=sqrt(sum((solve_wages(initial_guess[grep(paste0("^",cnty,"-",qy), names(initial_guess))]))^2))]
    }
    
  }
}


for (cnty in c('6037')){
  for (qy in 2021.2 ){
    print(paste("*******",cnty, qy, " -  Refine"))
    quad_wages<-broyden(solve_wages, as.numeric(res_wages[quarter_year==qy & county==cnty, c("w1", "w2", "w3", "w4","w5")]),maxiter=1000 )
    if (all(quad_wages$zero>0)){
      print("Successful final refine first try.")
      res_wages[quarter_year==qy & county==cnty, c("w1", "w2", "w3", "w4", "w5"):= as.list(quad_wages$zero)]
      res_wages[quarter_year==qy & county==cnty, fval:=quad_wages$fnorm]
    } else{
      
    }
    
    
    
    
  }
}


for (cnty in c('6037') ){
  for (qy in 2021.2 ){
    print(paste("*******",cnty, qy, " -  First Try"))
    quad_wages<-BBsolve(as.numeric(res_wages[quarter_year==qy & county==cnty, c("w1", "w2", "w3", "w4","w5")]),
                        solve_wages, control=list(maxit=10000,trace=TRUE, NM=TRUE, noimp=30, tol=1, M=c(2,20,5,50)))
    if (all(quad_wages$par>0)){
      res_wages[quarter_year==qy & county==cnty, c("w1", "w2", "w3", "w4", "w5"):= as.list(quad_wages$par)]
      res_wages[quarter_year==qy & county==cnty, fval:=quad_wages$residual]
    } else{
      res_wages[quarter_year==qy & county==cnty, c("w1", "w2", "w3", "w4", "w5"):= as.list(initial_guess[grep(paste0("^",cnty,"-",qy), names(initial_guess))])]
      res_wages[quarter_year==qy & county==cnty, fval:=sqrt(sum((solve_wages(initial_guess[grep(paste0("^",cnty,"-",qy), names(initial_guess))]))^2))]
    }
    
  }
}


for (cnty in c('17031') ){
  for (qy in 2021.2 ){
    print(paste("*******",cnty, qy, " -  First Try"))
    quad_wages<-BBsolve(initial_guess[grep(paste0("^",cnty,"-",qy), names(initial_guess))],
                        solve_wages, control=list(maxit=10000,trace=TRUE, NM=FALSE, noimp=50, tol=500000, M=c(100,20,5,50)))
    if (all(quad_wages$par>0)){
      res_wages[quarter_year==qy & county==cnty, c("w1", "w2", "w3", "w4", "w5"):= as.list(quad_wages$par)]
      res_wages[quarter_year==qy & county==cnty, fval:=quad_wages$residual]
    } else{
      res_wages[quarter_year==qy & county==cnty, c("w1", "w2", "w3", "w4", "w5"):= as.list(initial_guess[grep(paste0("^",cnty,"-",qy), names(initial_guess))])]
      res_wages[quarter_year==qy & county==cnty, fval:=sqrt(sum((solve_wages(initial_guess[grep(paste0("^",cnty,"-",qy), names(initial_guess))]))^2))]
    }
    
  }
}

for (cnty in c('17031') ){
  for (qy in 2021.2 ){
    print(paste("*******",cnty, qy, " -  First Try"))
    quad_wages<-BBsolve(as.numeric(res_wages[quarter_year==qy & county==cnty, c("w1", "w2", "w3", "w4","w5")]),
                        solve_wages, control=list(maxit=10000,trace=TRUE, NM=TRUE, noimp=50, tol=10000, M=c(100,20,5,50)))
    if (all(quad_wages$par>0)){
      res_wages[quarter_year==qy & county==cnty, c("w1", "w2", "w3", "w4", "w5"):= as.list(quad_wages$par)]
      res_wages[quarter_year==qy & county==cnty, fval:=quad_wages$residual]
    } else{
      res_wages[quarter_year==qy & county==cnty, c("w1", "w2", "w3", "w4", "w5"):= as.list(initial_guess[grep(paste0("^",cnty,"-",qy), names(initial_guess))])]
      res_wages[quarter_year==qy & county==cnty, fval:=sqrt(sum((solve_wages(initial_guess[grep(paste0("^",cnty,"-",qy), names(initial_guess))]))^2))]
    }
    
  }
}

for (cnty in c('17031')){
  for (qy in 2021.2 ){
    print(paste("*******",cnty, qy, " -  Refine"))
    quad_wages<-broyden(solve_wages, as.numeric(res_wages[quarter_year==qy & county==cnty, c("w1", "w2", "w3", "w4","w5")]),maxiter=1000 )
    if (quad_wages$fnorm<=40 & all(quad_wages$zero>0)){
      print("Successful final refine first try.")
      res_wages[quarter_year==qy & county==cnty, c("w1", "w2", "w3", "w4", "w5"):= as.list(quad_wages$zero)]
      res_wages[quarter_year==qy & county==cnty, fval:=quad_wages$fnorm]
    } else{
      
    }
    
    
    
    
  }
}

saveRDS(res_wages, "analysis_final/data/05_00_initial_wages.rds")
saveRDS(working_data, "analysis_final/data/05_00_working_data.rds")

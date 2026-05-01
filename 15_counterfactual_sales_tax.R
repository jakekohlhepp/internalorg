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
source("utils/logging.R")
source("utils/counterfactuals_core.R")

log_init("15_counterfactual_sales_tax.R")
script_success <- FALSE
on.exit(log_complete(script_success), add = TRUE)

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

get_everything<-function(wage_guess, cnty, qy){
  
  counter_res<-copy(working_data[county==cnty & quarter_year==qy, c("location_id","county","quarter_year","gamma_invert","avg_labor", "task_mix_1",
                                                                    "task_mix_2", "task_mix_3", "task_mix_4", "task_mix_5",
                                                                    "qual_exo", "cost_exo","weight", "cust_price",
                                                                    "CSPOP")])
  
  ## create the skill sets matrix (theta), wage vectors (wage_guess), and wage-adjusted skills (new_tild_theta)
  skill_matrices <- counterfactual_skill_matrices(wage_guess, cnty, market_parms, rho)

  new_theta <- skill_matrices$theta

  new_tild_theta <- skill_matrices$cost_matrix
  
  ## solve internal org
  
  
  solve_org<-Vectorize(function(a1, a2, a3, a4, a5,gamma){
    allocation <- counterfactual_assignment(new_tild_theta, c(a1, a2, a3, a4, a5), gamma, innertol)

    E <- allocation$E

    B <- allocation$B

    ## compute endogenous cost and quality components.
    # cost is wages plus org cost.
    cendog<-sum(E*wage_guess)+ifelse(is.finite(gamma),gamma*counterfactual_entropy(B),
                                     0)
    qendog<-sum(B*new_theta)
    return(list(c_endog=cendog,q_endog=qendog,s_index=counterfactual_entropy(B),
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
  
  
  counter_res[, newprice:=counterfactual_best_response_prices(cust_price, Q, C, weight, rho[cnty], outertol)]
  counter_res[, new_share:=exp(Q+rho[cnty]*newprice)]
  counter_res[,new_share:=new_share/(sum(weight*new_share)+1)]
  
  return(counter_res)
  
}

orig_struct <- build_counterfactual_structure_snapshot(get_everything, initial_wages)



## sales tax - equivalent to scale down rho
rho_orig<-rho
names(rho_orig)<-names(rho)
rho['36061']<-rho['36061']*1.08/1.04
rho['6037']<-rho['6037']*1.04
rho['17031']<-rho['17031']*1.04

res_wages<-new_counterfactual_wages_grid(unique(total_labor_orig$quarter_year))


solve_reloc<-function(wage_guess){
  
  counter_res<-copy(working_data[county==cnty & quarter_year==qy, c("location_id","county","quarter_year","gamma_invert","avg_labor", "task_mix_1",
                                                                    "task_mix_2", "task_mix_3", "task_mix_4", "task_mix_5",
                                                                    "qual_exo", "cost_exo","weight", "cust_price",
                                                                    "CSPOP", "E_1", "E_2", "E_3", "E_4", "E_5")])
  
  ## create the skill sets matrix (theta), wage vectors (wage_guess), and wage-adjusted skills (new_tild_theta)
  ## sweep here, because it improve snumericla performance. undo when calcing quality.
  skill_matrices <- counterfactual_skill_matrices(wage_guess, cnty, market_parms, rho)

  new_theta <- skill_matrices$theta

  new_tild_theta <- skill_matrices$cost_matrix
  
  ## solve internal org
  
  
  solve_org<-Vectorize(function(loc, gamma){
    B<-matrix(as.matrix(orig_struct[[cnty]][location_id==loc,.SD, .SDcols=grep("^B_", colnames(orig_struct[[cnty]]))]), byrow=FALSE,nrow=5, ncol=5)
    E<-rowSums(B)
    Brel<-t(t(B/E)/colSums(B))
    ## compute endogenous cost and quality components.
    # cost is wages plus org cost.
    cendog<-sum(E*wage_guess)+ifelse(is.finite(gamma),gamma*counterfactual_entropy(B),
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
  counter_res[, newprice:=counterfactual_best_response_prices(cust_price, Q, C, weight, rho[cnty], outertol)]
  
  
  counter_res[, new_share:=exp(Q+rho[cnty]*newprice)]
  counter_res[,new_share:=new_share/(sum(weight*new_share)+1)]
  
  new_total_labor<-counter_res[, .(tot_1=sum(weight*new_share*CSPOP*E_1*avg_labor),tot_2=sum(weight*new_share*CSPOP*E_2*avg_labor),
                                   tot_3=sum(weight*new_share*CSPOP*E_3*avg_labor),tot_4=sum(weight*new_share*CSPOP*E_4*avg_labor),
                                   tot_5=sum(weight*new_share*CSPOP*E_5*avg_labor))]
  
  return(counterfactual_labor_gap(new_total_labor, total_labor, cnty, qy))
}
for (cnty in c('36061','17031', '6037') ){
  for (qy in 2021.2 ){
    print(paste("*******",cnty, qy, " -  Realloc nleqslv"))
    quad_wages<-counterfactual_solve_wage_market(
      solve_reloc,
      as.numeric(initial_wages[county==cnty & quarter_year==qy, c("w1", "w2", "w3", "w4", "w5")]),
      label = paste("15 realloc", cnty, qy)
    )
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
  skill_matrices <- counterfactual_skill_matrices(wage_guess, cnty, market_parms, rho)

  new_theta <- skill_matrices$theta

  new_tild_theta <- skill_matrices$cost_matrix
  
  ## solve internal org
  
  
  solve_org<-Vectorize(function(a1, a2, a3, a4, a5,gamma){
    allocation <- counterfactual_assignment(new_tild_theta, c(a1, a2, a3, a4, a5), gamma, innertol)

    E <- allocation$E

    B <- allocation$B

    ## compute endogenous cost and quality components.
    # cost is wages plus org cost.
    cendog<-sum(E*wage_guess)+ifelse(is.finite(gamma),gamma*counterfactual_entropy(B),
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
  counter_res[, newprice:=counterfactual_best_response_prices(cust_price, Q, C, weight, rho[cnty], outertol)]
  
  
  counter_res[, new_share:=exp(Q+rho[cnty]*newprice)]
  counter_res[,new_share:=new_share/(sum(weight*new_share)+1)]
  
  new_total_labor<-counter_res[, .(tot_1=sum(weight*new_share*CSPOP*E_1*avg_labor),tot_2=sum(weight*new_share*CSPOP*E_2*avg_labor),
                                   tot_3=sum(weight*new_share*CSPOP*E_3*avg_labor),tot_4=sum(weight*new_share*CSPOP*E_4*avg_labor),
                                   tot_5=sum(weight*new_share*CSPOP*E_5*avg_labor))]
  
  return(counterfactual_labor_gap(new_total_labor, total_labor, cnty, qy))
}

for (cnty in c('36061','17031') ){
  for (qy in 2021.2 ){
    print(paste("*******",cnty, qy, " -  Reorg nleqslv"))
    quad_wages<-counterfactual_solve_wage_market(
      solve_wages,
      as.numeric(initial_wages[county==cnty & quarter_year==qy, c("w1", "w2", "w3", "w4", "w5")]),
      label = paste("15 reorg", cnty, qy)
    )
    
    
    res_wages[quarter_year==qy & county==cnty & sol_type=="reorg", c("w1", "w2", "w3", "w4", "w5"):= as.list(quad_wages$par)]
    res_wages[quarter_year==qy & county==cnty& sol_type=="reorg", fval:=quad_wages$residual]
    
  }
}


for (cnty in c('6037') ){
  for (qy in 2021.2 ){
    print(paste("*******",cnty, qy, " -  Reorg nleqslv"))
    quad_wages<-counterfactual_solve_wage_market(
      solve_wages,
      as.numeric(res_wages[county==cnty & quarter_year==qy &sol_type=="realloc", c("w1", "w2", "w3", "w4", "w5")]),
      label = paste("15 reorg", cnty, qy)
    )
    
    res_wages[quarter_year==qy & county==cnty & sol_type=="reorg", c("w1", "w2", "w3", "w4", "w5"):= as.list(quad_wages$par)]
    res_wages[quarter_year==qy & county==cnty& sol_type=="reorg", fval:=quad_wages$residual]
    
  }
}

save_counterfactual_rds(
  res_wages,
  "05_03_wages_salestax.rds",
  legacy_filename = "05_03_wages_salestax.rds"
)


### save productivity.


get_prod<-function(wage_guess, cnty, qy,stype){
  
  counter_res<-copy(working_data[county==cnty & quarter_year==qy, c("location_id","county","quarter_year","gamma_invert","avg_labor", "task_mix_1",
                                                                    "task_mix_2", "task_mix_3", "task_mix_4", "task_mix_5",
                                                                    "qual_exo", "cost_exo","weight", "cust_price",
                                                                    "CSPOP")])
  
  ## create the skill sets matrix (theta), wage vectors (wage_guess), and wage-adjusted skills (new_tild_theta)
  skill_matrices <- counterfactual_skill_matrices(wage_guess, cnty, market_parms, rho)

  new_theta <- skill_matrices$theta

  new_tild_theta <- skill_matrices$cost_matrix
  
  ## solve internal org
  
  if (stype=="reorg"){
    solve_org<-Vectorize(function(a1, a2, a3, a4, a5,gamma){
      allocation <- counterfactual_assignment(new_tild_theta, c(a1, a2, a3, a4, a5), gamma, innertol)

      E <- allocation$E

      B <- allocation$B

      ## compute endogenous cost and quality components.
      # cost is wages plus org cost.
      cendog<-sum(E*wage_guess)+ifelse(is.finite(gamma),gamma*counterfactual_entropy(B),
                                       0)
      qendog<-sum(B*new_theta)
      s_hold<-counterfactual_entropy(B)
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
      cendog<-sum(E*wage_guess)+ifelse(is.finite(gamma),gamma*counterfactual_entropy(B),
                                       0)
      qendog<-sum(B*new_theta)
      s_hold<-counterfactual_entropy(B)
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
  
  
  counter_res[, newprice:=counterfactual_best_response_prices(cust_price, Q, C, weight, rho[cnty], outertol)]
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
  "05_03_prod_salestax.rds",
  legacy_filename = "05_03_prod_salestax.rds"
)



## save productivity of original
rho<-rho_orig
prod_data_new<-data.table()
for (cnty in unique(initial_wages[is.finite(w1),]$county)){
  for (qy in unique(initial_wages[is.finite(w1),]$quarter_year)){
      prod_data_new<-rbind(prod_data_new, data.table(county=cnty, quarter_year=qy, sol_type=NA,get_prod(as.numeric(initial_wages[county==cnty & quarter_year==qy, c("w1", "w2", "w3", "w4", "w5")]),
                                                                                                         cnty, qy,"reorg") ))
  }
}
save_counterfactual_rds(
  prod_data_new,
  "05_03_prod_initial.rds",
  legacy_filename = "05_03_prod_initial.rds"
)



script_success <- TRUE

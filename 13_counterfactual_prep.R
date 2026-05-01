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

log_init("13_counterfactual_prep.R")
script_success <- FALSE
on.exit(log_complete(script_success), add = TRUE)

load_counterfactual_packages()
ensure_counterfactual_dirs()


## need to get back employee count
staff_task<-data.table(read_first_existing_rds(
  c(project_path(CONFIG$prep_output_dir, "01_staff_task.rds"),
    legacy_counterfactual_data_path("01_00_staff_task.rds")),
  "counterfactual staff-task input"
))
has_emps<-unique(staff_task[, c("emps", "location_id", "quarter_year")])


## market parameters
working_data<-read_first_existing_rds(
  c(project_path("results", "data", "12_data_for_counterfactuals.rds"),
    legacy_counterfactual_data_path("02_06_data_for_counterfactuals.rds")),
  "counterfactual working-data input"
)
working_data<-merge(working_data, has_emps, by=c("location_id", "quarter_year"), all.x=TRUE)
stopifnot(nrow(working_data[is.na(emps)])==0)
rm(has_emps)
rm(staff_task)



## wage adjusted skills

all_results<-read_counterfactual_parameters()
market_parms<-get_counterfactual_market_parms(all_results)
guess_setup<-build_counterfactual_initial_guess(working_data, market_parms)
tild_theta<-build_counterfactual_tild_theta(working_data, market_parms, guess_setup$rho)


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




addition_info<-read_first_existing_rds(
  c(project_path(CONFIG$prep_output_dir, "01_staff_task_full.rds"),
    legacy_counterfactual_data_path("01_00_staff_task_full.rds")),
  "counterfactual staff-task full input"
)
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
  counterfactual_assignment_vector(
    c(a1, a2, a3, a4, a5),
    tild_theta[[county]][[as.character(quarter_year)]],
    gamma,
    innertol,
    include_s_index = TRUE
  )
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
initial_guess<-guess_setup$initial_guess
rho<-guess_setup$rho


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
    counterfactual_org_values(
      c(a1, a2, a3, a4, a5),
      gamma,
      new_tild_theta,
      new_theta,
      wage_guess,
      innertol
    )
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

solve_wage_abs<-function(x){
  return(sum(abs(solve_wages(x))))
}


res_wages<-new_counterfactual_wages_grid(
  unique(total_labor_orig$quarter_year),
  include_solution_type = FALSE
)



for (cnty in c("36061", "6037", "17031")) {
  for (qy in 2021.2) {
    print(paste("*******", cnty, qy, "- nleqslv wage solve"))
    start_wages <- initial_guess[grep(paste0("^", cnty, "-", qy), names(initial_guess))]
    quad_wages <- counterfactual_solve_wage_market(
      solve_wages,
      start_wages,
      label = paste("13", cnty, qy)
    )

    res_wages[
      quarter_year == qy & county == cnty,
      c("w1", "w2", "w3", "w4", "w5") := as.list(quad_wages$par)
    ]
    res_wages[
      quarter_year == qy & county == cnty,
      fval := quad_wages$residual
    ]
  }
}

save_counterfactual_rds(
  res_wages,
  "05_00_initial_wages.rds",
  legacy_filename = "05_00_initial_wages.rds"
)
save_counterfactual_rds(
  working_data,
  "05_00_working_data.rds",
  legacy_filename = "05_00_working_data.rds"
)

script_success <- TRUE

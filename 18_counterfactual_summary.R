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

counterfactual_context <- load_counterfactual_context(extra_packages = c("kableExtra"))
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

#### initial
wage_vect_initial<-read_counterfactual_rds(
  "05_00_initial_wages.rds",
  legacy_filenames = "05_00_initial_wages.rds",
  description = "baseline counterfactual wages"
)
prod_data_initial<-read_counterfactual_rds(
  "05_03_prod_initial.rds",
  legacy_filenames = "05_03_prod_initial.rds",
  description = "baseline counterfactual productivity panel"
)


prod_data_initial<-melt(prod_data_initial,id.vars=c("location_id", "avg_labor", "new_share","CSPOP", "county", "s_index","sol_type", "weight"), measure.vars=patterns("^B_[0-9]") )
prod_data_initial[, multiplier:=avg_labor*CSPOP*new_share*weight]
prod_data_initial[, worker_type:=str_replace(variable, "^B_[0-9]_","")]

firm_initial<-prod_data_initial[,.(tot_prod=sum(value)), c('s_index', "multiplier","location_id", "county", "sol_type")]
firm_initial<-firm_initial[, .(s_avg=weighted.mean(s_index, multiplier),labor_prod=sum(tot_prod*multiplier)), by=c("county", "sol_type")]


prod_data_initial<-prod_data_initial[, .(tot_prod=sum(multiplier*value)), by=c("county", "worker_type","sol_type")]


prod_data_initial<-dcast(prod_data_initial, county+sol_type~worker_type, value.var="tot_prod")
prod_data_initial[, version:="Initial"]


#### sales tax
wage_vect_salestax<-read_counterfactual_rds(
  "05_03_wages_salestax.rds",
  legacy_filenames = "05_03_wages_salestax.rds",
  description = "sales-tax counterfactual wages"
)
prod_data_salestax<-read_counterfactual_rds(
  "05_03_prod_salestax.rds",
  legacy_filenames = "05_03_prod_salestax.rds",
  description = "sales-tax counterfactual productivity panel"
)


prod_data_salestax<-melt(prod_data_salestax,id.vars=c("location_id", "avg_labor", "new_share","CSPOP", "county", "s_index","sol_type", "weight"), measure.vars=patterns("^B_[0-9]") )
prod_data_salestax[, multiplier:=avg_labor*CSPOP*new_share*weight]
prod_data_salestax[, worker_type:=str_replace(variable, "^B_[0-9]_","")]

firm_salestax<-prod_data_salestax[,.(tot_prod=sum(value)), c('s_index', "multiplier","location_id", "county", "sol_type")]
firm_salestax<-firm_salestax[, .(s_avg=weighted.mean(s_index, multiplier),labor_prod=sum(tot_prod*multiplier)), by=c("county", "sol_type")]
firm_salestax[, version:="Sales Tax"]


prod_data_salestax<-prod_data_salestax[, .(tot_prod=sum(multiplier*value)), by=c("county", "worker_type","sol_type")]


prod_data_salestax<-dcast(prod_data_salestax, county+sol_type~worker_type, value.var="tot_prod")
prod_data_salestax[, version:="Sales Tax"]

#### management diffusion
wage_vect_diffusion<-read_counterfactual_rds(
  "05_02_wages_diffusion.rds",
  legacy_filenames = "05_02_wages_diffusion.rds",
  description = "diffusion counterfactual wages"
)
prod_data_diffusion<-read_counterfactual_rds(
  "05_02_prod_diffusion.rds",
  legacy_filenames = "05_02_prod_diffusion.rds",
  description = "diffusion counterfactual productivity panel"
)


prod_data_diffusion<-melt(prod_data_diffusion,id.vars=c("location_id", "avg_labor", "new_share","CSPOP", "county", "s_index","sol_type", "weight"), measure.vars=patterns("^B_[0-9]") )
prod_data_diffusion[, multiplier:=avg_labor*CSPOP*new_share*weight]
prod_data_diffusion[, worker_type:=str_replace(variable, "^B_[0-9]_","")]

firm_diffusion<-prod_data_diffusion[,.(tot_prod=sum(value)), c('s_index', "multiplier","location_id", "county", "sol_type")]
firm_diffusion<-firm_diffusion[, .(s_avg=weighted.mean(s_index, multiplier),labor_prod=sum(tot_prod*multiplier)), by=c("county", "sol_type")]
firm_diffusion[, version:="Management Diffusion"]



prod_data_diffusion<-prod_data_diffusion[, .(tot_prod=sum(multiplier*value)), by=c("county", "worker_type","sol_type")]


prod_data_diffusion<-dcast(prod_data_diffusion, county+sol_type~worker_type, value.var="tot_prod")
prod_data_diffusion[, version:="Management Diffusion"]

#### immigration
wage_vect_immigration<-read_counterfactual_rds(
  "05_04_wages_immigration.rds",
  legacy_filenames = "05_04_wages_immigration.rds",
  description = "immigration counterfactual wages"
)
prod_data_immigration<-read_counterfactual_rds(
  "05_04_prod_immigration.rds",
  legacy_filenames = "05_04_prod_immigration.rds",
  description = "immigration counterfactual productivity panel"
)


prod_data_immigration<-melt(prod_data_immigration,id.vars=c("location_id", "avg_labor", "new_share","CSPOP", "county", "s_index","sol_type", "weight"), measure.vars=patterns("^B_[0-9]") )
prod_data_immigration[, multiplier:=avg_labor*CSPOP*new_share*weight]
prod_data_immigration[, worker_type:=str_replace(variable, "^B_[0-9]_","")]

## to get labor productivity need to adjust by 1.1 the value for the immigration group
prod_data_immigration[worker_type==1 & county==6037, value:=value/1.1]
prod_data_immigration[worker_type==5 & county==17031, value:=value/1.1]
prod_data_immigration[worker_type==3 & county==36061, value:=value/1.1]

firm_immigration<-prod_data_immigration[,.(tot_prod=sum(value)), c('s_index', "multiplier","location_id", "county", "sol_type")]
firm_immigration<-firm_immigration[, .(s_avg=weighted.mean(s_index, multiplier),labor_prod=sum(tot_prod*multiplier)), by=c("county", "sol_type")]
firm_immigration[, version:="Immigration"]



prod_data_immigration<-prod_data_immigration[, .(tot_prod=sum(multiplier*value)), by=c("county", "worker_type","sol_type")]

prod_data_immigration<-dcast(prod_data_immigration, county+sol_type~worker_type, value.var="tot_prod")
prod_data_immigration[, version:="Immigration"]


#### merger
wage_vect_merger<-read_counterfactual_rds(
  "05_06_wages_merger.rds",
  legacy_filenames = "05_06_wages_merger.rds",
  description = "merger counterfactual wages"
)
prod_data_merger<-read_counterfactual_rds(
  "05_06_prod_merger.rds",
  legacy_filenames = "05_06_prod_merger.rds",
  description = "merger counterfactual productivity panel"
)


prod_data_merger<-melt(prod_data_merger,id.vars=c("location_id", "avg_labor", "new_share","CSPOP", "county", "s_index","sol_type", "weight"), measure.vars=patterns("^B_[0-9]") )
prod_data_merger[, multiplier:=avg_labor*CSPOP*new_share*weight]
prod_data_merger[, worker_type:=str_replace(variable, "^B_[0-9]_","")]

firm_merger<-prod_data_merger[,.(tot_prod=sum(value)), c('s_index', "multiplier","location_id", "county", "sol_type")]
firm_merger<-firm_merger[, .(s_avg=weighted.mean(s_index, multiplier),labor_prod=sum(tot_prod*multiplier)), by=c("county", "sol_type")]
firm_merger[, version:="Incr. Concentration"]



prod_data_merger<-prod_data_merger[, .(tot_prod=sum(multiplier*value)), by=c("county", "worker_type","sol_type")]

prod_data_merger<-dcast(prod_data_merger, county+sol_type~worker_type, value.var="tot_prod")
prod_data_merger[, version:="Incr. Concentration"]


## make table based on percentages.
tot_table<-rbind(firm_salestax, firm_diffusion, firm_immigration, firm_merger)
colnames(firm_initial)<-c("county", "sol_type", "initial_s", "initial_prod")

tot_table<-merge(tot_table, firm_initial[, -"sol_type"], by="county")
tot_table[, pct_sindex:=(s_avg-initial_s)/initial_s]
tot_table[, pct_prod:=(labor_prod-initial_prod)/initial_prod]

tot_table<-dcast(tot_table, version+county~sol_type, value.var=c("pct_sindex", "pct_prod"))
setcolorder(tot_table, c("version", "county", "pct_sindex_realloc", "pct_prod_realloc"))
cols<-colnames(tot_table)[-c(1,2)]
tot_table[,(cols) := lapply(.SD, function(x) as.character(format(round(x   , 3), nsmall = 3))),.SDcols=cols]




tot_table[, county_name:=ifelse(county=="17031", "Cook",ifelse(county=="36061", "New York", "Los Angeles"))]
setcolorder(tot_table, "county_name")
colnames(tot_table)[-c(1,2,3)]<-c("S-Index Change", "Prod. Change", "S-Index Change", "Prod. Change")
setnames(tot_table, "county_name", "County")
setnames(tot_table, "version", "Counterfactual")

output<-kable(tot_table[,-"county"], "latex", align="c", booktabs=TRUE,linesep = c(""), escape = F, caption = NA, label=NA) 
output<-add_header_above(output,c(" "," ", "Reallocation" = 2, "Reorganization" = 2))
write_counterfactual_text(
  output,
  "05_06_tot_counterfactuals.tex",
  type = "tables",
  legacy_filename = "05_06_tot_counterfactuals.tex"
)

## changes by worker type.
wage_vect_merger[, version:="Incr. Concentration"]
wage_vect_salestax[, version:="Sales Tax"]
wage_vect_immigration[,version:="Immigration"]
wage_vect_diffusion[, version:="Management Diffusion"]
wage_changes<-rbind(wage_vect_merger,wage_vect_salestax,wage_vect_immigration,wage_vect_diffusion)
wage_changes<-merge(wage_changes, wage_vect_initial, by=c("county", "quarter_year"))
wage_changes[, wage1:=(`w1.x`-`w1.y`)/`w1.y`]
wage_changes[, wage2:=(`w2.x`-`w2.y`)/`w2.y`]
wage_changes[, wage3:=(`w3.x`-`w3.y`)/`w3.y`]
wage_changes[, wage4:=(`w4.x`-`w4.y`)/`w4.y`]
wage_changes[, wage5:=(`w5.x`-`w5.y`)/`w5.y`]

prod_data_merger[, version:="Incr. Concentration"]
prod_data_salestax[, version:="Sales Tax"]
prod_data_immigration[,version:="Immigration"]
prod_data_diffusion[, version:="Management Diffusion"]
prod_changes<-rbind(prod_data_merger,prod_data_salestax,prod_data_immigration,prod_data_diffusion)
prod_changes<-merge(prod_changes, prod_data_initial[,-c("sol_type", "version")], by=c("county"))
prod_changes[, prod1:=(`1.x`-`1.y`)/`1.y`]
prod_changes[, prod2:=(`2.x`-`2.y`)/`2.y`]
prod_changes[, prod3:=(`3.x`-`3.y`)/`3.y`]
prod_changes[, prod4:=(`4.x`-`4.y`)/`4.y`]
prod_changes[, prod5:=(`5.x`-`5.y`)/`5.y`]

het_prod<-merge(prod_changes, wage_changes[is.finite(w1.x),], by=c("county", "sol_type", "version"))
het_prod[, c("county","version" ,"sol_type", "prod1", "wage1", "prod2", "wage2", "prod3", "wage3", "prod4", "wage4", "prod5", "wage5")]
cols<-c("prod1", "wage1", "prod2", "wage2", "prod3", "wage3", "prod4", "wage4", "prod5", "wage5")
het_prod[,(cols) := lapply(.SD, function(x) as.character(format(round(x   , 3), nsmall = 3))),.SDcols=cols]
het_prod[, county_name:=ifelse(county=="17031", "Cook",ifelse(county=="36061", "New York", "Los Angeles"))]
setnames(het_prod, "county_name", "County")
setnames(het_prod, "version", "Counterfactual")
setorder(het_prod, "Counterfactual", "County")
het_prod<-het_prod[sol_type=="reorg",c("Counterfactual", "County","prod1", "wage1", "prod2", "wage2", "prod3", "wage3", "prod4", "wage4", "prod5", "wage5")]
setnames(het_prod, old=c("prod1", "wage1", "prod2", "wage2", "prod3", "wage3", "prod4", "wage4", "prod5", "wage5"),
         new=rep(c("Prod.","Wage"),5))
output<-kable(het_prod, "latex", align="c", booktabs=TRUE,linesep = c(""), escape = F, caption = NA, label=NA) 
output<-add_header_above(output,c(" "," ", "Skill Set 1" = 2, "Skill Set 2" = 2,"Skill Set 3" = 2,"Skill Set 4" = 2,"Skill Set 5" = 2 ))
write_counterfactual_text(
  output,
  "05_06_bytype_counterfactuals.tex",
  type = "tables",
  legacy_filename = "05_06_bytype_counterfactuals.tex"
)

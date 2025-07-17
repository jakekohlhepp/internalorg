library('data.table')
library('lubridate')
library('stringr')
library('zoo')
library('binsreg')
library('ggplot2')
library('lessR')
library('stats')
library('dendextend')
library('qgraph')
library('ivreg')

### perform bootstrap
## use bayesian bootstrap
## reweight and redo.
## use existing programs



working_data<-data.table(readRDS('analysis_final/data/01_01_working.rds'))


## setup the data and main functions
source('analysis_final/preamble.R')



## check that by hand and package estimator are the same for unweighted data to 9th decimal
xnam<-c("factor(county):factor(quarter_year)",paste0("factor(county):avg_labor:",names(working_data)[grep("^B_raw_[0-9]_", names(working_data))]))
xnam <- paste0("log_rel_mkt~",paste0(xnam, collapse="+"),"-1|factor(county):cust_price |factor(county):dye_instrument")
m_iv <- ivreg(eval(xnam),
              data = working_data)
stopifnot(all(round(coef(m_iv)-beta,9)==0))



## draw weights for each location.
set.seed(322186)
bootreps<-200

boot_weight<-data.table()
for (iter in 1:bootreps){
  temp_weight<-rexp(uniqueN(working_data$location_id),1)
  temp_weight<-temp_weight/sum(temp_weight)
  boot_weight<-rbind(boot_weight,data.table(iteration=iter,location_id=unique(working_data$location_id),bweight=temp_weight))
}
saveRDS(boot_weight,"analysis_final/data/03_03_boot_weights.rds")
boot_res<-data.table()

get_weight<-Vectorize(function(loc, i){
  return(as.numeric(boot_weight[location_id==loc & iteration==i]$bweight))
})
## tolerances and whether we should accelerate.
source("analysis_final/boot_settings.R")


for (iter in 1:bootreps){
  
  # get weights
  working_data[,boot_weight:=get_weight(location_id, iter)]
  
  ## do first regression.
  xnam<-c("factor(county):factor(quarter_year)",paste0("factor(county):avg_labor:",names(working_data)[grep("^B_raw_[0-9]_", names(working_data))]))
  xnam <- paste0("log_rel_mkt~",paste0(xnam, collapse="+"),"-1|factor(county):cust_price |factor(county):dye_instrument")
  m_iv <- ivreg(eval(xnam),
                data = working_data,weights=boot_weight)
  beta<-as.matrix(coef(m_iv), ncol=1)
  
  ## use the point estimates as starting point
  all_results<-readRDS('analysis_final/data/02_00_parameters.rds')
  starting<-all_results$coefficients
  names(starting)<-all_results$parm_name

  beta_2_subset<-starting[grep("avg_labor:E_raw_[2-9]",names(starting))]
  names(beta_2_subset)<-names(starting)[grep("avg_labor:E_raw_[2-9]",names(starting))]
  
  ## update objective to weight
  
  objective_squared<-function(parms){
    res<-colSums(sweep(objective_gmm(theta=parms, x=estim_matrix),MARGIN=1,working_data$boot_weight/sum(working_data$boot_weight),`*` ))
    return(sum(res^2))
  }
  
  objective_vect<-function(parms){
    return(colSums(sweep(objective_gmm(theta=parms, x=estim_matrix),MARGIN=1,working_data$boot_weight/sum(working_data$boot_weight),`*` )))
  } 
  
  
  res_store<-BBsolve(beta_2_subset,
                     objective_vect, control=list(trace=TRUE, tol=obj_tol, maxit=1000000))
  print(res_store)
  coef_vect<-res_store$par
  names(coef_vect)<-names(beta_2_subset)
  
  ## run auxilliary regression.
  working_data[, wb_2:=coef_vect[paste0("factor(county)", county, ":avg_labor:E_raw_2")]*E_raw_2*avg_labor]
  working_data[, wb_3:=coef_vect[paste0("factor(county)", county, ":avg_labor:E_raw_3")]*E_raw_3*avg_labor]
  working_data[, wb_4:=coef_vect[paste0("factor(county)", county, ":avg_labor:E_raw_4")]*E_raw_4*avg_labor]
  working_data[, wb_5:=coef_vect[paste0("factor(county)", county, ":avg_labor:E_raw_5")]*E_raw_5*avg_labor]
  working_data[, gamma_invert:=get_gammas(coef_vect, estim_matrix)]
  working_data[,p_adj:=cust_price-wb_2-wb_3-wb_4-wb_5-gamma_invert*s_index+mk_piece/beta[paste0("factor(county)",county, ":cust_price"),]]
  
  first_try<-coef(lm(p_adj~avg_labor:factor(county):factor(quarter_year)+factor(county):factor(quarter_year)+factor(quarter_year):(task_mix_2+task_mix_3+task_mix_4+task_mix_5)-1,
                     data=working_data, weights=boot_weight))
  
  # need constraints
  
  temp<-data.table(V1=names(first_try), position=1:length(first_try))
  temp[,quarter_year:=as.numeric(str_extract(V1, "\\b\\d{4}\\.\\d\\b"))][,county:=as.numeric(gsub("factor\\(county\\)", "",str_extract(V1, "factor\\(county\\)\\s*(\\d+)")))]
  temp<-temp[grep("^avg_labor", V1),]
  
  temp2<-data.table(V1=names(coef_vect), value=coef_vect)
  temp2<-rbind(temp2,data.table(V1=unique(str_replace(names(coef_vect), "E_raw_[0-9]","E_raw_1")), value=0) )
  
  temp2[,county:=as.numeric(gsub("factor\\(county\\)", "",str_extract(V1, "factor\\(county\\)\\s*(\\d+)")))]
  
  
  temp<-merge(temp2, temp, by="county", allow.cartesian = TRUE)
  temp<-merge(temp, min_wage, by=c("county", "quarter_year"), all.x=TRUE)
  
  temp_bounds<-temp[, .(lb=max(min_wage-value)), by="position"]
  lower_bound<-rep(-Inf,length(first_try))
  lower_bound[temp_bounds$position]<-temp_bounds$lb
  
  starting_final_reg<-lower_bound+10
  starting_final_reg[is.infinite(starting_final_reg)]<-0
  
  
  xnam <- as.formula("~avg_labor:factor(county):factor(quarter_year)+factor(quarter_year):factor(county)+factor(quarter_year):(task_mix_2+task_mix_3+task_mix_4+task_mix_5)-1")
  
  mod_mm_2<-model.matrix(xnam, data=working_data)
  obj_final_reg<-function(x){
    res<-colSums(sweep(matrix(working_data$p_adj - mod_mm_2%*%x, nrow=nrow(mod_mm_2),ncol=ncol(mod_mm_2), byrow=FALSE)*mod_mm_2,MARGIN=1,working_data$boot_weight/sum(working_data$boot_weight),`*` ))
    return(sum((res)^2))
  }
  
  
  final_reg<-optim(starting_final_reg,obj_final_reg, lower=lower_bound, upper=rep(Inf,length(starting_final_reg)),method="L-BFGS-B", control=list(maxit=1000000, trace=3,xtol=obj_tol))
  print(final_reg)
  coef_vect2<-final_reg$par
  names(coef_vect2)<-names(first_try)
  
  
  
  
  ### save boot res.
  boot_res<-rbind(boot_res, t(c(iter,c(beta,coef_vect,coef_vect2))))
  
  
  saveRDS(boot_res, "analysis_final/data/03_03_bootstrap.rds")
  
  
  print(paste0("Completed Boot reps: ", iter))

}


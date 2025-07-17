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

### perform bootstrap
## use existing programs


staff_task<-readRDS('analysis_final/data/01_00_staff_task.rds')
staffnum_xwalk<-readRDS('analysis_final/data/01_00_xwalk.rds')

## draw firms within counties.
set.seed(833927) # note that the seed is set within 01_00 so this seed seed MUST come after.
bootreps<-200
sampled_data<-data.table()
for (iter in 1:bootreps){
  for (cnty in unique(staff_task$county)){
    locs_to_sample<-unique(staff_task[county==cnty,]$location_id)
    locs<-sample(locs_to_sample, length(locs_to_sample),replace=TRUE)
    sampled_data<-rbind(sampled_data, cbind(rep(iter,length(locs_to_sample)),rep(cnty,length(locs_to_sample)) ,locs))
  }
}
colnames(sampled_data)<-c("boot_rep", "county", "location_id")
sampled_data[, psuedo_location_id:=paste0(location_id, " - ",1:.N), by="county"]

## will now modify the data with resampling and feed it into the next program.

bootall_staff_task<-merge(sampled_data, staff_task, by=c("county", "location_id"), allow.cartesian = TRUE)
bootall_staff_task[, location_id:=psuedo_location_id]
bootall_staff_task[, psuedo_location_id:=NULL]

bootall_xwalk<-merge(sampled_data[,-"county"], staffnum_xwalk, by=c("location_id"), allow.cartesian = TRUE)
bootall_xwalk[, location_id:=psuedo_location_id]
bootall_xwalk[, psuedo_location_id:=NULL]

boot_res<-data.table()


for (iter in 1:bootreps){
  
  staffnum_xwalk<-bootall_xwalk[boot_rep==iter,][,boot_rep:=NULL]
  staff_task<-bootall_staff_task[boot_rep==iter,][,boot_rep:=NULL]
  
  # cluster
  source('analysis_final/cluster.R')
  working_data<-copy(verywide_expanded)
  # clear environment except for important objects
  rm(list=setdiff(ls(), c("working_data","iter", "bootreps",
                          "bootall_xwalk","bootall_staff_task","boot_res")))
  
  ### setup gmm environment
  source('analysis_final/preamble.R')
  
  
  

  ########################################################### 
  ########################################################### Run gmm
  ########################################################### 
  
  ## tolerances and whether we should accelerate.
  source("analysis_final/boot_settings.R")

  ##### initialize
  
  # moments evaluated at point estimate
  all_results<-readRDS('analysis_final/data/02_00_parameters.rds')
  starting<-all_results$coefficients
  names(starting)<-all_results$parm_name

  beta_2_subset<-starting[grep("avg_labor:E_raw_[2-9]",names(starting))]
  names(beta_2_subset)<-names(starting)[grep("avg_labor:E_raw_[2-9]",names(starting))]
  
  
  
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
                     data=working_data))
  
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
    return(sum(colMeans( matrix(working_data$p_adj - mod_mm_2%*%x, nrow=nrow(mod_mm_2),ncol=ncol(mod_mm_2), byrow=FALSE)*mod_mm_2)^2))
  }
  
  
  final_reg<-optim(starting_final_reg,obj_final_reg, lower=lower_bound, upper=rep(Inf,length(starting_final_reg)),method="L-BFGS-B", control=list(maxit=1000000, trace=3,xtol=obj_tol))
  print(final_reg)
  coef_vect2<-final_reg$par
  names(coef_vect2)<-names(first_try)
  
  
  
  
  ### save boot res.
  boot_res<-rbind(boot_res, t(c(iter,c(beta,coef_vect,coef_vect2))))
  
  
  saveRDS(boot_res, "analysis_final/data/03_00_bootstrap.rds")
  
  
  print(paste0("Completed Boot reps: ", iter))

}


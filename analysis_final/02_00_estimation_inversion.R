## estimate with moments normalized.

library('data.table')
set.seed(4459665)
working_data<-data.table(readRDS('analysis_final/data/01_01_working.rds'))


## setup the data and main functions
source('analysis_final/preamble.R')




## set innertolerance, and choose if we want to accelerate the fixedpoint
innertol<-1e-08
outertol<-1e-04
obj_tol<-1e-02

accel_on<-TRUE
pl_on<-TRUE


## get starting point.
beta_2_subset<-readRDS('analysis_final/data/seeit_bb.rds')
# label based on analytic estimate.
names(beta_2_subset)<-rownames(beta_2)[grep("avg_labor:E_raw_[2-9]{1}\\d{0,3}",rownames(beta_2))]




if (get_os()=="windows"){
  clust <- makeCluster(core_count)
  clusterExport(clust, ls())
  clusterEvalQ(clust, library("SQUAREM"))
}


## tolerance for the outerloop
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


### reassemble all 
res_all<-data.table(demand=c(rep(TRUE,length(rownames(beta)) ),rep(FALSE,length(c(coef_vect,coef_vect2)))),parm_name=c(rownames(beta),names(coef_vect),names(coef_vect2)), coefficients=c(as.numeric(beta),coef_vect,coef_vect2))
saveRDS(res_all, 'analysis_final/data/02_00_parameters.rds')

## estimate with moments normalized.

library('data.table')
set.seed(4459665)

## load estimation-ready dataset assembled by 04_estimation_sample.R
source('config.R')
estimation_sample <- readRDS(file.path(CONFIG$prep_output_dir, "04_estimation_sample.rds"))
working_data  <- estimation_sample$working_data
estim_matrix  <- estimation_sample$estim_matrix
quarter_count <- estimation_sample$quarter_count
county_count  <- estimation_sample$county_count
skill_count   <- estimation_sample$skill_count

## setup the data and main functions
source('preamble.R')




## get starting point.
beta_2_subset<-readRDS('mkdata/data/seeit_bb.rds')
# label based on analytic estimate.
names(beta_2_subset)<-rownames(beta_2)[grep("avg_labor:E_raw_[2-9]{1}\\d{0,3}",rownames(beta_2))]




if (get_os()=="windows"){
  clust <- makeCluster(get_core_count(CONFIG))
  on.exit(stopCluster(clust), add = TRUE)  # Ensure cleanup on exit
  clusterExport(clust, ls())
  clusterEvalQ(clust, library("SQUAREM"))
} else {
  clust <- NULL
}

# Define objective closures capturing local state
objective_vect <- function(parms) {
  return(colMeans(objective_gmm(theta=parms, x=estim_matrix, beta=beta,
                                beta_2_subset=beta_2_subset, config=CONFIG, clust=clust)))
}

objective_squared <- function(parms) {
  res <- colMeans(objective_gmm(theta=parms, x=estim_matrix, beta=beta,
                                beta_2_subset=beta_2_subset, config=CONFIG, clust=clust))
  return(sum(res^2))
}

## tolerance for the outerloop
res_store<-BBsolve(beta_2_subset,
                   objective_vect, control=list(trace=TRUE, tol=CONFIG$obj_tol, maxit=1000000))
print(res_store)

# Check convergence
if (res_store$convergence != 0) {
  warning("BBsolve did not converge. Convergence code: ", res_store$convergence,
          "\nResults may be unreliable. Consider adjusting tolerances or starting values.")
}

coef_vect<-res_store$par
names(coef_vect)<-names(beta_2_subset)



## run auxilliary regression.
for (i in 2:CONFIG$n_worker_types) {
  col_name <- paste0("wb_", i)
  e_raw_col <- paste0("E_raw_", i)
  working_data[, (col_name) := coef_vect[paste0("factor(county)", county, ":avg_labor:", e_raw_col)] * get(e_raw_col) * avg_labor]
}
working_data[, gamma_invert:=get_gammas(coef_vect, estim_matrix, beta=beta, beta_2_subset=beta_2_subset, config=CONFIG, clust=clust)]
wb_cols <- paste0("wb_", 2:CONFIG$n_worker_types)
wb_sum <- Reduce("+", lapply(wb_cols, function(col) working_data[[col]]))
working_data[,p_adj:=cust_price - wb_sum - gamma_invert*s_index+mk_piece/beta[paste0("factor(county)",county, ":cust_price"),]]

task_mix_formula_str <- build_task_mix_sum(CONFIG)
first_try_formula <- as.formula(paste0("p_adj~avg_labor:factor(county):factor(quarter_year)+factor(county):factor(quarter_year)+factor(quarter_year):(", task_mix_formula_str, ")-1"))
first_try<-coef(lm(first_try_formula, data=working_data))

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


xnam <- as.formula(paste0("~avg_labor:factor(county):factor(quarter_year)+factor(quarter_year):factor(county)+factor(quarter_year):(", task_mix_formula_str, ")-1"))

mod_mm_2<-model.matrix(xnam, data=working_data)
obj_final_reg<-function(x){
  return(sum(colMeans( matrix(working_data$p_adj - mod_mm_2%*%x, nrow=nrow(mod_mm_2),ncol=ncol(mod_mm_2), byrow=FALSE)*mod_mm_2)^2))
}


final_reg<-optim(starting_final_reg,obj_final_reg, lower=lower_bound, upper=rep(Inf,length(starting_final_reg)),method="L-BFGS-B", control=list(maxit=1000000, trace=3,factr=CONFIG$obj_tol/.Machine$double.eps))
print(final_reg)

# Check convergence (0 = success for optim)
if (final_reg$convergence != 0) {
  warning("L-BFGS-B did not converge. Convergence code: ", final_reg$convergence,
          "\nMessage: ", final_reg$message,
          "\nResults may be unreliable.")
}

coef_vect2<-final_reg$par
names(coef_vect2)<-names(first_try)


### reassemble all
res_all<-data.table(demand=c(rep(TRUE,length(rownames(beta)) ),rep(FALSE,length(c(coef_vect,coef_vect2)))),parm_name=c(rownames(beta),names(coef_vect),names(coef_vect2)), coefficients=c(as.numeric(beta),coef_vect,coef_vect2))
if (!dir.exists('results/data')) {
  dir.create('results/data', recursive = TRUE)
}
saveRDS(res_all, 'results/data/05_parameters.rds')


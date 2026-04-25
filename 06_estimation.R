#' =============================================================================
#' STEP 06: Structural Estimation
#' =============================================================================
#' Estimates the structural model on the estimation-ready sample assembled by
#' 04_estimation_sample.R.
#'
#' Inputs:
#'   - mkdata/data/04_estimation_sample.rds
#'   - mkdata/data/seeit_bb.rds
#'
#' Output:
#'   - results/data/06_parameters.rds
#' =============================================================================

library('data.table')
set.seed(4459665)

source('config.R')

estimation_sample_path <- file.path(CONFIG$prep_output_dir, "04_estimation_sample.rds")
assert_required_files(estimation_sample_path)

## 04_estimation_sample.R owns all estimation-only data assembly. This script
## only estimates from the saved artifact and the shared numerical helpers.
estimation_sample <- readRDS(estimation_sample_path)
working_data <- estimation_sample$working_data
estim_matrix <- estimation_sample$estim_matrix
if (!"min_wage_levels" %in% names(estimation_sample)) {
  min_wage_levels <- unique(data.table(working_data)[, .(county, quarter_year, min_wage)])
} else {
  min_wage_levels <- data.table(estimation_sample$min_wage_levels)
}
stopifnot(uniqueN(min_wage_levels[, .(county, quarter_year)]) == nrow(min_wage_levels))

source('preamble.R')
estimation_objects <- build_estimation_setup(working_data, estim_matrix, config = CONFIG)
beta <- estimation_objects$beta
beta_2 <- estimation_objects$beta_2
rm(estimation_objects)




## get starting point.
beta_2_subset<-readRDS('mkdata/data/seeit_bb.rds')
# label based on analytic estimate.
names(beta_2_subset)<-rownames(beta_2)[grep("avg_labor:E_raw_[2-9]{1}\\d{0,3}",rownames(beta_2))]




if (get_os()=="windows"){
  clust <- makeCluster(get_core_count(CONFIG))
  on.exit(stopCluster(clust), add = TRUE)  # Ensure cleanup on exit
  clusterExport(clust, ls())
  invisible(clusterEvalQ(clust, library("SQUAREM")))
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

if (isTRUE(CONFIG$check_structural_start)) {
  starting_moments <- objective_vect(beta_2_subset)
  message("Starting wage moment squared norm: ", signif(sum(starting_moments^2), 6))
}

if (!isTRUE(CONFIG$skip_structural_optimizer)) {
  ## tolerance for the outerloop
  res_store<-estimate_wage_parameters(
    beta_2_subset,
    estim_matrix,
    beta,
    beta_2_subset,
    config = CONFIG,
    clust = clust
  )
  print(res_store)

  # Check convergence
  if (res_store$convergence != 0) {
    warning("BBsolve did not converge. Convergence code: ", res_store$convergence,
            "\nResults may be unreliable. Consider adjusting tolerances or starting values.")
  }
} else {
  message(
    "Skipping BBsolve and reusing mkdata/data/seeit_bb.rds because ",
    "JMP_SKIP_STRUCTURAL_OPTIMIZER=true."
  )
  res_store <- list(par = beta_2_subset, convergence = NA_integer_)
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
## the minimum-wage lower bounds come from the saved 04 artifact so this
## regression does not depend on a hidden spreadsheet read from preamble.R.
min_wage_bounds <- copy(min_wage_levels)
min_wage_bounds[, county := as.numeric(county)]
temp<-merge(temp, min_wage_bounds, by=c("county", "quarter_year"), all.x=TRUE)
stopifnot(nrow(temp[is.na(min_wage)]) == 0)

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
ensure_directory('results/data')
saveRDS(res_all, 'results/data/06_parameters.rds')

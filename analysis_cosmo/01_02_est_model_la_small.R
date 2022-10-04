## estimate model. only do 2021.2 NY.

library('data.table')
library('gmm')
set.seed(4466)

#### general functions###########
get_os <- function(){
  sysinf <- Sys.info()
  if (!is.null(sysinf)){
    os <- sysinf['sysname']
    if (os == 'Darwin')
      os <- "osx"
  } else { ## mystery machine
    os <- .Platform$OS.type
    if (grepl("^darwin", R.version$os))
      os <- "osx"
    if (grepl("linux-gnu", R.version$os))
      os <- "linux"
  }
  tolower(os)
}
wage_bound<-Vectorize(function(x){
  return(185/(exp(-x)+1)+15)
})
#################################

### read in and limit main firm-based data
firm_quarter<-readRDS("data/00_00_firm_quarter.rds")

# limit just to 2019.4, LA for now
estim_sample<-firm_quarter[quarter_year %in% c(2021.2) & county %in% c(6037)]
# cannot use salons that do not record price.
estim_sample<-estim_sample[ cust_price>0]
stopifnot(nrow(estim_sample[is.na(s_index)])==0)
# cannot use salons that do not record price.

table(estim_sample$county)
# need to convert to matrix to feed into gmm package
estim_matrix<-as.matrix(estim_sample[,c("s_index", "task_mix2", "task_mix3", "task_mix4", "task_mix5","task_mix6",
                                        "cust_price", "salon_share_subdiv","outside_share",
                                        "avg_labor","avg_wage_qtr", "cust_count")])


# sales tax rate - 4.5% in ny
tau<-1.045


############### tolerance preferences
innertol<-1e-08
outertol<-1e-08
if (get_os()=="windows"){
  core_count<-1
} else{
  core_count<-44
} 

source('small_gmm_objective.R')
###############

############### parameters that are fixed
c0<-mean(estim_matrix[,7])
u0<-mean(log(estim_matrix[,8]/estim_matrix[,9]))

starting<-readRDS('data/best_starting_ny.rds')
###############
test<-g(starting, estim_matrix)
stopifnot(all(test<Inf))

weight_mat<-c(1,diag(var(cbind(estim_matrix[,2:6],estim_matrix[,1],estim_matrix[,2:6]*estim_matrix[,1], estim_matrix[,10]))))
weight_mat<-diag(c(weight_mat,weight_mat,var(estim_matrix[,10]) ))^(1/2)

  
##### gmm evaluation. use weights equal to variance of moment.

res_store<-gmm(g, x=estim_matrix, t0=starting, prewhite=0,optfct="optim",method="BFGS",centeredVcov='TRUE',weightsMatrix = weight_mat,
               control=list(abstol=1e-09,reltol=1e-09, trace=1, maxit=2000))
save(res_store, innertol, outertol,tau, wage_bound,estim_sample, estim_matrix, file="data/01_02_progress.RData")


for (i in 1:20){
  

  res_store<-gmm(g, x=estim_matrix, t0=coef(res_store), prewhite=0,optfct="optim",centeredVcov='TRUE',weightsMatrix = weight_mat,
                 control=list(abstol=1e-09,reltol=1e-09, trace=1, maxit=2000))
  save(res_store, innertol, outertol,tau, wage_bound,estim_sample, estim_matrix, file="data/01_02_progress.RData")
  
  
  res_store<-gmm(g, x=estim_matrix, t0=coef(res_store), prewhite=0,optfct="optim",method="BFGS",centeredVcov='TRUE',weightsMatrix = weight_mat,
                 control=list(abstol=1e-09,reltol=1e-09, trace=1, maxit=2000))
  save(res_store, innertol, outertol,tau, wage_bound,estim_sample, estim_matrix, file="data/01_02_progress.RData")
  
  
}

save(res_store, innertol, outertol,tau, wage_bound,estim_sample, estim_matrix, file="data/01_02_final_la.RData")






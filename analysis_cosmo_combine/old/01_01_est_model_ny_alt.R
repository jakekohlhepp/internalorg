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
estim_sample<-firm_quarter[quarter_year %in% c(2021.2) & county %in% c(36061)]

stopifnot(nrow(estim_sample[is.na(s_index)])==0)
stopifnot(nrow(estim_sample[cust_price<=0])==0)
# cannot use salons that do not record price.

table(estim_sample$county)
# need to convert to matrix to feed into gmm package
estim_matrix<-as.matrix(estim_sample[,c("s_index", "task_mix2", "task_mix3", "task_mix4", "task_mix5",
                                        "cust_price", "salon_share_subdiv","outside_share",
                                        "avg_labor","avg_wage_qtr", "cust_count")])


# sales tax rate - 4.5% in ny
tau<-1.045


############### tolerance preferences
innertol<-1e-10
outertol<-1e-10
if (get_os()=="windows"){
  core_count<-1
} else{
  core_count<-42
} 

source('small_gmm_objective_mod2.R')
###############

############### parameters that are fixed

starting<-rep(0,21)
starting[1]<-0.01
starting[4:8]<-rep(-1, 5)
starting[17:21]<-rep(50,5)

###############
test<-g(starting, estim_matrix)
stopifnot(all(test<Inf))

weight_mat<-c(1,diag(var(cbind(estim_matrix[,2:5],estim_matrix[,1],estim_matrix[,2:5]*estim_matrix[,1], estim_matrix[,9]))))
weight_mat<-diag(c(weight_mat,weight_mat,var(estim_matrix[,9]) ))^(1/2)

  
##### gmm evaluation. use weights equal to variance of moment.
#res_store<-gmm(g, x=estim_matrix, t0=starting, prewhite=0,optfct="optim",method="BFGS",centeredVcov='TRUE',weightsMatrix = weight_mat,
#               lower=c(rep(0,3),rep(15,5), rep(-100000,8), rep(0,5)),
#               upper=c(rep(100000,3),rep(200,5), rep(100000,8), rep(100000,5)),
#               control=list(abstol=1e-16,reltol=1e-20, trace=1, maxit=4000))
#optim(starting,gmm_obj, lower=c(rep(0,3),rep(15,5), rep(-100000,8), rep(0,5)),
#      upper=c(rep(100000,3),rep(200,5), rep(100000,8), rep(100000,5)), data=estim_matrix)
res_store<-gmm(g, x=estim_matrix, t0=starting, prewhite=0,optfct="optim",method="BFGS",centeredVcov='TRUE',weightsMatrix = weight_mat,
               control=list(abstol=1e-16,reltol=1e-20, trace=1, maxit=4000))
save(res_store, innertol, outertol,tau, wage_bound,estim_sample, estim_matrix, file="data/01_01_progress_alt.RData")


for (i in 1:50){
  

  res_store<-gmm(g, x=estim_matrix, t0=coef(res_store), prewhite=0,optfct="optim",centeredVcov='TRUE',weightsMatrix = weight_mat,
                 control=list(abstol=1e-16,reltol=1e-20, trace=1, maxit=1000))
  save(res_store, innertol, outertol,tau, wage_bound,estim_sample, estim_matrix, file="data/01_01_progress_alt.RData")
  
  res_store<-gmm(g, x=estim_matrix, t0=coef(res_store), prewhite=0,optfct="optim",method="BFGS",centeredVcov='TRUE',weightsMatrix = weight_mat,
                 control=list(abstol=1e-16,reltol=1e-20, trace=1, maxit=1000))
  save(res_store, innertol, outertol,tau, wage_bound,estim_sample, estim_matrix, file="data/01_01_progress_alt.RData")
  
  
}

save(res_store, innertol, outertol,tau, wage_bound,estim_sample, estim_matrix, file="data/01_01_final_ny_alt.RData")






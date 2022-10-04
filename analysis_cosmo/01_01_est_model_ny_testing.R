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
# cannot use salons that do not record price.
estim_sample<-estim_sample[ cust_price>0]
stopifnot(nrow(estim_sample[is.na(s_index)])==0)
# cannot use salons that do not record price.

table(estim_sample$county)
# need to convert to matrix to feed into gmm package
estim_matrix<-as.matrix(estim_sample[,c("s_index", "task_mix2", "task_mix3", "task_mix4", "task_mix5","task_mix6",
                                        "cust_price", "salon_share_subdiv",
                                        "avg_labor")])
## should be full column rank
stopifnot(ncol(estim_matrix)==qr(estim_matrix)$rank)
## save values that do not vary across salons
stopifnot(all(estim_sample$avg_wage_qtr[1]==estim_sample$avg_wage_qtr))
stopifnot(all(estim_sample$outside_share[1]==estim_sample$outside_share))
stopifnot(all(estim_sample$CSPOP[1]==estim_sample$CSPOP))
wage_match<-estim_sample$avg_wage_qtr[1]
outside_share<-estim_sample$outside_share[1]
pop_subdiv<-estim_sample$CSPOP[1]

# sales tax rate - 4.5% in ny
tau<-1.045


############### tolerance preferences
innertol<-1e-10
outertol<-1e-10
if (get_os()=="windows"){
  core_count<-1
} else{
  core_count<-100
} 

source('final_gmm_objective.R')
###############

############### parameters that are fixed

starting<-rep(0,25)
starting[1]<-0.1
starting[4:9]<-rep(-1, 6)
starting[20:25]<-rep(30,6)

starting<-readRDS('data/best_starting_ny.rds')

###############
test<-g(starting, estim_matrix)
stopifnot(all(test<Inf))

weight_mat<-c(1,diag(var(cbind(estim_matrix[,2:6],estim_matrix[,1],estim_matrix[,2:6]*estim_matrix[,1], estim_matrix[,9]))))
weight_mat<-diag(c(weight_mat,weight_mat,var(estim_matrix[,9]) ))^(1/2)
sum(t(colMeans(test))%*%weight_mat%*%colMeans(test))
  
##### gmm evaluation. use weights equal to variance of moment.

gmm_obj<-function(x, data){
  hold<-g(parms=x,x=data)
  hold<-sum(t(colMeans(hold))%*%weight_mat%*%colMeans(hold))
  return(hold)
}

----
  res_store<-gmm(g, x=estim_matrix, t0=starting, prewhite=0,optfct="optim",centeredVcov='TRUE',weightsMatrix = weight_mat,
                 control=list(abstol=1e-11,reltol=1e-11, trace=1, maxit=10))
library('numDeriv')
grad_test<-grad(gmm_obj,coef(res_store), data=estim_matrix)
jac_test<-jacobian(gmm_moments, starting)
hess_test<-hessian(gmm_obj, starting)
chol(hess_test, pivot="TRUE")

mom_vcov<-cov(test)
M<-solve(t(jac_test)%*%weight_mat%*%jac_test)

M<-solve(t(res_store$G)%*%res_store$G)
res_store<-gmm(g, x=estim_matrix, t0=starting, prewhite=0,optfct="optim",method="BFGS",centeredVcov='TRUE',weightsMatrix = weight_mat,
               control=list(abstol=1e-11,reltol=1e-11, trace=1, maxit=1000))
save(res_store, innertol, outertol,tau, wage_bound,estim_sample, estim_matrix, file="data/01_01_progress.RData")


for (i in 1:20){
  

  res_store<-gmm(g, x=estim_matrix, t0=coef(res_store), prewhite=0,optfct="optim",centeredVcov='TRUE',weightsMatrix = weight_mat,
                 control=list(abstol=1e-11,reltol=1e-11, trace=1, maxit=1000))
  save(res_store, innertol, outertol,tau, wage_bound,estim_sample, estim_matrix, file="data/01_01_progress.RData")
  
  res_store<-gmm(g, x=estim_matrix, t0=coef(res_store), prewhite=0,optfct="optim",method="BFGS",centeredVcov='TRUE',weightsMatrix = weight_mat,
                 control=list(abstol=1e-11,reltol=1e-11, trace=1, maxit=1000))
  save(res_store, innertol, outertol,tau, wage_bound,estim_sample, estim_matrix, file="data/01_01_progress.RData")
  
  
}

save(res_store, innertol, outertol,tau, wage_bound,estim_sample, estim_matrix, file="data/01_01_final_ny.RData")






## estimate model. only do 2021.2 NY.

library('data.table')
library('gmm')
set.seed(2332)

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
  return(190/(exp(-x)+1)+10)
})
#################################

### read in and limit main firm-based data
firm_quarter<-readRDS("data/00_00_firm_quarter.rds")

# limit just to 2019.4, LA for now
estim_sample<-firm_quarter[quarter_year %in% c(2021.2) & county %in% c(6037)]

# cannot identify when s_index is 0 or infinite
estim_sample<-estim_sample[!is.nan(s_norm)   & cust_price>0]
# cannot use salons that do not record price.

table(estim_sample$county)
# need to convert to matrix to feed into gmm package
estim_matrix<-as.matrix(estim_sample[,c("s_index", "task_mix2", "task_mix3", "task_mix4", "task_mix5","task_mix6",
                                        "cust_price", "salon_share_subdiv","outside_share", 
                                        "hausman_all","avg_labor","avg_wage_qtr", "cust_count")])


# sales tax rate - 4.5% in ny
tau<-1.045


############### tolerance preferences
innertol<-1e-08
outertol<-1e-08
if (get_os()=="windows"){
  core_count<-1
} else{
  core_count<-32
} 

source('small_gmm_objective.R')
###############

############### parameters that are fixed
c0<-mean(estim_matrix[,7])
u0<-mean(log(estim_matrix[,8]/estim_matrix[,9]))

starting<-c()
starting[1]<-0.01
starting[2:3]<-0
starting[4:9]<--1
starting[10:14]<-0
starting[15:19]<-0
starting[20:25]<-30
#starting<-readRDS('data/best_starting_ny.rds')

###############
test<-g(starting, estim_matrix)
stopifnot(all(test<Inf))

### do once with identity matrix.
prelim<-gmm(g, x=estim_matrix, t0=starting, prewhite=0, type="twoStep",optfct="optim",wmatrix='ident',centeredVcov='TRUE',
            control=list(abstol=1e-07,reltol=1e-07, trace=1, maxit=2000))
prelim<-gmm(g, x=estim_matrix, t0=coef(prelim), prewhite=0, type="twoStep",optfct="optim",method="BFGS", wmatrix='ident',centeredVcov='TRUE',
            control=list(abstol=1e-07,reltol=1e-07, trace=1, maxit=2000))
prelim<-gmm(g, x=estim_matrix, t0=coef(prelim), prewhite=0, type="twoStep",optfct="optim", wmatrix='ident',centeredVcov='TRUE',
            control=list(abstol=1e-07,reltol=1e-07, trace=1, maxit=2000))
gmat<-g(starting, estim_matrix)
stopifnot(all(gmat<Inf))
moment_weights<-diag(diag(cov(gmat))^(-1))

############### main evaluation function

##### gmm evaluation. use weights equal to variance of moment.

res_store<-gmm(g, x=estim_matrix, t0=starting, prewhite=0,optfct="optim",centeredVcov='TRUE',weightsMatrix=moment_weights,
               control=list(abstol=1e-07,reltol=1e-07, trace=1, maxit=2000))
save(res_store, file="data/01_02_progress.RData")


for (i in 1:20){
  
  res_store<-gmm(g, x=estim_matrix, t0=coef(res_store), prewhite=0,optfct="optim",centeredVcov='TRUE',weightsMatrix=moment_weights,
                 control=list(abstol=1e-07,reltol=1e-07, trace=1, maxit=2000))
  save(res_store, innertol, outertol,tau, wage_bound,estim_sample, estim_matrix,moment_weights, file="data/01_02_progress.RData")
  
  res_store<-gmm(g, x=estim_matrix, t0=coef(res_store), prewhite=0,optfct="optim",method="BFGS",centeredVcov='TRUE',weightsMatrix=moment_weights,
                 control=list(abstol=1e-07,reltol=1e-07, trace=1, maxit=30))
  save(res_store, innertol, outertol,tau, wage_bound,estim_sample, estim_matrix,moment_weights, file="data/01_02_progress.RData")
  
  
}

save(res_store, innertol, outertol,tau, wage_bound,estim_sample, estim_matrix,moment_weights, "data/01_01_final_la.RData")






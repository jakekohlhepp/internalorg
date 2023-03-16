## estimate model. only do 2021.2 NY.
## run again with new starting position

library('data.table')
library('gmm')
set.seed(554)

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
# cannot use salons that do not record price.
estim_sample<-estim_sample[ cust_price>0]
stopifnot(nrow(estim_sample[is.na(s_index)])==0)


table(estim_sample$county)
# need to convert to matrix to feed into gmm package
estim_matrix<-as.matrix(estim_sample[,c("s_index", "task_mix2", "task_mix3", "task_mix4", "task_mix5","task_mix6",
                                        "cust_price", "salon_share_subdiv","outside_share", 
                                        "hausman_all","avg_labor","avg_wage_qtr", "cust_count")])


# sales tax rate - 0% in ny
tau<-1


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

starting<-readRDS('data/la_newstart_alt.rds')
starting[1]<-0.04
###############
test<-g(starting, estim_matrix)
stopifnot(all(test<Inf))


##### gmm evaluation. use weights equal to variance of moment.

res_store<-gmm(g, x=estim_matrix, t0=starting, prewhite=0,optfct="optim",centeredVcov='TRUE',wmatrix='ident',
               control=list(abstol=1e-07,reltol=1e-07, trace=1, maxit=2000))
save(res_store, innertol, outertol,tau, wage_bound,estim_sample, estim_matrix, file="data/01_02b_progress.RData")


for (i in 1:20){
  

  res_store<-gmm(g, x=estim_matrix, t0=coef(res_store), prewhite=0,optfct="optim",method="BFGS",centeredVcov='TRUE',wmatrix='ident',
                 control=list(abstol=1e-07,reltol=1e-07, trace=1, maxit=2000))
  save(res_store, innertol, outertol,tau, wage_bound,estim_sample, estim_matrix, file="data/01_02b_progress.RData")
  
  res_store<-gmm(g, x=estim_matrix, t0=coef(res_store), prewhite=0,optfct="optim",centeredVcov='TRUE',wmatrix='ident',
                 control=list(abstol=1e-07,reltol=1e-07, trace=1, maxit=2000))
  save(res_store, innertol, outertol,tau, wage_bound,estim_sample, estim_matrix, file="data/01_02b_progress.RData")
  
  
  
  
}
save(res_store, innertol, outertol,tau, wage_bound,estim_sample, estim_matrix, "data/01_02b_final_la.RData")






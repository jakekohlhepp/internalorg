## randomly perturbing starting values.

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

load('data/final_estimates.RData')


############### tolerance preferences
innertol<-1e-09
outertol<-1e-08
if (get_os()=="windows"){
  core_count<-1
} else{
  core_count<-42
} 

source('small_gmm_objective_less.R')
###############

############### parameters that are fixed

starting<-coef(res_store)
###############

weight_mat<-c(1,diag(var(cbind(estim_matrix[,2:5],estim_matrix[,1],estim_matrix[,2:5]*estim_matrix[,1]))))
weight_mat<-diag(c(weight_mat,weight_mat,var(estim_matrix[,9]) ))^(1/2)

##### randomly perturb starting values 100 times - use uniform pertubations between -1% and +1%.
allres<-c(0,starting,starting)
for (i in 1:100){
  perturbed<-runif(length(starting), min=as.numeric(starting-abs(starting)*0.01), max=as.numeric(starting+abs(starting)*0.01))
  new_coefs<-gmm(g, x=estim_matrix, t0=perturbed, prewhite=0,optfct="optim",centeredVcov='TRUE',weightsMatrix = weight_mat,
                 control=list(abstol=1e-08,reltol=1e-06, trace=1, maxit=4000))
  new_coefs<-res_store
  allres<-rbind(allres,c(i,perturbed,coef(new_coefs)))
  print(i)
  saveRDS(allres, 'data/01_04_random_starts.rds')
}
allres<-data.table(allres)
colnames(allres)<-c("iter",paste0("start", 1:length(starting)),paste0("end", 1:length(starting)))
saveRDS(allres, 'data/01_04_random_starts.rds')


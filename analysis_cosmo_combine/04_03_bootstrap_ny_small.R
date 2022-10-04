## estimate model. only do 2021.2 NY.

library('data.table')
library('gmm')
set.seed(13244)

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
if (get_os()=="windows"){
  core_count<-1
} else{
  core_count<-20
} 
#################################

############### parameters that are fixed
load('data/final_estimates.RData')
point_estimate<-coef(res_store)
weight_mat<-c(1,diag(var(cbind(estim_matrix[,2:5],estim_matrix[,1],estim_matrix[,2:5]*estim_matrix[,1]))))
weight_mat<-diag(c(weight_mat,weight_mat,var(estim_matrix[,9]) ))^(1/2)
###############


############### tolerance preferences - need to reset for bootstrap
innertol<-1e-10
outertol<-1e-08

source('gmm_boot_diff.R')
###############







### resample

boot_piece<-mclapply(1:40, function(x){
  sampled_ids<-sample(1:nrow(estim_matrix),size=nrow(estim_matrix),replace=TRUE)
  sample_estim<-estim_matrix[sampled_ids,]
  boot_piece<-gmm(g, x=sample_estim, t0=point_estimate, prewhite=0,optfct="optim",centeredVcov='TRUE',weightsMatrix = weight_mat,onlyCoefficients=TRUE,
                  control=list(abstol=1e-08,reltol=1e-08, trace=1, maxit=4000))
  return(coef(boot_piece))
  
}, mc.cores=core_count)


save(boot_piece,file='data/04_03_boot_res.Rdata')

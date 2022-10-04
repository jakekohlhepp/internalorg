## estimate model. only do 2021.2 NY.

library('data.table')
library('gmm')
set.seed(4434343)

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
  core_count<-42
} 
#################################

############### parameters that are fixed
load('data/final_estimates.RData')
point_estimate<-coef(res_store)
weight_mat<-c(1,diag(var(cbind(estim_matrix[,2:5],estim_matrix[,1],estim_matrix[,2:5]*estim_matrix[,1]))))
weight_mat<-diag(c(weight_mat,weight_mat,var(estim_matrix[,9]) ))^(1/2)
###############


innertol<-1e-10
outertol<-1e-08

source('gmm_boot.R')
###############







### resample
boot_res <- vector(mode = "list", length = 100)

for (i in 1:43){
  sampled_ids<-sample(1:nrow(estim_matrix),size=nrow(estim_matrix),replace=TRUE)
  sample_estim<-estim_matrix[sampled_ids,]
  boot_piece<-gmm(g, x=sample_estim, t0=point_estimate, prewhite=0,optfct="optim",centeredVcov='TRUE',weightsMatrix = weight_mat,onlyCoefficients=TRUE,
                  control=list(abstol=1e-08,reltol=1e-08, trace=1, maxit=4000))
  boot_res[[i]]<-coef(boot_piece)
  save(boot_res,file='data/04_05_boot_res.Rdata')
}

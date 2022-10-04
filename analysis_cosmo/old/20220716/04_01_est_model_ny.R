## estimate model. use alternative method.

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
  return(195/(exp(-x)+1)+5)
})
#################################

### read in and limit main firm-based data
firm_quarter<-readRDS("data/00_00_firm_quarter.rds")

# limit just to 2019.4, LA for now
estim_sample<-firm_quarter[quarter_year %in% c(2019.3,2019.4, 2021.1, 2021.2) & county %in% c(36061)]
# time indicators
estim_sample[, qt1:=quarter_year==2019.3]
estim_sample[, qt2:=quarter_year==2019.4]
estim_sample[, qt3:=quarter_year==2021.1]

# cannot identify when s_index is 0 or infinite
estim_sample<-estim_sample[!is.nan(s_norm)   & cust_price>0]
# cannot use salons that do not record price.

table(estim_sample$county)
# need to convert to matrix to feed into gmm package
estim_matrix<-as.matrix(estim_sample[,c("s_index", "task_mix2", "task_mix3", "task_mix4", "task_mix5","task_mix6",
                                        "cust_price", "salon_share_subdiv","outside_share","qt1", "qt2", "qt3", 
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
###############

############### parameters that are fixed
c0<-mean(estim_matrix[,7])
u0<-mean(log(estim_matrix[,8]/estim_matrix[,9]))

starting<-c()
starting[1]<-0.1
starting[2]<- -30
starting[3]<- 137
starting[4:9]<- -2.35
starting[10:15]<- -2.35
starting[16:21]<- -2.6
starting[22:27]<- -2.8
starting[28]<-0
starting[29]<-0
starting[30]<-0
starting[31]<-0
starting[32]<-0
starting[33]<-0
starting[34:58]<-0
starting[59:82]<-60

## uncomment to start at saved values.
#starting<-readRDS('data/best_starting_ny.rds')

###############



############### main evaluation function
source('big_gmm_objective.R')
# check that starting works. and get g matrix.
test<-g(starting, estim_matrix)
stopifnot(all(test<Inf))
#
check<-colMeans(test)
sum(check^2)
m1<-c(rep("cost_", 112/2),rep("util_", 112/2))
m2<-rep(c(rep("all",14),rep("q2",14),rep("q3",14),rep("q4",14)),2)
m3<-rep(c("cons", "a2","a3", "a4", "a5", "a6", 
          "sindex", "a2s","a3s", "a4s", "a5s", "a6s",
          "avg_labor","haus"),8)
moment_names<-c(paste0(m1,m2,m3), paste0("wage_",c("all", "q1", "q2", "q3")))
tolook<-data.table(names=moment_names,values= as.numeric(check), abs_value=abs(as.numeric(check)))
#View(tolook)
###############

##### gmm evaluation. use weights equal to variance of moment.

res_store<-gmm(g, x=estim_matrix, t0=starting, prewhite=0,optfct="optim",method="BFGS",centeredVcov='TRUE',weightsMatrix=diag(diag(cov(test))^(-1)),
               control=list(abstol=1e-07,reltol=1e-07, trace=1, maxit=2000))

for (counter in 1:50){
  res_store<-gmm(g, x=estim_matrix, t0=coef(res_store), prewhite=0,weightsMatrix=diag(diag(cov(test))^(-1)),optfct="optim",method="BFGS",centeredVcov='TRUE',
                 control=list(abstol=1e-07,reltol=1e-07, trace=1, maxit=3000))
  save(res_store,counter, file="data/04_01_progress.RData")
}

maxrefine<-gmm(g, x=estim_matrix, t0=coef(res_store), prewhite=0,weightsMatrix=diag(diag(cov(test))^(-1)),optfct="optim",method="BFGS",centeredVcov='TRUE',
               control=list(abstol=1e-07,reltol=1e-07, trace=1, maxit=10000))



save(maxrefine,estim_sample, estim_matrix, starting,innertol, outertol, file="data/04_01_est_res_ny.RData")






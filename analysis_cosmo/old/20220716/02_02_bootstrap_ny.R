## perform bootstrap replications
## for now do simple logit.
library('data.table')
library('gmm')
library('parallel')

set.seed(5777)
tau<-1.045


load("data/02_01_est_res_ny.RData")

############### main evaluation function
bootreps<-1000
core_count<-16
core_outside<-8
############### main evaluation function
source('main_gmm_objective.R')
###############


### draw samples
samples_list<-vector("list", length = bootreps)
for (x in 1:bootreps){
  samples_list[[x]]<-sample(unique(estim_sample$location_id), replace=TRUE)
}

mega_function<-function(bootnum){
  locs<-samples_list[[bootnum]]
  estim_sample_new<-data.table()
  for (x in locs) estim_sample_new<-rbind(estim_sample_new,estim_sample[location_id==x,])
  estim_matrix_new<-as.matrix(estim_sample_new[,c("s_index", "task_mix2", "task_mix3", "task_mix4",
                                                  "task_mix5","task_mix6",
                                                  "cust_price", "salon_share_subdiv","outside_share",
                                                  "qt1", "qt2", "qt3", "hausman_all",
                                                  "male_flag", "child_flag", "m_serv", "child_serv")])
  gmmres<-gmm(g, x=estim_matrix_new, t0=coef(maxrefine), prewhite=0, type="twoStep",optfct="optim",wmatrix='ident',centeredVcov='TRUE',
                 control=list(abstol=1e-08,reltol=1e-08, trace=1, maxit=10000))
  return(gmmres)
}

bootres <- mclapply(1:bootreps, mega_function, mc.cores=core_outside)

save(bootreps,samples_list,bootres, file="data/02_02_bootstrap_ny.RData")






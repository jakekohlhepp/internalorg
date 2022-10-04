## estimate model.
## for now do simple logit.
library('data.table')
library('gmm')
set.seed(4466)


### read in and limit data
firm_quarter<-readRDS("data/00_00_firm_quarter.rds")
# make one extra variable:
firm_quarter[, avg_labor:=tot_duration/cust_count]

# limit just to 2019.4, LA for now
estim_sample<-firm_quarter[quarter_year %in% c(2019.2,2019.3, 2019.4, 2020.1) & county %in% c(36061)]
# time indicators
estim_sample[, qt1:=quarter_year==2019.2]
estim_sample[, qt2:=quarter_year==2019.3]
estim_sample[, qt3:=quarter_year==2019.4]

# cannot identify when s_index is 0 or infinite
estim_sample<-estim_sample[!is.nan(s_norm)   & cust_price>0]
# cannot use salons that do not record price.

table(estim_sample$county)
# need to convert to matrix to feed into gmm package
estim_matrix<-as.matrix(estim_sample[,c("s_index", "task_mix2", "task_mix3", "task_mix4",
                                        "task_mix5","task_mix6",
                                        "cust_price", "salon_share_subdiv","outside_share", "hausman_all",
                                        "male_flag", "child_flag", "m_serv", "child_serv")])
# sales tax rate - 0% in la
tau<-1.045
############### tolerance preferences
innertol<-1e-06
outertol<-1e-06
core_count<-32
############### 

############### parameters that are fixed
c0<-mean(estim_matrix[,7])
u0<-mean(log(estim_matrix[,8]/estim_matrix[,9]))

starting<-c()
starting[1]<-0.1
starting[2]<-u0
starting[3]<-c0
starting[4]<-1
starting[5]<-1
starting[6]<-1
starting[7]<-1
starting[8]<-1
starting[9]<-1
starting[10]<-1
starting[11]<-1
starting[12]<-1
starting[13]<-1
starting[14]<-1
starting[15]<-1
starting[16]<-0
starting[17]<-0

############### 



############### main evaluation function
source('simpler_gmm_objective.R')
############### 



############### Step 1. Compute first set of estimates using very coarse convergence.
outgmm1<-gmm(g, x=estim_matrix, t0=starting, prewhite=0, type="twoStep",optfct="optim",wmatrix='ident',centeredVcov='TRUE',
             control=list(abstol=1e-08,reltol=1e-08, trace=1, maxit=1000))
outgmm2<-gmm(g, x=estim_matrix, t0=coef(outgmm1), prewhite=0, type="twoStep",optfct="optim",wmatrix='ident',centeredVcov='TRUE',
             control=list(abstol=1e-08,reltol=1e-08, trace=1, maxit=1000))
outgmm3<-gmm(g, x=estim_matrix, t0=coef(outgmm2), prewhite=0, type="twoStep",optfct="optim",wmatrix='ident',centeredVcov='TRUE',
             control=list(abstol=1e-08,reltol=1e-08, trace=1, maxit=1000))
outgmm4<-gmm(g, x=estim_matrix, t0=coef(outgmm3), prewhite=0, type="twoStep",optfct="optim",wmatrix='ident',centeredVcov='TRUE',
             control=list(abstol=1e-08,reltol=1e-08, trace=1, maxit=1000))
outgmm5<-gmm(g, x=estim_matrix, t0=coef(outgmm4), prewhite=0, type="twoStep",optfct="optim",wmatrix='ident',centeredVcov='TRUE',
             control=list(abstol=1e-08,reltol=1e-08, trace=1, maxit=1000))
outgmm6<-gmm(g, x=estim_matrix, t0=coef(outgmm5), prewhite=0, type="twoStep",optfct="optim",wmatrix='ident',centeredVcov='TRUE',
             control=list(abstol=1e-08,reltol=1e-08, trace=1, maxit=1000))


############### Step 2. Computeestimates using more refined convergence.

refine1<-gmm(g, x=estim_matrix, t0=coef(outgmm6), prewhite=0, type="twoStep",optfct="optim",wmatrix='ident',centeredVcov='TRUE',
             control=list(abstol=1e-08,reltol=1e-08, trace=1, maxit=1000))
refine2<-gmm(g, x=estim_matrix, t0=coef(refine1), prewhite=0, type="twoStep",optfct="optim",wmatrix='ident',centeredVcov='TRUE',
             control=list(abstol=1e-08,reltol=1e-08, trace=1, maxit=1000))
refine3<-gmm(g, x=estim_matrix, t0=coef(refine2), prewhite=0, type="twoStep",optfct="optim",wmatrix='ident',centeredVcov='TRUE',
             control=list(abstol=1e-08,reltol=1e-08, trace=1, maxit=1000))
refine4<-gmm(g, x=estim_matrix, t0=coef(refine3), prewhite=0, type="twoStep",optfct="optim",wmatrix='ident',centeredVcov='TRUE',
             control=list(abstol=1e-08,reltol=1e-08, trace=1, maxit=1000))
refine5<-gmm(g, x=estim_matrix, t0=coef(refine4), prewhite=0, type="twoStep",optfct="optim",wmatrix='ident',centeredVcov='TRUE',
             control=list(abstol=1e-08,reltol=1e-08, trace=1, maxit=1000))
refine6<-gmm(g, x=estim_matrix, t0=coef(refine5), prewhite=0, type="twoStep",optfct="optim",wmatrix='ident',centeredVcov='TRUE',
             control=list(abstol=1e-08,reltol=1e-08, trace=1, maxit=1000))


############### Step 3. Refine convergence again.

refine_more1<-gmm(g, x=estim_matrix, t0=coef(refine6), prewhite=0, type="twoStep",optfct="optim",wmatrix='ident',centeredVcov='TRUE',
                  control=list(abstol=1e-08,reltol=1e-08, trace=1, maxit=1000))
refine_more2<-gmm(g, x=estim_matrix, t0=coef(refine_more1), prewhite=0, type="twoStep",optfct="optim",wmatrix='ident',centeredVcov='TRUE',
                  control=list(abstol=1e-08,reltol=1e-08, trace=1, maxit=1000))
refine_more3<-gmm(g, x=estim_matrix, t0=coef(refine_more2), prewhite=0, type="twoStep",optfct="optim",wmatrix='ident',centeredVcov='TRUE',
                  control=list(abstol=1e-08,reltol=1e-08, trace=1, maxit=1000))
refine_more4<-gmm(g, x=estim_matrix, t0=coef(refine_more3), prewhite=0, type="twoStep",optfct="optim",wmatrix='ident',centeredVcov='TRUE',
                  control=list(abstol=1e-08,reltol=1e-08, trace=1, maxit=1000))
refine_more5<-gmm(g, x=estim_matrix, t0=coef(refine_more4), prewhite=0, type="twoStep",optfct="optim",wmatrix='ident',centeredVcov='TRUE',
                  control=list(abstol=1e-08,reltol=1e-08, trace=1, maxit=1000))
refine_more6<-gmm(g, x=estim_matrix, t0=coef(refine_more5), prewhite=0, type="twoStep",optfct="optim",wmatrix='ident',centeredVcov='TRUE',
                  control=list(abstol=1e-08,reltol=1e-08, trace=1, maxit=1000))

############### Step 4. Refine everything

maxrefine<-gmm(g, x=estim_matrix, t0=coef(refine_more6), prewhite=0, type="twoStep",optfct="optim",wmatrix='ident',centeredVcov='TRUE',
               control=list(abstol=1e-08,reltol=1e-08, trace=1, maxit=10000))


##### gmm evaluation.

save(maxrefine,estim_sample, estim_matrix, starting,innertol, outertol, file="data/01_01_est_simple_ny.RData")







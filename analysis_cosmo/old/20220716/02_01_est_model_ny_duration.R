## estimate model.
## for now do simple logit.
library('data.table')
library('gmm')
set.seed(5777)
colMax <- function(data) apply(data,2, max, na.rm = TRUE)
colMin <- function(data) apply(data,2, min, na.rm = TRUE)
spec_log<-function(x)  ifelse(x==0 | x==-Inf | is.nan(x),0,log(x))
entropy<-function(x)  return(sum(-x*spec_log(x)))


### read in and limit data
firm_quarter<-readRDS("data/00_00_firm_quarter.rds")
firm_quarter[,avg_dur:=tot_duration/cust_count]
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
                                        "cust_price", "salon_share_subdiv","outside_share",
                                        "qt1", "qt2", "qt3", "hausman_all",
                                        "s_2", "s_3", "s_4", "s_5", "s_6","avg_dur")])
# sales tax rate - 0% in la
tau<-1.045
############### tolerance preferences
innertol<-1e-03
outertol<-1e-03
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
starting[16]<-1
starting[17]<-1
starting[18]<-1
starting[19]<-1
starting[20]<-1
starting[21]<-1
starting[22]<-1
starting[23]<-1
starting[24]<-0
starting[25]<-0
starting[26]<-0
starting[27]<-0
starting[28]<-0
starting[29]<-0
starting[30]<-1
starting[31]<-1
starting[32]<-1
starting[33]<-1
starting[34]<-1
starting[35]<-1
starting[36]<-0
starting[37]<-0
starting[38]<-0
starting[39]<-0
starting[40]<-0
starting[41]<-0
starting[42]<-0
starting[43]<-0
starting[44]<-0
starting[45]<-0

## uncomment to start at saved values.
starting<-readRDS('data/best_starting.rds')
############### 



############### main evaluation function
# id with 0 sindex: 2265bf06-65c4-47e8-a3f3-4c04a7ddd572
g<-function(parms, x){
  
  
  rho<-parms[1]*tau
  u_bar<-parms[2]
  c_bar<-parms[3]
  w1<-c(0,parms[4],parms[5],parms[6],parms[7], parms[8])
  w2<-c(0,parms[9],parms[10],parms[11],parms[12],parms[13])
  w3<-c(0,parms[14],parms[15],parms[16],parms[17],parms[18])
  w4<-c(0,parms[19],parms[20],parms[21], parms[22],parms[23])
  ut1<-parms[24]
  ut2<-parms[25]
  ut3<-parms[26]
  ct1<-parms[27]
  ct2<-parms[28]
  ct3<-parms[29]
  s1 <-parms[30]
  s2<-parms[31]
  s3<-parms[32]
  s4<-parms[33]
  s5<-parms[34]
  s6<-parms[35]
  m1<-0
  m2<-0
  m3<-0
  m4<-0
  m5<-0
  m6<-0
  cmat<-c(parms[36],parms[37],parms[38],parms[39], parms[40])
  umat<-c(parms[41],parms[42],parms[43],parms[44], parms[45])
  
  wmat<-list(matrix(w1, nrow=6, ncol=6, byrow=FALSE),
             matrix(w2, nrow=6, ncol=6, byrow=FALSE),
             matrix(w3, nrow=6, ncol=6, byrow=FALSE),
             matrix(w4, nrow=6, ncol=6, byrow=FALSE))
  h<-rbind(c(s1,m2,m3,m4,m5,m6),c(m1,s2,m3,m4,m5,m6), c(m1,m2,s3,m4,m5,m6), c(m1,m2,m3,s4,m5,m6),
           c(m1,m2,m3,m4,s5,m6), c(m1,m2,m3,m4,m5,s6))
  
  ## this function will return gamma given sindex.
  firm_estim<-function(sindex,ap2, ap3, ap4,ap5, ap6,qts){
    aleph<-c(1-ap2-ap3-ap4-ap5-ap6, ap2, ap3, ap4, ap5, ap6)
    findlambda<-function(lambda){
      A<-exp(-lambda*(wmat[[qts]] -h/rho) )
      A[A>=Inf]<-1e8
      fxpt<-function(p){
        C<-colSums(t(A)*aleph/colSums(A*p))
        return(p*C)
      }
      objval<-function(p){
        C<-colSums(t(A)*aleph/colSums(A*p))
        return(sum(p*spec_log(C), na.rm=TRUE)-max(spec_log(C), na.rm=TRUE))
      }
      E<-rep(1/(6),6)
      valold<-0
      val<-1
      icount<-0
      while (abs(val-valold)>innertol & icount<100000){
        E<-fxpt(E)
        valold<-val
        val<-objval(E)
        icount<-icount+1
      }
      a<-t(t(A*E)/colSums(A*E)*aleph)
      #E<-round(E,9)
      #a<-(round(E,9)>0)*a
      I<-sum(a*spec_log(t(t(a/rowSums(a))/colSums(a))))
      diff<-sindex-I
      return(diff)
    }
    if (round(sindex,5)>0){
      lambda<-tryCatch({uniroot(findlambda, lower=0, upper=100000, tol=outertol)$root},
                       error = function(e){
                         if (grepl("end points not of opposite sign",e, fixed=TRUE)){
                           return(NaN)
                         } else{
                           tryCatch({uniroot(findlambda, lower=0, upper=10000, tol=outertol)$root},
                                    error = function(e){
                                      if (grepl("end points not of opposite sign",e, fixed=TRUE)){
                                        return(NaN)
                                      } else{
                                        tryCatch({uniroot(findlambda, lower=0, upper=1000, tol=outertol)$root},
                                                 error = function(e){
                                                   if (grepl("end points not of opposite sign",e, fixed=TRUE)){
                                                     return(NaN)
                                                   } else{
                                                     tryCatch({uniroot(findlambda, lower=0, upper=100, tol=outertol)$root},
                                                              error = function(e){
                                                                if (grepl("end points not of opposite sign",e, fixed=TRUE)){
                                                                  return(NaN)
                                                                } else{
                                                                  tryCatch({uniroot(findlambda, lower=0, upper=10, tol=outertol)$root},
                                                                           error = function(e){
                                                                             if (grepl("end points not of opposite sign",e, fixed=TRUE)){
                                                                               return(NaN) 
                                                                             } else{
                                                                               return(NaN)
                                                                             }
                                                                           }) 
                                                                }
                                                              }) 
                                                   }
                                                 }) 
                                      }
                                    }) 
                         }
                       })
      
      A<-exp(-lambda*(wmat[[qts]] -h/rho) )
      A[A>=Inf]<-1e8
      fxpt<-function(p,y){
        C<-colSums(t(A)*aleph/colSums(A*p))
        return(p*C)
      }
      objval<-function(p,y){
        C<-colSums(t(A)*aleph/colSums(A*p))
        return(sum(p*spec_log(C), na.rm=TRUE)-max(spec_log(C), na.rm=TRUE))
      }
      E<-rep(1/(6),6)
      valold<-0
      val<-1
      icount<-0
      while (abs(val-valold)>innertol & icount<100000){
        E<-fxpt(E)
        valold<-val
        val<-objval(E)
        icount<-icount+1
      }
      a<-t(t(A*E)/colSums(A*E)*aleph)
      #E<-round(E,9)
      #a<-(round(E,9)>0)*a
    } else{
      ahelper<-t(matrix(aleph,length(aleph), 6))
      E<-rep(0,6)
      E[which.min(rowSums((wmat[[qts]] -h/rho)*ahelper))[1]]<-1
      a<-E*ahelper
      lambda<-1/1000000000
    }
    dstar<- sum(a*(-rho*wmat[[qts]] +h))
    return(list(a,dstar,lambda ))
  }
  firm_estim<-Vectorize(firm_estim)
  
  res<-lapply(1:nrow(x), function(y){firm_estim(x[y,1], x[y,2], x[y,3], x[y,4],x[y,5],x[y,6],
                                                ifelse(max(x[y,10:12])==1,which.max(x[y,10:12]),4)
                                                )})
  
  # helper functions
  finda<-Vectorize(function(x){res[x][[1]][1]})
  ret_gamma<-Vectorize(function(x){(res[x][[1]][[3]])^(-1) })
  findW<-Vectorize(function(y){sum(wmat[[ifelse(max(x[y,10:12])==1,which.max(x[y,10:12]),4)]]*res[y][[1]][[1]])})
  findqual<-Vectorize(function(x){sum(finda(x)[[1]]*(h)) })
  gamma<-ret_gamma(1:nrow(x))
  xi<-findqual(1:nrow(x))
  W<-findW(1:nrow(x))
  
  moment_mat<-cbind(x[,7]-1/rho/(1-x[,8]) - (W - gamma*x[,1])*h-c_bar
                    -ct1*x[,10]-ct2*x[,11]-ct3*x[,12]-x[,2:6]%*%cmat,
                    log(x[,8]/x[,9]) +rho*x[,7]-xi-u_bar
                    -ut1*x[,10]-ut2*x[,11]-ut3*x[,12]-x[,2:6]%*%umat)
  Z<-cbind(1,x[,2:6],x[,1],x[,2:6]*x[,1], x[,13], x[,14:18])
  Z<-cbind(Z, Z*x[,10], Z*x[,11],Z*x[,12])
  return(cbind(moment_mat[,1]*Z,moment_mat[,2]*Z ))
}

gmm_obj<-function(x){
  hold<-colMeans(g(parms=x,x=estim_matrix))
  return(hold)
}



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
innertol<-1e-04
outertol<-1e-04

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
innertol<-1e-05
outertol<-1e-05

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
innertol<-1e-07
outertol<-1e-07

maxrefine<-gmm(g, x=estim_matrix, t0=coef(refine_more6), prewhite=0, type="twoStep",optfct="optim",wmatrix='ident',centeredVcov='TRUE',
                  control=list(abstol=1e-08,reltol=1e-08, trace=1, maxit=10000))


##### gmm evaluation.

save(maxrefine,estim_sample, estim_matrix, starting,innertol, outertol, file="data/02_00_est_res_ny.RData")





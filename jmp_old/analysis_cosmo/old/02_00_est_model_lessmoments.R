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
# limit just to 2019.4, LA for now
estim_sample<-firm_quarter[quarter_year %in% c(2019.2,2019.3, 2019.4, 2020.1) & county %in% c(36061)]
# time indicators
estim_sample[, qt1:=quarter_year==2019.2]
estim_sample[, qt2:=quarter_year==2019.3]
estim_sample[, qt3:=quarter_year==2019.4]
estim_sample[, lac:=county==6037]

# cannot identify when s_index is 0 or infinite
estim_sample<-estim_sample[!is.nan(s_norm)   & cust_price>0]
# cannot use salons that do not record price.

table(estim_sample$county)
# need to convert to matrix to feed into gmm package
estim_matrix<-as.matrix(estim_sample[,c("s_index", "task_mix2", "task_mix3", "task_mix4",
                                        "cust_price", "salon_share_subdiv","outside_share",
                                        "qt1", "qt2", "qt3")])
# sales tax rate
tau<-1.045
############### tolerance preferences
innertol<-1e-03
outertol<-1e-03
kchoose=1 
meth_choose=3
############### 

############### parameters that are fixed
c0<-mean(estim_matrix[,5])
u0<-mean(log(estim_matrix[,6]/estim_matrix[,7]))
#starting<-readRDS('data/02_00_parms_ny.rds')

##starting<-starting[-c(10,11,13,14,15,16)]
#starting[2]<-0

starting<-c()
starting[1]<-3
starting[2]<-u0
starting[3]<-c0
starting[4]<-0
starting[5]<-0
starting[6]<-0
starting[7]<-0
starting[8]<-0
starting[9]<-0
starting[10]<-0
starting[11]<-0
starting[12]<-0
starting[13]<-1
starting[14]<-1
starting[15]<-1
starting[16]<-1
starting[17]<-0
starting[18]<-0
starting[19]<-0
starting[20]<-0
############### 



############### main evaluation function
# id with 0 sindex: 2265bf06-65c4-47e8-a3f3-4c04a7ddd572
g<-function(parms, x){
  
  
  rho<-parms[1]*tau
  u_bar<-parms[2]
  c_bar<-parms[3]
  w<-c(0,parms[4],parms[5],parms[6])
  ut1<-parms[7]
  ut2<-parms[8]
  ut3<-parms[9]
  ct1<-parms[10]
  ct2<-parms[11]
  ct3<-parms[12]
  s1 <-parms[13]
  s2<-parms[14]
  s3<-parms[15]
  s4<-parms[16]
  m1<-parms[17]
  m2<-parms[18]
  m3<-parms[19]
  m4<-parms[20]
  
  wmat<-matrix(w, nrow=4, ncol=4, byrow=FALSE)
  h<-rbind(c(s1,m2,m3,m4),c(m1,s2,m3,m4), c(m1,m2,s3,m4), c(m1,m2,m3,s4))
  
  ## this function will return gamma given sindex.
  firm_estim<-function(sindex,ap2, ap3, ap4){
    aleph<-c(1-ap2-ap3-ap4, ap2, ap3, ap4)
    findlambda<-function(lambda){
      A<-exp(-lambda*(wmat -h/rho) )
      A[A>=Inf]<-1e8
      fxpt<-function(p){
        C<-colSums(t(A)*aleph/colSums(A*p))
        return(p*C)
      }
      objval<-function(p){
        C<-colSums(t(A)*aleph/colSums(A*p))
        return(sum(p*spec_log(C), na.rm=TRUE)-max(spec_log(C), na.rm=TRUE))
      }
      E<-rep(1/(4),4)
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
      E<-round(E,9)
      a<-(round(E,9)>0)*a
      I<-sum(a*spec_log(t(t(a/rowSums(a))/colSums(a))))
      diff<-sindex-I
      return(diff)
    }
    if (round(sindex,5)>0){
      lambda<-tryCatch({uniroot(findlambda, lower=0, upper=100000, tol=outertol)$root},
                       error = function(e){
                         if (grepl("end points not of opposite sign",e, fixed=TRUE)){
                           if (findlambda(100000)<0){
                             return(Inf)
                           } else{
                             return(1/1000000000)
                           } 
                         } else{
                           tryCatch({uniroot(findlambda, lower=0, upper=10000, tol=outertol)$root},
                                    error = function(e){
                                      if (grepl("end points not of opposite sign",e, fixed=TRUE)){
                                        if (findlambda(10000)<0){
                                          return(Inf)
                                        } else{
                                          return(1/1000000000)
                                        } 
                                      } else{
                                        tryCatch({uniroot(findlambda, lower=0, upper=1000, tol=outertol)$root},
                                                 error = function(e){
                                                   if (grepl("end points not of opposite sign",e, fixed=TRUE)){
                                                     if (findlambda(1000)<0){
                                                       return(Inf)
                                                     } else{
                                                       return(1/1000000000)
                                                     } 
                                                   } else{
                                                     tryCatch({uniroot(findlambda, lower=0, upper=100, tol=outertol)$root},
                                                              error = function(e){
                                                                if (grepl("end points not of opposite sign",e, fixed=TRUE)){
                                                                  if (findlambda(100)<0){
                                                                    return(Inf)
                                                                  } else{
                                                                    return(1/1000000000)
                                                                  } 
                                                                } else{
                                                                  tryCatch({uniroot(findlambda, lower=0, upper=10, tol=outertol)$root},
                                                                           error = function(e){
                                                                             if (grepl("end points not of opposite sign",e, fixed=TRUE)){
                                                                               if (findlambda(10)<0){
                                                                                 return(Inf)
                                                                               } else{
                                                                                 return(1/1000000000)
                                                                               } 
                                                                             } else{
                                                                               return(1/1000000000)
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
      
      A<-exp(-lambda*(wmat -h/rho) )
      A[A>=Inf]<-1e8
      fxpt<-function(p,y){
        C<-colSums(t(A)*aleph/colSums(A*p))
        return(p*C)
      }
      objval<-function(p,y){
        C<-colSums(t(A)*aleph/colSums(A*p))
        return(sum(p*spec_log(C), na.rm=TRUE)-max(spec_log(C), na.rm=TRUE))
      }
      E<-rep(1/(4),4)
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
      E<-round(E,9)
      a<-(round(E,9)>0)*a
    } else{
      ahelper<-t(matrix(aleph,length(aleph), 4))
      E<-rep(0,4)
      E[which.min(rowSums((wmat -h/rho)*ahelper))[1]]<-1
      a<-E*ahelper
      lambda<-1/1000000000
    }
    dstar<- sum(a*(-rho*wmat +h))
    return(list(a,dstar,lambda ))
  }
  firm_estim<-Vectorize(firm_estim)
  
  res<-lapply(1:nrow(x), function(y){firm_estim(x[y,1], x[y,2], x[y,3], x[y,4])})
  
  # helper functions
  finda<-Vectorize(function(x){res[x][[1]][1]})
  ret_gamma<-Vectorize(function(x){(res[x][[1]][[3]])^(-1) })
  findW<-Vectorize(function(x){sum(wmat*res[x][[1]][[1]])})
  findqual<-Vectorize(function(x){sum(finda(x)[[1]]*(h)) })
  gamma<-ret_gamma(1:nrow(x))
  xi<-findqual(1:nrow(x))
  W<-findW(1:nrow(x))
  
  moment_mat<-cbind(x[,5]-1/rho/(1-x[,6]) - W - gamma*x[,1]-c_bar-ct1*x[,8]-ct2*x[,9]-ct3*x[,10],
                    log(x[,6]/x[,7]) +rho*x[,5]-xi-u_bar
                    -ut1*x[,8]-ut2*x[,9]-ut3*x[,10])
  Z<-cbind(1,x[,2:4],x[,1],x[,2:4]*x[,1], x[,8], x[,9],x[,10])
  return(cbind(moment_mat[,1]*Z,moment_mat[,2]*Z ))
}

gmm_obj<-function(x){
  hold<-colMeans(g(parms=x,x=estim_matrix))
  return(hold)
}
############### 


----
  
  
############### Step 1. Compute first set of estimates using very coarse convergence.
outgmm1<-gmm(g, x=estim_matrix, t0=starting, prewhite=0, type="twoStep",optfct="optim",wmatrix='ident',centeredVcov='TRUE',
             control=list(abstol=1e-08,reltol=1e-08, trace=1, maxit=100000))




  
  
  
outgmm2<-gmm(g, x=estim_matrix, t0=coef(outgmm1), prewhite=0, type="twoStep",optfct="optim",wmatrix='ident',centeredVcov='TRUE',
             control=list(abstol=1e-08,reltol=1e-08, trace=1, maxit=10000))

outgmm3<-gmm(g, x=estim_matrix, t0=coef(outgmm2), prewhite=0, type="twoStep",optfct="optim",wmatrix='ident',centeredVcov='TRUE',
             control=list(abstol=1e-08,reltol=1e-08, trace=1, maxit=10000))

############### Step 2. Compute first set of estimates using more refined convergence.
innertol<-1e-06
outertol<-1e-06
outgmm4<-gmm(g, x=estim_matrix, t0=coef(outgmm3), prewhite=0, type="twoStep",optfct="optim",wmatrix='ident',centeredVcov='TRUE',
             control=list(abstol=1e-08,reltol=1e-08, trace=1, maxit=10000))
outgmm5<-gmm(g, x=estim_matrix, t0=coef(outgmm4), prewhite=0, type="twoStep",optfct="optim",wmatrix='ident',centeredVcov='TRUE',
             control=list(abstol=1e-08,reltol=1e-08, trace=1, maxit=10000))
innertol<-1e-06
outertol<-1e-06
outgmm6<-gmm(g, x=estim_matrix, t0=coef(outgmm5), prewhite=0, type="twoStep",optfct="optim",wmatrix='ident',centeredVcov='TRUE',
             control=list(abstol=1e-10,reltol=1e-10, trace=1, maxit=10000))
innertol<-1e-07
outertol<-1e-07
outgmm7<-gmm(g, x=estim_matrix, t0=coef(outgmm6), prewhite=0, type="twoStep",optfct="optim",wmatrix='ident',centeredVcov='TRUE',
             control=list(abstol=1e-10,reltol=1e-10, trace=1, maxit=10000))

############### 




##### gmm evaluation.

save(outgmm7,estim_sample, estim_matrix, starting,innertol, outertol, file="data/02_00_est_res_ny_lessmoments.RData")





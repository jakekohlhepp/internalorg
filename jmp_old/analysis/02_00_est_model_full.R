## estimate model.
## for now do simple logit.
library('data.table')
library('gmm')
set.seed(121345)
colMax <- function(data) apply(data,2, max, na.rm = TRUE)
colMin <- function(data) apply(data,2, min, na.rm = TRUE)
spec_log<-function(x)  ifelse(x==0 | x==-Inf | is.nan(x),0,log(x))
entropy<-function(x)  return(sum(-x*spec_log(x)))


### read in and limit data
firm_quarter<-readRDS("data/00_00_firm_quarter.rds")
# limit just to 2019.4, LA for now
estim_sample<-firm_quarter[quarter_year %in% c(2019.2,2019.3, 2019.4, 2020.1) & county %in% c(36061, 6037)]
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
                                        "cust_price", "salon_share_subdiv","outside_share", "s_2", "s_3","s_4",
                                        "qt1", "qt2", "qt3", "lac")])

############### tolerance preferences
innertol<-1e-03
outertol<-1e-03
kchoose=1 
meth_choose=3
############### 

############### parameters that are fixed
N<-1 ## if this is changed we need to change whole program.
T<-4
c0<-mean(estim_matrix[,5])
u0<-mean(log(estim_matrix[,6]/estim_matrix[,7]))
starting<-c(readRDS('data/02_00_parms_ny.rds'),0,0,0,0,0,0)
#starting<-starting[-c(10,11,13,14,15,16)]
#starting[2]<-0

#starting<-c()
#starting[1]<-10
#starting[2]<-0.01
#starting[3]<-u0
#starting[4]<-0.01
#starting[5]<-c0
#starting[6]<-0
#starting[7]<-0
#starting[8]<-0
#starting[9]<-0
#starting[10]<-0
#starting[11]<-0
#starting[12]<-0
#starting[13]<-0
#starting[14]<-0



############### 



############### main evaluation function
# id with 0 sindex: 2265bf06-65c4-47e8-a3f3-4c04a7ddd572
g<-function(parms, x){
  
  # la parms
  rho<-parms[1]
  delta <-parms[2]
  u_bar<-parms[3]
  theta_grid<-c(parms[4])
  # these are common
  c_bar<-parms[5]
  c<-c(parms[6],parms[7],parms[8])
  w<-c(0,parms[9],parms[10],parms[11])
  beta<-c(parms[12],parms[13],parms[14])
  ut1<-parms[15]
  ut2<-parms[16]
  ut3<-parms[17]
  ct1<-parms[18]
  ct2<-parms[19]
  ct3<-parms[20]
  
  # nyc parms
  
  
  
  thetamat<-matrix(as.vector(sapply(1:N, function(x) {rep(theta_grid[x], length=T)}, simplify=TRUE)), ncol=T, nrow=N*T)
  wmat<-matrix(w, nrow=N*T, ncol=T, byrow=FALSE)
  h<-matrix(1, ncol=T, nrow=N*T)-  do.call(rbind,rep(list(diag(1,nrow=T, T)),N)) 
  
  ## this function will return gamma given sindex.
  firm_estim<-function(sindex,ap2, ap3, ap4){
    aleph<-c(1-ap2-ap3-ap4, ap2, ap3, ap4)
    findlambda<-function(lambda){
      A<-exp(-lambda*(wmat -thetamat/rho*delta^h) )
      A[A>=Inf]<-1e8
      fxpt<-function(p){
        C<-colSums(t(A)*aleph/colSums(A*p))
        return(p*C)
      }
      objval<-function(p){
        C<-colSums(t(A)*aleph/colSums(A*p))
        return(sum(p*spec_log(C), na.rm=TRUE)-max(spec_log(C), na.rm=TRUE))
      }
      E<-rep(1/(N*T),N*T)
      valold<-0
      val<-1
      while (abs(val-valold)>innertol){
        E<-fxpt(E)
        valold<-val
        val<-objval(E)
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
                             return(0)
                           } 
                         } else{
                           tryCatch({uniroot(findlambda, lower=0, upper=10000, tol=outertol)$root},
                                    error = function(e){
                                      if (grepl("end points not of opposite sign",e, fixed=TRUE)){
                                        if (findlambda(10000)<0){
                                          return(Inf)
                                        } else{
                                          return(0)
                                        } 
                                      } else{
                                        tryCatch({uniroot(findlambda, lower=0, upper=1000, tol=outertol)$root},
                                                 error = function(e){
                                                   if (grepl("end points not of opposite sign",e, fixed=TRUE)){
                                                     if (findlambda(1000)<0){
                                                       return(Inf)
                                                     } else{
                                                       return(0)
                                                     } 
                                                   } else{
                                                     tryCatch({uniroot(findlambda, lower=0, upper=100, tol=outertol)$root},
                                                              error = function(e){
                                                                if (grepl("end points not of opposite sign",e, fixed=TRUE)){
                                                                  if (findlambda(100)<0){
                                                                    return(Inf)
                                                                  } else{
                                                                    return(0)
                                                                  } 
                                                                } else{
                                                                  tryCatch({uniroot(findlambda, lower=0, upper=10, tol=outertol)$root},
                                                                           error = function(e){
                                                                             if (grepl("end points not of opposite sign",e, fixed=TRUE)){
                                                                               if (findlambda(10)<0){
                                                                                 return(Inf)
                                                                               } else{
                                                                                 return(0)
                                                                               } 
                                                                             } else{
                                                                               return(0)
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
      
      A<-exp(-lambda*(wmat -thetamat/rho*delta^h) )
      A[A>=Inf]<-1e8
      fxpt<-function(p,y){
        C<-colSums(t(A)*aleph/colSums(A*p))
        return(p*C)
      }
      objval<-function(p,y){
        C<-colSums(t(A)*aleph/colSums(A*p))
        return(sum(p*spec_log(C), na.rm=TRUE)-max(spec_log(C), na.rm=TRUE))
      }
      E<-rep(1/(N*T),N*T)
      valold<-0
      val<-1
      while (abs(val-valold)>innertol){
        E<-fxpt(E)
        valold<-val
        val<-objval(E)
      }
      a<-t(t(A*E)/colSums(A*E)*aleph)
      E<-round(E,9)
      a<-(round(E,9)>0)*a
    } else{
      ahelper<-t(matrix(aleph,length(aleph), N*T))
      E<-rep(0,N*T)
      E[which.min(rowSums((wmat -thetamat/rho*delta^h)*ahelper))[1]]<-1
      a<-E*ahelper
      lambda<-1/1000000000
    }
    dstar<- sum(a*(-rho*wmat +thetamat*delta^h))
    return(list(a,dstar,lambda ))
  }
  firm_estim<-Vectorize(firm_estim)
  
  res<-lapply(1:nrow(x), function(y){firm_estim(x[y,1], x[y,2], x[y,3], x[y,4])})
  
  # helper functions
  finda<-Vectorize(function(x){res[x][[1]][1]})
  ret_gamma<-Vectorize(function(x){(res[x][[1]][[3]])^(-1) })
  findW<-Vectorize(function(x){sum(wmat*res[x][[1]][[1]])})
  findqual<-Vectorize(function(x){sum(finda(x)[[1]]*(thetamat*delta^h)) })
  gamma<-ret_gamma(1:nrow(x))
  xi<-findqual(1:nrow(x))
  W<-findW(1:nrow(x))
  
  moment_mat<-cbind(x[,5]-1/rho/(1-x[,6])-x[,2:4]%*%c - W - gamma*x[,1]-c_bar-ct1*x[,11]-ct2*x[,12]-ct3*x[,13],
                    log(x[,6]/x[,7])-x[,2:4]%*%beta +rho*x[,5]-xi-u_bar
                    -ut1*x[,11]-ut2*x[,12]-ut3*x[,13])
  Z<-cbind(1,x[,2:4],x[,2:4]*x[,1], x[,8], x[,9],x[,10], x[,11], x[,12], x[,13])
  return(cbind(moment_mat[,1]*Z,moment_mat[,2]*Z ))
}
############### 


############### Step 1. Compute first set of estimates using very coarse convergence.
outgmm1<-gmm(g, x=estim_matrix, t0=starting, prewhite=0, type="twoStep",optfct="optim",wmatrix='ident',centeredVcov='TRUE',
             control=list(abstol=1e-03,reltol=1e-03, trace=1, maxit=10000))
outgmm1<-gmm(g, x=estim_matrix, t0=coef(outgmm1), prewhite=0, type="twoStep",optfct="optim",wmatrix='ident',centeredVcov='TRUE',
             control=list(abstol=1e-06,reltol=1e-06, trace=1, maxit=10000))
############### Step 2. Compute first set of estimates using more refined convergence.
innertol<-1e-05
outertol<-1e-05
outgmm1<-gmm(g, x=estim_matrix, t0=coef(outgmm1), prewhite=0, type="twoStep",optfct="optim",wmatrix='ident',centeredVcov='TRUE',
             control=list(abstol=1e-06,reltol=1e-06, trace=1, maxit=10000))
save(outgmm1, file="data/02_00_estimates.RData")
############### 







##### gmm evaluation.

save(outgmm1,estim_sample, estim_matrix, starting,innertol, outertol,N,T, file="data/02_00_est_res_ny.RData")





## estimate model.
## for now do simple logit.
library('data.table')
library('lessR') # for the to() command
library('SQUAREM')
library('gmm')

colMax <- function(data) apply(data,2, max, na.rm = TRUE)
colMin <- function(data) apply(data,2, min, na.rm = TRUE)
spec_log<-function(x)  ifelse(x==0 | x==-Inf | is.nan(x),0,log(x))
entropy<-function(x)  return(sum(-x*spec_log(x)))


### read in and limit data
firm_quarter<-readRDS("data/00_00_firm_quarter.rds")
firm_quarter[,share_given_zip:=salon_share/zip_share]
# limit just to 2019.4, LA for now
estim_sample<-firm_quarter[quarter_year==2019.3 & county %in% c(6037)]
# cannot identify when s_index is 0 or infinite
estim_sample<-estim_sample[!is.nan(s_norm) & round(s_norm,5)!=0 & round(s_norm,5)!=1]
table(estim_sample$county)
# need to convert to matrix to feed into gmm package
estim_matrix<-as.matrix(estim_sample[,c("s_norm", "task_mix2", "task_mix3", "task_mix4",
                                        "cust_price", "salon_share","outside_share", "hausman_2", "hausman_3", "s_2", "s_3")])

############### tolerance preferences
innertol<-1e-03
outertol<-1e-03
kchoose=1 
meth_choose=3
############### 

############### parameters that are fixed
N<-2 ## if this is changed we need to change whole program.
T<-4
c0<-mean(estim_matrix[,5])
u0<-mean(log(estim_matrix[,6]/estim_matrix[,7]))
starting<-c( 1,0.6,u0, 2,c0,1,1,1, 0.01, 0.01, 0.01,1.96, 1.96, 1.96, 4,0,0,0 )
############### 



############### main evaluation function
g<-function(parms, x){
  
  rho<-parms[1]
  delta <-parms[2]
  u_bar<-parms[3]
  theta_grid<-c(0, parms[4])
  c_bar<-parms[5]
  c<-c(parms[6],parms[7],parms[8])
  w<-c(0,parms[9],parms[10], parms[11],
       parms[12], parms[13], parms[14], parms[15])
  beta<-c(parms[16],parms[17],parms[18])
  thetamat<-matrix(as.vector(sapply(1:N, function(x) {rep(theta_grid[x], length=T)}, simplify=TRUE)), ncol=T, nrow=N*T)
  wmat<-matrix(w, nrow=N*T, ncol=T, byrow=FALSE)
  h<-matrix(1, ncol=T, nrow=N*T)-  do.call(rbind,rep(list(diag(1,nrow=T, T)),N)) 
  
  ## this function will return gamma given sindex.
  firm_estim<-function(sindex,ap2, ap3, ap4){
    aleph<-c(1-ap2-ap3-ap4, ap2, ap3, ap4)
    smax<- -(1-ap2-ap3-ap4)*spec_log(1-ap2-ap3-ap4)-ap2*spec_log(ap2)-ap3*spec_log(ap3)-ap4*spec_log(ap4)
    findlambda<-function(lambda){
      A<-exp(-lambda*(wmat -thetamat/rho*delta^h) )
      fxpt<-function(p,y){
        C<-colSums(t(A)*aleph/colSums(A*p))
        return(p*C)
      }
      objval<-function(p,y){
        C<-colSums(t(A)*aleph/colSums(A*p))
        return(sum(p*spec_log(C), na.rm=TRUE)-max(spec_log(C), na.rm=TRUE))
      }
      E0<-rep(1/(N*T),N*T)
      pf<-tryCatch({squarem(p=E0, y=1, fixptfn=fxpt, objfn=objval, control=list(tol=innertol, method=meth_choose,K=kchoose,maxiter=100000))},
               error = function(e){
                 list(par=E0,convergence=TRUE )
               })
      stopifnot(pf$convergence)
      E<-pf$par
      a<-t(t(A*E)/colSums(A*E)*aleph)
      E<-round(E,9)
      a<-(round(E,9)>0)*a
      I<-sum(a*spec_log(t(t(a/rowSums(a))/colSums(a))))
      diff<-sindex-I/smax
      diff<-ifelse(is.nan(diff),1,diff)
      return(diff)
    }
    lambda<-tryCatch({uniroot(findlambda, lower=0, upper=500, tol=outertol, extendInt = "downX")$root},
                     error = function(e){
                       if (findlambda(500)>0 & findlambda(0)>0 ){
                         return(500)
                       } else if (findlambda(0)<=0 & findlambda(500)<=0) {
                         return(0)
                       } else  
                       return(NA)
                     }) # when there is no root in range, it means constraint does not bind.
    # then we put in a large value for lambda.
    A<-exp(-lambda*(wmat -thetamat/rho*delta^h) )
    fxpt<-function(p,y){
      C<-colSums(t(A)*aleph/colSums(A*p))
      return(p*C)
    }
    objval<-function(p,y){
      C<-colSums(t(A)*aleph/colSums(A*p))
      return(sum(p*spec_log(C), na.rm=TRUE)-max(spec_log(C), na.rm=TRUE))
    }
    E0<-rep(1/(N*T),N*T)
    pf<-tryCatch({squarem(p=E0, y=1, fixptfn=fxpt, objfn=objval, control=list(tol=innertol, method=meth_choose,K=kchoose,maxiter=100000))},
                 error = function(e){
                   list(par=E0,convergence=TRUE )
                 })
    stopifnot(pf$convergence)
    E<-pf$par
    a<-t(t(A*E)/colSums(A*E)*aleph)
    E<-round(E,9)
    a<-(round(E,9)>0)*a
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
  
  #moment_mat<-cbind(x[,6]-(1-sigma)/rho/(1-sigma*x[,13]-(1-sigma)*x[,7])-x[,2:5]%*%c - W - gamma*x[,1],
   #                 log(x[,7]/x[,8])-x[,2:5]%*%beta+rho*x[,6]-xi-sigma*log(x[,13]))
  moment_mat<-cbind(x[,5]-1/rho/(1-x[,6])-x[,2:4]%*%c - W - gamma*x[,1]-c_bar,
                    log(x[,6]/x[,7])-x[,2:4]%*%beta +rho*x[,5]-xi-u_bar)
  Z<-cbind(x[,2:4],x[,2:4]*x[,1], x[,8], x[,9],x[,10], x[,11])
  return(cbind(moment_mat[1,]*Z,moment_mat[2,]*Z ))
}
############### 

##### gmm evaluation.


outgmm<-gmm(g, x=estim_matrix, t0=starting, prewhite=0, type="twoStep",optfct="optim",wmatrix='ident',centeredVcov='TRUE', control=list(abstol=1e-03,reltol=1e-03, trace=5, maxit=5000))


parms<- coef(outgmm)
x<-estim_matrix

rho<-parms[1]
delta <-parms[2]
u_bar<-parms[3]
theta_grid<-c(0, parms[4])
c_bar<-parms[5]
c<-c(parms[6],parms[7],parms[8])
w<-c(0,parms[9],parms[10], parms[11],
     parms[12], parms[13], parms[14], parms[15])
beta<-c(parms[16],parms[17],parms[18])
thetamat<-matrix(as.vector(sapply(1:N, function(x) {rep(theta_grid[x], length=T)}, simplify=TRUE)), ncol=T, nrow=N*T)
wmat<-matrix(w, nrow=N*T, ncol=T, byrow=FALSE)
h<-matrix(1, ncol=T, nrow=N*T)-  do.call(rbind,rep(list(diag(1,nrow=T, T)),N)) 

## this function will return gamma given sindex.
firm_estim<-function(sindex,ap2, ap3, ap4){
  aleph<-c(1-ap2-ap3-ap4, ap2, ap3, ap4)
  smax<- -(1-ap2-ap3-ap4)*spec_log(1-ap2-ap3-ap4)-ap2*spec_log(ap2)-ap3*spec_log(ap3)-ap4*spec_log(ap4)
  findlambda<-function(lambda){
    A<-exp(-lambda*(wmat -thetamat/rho*delta^h) )
    fxpt<-function(p,y){
      C<-colSums(t(A)*aleph/colSums(A*p))
      return(p*C)
    }
    objval<-function(p,y){
      C<-colSums(t(A)*aleph/colSums(A*p))
      return(sum(p*spec_log(C), na.rm=TRUE)-max(spec_log(C), na.rm=TRUE))
    }
    E0<-rep(1/(N*T),N*T)
    pf<-tryCatch({squarem(p=E0, y=1, fixptfn=fxpt, objfn=objval, control=list(tol=innertol, method=meth_choose,K=kchoose,maxiter=100000))},
                 error = function(e){
                   list(par=E0,convergence=TRUE )
                 })
    stopifnot(pf$convergence)
    E<-pf$par
    a<-t(t(A*E)/colSums(A*E)*aleph)
    E<-round(E,9)
    a<-(round(E,9)>0)*a
    I<-sum(a*spec_log(t(t(a/rowSums(a))/colSums(a))))
    diff<-sindex-I/smax
    diff<-ifelse(is.nan(diff),1,diff)
    return(diff)
  }
  lambda<-tryCatch({uniroot(findlambda, lower=0, upper=500, tol=outertol, extendInt = "downX")$root},
                   error = function(e){
                     if (findlambda(500)>0 & findlambda(0)>0 ){
                       return(500)
                     } else if (findlambda(0)<=0 & findlambda(500)<=0) {
                       return(0)
                     } else  
                       return(NA)
                   }) # when there is no root in range, it means constraint does not bind.
  # then we put in a large value for lambda.
  A<-exp(-lambda*(wmat -thetamat/rho*delta^h) )
  fxpt<-function(p,y){
    C<-colSums(t(A)*aleph/colSums(A*p))
    return(p*C)
  }
  objval<-function(p,y){
    C<-colSums(t(A)*aleph/colSums(A*p))
    return(sum(p*spec_log(C), na.rm=TRUE)-max(spec_log(C), na.rm=TRUE))
  }
  E0<-rep(1/(N*T),N*T)
  pf<-tryCatch({squarem(p=E0, y=1, fixptfn=fxpt, objfn=objval, control=list(tol=innertol, method=meth_choose,K=kchoose,maxiter=100000))},
               error = function(e){
                 list(par=E0,convergence=TRUE )
               })
  stopifnot(pf$convergence)
  E<-pf$par
  a<-t(t(A*E)/colSums(A*E)*aleph)
  E<-round(E,9)
  a<-(round(E,9)>0)*a
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

resid_cost<-x[,5]-1/rho/(1-x[,6])-x[,2:4]%*%c - W - gamma*x[,1]-c_bar
resid_u<-log(x[,6]/x[,7])-x[,2:4]%*%beta +rho*x[,5]-xi-u_bar


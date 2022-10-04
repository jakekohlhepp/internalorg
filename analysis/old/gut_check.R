### this script is to understand the method for a single firm.

library('data.table')
library('SQUAREM') # accelerate fixed point.
colMax <- function(data) apply(data,2, max, na.rm = TRUE)
colMin <- function(data) apply(data,2, min, na.rm = TRUE)
entropy<-function(x)  return(sum(-x*spec_log(x)))

spec_log<-function(x){
  ifelse(x==0 | x==-Inf | is.nan(x),0,log(x))
}
innertol<-1e-06
outertol<-1e-08
kchoose=1 
meth_choose=3

N<-2
T<-4
rho<-1.1
delta <-0.5
theta_grid<-c(0.01, 2)
gamma<-0.004
alpha<-c(0.01, 0.25, 0.25, 0.49)
w_base<-0
w<-c(0.4, 0.5, 0.3, 0.7,
     2, 3, 2.1, 2)+w_base
thetamat<-matrix(as.vector(sapply(1:N, function(x) {rep(theta_grid[x], length=T)}, simplify=TRUE)), ncol=T, nrow=N*T)
wmat<-matrix(w, nrow=N*T, ncol=T, byrow=FALSE)
h<-matrix(1, ncol=T, nrow=N*T)-  do.call(rbind,rep(list(diag(1,nrow=T, T)),N)) 
# construct inner mat. make everything relative to lowest.
m_mat<-wmat -thetamat/rho*delta^h
m_mat<-m_mat-min(m_mat)

### find optimal I
firm_estim<-function(gamma, ap1, ap2, ap3, ap4){
  aleph<-c(ap1, ap2, ap3, ap4)
  lambda<-1/gamma
  A<-exp(-lambda*(m_mat) )
  fxpt<-function(p,y){
    C<-colSums(t(A)*aleph/colSums(A*p))
    return(p*C)
  }
  objval<-function(p,y){
    C<-colSums(t(A)*aleph/colSums(A*p))
    return(sum(p*spec_log(C), na.rm=TRUE)-max(spec_log(C), na.rm=TRUE))
  }
  E0<-rep(1/(N*T),N*T)
  #, objfn=objval,
  pf<-squarem(p=E0, y=1, fixptfn=fxpt,objfn=objval,control=list(tol=innertol, method=meth_choose,K=kchoose,maxiter=100000))
  
  stopifnot(pf$convergence)
  E<-pf$par
  a<-t(t(A*E)/colSums(A*E)*aleph)
  E<-round(E,9)
  a<-(round(E,9)>0)*a
  dstar<- sum(a*(-rho*wmat +thetamat*delta^h))
  I<-sum(a*spec_log(t(t(a/rowSums(a))/colSums(a))))
  return(list(a,dstar,lambda,I ))
}
output<-firm_estim(gamma, alpha[1], alpha[2], alpha[3], alpha[4])
realI<-output[[4]][1]

### can we recover gamma?
get_gamma<-function(sindex, ap1, ap2, ap3, ap4){
  aleph<-c(ap1, ap2, ap3, ap4)
  findlambda<-function(lambda){
    A<-exp(-lambda*(m_mat) )
    fxpt<-function(p,y){
      C<-colSums(t(A)*aleph/colSums(A*p))
      return(p*C)
    }
    objval<-function(p,y){
      C<-colSums(t(A)*aleph/colSums(A*p))
      return(sum(p*spec_log(C), na.rm=TRUE)-max(spec_log(C), na.rm=TRUE))
    }
    E0<-rep(1/(N*T),N*T)
    pf<-squarem(p=E0, y=1, fixptfn=fxpt, objfn=objval, control=list(tol=innertol, method=meth_choose,K=kchoose,maxiter=100000))
    stopifnot(pf$convergence)
    E<-pf$par
    a<-t(t(A*E)/colSums(A*E)*aleph)
    E<-round(E,9)
    a<-(round(E,9)>0)*a
    I<-sum(a*spec_log(t(t(a/rowSums(a))/colSums(a))))
    diff<-sindex-I
    return(diff)
  }
  lambda<-tryCatch({uniroot(findlambda, lower=0, upper=1000, tol=outertol)$root},
                   error = function(e){
                     if (findlambda(1000)>0 & findlambda(0)>0 ){
                       return(1000)
                     } else if (findlambda(0)<=0 & findlambda(1000)<=0) {
                       return(0)
                     } else if (is.na(findlambda(1000))) {
                       return(NA)
                     }
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
  pf<-squarem(p=E0, y=1, fixptfn=fxpt, objfn=objval, control=list(tol=innertol, method=meth_choose,K=kchoose,maxiter=100000))
  stopifnot(pf$convergence)
  E<-pf$par
  a<-t(t(A*E)/colSums(A*E)*aleph)
  E<-round(E,9)
  a<-(round(E,9)>0)*a
  dstar<- sum(a*(-rho*wmat +thetamat*delta^h))
  return(list(a,dstar,lambda ))
}
output2<-get_gamma(realI, alpha[1], alpha[2], alpha[3], alpha[4])
print(output2[[3]][1]^(-1)-gamma)
entropy(alpha)
### this script works. this indicates there is some programming error in the two other scripts.
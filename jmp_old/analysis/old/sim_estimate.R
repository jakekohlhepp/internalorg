# write new program

library('data.table')
library('SQUAREM')

outertol<-1e-06
innertol<-1e-06
kchoose=1 
meth_choose=3



N<-2 ## if this is changed we need to change whole program.
T<-4
starting<-rep(1/(T*N), T*N)
c<-c(1,1,1,1)
rho<-1
delta <-0.5
beta<-c(1,2,3,4)
s_0<-0.2
theta_grid<-c(0.01, 2)

w_base<-0
w<-c(0.0006, 0.0005, 0.0005, 0.0005,
     1.99, 1.985, 1.985, 1.985)+w_base
thetamat<-matrix(as.vector(sapply(1:N, function(x) {rep(theta_grid[x], length=T)}, simplify=TRUE)), ncol=T, nrow=N*T)
wmat<-matrix(w, nrow=N*T, ncol=T, byrow=FALSE)

colMax <- function(data) apply(data,2, max, na.rm = TRUE)
colMin <- function(data) apply(data,2, min, na.rm = TRUE)
spec_log<-function(x){
  ifelse(x==0 | x==-Inf | is.nan(x),0,log(x))
}
h<-matrix(1, ncol=T, nrow=N*T)-  do.call(rbind,rep(list(diag(1,nrow=T, T)),N)) 
m_mat<-wmat -thetamat/rho*delta^h
m_mat<-m_mat-min(m_mat)

load("../analysis/data/01_00_sim_data.RData")

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
res<-lapply(1:200, function(x){get_gamma(tot_data[x,]$sindex, 
                                          tot_data[x,]$task_mix1,tot_data[x,]$task_mix2,
                                          tot_data[x,]$task_mix3, tot_data[x,]$task_mix4 )})
ret_gamma<-Vectorize(function(x){(res[x][[1]][[3]])^(-1) })


## check the result.
check<-data.table(est_gamma=ret_gamma(1:200), real_gamma=tot_data$gcheck, sindex=tot_data$sindex, entropy= tot_data$Imax )
View(check)


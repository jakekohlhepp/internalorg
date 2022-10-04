### given simulated data use estimation procedure.
### fixed to remove task 4 as covariate.
library('data.table')
library('lessR') # for the to() command
library('EnvStats') # for extreme value
library('SQUAREM')
library('gmm')

innertol<-1e-03
outertol<-1e-03
kchoose=1 
meth_choose=3

colMax <- function(data) apply(data,2, max, na.rm = TRUE)
colMin <- function(data) apply(data,2, min, na.rm = TRUE)
spec_log<-function(x)  ifelse(x==0 | x==-Inf | is.nan(x),0,log(x))
entropy<-function(x)  return(sum(-x*spec_log(x)))

load("../analysis/data/01_00_sim_data.RData")
tot_data[sindex<0,sindex:=0]

N<-2 ## if this is changed we need to change whole program.
T<-4
starting<-rep(1/(T*N), T*N)
# true
starting<-c(1.01,0.5, 0.024, 2,0,0,0,0, 0, 0,1.96-0.01,1.96-0.01,1.96-0.01,1.96-0.01,0,0,0,1.01+exp(0+0.2^2/2),1.01+exp(0+0.2^2/2) )
g<-function(parms, x){
  
  rho<-parms[1]
  delta <-parms[2]
  theta_grid<-c(parms[3], parms[4])
  
  c<-c(parms[5],parms[6],parms[7],0)
  w<-c(parms[8],parms[9],parms[10], 0,
       parms[11], parms[12], parms[13], parms[14])
  beta<-c(parms[15],parms[16],parms[17],0)
  omega_bar<-parms[18]
  nu_bar<-parms[19]
  
  thetamat<-matrix(as.vector(sapply(1:N, function(x) {rep(theta_grid[x], length=T)}, simplify=TRUE)), ncol=T, nrow=N*T)
  wmat<-matrix(w, nrow=N*T, ncol=T, byrow=FALSE)
  h<-matrix(1, ncol=T, nrow=N*T)-  do.call(rbind,rep(list(diag(1,nrow=T, T)),N)) 
  
  
  ## this function will return gamma given sindex.
  firm_estim<-function(sindex, ap1, ap2, ap3, ap4){
    aleph<-c(ap1, ap2, ap3, ap4)
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
                       #if (findlambda(1000)>0 & findlambda(0)>0 ){
                       #  return(1000)
                       #} else if (findlambda(0)<=0 & findlambda(1000)<=0) {
                       #  return(0)
                       #} else if (is.na(findlambda(1000))) {
                       #  return(NA)
                       #}
                       return(0)
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
  firm_estim<-Vectorize(firm_estim)
  
  res<-lapply(1:nrow(x), function(y){firm_estim(x[y,1], x[y,2], x[y,3], x[y,4], x[y,5])})
  
  # helper functions
  finda<-Vectorize(function(x){res[x][[1]][1]})
  ret_gamma<-Vectorize(function(x){(res[x][[1]][[3]])^(-1) })
  findW<-Vectorize(function(x){sum(wmat*res[x][[1]][[1]])})
  findqual<-Vectorize(function(x){sum(finda(x)[[1]]*(thetamat*delta^h)) })
  gamma<-ret_gamma(1:nrow(x))
  xi<-findqual(1:nrow(x))
  W<-findW(1:nrow(x))
  
  moment_mat<-cbind(x[,6]-1/rho-x[,2:5]%*%c - W - gamma*x[,1]-omega_bar,
                    log(x[,7]/x[,8])-x[,2:5]%*%beta+rho*x[,6]-xi-nu_bar)
  Z<-cbind(x[,2:4],x[,2:4]*x[,1], x[,2]*x[,9], x[,3]*x[,10], x[,4]*x[,11], x[,6])
  return(cbind(moment_mat[1,]*Z,moment_mat[2,]*Z ))
}

############## GMM
# x matrix should be:
# sindex, taskmix, price, share, share0, instruments.
data<-as.matrix(tot_data[1:200 & sindex/Imax<0.99,c("sindex", "task_mix1", "task_mix2", "task_mix3", "task_mix4",
                                   "price", "share", "s_0", "z1", "z2", "z3", "z4")])



outgmm<-gmm(g, x=data, t0=outgmm$coefficients, prewhite=0, type="twoStep",optfct="optim",wmatrix='ident',centeredVcov='TRUE', control=list(abstol=1e-1,reltol=1e-01, trace=5, maxit=1500))

cbind(round(starting,2), as.numeric(round(coef(outgmm),2)))


mom_calc<-function(x,data){
  output<-g(parms=x, x=data)
  for (x in 1:nrow(data)){
    if (x==1) tot<-output[x,]%*%t(output[x,])
    tot<-tot+output[x,]%*%t(output[x,])
  }
  return(tot/nrow(data))
}

mom_calc(outgmm$coefficients, data)->hand_vcov
outgmm$vcov<-hand_vcov
bigG<-function(parms){
  return(as.numeric(colSums(g(parms, data))/nrow(data)))
}
myenv <- new.env()
assign('parms', outgmm$coefficients, envir=myenv)

numericDeriv(quote(bigG(parms)),theta="parms", rho=myenv)->objective_deriv
attributes(objective_deriv)$gradient->gradient
library('MASS')
se<-ginv(t(gradient)%*%gradient)%*%t(gradient)%*%hand_vcov%*%gradient%*%ginv(t(gradient)%*%gradient)

sqrt(diag(se))



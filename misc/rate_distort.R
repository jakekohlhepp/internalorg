

T<-c(0.1,0.2, 0.3, 0.4)
E<-c(0.4,0.3, 0.2, 0.1)
l0<- -1

# in words:
# fix lambda. Find the optimal conditional.
# find lambda given conditional. repeat until convergence.
d<-matrix(1,nrow=length(T), ncol=length(T))-diag(1,nrow=length(T), ncol=length(T))

cond<-function(x){
  h<-matrix(x,nrow=length(T), ncol=length(T))
  return(  h-T*(exp(-l0*d))/rowSums(T*(exp(-l0*d)))
         )
}

# must also satisfy feasibility:
feas<-function(x){
  h<-matrix(x,nrow=length(T), ncol=length(T))
  return(T-t(E)%*%h)
}

full<-function(x){
  return(c(as.vector(feas(x)), as.vector(feas(x))))
}

part<-function(x){
  return(as.vector(cond(x)))
}


library('nleqslv')

sol<-nleqslv(rep(T,4),part)
# must also yield consistent lambda




# first fix 
# first 4 are mu, next 4 are v, last is lambda.
E<-E0
focs<-function(x){
  return(c(T-colSums(t(t(E))%*%T*exp(-1+theta^(-1)*(x[9]*d+t(x[1:4]+matrix(0,nrow=length(T), ncol=length(T)))+x[5:8]))),
           1- rowSums(rbind(T,T,T,T)*exp(-1+theta^(-1)*(x[9]*d+t(x[1:4]+matrix(0,nrow=length(T), ncol=length(T)))+x[5:8]))),
           q- sum(d*(t(t(E))%*%T)*exp(-1+theta^(-1)*(x[9]*d+t(x[1:4]+matrix(0,nrow=length(T), ncol=length(T)))+x[5:8]))) ))
}


library('nleqslv')

# subproblem: find point which generates generalist
generalist<-function(x){
  return(-1+theta^(-1)*(t(x[1:4]+matrix(0,nrow=length(T), ncol=length(T)))+x[5:8] ))
}
X0<-rep(1,8)

# sol<-nleqslv(X0,generalist, control=list(allowSingular=TRUE,maxit=1000))
# start at generalist point.
X0<-c(X0,0)

sol<-nleqslv(X0,focs, control=list(allowSingular=TRUE,maxit=2000))

calcb<-function(x){
  return(exp(-1+theta^(-1)*(x[9]*d+t(x[1:4]+matrix(0,nrow=length(T), ncol=length(T)))+x[5:8])) *rbind(T,T,T,T))
}

## next find what the implied inner objective are:
inner<-function(x){
  return( theta*colSums(calcb(sol$x) * log(calcb(sol$x)/rbind(T,T,T,T))) - w 
          + x[9]*(q-sum(calcb(sol$x)*d))+x[1:4]*(1-colSums(calcb(sol$x)))
          +colSums(x[5:8]*T-calcb(sol$x)))
}

## next find e:
calc<-function(x){
  calcb(sol$x)
}






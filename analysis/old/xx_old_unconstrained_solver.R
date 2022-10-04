#### First attempt: minimize unconstrained problem:
library('nloptr')
spec_log<-function(x){
  ifelse(x==0 | x==Inf | x==-Inf | is.nan(x),0,log(x))
}
d<-diag(1,nrow=T, T)

objective<-function(x){
  e<-x[1:6]
  b<-matrix(x[7:42], ncol=T, nrow=T)
  theta = xi/(1-delta)/( sum(d*b*e) + delta/(1-delta))
  return(gamma*sum(e*rowSums(b*spec_log(b/alpha)))+ 
           w_v[1]*theta + w_v[2]*theta^2 +
           w_h*e)
}

eval_g0 <- function( x) {
  return(colSums(x[1:6]*matrix(x[7:42], ncol=T, nrow=T))-alpha)
}

res1 <- nloptr( x0=c(rep(1/6,6),rep(alpha,6)),
                eval_f=objective,
                lb = rep(0,42),
                ub = rep(1,42),
                eval_g_ineq = eval_g0,
                opts = list("algorithm"="NLOPT_GN_ISRES",
                            "xtol_rel"=1.0e-6, "maxeval" = 100000))


### old ba way

lambda = 3
d<-matrix(1, ncol=T, nrow=T)-diag(1,nrow=T, T)
A<-exp(lambda*d + 1/gamma*matrix(w_h, ncol=6, nrow=6, byrow=FALSE))

p0<-rep(1/6,6)
pcond0<-matrix(1/6,ncol=6, nrow=6)
val<-100
i<-1
while (abs(val)>0.0001){
  if (i==1){
    p<-p0
    pcond<-pcond0
  } 
  pcond<-t(t(A)*alpha)/rowSums(p*A)
  
  i<-i+1
  val<-sum(p*spec_log(p*pcond))-max(spec_log(p*pcond))
  print(val)
  p<-rowSums(t(alpha*t(pcond)))
  
}
### code up b-a algorithm to solve job-design problem.
## use new model


### test module

if(TRUE==TRUE){
  alpha<-c(0.2,0.2, 0.2, 0.1,0.1,0.2)
  xi<-2.4
  xi_bar<-5
  xi_lower<-0
  T=6 # number of tasks
  N=5 # number of skill levels
  theta<-seq(from=xi_lower, to=xi_bar, length=N)
  d<-c(0.2,0.2, 0.3, 0.1,0.1,0.2)
  m<-5
  w<- m*as.vector(sapply(1:N, function(x) {rep(theta[x], length=T)}, simplify=TRUE))+rep(d, N)
  wmat<-matrix(w, nrow=N*T, ncol=T, byrow=FALSE)
  thetamat<-matrix(as.vector(sapply(1:N, function(x) {rep(theta[x], length=T)}, simplify=TRUE)), ncol=T, nrow=N*T)
  delta<-1

}

#####################
colMax <- function(data) apply(data,2, max, na.rm = TRUE)
colMin <- function(data) apply(data,2, min, na.rm = TRUE)
spec_log<-function(x){
  ifelse(x==0 | x==Inf | x==-Inf | is.nan(x),0,log(x))
}
h<-matrix(delta, ncol=T, nrow=N*T)-  do.call(rbind,rep(list(diag(delta,nrow=T, T)),N)) 

uu <- function(f, lower, upper, tol = 1e-8, maxiter =1000L, ...) {
  f.lower <- f(lower, ...)
  f.upper <- f(upper, ...)
  val <- .External2(stats:::C_zeroin2, function(arg) f(arg, ...),
                    lower, upper, f.lower, f.upper, tol, as.integer(maxiter))
  return(val[1])
}

## set tolerances
innertol<-1e-03
midtol<-1e-04
outertol<-1e-03
sindex<-1.5

 
findgamma<-function(gamma){

findlambda<-function(lambda){
  E<-rep(1/(N*T),N*T)
  newe<-0
  i<-1
  A<-exp(-lambda*(xi_bar - thetamat+h )-1/gamma*wmat )
  val<-100
  while (abs(val)>innertol){
    if (i>1){
      E<-newe
    } 
    C<-colSums(t(A)*alpha/colSums(A*E))
    newe<-E*C
    i<-i+1
    val<-sum(E*spec_log(C))-max(spec_log(C))
  }
  E<-newe
  
  a<-t(t(A*E)/colSums(A*E)*alpha)
  #a<-round(a, digits=8)
  #E<-round(E,digits=8)
  #I<-sum(a*spec_log(t(t(a/E)/alpha)))
  qdiff<-xi - sum(E*thetamat[,1])+sum(a*h)
  return(qdiff)
}

lambda<-uu(findlambda, lower=0, upper=100, tol=midtol)

h<-matrix(delta, ncol=T, nrow=N*T)-  do.call(rbind,rep(list(diag(delta,nrow=T, T)),N)) 
E<-rep(1/(N*T),N*T)
newe<-0
i<-1
A<-exp(-lambda*(xi_bar - thetamat+h )-1/gamma*wmat )
val<-100
while (abs(val)>innertol){
  if (i>1){
    E<-newe
  } 
  C<-colSums(t(A)*alpha/colSums(A*E))
  newe<-E*C
  i<-i+1
  val<-sum(E*spec_log(C))-max(spec_log(C))
}
E<-newe

a<-t(t(A*E)/colSums(A*E)*alpha)
#a<-round(a, digits=8)
#E<-round(E,digits=8)
I<-sum(a*spec_log(t(t(a/E)/alpha)))
#, gamma*I+sum(w*E)#
return(c(I-sindex ))
}
gamma<-uniroot(findgamma, lower=0.1, upper=100, tol=outertol, check.conv=TRUE)$root

findlambda<-function(lambda){
  E<-rep(1/(N*T),N*T)
  newe<-0
  i<-1
  A<-exp(-lambda*(xi_bar - thetamat+h )-1/gamma*wmat )
  val<-100
  while (abs(val)>innertol){
    if (i>1){
      E<-newe
    } 
    C<-colSums(t(A)*alpha/colSums(A*E))
    newe<-E*C
    i<-i+1
    val<-sum(E*spec_log(C))-max(spec_log(C))
  }
  E<-newe
  
  a<-t(t(A*E)/colSums(A*E)*alpha)
  #a<-round(a, digits=8)
  #E<-round(E,digits=8)
  #I<-sum(a*spec_log(t(t(a/E)/alpha)))
  qdiff<-xi - sum(E*thetamat[,1])+sum(a*h)
  return(qdiff)
}
lambda<-uniroot(findlambda, lower=0, upper=100, tol=midtol, check.conv=TRUE)$root

h<-matrix(delta, ncol=T, nrow=N*T)-  do.call(rbind,rep(list(diag(delta,nrow=T, T)),N)) 
E<-rep(1/(N*T),N*T)
newe<-0
i<-1
A<-exp(-lambda*(xi_bar - thetamat+h )-1/gamma*wmat )
val<-100
while (abs(val)>innertol){
  if (i>1){
    E<-newe
  } 
  C<-colSums(t(A)*alpha/colSums(A*E))
  newe<-E*C
  i<-i+1
  val<-sum(E*spec_log(C))-max(spec_log(C))
}
E<-newe

a<-t(t(A*E)/colSums(A*E)*alpha)
I<-sum(a*spec_log(t(t(a/E)/alpha)))
gamma*sindex + sum(E*w)








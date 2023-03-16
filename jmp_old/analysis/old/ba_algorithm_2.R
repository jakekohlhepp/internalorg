### code up b-a algorithm to solve job-design problem.
## use new model


### test module

if(TRUE==TRUE){
alpha<-c(0.2,0.2, 0.2, 0.1,0.1,0.2)
xi<-2.4
xi_bar<-3.3
xi_lower<-0
T=6 # number of tasks
N=2 # number of skill levels
theta<-seq(from=xi_lower, to=xi_bar, length=N)
d<-c(0.2,0.2, 0.3, 0.1,0.1,0.2)
m<-5
w<- m*as.vector(sapply(1:N, function(x) {rep(theta[x], length=T)}, simplify=TRUE))+rep(d, N)
wmat<-matrix(w, nrow=N*T, ncol=T, byrow=FALSE)
thetamat<-matrix(as.vector(sapply(1:N, function(x) {rep(theta[x], length=T)}, simplify=TRUE)), ncol=T, nrow=N*T)
delta<-1
sindex=0.4

}

#####################
colMax <- function(data) apply(data,2, max, na.rm = TRUE)
colMin <- function(data) apply(data,2, min, na.rm = TRUE)
spec_log<-function(x){
  ifelse(x==0 | x==Inf | x==-Inf | is.nan(x),0,log(x))
}


findgamma<-function(gamma){

findlambda<-function(lambda){
  e0<-rep(1,N*T)/(N*T)
  p0<-matrix(rep(1,N*T)/(N*T), nrow=N*T, ncol=T, byrow=FALSE)
  h<-matrix(delta, ncol=T, nrow=N*T)-  do.call(rbind,rep(list(diag(delta,nrow=T, T)),N))
  d_tort<-exp(-lambda*(xi_bar-thetamat+ rep(d, N))-1/gamma*(wmat) )
  val<-1000
  i<-1
  
  while (abs(val)>0.00000001 ){
    if (i==1) { 
      e<-e0
      p_cond<-p0
    }
    val<-sum(t(t(p_cond)*alpha)*spec_log(p_cond/e))
    p_cond<-t(t(e*d_tort)/colSums(e*d_tort))
    e<-rowSums(t(alpha*t(p_cond)))
    val<-val-sum(t(t(p_cond)*alpha)*spec_log(p_cond/e))
    i = i+1
  }
  I<-sum(t(t(p_cond)*alpha)*spec_log(p_cond/e))
  # check how far off quality is.
  return(xi - sum(e*thetamat[,1])+sum(t(t(p_cond)*alpha)*h))
  #I<-sum(t(t(p_cond)*alpha)*log(p_cond/e))
  #c<-gamma*I+sum(e*w)
}

lambda<-uniroot(findlambda, lower=0, upper=100, check.conv = TRUE)$root

e0<-rep(1,N*T)/(N*T)
p0<-matrix(rep(1,N*T)/(N*T), nrow=N*T, ncol=T, byrow=FALSE)
h<-matrix(delta, ncol=T, nrow=N*T)-  do.call(rbind,rep(list(diag(delta,nrow=T, T)),N))
d_tort<-exp(-lambda*(xi_bar-thetamat+ rep(d, N))-1/gamma*(wmat) )
val<-1000
i<-1

while (abs(val)>0.00000001 ){
  if (i==1) { 
    e<-e0
    p_cond<-p0
  }
  val<-sum(t(t(p_cond)*alpha)*spec_log(p_cond/e))
  p_cond<-t(t(e*d_tort)/colSums(e*d_tort))
  e<-rowSums(t(alpha*t(p_cond)))
  val<-val-sum(t(t(p_cond)*alpha)*spec_log(p_cond/e))
  i = i+1
}
I<-sum(t(t(p_cond)*alpha)*spec_log(p_cond/e))
return(sindex-I)

}





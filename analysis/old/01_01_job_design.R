### code up b-a algorithm to solve job-design problem.
## use new model


### test module

if(TRUE==FALSE){
alpha<-c(0.2,0.2, 0.2, 0.1,0.1,0.2)
xi<-2.4
xi_bar<-3.3
xi_lower<-1
T=6 # number of tasks
N=4 # number of skill levels
theta<-seq(from=xi_lower, to=xi_bar, length=N)
d<-c(0.2,0.2, 0.3, 0.1,0.1,0.2)
m<-3
w<- m*as.vector(sapply(1:N, function(x) {rep(theta[x], length=T)}, simplify=TRUE))+rep(d, N)
thetamat<-matrix(as.vector(sapply(1:N, function(x) {rep(theta[x], length=T)}, simplify=TRUE)), ncol=T, nrow=N*T)
delta<-0.5
}

#####################
colMax <- function(data) apply(data,2, max, na.rm = TRUE)
colMin <- function(data) apply(data,2, min, na.rm = TRUE)
spec_log<-function(x){
  ifelse(x==0 | x==Inf | x==-Inf | is.nan(x),0,log(x))
}

firm_estim<-function(sindex,xi,t1, t2, t3, t4, t5,t6 ){
  alpha<-c(t1, t2, t3, t4, t5,t6)
  findgamma<-function(gamma, Istar){
    lambda<-m/gamma
    h<-matrix(delta, ncol=T, nrow=N*T)-  do.call(rbind,rep(list(diag(delta,nrow=T, T)),N)) 
    E<-rep(1/N,N)
    newe<-0
    i<-1
    A<-exp(-lambda*(h*m + rep(d, N)) )
    val<-100
    while (abs(val)>0.000001){
      if (i>1){
        E<-newe
      } 
      C<-colSums(t(A)*alpha/colSums(A*E))
      newe<-E*C
      i<-i+1
      val<-sum(E*spec_log(C))-max(spec_log(C))
      #print(val)
    }
    E<-newe
    
    calc_emp_giventask<-function(x){
      return(t(t(A*x)/colSums(A*x)))
    }
    
    
    stopifnot(abs(sum(E)-1)<0.000001)
    
    calc_a<-function(x){
      return(t(t(calc_emp_giventask(x))*alpha))
    }
    
    #calc_a(E)
    
    calc_job<-function(x){
      return(calc_a(x)/E)
    }
    
    #calc_job(E)
    
    e<-round(E,digits=3)
    b<-calc_job(E)[e>0,]
    e<-e[e>0]
    I<-sum(e*b*spec_log(t(t(b)/alpha)))
    
    ## find q, check with constraints
    return(I-Istar)
    
  }
  res<-uniroot(findgamma, lower=0.1, upper=40, check.conv = TRUE, Istar=sindex)
  
  # now produce everything.
  lambda<-m/res$root
  h<-matrix(delta, ncol=T, nrow=N*T)-  do.call(rbind,rep(list(diag(delta,nrow=T, T)),N)) 
  E<-rep(1/N,N)
  newe<-0
  i<-1
  A<-exp(-lambda*(h*m + rep(d, N)) )
  val<-100
  while (abs(val)>0.000001){
    if (i>1){
      E<-newe
    } 
    C<-colSums(t(A)*alpha/colSums(A*E))
    newe<-E*C
    i<-i+1
    val<-sum(E*spec_log(C))-max(spec_log(C))
  }
  E<-newe
  calc_emp_giventask<-function(x){
    return(t(t(A*x)/colSums(A*x)))
  }
  calc_a<-function(x){
    return(t(t(calc_emp_giventask(x))*alpha))
  }
  calc_job<-function(x){
    return(calc_a(x)/E)
  }

  e<-round(E,digits=3)
  b<-calc_job(E)[e>0,]
  w_edit<-w[e>0]
  theta_help<-thetamat[e>0,]
  h_help<-h[e>0,]
  e<-e[e>0]
  I<-sum(e*b*spec_log(t(t(b)/alpha)))
  probqual<- sum(e*b*(theta_help-h_help>xi))
  return(c(res$root, res$root*sindex + sum(e*w_edit),
           colMax(b)-colMin(b),
           probqual))
}















### code up b-a algorithm to solve job-design problem.
## use new model
library('rootSolve')
###### Given by data:
alpha<-c(0.2,0.2, 0.2, 0.1,0.1,0.2)
delta<-0.9
theta<-c(1, 2,3) #third must be max
xi<-2.4
T=6
N=18
theta_vec<-c(rep(theta[1], N/T),rep(theta[2], N/T),rep(theta[3], N/T) )
c<-5
#####################

##### Parameter guesses.
w<-c(0.01,0.2, 0.2, 0.2,0.2,0.2,
     0.6,0.6, 0.6, 0.6,0.6,0.6,
     3,3, 3, 0.3,3,3) 
stopifnot(length(w)==N)
#####################

### re-write problem as constrained optimization over MI (so an R-D problem).
### know that multiplier on wage constraint should be 1/gamma.
### then free multiplier is on quality constraint. iterate over this multiplier until:
# summation of off identical indicator is equal to 1-xi/theta/(1-delta)+ delta/(1-delta)
# then create grid of theta. theta will be bounded between [xi, xi/delta]
# find theta which minimizes objective.



#### first solve for fixed multipliers with fixed theta
### then iterate over theta.
spec_log<-function(x){
  ifelse(x==0 | x==Inf | x==-Inf | is.nan(x),0,log(x))
}


ba<-function(l1, l2){
  d<-matrix(1, ncol=T, nrow=N)-rbind(diag(1,nrow=T, T),diag(1,nrow=T, T),diag(1,nrow=T, T))
  E<-rep(1/N,N)
  newe<-0
  i<-1
  A<-exp(-l1*(theta[3]-matrix(theta_vec, ncol=T, nrow=N, byrow=FALSE)*delta^d) - l2*matrix(w, ncol=T, nrow=N, byrow=FALSE))
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
  q<-sum(calc_a(E)*(theta[3]-matrix(theta_vec, ncol=T, nrow=N, byrow=FALSE)*delta^d))
  cost_resid<-c-1/l2*I-sum(e*w)
  return(c(q-theta[3]+xi,cost_resid))
}
ba_all<-function(l1,l2){
  d<-matrix(1, ncol=T, nrow=N)-rbind(diag(1,nrow=T, T),diag(1,nrow=T, T),diag(1,nrow=T, T))
  E<-rep(1/N,N)
  newe<-0
  i<-1
  A<-exp(-l1*(theta[3]-matrix(theta_vec, ncol=T, nrow=N, byrow=FALSE)*delta^d) - l2*matrix(w, ncol=T, nrow=N, byrow=FALSE))
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
  
  
  stopifnot(all(abs(colSums(calc_emp_giventask(E))-1)<0.000001))
  
  calc_a<-function(x){
    return(t(t(calc_emp_giventask(x))*alpha))
  }
  
  #calc_a(E)
  
  calc_job<-function(x){
    return(calc_a(x)/E)
  }
  
  #calc_job(E)
  
  return(list(e=E, b=calc_job(E)))
}

# for each l2, find l1 which minimizes.
lambda_star<-function(x){
  return(uniroot(ba, interval=c(0,100),l2=x)$f.root)
}
# select l2 so that 

calc_i<-function(x){
  e<-round(x$e,digits=3)
  b<-x$b[e>0]
  return(sum(e*b*spec_log(t(t(b)/alpha))))
}

model<-function(x){
  hold<-ba(x[1], x[2])
  return(c(F1=hold[1],F2=hold[2] ))
}
ba(1,0.1)

res<-sapply(seq(from=0.1, to=5, by=0.5), function(x) sapply(seq(from=0.1, to=5, by=0.5), function(y) ba(x,y)))

res<-outer(seq(from=0.1, to=5, by=0.1),seq(from=0.1, to=5, by=0.1),ba)
#ss <- multiroot(f = model, start = c(1, 0.1), positive=TRUE)
#ss<-nleqslv(fn = model, x = c(2, 1))
res<-ba_all(ss$root[1],ss$root[2] )
calc_i(res)
### code up b-a algorithm to solve job-design problem.


###### Given by data:
alpha<-c(0.2,0.2, 0.2, 0.1,0.1,0.2)
delta<-0.9
xi<-1
gamma<-10
T=6
#####################

##### Parameter guesses.
w_h<-c(0.1,0.2, 0.2, 0.2,0.3,0.2) 
w_v <-c(0.01,0) # wage is quadratic in theta. first coef is linear, second is quadratic
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


ba<-function(lambda, theta){
  d<-matrix(1, ncol=T, nrow=T)-diag(1,nrow=T, T)
  E<-rep(1/6,6)
  newe<-0
  i<-1
  A<-exp(-lambda*d - 1/gamma*matrix(w_h, ncol=6, nrow=6, byrow=FALSE))
  val<-100
  while (abs(val)>0.0001){
    if (i>1){
      E<-newe
    } 
    C<-rowSums(t(t(A)*alpha)/rowSums(A*E))
    newe<-E*C
    i<-i+1
    val<-sum(E*log(C))-max(log(C))
    #print(val)
  }
  E<-newe
  
  calc_emp_giventask<-function(x){
    return(t(t(A*E)/colSums(A*E)))
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
  
  ## find q
  q<-sum(diag(calc_a(E)))
  return(q - (1-xi/theta/(1-delta)+ delta/(1-delta)))
}
ba_all<-function(lambda){
  d<-matrix(1, ncol=T, nrow=T)-diag(1,nrow=T, T)
  E<-rep(1/6,6)
  newe<-0
  i<-1
  A<-exp(-lambda*d - 1/gamma*matrix(w_h, ncol=6, nrow=6, byrow=FALSE))
  val<-100
  while (abs(val)>0.0001){
    if (i>1){
      E<-newe
    } 
    C<-rowSums(t(t(A)*alpha)/rowSums(A*E))
    newe<-E*C
    i<-i+1
    val<-sum(E*log(C))-max(log(C))
    #print(val)
  }
  E<-newe
  
  calc_emp_giventask<-function(x){
    return(t(t(A*E)/colSums(A*E)))
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
  
  ## find q
  return(list(e=E, b=calc_job(E)))
}



findtheta<-function(x){

  lambda_star<-uniroot(ba, interval=c(-100,100), theta=x)
  jobs_types<-ba_all(lambda_star$root)
  return(gamma*sum(jobs_types$e*rowSums(jobs_types$b*spec_log(t(t(jobs_types$b)/alpha))
                   ))+sum(jobs_types$e*w_h)+x*w_v[1]+x^2*w_v[2])
}


res<-cbind(sapply(seq(xi+0.01,xi/delta, by=0.01),findtheta ),seq(xi+0.01,xi/delta, by=0.01))
res<-data.table(res)
names(res)<-c("val","theta")
plot(res$theta, res$val)
res[which.min(res$val),]$theta
theta_star<-res[which.min(res$val),]$theta
lambda_star<-uniroot(ba, interval=c(-100,100), theta=theta_star)$root
job_star<-ba_all(lambda_star)
sum(diag(job_star$e*job_star$b))
### code up b-a algorithm to solve job-design problem.
## use new model
###### Given by data:
c<-3
alpha<-c(0.2,0.2, 0.2, 0.1,0.1,0.2)
xi<-2.4
xi_bar<-3.3
xi_lower<-1
#####################

##### Parameter guesses.
d<-c(0.2,0.2, 0.2, 0.1,0.1,0.2)
m<-c(0.2,0.2, 0.2, 0.1,0.1,0.2)

delta<-0.5
T=6
N=6
theta<-seq(from=xi_lower, to=xi_upper, by=N)
theta_vec<-c(rep(theta[1], N),rep(theta[2], N),rep(theta[3], N) )
#####################

spec_log<-function(x){
  ifelse(x==0 | x==Inf | x==-Inf | is.nan(x),0,log(x))
}
ba<-function(l1, l2){
  d<-matrix(1, ncol=T, nrow=N*T)-rbind(diag(1,nrow=T, T),diag(1,nrow=T, T),diag(1,nrow=T, T))
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
  return(q-theta[3]+xi)
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
calc_c<-function(x, l){
  e<-round(x$e,digits=3)
  b<-x$b[e>0,]
  temp<-w[e>0]
  e<-e[e>0]
  return(sum(e*temp)+1/l*sum(e*b*spec_log(t(t(b)/alpha))))
}
calc_i<-function(x){
  e<-round(x$e,digits=3)
  b<-x$b[e>0,]
  e<-e[e>0]
  return(sum(e*b*spec_log(t(t(b)/alpha))))
}
doalgo<-function(g){
  l2=1/g
  
  res<-uniroot(ba,l2=l2, lower=0.00001, upper=20)
  return(c-calc_c(ba_all(res$root,l2), l2))
}

final_res<-sapply(seq(from=0.3, to=40, by=1),doalgo, c=5,xi=2.3,xi_bar=3,alpha=alpha)
final_res<-uniroot(doalgo,lower=0.3, upper=50)
final_res$root









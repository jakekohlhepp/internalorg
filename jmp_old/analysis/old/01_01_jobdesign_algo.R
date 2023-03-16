### code up b-a algorithm to solve job-design problem.
## use new model
###### Given by data:
#c<-400
#alpha<-c(0.1,0.3, 0.2, 0.1,0.1,0.2)
#xi<-2.8
#xi_bar<-3.3
#####################

##### Parameter guesses.
#m<-c(0.1, 0.1, 0.1, 0.1, 0.1, 0.1)
#d<-c(0.2, 0.2, 0.2, 0.2, 0.2, 0.2)
#w<-rep(d, 3)+c(m*0, m*1, m*2)
#delta<-0.5
#theta<-c(1, 2,xi_bar)

 #third must be max


solve_model<-function(c, alpha, xi, xi_bar, w, delta, theta){
T=6
N=18
theta_vec<-c(rep(theta[1], N/T),rep(theta[2], N/T),rep(theta[3], N/T) )
stopifnot(length(w)==N)
#####################

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
  E<-round(E, digits=4)
  k<-E>0
  b<-calc_job(E)[E>0,]
  E<-E[E>0]
  return(list(e=E, b=b,k=k))
}
calc_c<-function(x, l){
  temp<-w[x$k]
  return(sum(x$e*temp)+1/l*sum(x$e*x$b*spec_log(t(t(x$b)/alpha))))
}
calc_i<-function(x){
  return(sum(x$e*x$b*spec_log(t(t(x$b)/alpha))))
}
doalgo<-function(g){
  l2=1/g

  res<-uniroot(ba,l2=l2, lower=0.01, upper=10000)
  return(c-calc_c(ba_all(res$root,l2), l2))
}

#final_res<-sapply(seq(from=0.3, to=40, by=1),doalgo, c=5,xi=2.3,xi_bar=3,alpha=alpha)
final_res<-uniroot(doalgo,lower=0.01, upper=1000)
l2<-1/final_res$root
l1<-uniroot(ba,l2=l2, lower=0.01, upper=1000)
mat_all<-ba_all(l1$root,l2)
min_mat<-apply(mat_all$b, 2, min, na.rm=TRUE)
max_mat<-apply(mat_all$b, 2, max, na.rm=TRUE)

## produce the firm-specifc objects we need.
return(list(gamma=final_res$root,
            s_index=calc_i(mat_all),
            max_mat=max_mat,
            min_mat=min_mat
            ))

}

#solve_model(c=c, alpha=alpha, xi=xi, xi_bar=xi_bar,w=w, delta=delta, theta=theta )




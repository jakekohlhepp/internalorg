# first way - find gamma to match cost.
#spec_log<-function(x){
#  ifelse(x==0 | x==Inf | x==-Inf | is.nan(x),0,log(x))
#}


#costdiff<-function(gamma){
ba<-function(lambda){
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
  q<-sum(calc_a(E)*(h*m + rep(d, N)))
  return(q-(c-gamma*I)+m*xi)
}
res<-uniroot(ba, lower=0, upper=100, check.conv = TRUE, extendInt = "upX")
ba_all<-function(lambda){
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
  
  return(list(e=E, b=calc_job(E), I=I))
}
final_res<-ba_all(res$root)
#return(c-gamma*final_res$I-sum(w*final_res$e))
return(Istar-final_res$I)
}
#gamma<-uniroot(costdiff, lower=1, upper=20, extendInt = "yes", check.conv = TRUE)

# other way - find lambda to match I.



#ba<-function(lambda){
#    h<-matrix(delta, ncol=T, nrow=N*T)-  do.call(rbind,rep(list(diag(delta,nrow=T, T)),N)) 
#    E<-rep(1/N,N)
#    newe<-0
#    i<-1
#    A<-exp(-lambda*(h*m + rep(d, N)) )
#    val<-100
#    while (abs(val)>0.000001){
#      if (i>1){
#        E<-newe
#      } 
#      C<-colSums(t(A)*alpha/colSums(A*E))
#      newe<-E*C
#      i<-i+1
#      val<-sum(E*spec_log(C))-max(spec_log(C))
#      #print(val)
#    }
#    E<-newe
#    
#    calc_emp_giventask<-function(x){
#      return(t(t(A*x)/colSums(A*x)))
#    }
#    
#    
#    stopifnot(abs(sum(E)-1)<0.000001)
#    
#    calc_a<-function(x){
#      return(t(t(calc_emp_giventask(x))*alpha))
#    }
#    
#    #calc_a(E)
#    
#    calc_job<-function(x){
#      return(calc_a(x)/E)
#    }
#    
#    #calc_job(E)
#    
#    e<-round(E,digits=3)
#    b<-calc_job(E)[e>0,]
#    e<-e[e>0]
#    I<-sum(e*b*spec_log(t(t(b)/alpha)))
#    
#    ## find q, check with constraints
#    return(I-Istar)
#  }
#res<-uniroot(ba, lower=0, upper=100, check.conv = TRUE, extendInt = "upX")
#res$root

# lambda should be m/gamma. find gamma which matches I
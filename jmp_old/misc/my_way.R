
T<-c(0.1,0.2, 0.3, 0.4)
E0<-c(0.25, 0.25, 0.25, 0.25)
theta = 3
d<-diag(1,nrow=length(T), ncol=length(T))
q<-0.3


# suppose we drop the consistency constraint, and allow ourselves to find whatever e gives us the solution.
E<-E0
newe<-0
i<-1
while (sum(abs(newe-E))>0.000001){
  if (i>1){
    E<-newe
  } 
  
  X0<-c(rep(1,4),0)
  focs<-function(x){
    return(c(1- rowSums(rbind(T,T,T,T)*exp(-1+theta^(-1)*(x[5]*d+t(x[1:4]+matrix(0,nrow=length(T), ncol=length(T)))))),
             q- sum(d*(t(t(E))%*%T)*exp(-1+theta^(-1)*(x[5]*d+t(x[1:4]+matrix(0,nrow=length(T), ncol=length(T)))))) ))
  }
  sol<-nleqslv(X0,focs, control=list(allowSingular=TRUE,maxit=2000))
  # compute e
  
  A<-exp(-1+theta^(-1)*(sol$x[5]*d+t(sol$x[1:4]+matrix(0,nrow=length(T), ncol=length(T)))))
  newe<-colSums(solve(A))
  print(sum(abs(newe-E)))
  i<-i+1
}



calcb<-function(x){
  return(exp(-1+theta^(-1)*(x[5]*d+t(x[1:4]+matrix(0,nrow=length(T), ncol=length(T))))) *rbind(T,T,T,T))
}
calcb(sol$x)
calcb(sol$x)%*%E


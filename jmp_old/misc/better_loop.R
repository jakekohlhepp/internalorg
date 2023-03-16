
T<-c(0.30, 0.7)
w<-c(1,1)
E0<-c(0.25, 0.25)
d1<-matrix(1,nrow=length(T), ncol=length(T))-diag(1,nrow=length(T), ncol=length(T))
d2<-matrix(w, nrow=length(T), ncol=length(T))
l1<--2
l2<- 0

# suppose we drop the consistency constraint, and allow ourselves to find whatever e gives us the solution.
E<-E0
newe<-0
i<-1
A<-exp(l1*d1+l2*d2)
val<-100
while (abs(val)>0.0001){
  if (i>1){
    E<-newe
  } 
  C<-colSums(t(t(A)*T/rowSums(A*E)))
  newe<-E*C
  i<-i+1
  val<-sum(E*log(C))-max(log(C))
  print(val)
}
E<-newe
calc_emp_giventask<-function(x){
  return(t(t(A*E)/colSums(A*E)))
}

# columns should add to 1
stopifnot(all(abs(colSums(calc_emp_giventask(E))-1)<0.000001))

calc_a<-function(x){
  return(t(t(calc_emp_giventask(x))*T))
}

calc_a(E)

calc_job<-function(x){
  return(calc_a(x)/E)
}

calc_job(E)

## find q
sum(diag(calc_a(E)))

# find expense
sum(E*w)

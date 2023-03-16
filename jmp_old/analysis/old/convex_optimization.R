## use convex optimization package
#suppressWarnings(library(CVXR, warn.conflicts=FALSE))

c<-400
alpha<-c(0.1,0.3, 0.2, 0.25,0.05,0.1)
xi<- 2.5
xi_bar<-3

m<-c(0.1, 0.1, 0.1, 0.1, 0.1, 0.1)
d<-c(0.2, 0.2, 0.2, 0.2, 0.2, 0.2)
w<-rep(d, 3)+c(m*0, m*4, m*10)
delta<-0.6
theta<-c(0, 2,xi_bar)
T=6
N=18
theta_vec<-c(rep(theta[1], T),rep(theta[2], T),rep(theta[3], T) )
gamma<-2
spec_log<-function(x){
  ifelse(x==0,0,log(x))
}
spec_Log<-Vectorize(spec_log)
d<-matrix(1-delta, ncol=T, nrow=N)+rbind(diag(delta,nrow=T, T),diag(delta,nrow=T, T),diag(delta,nrow=T, T))

library('nloptr')
objective<-function(x){
  B<-matrix(x, nrow=N-1, ncol=T, byrow=TRUE)
  B<-rbind(B,1-colSums(B))
  return(gamma*sum(t(t(B)*alpha)*log(B/rowSums(t(t(B)*alpha)))) + sum(rowSums(t(t(B)*alpha))*w) )
}
constraints<-function(x){
  h<-numeric(T)
  B<-matrix(x, nrow=N-1, ncol=T, byrow=TRUE)
  h[1:(T-1)]<-1-colSums(B)
  B<-rbind(B,1-colSums(B))
  h[T]<-sum(t(t(B)*alpha)*theta_vec*d)-xi
  return(h)
}
x0<-rep(1/N, T*(N-1))
res<-cobyla(x0, objective,hin=constraints,
            lower=rep(0,(N-1)*T), upper=rep(1, (N-1)*T), control = list(xtol_rel = 1e-4, maxeval = 10000) )
print(res$value)






#e<-rep(1/N,N)
#b<-matrix(alpha, ncol=T, nrow=N, byrow=TRUE)
alphamat<-matrix(alpha, nrow=N, ncol=T, byrow=TRUE)
B<-Variable(rows=N, cols=T, nonneg = TRUE)
E<-Variable(rows=N, cols=T, nonneg = TRUE)

objective<-Minimize(  -sum(entr(alphamat*B))   ) 
constraint1<- sum_entries(B, axis=2) == rep(1,T)
constraint2<- sum(gamma*alphamat*B*( theta_vec*d)) <= xi
problem <- Problem(objective, constraints = list(constraint1, constraint2))
result <- solve(problem, gp=TRUE)

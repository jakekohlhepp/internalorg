## estimate model.
library('data.table')
library('lamW')
library('gmm')
library('matrixStats') # logsumexp function avoids overflow issue when computing welfare.
library('spatstat')
set.seed(5777)
get_os <- function(){
  sysinf <- Sys.info()
  if (!is.null(sysinf)){
    os <- sysinf['sysname']
    if (os == 'Darwin')
      os <- "osx"
  } else { ## mystery machine
    os <- .Platform$OS.type
    if (grepl("^darwin", R.version$os))
      os <- "osx"
    if (grepl("linux-gnu", R.version$os))
      os <- "linux"
  }
  tolower(os)
}
if (get_os()=="windows"){
  core_count<-1
} else{
  core_count<-42
} 
bisection <- function(f, a, b, n, xtol, ftol) {
  # If the signs of the function at the evaluated points, a and b, stop the function and return message.
  if (!(f(a) < 0) && (f(b) > 0)) {
    stop('end points not of opposite sign')
  } else if ((f(a) > 0) && (f(b) < 0)) {
    stop('end points not of opposite sign')
  }
  
  for (i in 1:n) {
    c <- (a + b) / 2 # Calculate midpoint
    
    # If the function equals 0 at the midpoint or the midpoint is below the desired tolerance, stop the 
    # function and return the root.
    #if (abs(f(c))<ftol || ((b - a) / 2) < xtol) {
    if (abs(f(c))<ftol || ((b - a) / 2) < xtol) {
      return(list("root"=c, "val"=f(c),"conv"=abs(f(c))<ftol || ((b - a) / 2) < xtol  ))
    }
    
    # If another iteration is required, 
    # check the signs of the function at the points c and a and reassign
    # a or b accordingly as the midpoint to be used in the next iteration.
    ifelse(sign(f(c)) == sign(f(a)), 
           a <- c,
           b <- c)
  }
  # If the max number of iterations is reached and no root has been found, 
  # return message and end function.
  return(list("root"=c, "val"=f(c),"conv"=abs(f(c))<ftol || ((b - a) / 2) < xtol  ))
}
innertol<-1e-08
outertol<-1e-08
############### import results
gamma_vec<-seq(from=0, to=10, length=1000)
alpha<-rep(1/3, 3)
oldwages<-rep(20,3)
smat<-rbind(c(29,33,40),
            c(38,34,30),
            c(35,35,35))
rho<-1


### function for solving firm's problem

counterfactual<-function(gamma_vec,wages){
  w1<-wages
  wmat<-matrix(w1, nrow=3, ncol=3, byrow=FALSE)
  h<-smat
  ## this function will return matrix given gamma
  firm_estim<-function(gamma){
    if (gamma<1000000000 & gamma>0){
      lambda<-1/gamma
      A<-exp(-lambda*(wmat -h/rho) )
      A[A>=Inf]<-1e16
      fxpt<-function(p){
        C<-colSums(t(A)*alpha/colSums(A*p))
        return(p*C)
      }
      objval<-function(p){
        C<-colSums(t(A)*alpha/colSums(A*p))
        return(-sum(p*spec_log(C), na.rm=TRUE)+max(spec_log(C), na.rm=TRUE))
      }
      E<-rep(1/3,3)
      E<-squarem(E,fixptfn = fxpt, control=list(maxiter=100000,tol=innertol) )$par
      a<-t(t(A*E)/colSums(A*E)*alpha)
    } else{
      if (gamma==0){
        # this is when the firm is frictionless, in which case they use water filling method.
        a<-matrix(0, ncol=3, nrow=3)
        for (col in 1:3){
          a[which.min((wmat -h/rho)[,col]),col]<-alpha[col]
        }
        E<-rowSums(a)
        lambda<-Inf
      } else{
        # this is for firms with unidentified high gamma.
        # assume their gamma is still high enough s.t. they choose least complex structure.
        ahelper<-t(matrix(alpha,length(alpha), 3))
        E<-rep(0,3)
        E[which.min(rowSums((wmat -h/rho)*ahelper))[1]]<-1
        a<-E*ahelper
        lambda<-0
      }
    }
    dstar<- sum(a*(-rho*wmat +h))
    return(list(a,dstar,lambda ))
  }
  firm_estim<-Vectorize(firm_estim)
  
  res<-mclapply(1:length(gamma_vec), function(y){firm_estim(gamma_vec[y])}, mc.cores=core_count)
  
  # helper functions
  finda<-Vectorize(function(x){res[x][[1]][1]})
  findW<-Vectorize(function(y){sum(wmat*res[y][[1]][[1]])})
  findqual<-Vectorize(function(y){sum(finda(y)[[1]]*(h)) })
  findI<-Vectorize(function(y){
    a<-res[[y]][[1]]
    return(sum(a*spec_log(t(t(a/rowSums(a))/colSums(a)))))
  })
  
  bmats<-finda(1:length(gamma_vec))
  return(list("bmats"=bmats))
  
}


# perturb wages
out<-counterfactual( gamma_vec, oldwages)
wages<-oldwages
wages[1]<-21
out2<-counterfactual(gamma_vec, wages)


change_out<-data.table()

for (estab in 1:length(gamma_vec)){
  piece<-rowSums(out$bmats[[estab]])
  piece<-c(piece,rowSums(out2$bmats[[estab]]))
  change_out<-rbind(change_out, t(piece))
}
names(change_out)<-c(paste0("E0_", 1:3),paste0("E1_", 1:3))


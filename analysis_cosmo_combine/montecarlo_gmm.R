# this version imposes a higher penalty
library('parallel')
library('SQUAREM')

## special helper functions
colMax <- function(data) apply(data,2, max, na.rm = TRUE)
colMin <- function(data) apply(data,2, min, na.rm = TRUE)
spec_log<-function(x)  ifelse(x==0 | x==-Inf | is.nan(x),0,log(x))
entropy<-function(x)  return(sum(-x*spec_log(x)))


## taken from https://rpubs.com/aaronsc32/bisection-method-r

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


## gmm objective
g<-function(parms, x){
  
  rho<-exp(parms[1])*tau
  u_bar<-parms[2]
  c_bar<-parms[3]
  w1<-wage_bound(c(parms[4],parms[5],parms[6]))
  cmat1<-parms[7:8]
  umat<-parms[9:10]
  smat<-parms[11:13]
  
  wmat<-matrix(w1, nrow=3, ncol=3, byrow=FALSE)
  h<-diag(smat)
  
  ## this function will return gamma given sindex.
  firm_estim<-function(sindex,ap2, ap3, ap4,ap5, abar){
    aleph<-c(1-ap2-ap3-ap4-ap5, ap2, ap3, ap4, ap5)
    findlambda<-function(lambda){
      A<-exp(-lambda*(wmat -h/rho/abar) )
      A[A>=Inf]<-1e16
      fxpt<-function(p){
        C<-colSums(t(A)*aleph/colSums(A*p))
        return(p*C)
      }
      objval<-function(p){
        C<-colSums(t(A)*aleph/colSums(A*p))
        return(-sum(p*spec_log(C), na.rm=TRUE)+max(spec_log(C), na.rm=TRUE))
      }
      E<-rep(1/5,5)
      E<-squarem(E,fixptfn = fxpt, control=list(maxiter=100000000,tol=innertol) )$par
      a<-t(t(A*E)/colSums(A*E)*aleph)
      I<-sum(a*spec_log(t(t(a/rowSums(a))/colSums(a))))
      diff<-I-sindex
      return(as.numeric(diff))
    }
    if (round(sindex/outertol)*outertol>0){
      # if end points not of opposite sign, this means either we are hitting a maximum machine readable number issue OR we gamma is 0.
      lambda<-tryCatch({bisection(findlambda, a=0, b=100000, ftol=outertol,xtol=outertol,n=100000000000000)$root},
                       error = function(e){
                         if (grepl("end points not of opposite sign",e, fixed=TRUE)){
                           return(Inf)
                         } else{
                           tryCatch({bisection(findlambda, a=0, b=10000, ftol=outertol,xtol=outertol,n=100000000000000)$root},
                                    error = function(e){
                                      if (grepl("end points not of opposite sign",e, fixed=TRUE)){
                                        return(Inf)
                                      } else{
                                        tryCatch({bisection(findlambda, a=0, b=1000, ftol=outertol,xtol=outertol,n=1000000000000000)$root},
                                                 error = function(e){
                                                   if (grepl("end points not of opposite sign",e, fixed=TRUE)){
                                                     return(Inf)
                                                   } else{
                                                     tryCatch({bisection(findlambda, a=0, b=100, ftol=outertol,xtol=outertol,n=100000000000000)$root},
                                                              error = function(e){
                                                                if (grepl("end points not of opposite sign",e, fixed=TRUE)){
                                                                  return(Inf)
                                                                } else{
                                                                  tryCatch({bisection(findlambda, a=0, b=10, ftol=outertol,xtol=outertol,n=100000000000000)$root},
                                                                           error = function(e){
                                                                             if (grepl("end points not of opposite sign",e, fixed=TRUE)){
                                                                               return(Inf) 
                                                                             } else{
                                                                               tryCatch({bisection(findlambda, a=0, b=1, ftol=outertol,xtol=outertol,n=100000000000000)$root},
                                                                                        error = function(e){
                                                                                          if (grepl("end points not of opposite sign",e, fixed=TRUE)){
                                                                                            return(Inf) 
                                                                                          } else{
                                                                                            tryCatch({bisection(findlambda, a=0, b=0.1, ftol=outertol,xtol=outertol,n=100000000000000)$root},
                                                                                                     error = function(e){
                                                                                                       if (grepl("end points not of opposite sign",e, fixed=TRUE)){
                                                                                                         return(Inf) 
                                                                                                       } else{
                                                                                                         tryCatch({bisection(findlambda, a=0, b=0.01, ftol=outertol,xtol=outertol,n=100000000000000)$root},
                                                                                                                  error = function(e){
                                                                                                                    if (grepl("end points not of opposite sign",e, fixed=TRUE)){
                                                                                                                      return(Inf) 
                                                                                                                    } else{
                                                                                                                      return(NaN)
                                                                                                                    }
                                                                                                                  })
                                                                                                       }
                                                                                                     })
                                                                                          }
                                                                                        })
                                                                             }
                                                                           }) 
                                                                }
                                                              }) 
                                                   }
                                                 }) 
                                      }
                                    }) 
                         }
                       })
      if (is.finite(lambda)){
        A<-exp(-lambda*(wmat -h/rho/abar) )
        A[A>=Inf]<-1e16
        fxpt<-function(p,y){
          C<-colSums(t(A)*aleph/colSums(A*p))
          return(p*C)
        }
        objval<-function(p,y){
          C<-colSums(t(A)*aleph/colSums(A*p))
          return(-sum(p*spec_log(C), na.rm=TRUE)+max(spec_log(C), na.rm=TRUE))
        }
        E<-rep(1/3,3)
        E<-squarem(E,fixptfn = fxpt, control=list(maxiter=100000000,tol=innertol) )$par
        a<-t(t(A*E)/colSums(A*E)*aleph)
      } else{
          a<-matrix(0, ncol=3, nrow=3)
          for (col in 1:3){
            a[which.min((wmat -h/rho/abar)[,col]),col]<-aleph[col]
          }
          E<-rowSums(a)
      }
    } else{
      ahelper<-t(matrix(aleph,length(aleph), 3))
      E<-rep(0,3)
      E[which.min(rowSums((wmat -h/rho/abar)*ahelper))[1]]<-1
      a<-E*ahelper
      lambda<-1/1000000000
    }
    dstar<- sum(a*(-rho*wmat +h))
    return(list(a,dstar,lambda ))
  }
  firm_estim<-Vectorize(firm_estim)
  
  res<-mclapply(1:nrow(x), function(y){firm_estim(x[y,1], x[y,2], x[y,3], x[y,4],x[y,5],x[y,7]
  )}, mc.cores=core_count)
  
  # helper functions
  finda<-Vectorize(function(x){res[x][[1]][1]})
  ret_gamma<-Vectorize(function(x){(res[x][[1]][[3]])^(-1) })
  findW<-Vectorize(function(y){sum(wmat*res[y][[1]][[1]])})
  findqual<-Vectorize(function(y){sum(finda(y)[[1]]*(h)) })
  gamma<-ret_gamma(1:nrow(x))
  xi<-findqual(1:nrow(x))
  W<-findW(1:nrow(x))
  
  ### penalty for guessing wage that gives 0 demand for a type.
  
  puzzle<-data.table()
  
  for (estab in 1:nrow(estim_sample)){
    piece<-rowSums(finda(estab)[[1]])*x[estab,7]*x[estab,9]
    piece<-c(piece, estim_sample[estab,]$quarter_year)
    puzzle<-rbind(puzzle, t(piece))
  }
  tot_labor<-puzzle[, .(L1=sum(V1), L2=sum(V2), L3=sum(V3)) ]
  penalty<-ifelse(any(as.numeric(tot_labor[, -c("V6")])/sum(as.numeric(tot_labor[, -c("V6")]))<0.01), Inf,1)
  
  moment_mat<-cbind(x[,4]-1/rho/(1-x[,5]) - x[,7]*(W + gamma*x[,1])-x[,2:3]%*%cmat1-c_bar,
                    log(x[,5]/x[,6]) +rho*x[,4]-xi-u_bar-x[,2:3]%*%umat)
  
  Z<-cbind(1,x[,2:3],x[,1],x[,2:3]*x[,1])
  wagemom<-x[,8]-W*x[,7]*x[,9]
  #return(penalty*cbind(moment_mat[,1]*Z,moment_mat[,2]*Z,wagemom)  )
  return(penalty*cbind(moment_mat[,1]*Z,moment_mat[,2]*Z, wagemom)  )
}






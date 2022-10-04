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
  
  rho<-parms[1]*tau
  u_bar<-parms[2]
  c_bar<-parms[3]
  w1<-c(parms[4],parms[5],parms[6],parms[7], parms[8])
  #w1<-c(parms[4],parms[5],parms[6],parms[7], parms[8])
  cmat1<-parms[9:12]
  umat<-parms[13:16]
  smat<-parms[17:21]
  
  wmat<-matrix(w1, nrow=5, ncol=5, byrow=FALSE)
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
        E<-rep(1/5,5)
        E<-squarem(E,fixptfn = fxpt, control=list(maxiter=100000000,tol=innertol) )$par
        a<-t(t(A*E)/colSums(A*E)*aleph)
      } else{
          a<-matrix(0, ncol=5, nrow=5)
          for (col in 1:5){
            a[which.min((wmat -h/rho/abar)[,col]),col]<-aleph[col]
          }
          E<-rowSums(a)
      }
    } else{
      ahelper<-t(matrix(aleph,length(aleph), 5))
      E<-rep(0,5)
      E[which.min(rowSums((wmat -h/rho/abar)*ahelper))[1]]<-1
      a<-E*ahelper
      lambda<-1/1000000000
    }
    dstar<- sum(a*(-rho*wmat +h))
    return(list(a,dstar,lambda ))
  }
  firm_estim<-Vectorize(firm_estim)
  
  res<-mclapply(1:nrow(x), function(y){firm_estim(x[y,1], x[y,2], x[y,3], x[y,4],x[y,5],x[y,9]
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
    piece<-rowSums(finda(estab)[[1]])*x[estab,9]*x[estab,11]
    piece<-c(piece, estim_sample[estab,]$quarter_year)
    puzzle<-rbind(puzzle, t(piece))
  }
  tot_labor<-puzzle[, .(L1=sum(V1), L2=sum(V2), L3=sum(V3), L4=sum(V4), L5=sum(V5)) , by=V6 ]
  penalty<-ifelse(any(as.numeric(tot_labor[, -c("V6")])/sum(as.numeric(tot_labor[, -c("V6")]))<0.01), Inf,1)
  
  moment_mat<-cbind(x[,6]-1/rho/(1-x[,7]) - x[,9]*(W + gamma*x[,1])-x[,2:5]%*%cmat1-c_bar,
                    log(x[,7]/x[,8]) +rho*x[,6]-xi-u_bar-x[,2:5]%*%umat)
  
  Z<-cbind(1,x[,2:5],x[,1],x[,2:5]*x[,1])
  wagemom<-x[,10]-W*x[,9]*x[,11]
  #return(penalty*cbind(moment_mat[,1]*Z,moment_mat[,2]*Z,wagemom)  )
  return(penalty*cbind(moment_mat[,1]*Z,moment_mat[,2]*Z, wagemom)  )
}

gmm_obj<-function(x, data){
  hold<-g(parms=x,x=data)
  hold<-sum(t(colMeans(hold))%*%weight_mat%*%colMeans(hold))
  return(hold)
}


gmm_moments<-function(x, data){
  hold<-g(parms=x,x=data)
  hold<-as.numeric(colMeans(hold))
  return(hold)
}

## return estimated objects
ret_objects<-function(parms, x){
  rho<-exp(parms[1])*tau
  u_bar<-parms[2]
  c_bar<-parms[3]
  w1<-wage_bound(c(parms[4],parms[5],parms[6],parms[7], parms[8]))
  cmat1<-parms[9:12]
  umat<-parms[13:16]
  smat<-parms[17:21]
  
  
  wmat<-matrix(w1, nrow=5, ncol=5, byrow=FALSE)
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
                                        tryCatch({bisection(findlambda, a=0, b=1000, ftol=outertol,xtol=outertol,n=100000000000000)$root},
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
                                                                               tryCatch({bisection(findlambda, a=0, b=1, ftol=outertol,xtol=outertol,n=100000000000000, extendInt = 'yes')$root},
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
        E<-rep(1/5,5)
        E<-squarem(E,fixptfn = fxpt, control=list(maxiter=100000,tol=innertol) )$par
        a<-t(t(A*E)/colSums(A*E)*aleph)
      } else{
        a<-matrix(0, ncol=5, nrow=5)
        for (col in 1:5){
          a[which.min((wmat -h/rho/abar)[,col]),col]<-aleph[col]
        }
        E<-rowSums(a)
    }
    } else{
      ahelper<-t(matrix(aleph,length(aleph), 5))
      E<-rep(0,5)
      E[which.min(rowSums((wmat -h/rho/abar)*ahelper))[1]]<-1
      a<-E*ahelper
      lambda<-1/1000000000
    }
    dstar<- sum(a*(-rho*wmat +h))
    return(list(a,dstar,lambda ))
  }
  firm_estim<-Vectorize(firm_estim)
  
  res<-mclapply(1:nrow(x), function(y){firm_estim(x[y,1], x[y,2], x[y,3], x[y,4],x[y,5],x[y,9]
  )}, mc.cores=core_count)
  
  # helper functions
  finda<-Vectorize(function(x){res[x][[1]][1]})
  ret_gamma<-Vectorize(function(x){(res[x][[1]][[3]])^(-1) })
  findW<-Vectorize(function(y){sum(wmat*res[y][[1]][[1]])})
  findqual<-Vectorize(function(y){sum(finda(y)[[1]]*(h)) })
  gamma<-ret_gamma(1:nrow(x))
  xi<-findqual(1:nrow(x))
  W<-findW(1:nrow(x))

  cost_qual<-x[,9]*(W + gamma*x[,1])-xi/rho
  wage_qual<-x[,9]*W -xi/rho
  moment_mat<-cbind(x[,6]-1/rho/(1-x[,7]) - x[,9]*(W + gamma*x[,1])-x[,2:5]%*%cmat1-c_bar,
                    log(x[,7]/x[,8]) +rho*x[,6]-xi-u_bar-x[,2:5]%*%umat)
  
  Q<-xi+u_bar+x[,2:5]%*%umat+moment_mat[,2]
  C<-x[,9]*(W + gamma*x[,1]) + x[,2:5]%*%cmat1 +c_bar+moment_mat[,1]
  
  
  return(list("gammas"=ret_gamma(1:nrow(x)),
              "wagebills"=findW(1:nrow(x)),
              "quals"=findqual(1:nrow(x)),
              "bmats"=finda(1:nrow(x)),
              "phi"=moment_mat[,1],
              "nu"=moment_mat[,2],
              "cost_qual"=cost_qual,
              "wage_qual"=wage_qual,
              "Q"=Q,
              "C"=C
  ))
}




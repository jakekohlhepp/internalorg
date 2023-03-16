
library('parallel')

## special helper functions
colMax <- function(data) apply(data,2, max, na.rm = TRUE)
colMin <- function(data) apply(data,2, min, na.rm = TRUE)
spec_log<-function(x)  ifelse(x==0 | x==-Inf | is.nan(x),0,log(x))
entropy<-function(x)  return(sum(-x*spec_log(x)))


## gmm objective
g<-function(parms, x){
  rho<-parms[1]*tau
  u_bar<-parms[2]
  c_bar<-parms[3]
  w1<-wage_bound(c(parms[4],parms[5],parms[6],parms[7], parms[8], parms[9]))
  w2<-wage_bound(c(parms[10],parms[11],parms[12],parms[13], parms[14], parms[15]))
  w3<-wage_bound(c(parms[16],parms[17],parms[18],parms[19],parms[20],parms[21]))
  w4<-wage_bound(c(parms[22],parms[23],parms[24],parms[25],parms[26],parms[27] ))
  ut1<-parms[28]
  ut2<-parms[29]
  ut3<-parms[30]
  ct1<-parms[31]
  ct2<-parms[32]
  ct3<-parms[33]
  s1<-parms[34]
  s2<-parms[35]
  s3<-parms[36]
  s4<-parms[37]
  s5<-parms[38]
  s6<-parms[39]
  m1<-0
  m2<-0
  m3<-0
  m4<-0
  m5<-0
  m6<-0
  cmat<-c(parms[40],parms[41],parms[42],parms[43], parms[44])
  umat<-c(parms[45], parms[46], parms[47], parms[48], parms[49])
  mcoef<-parms[50]
  childcoef<-parms[51]
  
  wmat<-list(matrix(w1, nrow=6, ncol=6, byrow=FALSE),
             matrix(w2, nrow=6, ncol=6, byrow=FALSE),
             matrix(w3, nrow=6, ncol=6, byrow=FALSE),
             matrix(w4, nrow=6, ncol=6, byrow=FALSE))
  h<-rbind(c(s1,m2,m3,m4,m5,m6),c(m1,s2,m3,m4,m5,m6), c(m1,m2,s3,m4,m5,m6), c(m1,m2,m3,s4,m5,m6),
           c(m1,m2,m3,m4,s5,m6), c(m1,m2,m3,m4,m5,s6))
  
  ## this function will return gamma given sindex.
  firm_estim<-function(sindex,ap2, ap3, ap4,ap5, ap6,qts, abar){
    aleph<-c(1-ap2-ap3-ap4-ap5-ap6, ap2, ap3, ap4, ap5, ap6)
    findlambda<-function(lambda){
      A<-exp(-lambda*(wmat[[qts]] -h/rho/abar) )
      A[A>=Inf]<-1e8
      fxpt<-function(p){
        C<-colSums(t(A)*aleph/colSums(A*p))
        return(p*C)
      }
      objval<-function(p){
        C<-colSums(t(A)*aleph/colSums(A*p))
        return(sum(p*spec_log(C), na.rm=TRUE)-max(spec_log(C), na.rm=TRUE))
      }
      E<-rep(1/(6),6)
      valold<-0
      val<-1
      icount<-0
      while (abs(val-valold)>innertol & icount<100000){
        E<-fxpt(E)
        valold<-val
        val<-objval(E)
        icount<-icount+1
      }
      a<-t(t(A*E)/colSums(A*E)*aleph)
      a<-(round(E,7)>0)*a
      E<-round(E,7)
      I<-sum(a*spec_log(t(t(a/rowSums(a))/colSums(a))))
      diff<-sindex-I
      return(diff)
    }
    if (round(sindex,5)>0){
      lambda<-tryCatch({uniroot(findlambda, lower=0, upper=100000, tol=outertol)$root},
                       error = function(e){
                         if (grepl("end points not of opposite sign",e, fixed=TRUE)){
                           return(NaN)
                         } else{
                           tryCatch({uniroot(findlambda, lower=0, upper=10000, tol=outertol)$root},
                                    error = function(e){
                                      if (grepl("end points not of opposite sign",e, fixed=TRUE)){
                                        return(NaN)
                                      } else{
                                        tryCatch({uniroot(findlambda, lower=0, upper=1000, tol=outertol)$root},
                                                 error = function(e){
                                                   if (grepl("end points not of opposite sign",e, fixed=TRUE)){
                                                     return(NaN)
                                                   } else{
                                                     tryCatch({uniroot(findlambda, lower=0, upper=100, tol=outertol)$root},
                                                              error = function(e){
                                                                if (grepl("end points not of opposite sign",e, fixed=TRUE)){
                                                                  return(NaN)
                                                                } else{
                                                                  tryCatch({uniroot(findlambda, lower=0, upper=10, tol=outertol)$root},
                                                                           error = function(e){
                                                                             if (grepl("end points not of opposite sign",e, fixed=TRUE)){
                                                                               return(NaN) 
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
      
      A<-exp(-lambda*(wmat[[qts]] -h/rho/abar) )
      A[A>=Inf]<-1e8
      fxpt<-function(p,y){
        C<-colSums(t(A)*aleph/colSums(A*p))
        return(p*C)
      }
      objval<-function(p,y){
        C<-colSums(t(A)*aleph/colSums(A*p))
        return(sum(p*spec_log(C), na.rm=TRUE)-max(spec_log(C), na.rm=TRUE))
      }
      E<-rep(1/(6),6)
      valold<-0
      val<-1
      icount<-0
      while (abs(val-valold)>innertol & icount<100000){
        E<-fxpt(E)
        valold<-val
        val<-objval(E)
        icount<-icount+1
      }
      a<-t(t(A*E)/colSums(A*E)*aleph)
    } else{
      ahelper<-t(matrix(aleph,length(aleph), 6))
      E<-rep(0,6)
      E[which.min(rowSums((wmat[[qts]] -h/rho/abar)*ahelper))[1]]<-1
      a<-E*ahelper
      a<-(round(E,7)>0)*a
      E<-round(E,7)
      lambda<-1/1000000000
    }
    dstar<- sum(a*(-rho*wmat[[qts]] +h))
    return(list(a,dstar,lambda ))
  }
  firm_estim<-Vectorize(firm_estim)
  
  res<-mclapply(1:nrow(x), function(y){firm_estim(x[y,1], x[y,2], x[y,3], x[y,4],x[y,5],x[y,6],
                                                  ifelse(max(x[y,10:12])==1,which.max(x[y,10:12]),4),x[y,18]
  )}, mc.cores=core_count)
  
  # helper functions
  finda<-Vectorize(function(x){res[x][[1]][1]})
  ret_gamma<-Vectorize(function(x){(res[x][[1]][[3]])^(-1) })
  findW<-Vectorize(function(y){sum(wmat[[ifelse(max(x[y,10:12])==1,which.max(x[y,10:12]),4)]]*res[y][[1]][[1]])})
  findqual<-Vectorize(function(x){sum(finda(x)[[1]]*(h)) })
  gamma<-ret_gamma(1:nrow(x))
  xi<-findqual(1:nrow(x))
  W<-findW(1:nrow(x))
  
  
  ### penalty for guessing wage that gives 0 demand for a type.
  
  puzzle<-data.table()
  
  for (estab in 1:nrow(estim_sample)){
    piece<-rowSums(finda(estab)[[1]])
    piece<-c(piece, estim_sample[estab,]$quarter_year)
    puzzle<-rbind(puzzle, t(piece))
  }
  tot_labor<-puzzle[, .(L1=sum(V1), L2=sum(V2), L3=sum(V3), L4=sum(V4), L5=sum(V5), L6=sum(V6)) , by=V7 ]
  penalty<-ifelse(any(tot_labor<0.001), Inf,1)
  
  moment_mat<-cbind(x[,7]-1/rho/(1-x[,8]) - x[,18]*(W + gamma*x[,1])-x[,2:6]%*%cmat-c_bar
                    -ct1*x[,10]-ct2*x[,11]-ct3*x[,12],
                    log(x[,8]/x[,9]) +rho*x[,7]-xi-u_bar
                    -ut1*x[,10]-ut2*x[,11]-ut3*x[,12]-x[,2:6]%*%umat-x[,14]*mcoef-x[,15]*childcoef)
  
  Z<-cbind(1,x[,2:6],x[,1],x[,2:6]*x[,1], x[,13:17])
  Z<-cbind(Z, Z*x[,10], Z*x[,11],Z*x[,12])
  wagemom<-(W*x[,18]*x[,20]-x[,19])/sd(x[,19]) 
  wagemom<-cbind(wagemom, wagemom*x[,10], wagemom*x[,11],wagemom*x[,12])
  return(penalty*cbind(moment_mat[,1]*Z,moment_mat[,2]*Z,wagemom)  )
}

gmm_obj<-function(x, data){
  hold<-sum(colMeans(g(parms=x,x=data))^2)
  return(hold)
}

## return estimated objects
ret_objects<-function(parms, x){
  rho<-parms[1]*tau
  u_bar<-parms[2]
  c_bar<-parms[3]
  w1<-wage_bound(c(parms[4],parms[5],parms[6],parms[7], parms[8], parms[9]))
  w2<-wage_bound(c(parms[10],parms[11],parms[12],parms[13], parms[14], parms[15]))
  w3<-wage_bound(c(parms[16],parms[17],parms[18],parms[19],parms[20],parms[21]))
  w4<-wage_bound(c(parms[22],parms[23],parms[24],parms[25],parms[26],parms[27] ))
  ut1<-parms[28]
  ut2<-parms[29]
  ut3<-parms[30]
  ct1<-parms[31]
  ct2<-parms[32]
  ct3<-parms[33]
  s1<-parms[34]
  s2<-parms[35]
  s3<-parms[36]
  s4<-parms[37]
  s5<-parms[38]
  s6<-parms[39]
  m1<-0
  m2<-0
  m3<-0
  m4<-0
  m5<-0
  m6<-0
  cmat<-c(parms[40],parms[41],parms[42],parms[43], parms[44])
  umat<-c(parms[45], parms[46], parms[47], parms[48], parms[49])
  mcoef<-parms[50]
  childcoef<-parms[51]
  
  wmat<-list(matrix(w1, nrow=6, ncol=6, byrow=FALSE),
             matrix(w2, nrow=6, ncol=6, byrow=FALSE),
             matrix(w3, nrow=6, ncol=6, byrow=FALSE),
             matrix(w4, nrow=6, ncol=6, byrow=FALSE))
  h<-rbind(c(s1,m2,m3,m4,m5,m6),c(m1,s2,m3,m4,m5,m6), c(m1,m2,s3,m4,m5,m6), c(m1,m2,m3,s4,m5,m6),
           c(m1,m2,m3,m4,s5,m6), c(m1,m2,m3,m4,m5,s6))
  
  ## this function will return gamma given sindex.
  firm_estim<-function(sindex,ap2, ap3, ap4,ap5, ap6,qts,abar){
    aleph<-c(1-ap2-ap3-ap4-ap5-ap6, ap2, ap3, ap4, ap5, ap6)
    findlambda<-function(lambda){
      A<-exp(-lambda*(wmat[[qts]] -h/rho/abar) )
      A[A>=Inf]<-1e8
      fxpt<-function(p){
        C<-colSums(t(A)*aleph/colSums(A*p))
        return(p*C)
      }
      objval<-function(p){
        C<-colSums(t(A)*aleph/colSums(A*p))
        return(sum(p*spec_log(C), na.rm=TRUE)-max(spec_log(C), na.rm=TRUE))
      }
      E<-rep(1/(6),6)
      valold<-0
      val<-1
      icount<-0
      while (abs(val-valold)>innertol & icount<100000){
        E<-fxpt(E)
        valold<-val
        val<-objval(E)
        icount<-icount+1
      }
      a<-t(t(A*E)/colSums(A*E)*aleph)
      a<-(round(E,7)>0)*a
      E<-round(E,7)
      I<-sum(a*spec_log(t(t(a/rowSums(a))/colSums(a))))
      diff<-sindex-I
      return(diff)
    }
    if (round(sindex,5)>0){
      lambda<-tryCatch({uniroot(findlambda, lower=0, upper=100000, tol=outertol)$root},
                       error = function(e){
                         if (grepl("end points not of opposite sign",e, fixed=TRUE)){
                           return(NaN)
                         } else{
                           tryCatch({uniroot(findlambda, lower=0, upper=10000, tol=outertol)$root},
                                    error = function(e){
                                      if (grepl("end points not of opposite sign",e, fixed=TRUE)){
                                        return(NaN)
                                      } else{
                                        tryCatch({uniroot(findlambda, lower=0, upper=1000, tol=outertol)$root},
                                                 error = function(e){
                                                   if (grepl("end points not of opposite sign",e, fixed=TRUE)){
                                                     return(NaN)
                                                   } else{
                                                     tryCatch({uniroot(findlambda, lower=0, upper=100, tol=outertol)$root},
                                                              error = function(e){
                                                                if (grepl("end points not of opposite sign",e, fixed=TRUE)){
                                                                  return(NaN)
                                                                } else{
                                                                  tryCatch({uniroot(findlambda, lower=0, upper=10, tol=outertol)$root},
                                                                           error = function(e){
                                                                             if (grepl("end points not of opposite sign",e, fixed=TRUE)){
                                                                               return(NaN) 
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
      
      A<-exp(-lambda*(wmat[[qts]] -h/rho/abar) )
      A[A>=Inf]<-1e8
      fxpt<-function(p,y){
        C<-colSums(t(A)*aleph/colSums(A*p))
        return(p*C)
      }
      objval<-function(p,y){
        C<-colSums(t(A)*aleph/colSums(A*p))
        return(sum(p*spec_log(C), na.rm=TRUE)-max(spec_log(C), na.rm=TRUE))
      }
      E<-rep(1/(6),6)
      valold<-0
      val<-1
      icount<-0
      while (abs(val-valold)>innertol & icount<100000){
        E<-fxpt(E)
        valold<-val
        val<-objval(E)
        icount<-icount+1
      }
      a<-t(t(A*E)/colSums(A*E)*aleph)
    } else{
      ahelper<-t(matrix(aleph,length(aleph), 6))
      E<-rep(0,6)
      E[which.min(rowSums((wmat[[qts]] -h/rho/abar)*ahelper))[1]]<-1
      a<-E*ahelper
      a<-(round(E,7)>0)*a
      E<-round(E,7)
      lambda<-1/1000000000
    }
    dstar<- sum(a*(-rho*wmat[[qts]] +h))
    return(list(a,dstar,lambda ))
  }
  firm_estim<-Vectorize(firm_estim)
  
  res<-mclapply(1:nrow(x), function(y){firm_estim(x[y,1], x[y,2], x[y,3], x[y,4],x[y,5],x[y,6],
                                                  ifelse(max(x[y,10:12])==1,which.max(x[y,10:12]),4),x[y,18]
  )}, mc.cores=core_count)
  
  # helper functions
  finda<-Vectorize(function(x){res[x][[1]][1]})
  ret_gamma<-Vectorize(function(x){(res[x][[1]][[3]])^(-1) })
  findW<-Vectorize(function(y){sum(wmat[[ifelse(max(x[y,10:12])==1,which.max(x[y,10:12]),4)]]*res[y][[1]][[1]])})
  findqual<-Vectorize(function(x){sum(finda(x)[[1]]*(h)) })
  gamma<-ret_gamma(1:nrow(x))
  xi<-findqual(1:nrow(x))
  W<-findW(1:nrow(x))
  moment_mat<-cbind(x[,7]-1/rho/(1-x[,8]) - x[,18]*(W + gamma*x[,1])-x[,2:6]%*%cmat-c_bar
                    -ct1*x[,10]-ct2*x[,11]-ct3*x[,12],
                    log(x[,8]/x[,9]) +rho*x[,7]-xi-u_bar
                    -ut1*x[,10]-ut2*x[,11]-ut3*x[,12]-x[,2:6]%*%umat-x[,14]*mcoef-x[,15]*childcoef)
  
  return(list("gammas"=ret_gamma(1:nrow(x)),
              "wagebills"=findW(1:nrow(x)),
              "quals"=findqual(1:nrow(x)),
              "bmats"=finda(1:nrow(x)),
              "phi"=moment_mat[,1],
              "nu"=moment_mat[,2]
         ))
}




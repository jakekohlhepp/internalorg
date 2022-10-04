
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
  cmat1<-parms[34:38]
  cmat2<-parms[39:43]
  cmat3<-parms[44:48]
  cmat4<-parms[49:53]
  umat<-parms[54:58]
  
  smat<-rbind(parms[59:64],parms[65:70],parms[71:76],parms[77:82])
  
  
  wmat<-list(matrix(w1, nrow=6, ncol=6, byrow=FALSE),
             matrix(w2, nrow=6, ncol=6, byrow=FALSE),
             matrix(w3, nrow=6, ncol=6, byrow=FALSE),
             matrix(w4, nrow=6, ncol=6, byrow=FALSE))
  h<-lapply(1:4, function(x){
    return(diag(smat[x,]))
    
  })
  
  ## this function will return gamma given sindex.
  firm_estim<-function(sindex,ap2, ap3, ap4,ap5, ap6,qts, abar){
    aleph<-c(1-ap2-ap3-ap4-ap5-ap6, ap2, ap3, ap4, ap5, ap6)
    findlambda<-function(lambda){
      A<-exp(-lambda*(wmat[[qts]] -h[[qts]]/rho/abar) )
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
      # if end points not of opposite sign, this means either we are hitting a maximum machine readable number issue OR we gamma is 0.
      lambda<-tryCatch({uniroot(findlambda, lower=0, upper=100000, tol=outertol)$root},
                       error = function(e){
                         if (grepl("end points not of opposite sign",e, fixed=TRUE)){
                           return(Inf)
                         } else{
                           tryCatch({uniroot(findlambda, lower=0, upper=10000, tol=outertol)$root},
                                    error = function(e){
                                      if (grepl("end points not of opposite sign",e, fixed=TRUE)){
                                        return(Inf)
                                      } else{
                                        tryCatch({uniroot(findlambda, lower=0, upper=1000, tol=outertol)$root},
                                                 error = function(e){
                                                   if (grepl("end points not of opposite sign",e, fixed=TRUE)){
                                                     return(Inf)
                                                   } else{
                                                     tryCatch({uniroot(findlambda, lower=0, upper=100, tol=outertol)$root},
                                                              error = function(e){
                                                                if (grepl("end points not of opposite sign",e, fixed=TRUE)){
                                                                  return(Inf)
                                                                } else{
                                                                  tryCatch({uniroot(findlambda, lower=0, upper=10, tol=outertol)$root},
                                                                           error = function(e){
                                                                             if (grepl("end points not of opposite sign",e, fixed=TRUE)){
                                                                               return(Inf) 
                                                                             } else{
                                                                               tryCatch({uniroot(findlambda, lower=0, upper=1, tol=outertol, extendInt = 'yes')$root},
                                                                                        error = function(e){
                                                                                          if (grepl("end points not of opposite sign",e, fixed=TRUE)){
                                                                                            return(Inf) 
                                                                                          } else{
                                                                                            return(Inf)
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
      
      A<-exp(-lambda*(wmat[[qts]] -h[[qts]]/rho/abar) )
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
      E[which.min(rowSums((wmat[[qts]] -h[[qts]]/rho/abar)*ahelper))[1]]<-1
      a<-E*ahelper
      a<-(round(E,7)>0)*a
      E<-round(E,7)
      lambda<-1/1000000000
    }
    dstar<- sum(a*(-rho*wmat[[qts]] +h[[qts]]))
    return(list(a,dstar,lambda ))
  }
  firm_estim<-Vectorize(firm_estim)
  
  res<-mclapply(1:nrow(x), function(y){firm_estim(x[y,1], x[y,2], x[y,3], x[y,4],x[y,5],x[y,6],
                                                  ifelse(max(x[y,10:12])==1,which.max(x[y,10:12]),4),x[y,14]
  )}, mc.cores=core_count)
  
  # helper functions
  finda<-Vectorize(function(x){res[x][[1]][1]})
  ret_gamma<-Vectorize(function(x){(res[x][[1]][[3]])^(-1) })
  findW<-Vectorize(function(y){sum(wmat[[ifelse(max(x[y,10:12])==1,which.max(x[y,10:12]),4)]]*res[y][[1]][[1]])})
  findqual<-Vectorize(function(y){sum(finda(y)[[1]]*(h[[ifelse(max(x[y,10:12])==1,which.max(x[y,10:12]),4)]])) })
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
  penalty<-ifelse(any(tot_labor<0.0001), Inf,1)
  
  
  #### job based moments - variance in time spent across workers.
  #jobs_model<-c()
  #for (y in 1:nrow(x)){
  #  piece<-cbind(finda(y)[[1]]/rowSums(finda(y)[[1]]),rep(as.numeric(estim_sample[y,"cust_count"]*estim_sample[y,"avg_labor"]),6),rowSums(finda(y)[[1]]),y)
  #  jobs_model<-rbind(jobs_model,piece)
  #}
  #jobs_model<-data.table(jobs_model)
  #names(jobs_model)<-c("jobvect_1", "jobvect_2", "jobvect_3", "jobvect_4", "jobvect_5", "jobvect_6",
  #                     "tot_firm", "type_frac","estab")
  #jobs_model[,emp_tot:=tot_firm*type_frac]
  #jobs_model<-jobs_model[type_frac>1e-08]
  
  
  
  moment_mat<-cbind(x[,7]-1/rho/(1-x[,8]) - x[,14]*(W + gamma*x[,1])-(1-rowSums(x[,10:12]))*x[,2:6]%*%cmat1-x[,10]*x[,2:6]%*%cmat2-x[,11]*x[,2:6]%*%cmat3-x[,12]*x[,2:6]%*%cmat4 -c_bar
                    -ct1*x[,10]-ct2*x[,11]-ct3*x[,12],
                    log(x[,8]/x[,9]) +rho*x[,7]-xi-u_bar
                    -ut1*x[,10]-ut2*x[,11]-ut3*x[,12]-x[,2:6]%*%umat)
  
  Z<-cbind(1,x[,2:6],x[,1],x[,2:6]*x[,1], x[,13:14])
  Z<-cbind(Z, Z*x[,10], Z*x[,11],Z*x[,12])
  wagemom<-x[,15]-W*x[,14]*x[,16]
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
  cmat1<-parms[34:38]
  cmat2<-parms[39:43]
  cmat3<-parms[44:48]
  cmat4<-parms[49:53]
  umat<-parms[54:58]
  
  smat<-rbind(parms[59:64],parms[65:70],parms[71:76],parms[77:82])
  
  
  wmat<-list(matrix(w1, nrow=6, ncol=6, byrow=FALSE),
             matrix(w2, nrow=6, ncol=6, byrow=FALSE),
             matrix(w3, nrow=6, ncol=6, byrow=FALSE),
             matrix(w4, nrow=6, ncol=6, byrow=FALSE))
  h<-lapply(1:4, function(x){
    return(diag(smat[x,]))
    
  })
  
  
  ## this function will return gamma given sindex.
  firm_estim<-function(sindex,ap2, ap3, ap4,ap5, ap6,qts,abar){
    aleph<-c(1-ap2-ap3-ap4-ap5-ap6, ap2, ap3, ap4, ap5, ap6)
    findlambda<-function(lambda){
      A<-exp(-lambda*(wmat[[qts]] -h[[qts]]/rho/abar) )
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
      # if end points not of opposite sign, this means either we are hitting a maximum machine readable number issue OR we gamma is 0.
      lambda<-tryCatch({uniroot(findlambda, lower=0, upper=100000, tol=outertol)$root},
                       error = function(e){
                         if (grepl("end points not of opposite sign",e, fixed=TRUE)){
                           return(Inf)
                         } else{
                           tryCatch({uniroot(findlambda, lower=0, upper=10000, tol=outertol)$root},
                                    error = function(e){
                                      if (grepl("end points not of opposite sign",e, fixed=TRUE)){
                                        return(Inf)
                                      } else{
                                        tryCatch({uniroot(findlambda, lower=0, upper=1000, tol=outertol)$root},
                                                 error = function(e){
                                                   if (grepl("end points not of opposite sign",e, fixed=TRUE)){
                                                     return(Inf)
                                                   } else{
                                                     tryCatch({uniroot(findlambda, lower=0, upper=100, tol=outertol)$root},
                                                              error = function(e){
                                                                if (grepl("end points not of opposite sign",e, fixed=TRUE)){
                                                                  return(Inf)
                                                                } else{
                                                                  tryCatch({uniroot(findlambda, lower=0, upper=10, tol=outertol)$root},
                                                                           error = function(e){
                                                                             if (grepl("end points not of opposite sign",e, fixed=TRUE)){
                                                                               return(Inf) 
                                                                             } else{
                                                                               tryCatch({uniroot(findlambda, lower=0, upper=1, tol=outertol, extendInt = 'yes')$root},
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
      
      A<-exp(-lambda*(wmat[[qts]] -h[[qts]]/rho/abar) )
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
      E[which.min(rowSums((wmat[[qts]] -h[[qts]]/rho/abar)*ahelper))[1]]<-1
      a<-E*ahelper
      a<-(round(E,7)>0)*a
      E<-round(E,7)
      lambda<-1/1000000000
    }
    dstar<- sum(a*(-rho*wmat[[qts]] +h[[qts]]))
    return(list(a,dstar,lambda ))
  }
  firm_estim<-Vectorize(firm_estim)
  
  res<-mclapply(1:nrow(x), function(y){firm_estim(x[y,1], x[y,2], x[y,3], x[y,4],x[y,5],x[y,6],
                                                  ifelse(max(x[y,10:12])==1,which.max(x[y,10:12]),4),x[y,14]
  )}, mc.cores=core_count)
  
  # helper functions
  finda<-Vectorize(function(x){res[x][[1]][1]})
  ret_gamma<-Vectorize(function(x){(res[x][[1]][[3]])^(-1) })
  findW<-Vectorize(function(y){sum(wmat[[ifelse(max(x[y,10:12])==1,which.max(x[y,10:12]),4)]]*res[y][[1]][[1]])})
  findqual<-Vectorize(function(y){sum(finda(y)[[1]]*(h[[ifelse(max(x[y,10:12])==1,which.max(x[y,10:12]),4)]])) })
  gamma<-ret_gamma(1:nrow(x))
  xi<-findqual(1:nrow(x))
  W<-findW(1:nrow(x))
  
  cost_qual<-x[,14]*(W + gamma*x[,1])-xi/rho
  wage_qual<-x[,14]*W -xi/rho
  moment_mat<-cbind(x[,7]-1/rho/(1-x[,8]) - x[,14]*(W + gamma*x[,1])-(1-rowSums(x[,10:12]))*x[,2:6]%*%cmat1-x[,10]*x[,2:6]%*%cmat2-x[,11]*x[,2:6]%*%cmat3-x[,12]*x[,2:6]%*%cmat4 -c_bar
                    -ct1*x[,10]-ct2*x[,11]-ct3*x[,12],
                    log(x[,8]/x[,9]) +rho*x[,7]-xi-u_bar
                    -ut1*x[,10]-ut2*x[,11]-ut3*x[,12]-x[,2:6]%*%umat)
  
  
  return(list("gammas"=ret_gamma(1:nrow(x)),
              "wagebills"=findW(1:nrow(x)),
              "quals"=findqual(1:nrow(x)),
              "bmats"=finda(1:nrow(x)),
              "phi"=moment_mat[,1],
              "nu"=moment_mat[,2],
              "cost_qual"=cost_qual,
              "wage_qual"=wage_qual
         ))
}




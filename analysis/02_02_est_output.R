
## make estimate graphs
library('data.table')
library('lessR') # for the to() command
library('SQUAREM')
library('gmm')
library('ggplot2')

theme_set(theme_bw(base_size=22))



colMax <- function(data) apply(data,2, max, na.rm = TRUE)
colMin <- function(data) apply(data,2, min, na.rm = TRUE)
spec_log<-function(x)  ifelse(x==0 | x==-Inf | is.nan(x),0,log(x))
entropy<-function(x)  return(sum(-x*spec_log(x)))

############### 

load('data/02_00_est_res_ny_lessmoments_lesscond.RData')
# sales tax rate
tau<-1.045
parms<-coef(outgmm4)
x<-estim_matrix

#################
rho<-parms[1]*tau
u_bar<-parms[2]
c_bar<-parms[3]
w<-c(0,parms[4],parms[5],parms[6])
ut1<-parms[7]
ut2<-parms[8]
ut3<-parms[9]
ct1<-parms[10]
ct2<-parms[11]
ct3<-parms[12]
s1 <-parms[13]
s2<-parms[14]
s3<-parms[15]
s4<-parms[16]
m1<-0
m2<-0
m3<-0
m4<-0

wmat<-matrix(w, nrow=4, ncol=4, byrow=FALSE)
h<-rbind(c(s1,m2,m3,m4),c(m1,s2,m3,m4), c(m1,m2,s3,m4), c(m1,m2,m3,s4))

## this function will return gamma given sindex.
firm_estim<-function(sindex,ap2, ap3, ap4){
  aleph<-c(1-ap2-ap3-ap4, ap2, ap3, ap4)
  findlambda<-function(lambda){
    A<-exp(-lambda*(wmat -h/rho) )
    A[A>=Inf]<-1e8
    fxpt<-function(p){
      C<-colSums(t(A)*aleph/colSums(A*p))
      return(p*C)
    }
    objval<-function(p){
      C<-colSums(t(A)*aleph/colSums(A*p))
      return(sum(p*spec_log(C), na.rm=TRUE)-max(spec_log(C), na.rm=TRUE))
    }
    E<-rep(1/(4),4)
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
    E<-round(E,9)
    a<-(round(E,9)>0)*a
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
    
    A<-exp(-lambda*(wmat -h/rho) )
    A[A>=Inf]<-1e8
    fxpt<-function(p,y){
      C<-colSums(t(A)*aleph/colSums(A*p))
      return(p*C)
    }
    objval<-function(p,y){
      C<-colSums(t(A)*aleph/colSums(A*p))
      return(sum(p*spec_log(C), na.rm=TRUE)-max(spec_log(C), na.rm=TRUE))
    }
    E<-rep(1/(4),4)
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
    E<-round(E,9)
    a<-(round(E,9)>0)*a
  } else{
    ahelper<-t(matrix(aleph,length(aleph), 4))
    E<-rep(0,4)
    E[which.min(rowSums((wmat -h/rho)*ahelper))[1]]<-1
    a<-E*ahelper
    lambda<-1/1000000000
  }
  dstar<- sum(a*(-rho*wmat +h))
  return(list(a,dstar,lambda ))
}
firm_estim<-Vectorize(firm_estim)

res<-lapply(1:nrow(x), function(y){firm_estim(x[y,1], x[y,2], x[y,3], x[y,4])})

# helper functions
finda<-Vectorize(function(x){res[x][[1]][1]})
ret_gamma<-Vectorize(function(x){(res[x][[1]][[3]])^(-1) })
findW<-Vectorize(function(x){sum(wmat*res[x][[1]][[1]])})
findqual<-Vectorize(function(x){sum(finda(x)[[1]]*(h)) })
gamma<-ret_gamma(1:nrow(x))
xi<-findqual(1:nrow(x))
W<-findW(1:nrow(x))









###############





ggplot(data=data.frame(gamma=gamma[estim_matrix[,1]>0]),aes(x=gamma[estim_matrix[,1]>0])) +
  geom_histogram(color="black", fill="lightblue", size=1, bins = 12)+ ylab("Establishment-Quarter Count") + xlab("Internal Organization Cost Parameter")+ theme(legend.position = "none")
ggsave("out/figures/02_02_gamma_nycounty.png", width=12, heigh=6, units="in")



### moments
g<-function(parms, x){
  
  
  rho<-parms[1]*tau
  u_bar<-parms[2]
  c_bar<-parms[3]
  w<-c(0,parms[4],parms[5],parms[6])
  ut1<-parms[7]
  ut2<-parms[8]
  ut3<-parms[9]
  ct1<-parms[10]
  ct2<-parms[11]
  ct3<-parms[12]
  s1 <-parms[13]
  s2<-parms[14]
  s3<-parms[15]
  s4<-parms[16]
  m1<-0
  m2<-0
  m3<-0
  m4<-0
  
  wmat<-matrix(w, nrow=4, ncol=4, byrow=FALSE)
  h<-rbind(c(s1,m2,m3,m4),c(m1,s2,m3,m4), c(m1,m2,s3,m4), c(m1,m2,m3,s4))
  
  ## this function will return gamma given sindex.
  firm_estim<-function(sindex,ap2, ap3, ap4){
    aleph<-c(1-ap2-ap3-ap4, ap2, ap3, ap4)
    findlambda<-function(lambda){
      A<-exp(-lambda*(wmat -h/rho) )
      A[A>=Inf]<-1e8
      fxpt<-function(p){
        C<-colSums(t(A)*aleph/colSums(A*p))
        return(p*C)
      }
      objval<-function(p){
        C<-colSums(t(A)*aleph/colSums(A*p))
        return(sum(p*spec_log(C), na.rm=TRUE)-max(spec_log(C), na.rm=TRUE))
      }
      E<-rep(1/(4),4)
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
      E<-round(E,9)
      a<-(round(E,9)>0)*a
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
      
      A<-exp(-lambda*(wmat -h/rho) )
      A[A>=Inf]<-1e8
      fxpt<-function(p,y){
        C<-colSums(t(A)*aleph/colSums(A*p))
        return(p*C)
      }
      objval<-function(p,y){
        C<-colSums(t(A)*aleph/colSums(A*p))
        return(sum(p*spec_log(C), na.rm=TRUE)-max(spec_log(C), na.rm=TRUE))
      }
      E<-rep(1/(4),4)
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
      E<-round(E,9)
      a<-(round(E,9)>0)*a
    } else{
      ahelper<-t(matrix(aleph,length(aleph), 4))
      E<-rep(0,4)
      E[which.min(rowSums((wmat -h/rho)*ahelper))[1]]<-1
      a<-E*ahelper
      lambda<-1/1000000000
    }
    dstar<- sum(a*(-rho*wmat +h))
    return(list(a,dstar,lambda ))
  }
  firm_estim<-Vectorize(firm_estim)
  
  res<-lapply(1:nrow(x), function(y){firm_estim(x[y,1], x[y,2], x[y,3], x[y,4])})
  
  # helper functions
  finda<-Vectorize(function(x){res[x][[1]][1]})
  ret_gamma<-Vectorize(function(x){(res[x][[1]][[3]])^(-1) })
  findW<-Vectorize(function(x){sum(wmat*res[x][[1]][[1]])})
  findqual<-Vectorize(function(x){sum(finda(x)[[1]]*(h)) })
  gamma<-ret_gamma(1:nrow(x))
  xi<-findqual(1:nrow(x))
  W<-findW(1:nrow(x))
  
  moment_mat<-cbind(x[,5]-1/rho/(1-x[,6]) - W - gamma*x[,1]-c_bar-ct1*x[,8]-ct2*x[,9]-ct3*x[,10],
                    log(x[,6]/x[,7]) +rho*x[,5]-xi-u_bar
                    -ut1*x[,8]-ut2*x[,9]-ut3*x[,10])
  Z<-cbind(1,x[,2:4],x[,1],x[,2:4]*x[,1], x[,11])
  Z<-cbind(Z, Z*x[,8], Z*x[,9],Z*x[,10])
  return(cbind(moment_mat[,1]*Z,moment_mat[,2]*Z ))
}

gmm_obj<-function(x){
  hold<-colMeans(g(parms=x,x=estim_matrix))
  return(hold)
}



profit<-1/rho*estim_sample$CSPOP*estim_sample$salon_share_subdiv/(1-estim_sample$salon_share_subdiv)
rev<-estim_sample$cust_price*estim_sample$CSPOP*estim_sample$salon_share_subdiv/(1-estim_sample$salon_share_subdiv)
margin<-profit/rev
C<-estim_sample$cust_price-1/rho/(1-estim_sample$salon_share_subdiv)-c_bar-ct1*x[,8]-ct2*x[,9]-ct3*x[,10]
phi<-x[,5]-1/rho/(1-x[,6]) - W - gamma*x[,1]-c_bar-ct1*x[,8]-ct2*x[,9]-ct3*x[,10]
nu<-log(x[,6]/x[,7]) +rho*x[,5]-xi-u_bar-ut1*x[,8]-ut2*x[,9]-ut3*x[,10]

comp_index<-xi+nu - rho*tau*(W+gamma*x[,1]+phi)
org_part<-xi-rho*tau*(W+gamma*x[,1])
other_part<-nu-phi*rho*tau

ggplot() +
  geom_point(aes(x=rank(gamma[estim_matrix[,1]>0])/length(gamma[estim_matrix[,1]>0])*100, y=profit[estim_matrix[,1]>0] ),color="black")+ ylab("Quality-Adjusted Cost Percentile") + xlab("Internal Organization Cost Percentile")+ theme(legend.position = "none")
ggsave("out/figures/02_02_prank_rank_nycounty.png", width=12, heigh=6, units="in")

ggplot() +
  geom_point(aes(x=rank(gamma[estim_matrix[,1]>0])/length(gamma[estim_matrix[,1]>0])*100, y=rank(comp_index[estim_matrix[,1]>0])/length(comp_index[estim_matrix[,1]>0])*100 ),color="black")+ ylab("Quality-Adjusted Cost Percentile") + xlab("Internal Organization Cost Percentile")+ theme(legend.position = "none")
ggsave("out/figures/02_02_prank_rank_nycounty.png", width=12, heigh=6, units="in")

ggplot() +
  geom_point(aes(x=gamma[estim_matrix[,1]>0], y=xi[estim_matrix[,1]>0]/rho-W[estim_matrix[,1]>0]),color="black")+ ylab("Wage-Adjusted Quality ($)") + xlab("Internal Organization Cost Parameter")+ theme(legend.position = "none")
ggsave("out/figures/02_02_quality_absolute_nycounty.png", width=12, heigh=6, units="in")



#findbmax<-function(x){colMax(finda(x)[[1]]/rowSums(finda(x)[[1]]))}
#findbmin<-function(x){colMin(finda(x)[[1]]/rowSums(finda(x)[[1]]))}
#colMean <- function(data) apply(data,2, mean, na.rm = TRUE)
#bmax_model<-colMean(t(sapply(1:121,findbmax )))
#bmax_real<-colMean(estim_sample[,c("max_jobvect_1","max_jobvect_2","max_jobvect_3", "max_jobvect_4")])
#
#bmin_model<-colMean(t(sapply(1:121,findbmin )))
#bmin_real<-colMean(estim_sample[,c("min_jobvect_1","min_jobvect_2","min_jobvect_3", "min_jobvect_4")])


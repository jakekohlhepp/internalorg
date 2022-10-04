## estimate model.
library('data.table')
library('gmm')
set.seed(5777)
colMax <- function(data) apply(data,2, max, na.rm = TRUE)
colMin <- function(data) apply(data,2, min, na.rm = TRUE)
spec_log<-function(x)  ifelse(x==0 | x==-Inf | is.nan(x),0,log(x))
entropy<-function(x)  return(sum(-x*spec_log(x)))


## retrieve estimates
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

## create weight multiplier: divide total share demanded by total share observed.
estim_sample[,weight:= (1-outside_share)/sum(salon_share_subdiv), by=c("COUSUB", "quarter_year")]

## compute the amount of labor. this is population, times share, times 
puzzle<-data.table()
for (estab in 1:nrow(x)){
  piece<-rowSums(finda(estab)[[1]])*estim_sample$salon_share_subdiv[estab]*estim_sample$weight[estab]*estim_sample$COUSUB[estab]
  puzzle<-rbind(puzzle, t(piece))
}
tot_labor<-colSums(puzzle)
## this will be 



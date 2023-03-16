# estimate NY and LA together.
library('parallel')
library('data.table')
library('gmm')
library('SQUAREM')
set.seed(6772)
########### special helper functions #######################################
colMax <- function(data) apply(data,2, max, na.rm = TRUE)
colMin <- function(data) apply(data,2, min, na.rm = TRUE)
spec_log<-function(x)  ifelse(x==0 | x==-Inf | is.nan(x),0,log(x))
entropy<-function(x)  return(sum(-x*spec_log(x)))
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
wage_bound<-Vectorize(function(x){
  return(185/(exp(-x)+1)+15)
})
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

########## load data ######################################################

firm_quarter<-readRDS("data/00_00_firm_quarter.rds")

# limit just to 2019.4, LA for now
estim_sample<-firm_quarter[quarter_year %in% c(2021.1) & county %in% c(36061, 6037)]
## county indicator
estim_sample[, is_la:=county==6037]
table(estim_sample$is_la)
# cannot use salons that do not record price.
estim_sample<-estim_sample[ cust_price>0]
stopifnot(nrow(estim_sample[is.na(s_index)])==0)

# need to convert to matrix to feed into gmm package
estim_matrix<-as.matrix(estim_sample[,c("s_index", "task_mix2", "task_mix3", "task_mix4", "task_mix5","task_mix6",
                                        "cust_price", "salon_share_subdiv","outside_share","avg_labor","avg_wage_qtr", "cust_count",
                                        "is_la")])
############### tolerance preferences #######################################
innertol<-1e-08
outertol<-1e-08
if (get_os()=="windows"){
  core_count<-1
} else{
  core_count<-100
} 
##################################starting point ############################
starting<-readRDS('data/best_joint.rds')
starting[26:45]<-starting[c(1:9, 15:25)]
starting[c(1,26)]<-log(starting[c(1,1)])

stopifnot(length(starting)==45)
################ GMM function #################################################

#for testing
#
#parms<-starting
#x<-estim_matrix


## gmm objective
g<-function(parms, x){
  # 1-25 parms are for NY
  
  
  rho<-c(exp(parms[1])*1.045, exp(parms[26]))
  u_bar<-c(parms[2], parms[27])
  c_bar<-c(parms[3], parms[28]) 
  w1<-rbind(wage_bound(parms[4:9]),
            wage_bound(parms[29:34]))
  cmat1<-parms[10:14] # same material costs.
  umat<-rbind(parms[15:19],
              parms[35:39])
  smat<-rbind(parms[20:25],
              parms[40:45])
  
  #for testing
  #ap2<-estim_matrix[1,2]
  #ap3<-estim_matrix[1,3]
  #ap4<-estim_matrix[1,4]
  #ap5<-estim_matrix[1,5]
  #ap6<-estim_matrix[1,6]
  #sindex<-estim_matrix[1,1]
  #abar<-estim_matrix[1,10]
  #isla<-estim_matrix[1,13]
  
  ## this function will return gamma given sindex.
  firm_estim<-function(sindex,ap2, ap3, ap4,ap5, ap6, abar, isla){
    wmat<-matrix(w1[1+isla,], nrow=6, ncol=6, byrow=FALSE)
    h<-diag(smat[1+isla,])
    aleph<-c(1-ap2-ap3-ap4-ap5-ap6, ap2, ap3, ap4, ap5, ap6)
    findlambda<-function(lambda){
      A<-exp(-lambda*(wmat -h/rho[1+isla]/abar) )
      A[A>=Inf]<-1e8
      fxpt<-function(p){
        C<-colSums(t(A)*aleph/colSums(A*p))
        return(p*C)
      }
      objval<-function(p){
        C<-colSums(t(A)*aleph/colSums(A*p))
        return(-sum(p*spec_log(C), na.rm=TRUE)+max(spec_log(C), na.rm=TRUE))
      }
      E<-rep(1/6,6)
      val<-1
      icount<-0
      while (abs(val)>innertol & icount<1000000){
        E<-fxpt(E)
        val<-objval(E)
        icount<-icount+1
      }
      a<-t(t(A*E)/colSums(A*E)*aleph)
      I<-sum(a*spec_log(t(t(a/rowSums(a))/colSums(a))))
      diff<-I-sindex
      return(as.numeric(diff))
    }
    if (round(sindex/outertol)*outertol>0){
      # if end points not of opposite sign, this means either we are hitting a maximum machine readable number issue OR we gamma is 0.
      lambda<-tryCatch({bisection(findlambda, a=0, b=100000, ftol=outertol,xtol=outertol,n=100000)$root},
                       error = function(e){
                         if (grepl("end points not of opposite sign",e, fixed=TRUE)){
                           return(Inf)
                         } else{
                           tryCatch({bisection(findlambda, a=0, b=10000, ftol=outertol,xtol=outertol,n=100000)$root},
                                    error = function(e){
                                      if (grepl("end points not of opposite sign",e, fixed=TRUE)){
                                        return(Inf)
                                      } else{
                                        tryCatch({bisection(findlambda, a=0, b=1000, ftol=outertol,xtol=outertol,n=100000)$root},
                                                 error = function(e){
                                                   if (grepl("end points not of opposite sign",e, fixed=TRUE)){
                                                     return(Inf)
                                                   } else{
                                                     tryCatch({bisection(findlambda, a=0, b=100, ftol=outertol,xtol=outertol,n=100000)$root},
                                                              error = function(e){
                                                                if (grepl("end points not of opposite sign",e, fixed=TRUE)){
                                                                  return(Inf)
                                                                } else{
                                                                  tryCatch({bisection(findlambda, a=0, b=10, ftol=outertol,xtol=outertol,n=100000)$root},
                                                                           error = function(e){
                                                                             if (grepl("end points not of opposite sign",e, fixed=TRUE)){
                                                                               return(Inf) 
                                                                             } else{
                                                                               tryCatch({bisection(findlambda, a=0, b=1, ftol=outertol,xtol=outertol,n=100000, extendInt = 'yes')$root},
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
      
      if (lambda<Inf){
        A<-exp(-lambda*(wmat -h/rho[1+isla]/abar) )
        A[A>=Inf]<-1e8
        fxpt<-function(p){
          C<-colSums(t(A)*aleph/colSums(A*p))
          return(p*C)
        }
        objval<-function(p){
          C<-colSums(t(A)*aleph/colSums(A*p))
          return(-sum(p*spec_log(C), na.rm=TRUE)+max(spec_log(C), na.rm=TRUE))
        }
        E<-rep(1/6,6)
        val<-1
        icount<-0
        while (abs(val)>innertol & icount<1000000){
          E<-fxpt(E)
          val<-objval(E)
          icount<-icount+1
        }
        a<-t(t(A*E)/colSums(A*E)*aleph)
      } else{
        a<-matrix(0, ncol=6, nrow=6)
        for (col in 1:6){
          a[which.min((wmat -h/rho[1+isla]/abar)[,col]),col]<-aleph[col]
        }
        E<-rowSums(a)
      }
    } else{
      ahelper<-t(matrix(aleph,length(aleph), 6))
      E<-rep(0,6)
      E[which.min(rowSums((wmat -h/rho[1+isla]/abar)*ahelper))[1]]<-1
      a<-E*ahelper
      #a<-(round(E,12)>0)*a
      #E<-round(E,12)
      lambda<-1/1000000000
    }
    dstar<- sum(a*(-rho[1+isla]*wmat +h))
    return(list(a,dstar,lambda ))
  }
  firm_estim<-Vectorize(firm_estim)
  
  res<-mclapply(1:nrow(x), function(y){firm_estim(x[y,1], x[y,2], x[y,3], x[y,4],x[y,5],x[y,6],x[y,10],x[y,13]
  )}, mc.cores=core_count)
  
  # helper functions
  finda<-Vectorize(function(x){res[x][[1]][1]})
  ret_gamma<-Vectorize(function(x){(res[x][[1]][[3]])^(-1) })
  findW<-Vectorize(function(y){
    wmat<-matrix(w1[1+estim_matrix[y,13],], nrow=6, ncol=6, byrow=FALSE)
    return(sum(wmat*res[y][[1]][[1]]))
  })
  findqual<-Vectorize(function(y){
    h<-diag(smat[1+estim_matrix[y,13],])
    return(sum(finda(y)[[1]]*(h)))
  })
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
  penalty<-ifelse(any(colSums(puzzle[,-c("V7")]>0.01)==0), 1000000,1)
  
  
  moment_mat<-cbind(x[,7]-1/rho[1+x[,13]]/(1-x[,8]) - x[,10]*(W + gamma*x[,1])-x[,2:6]%*%cmat1-c_bar[1+x[,13]],
                    log(x[,8]/x[,9]) +rho[1+x[,13]]*x[,7]-xi-u_bar[1+x[,13]]-rowSums(x[,2:6]*umat[1+x[,13],]) )
  
  Z<-cbind(1,x[,2:6],x[,1],x[,2:6]*x[,1],x[,10])
  Z<-cbind(moment_mat[,1]*Z,moment_mat[,2]*Z)
  wagemom<-x[,11]-W*x[,10]*x[,12]
  return(penalty*cbind(Z*(1-x[,13]),wagemom*(1-x[,13]), Z*x[,13], wagemom*x[,13])  )
}


res_store<-gmm(g, x=estim_matrix, t0=starting, prewhite=0,optfct="optim",method="BFGS",centeredVcov='TRUE',wmatrix='ident',
               control=list(abstol=1e-08,reltol=1e-08, trace=1, maxit=2000))
save(res_store, innertol, outertol, wage_bound,estim_sample, estim_matrix, file="data/01_07_progress.RData")


for (i in 1:40){
  
  
  res_store<-gmm(g, x=estim_matrix, t0=coef(res_store), prewhite=0,optfct="optim",centeredVcov='TRUE',wmatrix='ident',
                 control=list(abstol=1e-08,reltol=1e-08, trace=1, maxit=2000))
  save(res_store, innertol, outertol, wage_bound,estim_sample, estim_matrix, file="data/01_07_progress.RData")
  

  
  
}

save(res_store, innertol, outertol,tau, wage_bound,estim_sample, estim_matrix, file="data/01_07_final_joint.RData")











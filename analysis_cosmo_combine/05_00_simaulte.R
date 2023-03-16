# Purpose: Simulate the model
library('data.table')
library('lamW')
library('gmm')
library('matrixStats') # logsumexp function avoids overflow issue when computing welfare.
library('spatstat')
library('nloptr')
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
if (get_os()=="windows"){
  core_count<-1
} else{
  core_count<-42
} 

source('small_gmm_objective_less.R')
innertol<-1e-08
outertol<-1e-08

### create simulated world.



rho<-1
u_bar<-0
c_bar<-0
smat<-rbind(c(15,19,15+11),
            c(15+8,19,15),
            c(15,19-4,15))
cmat<-rep(0,2)
umat<-rep(0,2)
oldwages<-c(6,5,0)+15
tau<-1

## 2 firms, one high org cost one low. same task mix.
firm_parms<-data.table(gamma=c(4,8),
                         avg_labor=c(1,1),
                         alpha1=c(1/3, 1/3),
                         alpha2=c(1/3, 1/3),
                         alpha3=c(1/3, 1/3),
                        nu=c(0,0),
                       phi=c(0,0),
                       weight=c(1,1),
                       CSPOP=100000)

#########################


### solve for equilibrium with old wages fixed.
produce_outcomes<-function(wages){
  w1<-wages
  wmat<-matrix(w1, nrow=3, ncol=3, byrow=FALSE)
  h<-smat
  
  ## this function will return matrix given gamma
  firm_estim<-function(gamma, aleph, abar){
    if (gamma<1000000000 & gamma>0){
      lambda<-1/gamma
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
      E<-rep(1/3,3)
      E<-squarem(E,fixptfn = fxpt, control=list(maxiter=100000,tol=innertol) )$par
      a<-t(t(A*E)/colSums(A*E)*aleph)
    } else{
      if (gamma==0){
        # this is when the firm is frictionless, in which case they use water filling method.
        a<-matrix(0, ncol=3, nrow=3)
        for (col in 1:3){
          a[which.min((wmat -h/rho/abar)[,col]),col]<-aleph[col]
        }
        E<-rowSums(a)
        lambda<-Inf
      } else{
        # this is for firms with unidentified high gamma.
        # assume their gamma is still high enough s.t. they choose least complex structure.
        ahelper<-t(matrix(aleph,length(aleph), 3))
        E<-rep(0,3)
        E[which.min(rowSums((wmat -h/rho/abar)*ahelper))[1]]<-1
        a<-E*ahelper
        lambda<-0
      }
    }
    dstar<- sum(a*(-rho*wmat +h))
    return(list(a,dstar,lambda ))
  }
  firm_estim<-Vectorize(firm_estim)
  
  res<-mclapply(1:nrow(firm_parms), function(y){firm_estim(firm_parms[y, "gamma"], 
                                                           firm_parms[y, c("alpha1", "alpha2", "alpha3")],
                                                           firm_parms[y, "avg_labor"])}, mc.cores=core_count)
  
  # helper functions
  finda<-Vectorize(function(x){res[x][[1]][1]})
  findW<-Vectorize(function(y){sum(wmat*res[y][[1]][[1]])})
  findqual<-Vectorize(function(y){sum(finda(y)[[1]]*(h)) })
  findI<-Vectorize(function(y){
    a<-res[[y]][[1]]
    return(sum(a*spec_log(t(t(a/rowSums(a))/colSums(a)))))
  })
  
  
  ### new to store matrix of all objects.
  holder<-copy(firm_parms)
  holder[,W:=findW(1:nrow(holder))]
  holder[,xi:=findqual(1:nrow(holder))]
  holder[,I:=findI(1:nrow(holder))]
  holder[, Q:=xi+u_bar+alpha2*umat[1]+alpha3*umat[2]+nu]
  holder[, C:=avg_labor*(W + gamma*I) +alpha2*cmat[1]+alpha3*cmat[2] +c_bar+phi]
  
  ### given optimal orgs, solve for price
  
  best_respond<-function(p0, Q,C, wgt){
    old_p<-p0
    for (i in 1:10000000){
      new_p<-sapply(1:length(old_p), 
                    function(y){ 
                      1/rho+C[y]+lambertW0(exp(-1+Q[y]-rho*C[y])/(1+sum(wgt*exp(Q-rho*old_p))-exp(Q[y]-rho*old_p[y])) )/rho 
                    })
      if (all(abs(new_p-old_p)<1e-10)) break
      old_p<-new_p
    }
    return(new_p)
  }
  holder[, newprice:=best_respond(C,Q=Q, C=C, wgt=weight)]
  
  ### compute new shares. recall that outside good quality normalized to 0
  
  holder[, new_share:=exp(Q-rho*newprice)]
  holder[,new_share:=new_share/(sum(weight*new_share)+1)]
  
  puzzle<-data.table()
  ## with shares, recall demand will be share times total addressable market which is county subdiv pop.
  bmats<-finda(1:nrow(holder))
  for (estab in 1:nrow(holder)){
    piece<-rowSums(bmats[[estab]])*holder$avg_labor[estab]*holder$new_share[estab]*holder$weight[estab]*holder$CSPOP[estab]
    piece<-c( piece)
    puzzle<-rbind(puzzle, t(piece))
  }
  check_labor<-puzzle[, .(L1=sum(V1), L2=sum(V2), L3=sum(V3)) ]
  stopifnot(nrow(check_labor)==1)
  
  return(list("mainstats"=holder,
              "bmats"=bmats,
              labor_amounts=check_labor))
  
}

before<-produce_outcomes(oldwages)

labor_supply<-as.numeric(before$labor_amounts)
solve_wages<-function(wages){
  w1<-wages
  wmat<-matrix(w1, nrow=3, ncol=3, byrow=FALSE)
  h<-smat
  
  ## this function will return matrix given gamma
  firm_estim<-function(gamma, aleph, abar){
    if (gamma<1000000000 & gamma>0){
      lambda<-1/gamma
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
      E<-rep(1/3,3)
      E<-squarem(E,fixptfn = fxpt, control=list(maxiter=100000,tol=innertol) )$par
      a<-t(t(A*E)/colSums(A*E)*aleph)
    } else{
      if (gamma==0){
        # this is when the firm is frictionless, in which case they use water filling method.
        a<-matrix(0, ncol=3, nrow=3)
        for (col in 1:3){
          a[which.min((wmat -h/rho/abar)[,col]),col]<-aleph[col]
        }
        E<-rowSums(a)
        lambda<-Inf
      } else{
        # this is for firms with unidentified high gamma.
        # assume their gamma is still high enough s.t. they choose least complex structure.
        ahelper<-t(matrix(aleph,length(aleph), 3))
        E<-rep(0,3)
        E[which.min(rowSums((wmat -h/rho/abar)*ahelper))[1]]<-1
        a<-E*ahelper
        lambda<-0
      }
    }
    dstar<- sum(a*(-rho*wmat +h))
    return(list(a,dstar,lambda ))
  }
  firm_estim<-Vectorize(firm_estim)
  
  res<-mclapply(1:nrow(firm_parms), function(y){firm_estim(firm_parms[y, "gamma"], 
                                                           firm_parms[y, c("alpha1", "alpha2", "alpha3")],
                                                           firm_parms[y, "avg_labor"])}, mc.cores=core_count)
  
  # helper functions
  finda<-Vectorize(function(x){res[x][[1]][1]})
  findW<-Vectorize(function(y){sum(wmat*res[y][[1]][[1]])})
  findqual<-Vectorize(function(y){sum(finda(y)[[1]]*(h)) })
  findI<-Vectorize(function(y){
    a<-res[[y]][[1]]
    return(sum(a*spec_log(t(t(a/rowSums(a))/colSums(a)))))
  })
  
  
  ### new to store matrix of all objects.
  holder<-copy(firm_parms)
  holder[,W:=findW(1:nrow(holder))]
  holder[,xi:=findqual(1:nrow(holder))]
  holder[,I:=findI(1:nrow(holder))]
  holder[, Q:=xi+u_bar+alpha2*umat[1]+alpha3*umat[2]+nu]
  holder[, C:=avg_labor*(W + gamma*I) +alpha2*cmat[1]+alpha3*cmat[2] +c_bar+phi]
  
  ### given optimal orgs, solve for price
  
  best_respond<-function(p0, Q,C, wgt){
    old_p<-p0
    for (i in 1:10000000){
      new_p<-sapply(1:length(old_p), 
                    function(y){ 
                      1/rho+C[y]+lambertW0(exp(-1+Q[y]-rho*C[y])/(1+sum(wgt*exp(Q-rho*old_p))-exp(Q[y]-rho*old_p[y])) )/rho 
                    })
      if (all(abs(new_p-old_p)<1e-10)) break
      old_p<-new_p
    }
    return(new_p)
  }
  holder[, newprice:=best_respond(C,Q=Q, C=C, wgt=weight)]
  
  ### compute new shares. recall that outside good quality normalized to 0
  
  holder[, new_share:=exp(Q-rho*newprice)]
  holder[,new_share:=new_share/(sum(weight*new_share)+1)]
  
  puzzle<-data.table()
  ## with shares, recall demand will be share times total addressable market which is county subdiv pop.
  bmats<-finda(1:nrow(holder))
  for (estab in 1:nrow(holder)){
    piece<-rowSums(bmats[[estab]])*holder$avg_labor[estab]*holder$new_share[estab]*holder$weight[estab]*holder$CSPOP[estab]
    piece<-c( piece)
    puzzle<-rbind(puzzle, t(piece))
  }
  check_labor<-puzzle[, .(L1=sum(V1), L2=sum(V2), L3=sum(V3)) ]
  stopifnot(nrow(check_labor)==1)
  
  return(sum(as.numeric(labor_supply-check_labor)^2))
  
  
}
new_wages<-oldwages+runif(3, -4,4)
## test.
quad_wages<-optim(as.numeric(new_wages),solve_wages,method="L-BFGS-B", lower=rep(0,3), upper=rep(Inf, 3), control=list(trace=1))
counter=1
while (quad_wages$value>0.01 & counter<=10){
  quad_wages<-optim(quad_wages$par,solve_wages, control=list(trace=1))
  if (quad_wages$value>0.01) quad_wages<-optim(quad_wages$par,solve_wages,method="L-BFGS-B", lower=rep(0,3), upper=rep(Inf, 3), control=list(trace=1))
  counter<-counter+1
}

# do minimum wage to 16. allow for non-binding 
allcombos<-expand.grid(c(TRUE, FALSE),c(TRUE, FALSE),c(TRUE, FALSE) )
stopifnot(nrow(allcombos)==8)

## compute average time spent on specialty.
puzzle<-data.table()
for (estab in 1:nrow(before$mainstats)){
  piece<-sum(diag(before$bmats[[estab]]))*before$mainstats$avg_labor[estab]*before$mainstats$new_share[estab]*before$mainstats$weight[estab]*before$mainstats$CSPOP[estab]
  piece<-c( piece)
  puzzle<-rbind(puzzle, t(piece))
}

minwage_res<-data.table(context="Initial",mean_wage=as.numeric(weighted.mean(oldwages,labor_supply )),
                        sd_wage=as.numeric(weighted.var(oldwages,labor_supply ))^(1/2),
                        task_spec=sum(puzzle$V1)/sum(labor_supply),
                        avg_complexity=mean(before$mainstats$I),
                        sd_complexity=sd(before$mainstats$I),
                        markup=mean(1/(1-before$mainstats$new_share)/rho/tau),
                        pct_markup=mean(1/(1-before$mainstats$new_share)/rho/tau/before$mainstats$C),
                        total_profit=sum(before$mainstats$weight[1]*before$mainstats$CSPOP[1]*before$mainstats$new_share/(1-before$mainstats$new_share)/rho/tau),
                        avg_profit=mean(before$mainstats$CSPOP[1]*before$mainstats$new_share/(1-before$mainstats$new_share)/rho/tau),
                        sd_profit=sd(before$mainstats$CSPOP[1]*before$mainstats$new_share/(1-before$mainstats$new_share)/rho/tau),
                        consumer_welfare=before$mainstats$CSPOP[1]/rho*(logSumExp(c(before$mainstats$Q-tau*rho*before$mainstats$newprice,-log(before$mainstats$weight[1])))+log(before$mainstats$weight[1])-digamma(1)),
                        quality=mean(before$mainstats$Q),
                        w1=oldwages[1],
                        w2=oldwages[2],
                        w3=oldwages[3],
                        unemployed=0,
                        obj_value=NA,
                        check_binding=NA
)

minwage_objects<-list("initial"=before)


for (combo in 1:nrow(allcombos)){
  #for (combo in 31){
  
  bindwages<-which(as.logical(allcombos[combo,]))
  
  if (length(bindwages)==0) next
  if (length(bindwages)<3){
    quad_solver<-function(w){
      holdwage<-rep(20,3)
      holdwage[-bindwages]<-w
      main_res<-produce_outcomes(wages=holdwage)$labor_amounts
      main_res<-(as.numeric(main_res)-labor_supply)[-bindwages]
      return(sum(main_res^2))
    }
    if (length(bindwages)==2){
      method="Brent"
    } else{
      method="L-BFGS-B"
    }
    quad_wages<-optim(oldwages[-bindwages],quad_solver,method=method, lower=rep(0,3-length(bindwages)), upper=rep(10000,3-length(bindwages)), control=list(trace=1))
    counter=1
    while (quad_wages$value>0.01 & counter<=5){
      if (length(bindwages)<2) quad_wages<-optim(quad_wages$par,quad_solver, control=list(trace=1))
      if (quad_wages$value>0.01) quad_wages<-optim(quad_wages$par,quad_solver,method=method, lower=rep(0,3-length(bindwages)), upper=rep(10000,3-length(bindwages)), control=list(trace=1))
      counter<-counter+1
    }
    tempwage<-rep(20,3)
    tempwage[-bindwages]<-quad_wages$par
  } else{
    # for the case where all binding, no need to solve. just put quad_wages as 20
    tempwage<-rep(20,3)
    quad_wages<-data.frame(value=NA, par=1000)
  }
  ## check bindingness
  
  out<-produce_outcomes(tempwage)
  puzzle<-data.table()
  for (estab in 1:nrow(before$mainstats)){
    piece<-sum(diag(out$bmats[[estab]]))*out$mainstats$avg_labor[estab]*out$mainstats$new_share[estab]*out$mainstats$weight[estab]*out$mainstats$CSPOP[estab]
    puzzle<-rbind(puzzle, t(piece))
  }
  
  helper<-as.numeric(labor_supply)-as.numeric(out$labor_amounts)
  unemployed<-sum(helper[helper>0])
  minls<-pmax(as.numeric(out$labor_amounts), 0)
  
  
  ## binding minimum wage means we should have excess supply (positive values)
  #print(all(excess[bindwages]>0))
  ## should also have all wages be above minimum
  #print(all(quad_wages$par>20))
  
  minwage_res<-rbind(minwage_res,data.table(context=paste("Minimum Wage", paste0(bindwages, collapse="-")),mean_wage=as.numeric(weighted.mean(tempwage,minls )),
                                                        sd_wage=as.numeric(weighted.var(tempwage,minls ))^(1/2),
                                                        task_spec=sum(puzzle$V1)/sum(minls),
                                                        avg_complexity=mean(out$mainstats$I),
                                                        sd_complexity=sd(out$mainstats$I),
                                                        markup=mean(1/(1-out$mainstats$new_share)/rho/tau),
                                                        pct_markup=mean(1/(1-out$mainstats$new_share)/rho/tau/out$mainstats$C),
                                                        total_profit=sum(out$mainstats$weight[1]*out$mainstats$CSPOP[1]*out$mainstats$new_share/(1-out$mainstats$new_share)/rho/tau),
                                                        avg_profit=mean(out$mainstats$CSPOP[1]*out$mainstats$new_share/(1-out$mainstats$new_share)/rho/tau),
                                                        sd_profit=sd(out$mainstats$CSPOP[1]*out$mainstats$new_share/(1-out$mainstats$new_share)/rho/tau),
                                                        consumer_welfare=out$mainstats$CSPOP[1]/rho*(logSumExp(c(out$mainstats$Q-tau*rho*out$mainstats$newprice,-log(out$mainstats$weight[1])))+log(out$mainstats$weight[1])-digamma(1)),
                                                        quality=mean(out$mainstats$Q),
                                                        w1=tempwage[1],
                                                        w2=tempwage[2],
                                                        w3=tempwage[3],
                                                        unemployed=unemployed,
                                                        obj_value=quad_wages$value,
                                                        check_binding=all(c(helper[bindwages]>0,quad_wages$par>20))
  ))
  
  names<-paste0(bindwages, collapse="-")
  print(names)
  minwage_objects[[names]]<-out
  
}


#save(minwage_noadj_res,minwage_noadj_objects, file="data/03_00_counterfactual_minwage_noadj.RData")


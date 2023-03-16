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

############### import results
load('data/01_01b_progress.RData')
###### create objects
innertol<-1e-08
outertol<-1e-08

if (get_os()=="windows"){
  core_count<-1
} else{
  core_count<-84
} 

source('more_gmm_objective.R')

load('data/03_00_counterfactual.RData')
w_guesses<-as.matrix(counter_res[, c("w1", "w2", "w3", "w4", "w5", "w6")])
rm(counter_res, counter_objects)

###temp chunk
ny_objects<-ret_objects(coef(res_store), estim_matrix)
###############


#### Prep Data
# weight is just scaled up share.
estim_sample[, weight:=(1-outside_share)/(sum(salon_share_subdiv)), by=c("county","quarter_year")]

## Compute amount of each type.
# this is row sum of b, times total duration times weight. divide by 60 to get in hours.
puzzle<-data.table()

for (estab in 1:nrow(estim_sample)){
  piece<-rowSums(ny_objects$bmats[[estab]])*estim_sample$salon_share_subdiv[estab]*estim_sample$avg_labor[estab]*estim_sample$weight[estab]*estim_sample$CSPOP[estab]
  piece<-c( piece,estim_sample[estab,]$quarter_year)
  puzzle<-rbind(puzzle, t(piece))
}
tot_labor<-puzzle[, .(L1=sum(V1), L2=sum(V2), L3=sum(V3), L4=sum(V4), L5=sum(V5), L6=sum(V6)) , by=V7 ]
setnames(tot_labor, "V7", "quarter_year")
labor_supply<-as.numeric(tot_labor[1,-1])
wages<-wage_bound(coef(res_store)[4:9])




####### initial stats

## compute average time spent on specialty.
puzzle<-data.table()
for (estab in 1:nrow(estim_sample)){
  piece<-sum(diag(ny_objects$bmats[[estab]]))*estim_sample$avg_labor[estab]*estim_sample$salon_share_subdiv[estab]*estim_sample$weight[estab]*estim_sample$CSPOP[estab]
  piece<-c( piece,estim_sample[estab,]$quarter_year)
  puzzle<-rbind(puzzle, t(piece))
}





### store results from counterfactuals
counter_res<-data.table(context="Initial",mean_wage=as.numeric(weighted.mean(wages,labor_supply )),
                        sd_wage=as.numeric(weighted.var(wages,labor_supply ))^(1/2),
                        task_spec=sum(puzzle$V1)/sum(labor_supply),
                        avg_complexity=mean(estim_sample$s_index),
                        sd_complexity=sd(estim_sample$s_index),
                        markup=mean(1/(1-estim_sample$salon_share_subdiv)/coef(res_store)[1]/tau),
                        pct_markup=mean(1/(1-estim_sample$salon_share_subdiv)/coef(res_store)[1]/tau/ny_objects$C),
                        total_profit=sum(estim_sample$CSPOP[1]*estim_sample$salon_share_subdiv/(1-estim_sample$salon_share_subdiv)/coef(res_store)[1]/tau),
                        avg_profit=mean(estim_sample$CSPOP[1]*estim_sample$salon_share_subdiv/(1-estim_sample$salon_share_subdiv)/coef(res_store)[1]/tau),
                        sd_profit=sd(estim_sample$CSPOP[1]*estim_sample$salon_share_subdiv/(1-estim_sample$salon_share_subdiv)/coef(res_store)[1]/tau),
                        consumer_welfare=estim_sample$CSPOP[1]/coef(res_store)[1]*(logSumExp(c(ny_objects$Q-tau*coef(res_store)[1]*estim_sample$cust_price,0))-digamma(1)),
                        quality=mean(ny_objects$Q),
                        w1=wages[1],
                        w2=wages[2],
                        w3=wages[3],
                        w4=wages[4],
                        w5=wages[5],
                        w6=wages[6],
                        obj_value=NA
                        )

counter_objects<-list("initial"=ny_objects)


#################



### create function which solves game.
### now big  - BE CAREFUL - THIS FUNCTION USES BOTH ESTIM_MATRIX AND ESTIM_SAMPLE
solve_game<-function(parms,gamma_vec,wages, x){
  rho<-parms[1]*tau
  u_bar<-parms[2]
  c_bar<-parms[3]
  w1<-wages
  cmat1<-parms[10:14]
  umat<-parms[15:19]
  smat<-parms[20:25]
  
  
  wmat<-matrix(w1, nrow=6, ncol=6, byrow=FALSE)
  h<-diag(smat)
  
  ## this function will return matrix given gamma
  firm_estim<-function(gamma,ap2, ap3, ap4,ap5, ap6, abar){
    aleph<-c(1-ap2-ap3-ap4-ap5-ap6, ap2, ap3, ap4, ap5, ap6)
    if (gamma<1000000000 & !is.na(gamma)){
      lambda<-1/gamma
      A<-exp(-lambda*(wmat -h/rho/abar) )
      A[A>=Inf]<-1e8
      fxpt<-function(p,y){
        C<-colSums(t(A)*aleph/colSums(A*p))
        return(p*C)
      }
      objval<-function(p,y){
        C<-colSums(t(A)*aleph/colSums(A*p))
        return(sum(p*spec_log(C), na.rm=TRUE)-max(spec_log(C), na.rm=TRUE))
      }
      E<-rep(1/6,6)
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
    } else{# this is for firms with unidentified high gamma.
      # assume their gamma is still high enough s.t. they choose least complex structure.
      ahelper<-t(matrix(aleph,length(aleph), 6))
      E<-rep(0,6)
      E[which.min(rowSums((wmat -h/rho/abar)*ahelper))[1]]<-1
      a<-E*ahelper
      lambda<-0
    }
    dstar<- sum(a*(-rho*wmat +h))
    return(list(a,dstar,lambda ))
  }
  firm_estim<-Vectorize(firm_estim)
  
  res<-mclapply(1:nrow(x), function(y){firm_estim(gamma_vec[y], x[y,2], x[y,3], x[y,4],x[y,5],x[y,6],x[y,11]
  )}, mc.cores=core_count)
  
  # helper functions
  finda<-Vectorize(function(x){res[x][[1]][1]})
  findW<-Vectorize(function(y){sum(wmat*res[y][[1]][[1]])})
  findqual<-Vectorize(function(y){sum(finda(y)[[1]]*(h)) })
  findI<-Vectorize(function(y){
    a<-res[[y]][[1]]
    return(sum(a*spec_log(t(t(a/rowSums(a))/colSums(a)))))
  })
  
  
  ### new to store matrix of all objects.
  holder<-copy(estim_sample)
  holder[,W:=findW(1:nrow(x))]
  holder[,xi:=findqual(1:nrow(x))]
  holder[,Imodel:=findI(1:nrow(x))]
  holder[, Q:=xi+u_bar+x[,2:6]%*%umat+ny_objects$nu]
  holder[, C:=x[,11]*(W + gamma_vec*Imodel) + x[,2:6]%*%cmat1 +c_bar+ny_objects$phi]
  
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
  holder[, newprice:=best_respond(cust_price,Q=Q, C=C, wgt=weight), by=quarter_year]
  
  ### compute new shares. recall that outside good quality normalized to 0
  
  holder[, new_share:=exp(Q-rho*newprice)]
  holder[,new_share:=new_share/(sum(weight*new_share)+1), by=quarter_year]
  
  puzzle<-data.table()
  ## with shares, recall demand will be share times total addressable market which is county subdiv pop.
  bmats<-finda(1:nrow(holder))
  for (estab in 1:nrow(holder)){
    piece<-rowSums(bmats[[estab]])*holder$avg_labor[estab]*holder$new_share[estab]*holder$weight[estab]*holder$CSPOP[estab]
    piece<-c( piece,estim_sample[estab,]$quarter_year)
    puzzle<-rbind(puzzle, t(piece))
  }
  check_labor<-puzzle[, .(L1=sum(V1), L2=sum(V2), L3=sum(V3), L4=sum(V4), L5=sum(V5), l6=sum(V6)) , by=V7 ]
  stopifnot(nrow(check_labor)==1)
  
  return(as.numeric(labor_supply-check_labor[,-1])) # negative means excess demand, positive means excess supply.
  # so negative means we need higher wages.
}
solve_helper<-function(x,t){
  temp<-wage_new
  temp[t]<-x
  output<-solve_game(parms=coef(res_store), gamma_vec=gamma_vec,wages=temp, x=estim_matrix)
  return(as.numeric(output[t])^2)
}
return_excess<-function(w){
  return(solve_game(parms=coef(res_store), gamma_vec=gamma_vec,wages=w, x=estim_matrix))
}
quad_solver<-function(w){
  return(sum(solve_game(parms=coef(res_store), gamma_vec=gamma_vec,wages=w, x=estim_matrix)^2))
}


###############

gamma_vec<-ny_objects$gammas
tau<-1

## counterfactual 0: sales tax change but shut down any internal changes.
solve_no_int<-function(parms,gamma_vec,wages, x){
  rho<-parms[1]*tau
  u_bar<-parms[2]
  c_bar<-parms[3]
  w1<-wages
  cmat1<-parms[10:14]
  umat<-parms[15:19]
  smat<-parms[20:25]
  
  
  wmat<-matrix(w1, nrow=6, ncol=6, byrow=FALSE)
  h<-diag(smat)
  
  ## this function will return matrix given gamma
  res<-ny_objects$bmats
  
  # helper functions
  finda<-Vectorize(function(x){res[x]})
  findW<-Vectorize(function(y){sum(wmat*res[y][[1]])})
  findqual<-Vectorize(function(y){sum(res[y][[1]]*(h)) })
  findI<-Vectorize(function(y){
    a<-res[y][[1]]
    return(sum(a*spec_log(t(t(a/rowSums(a))/colSums(a)))))
  })
  
  
  ### new to store matrix of all objects.
  holder<-copy(estim_sample)
  holder[,W:=findW(1:nrow(x))]
  holder[,xi:=findqual(1:nrow(x))]
  holder[,Imodel:=findI(1:nrow(x))]
  holder[, Q:=xi+u_bar+x[,2:6]%*%umat+ny_objects$nu]
  holder[, C:=x[,11]*(W + gamma_vec*Imodel) + x[,2:6]%*%cmat1 +c_bar+ny_objects$phi]
  
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
  holder[, newprice:=best_respond(cust_price,Q=Q, C=C, wgt=weight), by=quarter_year]
  
  ### compute new shares. recall that outside good quality normalized to 0
  
  holder[, new_share:=exp(Q-rho*newprice)]
  holder[,new_share:=new_share/(sum(weight*new_share)+1), by=quarter_year]
  
  puzzle<-data.table()
  ## with shares, recall demand will be share times total addressable market which is county subdiv pop.
  bmats<-finda(1:nrow(holder))
  for (estab in 1:nrow(holder)){
    piece<-rowSums(bmats[[estab]])*holder$avg_labor[estab]*holder$new_share[estab]*holder$weight[estab]*holder$CSPOP[estab]
    piece<-c( piece,estim_sample[estab,]$quarter_year)
    puzzle<-rbind(puzzle, t(piece))
  }
  check_labor<-puzzle[, .(L1=sum(V1), L2=sum(V2), L3=sum(V3), L4=sum(V4), L5=sum(V5), l6=sum(V6)) , by=V7 ]
  stopifnot(nrow(check_labor)==1)
  
  return(as.numeric(labor_supply-check_labor[,-1])) # negative means excess demand, positive means excess supply.
  # so negative means we need higher wages.
}
quad_solver_no_int<-function(w){
  return(sum(solve_no_int(parms=coef(res_store), gamma_vec=gamma_vec,wages=w, x=estim_matrix)^2))
}
quad_wages<-optim(as.numeric(w_guesses[2,]),quad_solver_no_int,method="L-BFGS-B", lower=rep(0,6), upper=rep(Inf, 6), control=list(trace=1))
counter=1
while (quad_wages$value>0.01 & counter<=3){
  quad_wages<-optim(quad_wages$par,quad_solver_no_int, control=list(trace=1))
  if (quad_wages$value>0.01) quad_wages<-optim(quad_wages$par,quad_solver_no_int,method="L-BFGS-B", lower=rep(0,6), upper=rep(Inf, 6), control=list(trace=1))
  counter<-counter+1
}


counterfactual_no_int<-function(parms,gamma_vec,wages, x){
  rho<-parms[1]*tau
  u_bar<-parms[2]
  c_bar<-parms[3]
  w1<-wages
  cmat1<-parms[10:14]
  umat<-parms[15:19]
  smat<-parms[20:25]
  
  
  wmat<-matrix(w1, nrow=6, ncol=6, byrow=FALSE)
  h<-diag(smat)
  
  ## this function will return matrix given gamma
  res<-ny_objects$bmats
  
  # helper functions
  finda<-Vectorize(function(x){res[x]})
  findW<-Vectorize(function(y){sum(wmat*res[y][[1]])})
  findqual<-Vectorize(function(y){sum(res[y][[1]]*(h)) })
  findI<-Vectorize(function(y){
    a<-res[y][[1]]
    return(sum(a*spec_log(t(t(a/rowSums(a))/colSums(a)))))
  })
  
  
  ### new to store matrix of all objects.
  holder<-copy(estim_sample)
  holder[,W:=findW(1:nrow(x))]
  holder[,xi:=findqual(1:nrow(x))]
  holder[,Imodel:=findI(1:nrow(x))]
  holder[, Q:=xi+u_bar+x[,2:6]%*%umat+ny_objects$nu]
  holder[, C:=x[,11]*(W + gamma_vec*Imodel) + x[,2:6]%*%cmat1 +c_bar+ny_objects$phi]
  
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
  holder[, newprice:=best_respond(cust_price,Q=Q, C=C, wgt=weight), by=quarter_year]
  
  ### compute new shares. recall that outside good quality normalized to 0
  
  holder[, new_share:=exp(Q-rho*newprice)]
  holder[,new_share:=new_share/(sum(weight*new_share)+1), by=quarter_year]
  
  puzzle<-data.table()
  ## with shares, recall demand will be share times total addressable market which is county subdiv pop.
  bmats<-finda(1:nrow(holder))
  for (estab in 1:nrow(holder)){
    piece<-rowSums(bmats[[estab]])*holder$avg_labor[estab]*holder$new_share[estab]*holder$weight[estab]*holder$CSPOP[estab]
    piece<-c( piece,estim_sample[estab,]$quarter_year)
    puzzle<-rbind(puzzle, t(piece))
  }
  check_labor<-puzzle[, .(L1=sum(V1), L2=sum(V2), L3=sum(V3), L4=sum(V4), L5=sum(V5), l6=sum(V6)) , by=V7 ]
  stopifnot(nrow(check_labor)==1)
  
  return(list("mainstats"=holder[,c("location_id", "newprice","cust_price","new_share","salon_share_subdiv","Imodel","s_index", "Q", "C")],
              "bmats"=finda(1:nrow(x))))
  
}


out<-counterfactual_no_int(coef(res_store), gamma_vec, quad_wages$par,estim_matrix)
puzzle<-data.table()
for (estab in 1:nrow(estim_sample)){
  piece<-sum(diag(out$bmats[[estab]]))*estim_sample$avg_labor[estab]*out$mainstats$new_share[estab]*estim_sample$weight[estab]*estim_sample$CSPOP[estab]
  piece<-c( piece,estim_sample[estab,]$quarter_year)
  puzzle<-rbind(puzzle, t(piece))
}

counter_res<-rbind(counter_res,data.table(context="Sales Tax Eliminated  - No Adj.",mean_wage=as.numeric(weighted.mean(quad_wages$par,labor_supply )),
                                          sd_wage=as.numeric(weighted.var(quad_wages$par,labor_supply ))^(1/2),
                                          task_spec=sum(puzzle$V1)/sum(labor_supply),
                                          avg_complexity=mean(out$mainstats$Imodel),
                                          sd_complexity=sd(out$mainstats$Imodel),
                                          markup=mean(1/(1-out$mainstats$new_share)/coef(res_store)[1]/tau),
                                          pct_markup=mean(1/(1-out$mainstats$new_share)/coef(res_store)[1]/tau/out$mainstats$C),
                                          total_profit=sum(estim_sample$CSPOP[1]*out$mainstats$new_share/(1-out$mainstats$new_share)/coef(res_store)[1]/tau),
                                          avg_profit=mean(estim_sample$CSPOP[1]*out$mainstats$new_share/(1-out$mainstats$new_share)/coef(res_store)[1]/tau),
                                          sd_profit=sd(estim_sample$CSPOP[1]*out$mainstats$new_share/(1-out$mainstats$new_share)/coef(res_store)[1]/tau),
                                          consumer_welfare=estim_sample$CSPOP[1]/coef(res_store)[1]*(logSumExp(c(out$mainstats$Q-tau*coef(res_store)[1]*out$mainstats$newprice,0))-digamma(1)),
                                          quality=mean(out$mainstats$Q),
                                          w1=quad_wages$par[1],
                                          w2=quad_wages$par[2],
                                          w3=quad_wages$par[3],
                                          w4=quad_wages$par[4],
                                          w5=quad_wages$par[5],
                                          w6=quad_wages$par[6],
                                          obj_value=quad_wages$value
))




names<-"Sales Tax Eliminated - No Adj."
counter_objects[[names]]<-out
save(counter_res,counter_objects, file="data/03_00_counterfactual.RData")



## Counterfactual 1: sales tax as in NYC
tau<-1
quad_wages<-optim(as.numeric(w_guesses[3,]),quad_solver,method="L-BFGS-B", lower=rep(0,6), upper=rep(Inf, 6), control=list(trace=1))
counter=1
while (quad_wages$value>0.01 & counter<=3){
  quad_wages<-optim(quad_wages$par,quad_solver, control=list(trace=1))
  if (quad_wages$value>0.01) quad_wages<-optim(quad_wages$par,quad_solver,method="L-BFGS-B", lower=rep(0,6), upper=rep(Inf, 6), control=list(trace=1))
  counter<-counter+1
}

counterfactual<-function(parms,gamma_vec,wages, x){
  rho<-parms[1]*tau
  u_bar<-parms[2]
  c_bar<-parms[3]
  w1<-wages
  cmat1<-parms[10:14]
  umat<-parms[15:19]
  smat<-parms[20:25]
  
  
  wmat<-matrix(w1, nrow=6, ncol=6, byrow=FALSE)
  h<-diag(smat)
  
  ## this function will return matrix given gamma
  firm_estim<-function(gamma,ap2, ap3, ap4,ap5, ap6, abar){
    aleph<-c(1-ap2-ap3-ap4-ap5-ap6, ap2, ap3, ap4, ap5, ap6)
    if (gamma<1000000000 & !is.na(gamma)){
      lambda<-1/gamma
      A<-exp(-lambda*(wmat -h/rho/abar) )
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
    } else{# this is for firms with unidentified high gamma.
      # assume their gamma is still high enough s.t. they choose least complex structure.
      ahelper<-t(matrix(aleph,length(aleph), 6))
      E<-rep(0,6)
      E[which.min(rowSums((wmat -h/rho/abar)*ahelper))[1]]<-1
      a<-E*ahelper
      a<-(round(E,7)>0)*a
      E<-round(E,7)
      lambda<-0
    }
    dstar<- sum(a*(-rho*wmat +h))
    return(list(a,dstar,lambda ))
  }
  firm_estim<-Vectorize(firm_estim)
  
  res<-mclapply(1:nrow(x), function(y){firm_estim(gamma_vec[y], x[y,2], x[y,3], x[y,4],x[y,5],x[y,6],x[y,11]
  )}, mc.cores=core_count)
  
  # helper functions
  finda<-Vectorize(function(x){res[x][[1]][1]})
  findW<-Vectorize(function(y){sum(wmat*res[y][[1]][[1]])})
  findqual<-Vectorize(function(y){sum(finda(y)[[1]]*(h)) })
  findI<-Vectorize(function(y){
    a<-res[[y]][[1]]
    return(sum(a*spec_log(t(t(a/rowSums(a))/colSums(a)))))
  })
  
  
  ### new to store matrix of all objects.
  holder<-copy(estim_sample)
  holder[,W:=findW(1:nrow(x))]
  holder[,xi:=findqual(1:nrow(x))]
  holder[,Imodel:=findI(1:nrow(x))]
  holder[, Q:=xi+u_bar+x[,2:6]%*%umat+ny_objects$nu]
  holder[, C:=x[,11]*(W + gamma_vec*Imodel) + x[,2:6]%*%cmat1 +c_bar+ny_objects$phi]
  
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
  holder[, newprice:=best_respond(cust_price,Q=Q, C=C, wgt=weight), by=quarter_year]
  
  ### compute new shares. recall that outside good quality normalized to 0
  
  holder[, new_share:=exp(Q-rho*newprice)]
  holder[,new_share:=new_share/(sum(weight*new_share)+1), by=quarter_year]
  
  puzzle<-data.table()
  ## with shares, recall demand will be share times total addressable market which is county subdiv pop.
  bmats<-finda(1:nrow(holder))
  for (estab in 1:nrow(holder)){
    piece<-rowSums(bmats[[estab]])*holder$avg_labor[estab]*holder$new_share[estab]*holder$weight[estab]*holder$CSPOP[estab]
    piece<-c( piece,estim_sample[estab,]$quarter_year)
    puzzle<-rbind(puzzle, t(piece))
  }
  check_labor<-puzzle[, .(L1=sum(V1), L2=sum(V2), L3=sum(V3), L4=sum(V4), L5=sum(V5), l6=sum(V6)) , by=V7 ]
  stopifnot(nrow(check_labor)==1)
  
  return(list("mainstats"=holder[,c("location_id", "newprice","cust_price","new_share","salon_share_subdiv","Imodel","s_index", "Q", "C")],
              "bmats"=finda(1:nrow(x))))
  
}
out<-counterfactual(coef(res_store), gamma_vec, quad_wages$par,estim_matrix)
puzzle<-data.table()
for (estab in 1:nrow(estim_sample)){
  piece<-sum(diag(out$bmats[[estab]]))*estim_sample$avg_labor[estab]*out$mainstats$new_share[estab]*estim_sample$weight[estab]*estim_sample$CSPOP[estab]
  piece<-c( piece,estim_sample[estab,]$quarter_year)
  puzzle<-rbind(puzzle, t(piece))
}


counter_res<-rbind(counter_res,data.table(context="Sales Tax Eliminated",mean_wage=as.numeric(weighted.mean(quad_wages$par,labor_supply )),
                        sd_wage=as.numeric(weighted.var(quad_wages$par,labor_supply ))^(1/2),
                        task_spec=sum(puzzle$V1)/sum(labor_supply),
                        avg_complexity=mean(out$mainstats$Imodel),
                        sd_complexity=sd(out$mainstats$Imodel),
                        markup=mean(1/(1-out$mainstats$new_share)/coef(res_store)[1]/tau),
                        pct_markup=mean(1/(1-out$mainstats$new_share)/coef(res_store)[1]/tau/out$mainstats$C),
                        total_profit=sum(estim_sample$CSPOP[1]*out$mainstats$new_share/(1-out$mainstats$new_share)/coef(res_store)[1]/tau),
                        avg_profit=mean(estim_sample$CSPOP[1]*out$mainstats$new_share/(1-out$mainstats$new_share)/coef(res_store)[1]/tau),
                        sd_profit=sd(estim_sample$CSPOP[1]*out$mainstats$new_share/(1-out$mainstats$new_share)/coef(res_store)[1]/tau),
                        consumer_welfare=estim_sample$CSPOP[1]/coef(res_store)[1]*(logSumExp(c(out$mainstats$Q-tau*coef(res_store)[1]*out$mainstats$newprice,0))-digamma(1)),
                        quality=mean(out$mainstats$Q),
                        w1=quad_wages$par[1],
                        w2=quad_wages$par[2],
                        w3=quad_wages$par[3],
                        w4=quad_wages$par[4],
                        w5=quad_wages$par[5],
                        w6=quad_wages$par[6],
                        obj_value=quad_wages$value
))

names<-"Sales Tax Eliminated"
counter_objects[[names]]<-out
save(counter_res,counter_objects, file="data/03_00_counterfactual.RData")
tau<-1.045


## Counterfactual 1: Convergence of all gammas to lowest observed gamma (dissemination of management practices)
## at each step, each firm "copies" lower firm
rank_gamma<-rank(ny_objects$gammas, ties="random")
sorted_gamma<-sort(ny_objects$gammas)
for (leaps in 1:length(ny_objects$gammas)){
  gamma_vec<-sorted_gamma[pmax(rank_gamma-leaps,1)]
  gamma_vec[which(ny_objects$gammas>2000)]<-ny_objects$gammas[which(ny_objects$gammas>2000)] # keep the same.
  stopifnot(length(gamma_vec)==length(ny_objects$gammas))
  solve_game<-function(parms,gamma_vec,wages, x){
    rho<-parms[1]*tau
    u_bar<-parms[2]
    c_bar<-parms[3]
    w1<-wages
    cmat1<-parms[10:14]
    umat<-parms[15:19]
    smat<-parms[20:25]
    
    
    wmat<-matrix(w1, nrow=6, ncol=6, byrow=FALSE)
    h<-diag(smat)
    
    ## this function will return matrix given gamma
    firm_estim<-function(gamma,ap2, ap3, ap4,ap5, ap6, abar){
      aleph<-c(1-ap2-ap3-ap4-ap5-ap6, ap2, ap3, ap4, ap5, ap6)
      if (gamma<1000000000 & !is.na(gamma)){
        lambda<-1/gamma
        A<-exp(-lambda*(wmat -h/rho/abar) )
        A[A>=Inf]<-1e8
        fxpt<-function(p,y){
          C<-colSums(t(A)*aleph/colSums(A*p))
          return(p*C)
        }
        objval<-function(p,y){
          C<-colSums(t(A)*aleph/colSums(A*p))
          return(sum(p*spec_log(C), na.rm=TRUE)-max(spec_log(C), na.rm=TRUE))
        }
        E<-rep(1/6,6)
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
      } else{# this is for firms with unidentified high gamma.
        # assume their gamma is still high enough s.t. they choose least complex structure.
        ahelper<-t(matrix(aleph,length(aleph), 6))
        E<-rep(0,6)
        E[which.min(rowSums((wmat -h/rho/abar)*ahelper))[1]]<-1
        a<-E*ahelper
        lambda<-0
      }
      dstar<- sum(a*(-rho*wmat +h))
      return(list(a,dstar,lambda ))
    }
    firm_estim<-Vectorize(firm_estim)
    
    res<-mclapply(1:nrow(x), function(y){firm_estim(gamma_vec[y], x[y,2], x[y,3], x[y,4],x[y,5],x[y,6],x[y,11]
    )}, mc.cores=core_count)
    
    # helper functions
    finda<-Vectorize(function(x){res[x][[1]][1]})
    findW<-Vectorize(function(y){sum(wmat*res[y][[1]][[1]])})
    findqual<-Vectorize(function(y){sum(finda(y)[[1]]*(h)) })
    findI<-Vectorize(function(y){
      a<-res[[y]][[1]]
      return(sum(a*spec_log(t(t(a/rowSums(a))/colSums(a)))))
    })
    
    
    ### new to store matrix of all objects.
    holder<-copy(estim_sample)
    holder[,W:=findW(1:nrow(x))]
    holder[,xi:=findqual(1:nrow(x))]
    holder[,Imodel:=findI(1:nrow(x))]
    holder[, Q:=xi+u_bar+x[,2:6]%*%umat+ny_objects$nu]
    holder[, C:=x[,11]*(W + gamma_vec*Imodel) + x[,2:6]%*%cmat1 +c_bar+ny_objects$phi]
    
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
    holder[, newprice:=best_respond(cust_price,Q=Q, C=C, wgt=weight), by=quarter_year]
    
    ### compute new shares. recall that outside good quality normalized to 0
    
    holder[, new_share:=exp(Q-rho*newprice)]
    holder[,new_share:=new_share/(sum(weight*new_share)+1), by=quarter_year]
    
    puzzle<-data.table()
    ## with shares, recall demand will be share times total addressable market which is county subdiv pop.
    bmats<-finda(1:nrow(holder))
    for (estab in 1:nrow(holder)){
      piece<-rowSums(bmats[[estab]])*holder$avg_labor[estab]*holder$new_share[estab]*holder$weight[estab]*holder$CSPOP[estab]
      piece<-c( piece,estim_sample[estab,]$quarter_year)
      puzzle<-rbind(puzzle, t(piece))
    }
    check_labor<-puzzle[, .(L1=sum(V1), L2=sum(V2), L3=sum(V3), L4=sum(V4), L5=sum(V5), l6=sum(V6)) , by=V7 ]
    stopifnot(nrow(check_labor)==1)
    
    return(as.numeric(labor_supply-check_labor[,-1])) # negative means excess demand, positive means excess supply.
    # so negative means we need higher wages.
  }
  solve_helper<-function(x,t){
    temp<-wage_new
    temp[t]<-x
    output<-solve_game(parms=coef(res_store), gamma_vec=gamma_vec,wages=temp, x=estim_matrix)
    return(as.numeric(output[t])^2)
  }
  
  ### Find wages.
  return_excess<-function(w){
    return(solve_game(parms=coef(res_store), gamma_vec=gamma_vec,wages=w, x=estim_matrix))
  }
  
  quad_solver<-function(w){
    return(sum(solve_game(parms=coef(res_store), gamma_vec=gamma_vec,wages=w, x=estim_matrix)^2))
  }
  
  quad_wages<-optim(as.numeric(w_guesses[3+leaps,]),quad_solver,method="L-BFGS-B", lower=rep(0,6), upper=rep(Inf, 6), control=list(trace=1))
  counter=1
  while (quad_wages$value>0.01 & counter<=3){
    quad_wages<-optim(quad_wages$par,quad_solver, control=list(trace=1))
    if (quad_wages$value>0.01) quad_wages<-optim(quad_wages$par,quad_solver,method="L-BFGS-B", lower=rep(0,6), upper=rep(Inf, 6), control=list(trace=1))
    counter<-counter+1
  }
  counterfactual<-function(parms,gamma_vec,wages, x){
    rho<-parms[1]*tau
    u_bar<-parms[2]
    c_bar<-parms[3]
    w1<-wages
    cmat1<-parms[10:14]
    umat<-parms[15:19]
    smat<-parms[20:25]
    
    
    wmat<-matrix(w1, nrow=6, ncol=6, byrow=FALSE)
    h<-diag(smat)
    
    ## this function will return matrix given gamma
    firm_estim<-function(gamma,ap2, ap3, ap4,ap5, ap6, abar){
      aleph<-c(1-ap2-ap3-ap4-ap5-ap6, ap2, ap3, ap4, ap5, ap6)
      if (gamma<1000000000 & !is.na(gamma)){
        lambda<-1/gamma
        A<-exp(-lambda*(wmat -h/rho/abar) )
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
      } else{# this is for firms with unidentified high gamma.
        # assume their gamma is still high enough s.t. they choose least complex structure.
        ahelper<-t(matrix(aleph,length(aleph), 6))
        E<-rep(0,6)
        E[which.min(rowSums((wmat -h/rho/abar)*ahelper))[1]]<-1
        a<-E*ahelper
        a<-(round(E,7)>0)*a
        E<-round(E,7)
        lambda<-0
      }
      dstar<- sum(a*(-rho*wmat +h))
      return(list(a,dstar,lambda ))
    }
    firm_estim<-Vectorize(firm_estim)
    
    res<-mclapply(1:nrow(x), function(y){firm_estim(gamma_vec[y], x[y,2], x[y,3], x[y,4],x[y,5],x[y,6],x[y,11]
    )}, mc.cores=core_count)
    
    # helper functions
    finda<-Vectorize(function(x){res[x][[1]][1]})
    findW<-Vectorize(function(y){sum(wmat*res[y][[1]][[1]])})
    findqual<-Vectorize(function(y){sum(finda(y)[[1]]*(h)) })
    findI<-Vectorize(function(y){
      a<-res[[y]][[1]]
      return(sum(a*spec_log(t(t(a/rowSums(a))/colSums(a)))))
    })
    
    
    ### new to store matrix of all objects.
    holder<-copy(estim_sample)
    holder[,W:=findW(1:nrow(x))]
    holder[,xi:=findqual(1:nrow(x))]
    holder[,Imodel:=findI(1:nrow(x))]
    holder[, Q:=xi+u_bar+x[,2:6]%*%umat+ny_objects$nu]
    holder[, C:=x[,11]*(W + gamma_vec*Imodel) + x[,2:6]%*%cmat1 +c_bar+ny_objects$phi]
    
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
    holder[, newprice:=best_respond(cust_price,Q=Q, C=C, wgt=weight), by=quarter_year]
    
    ### compute new shares. recall that outside good quality normalized to 0
    
    holder[, new_share:=exp(Q-rho*newprice)]
    holder[,new_share:=new_share/(sum(weight*new_share)+1), by=quarter_year]
    
    puzzle<-data.table()
    ## with shares, recall demand will be share times total addressable market which is county subdiv pop.
    bmats<-finda(1:nrow(holder))
    for (estab in 1:nrow(holder)){
      piece<-rowSums(bmats[[estab]])*holder$avg_labor[estab]*holder$new_share[estab]*holder$weight[estab]*holder$CSPOP[estab]
      piece<-c( piece,estim_sample[estab,]$quarter_year)
      puzzle<-rbind(puzzle, t(piece))
    }
    check_labor<-puzzle[, .(L1=sum(V1), L2=sum(V2), L3=sum(V3), L4=sum(V4), L5=sum(V5), l6=sum(V6)) , by=V7 ]
    stopifnot(nrow(check_labor)==1)
    
    return(list("mainstats"=holder[,c("location_id", "newprice","cust_price","new_share","salon_share_subdiv","Imodel","s_index", "Q", "C")],
                "bmats"=finda(1:nrow(x))))
    
  }
  
  out<-counterfactual(coef(res_store), gamma_vec, quad_wages$par,estim_matrix)
  
  ## compute average time spent on specialty.
  puzzle<-data.table()
  for (estab in 1:nrow(estim_sample)){
    piece<-sum(diag(out$bmats[[estab]]))*estim_sample$avg_labor[estab]*out$mainstats$new_share[estab]*estim_sample$weight[estab]*estim_sample$CSPOP[estab]
    piece<-c( piece,estim_sample[estab,]$quarter_year)
    puzzle<-rbind(puzzle, t(piece))
  }
  
  counter_res<-rbind(counter_res,data.table(context=paste("Mimic", leaps),mean_wage=as.numeric(weighted.mean(quad_wages$par,labor_supply )),
                                            sd_wage=as.numeric(weighted.var(quad_wages$par,labor_supply ))^(1/2),
                                            task_spec=sum(puzzle$V1)/sum(labor_supply),
                                            avg_complexity=mean(out$mainstats$Imodel),
                                            sd_complexity=sd(out$mainstats$Imodel),
                                            markup=mean(1/(1-out$mainstats$new_share)/coef(res_store)[1]),
                                            pct_markup=mean(1/(1-out$mainstats$new_share)/coef(res_store)[1]/out$mainstats$C),
                                            total_profit=sum(estim_sample$CSPOP[1]*out$mainstats$new_share/(1-out$mainstats$new_share)/coef(res_store)[1]),
                                            avg_profit=mean(estim_sample$CSPOP[1]*out$mainstats$new_share/(1-out$mainstats$new_share)/coef(res_store)[1]),
                                            sd_profit=sd(estim_sample$CSPOP[1]*out$mainstats$new_share/(1-out$mainstats$new_share)/coef(res_store)[1]),
                                            consumer_welfare=estim_sample$CSPOP[1]/coef(res_store)[1]*(logSumExp(c(out$mainstats$Q-tau*coef(res_store)[1]*out$mainstats$newprice,0))-digamma(1)),
                                            quality=mean(out$mainstats$Q),
                                            w1=quad_wages$par[1],
                                            w2=quad_wages$par[2],
                                            w3=quad_wages$par[3],
                                            w4=quad_wages$par[4],
                                            w5=quad_wages$par[5],
                                            w6=quad_wages$par[6],
                                            obj_value=quad_wages$value
  ))
  
  
  names<-paste("Mimic", leaps)
  counter_objects[[names]]<-out
  
  save(counter_res,counter_objects, file="data/03_00_counterfactual.RData")
  
}



### 3. Uniform reduction in management costs.
# for each firm, compute new gamma as dollar savings for one hour of work
gamma_vec<-0.9*ny_objects$gammas

quad_wages<-optim(as.numeric(wages),quad_solver,method="L-BFGS-B", lower=rep(0,6), upper=rep(Inf, 6), control=list(trace=1))
counter=1
while (quad_wages$value>0.01 & counter<=3){
  quad_wages<-optim(quad_wages$par,quad_solver, control=list(trace=1))
  if (quad_wages$value>0.01) quad_wages<-optim(quad_wages$par,quad_solver,method="L-BFGS-B", lower=rep(0,6), upper=rep(Inf, 6), control=list(trace=1))
  counter<-counter+1
}

counterfactual<-function(parms,gamma_vec,wages, x){
  rho<-parms[1]*tau
  u_bar<-parms[2]
  c_bar<-parms[3]
  w1<-wages
  cmat1<-parms[10:14]
  umat<-parms[15:19]
  smat<-parms[20:25]
  
  
  wmat<-matrix(w1, nrow=6, ncol=6, byrow=FALSE)
  h<-diag(smat)
  
  ## this function will return matrix given gamma
  firm_estim<-function(gamma,ap2, ap3, ap4,ap5, ap6, abar){
    aleph<-c(1-ap2-ap3-ap4-ap5-ap6, ap2, ap3, ap4, ap5, ap6)
    if (gamma<1000000000 & !is.na(gamma)){
      lambda<-1/gamma
      A<-exp(-lambda*(wmat -h/rho/abar) )
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
    } else{# this is for firms with unidentified high gamma.
      # assume their gamma is still high enough s.t. they choose least complex structure.
      ahelper<-t(matrix(aleph,length(aleph), 6))
      E<-rep(0,6)
      E[which.min(rowSums((wmat -h/rho/abar)*ahelper))[1]]<-1
      a<-E*ahelper
      a<-(round(E,7)>0)*a
      E<-round(E,7)
      lambda<-0
    }
    dstar<- sum(a*(-rho*wmat +h))
    return(list(a,dstar,lambda ))
  }
  firm_estim<-Vectorize(firm_estim)
  
  res<-mclapply(1:nrow(x), function(y){firm_estim(gamma_vec[y], x[y,2], x[y,3], x[y,4],x[y,5],x[y,6],x[y,11]
  )}, mc.cores=core_count)
  
  # helper functions
  finda<-Vectorize(function(x){res[x][[1]][1]})
  findW<-Vectorize(function(y){sum(wmat*res[y][[1]][[1]])})
  findqual<-Vectorize(function(y){sum(finda(y)[[1]]*(h)) })
  findI<-Vectorize(function(y){
    a<-res[[y]][[1]]
    return(sum(a*spec_log(t(t(a/rowSums(a))/colSums(a)))))
  })
  
  
  ### new to store matrix of all objects.
  holder<-copy(estim_sample)
  holder[,W:=findW(1:nrow(x))]
  holder[,xi:=findqual(1:nrow(x))]
  holder[,Imodel:=findI(1:nrow(x))]
  holder[, Q:=xi+u_bar+x[,2:6]%*%umat+ny_objects$nu]
  holder[, C:=x[,11]*(W + gamma_vec*Imodel) + x[,2:6]%*%cmat1 +c_bar+ny_objects$phi]
  
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
  holder[, newprice:=best_respond(cust_price,Q=Q, C=C, wgt=weight), by=quarter_year]
  
  ### compute new shares. recall that outside good quality normalized to 0
  
  holder[, new_share:=exp(Q-rho*newprice)]
  holder[,new_share:=new_share/(sum(weight*new_share)+1), by=quarter_year]
  
  puzzle<-data.table()
  ## with shares, recall demand will be share times total addressable market which is county subdiv pop.
  bmats<-finda(1:nrow(holder))
  for (estab in 1:nrow(holder)){
    piece<-rowSums(bmats[[estab]])*holder$avg_labor[estab]*holder$new_share[estab]*holder$weight[estab]*holder$CSPOP[estab]
    piece<-c( piece,estim_sample[estab,]$quarter_year)
    puzzle<-rbind(puzzle, t(piece))
  }
  check_labor<-puzzle[, .(L1=sum(V1), L2=sum(V2), L3=sum(V3), L4=sum(V4), L5=sum(V5), l6=sum(V6)) , by=V7 ]
  stopifnot(nrow(check_labor)==1)
  
  return(list("mainstats"=holder[,c("location_id", "newprice","cust_price","new_share","salon_share_subdiv","Imodel","s_index", "Q", "C")],
              "bmats"=finda(1:nrow(x))))
  
}
out<-counterfactual(coef(res_store), gamma_vec, quad_wages$par,estim_matrix)
puzzle<-data.table()
for (estab in 1:nrow(estim_sample)){
  piece<-sum(diag(out$bmats[[estab]]))*estim_sample$avg_labor[estab]*out$mainstats$new_share[estab]*estim_sample$weight[estab]*estim_sample$CSPOP[estab]
  piece<-c( piece,estim_sample[estab,]$quarter_year)
  puzzle<-rbind(puzzle, t(piece))
}


counter_res<-rbind(counter_res,data.table(context="Improvement in Management Software 2",mean_wage=as.numeric(weighted.mean(quad_wages$par,labor_supply )),
                                          sd_wage=as.numeric(weighted.var(quad_wages$par,labor_supply ))^(1/2),
                                          task_spec=sum(puzzle$V1)/sum(labor_supply),
                                          avg_complexity=mean(out$mainstats$Imodel),
                                          sd_complexity=sd(out$mainstats$Imodel),
                                          markup=mean(1/(1-out$mainstats$new_share)/coef(res_store)[1]/tau),
                                          pct_markup=mean(1/(1-out$mainstats$new_share)/coef(res_store)[1]/tau/out$mainstats$C),
                                          total_profit=sum(estim_sample$CSPOP[1]*out$mainstats$new_share/(1-out$mainstats$new_share)/coef(res_store)[1]/tau),
                                          avg_profit=mean(estim_sample$CSPOP[1]*out$mainstats$new_share/(1-out$mainstats$new_share)/coef(res_store)[1]/tau),
                                          sd_profit=sd(estim_sample$CSPOP[1]*out$mainstats$new_share/(1-out$mainstats$new_share)/coef(res_store)[1]/tau),
                                          consumer_welfare=estim_sample$CSPOP[1]/coef(res_store)[1]*(logSumExp(c(out$mainstats$Q-tau*coef(res_store)[1]*out$mainstats$newprice,0))-digamma(1)),
                                          quality=mean(out$mainstats$Q),
                                          w1=quad_wages$par[1],
                                          w2=quad_wages$par[2],
                                          w3=quad_wages$par[3],
                                          w4=quad_wages$par[4],
                                          w5=quad_wages$par[5],
                                          w6=quad_wages$par[6],
                                          obj_value=quad_wages$value
))

names<-"Improvement in Management Software 2"
counter_objects[[names]]<-out
save(counter_res,counter_objects, file="data/03_00_counterfactual.RData")

### counterfactual: reduction in total labor from admin. specialists.
labor_supply[5]<-0.5*labor_supply[5]
gamma_vec<-ny_objects$gammas

quad_wages<-optim(as.numeric(wages),quad_solver,method="L-BFGS-B", lower=rep(0,6), upper=rep(Inf, 6), control=list(trace=1))
counter=1
while (quad_wages$value>0.01 & counter<=3){
  quad_wages<-optim(quad_wages$par,quad_solver, control=list(trace=1))
  if (quad_wages$value>0.01) quad_wages<-optim(quad_wages$par,quad_solver,method="L-BFGS-B", lower=rep(0,6), upper=rep(Inf, 6), control=list(trace=1))
  counter<-counter+1
}

counterfactual<-function(parms,gamma_vec,wages, x){
  rho<-parms[1]*tau
  u_bar<-parms[2]
  c_bar<-parms[3]
  w1<-wages
  cmat1<-parms[10:14]
  umat<-parms[15:19]
  smat<-parms[20:25]
  
  
  wmat<-matrix(w1, nrow=6, ncol=6, byrow=FALSE)
  h<-diag(smat)
  
  ## this function will return matrix given gamma
  firm_estim<-function(gamma,ap2, ap3, ap4,ap5, ap6, abar){
    aleph<-c(1-ap2-ap3-ap4-ap5-ap6, ap2, ap3, ap4, ap5, ap6)
    if (gamma<1000000000 & !is.na(gamma)){
      lambda<-1/gamma
      A<-exp(-lambda*(wmat -h/rho/abar) )
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
    } else{# this is for firms with unidentified high gamma.
      # assume their gamma is still high enough s.t. they choose least complex structure.
      ahelper<-t(matrix(aleph,length(aleph), 6))
      E<-rep(0,6)
      E[which.min(rowSums((wmat -h/rho/abar)*ahelper))[1]]<-1
      a<-E*ahelper
      a<-(round(E,7)>0)*a
      E<-round(E,7)
      lambda<-0
    }
    dstar<- sum(a*(-rho*wmat +h))
    return(list(a,dstar,lambda ))
  }
  firm_estim<-Vectorize(firm_estim)
  
  res<-mclapply(1:nrow(x), function(y){firm_estim(gamma_vec[y], x[y,2], x[y,3], x[y,4],x[y,5],x[y,6],x[y,11]
  )}, mc.cores=core_count)
  
  # helper functions
  finda<-Vectorize(function(x){res[x][[1]][1]})
  findW<-Vectorize(function(y){sum(wmat*res[y][[1]][[1]])})
  findqual<-Vectorize(function(y){sum(finda(y)[[1]]*(h)) })
  findI<-Vectorize(function(y){
    a<-res[[y]][[1]]
    return(sum(a*spec_log(t(t(a/rowSums(a))/colSums(a)))))
  })
  
  
  ### new to store matrix of all objects.
  holder<-copy(estim_sample)
  holder[,W:=findW(1:nrow(x))]
  holder[,xi:=findqual(1:nrow(x))]
  holder[,Imodel:=findI(1:nrow(x))]
  holder[, Q:=xi+u_bar+x[,2:6]%*%umat+ny_objects$nu]
  holder[, C:=x[,11]*(W + gamma_vec*Imodel) + x[,2:6]%*%cmat1 +c_bar+ny_objects$phi]
  
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
  holder[, newprice:=best_respond(cust_price,Q=Q, C=C, wgt=weight), by=quarter_year]
  
  ### compute new shares. recall that outside good quality normalized to 0
  
  holder[, new_share:=exp(Q-rho*newprice)]
  holder[,new_share:=new_share/(sum(weight*new_share)+1), by=quarter_year]
  
  puzzle<-data.table()
  ## with shares, recall demand will be share times total addressable market which is county subdiv pop.
  bmats<-finda(1:nrow(holder))
  for (estab in 1:nrow(holder)){
    piece<-rowSums(bmats[[estab]])*holder$avg_labor[estab]*holder$new_share[estab]*holder$weight[estab]*holder$CSPOP[estab]
    piece<-c( piece,estim_sample[estab,]$quarter_year)
    puzzle<-rbind(puzzle, t(piece))
  }
  check_labor<-puzzle[, .(L1=sum(V1), L2=sum(V2), L3=sum(V3), L4=sum(V4), L5=sum(V5), l6=sum(V6)) , by=V7 ]
  stopifnot(nrow(check_labor)==1)
  
  return(list("mainstats"=holder[,c("location_id", "newprice","cust_price","new_share","salon_share_subdiv","Imodel","s_index", "Q", "C")],
              "bmats"=finda(1:nrow(x))))
  
}
out<-counterfactual(coef(res_store), gamma_vec, quad_wages$par,estim_matrix)
puzzle<-data.table()
for (estab in 1:nrow(estim_sample)){
  piece<-sum(diag(out$bmats[[estab]]))*estim_sample$avg_labor[estab]*out$mainstats$new_share[estab]*estim_sample$weight[estab]*estim_sample$CSPOP[estab]
  piece<-c( piece,estim_sample[estab,]$quarter_year)
  puzzle<-rbind(puzzle, t(piece))
}


counter_res<-rbind(counter_res,data.table(context="Admin. Gone",mean_wage=as.numeric(weighted.mean(quad_wages$par,labor_supply )),
                                          sd_wage=as.numeric(weighted.var(quad_wages$par,labor_supply ))^(1/2),
                                          task_spec=sum(puzzle$V1)/sum(labor_supply),
                                          avg_complexity=mean(out$mainstats$Imodel),
                                          sd_complexity=sd(out$mainstats$Imodel),
                                          markup=mean(1/(1-out$mainstats$new_share)/coef(res_store)[1]/tau),
                                          pct_markup=mean(1/(1-out$mainstats$new_share)/coef(res_store)[1]/tau/out$mainstats$C),
                                          total_profit=sum(estim_sample$CSPOP[1]*out$mainstats$new_share/(1-out$mainstats$new_share)/coef(res_store)[1]/tau),
                                          avg_profit=mean(estim_sample$CSPOP[1]*out$mainstats$new_share/(1-out$mainstats$new_share)/coef(res_store)[1]/tau),
                                          sd_profit=sd(estim_sample$CSPOP[1]*out$mainstats$new_share/(1-out$mainstats$new_share)/coef(res_store)[1]/tau),
                                          consumer_welfare=estim_sample$CSPOP[1]/coef(res_store)[1]*(logSumExp(c(out$mainstats$Q-tau*coef(res_store)[1]*out$mainstats$newprice,0))-digamma(1)),
                                          quality=mean(out$mainstats$Q),
                                          w1=quad_wages$par[1],
                                          w2=quad_wages$par[2],
                                          w3=quad_wages$par[3],
                                          w4=quad_wages$par[4],
                                          w5=quad_wages$par[5],
                                          w6=quad_wages$par[6],
                                          obj_value=quad_wages$value
))

names<-"Admin. Gone"
counter_objects[[names]]<-out
save(counter_res,counter_objects, file="data/03_00_counterfactual.RData")



### counterfactual: reduction in total labor from admin. specialists.
labor_supply<-as.numeric(tot_labor[1,-1])
labor_supply[4]<-0.5*labor_supply[4]
gamma_vec<-ny_objects$gammas

quad_wages<-optim(as.numeric(wages),quad_solver,method="L-BFGS-B", lower=rep(0,6), upper=rep(Inf, 6), control=list(trace=1))
counter=1
while (quad_wages$value>0.01 & counter<=3){
  quad_wages<-optim(quad_wages$par,quad_solver, control=list(trace=1))
  if (quad_wages$value>0.01) quad_wages<-optim(quad_wages$par,quad_solver,method="L-BFGS-B", lower=rep(0,6), upper=rep(Inf, 6), control=list(trace=1))
  counter<-counter+1
}

counterfactual<-function(parms,gamma_vec,wages, x){
  rho<-parms[1]*tau
  u_bar<-parms[2]
  c_bar<-parms[3]
  w1<-wages
  cmat1<-parms[10:14]
  umat<-parms[15:19]
  smat<-parms[20:25]
  
  
  wmat<-matrix(w1, nrow=6, ncol=6, byrow=FALSE)
  h<-diag(smat)
  
  ## this function will return matrix given gamma
  firm_estim<-function(gamma,ap2, ap3, ap4,ap5, ap6, abar){
    aleph<-c(1-ap2-ap3-ap4-ap5-ap6, ap2, ap3, ap4, ap5, ap6)
    if (gamma<1000000000 & !is.na(gamma)){
      lambda<-1/gamma
      A<-exp(-lambda*(wmat -h/rho/abar) )
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
    } else{# this is for firms with unidentified high gamma.
      # assume their gamma is still high enough s.t. they choose least complex structure.
      ahelper<-t(matrix(aleph,length(aleph), 6))
      E<-rep(0,6)
      E[which.min(rowSums((wmat -h/rho/abar)*ahelper))[1]]<-1
      a<-E*ahelper
      a<-(round(E,7)>0)*a
      E<-round(E,7)
      lambda<-0
    }
    dstar<- sum(a*(-rho*wmat +h))
    return(list(a,dstar,lambda ))
  }
  firm_estim<-Vectorize(firm_estim)
  
  res<-mclapply(1:nrow(x), function(y){firm_estim(gamma_vec[y], x[y,2], x[y,3], x[y,4],x[y,5],x[y,6],x[y,11]
  )}, mc.cores=core_count)
  
  # helper functions
  finda<-Vectorize(function(x){res[x][[1]][1]})
  findW<-Vectorize(function(y){sum(wmat*res[y][[1]][[1]])})
  findqual<-Vectorize(function(y){sum(finda(y)[[1]]*(h)) })
  findI<-Vectorize(function(y){
    a<-res[[y]][[1]]
    return(sum(a*spec_log(t(t(a/rowSums(a))/colSums(a)))))
  })
  
  
  ### new to store matrix of all objects.
  holder<-copy(estim_sample)
  holder[,W:=findW(1:nrow(x))]
  holder[,xi:=findqual(1:nrow(x))]
  holder[,Imodel:=findI(1:nrow(x))]
  holder[, Q:=xi+u_bar+x[,2:6]%*%umat+ny_objects$nu]
  holder[, C:=x[,11]*(W + gamma_vec*Imodel) + x[,2:6]%*%cmat1 +c_bar+ny_objects$phi]
  
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
  holder[, newprice:=best_respond(cust_price,Q=Q, C=C, wgt=weight), by=quarter_year]
  
  ### compute new shares. recall that outside good quality normalized to 0
  
  holder[, new_share:=exp(Q-rho*newprice)]
  holder[,new_share:=new_share/(sum(weight*new_share)+1), by=quarter_year]
  
  puzzle<-data.table()
  ## with shares, recall demand will be share times total addressable market which is county subdiv pop.
  bmats<-finda(1:nrow(holder))
  for (estab in 1:nrow(holder)){
    piece<-rowSums(bmats[[estab]])*holder$avg_labor[estab]*holder$new_share[estab]*holder$weight[estab]*holder$CSPOP[estab]
    piece<-c( piece,estim_sample[estab,]$quarter_year)
    puzzle<-rbind(puzzle, t(piece))
  }
  check_labor<-puzzle[, .(L1=sum(V1), L2=sum(V2), L3=sum(V3), L4=sum(V4), L5=sum(V5), l6=sum(V6)) , by=V7 ]
  stopifnot(nrow(check_labor)==1)
  
  return(list("mainstats"=holder[,c("location_id", "newprice","cust_price","new_share","salon_share_subdiv","Imodel","s_index", "Q", "C")],
              "bmats"=finda(1:nrow(x))))
  
}
out<-counterfactual(coef(res_store), gamma_vec, quad_wages$par,estim_matrix)
puzzle<-data.table()
for (estab in 1:nrow(estim_sample)){
  piece<-sum(diag(out$bmats[[estab]]))*estim_sample$avg_labor[estab]*out$mainstats$new_share[estab]*estim_sample$weight[estab]*estim_sample$CSPOP[estab]
  piece<-c( piece,estim_sample[estab,]$quarter_year)
  puzzle<-rbind(puzzle, t(piece))
}


counter_res<-rbind(counter_res,data.table(context="Blowdry Gone",mean_wage=as.numeric(weighted.mean(quad_wages$par,labor_supply )),
                                          sd_wage=as.numeric(weighted.var(quad_wages$par,labor_supply ))^(1/2),
                                          task_spec=sum(puzzle$V1)/sum(labor_supply),
                                          avg_complexity=mean(out$mainstats$Imodel),
                                          sd_complexity=sd(out$mainstats$Imodel),
                                          markup=mean(1/(1-out$mainstats$new_share)/coef(res_store)[1]/tau),
                                          pct_markup=mean(1/(1-out$mainstats$new_share)/coef(res_store)[1]/tau/out$mainstats$C),
                                          total_profit=sum(estim_sample$CSPOP[1]*out$mainstats$new_share/(1-out$mainstats$new_share)/coef(res_store)[1]/tau),
                                          avg_profit=mean(estim_sample$CSPOP[1]*out$mainstats$new_share/(1-out$mainstats$new_share)/coef(res_store)[1]/tau),
                                          sd_profit=sd(estim_sample$CSPOP[1]*out$mainstats$new_share/(1-out$mainstats$new_share)/coef(res_store)[1]/tau),
                                          consumer_welfare=estim_sample$CSPOP[1]/coef(res_store)[1]*(logSumExp(c(out$mainstats$Q-tau*coef(res_store)[1]*out$mainstats$newprice,0))-digamma(1)),
                                          quality=mean(out$mainstats$Q),
                                          w1=quad_wages$par[1],
                                          w2=quad_wages$par[2],
                                          w3=quad_wages$par[3],
                                          w4=quad_wages$par[4],
                                          w5=quad_wages$par[5],
                                          w6=quad_wages$par[6],
                                          obj_value=quad_wages$value
))

names<-"Blowdry Gone"
counter_objects[[names]]<-out
save(counter_res,counter_objects, file="data/03_00_counterfactual.RData")



#### Counterfactual - make all firms organizationally equivalent.
labor_supply<-as.numeric(tot_labor[1,-1])
gamma_vec<-rep(median(ny_objects$gammas),length(ny_objects$gammas))
# do not change one firm with very high gamma
gamma_vec[which(ny_objects$gammas>4000)]<- ny_objects$gammas[which(ny_objects$gammas>4000)]
quad_wages<-optim(as.numeric(wages),quad_solver,method="L-BFGS-B", lower=rep(0,6), upper=rep(Inf, 6), control=list(trace=1))
counter=1
while (quad_wages$value>0.01 & counter<=6){
  if (quad_wages$value>0.01) quad_wages<-optim(quad_wages$par,quad_solver,method="L-BFGS-B", lower=rep(0,6), upper=rep(Inf, 6), control=list(trace=1))
  counter<-counter+1
}

counterfactual<-function(parms,gamma_vec,wages, x){
  rho<-parms[1]*tau
  u_bar<-parms[2]
  c_bar<-parms[3]
  w1<-wages
  cmat1<-parms[10:14]
  umat<-parms[15:19]
  smat<-parms[20:25]
  
  
  wmat<-matrix(w1, nrow=6, ncol=6, byrow=FALSE)
  h<-diag(smat)
  
  ## this function will return matrix given gamma
  firm_estim<-function(gamma,ap2, ap3, ap4,ap5, ap6, abar){
    aleph<-c(1-ap2-ap3-ap4-ap5-ap6, ap2, ap3, ap4, ap5, ap6)
    if (gamma<1000000000 & !is.na(gamma)){
      lambda<-1/gamma
      A<-exp(-lambda*(wmat -h/rho/abar) )
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
    } else{# this is for firms with unidentified high gamma.
      # assume their gamma is still high enough s.t. they choose least complex structure.
      ahelper<-t(matrix(aleph,length(aleph), 6))
      E<-rep(0,6)
      E[which.min(rowSums((wmat -h/rho/abar)*ahelper))[1]]<-1
      a<-E*ahelper
      a<-(round(E,7)>0)*a
      E<-round(E,7)
      lambda<-0
    }
    dstar<- sum(a*(-rho*wmat +h))
    return(list(a,dstar,lambda ))
  }
  firm_estim<-Vectorize(firm_estim)
  
  res<-mclapply(1:nrow(x), function(y){firm_estim(gamma_vec[y], x[y,2], x[y,3], x[y,4],x[y,5],x[y,6],x[y,11]
  )}, mc.cores=core_count)
  
  # helper functions
  finda<-Vectorize(function(x){res[x][[1]][1]})
  findW<-Vectorize(function(y){sum(wmat*res[y][[1]][[1]])})
  findqual<-Vectorize(function(y){sum(finda(y)[[1]]*(h)) })
  findI<-Vectorize(function(y){
    a<-res[[y]][[1]]
    return(sum(a*spec_log(t(t(a/rowSums(a))/colSums(a)))))
  })
  
  
  ### new to store matrix of all objects.
  holder<-copy(estim_sample)
  holder[,W:=findW(1:nrow(x))]
  holder[,xi:=findqual(1:nrow(x))]
  holder[,Imodel:=findI(1:nrow(x))]
  holder[, Q:=xi+u_bar+x[,2:6]%*%umat+ny_objects$nu]
  holder[, C:=x[,11]*(W + gamma_vec*Imodel) + x[,2:6]%*%cmat1 +c_bar+ny_objects$phi]
  
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
  holder[, newprice:=best_respond(cust_price,Q=Q, C=C, wgt=weight), by=quarter_year]
  
  ### compute new shares. recall that outside good quality normalized to 0
  
  holder[, new_share:=exp(Q-rho*newprice)]
  holder[,new_share:=new_share/(sum(weight*new_share)+1), by=quarter_year]
  
  puzzle<-data.table()
  ## with shares, recall demand will be share times total addressable market which is county subdiv pop.
  bmats<-finda(1:nrow(holder))
  for (estab in 1:nrow(holder)){
    piece<-rowSums(bmats[[estab]])*holder$avg_labor[estab]*holder$new_share[estab]*holder$weight[estab]*holder$CSPOP[estab]
    piece<-c( piece,estim_sample[estab,]$quarter_year)
    puzzle<-rbind(puzzle, t(piece))
  }
  check_labor<-puzzle[, .(L1=sum(V1), L2=sum(V2), L3=sum(V3), L4=sum(V4), L5=sum(V5), l6=sum(V6)) , by=V7 ]
  stopifnot(nrow(check_labor)==1)
  
  return(list("mainstats"=holder[,c("location_id", "newprice","cust_price","new_share","salon_share_subdiv","Imodel","s_index", "Q", "C")],
              "bmats"=finda(1:nrow(x))))
  
}
out<-counterfactual(coef(res_store), gamma_vec, quad_wages$par,estim_matrix)
puzzle<-data.table()
for (estab in 1:nrow(estim_sample)){
  piece<-sum(diag(out$bmats[[estab]]))*estim_sample$avg_labor[estab]*out$mainstats$new_share[estab]*estim_sample$weight[estab]*estim_sample$CSPOP[estab]
  piece<-c( piece,estim_sample[estab,]$quarter_year)
  puzzle<-rbind(puzzle, t(piece))
}


counter_res<-rbind(counter_res,data.table(context="No Heterogeneity",mean_wage=as.numeric(weighted.mean(quad_wages$par,labor_supply )),
                                          sd_wage=as.numeric(weighted.var(quad_wages$par,labor_supply ))^(1/2),
                                          task_spec=sum(puzzle$V1)/sum(labor_supply),
                                          avg_complexity=mean(out$mainstats$Imodel),
                                          sd_complexity=sd(out$mainstats$Imodel),
                                          markup=mean(1/(1-out$mainstats$new_share)/coef(res_store)[1]/tau),
                                          pct_markup=mean(1/(1-out$mainstats$new_share)/coef(res_store)[1]/tau/out$mainstats$C),
                                          total_profit=sum(estim_sample$CSPOP[1]*out$mainstats$new_share/(1-out$mainstats$new_share)/coef(res_store)[1]/tau),
                                          avg_profit=mean(estim_sample$CSPOP[1]*out$mainstats$new_share/(1-out$mainstats$new_share)/coef(res_store)[1]/tau),
                                          sd_profit=sd(estim_sample$CSPOP[1]*out$mainstats$new_share/(1-out$mainstats$new_share)/coef(res_store)[1]/tau),
                                          consumer_welfare=estim_sample$CSPOP[1]/coef(res_store)[1]*(logSumExp(c(out$mainstats$Q-tau*coef(res_store)[1]*out$mainstats$newprice,0))-digamma(1)),
                                          quality=mean(out$mainstats$Q),
                                          w1=quad_wages$par[1],
                                          w2=quad_wages$par[2],
                                          w3=quad_wages$par[3],
                                          w4=quad_wages$par[4],
                                          w5=quad_wages$par[5],
                                          w6=quad_wages$par[6],
                                          obj_value=quad_wages$value
))

names<-"No Heterogeneity"
counter_objects[[names]]<-out
save(counter_res,counter_objects, file="data/03_00_counterfactual.RData")



#### Counterfactual - minimum wage to 20
labor_supply<-as.numeric(tot_labor[1,-1])
gamma_vec<-ny_objects$gammas
rm(solve_game)
solve_min_game<-function(parms,gamma_vec,wages, x){
  rho<-parms[1]*tau
  u_bar<-parms[2]
  c_bar<-parms[3]
  w1<-c(wages[1:3], 20,20,wages[4])
  cmat1<-parms[10:14]
  umat<-parms[15:19]
  smat<-parms[20:25]
  
  
  wmat<-matrix(w1, nrow=6, ncol=6, byrow=FALSE)
  h<-diag(smat)
  
  ## this function will return matrix given gamma
  firm_estim<-function(gamma,ap2, ap3, ap4,ap5, ap6, abar){
    aleph<-c(1-ap2-ap3-ap4-ap5-ap6, ap2, ap3, ap4, ap5, ap6)
    if (gamma<1000000000 & !is.na(gamma)){
      lambda<-1/gamma
      A<-exp(-lambda*(wmat -h/rho/abar) )
      A[A>=Inf]<-1e8
      fxpt<-function(p,y){
        C<-colSums(t(A)*aleph/colSums(A*p))
        return(p*C)
      }
      objval<-function(p,y){
        C<-colSums(t(A)*aleph/colSums(A*p))
        return(sum(p*spec_log(C), na.rm=TRUE)-max(spec_log(C), na.rm=TRUE))
      }
      E<-rep(1/6,6)
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
    } else{# this is for firms with unidentified high gamma.
      # assume their gamma is still high enough s.t. they choose least complex structure.
      ahelper<-t(matrix(aleph,length(aleph), 6))
      E<-rep(0,6)
      E[which.min(rowSums((wmat -h/rho/abar)*ahelper))[1]]<-1
      a<-E*ahelper
      lambda<-0
    }
    dstar<- sum(a*(-rho*wmat +h))
    return(list(a,dstar,lambda ))
  }
  firm_estim<-Vectorize(firm_estim)
  
  res<-mclapply(1:nrow(x), function(y){firm_estim(gamma_vec[y], x[y,2], x[y,3], x[y,4],x[y,5],x[y,6],x[y,11]
  )}, mc.cores=core_count)
  
  # helper functions
  finda<-Vectorize(function(x){res[x][[1]][1]})
  findW<-Vectorize(function(y){sum(wmat*res[y][[1]][[1]])})
  findqual<-Vectorize(function(y){sum(finda(y)[[1]]*(h)) })
  findI<-Vectorize(function(y){
    a<-res[[y]][[1]]
    return(sum(a*spec_log(t(t(a/rowSums(a))/colSums(a)))))
  })
  
  
  ### new to store matrix of all objects.
  holder<-copy(estim_sample)
  holder[,W:=findW(1:nrow(x))]
  holder[,xi:=findqual(1:nrow(x))]
  holder[,Imodel:=findI(1:nrow(x))]
  holder[, Q:=xi+u_bar+x[,2:6]%*%umat+ny_objects$nu]
  holder[, C:=x[,11]*(W + gamma_vec*Imodel) + x[,2:6]%*%cmat1 +c_bar+ny_objects$phi]
  
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
  holder[, newprice:=best_respond(cust_price,Q=Q, C=C, wgt=weight), by=quarter_year]
  
  ### compute new shares. recall that outside good quality normalized to 0
  
  holder[, new_share:=exp(Q-rho*newprice)]
  holder[,new_share:=new_share/(sum(weight*new_share)+1), by=quarter_year]
  
  puzzle<-data.table()
  ## with shares, recall demand will be share times total addressable market which is county subdiv pop.
  bmats<-finda(1:nrow(holder))
  for (estab in 1:nrow(holder)){
    piece<-rowSums(bmats[[estab]])*holder$avg_labor[estab]*holder$new_share[estab]*holder$weight[estab]*holder$CSPOP[estab]
    piece<-c( piece,estim_sample[estab,]$quarter_year)
    puzzle<-rbind(puzzle, t(piece))
  }
  check_labor<-puzzle[, .(L1=sum(V1), L2=sum(V2), L3=sum(V3), L4=sum(V4), L5=sum(V5), l6=sum(V6)) , by=V7 ]
  stopifnot(nrow(check_labor)==1)
  
  return(as.numeric(labor_supply[-c(4,5)]-check_labor[,-c(1,4,5)])) # negative means excess demand, positive means excess supply.
  # so negative means we need higher wages.
}
quad_solver_min<-function(w){
  return(sum(solve_min_game(parms=coef(res_store), gamma_vec=gamma_vec,wages=w, x=estim_matrix)^2))
}
quad_wages<-optim(as.numeric(wages)[c(1,2,3,6)],quad_solver_min,method="L-BFGS-B", lower=rep(0,4), upper=rep(Inf, 4), control=list(trace=1))
counter=1
while (quad_wages$value>0.01 & counter<=6){
  quad_wages<-optim(quad_wages$par,quad_solver_min, control=list(trace=1))
  if (quad_wages$value>0.01) quad_wages<-optim(quad_wages$par,quad_solver_min,method="L-BFGS-B", lower=rep(0,4), upper=rep(Inf, 4), control=list(trace=1))
  counter<-counter+1
}

counterfactual<-function(parms,gamma_vec,wages, x){
  rho<-parms[1]*tau
  u_bar<-parms[2]
  c_bar<-parms[3]
  w1<-wages
  cmat1<-parms[10:14]
  umat<-parms[15:19]
  smat<-parms[20:25]
  
  
  wmat<-matrix(w1, nrow=6, ncol=6, byrow=FALSE)
  h<-diag(smat)
  
  ## this function will return matrix given gamma
  firm_estim<-function(gamma,ap2, ap3, ap4,ap5, ap6, abar){
    aleph<-c(1-ap2-ap3-ap4-ap5-ap6, ap2, ap3, ap4, ap5, ap6)
    if (gamma<1000000000 & !is.na(gamma)){
      lambda<-1/gamma
      A<-exp(-lambda*(wmat -h/rho/abar) )
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
    } else{# this is for firms with unidentified high gamma.
      # assume their gamma is still high enough s.t. they choose least complex structure.
      ahelper<-t(matrix(aleph,length(aleph), 6))
      E<-rep(0,6)
      E[which.min(rowSums((wmat -h/rho/abar)*ahelper))[1]]<-1
      a<-E*ahelper
      a<-(round(E,7)>0)*a
      E<-round(E,7)
      lambda<-0
    }
    dstar<- sum(a*(-rho*wmat +h))
    return(list(a,dstar,lambda ))
  }
  firm_estim<-Vectorize(firm_estim)
  
  res<-mclapply(1:nrow(x), function(y){firm_estim(gamma_vec[y], x[y,2], x[y,3], x[y,4],x[y,5],x[y,6],x[y,11]
  )}, mc.cores=core_count)
  
  # helper functions
  finda<-Vectorize(function(x){res[x][[1]][1]})
  findW<-Vectorize(function(y){sum(wmat*res[y][[1]][[1]])})
  findqual<-Vectorize(function(y){sum(finda(y)[[1]]*(h)) })
  findI<-Vectorize(function(y){
    a<-res[[y]][[1]]
    return(sum(a*spec_log(t(t(a/rowSums(a))/colSums(a)))))
  })
  
  
  ### new to store matrix of all objects.
  holder<-copy(estim_sample)
  holder[,W:=findW(1:nrow(x))]
  holder[,xi:=findqual(1:nrow(x))]
  holder[,Imodel:=findI(1:nrow(x))]
  holder[, Q:=xi+u_bar+x[,2:6]%*%umat+ny_objects$nu]
  holder[, C:=x[,11]*(W + gamma_vec*Imodel) + x[,2:6]%*%cmat1 +c_bar+ny_objects$phi]
  
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
  holder[, newprice:=best_respond(cust_price,Q=Q, C=C, wgt=weight), by=quarter_year]
  
  ### compute new shares. recall that outside good quality normalized to 0
  
  holder[, new_share:=exp(Q-rho*newprice)]
  holder[,new_share:=new_share/(sum(weight*new_share)+1), by=quarter_year]
  
  puzzle<-data.table()
  ## with shares, recall demand will be share times total addressable market which is county subdiv pop.
  bmats<-finda(1:nrow(holder))
  for (estab in 1:nrow(holder)){
    piece<-rowSums(bmats[[estab]])*holder$avg_labor[estab]*holder$new_share[estab]*holder$weight[estab]*holder$CSPOP[estab]
    piece<-c( piece,estim_sample[estab,]$quarter_year)
    puzzle<-rbind(puzzle, t(piece))
  }
  check_labor<-puzzle[, .(L1=sum(V1), L2=sum(V2), L3=sum(V3), L4=sum(V4), L5=sum(V5), l6=sum(V6)) , by=V7 ]
  stopifnot(nrow(check_labor)==1)
  
  return(list("mainstats"=holder[,c("location_id", "newprice","cust_price","new_share","salon_share_subdiv","Imodel","s_index", "Q", "C")],
              "bmats"=finda(1:nrow(x))))
  
}

new_wages<-c(quad_wages$par[1:3],20,20,quad_wages$par[4])
out<-counterfactual(coef(res_store), gamma_vec, new_wages,estim_matrix)
puzzle<-data.table()
for (estab in 1:nrow(estim_sample)){
  piece<-sum(diag(out$bmats[[estab]]))*estim_sample$avg_labor[estab]*out$mainstats$new_share[estab]*estim_sample$weight[estab]*estim_sample$CSPOP[estab]
  piece<-c( piece,estim_sample[estab,]$quarter_year)
  puzzle<-rbind(puzzle, t(piece))
}


counter_res<-rbind(counter_res,data.table(context="Minimum Wage",mean_wage=as.numeric(weighted.mean(new_wages,labor_supply )),
                                          sd_wage=as.numeric(weighted.var(new_wages,labor_supply ))^(1/2),
                                          task_spec=sum(puzzle$V1)/sum(labor_supply),
                                          avg_complexity=mean(out$mainstats$Imodel),
                                          sd_complexity=sd(out$mainstats$Imodel),
                                          markup=mean(1/(1-out$mainstats$new_share)/coef(res_store)[1]/tau),
                                          pct_markup=mean(1/(1-out$mainstats$new_share)/coef(res_store)[1]/tau/out$mainstats$C),
                                          total_profit=sum(estim_sample$CSPOP[1]*out$mainstats$new_share/(1-out$mainstats$new_share)/coef(res_store)[1]/tau),
                                          avg_profit=mean(estim_sample$CSPOP[1]*out$mainstats$new_share/(1-out$mainstats$new_share)/coef(res_store)[1]/tau),
                                          sd_profit=sd(estim_sample$CSPOP[1]*out$mainstats$new_share/(1-out$mainstats$new_share)/coef(res_store)[1]/tau),
                                          consumer_welfare=estim_sample$CSPOP[1]/coef(res_store)[1]*(logSumExp(c(out$mainstats$Q-tau*coef(res_store)[1]*out$mainstats$newprice,0))-digamma(1)),
                                          quality=mean(out$mainstats$Q),
                                          w1=quad_wages$par[1],
                                          w2=quad_wages$par[2],
                                          w3=quad_wages$par[3],
                                          w4=quad_wages$par[4],
                                          w5=quad_wages$par[5],
                                          w6=quad_wages$par[6],
                                          obj_value=quad_wages$value
))

names<-"Minimum Wage"
counter_objects[[names]]<-out
save(counter_res,counter_objects, file="data/03_00_counterfactual.RData")



## counterfactual where firms can change B but not p

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
load('data/final_estimates.RData')
###### create objects

if (get_os()=="windows"){
  core_count<-1
} else{
  core_count<-42
} 

source('small_gmm_objective_less.R')



ny_objects<-ret_objects(coef(res_store), estim_matrix)


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
tot_labor<-puzzle[, .(L1=sum(V1), L2=sum(V2), L3=sum(V3), L4=sum(V4), L5=sum(V5)) , by=V6 ]
setnames(tot_labor, "V6", "quarter_year")
labor_supply<-as.numeric(tot_labor[1,-1])
oldwages<-wage_bound(coef(res_store)[4:8])



####### initial stats

## compute average time spent on specialty.
puzzle<-data.table()
for (estab in 1:nrow(estim_sample)){
  piece<-sum(diag(ny_objects$bmats[[estab]]))*estim_sample$avg_labor[estab]*estim_sample$salon_share_subdiv[estab]*estim_sample$weight[estab]*estim_sample$CSPOP[estab]
  piece<-c( piece,estim_sample[estab,]$quarter_year)
  puzzle<-rbind(puzzle, t(piece))
}

minwage_res<-data.table(context="Initial",mean_wage=as.numeric(weighted.mean(oldwages,labor_supply )),
                        sd_wage=as.numeric(weighted.var(oldwages,labor_supply ))^(1/2),
                        task_spec=sum(puzzle$V1)/sum(labor_supply),
                        avg_complexity=mean(estim_sample$s_index),
                        sd_complexity=sd(estim_sample$s_index),
                        markup=mean(1/(1-estim_sample$salon_share_subdiv)/exp(coef(res_store)[1])/tau),
                        pct_markup=mean(1/(1-estim_sample$salon_share_subdiv)/exp(coef(res_store)[1])/tau/ny_objects$C),
                        total_profit=sum(estim_sample$weight[1]*estim_sample$CSPOP[1]*estim_sample$salon_share_subdiv/(1-estim_sample$salon_share_subdiv)/exp(coef(res_store)[1])/tau),
                        avg_profit=mean(estim_sample$CSPOP[1]*estim_sample$salon_share_subdiv/(1-estim_sample$salon_share_subdiv)/exp(coef(res_store)[1])/tau),
                        sd_profit=sd(estim_sample$CSPOP[1]*estim_sample$salon_share_subdiv/(1-estim_sample$salon_share_subdiv)/exp(coef(res_store)[1])/tau),
                        consumer_welfare=estim_sample$CSPOP[1]/exp(coef(res_store)[1])*(logSumExp(c(ny_objects$Q-tau*exp(coef(res_store)[1])*estim_sample$cust_price,-log(estim_sample$weight[1])))+log(estim_sample$weight[1])-digamma(1)),
                        quality=mean(ny_objects$Q),
                        w1=oldwages[1],
                        w2=oldwages[2],
                        w3=oldwages[3],
                        w4=oldwages[4],
                        w5=oldwages[5],
                        unemployed=0,
                        obj_value=NA,
                        check_binding=NA
)

minwage_objects<-list("initial"=ny_objects)

#################



### create function which solves game.
### now big  - BE CAREFUL - THIS FUNCTION USES BOTH ESTIM_MATRIX AND ESTIM_SAMPLE

## firm can now only choose between all or nothing.
solve_game<-function(parms,gamma_vec,wages, x){
  rho<-exp(parms[1])*tau
  u_bar<-parms[2]
  c_bar<-parms[3]
  w1<-wages
  cmat1<-parms[9:12]
  umat<-parms[13:16]
  smat<-parms[17:21]
  
  
  wmat<-matrix(w1, nrow=5, ncol=5, byrow=FALSE)
  h<-diag(smat)
  
  ## this function will return matrix given gamma
  firm_estim<-function(gamma,ap2, ap3, ap4,ap5, abar){
    aleph<-c(1-ap2-ap3-ap4-ap5, ap2, ap3, ap4, ap5)

    # this is when the firm is frictionless, in which case they use water filling method.
    a<-matrix(0, ncol=5, nrow=5)
    for (col in 1:5){
      a[which.min((wmat -h/rho/abar)[,col]),col]<-aleph[col]
    }
    E<-rowSums(a)
    check_upper<-sum(a*spec_log(t(t(a/rowSums(a))/colSums(a))))
    check_upper<-check_upper-sum(a*(wmat -h/rho/abar)/gamma)
    a_upper<-a
    E_upper<-E
    lambda<-Inf

    # this is fully chair renter.
    ahelper<-t(matrix(aleph,length(aleph), 5))
    E<-rep(0,5)
    E[which.min(rowSums((wmat -h/rho/abar)*ahelper))[1]]<-1
    a<-E*ahelper
    lambda<-0
    
    ## choose whichever gives better profit.
    check_lower<-sum(a*spec_log(t(t(a/rowSums(a))/colSums(a))))
    check_lower<-check_lower-sum(a*(wmat -h/rho/abar)/gamma)
    if (check_lower>=check_upper){
      a<-a_upper
      E<-E_upper
      lambda<-Inf
    }
    
    
    
    dstar<- sum(a*(-rho*wmat +h))
    return(list(a,dstar,lambda ))
  }
  firm_estim<-Vectorize(firm_estim)
  
  res<-mclapply(1:nrow(x), function(y){firm_estim(gamma_vec[y], x[y,2], x[y,3], x[y,4],x[y,5],x[y,9]
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
  holder[, Q:=xi+u_bar+x[,2:5]%*%umat+ny_objects$nu]
  holder[, C:=x[,9]*(W + gamma_vec*Imodel) + x[,2:5]%*%cmat1 +c_bar+ny_objects$phi]
  
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
  check_labor<-puzzle[, .(L1=sum(V1), L2=sum(V2), L3=sum(V3), L4=sum(V4), L5=sum(V5)) , by=V6 ]
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


counterfactual<-function(parms,gamma_vec,wages, x){
  rho<-exp(parms[1])*tau
  u_bar<-parms[2]
  c_bar<-parms[3]
  w1<-wages
  cmat1<-parms[9:12]
  umat<-parms[13:16]
  smat<-parms[17:21]
  
  
  wmat<-matrix(w1, nrow=5, ncol=5, byrow=FALSE)
  h<-diag(smat)
  
  ## this function will return matrix given gamma
  firm_estim<-function(gamma,ap2, ap3, ap4,ap5, abar){
    aleph<-c(1-ap2-ap3-ap4-ap5, ap2, ap3, ap4, ap5)
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
      E<-rep(1/5,5)
      E<-squarem(E,fixptfn = fxpt, control=list(maxiter=100000,tol=innertol) )$par
      a<-t(t(A*E)/colSums(A*E)*aleph)
    } else{
      if (gamma==0){
        # this is when the firm is frictionless, in which case they use water filling method.
        a<-matrix(0, ncol=5, nrow=5)
        for (col in 1:5){
          a[which.min((wmat -h/rho/abar)[,col]),col]<-aleph[col]
        }
        E<-rowSums(a)
        lambda<-Inf
      } else{
        # this is for firms with unidentified high gamma.
        # assume their gamma is still high enough s.t. they choose least complex structure.
        ahelper<-t(matrix(aleph,length(aleph), 5))
        E<-rep(0,5)
        E[which.min(rowSums((wmat -h/rho/abar)*ahelper))[1]]<-1
        a<-E*ahelper
        lambda<-0
      }
    }
    dstar<- sum(a*(-rho*wmat +h))
    return(list(a,dstar,lambda ))
  }
  firm_estim<-Vectorize(firm_estim)
  
  res<-mclapply(1:nrow(x), function(y){firm_estim(gamma_vec[y], x[y,2], x[y,3], x[y,4],x[y,5],x[y,9]
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
  holder[, Q:=xi+u_bar+x[,2:5]%*%umat+ny_objects$nu]
  holder[, C:=x[,9]*(W + gamma_vec*Imodel) + x[,2:5]%*%cmat1 +c_bar+ny_objects$phi]
  
  ### given optimal orgs, keep price same as old.
  holder[, newprice:=cust_price, by=quarter_year]
  
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
  check_labor<-puzzle[, .(L1=sum(V1), L2=sum(V2), L3=sum(V3), L4=sum(V4), L5=sum(V5)) , by=V6 ]
  stopifnot(nrow(check_labor)==1)
  
  return(list("mainstats"=holder[,c("location_id", "newprice","cust_price","new_share","salon_share_subdiv","Imodel","s_index", "Q", "C")],
              "bmats"=bmats))
  
}

############### Independent contractor laws.
gamma_vec<-ny_objects$gammas

quad_wages<-optim(as.numeric(oldwages),quad_solver,method="L-BFGS-B", lower=rep(15,5), upper=rep(Inf, 5), control=list(trace=1))
counter=1
while (quad_wages$value>0.01 & counter<=10){
  quad_wages<-optim(quad_wages$par,quad_solver, control=list(trace=1))
  if (quad_wages$value>0.01) quad_wages<-optim(quad_wages$par,quad_solver,method="L-BFGS-B", lower=rep(15,5), upper=rep(Inf, 5), control=list(trace=1))
  counter<-counter+1
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
                                          markup=mean(1/(1-out$mainstats$new_share)/exp(coef(res_store)[1])/tau),
                                          pct_markup=mean(1/(1-out$mainstats$new_share)/exp(coef(res_store)[1])/tau/out$mainstats$C),
                                          total_profit=sum(estim_sample$weight[1]*estim_sample$CSPOP[1]*out$mainstats$new_share/(1-out$mainstats$new_share)/exp(coef(res_store)[1])/tau),
                                          avg_profit=mean(estim_sample$CSPOP[1]*out$mainstats$new_share/(1-out$mainstats$new_share)/exp(coef(res_store)[1])/tau),
                                          sd_profit=sd(estim_sample$CSPOP[1]*out$mainstats$new_share/(1-out$mainstats$new_share)/exp(coef(res_store)[1])/tau),
                                          consumer_welfare=estim_sample$CSPOP[1]/exp(coef(res_store)[1])*(logSumExp(c(out$mainstats$Q-tau*exp(coef(res_store)[1])*out$mainstats$newprice,-log(estim_sample$weight[1])))+log(estim_sample$weight[1])-digamma(1)),
                                          quality=mean(out$mainstats$Q),
                                          w1=quad_wages$par[1],
                                          w2=quad_wages$par[2],
                                          w3=quad_wages$par[3],
                                          w4=quad_wages$par[4],
                                          w5=quad_wages$par[5],
                                          obj_value=quad_wages$value
))

names<-"Improvement in Management Software"
counter_objects[[names]]<-out
save(counter_res,counter_objects, file="data/03_00_counterfactual.RData")


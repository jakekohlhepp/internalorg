## estimate model.
library('data.table')
library('lamW')
library('gmm')
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

############### import results
load('data/02_01_est_res_ny.RData')
###### create objects
tau<-1.045


if (get_os()=="windows"){
  core_count<-1
} else{
  core_count<-32
} 


innertol<-1e-04
outertol<-1e-04
wage_bound<-Vectorize(function(x){
  return(195/(exp(-x)+1)+5)
})
source('big_gmm_objective.R')

###temp chunk
load('data/02_01_progress.RData')
maxrefine<-res_store
ny_objects<-ret_objects(coef(maxrefine), estim_matrix)
###############


#### Prep Data
# weight is just scaled up share.
estim_sample[, weight:=(1-outside_share)/(sum(salon_share_subdiv)), by=c("COUSUB","quarter_year")]

## Compute amount fo each type.
# this is row sum of b, times total duration times weight. divide by 60 to get in hours.
puzzle<-data.table()

for (estab in 1:nrow(estim_sample)){
  piece<-rowSums(ny_objects$bmats[[estab]])*estim_sample$salon_share_subdiv[estab]*estim_sample$avg_labor[estab]*estim_sample$weight[estab]*estim_sample$CSPOP[estab]
  piece<-c( piece,estim_sample[estab,]$quarter_year)
  puzzle<-rbind(puzzle, t(piece))
}
tot_labor<-puzzle[, .(L1=sum(V1), L2=sum(V2), L3=sum(V3), L4=sum(V4), L5=sum(V5), L6=sum(V6)) , by=V7 ]
setnames(tot_labor, "V7", "quarter_year")

### create function which solves game.
# as test divide all gammas by half.

newgamma<-ny_objects$gammas
wages<-coef(maxrefine)[4:27]



parms<-coef(maxrefine)
gamma_vec<-ny_objects$gammas
gamma_vec<-ny_objects$gammas*0.8
x<-estim_matrix





### now big  - BE CAREFUL - THIS FUNCTION USES BOTH ESTIM_MATRIX AND ESTIM_SAMPLE
solve_game<-function(parms,gamma_vec,wages, x){
  rho<-parms[1]*tau
  u_bar<-parms[2]
  c_bar<-parms[3]
  w1<-wage_bound(wages[1:6])
  w2<-wage_bound(wages[7:12])
  w3<-wage_bound(wages[13:18])
  w4<-wage_bound(wages[19:24])
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
  
  ## this function will return matrix given gamma
  firm_estim<-function(gamma,ap2, ap3, ap4,ap5, ap6,qts, abar){
    aleph<-c(1-ap2-ap3-ap4-ap5-ap6, ap2, ap3, ap4, ap5, ap6)
    if (gamma<1000000000 & !is.na(gamma)){
      lambda<-1/gamma
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
    } else{# this is for firms with unidentified high gamma.
      # assume their gamma is still high enough s.t. they choose least complex structure.
      ahelper<-t(matrix(aleph,length(aleph), 6))
      E<-rep(0,6)
      E[which.min(rowSums((wmat[[qts]] -h[[qts]]/rho/abar)*ahelper))[1]]<-1
      a<-E*ahelper
      a<-(round(E,7)>0)*a
      E<-round(E,7)
      lambda<-0
    }
    dstar<- sum(a*(-rho*wmat[[qts]] +h[[qts]]))
    return(list(a,dstar,lambda ))
  }
  firm_estim<-Vectorize(firm_estim)
  
  res<-mclapply(1:nrow(x), function(y){firm_estim(gamma_vec[y], x[y,2], x[y,3], x[y,4],x[y,5],x[y,6],
                                                  ifelse(max(x[y,10:12])==1,which.max(x[y,10:12]),4),x[y,14]
  )}, mc.cores=core_count)
  
  # helper functions
  finda<-Vectorize(function(x){res[x][[1]][1]})
  findW<-Vectorize(function(y){sum(wmat[[ifelse(max(x[y,10:12])==1,which.max(x[y,10:12]),4)]]*res[y][[1]][[1]])})
  findqual<-Vectorize(function(y){sum(finda(y)[[1]]*(h[[ifelse(max(x[y,10:12])==1,which.max(x[y,10:12]),4)]])) })
  findI<-Vectorize(function(y){
    a<-res[[y]][[1]]
    return(sum(a*spec_log(t(t(a/rowSums(a))/colSums(a)))))
  })

  
  ### new to store matrix of all objects.
  holder<-copy(estim_sample)
  holder[,W:=findW(1:nrow(x))]
  holder[,xi:=findqual(1:nrow(x))]
  holder[,Imodel:=findI(1:nrow(x))]
  holder[, Q:=xi+u_bar+ut1*x[,10]+ut2*x[,11]+ut3*x[,12]+x[,2:6]%*%umat+ny_objects$nu]
  holder[, C:=x[,14]*(W + gamma_vec*Imodel) + (1-rowSums(x[,10:12]))*x[,2:6]%*%cmat1+x[,10]*x[,2:6]%*%cmat2+x[,11]*x[,2:6]%*%cmat3+x[,12]*x[,2:6]%*%cmat4 +c_bar+ct1*x[,10]+ct2*x[,11]+ct3*x[,12]+ny_objects$phi]

  ### given optimal orgs, solve for price

  best_respond<-function(p0, Q,C, wgt){
    old_p<-p0
    for (i in 1:10000000){
      new_p<-sapply(1:length(old_p), 
                    function(y){ 
                      1/rho+C[y]+lambertW0(exp(-1+Q[y]-rho*C[y])/(1+sum(wgt*exp(Q-rho*old_p))-exp(Q[y]-rho*old_p[y])) )/rho 
                    })
      if (all(abs(new_p-old_p)<1e-14)) break
      old_p<-new_p
    }
    return(new_p)
  }
  holder[, newprice:=best_respond(cust_price,Q=Q, C=C, wgt=weight), by=quarter_year]
  
  #holder[,newprice:=bobyqa(cust_price,function(pvec){
  #  out<-sapply(1:length(pvec), 
  #              function(y){ 
  #                  1/rho+C[y]+lambertW0(exp(-1+Q[y]-rho*C[y])/(1+sum(exp(Q-rho*pvec))-exp(Q[y]-rho*pvec[y])) )/rho-pvec[y] 
  #              })
  #  return(sum(out^2))
  #}, control=list(maxeval=100000,ftol_abs=1e-08,xtol_rel=-Inf, ftol_rel=-Inf))$par, by=quarter_year]
  #
  #
  #holder[,check:=sapply(newprice,function(pvec){
  #  out<-sapply(1:length(pvec), 
  #              function(y){ 
  #                1/rho+C[y]+lambertW0(exp(-1+Q[y]-rho*C[y])/(1+sum(exp(Q-rho*pvec))-exp(Q[y]-rho*pvec[y])) )/rho-pvec[y] 
  #              })
  #  return(out)
  #}), by=quarter_year]
  
  
  
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
  return(tot_labor[,-1]-check_labor[,-1]) # negative means excess demand, positive means excess supply.
  # so negative means we need higher wages.
}
solve_helper<-function(x, q,t,latent){
  wage_new<-latent
  wage_new[(q-1)*6+t]<-x
  output<-solve_game(parms=coef(maxrefine), gamma_vec=gamma_vec,wages=wage_new, x=estim_matrix)
  return(as.numeric(output[q,..t]))
}

### Find wages. do one quarter, one type at a time
return_excess<-function(w){
  return(solve_game(parms=coef(maxrefine), gamma_vec=gamma_vec,wages=w, x=estim_matrix))
}
wage_new<-wages
for (q  in 3:4){
t<-1
while (t<7){
  tryit<-uniroot(solve_helper,lower=-10,upper=10, q=q, t=t, latent=wage_new)
  wage_new[(q-1)*6+t]<-tryit$root
  print(paste(q,t))
  if (t==6 ) {
    check<-return_excess(wage_new)
    print(check)
    if (any(abs(as.numeric(check[q,]))>20)) t<-0
  }
  t<-t+1
}
}

quad_solver<-function(w){
  return(sum(solve_game(parms=coef(maxrefine), gamma_vec=gamma_vec,wages=w, x=estim_matrix)^2))
}

quad_wages<-optim(wage_new,quad_solver, method="BFGS", control=list(trace=1,maxit=100))

quad_wages<-optim(quad_wages$par,quad_solver, control=list(trace=1,maxit=100))

library('nloptr')

new_wages<-bobyqa(wages,solve_helper,
             control=list(maxeval=1000, xtol_rel=1e-12, xtol_abs=1e-12, trace=1))

wage_new<-optim(wages,solve_helper, method="BFGS", control=list(trace=1,maxit=30))

solve_helper<-function(w){
  return(as.numeric(unlist(solve_game(parms=coef(maxrefine), gamma_vec=gamma_vec,wages=w, x=estim_matrix))))
}




wage_new<-multiroot(solve_helper, start=wages+0.5)






#zero_game<-function(x){
#  return(c(as.matrix(solve_game(parms=coef(maxrefine), gamma_vec=newgamma,wages=x, x=estim_matrix))))
#}
#
## Counterfactual 0: equilibrium when all firms have maximum frictions (gamma=Inf)




## Counterfactual 1: Convergence of all gammas to lowest observed gamma


## Counterfactual 2: Remove of all organizational frictions


## Counterfactual 3: Sales tax increase from 4 to 7.6%

















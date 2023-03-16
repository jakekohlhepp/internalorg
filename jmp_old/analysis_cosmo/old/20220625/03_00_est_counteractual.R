## estimate model.
library('data.table')
library('parallel')
library('lamW')
#library('nloptr')
set.seed(5777)




############### import results
load('data/02_00_est_res_la.RData')
###### create objects
outertol<-1e-06
innertol<-1e-06
tau<-1.045
core_count<-120
source('main_gmm_objective.R')
model_objects<-ret_objects(coef(maxrefine), estim_matrix)
###############




# weight is just scaled up share.
estim_sample[, weight:=(1-outside_share)/(sum(salon_share_subdiv)), by=COUSUB]
## compute the amount of labor. this is population, times avg labor, times share, times weight, times 
puzzle<-data.table()

for (estab in 1:nrow(estim_sample)){
  piece<-rowSums(model_objects$bmats[[estab]])*estim_sample$avg_labor[estab]*estim_sample$salon_share_subdiv[estab]*estim_sample$weight[estab]*estim_sample$COUSUB[estab]
  piece<-c( piece,estim_sample[estab,]$quarter_year)
  puzzle<-rbind(puzzle, t(piece))
}
tot_labor<-puzzle[, .(L1=sum(V1), L2=sum(V2), L3=sum(V3), L4=sum(V4), L5=sum(V5), l6=sum(V6)) , by=V7 ]
setnames(tot_labor, "V7", "quarter_year")

### create function which solves game.

newgamma<-model_objects$gammas/2
wages<-coef(maxrefine)[4:27]

## test just org structures.
#solve_org<-function(parms,gamma_vec,wages, x){
#  rho<-parms[1]*tau
#  u_bar<-parms[2]
#  c_bar<-parms[3]
#  w1<-c(wages[1],wages[2],wages[3],wages[4], wages[5], wages[6])
#  w2<-c(wages[7],wages[8],wages[9],wages[10], wages[11], wages[12])
#  w3<-c(wages[13],wages[14],wages[15],wages[16],wages[17],wages[18])
#  w4<-c(wages[19],wages[20],wages[21],wages[22],wages[23],wages[24] )
#  ut1<-parms[28]
#  ut2<-parms[29]
#  ut3<-parms[30]
#  ct1<-parms[31]
#  ct2<-parms[32]
#  ct3<-parms[33]
#  s1<-parms[34]
#  s2<-parms[35]
#  s3<-parms[36]
#  s4<-parms[37]
#  s5<-parms[38]
#  s6<-parms[39]
#  m1<-0
#  m2<-0
#  m3<-0
#  m4<-0
#  m5<-0
#  m6<-0
#  cmat<-c(parms[40],parms[41],parms[42],parms[43], parms[44])
#  umat<-c(parms[45], parms[46], parms[47], parms[48], parms[49])
#  mcoef<-parms[50]
#  childcoef<-parms[51]
#  
#  wmat<-list(matrix(w1, nrow=6, ncol=6, byrow=FALSE),
#             matrix(w2, nrow=6, ncol=6, byrow=FALSE),
#             matrix(w3, nrow=6, ncol=6, byrow=FALSE),
#             matrix(w4, nrow=6, ncol=6, byrow=FALSE))
#  h<-rbind(c(s1,m2,m3,m4,m5,m6),c(m1,s2,m3,m4,m5,m6), c(m1,m2,s3,m4,m5,m6), c(m1,m2,m3,s4,m5,m6),
#           c(m1,m2,m3,m4,s5,m6), c(m1,m2,m3,m4,m5,s6))
#  
#  ## this function will return matrix given gamma
#  firm_estim<-function(gamma,ap2, ap3, ap4,ap5, ap6,qts){
#    aleph<-c(1-ap2-ap3-ap4-ap5-ap6, ap2, ap3, ap4, ap5, ap6)
#    if (gamma<10000){
#      lambda<-1/gamma
#      A<-exp(-lambda*(wmat[[qts]] -h/rho) )
#      A[A>=Inf]<-1e8
#      fxpt<-function(p,y){
#        C<-colSums(t(A)*aleph/colSums(A*p))
#        return(p*C)
#      }
#      objval<-function(p,y){
#        C<-colSums(t(A)*aleph/colSums(A*p))
#        return(sum(p*spec_log(C), na.rm=TRUE)-max(spec_log(C), na.rm=TRUE))
#      }
#      E<-rep(1/(6),6)
#      valold<-0
#      val<-1
#      icount<-0
#      while (abs(val-valold)>innertol & icount<100000){
#        E<-fxpt(E)
#        valold<-val
#        val<-objval(E)
#        icount<-icount+1
#      }
#      a<-t(t(A*E)/colSums(A*E)*aleph)
#    } else{# this is for firms with gamma=0. assume they remain this way.
#      ahelper<-t(matrix(aleph,length(aleph), 6))
#      E<-rep(0,6)
#      E[which.min(rowSums((wmat[[qts]] -h/rho)*ahelper))[1]]<-1
#      a<-E*ahelper
#      lambda<-1/1000000000
#    }
#    dstar<- sum(a*(-rho*wmat[[qts]] +h))
#    return(list(a,dstar,lambda ))
#  }
#  firm_estim<-Vectorize(firm_estim)
#  
#  res<-mclapply(1:nrow(x), function(y){firm_estim(gamma_vec[y], x[y,2], x[y,3], x[y,4],x[y,5],x[y,6],
#                                                  ifelse(max(x[y,10:12])==1,which.max(x[y,10:12]),4)
#  )}, mc.cores=core_count)
#  
#  # helper functions
#  finda<-Vectorize(function(x){res[x][[1]][1]})
#  findW<-Vectorize(function(y){sum(wmat[[ifelse(max(x[y,10:12])==1,which.max(x[y,10:12]),4)]]*res[y][[1]][[1]])})
#  findqual<-Vectorize(function(x){sum(finda(x)[[1]]*(h)) })
#  
#  return(list("wagebills"=findW(1:nrow(x)),
#              "quals"=findqual(1:nrow(x)),
#              "bmats"=finda(1:nrow(x))))
#}
#res<-solve_org(coef(maxrefine), newgamma,wages,estim_matrix )


parms<-coef(maxrefine)
gamma_vec<-model_objects$gammas
x<-estim_matrix
### now big function
solve_game<-function(parms,gamma_vec,wages, x){
  rho<-parms[1]*tau
  u_bar<-parms[2]
  c_bar<-parms[3]
  w1<-c(wages[1],wages[2],wages[3],wages[4], wages[5], wages[6])
  w2<-c(wages[7],wages[8],wages[9],wages[10], wages[11], wages[12])
  w3<-c(wages[13],wages[14],wages[15],wages[16],wages[17],wages[18])
  w4<-c(wages[19],wages[20],wages[21],wages[22],wages[23],wages[24] )
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
  
  #### solve for optimal org structures.
  wmat<-list(matrix(w1, nrow=6, ncol=6, byrow=FALSE),
             matrix(w2, nrow=6, ncol=6, byrow=FALSE),
             matrix(w3, nrow=6, ncol=6, byrow=FALSE),
             matrix(w4, nrow=6, ncol=6, byrow=FALSE))
  h<-rbind(c(s1,m2,m3,m4,m5,m6),c(m1,s2,m3,m4,m5,m6), c(m1,m2,s3,m4,m5,m6), c(m1,m2,m3,s4,m5,m6),
           c(m1,m2,m3,m4,s5,m6), c(m1,m2,m3,m4,m5,s6))
  
  ## this function will return matrix given gamma
  firm_estim<-function(gamma,ap2, ap3, ap4,ap5, ap6,qts){
    aleph<-c(1-ap2-ap3-ap4-ap5-ap6, ap2, ap3, ap4, ap5, ap6)
    if (gamma<10000){
      lambda<-1/gamma
      A<-exp(-lambda*(wmat[[qts]] -h/rho) )
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
    } else{# this is for firms with gamma=0. assume they remain this way.
      ahelper<-t(matrix(aleph,length(aleph), 6))
      E<-rep(0,6)
      E[which.min(rowSums((wmat[[qts]] -h/rho)*ahelper))[1]]<-1
      a<-E*ahelper
      lambda<-1/1000000000
    }
    dstar<- sum(a*(-rho*wmat[[qts]] +h))
    return(list(a,dstar,lambda ))
  }
  firm_estim<-Vectorize(firm_estim)
  
  res<-mclapply(1:nrow(x), function(y){firm_estim(gamma_vec[y], x[y,2], x[y,3], x[y,4],x[y,5],x[y,6],
                                                  ifelse(max(x[y,10:12])==1,which.max(x[y,10:12]),4)
  )}, mc.cores=core_count)
  
  # helper functions
  finda<-Vectorize(function(x){res[x][[1]][1]})
  findW<-Vectorize(function(y){sum(wmat[[ifelse(max(x[y,10:12])==1,which.max(x[y,10:12]),4)]]*res[y][[1]][[1]])})
  findqual<-Vectorize(function(x){sum(finda(x)[[1]]*(h)) })
  findI<-Vectorize(function(y){
    a<-res[[y]][[1]]
    return(sum(a*spec_log(t(t(a/rowSums(a))/colSums(a)))))
  })
  W<-findW(1:nrow(x))
  xi<-findqual(1:nrow(x))
  I<-findI(1:nrow(x))
  Q<-xi+u_bar+ut1*x[,10]+ut2*x[,11]+ut3*x[,12]+x[,2:6]%*%umat+model_objects$nu+x[,14]*mcoef+x[,15]*childcoef
  C<-(W+I*gamma_vec+x[,2:6]%*%cmat)+c_bar+ct1*x[,10]+ct2*x[,11]+ct3*x[,12]+model_objects$phi
  bmats<-finda(1:nrow(x))
  ### given optimal orgs, solve for price
  # function which given prices computes distance.
  solve_price<-function(x){
      return(sum(sapply(1:length(x), function(y){ 1/rho+C[y]+lambertW0(exp(-1+Q[y]-rho*C[y])/(sum(exp(Q-rho*C))-exp(Q[y]-rho*C[y])) )/rho-x[y] })^2))
  }
  

  # find using nonlinear optimization. then check
  #prices<-bobyqa(as.numeric(estim_matrix[,7]),solve_price, upper=rep(Inf, nrow(estim_matrix)), lower=rep(0, nrow(estim_matrix)),
  #                   control=list(maxeval=100000, xtol_rel=1e-12, xtol_abs=1e-12))$par
  prices<-optim(as.numeric(estim_matrix[,7]),solve_price, upper=Inf, lower=0,
                     control=list(maxeval=100000, xtol_rel=1e-12, xtol_abs=1e-12))$par
  check_price<-function(x){
    return(sapply(1:length(x), function(y){ 1/rho+C[y]+lambertW0(exp(-1+Q[y]-rho*C[y])/(sum(exp(Q-rho*C))-exp(Q[y]-rho*C[y])) )/rho-x[y] }))
  }
  #stopifnot( all(round(check_price(prices), 9)==0))
  
  ### compute new shares.
  salon_share<-exp(Q-rho*prices)
  salon_share<-salon_share/sum(salon_share)
  
  puzzle<-data.table()
  for (estab in 1:nrow(estim_sample)){
    piece<-rowSums(bmats[[estab]])*estim_sample$avg_labor[estab]*salon_share[estab]*estim_sample$weight[estab]*estim_sample$COUSUB[estab]
    piece<-c( piece,estim_sample[estab,]$quarter_year)
    puzzle<-rbind(puzzle, t(piece))
  }
  check_labor<-puzzle[, .(L1=sum(V1), L2=sum(V2), L3=sum(V3), L4=sum(V4), L5=sum(V5), l6=sum(V6)) , by=V7 ]
  return(check_labor[,-1]-tot_labor[,-1])
}

solve_helper<-function(x){
  return(sum(solve_game(parms=coef(maxrefine), gamma_vec=model_objects$gammas,wages=x, x=estim_matrix)^2))
}




zero_game<-function(x){
  return(c(as.matrix(solve_game(parms=coef(maxrefine), gamma_vec=model_objects$gammas,wages=x, x=estim_matrix))))
}

#test<-function(x){return(c(x[1]^2+x[2],x[2]))}
#multiroot(f=zero_game,start=wages)->res

# find using nonlinear optimization. then check
#wage_new<-bobyqa(wage_new$par,solve_helper, upper=rep(Inf, length(wages)), lower=rep(0, length(wages)),
#               control=list(maxeval=10000, xtol_rel=1e-12, xtol_abs=1e-12))


wage_new<-optim(wages,solve_helper, upper=Inf, lower=0, method="Nelder-Mead", control=list(trace=1,maxeval=2))

#check_wages<-function(x){
#  return(solve_game(parms=coef(maxrefine), gamma_vec=model_objects$gammas,wages=x, x=estim_matrix))
#}
#stopifnot( all(round(check_wages(wage_new$par), 11)==0))





## Counterfactual 0: equilibrium when all firms have maximum frictions (gamma=Inf)




## Counterfactual 1: Convergence of all gammas to lowest observed gamma


## Counterfactual 2: Remove of all organizational frictions


## Counterfactual 3: Sales tax increase from 4 to 7.6%

















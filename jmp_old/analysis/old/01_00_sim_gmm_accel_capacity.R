library('data.table')
library('lessR') # for the to() command
library('EnvStats') # for extreme value
library('SQUAREM') # accelerate fixed point.
library('gmm')
spec_log<-function(x){
  ifelse(x==0 | x==-Inf | is.nan(x),0,log(x))
}

entropy<-function(x){
  return(sum(-x*spec_log(x)))
}

## create simulated data
set.seed(10012)


N<-2 ## if this is changed we need to change whole program.
T<-4
firm_count<-1000
starting<-rep(1/(T*N), T*N)
c_0<-c(1,1,1,1)
rho_0<-1
delta_0 <-0.5
beta_0<-c(1,2,3,4)
s_0<-0.2
theta_grid<-c(0.01, 2)

  
alpha<-cbind(exp(rnorm(firm_count)),exp(rnorm(firm_count)),exp(rnorm(firm_count)),exp(rnorm(firm_count)) )
alpha<-alpha/rowSums(alpha)
I<-sapply(1:firm_count,function(x){ runif(1,min=0, max=0.6*entropy(alpha[x,]))})
entrop<-sapply(1:firm_count,function(x){ entropy(alpha[x,])})

tot_data<-data.table()
# only see 5% of firms
firms_see<-sample(1:firm_count,0.2*firm_count, replace=FALSE)
market_groups<-data.table(firm_id=1:firm_count, market=seq(from=1, to=firm_count, length=1))

for (t in 1){
  firm_essentials<-data.table(market=seq(from=1, to=firm_count, length=1),
                              firm_id=1:firm_count,smax=entrop,sindex=I,
                              task_mix1=alpha[,1],task_mix2=alpha[,2],task_mix3=alpha[,3],task_mix4=alpha[,4],
                              rent_shock=exp(rnorm(firm_count, mean=-3)),
                              q_shock=rnorm(firm_count, mean=0, sd=0.2))
  firm_essentials[, c_shock:=exp(rnorm(firm_count, mean=-3))]
  # this iter vars
  delta<-delta_0
  rho<-rho_0
  beta<-beta_0
  c<-c_0
  
  #0.9972808
  # firms cost-minimize - for each wage guess they solve b-a algorithm
  w_base<-0
  w<-c(0.0005, 0.0005, 0.0005, 0.0005,
      1.985, 1.985, 1.985, 1.985)+w_base
  #w<-c(0.1, 0.0005, 0.0005, 0.0005,
  #    2.08, 1.985, 1.985, 1.985)+w_base
  thetamat<-matrix(as.vector(sapply(1:N, function(x) {rep(theta_grid[x], length=T)}, simplify=TRUE)), ncol=T, nrow=N*T)
  wmat<-matrix(w, nrow=N*T, ncol=T, byrow=FALSE)
  
  colMax <- function(data) apply(data,2, max, na.rm = TRUE)
  colMin <- function(data) apply(data,2, min, na.rm = TRUE)
  spec_log<-function(x){
    ifelse(x==0 | x==-Inf | is.nan(x),0,log(x))
  }
  h<-matrix(1, ncol=T, nrow=N*T)-  do.call(rbind,rep(list(diag(1,nrow=T, T)),N)) 
  
  ## set tolerances
  innertol<-1e-04
  outertol<-1e-04
  firm_estim<-function(sindex, ap1, ap2, ap3, ap4){
    aleph<-c(ap1, ap2, ap3, ap4)
    findlambda<-function(lambda){
      A<-exp(-lambda*(rho*wmat -thetamat*delta^h) )
      fxpt<-function(p,y){
        C<-colSums(t(A)*aleph/colSums(A*p))
        return(p*C)
      }
      objval<-function(p,y){
        C<-colSums(t(A)*aleph/colSums(A*p))
        return(sum(p*spec_log(C), na.rm=TRUE)-max(spec_log(C), na.rm=TRUE))
      }
      E0<-rep(1/(N*T),N*T)
      pf<-squarem(p=E0, y=1, fixptfn=fxpt, objfn=objval, control=list(tol=innertol))
      stopifnot(pf$convergence)
      E<-pf$par
      E[is.nan(E)] <- 0
      a<-t(t(A*E)/colSums(A*E)*aleph)
      a[is.nan(a)] <- 0
      E<-round(E,5)
      a<-(round(E,5)>0)*a
      I<-sum(a*spec_log(t(t(a/E)/aleph)))
      diff<-sindex-I
      return(diff)
    }
    lambda<-tryCatch({uniroot(findlambda, lower=0, upper=500, tol=outertol)$root},
                     error = function(e){
                       if (findlambda(500)>0 & findlambda(0)>0 ){
                         return(500)
                         } else if (findlambda(0)<=0 & findlambda(500)<=0) {
                           return(0)
                         } else if (findlambda(500)) {
                           return(NA)
                         }
                       }) # when there is no root in range, it means constraint does not bind.
    # then we put in a large value for lambda.
    A<-exp(-lambda*(rho*wmat -thetamat*delta^h) )
    fxpt<-function(p,y){
      C<-colSums(t(A)*aleph/colSums(A*p))
      return(p*C)
    }
    objval<-function(p,y){
      C<-colSums(t(A)*aleph/colSums(A*p))
      return(sum(p*spec_log(C), na.rm=TRUE)-max(spec_log(C), na.rm=TRUE))
    }
    E0<-rep(1/(N*T),N*T)
    pf<-squarem(p=E0, y=1, fixptfn=fxpt, objfn=objval, control=list(tol=innertol))
    
    stopifnot(pf$convergence)
    E<-pf$par
    a<-t(t(A*E)/colSums(A*E)*aleph)
    E<-round(E,3)
    a<-(round(E,3)>0)*a
    b<-a/E
    dstar<- sum(a*(-rho*wmat +thetamat*delta^h))
    return(list(a,dstar,lambda ))
  }
  
  #qual(firm_estim(1, alpha[2,]))
  firm_estim<-Vectorize(firm_estim)
  res<-lapply(1:firm_count, function(x){firm_estim(firm_essentials[x,]$sindex, 
                                            firm_essentials[x,]$task_mix1,firm_essentials[x,]$task_mix2,
                                            firm_essentials[x,]$task_mix3, firm_essentials[x,]$task_mix4 )})
  check<-sapply(res, length) # should all be length 2
  #print(paste("Firms Not Able to Optimize:",sum(check<2) ))
  stopifnot(sum(check<2)==0)
  
  findd<-Vectorize(function(x){tryCatch({res[x][[1]][[2]]},error=function(e){NA})})
  finda<-Vectorize(function(x){res[x][[1]][1]})
  findqual<-Vectorize(function(x){sum(finda(x)[[1]]*(thetamat*delta^h)) })
  findprob<-Vectorize(function(x){sum(finda(x)[[1]]*(thetamat*delta^h >findqual(x) )) })
  findavgdelta<-Vectorize(function(x){sum(finda(x)[[1]]*(delta^h)) })
  ret_lambda<-Vectorize(function(x){res[x][[1]][[3]] })
  # given optimal strategies, what are quantities demanded of each type?
  firm_essentials[, top:=exp(q_shock+task_mix1*beta[1] +task_mix2*beta[2]+task_mix3*beta[3]+task_mix4*beta[4]+
                             +findd(firm_id)-
                               rho*c_shock -
                               rho*(task_mix1*c[1] +task_mix2*c[2]+task_mix3*c[3]+task_mix4*c[4] ))]
  firm_essentials[,share:=top/(1+sum(top)), by="market"]
  type_mat<-data.table(t(sapply(1:firm_count,function(x){ rowSums(finda(x)[[1]] )})))
  names(type_mat)<-to("e_", 8)
  
  type_shares<-rowSums(sapply(1:firm_count,function(x){ rowSums(finda(x)[[1]]*as.numeric(firm_essentials$share[x]) )}))

  
  # observed data: shares, prices, alpha, max, sum of characteristics
  firm_essentials[,wagebill:=sapply(1:firm_count,function(x){ sum(finda(x)[[1]]*w )})]
  firm_essentials[,qual:=findqual(1:firm_count)]
  firm_essentials[, mc:=wagebill+task_mix1*c[1] +task_mix2*c[2]+task_mix3*c[3]+task_mix4*c[4] +c_shock]
  firm_essentials[,price:=mc+1/rho_0]
  firm_essentials[,s_0:=1-sum(share),by="market"]
  firm_essentials[,det_util:=q_shock+qual-rho_0*price+task_mix1*beta[1] +task_mix2*beta[2]+task_mix3*beta[3]+task_mix4*beta[4]]
  firm_essentials[,probqual:=findprob(1:firm_count)]
  firm_essentials[,avgdelta:=findavgdelta(1:firm_count)]
  firm_essentials[,lambda:=ret_lambda(1:firm_count)]
  firm_essentials<-cbind(firm_essentials, type_mat)
  # for each firm create fake ratings data.
  # consumers report whether their haircut met expectations. that is if it is above or below xi.
  # this is bernoulli draw where p is prob above xi
  rating_gen<-Vectorize(function(x){
    hold<-rbinom(30, 1,x)
    return(var(hold))
  })
  firm_essentials[, rat_var:=rating_gen(probqual)]
 
  observed_data<-data.table(firm_id=firms_see, 
                            share=firm_essentials[firms_see]$share,
                            alpha[firms_see,], 
                            price=firm_essentials[firms_see,]$prce,
                            sindex=firm_essentials[firms_see,]$sindex, 
                            market=market_groups[firms_see]$market,
                            quality=firm_essentials[firms_see]$qual,
                            s_0=firm_essentials[firms_see]$s_0,
                            rat_var=firm_essentials[firms_see]$rat_var,
                            rent=firm_essentials[firms_see]$rent_shock
                            )
  names(observed_data)[3:6]<-to("task_mix",4)
  # last few are max of b
  bmaxes<-data.table(t(sapply(firms_see, function(x){
    hold<-finda(x)[[1]]
    zero<-(round(rowSums(finda(x)[[1]]),3)==0)
    hold[zero,]<-0
    return(colMax(t(t(hold/rowSums(hold))/colSums(hold)) ))
  })) )  
  names(bmaxes)<-to("bmax", 4)
  bmines<-data.table(t(sapply(firms_see, function(x){
    hold<-finda(x)[[1]]
    zero<-(round(rowSums(finda(x)[[1]]),3)==0)
    hold[zero,]<-0
    return(colMin(t(t(hold/rowSums(hold))/colSums(hold)) ))
    })) )
  names(bmines)<-to("bmin", 4)
  observed_data<-cbind(observed_data,bmaxes,bmines)
  observed_data[,period:=t]
  observed_data[,mean_shock:=mean(firm_essentials$c_shock)]
  observed_data[, s_sum:=sum(task_mix1)+sum(task_mix2)+sum(task_mix3)-task_mix1-task_mix2-task_mix3]
  tot_data<-rbind(tot_data,observed_data)
}

## suppose we knew the share of each type hired y the firm. Then we should be able to estimate wages.
test<-lm(price~e_2+e_3+e_4+e_5+e_6+e_7+e_8,data=firm_essentials)
summary(test)



### now we try to estimate the data given a subset of firms.
# price and shares equations given us everything but wages and delta.
#qual_res<-lm(log(share)-log(s_0)~qual+task_mix1+task_mix2+task_mix3+price,data=firm_essentials) # should always work
qual_res<-tsls(log(share)-log(s_0)~price+task_mix1+task_mix2+task_mix3,~task_mix1+task_mix2+task_mix3+rent, data=observed_data)
qual_hat<-resid(qual_res)+coef(qual_res)[1]-1
summary(observed_data$quality-qual_hat)
task_data<-finda(firms_see)

save(tot_data, task_data, file="../analysis/data/01_00_sim_data.RData")





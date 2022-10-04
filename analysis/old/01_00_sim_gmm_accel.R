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
set.seed(1225)


N<-2 ## if this is changed we need to change whole program.
T<-4
firm_count<-1000
starting<-rep(1/(T*N), T*N)
c<-c(1,1,1,1)
rho<-1.01
delta <-0.5
beta<-c(1,1,1,1)
s_0<-0.2
theta_grid<-c(0.024, 2)

  
alpha<-cbind(exp(rnorm(firm_count,sd=0.2)),exp(rnorm(firm_count,sd=0.2)),exp(rnorm(firm_count,sd=0.2)),exp(rnorm(firm_count,sd=0.2)) )
#alpha<-cbind(rep(0.25, firm_count),rep(0.25, firm_count),rep(0.25, firm_count),rep(0.25, firm_count))
alpha<-alpha/rowSums(alpha)

tot_data<-data.table()
# only see 5% of firms
firms_see<-sample(1:firm_count,0.2*firm_count, replace=FALSE)
market_groups<-data.table(firm_id=1:firm_count, market=seq(from=1, to=firm_count, length=1))

for (t in 1){
  Imax<-sapply(1:firm_count,function(x){ entropy(alpha[x,])})
  firm_essentials<-data.table(market=seq(from=1, to=firm_count, length=1),
                              firm_id=1:firm_count,
                              gamma=runif(firm_count, min=0.001, max=0.035),
                              task_mix1=alpha[,1],task_mix2=alpha[,2],task_mix3=alpha[,3],task_mix4=alpha[,4],
                              rent_shock=exp(rnorm(firm_count, mean=-3)),
                              q_shock=exp(rnorm(firm_count, mean=0, sd=0.4)),
                              Imax=Imax)
  firm_essentials[, c_shock:=exp(rnorm(firm_count, mean=0,sd=0.4))]

  # firms cost-minimize - for each wage guess they solve b-a algorithm
  w<-c(0.01, 0.01, 0.01, 0.01,
      1.96, 1.96, 1.96, 1.96)
  thetamat<-matrix(as.vector(sapply(1:N, function(x) {rep(theta_grid[x], length=T)}, simplify=TRUE)), ncol=T, nrow=N*T)
  wmat<-matrix(w, nrow=N*T, ncol=T, byrow=FALSE)
  
  colMax <- function(data) apply(data,2, max, na.rm = TRUE)
  colMin <- function(data) apply(data,2, min, na.rm = TRUE)
  spec_log<-function(x){
    ifelse(x==0 | x==-Inf | is.nan(x),0,log(x))
  }
  h<-matrix(1, ncol=T, nrow=N*T)-  do.call(rbind,rep(list(diag(1,nrow=T, T)),N)) 
  
  ## set tolerances
  firm_estim<-function(gamma, ap1, ap2, ap3, ap4){
    aleph<-c(ap1, ap2, ap3, ap4)
    lambda<-1/gamma
    A<-exp(-lambda*(wmat -thetamat/rho*delta^h) )
    fxpt<-function(p,y){
      C<-colSums(t(A)*aleph/colSums(A*p))
      return(p*C)
    }
    objval<-function(p,y){
      C<-colSums(t(A)*aleph/colSums(A*p))
      return(sum(p*spec_log(C), na.rm=TRUE)-max(spec_log(C), na.rm=TRUE))
    }
    E0<-rep(1/(N*T),N*T)
    #, objfn=objval,
    pf<-squarem(p=E0, y=1, fixptfn=fxpt,objfn=objval,control=list(tol=innertol, method=meth_choose,K=kchoose,maxiter=100000))
    
    stopifnot(pf$convergence)
    E<-pf$par
    a<-t(t(A*E)/colSums(A*E)*aleph)
    E<-round(E,9)
    a<-(round(E,9)>0)*a
    dstar<- sum(a*(-rho*wmat +thetamat*delta^h))
    I<-sum(a*spec_log(t(t(a/rowSums(a))/colSums(a))))
    return(list(a,dstar,lambda,I ))
  }
  
  #qual(firm_estim(1, alpha[2,]))
  firm_estim<-Vectorize(firm_estim)
  res<-lapply(1:firm_count, function(x){firm_estim(firm_essentials[x,]$gamma, 
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
  ret_I<-Vectorize(function(x){res[x][[1]][[4]] })
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
  firm_essentials[,price:=mc+1/rho]
  firm_essentials[,s_0:=1-sum(share),by="market"]
  firm_essentials[,det_util:=q_shock+qual-rho*price+task_mix1*beta[1] +task_mix2*beta[2]+task_mix3*beta[3]+task_mix4*beta[4]]
  firm_essentials[,probqual:=findprob(1:firm_count)]
  firm_essentials[,avgdelta:=findavgdelta(1:firm_count)]
  firm_essentials[,lambda:=ret_lambda(1:firm_count)]
  firm_essentials[,sindex:=ret_I(1:firm_count)]
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
                            sindex=firm_essentials[firms_see]$sindex,
                            share=firm_essentials[firms_see]$share,
                            alpha[firms_see,], 
                            price=firm_essentials[firms_see,]$price,
                            market=market_groups[firms_see]$market,
                            quality=firm_essentials[firms_see]$qual,
                            s_0=firm_essentials[firms_see]$s_0,
                            rat_var=firm_essentials[firms_see]$rat_var,
                            rent=firm_essentials[firms_see]$rent_shock,
                            gcheck=firm_essentials[firms_see]$gamma,
                            Imax=firm_essentials[firms_see]$Imax
                            )
  names(observed_data)[4:7]<-to("task_mix",4)
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


## create 4 firms used for instruments.
# price will be alpha times c plus markup
alpha_extra<-cbind(exp(rnorm(4,0,sd=0.5)),exp(rnorm(4,sd=0.5)),exp(rnorm(4,sd=0.5)),exp(rnorm(4,sd=0.5)) )
alpha_extra<-alpha_extra/rowSums(alpha_extra)
c_extra<-c+exp(rnorm(4,1,sd=0.5))
#extra_prices<-alpha_extra%*%c_extra+exp(rnorm(4,1,sd=0.5))

# instrument is euclidean distance between 
#hdist<-function(z1,z2,z3,z4, num){
#  x<-c(z1, z2,z3,z4)
#  dist<-1-2^(-1/2)*(sum((sqrt(x)-sqrt(alpha_extra[num,]))^2))
#  return(dist*extra_prices[num])
#}
#hdist<-Vectorize(hdist)
#tot_data[,z1:=hdist(task_mix1,task_mix1,task_mix1,task_mix1,1) ]
#tot_data[,z2:=hdist(task_mix1,task_mix1,task_mix1,task_mix1,2) ]
#tot_data[,z3:=hdist(task_mix1,task_mix1,task_mix1,task_mix1,3) ]
#tot_data[,z4:=hdist(task_mix1,task_mix1,task_mix1,task_mix1,4) ]

# instrument task_mix times price in that city.
tot_data[,z1:=c_extra[1]  ]
tot_data[,z2:=c_extra[2] ]
tot_data[,z3:=c_extra[3] ]
tot_data[,z4:= c_extra[4]]


## suppose we knew the share of each type hired y the firm. Then we should be able to estimate wages.
test<-lm(price~e_2+e_3+e_4+e_5+e_6+e_7+e_8,data=firm_essentials)
summary(test)
task_data<-finda(firms_see)

save(tot_data, task_data, file="../analysis/data/01_00_sim_data.RData")

## diagnostic

summary(firm_essentials$sindex/firm_essentials$Imax)
type_shares




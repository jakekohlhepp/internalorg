library('data.table')
library('lessR') # for the to() command
library('EnvStats') # for extreme value

spec_log<-function(x){
  ifelse(x==0 | x==Inf | x==-Inf | is.nan(x),0,log(x))
}

entropy<-function(x){
  return(sum(-x*spec_log(x)))
}

## create simulated data
set.seed(10012)



N<-4
T<-4
firm_count<-1000
starting<-rep(1/(T*N), T*N)
c_0<-c(1,1,1,1)
rho_0<-1
delta_0 <-3
beta_0<-c(1,2,3,4)
s_0<-0.2
theta_grid<-seq(from=0, to=10, length=N)


alpha<-cbind(exp(rnorm(firm_count)),exp(rnorm(firm_count)),exp(rnorm(firm_count)),exp(rnorm(firm_count)) )
alpha<-alpha/rowSums(alpha)
I<-sapply(1:firm_count,function(x){ runif(1,min=0, max=0.9*entropy(alpha[x,]))})
entrop<-sapply(1:firm_count,function(x){ entropy(alpha[x,])})

tot_data<-data.table()
# only see 5% of firms
firms_see<-sample(1:firm_count,0.2*firm_count, replace=FALSE)
market_groups<-data.table(firm_id=1:firm_count, market=seq(from=1, to=firm_count, length=1))

for (t in 1){
  c_shock<-exp(rnorm(firm_count, mean=-2))
  firm_essentials<-data.table(market=seq(from=1, to=firm_count, length=1),
                              firm_id=1:firm_count,smax=entrop,sindex=I,
                              task_mix1=alpha[,1],task_mix2=alpha[,2],task_mix3=alpha[,3],task_mix4=alpha[,4])
  
  # this iter vars
  delta<-delta_0
  rho<-rho_0
  
  
  # firms cost-minimize - for each wage guess they solve b-a algorithm
  d<-c(1,1.1,1.1,1.4)*t
  m<-c(0.9,0.9,0.9,0.9)
  #w<- rep(m, N)*as.vector(sapply(1:N, function(x) {rep(theta_grid[x], length=T)}, simplify=TRUE))+rep(d, N)
  thetamat<-matrix(as.vector(sapply(1:N, function(x) {rep(theta_grid[x], length=T)}, simplify=TRUE)), ncol=T, nrow=N*T)
  w<-thetamat[,1]/rho*c(c(1,1, 1, 1), c(1,1, 1, 1), c(1,1, 1, 1), c(1,1, 1, 1)) +rep(d,N)#+c(rep(0, T), rep(0.001, T),rep(0.001, T),rep(0.001, T))#
  wmat<-matrix(w, nrow=N*T, ncol=T, byrow=FALSE)
  
  colMax <- function(data) apply(data,2, max, na.rm = TRUE)
  colMin <- function(data) apply(data,2, min, na.rm = TRUE)
  spec_log<-function(x){
    ifelse(x==0 | x==Inf | x==-Inf | is.nan(x),0,log(x))
  }
  h<-matrix(delta, ncol=T, nrow=N*T)-  do.call(rbind,rep(list(diag(delta,nrow=T, T)),N)) 
  
  ## set tolerances
  innertol<-1e-03
  
  firm_estim<-function(sindex, ap1, ap2, ap3, ap4){
    aleph<-c(ap1, ap2, ap3, ap4)
    findlambda<-function(lambda){
      E<-rep(1/(N*T),N*T)
      newe<-0
      i<-1
      A<-exp(-lambda*(rho*wmat -thetamat + h) )
      val<-100
      while (abs(val)>innertol){
        if (i>1){
          E<-newe
        } 
        C<-colSums(t(A)*aleph/colSums(A*E))
        newe<-E*C
        i<-i+1
        val<-sum(E*spec_log(C), na.rm=TRUE)-max(spec_log(C), na.rm=TRUE)
      }
      E<-newe
      
      a<-t(t(A*E)/colSums(A*E)*aleph)
      I<-sum(a*spec_log(t(t(a/E)/aleph)), na.rm=TRUE)
      diff<-sindex-I
      return(diff)
    }
    lambda<-tryCatch({uniroot(findlambda, lower=0, upper=90, tol=innertol, check.conv=TRUE)$root},
                     error = function(e){1000                     })
    if (lambda==1000){
      return(list(NA))
    } else {
      
      E<-starting
      newe<-0
      i<-1
      A<-exp(-lambda*(rho*wmat -thetamat + h) )
      val<-100
      while (abs(val)>innertol){
        if (i>1){
          E<-newe
        } 
        C<-colSums(t(A)*aleph/colSums(A*E))
        newe<-E*C
        i<-i+1
        val<-sum(E*spec_log(C), na.rm=TRUE)-max(spec_log(C), na.rm=TRUE)
      }
      E<-newe
      
      a<-t(t(A*E)/colSums(A*E)*aleph)
      b<-a/E
      b<-round(b, digits=7)
      E<-round(E,7)
      dstar<- sum(a*(-rho*wmat +thetamat - h))
      return(list(a,dstar ))
    }
  }
  
  qual<-function(x){
    sum(x$E*x$b*(thetamat - h), na.rm=TRUE)
  }
  
  
  #qual(firm_estim(1, alpha[2,]))
  firm_estim<-Vectorize(firm_estim)
  res<-lapply(1:firm_count, function(x){firm_estim(firm_essentials[x,]$sindex, 
                                                   firm_essentials[x,]$task_mix1,firm_essentials[x,]$task_mix2,
                                                   firm_essentials[x,]$task_mix3, firm_essentials[x,]$task_mix4 )})
  
  findd<-Vectorize(function(x){res[x][[1]][[2]]})
  finda<-Vectorize(function(x){res[x][[1]]})
  findqual<-Vectorize(function(x){sum(finda(x)[[1]]*(thetamat-h)) })
  # given optimal strategies, what are quantities demand of each type?
  firm_essentials[, top:=exp(task_mix1*beta_0[1] +task_mix2*beta_0[2]+task_mix3*beta_0[3]+task_mix4*beta_0[4]
                             +findd(firm_id)-
                               rho*c_shock -
                               rho*(task_mix1*c_0[1] +task_mix2*c_0[2]+task_mix3*c_0[3]+task_mix4*c_0[4] ))]
  firm_essentials[,share:=top/(1+sum(top)), by="market"]
  type_shares<-rowSums(sapply(1:firm_count,function(x){ rowSums(finda(x)[[1]]*as.numeric(firm_essentials$share[x]) )}))
  
  
  # observed data: shares, prices, alpha, max, sum of characteristics
  firm_essentials[,wagebill:=sapply(1:firm_count,function(x){ sum(finda(x)[[1]]*w )})]
  firm_essentials[,qual:=findqual(1:.N)]
  firm_essentials[, mc:=wagebill+task_mix1*c_0[1] +task_mix2*c_0[2]+task_mix3*c_0[3]+task_mix4*c_0[4] +c_shock]
  firm_essentials[,price:=mc+1/rho_0]
  firm_essentials[,s_0:=1-sum(share),by="market"]
  firm_essentials[,det_util:=qual-rho_0*price+task_mix1*beta_0[1] +task_mix2*beta_0[2]+task_mix3*beta_0[3]+task_mix4*beta_0[4]]
  
  
  # for each firm create fake ratings data.
  
  for(j in 1:firm_count){
    piece<-data.table(rating= revd(20)+firm_essentials[j,]$util_det,)
  }
  
  observed_data<-data.table(firm_id=firms_see, 
                            share=firm_essentials[firms_see]$share,
                            alpha[firms_see,], 
                            price=firm_essentials[firms_see,]$mc+1/rho_0,
                            sindex=firm_essentials[firms_see,]$sindex, 
                            market=market_groups[firms_see]$market,
                            quality=firm_essentials[firms_see]$qual,
                            s_0=firm_essentials[firms_see]$s_0
  )
  names(observed_data)[3:6]<-to("task_mix",4)
  # last few are max of b
  bmaxes<-data.table(t(sapply(firms_see, function(x){colMax(finda(x)[[1]]/rowSums(finda(x)[[1]]))})))
  names(bmaxes)<-to("bmax", 4)
  observed_data<-cbind(observed_data,bmaxes)
  
  observed_data[,period:=t]
  observed_data[,mean_shock:=mean(c_shock)]
  observed_data[, s_sum:=sum(task_mix1)+sum(task_mix2)+sum(task_mix3)-task_mix1-task_mix2-task_mix3]
  tot_data<-rbind(tot_data,observed_data)
}

### now we try to estimate the data given a subset of firms.
# price and shares equations given us everything but wages and delta.



tot_data[,lhs:=log(share)-log(s_0)-quality]
parms<-lm(lhs~task_mix1+task_mix2+task_mix3+price,
          data=tot_data)


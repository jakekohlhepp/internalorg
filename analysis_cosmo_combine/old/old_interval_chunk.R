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
      s_index<-sum(a*spec_log(t(t(a/rowSums(a))/colSums(a))))
      #### need to check that sindex is outside range. if inside, try bounds
      if (s_index<= usindex & s_index>=dsindex){
        s_index<-usindex
        
        findlambda<-function(lambda){
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
          E<-squarem(E,fixptfn = fxpt, control=list(maxiter=100000000,tol=innertol) )$par
          a<-t(t(A*E)/colSums(A*E)*aleph)
          I<-sum(a*spec_log(t(t(a/rowSums(a))/colSums(a))))
          diff<-I-sindex
          return(as.numeric(diff))
        }
        if (is.finite(lambda)){
          A<-exp(-lambda*(wmat -h/rho/abar) )
          A[A>=Inf]<-1e16
          fxpt<-function(p,y){
            C<-colSums(t(A)*aleph/colSums(A*p))
            return(p*C)
          }
          objval<-function(p,y){
            C<-colSums(t(A)*aleph/colSums(A*p))
            return(-sum(p*spec_log(C), na.rm=TRUE)+max(spec_log(C), na.rm=TRUE))
          }
          E<-rep(1/5,5)
          E<-squarem(E,fixptfn = fxpt, control=list(maxiter=100000000,tol=innertol) )$par
          a<-t(t(A*E)/colSums(A*E)*aleph)
        } else{
          a<-matrix(0, ncol=5, nrow=5)
          for (col in 1:5){
            a[which.min((wmat -h/rho/abar)[,col]),col]<-aleph[col]
          }
          E<-rowSums(a)
        }
        check_upper<-sum(a*spec_log(t(t(a/rowSums(a))/colSums(a))))
        check_upper<-check_upper-sum(a*(wmat -h/rho/abar))
        a_upper<-a
        E_upper<-E
        
        s_index<-dsindex
        if (is.finite(lambda)){
          A<-exp(-lambda*(wmat -h/rho/abar) )
          A[A>=Inf]<-1e16
          fxpt<-function(p,y){
            C<-colSums(t(A)*aleph/colSums(A*p))
            return(p*C)
          }
          objval<-function(p,y){
            C<-colSums(t(A)*aleph/colSums(A*p))
            return(-sum(p*spec_log(C), na.rm=TRUE)+max(spec_log(C), na.rm=TRUE))
          }
          E<-rep(1/5,5)
          E<-squarem(E,fixptfn = fxpt, control=list(maxiter=100000000,tol=innertol) )$par
          a<-t(t(A*E)/colSums(A*E)*aleph)
        } else{
          a<-matrix(0, ncol=5, nrow=5)
          for (col in 1:5){
            a[which.min((wmat -h/rho/abar)[,col]),col]<-aleph[col]
          }
          E<-rowSums(a)
        }
        check_lower<-sum(a*spec_log(t(t(a/rowSums(a))/colSums(a))))
        check_lower<-check_upper-sum(a*(wmat -h/rho/abar))
        if (check_lower>=check_upper){
          a<-a_upper
          E<-E_upper
        }
        
      }
      
      
      
      
      
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
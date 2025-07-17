library('lubridate')
library('stringr')
library('zoo')
library('lessR')
library('stats')
library('parallel')
library('xtable')
library('gmm')
library('readxl')
library('nloptr')
library('SQUAREM')
library('BB')

spec_log<-function(x)  ifelse(x==0 | is.nan(x),0,log(x))
bisection <- function(f, a, b, n, xtol, ftol) {
  
  a_prime<-a
  b_prime<-b
  
  # when very low function value is positive, start a higher
  if (f(0.1)>0) a_prime<-40
  
  if (is.nan(f(a_prime))){
    while (is.nan(f(a_prime))){
      a_prime<-a_prime+10
      stopifnot(b_prime>a_prime)
    }
  }
  
  
  while (f(a_prime)>0){
    a_prime<-a_prime-0.005
    stopifnot(a_prime>0)
  }
  
  while (f(b_prime)<0){
    b_prime<-b_prime+10
  }
  

  stopifnot(b_prime>a_prime)

  
  for (i in 1:n) {
    c <- (a_prime + b_prime) / 2 # Calculate midpoint
    
    # If the function equals 0 at the midpoint or the midpoint is below the desired tolerance, stop the 
    # function and return the root.
    if (abs(f(c))<ftol || ((b_prime - a_prime) / 2) < xtol) {
      return(list("root"=c, "val"=f(c),"conv"=abs(f(c))<ftol || ((b_prime - a_prime) / 2) < xtol  ))
    }
    
    # If another iteration is required, 
    # check the signs of the function at the points c and a and reassign
    # a or b accordingly as the midpoint to be used in the next iteration.
    ifelse(sign(f(c)) == sign(f(a_prime)), 
           a_prime <- c,
           b_prime <- c)
  }
  # If the max number of iterations is reached and no root has been found, 
  # return message and end function.
  return(list("root"=c, "val"=f(c),"conv"=abs(f(c))<ftol || ((b_prime - a_prime) / 2) < xtol  ))
}


## estimate new york city only. use BFGS, so internalize all constraints.
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
if (get_os()=="windows"){
  core_count<-detectCores()-1
} else{
  core_count<-75
} 




### Purpose: estimate via GMM the market parameters.
stopifnot(nrow(working_data[cust_price<=0])==0)

# have ppi be one quarter lagged
ppi<-data.table(readRDS("mkdata/data/ppi.rds"))
setkey(ppi, "quarter_year")
ppi[, lag_ppi:=data.table::shift(ppi_inputs )]
working_data<-merge(working_data,ppi ,all.x=TRUE, by="quarter_year")
stopifnot(nrow(working_data[is.na(ppi_inputs)])==0)
stopifnot(nrow(working_data[cust_price<=0])==0)
working_data[, labor_instrument:=avg_wkly_wage/40*avg_labor]
working_data[, year:=floor(quarter_year)]
working_data[, quarter:=round((quarter_year-floor(quarter_year))*10)]
working_data[, qy_cnty:=paste0(county," - ",as.character(quarter_year))]
stopifnot(any(rowSums(working_data[,.SD, .SDcols = names(working_data)[grep("^B_raw_[0-9]_", names(working_data))]])==1))
stopifnot(any(rowSums(working_data[,.SD, .SDcols = names(working_data)[grep("^B_[0-9]_", names(working_data))]])==1))

# minimum wages
min_wage<-data.table(read_excel('analysis_final/minwage.xlsx'))
working_data<-merge(working_data,min_wage[,c("county", "quarter_year", "min_wage")],by=c("county", "quarter_year"),all.x=TRUE)
stopifnot(nrow(working_data[is.na(min_wage)])==0)

working_data[, log_rel_mkt:=log(salon_share_subdiv/outside_share)]
working_data[, mk_piece:=1/(1-salon_share_subdiv)]
working_data[, org_cost:=gamma_normalized*s_index*avg_labor]
setorder(working_data, "location_id", "quarter_year")
working_data[, dye_instrument:=task_mix_2*ppi_inputs]
stopifnot(nrow(working_data[is.na(org_cost)])==0)
sumcount<-function(x) return(sum(x>0))
# no situatons where wrker type is not observed
working_data[,lapply(.SD,sumcount), by=c("qy_cnty"), .SDcols=names(working_data)[grep("^E_raw_[0-9]", names(working_data))]]->check
#stopifnot(all(check>0))

# estimate via gmm. all counties share the same material cost parameters.
# national-quarter parameters: 5 material costs (5 parms times number of quarters)
# county time invariant: price sensitivity, org cost, skills (27 parms times number of counties)
# county-quarter: 5 wages, 1 demand intercept, 1 cost intercept (7 parms times the number of county-quarters)
quarter_count<-uniqueN(working_data$quarter_year)
county_count<-uniqueN(working_data$county)
skill_count<-length(names(working_data)[grep("^B_raw_[0-9]_", names(working_data))])

# make county string
working_data[, county:=as.character(county)]
#for (y in unique(working_data$quarter_year)) working_data[,(paste0("fe_",y))  := quarter_year==y ]

working_data[, mult_duration_hrs:=tot_duration/60]
estim_matrix<-as.data.frame(working_data[,.SD, .SDcols=c("avg_labor","dye_instrument","county","quarter_year","log_rel_mkt", "cust_price", 
                                                         names(working_data)[grep("^B_raw_[0-9]_", names(working_data))],
                                                         "org_cost","mk_piece",
                                                         names(working_data)[grep("^E_raw_[0-9]", names(working_data))],
                                                         names(working_data)[grep("^task_mix_[0-9]", names(working_data))],"qy_cnty",
                                                         "gamma_normalized", "s_index")])

####### setup estimation equations
## none should be missing
stopifnot(all(!is.na(estim_matrix)))

xnam<-c("factor(county):cust_price","factor(county):factor(quarter_year)",paste0("factor(county):avg_labor:",names(working_data)[grep("^B_raw_[0-9]_", names(working_data))]))
xnam <- as.formula(paste0("~",paste0(xnam, collapse="+"),"-1"))
mm_1<-model.matrix(xnam, data=estim_matrix)

xnam2 <- paste0("factor(quarter_year):",names(working_data)[grep("^task_mix", names(working_data))])
xnam3 <- paste0("factor(county):avg_labor:",names(working_data)[grep("^E_raw_[0-9]", names(working_data))])
xnam4<-c(xnam2, xnam3)
xnam4 <- as.formula(paste0("~factor(county):mk_piece+factor(county):org_cost+",paste0(xnam4, collapse="+"),"+factor(county):factor(quarter_year):avg_labor+factor(county):factor(quarter_year)-1"))
mm_2<-model.matrix(xnam4, data=estim_matrix)
# instruments matrix is the same except cust_price is not used and we drop out . instead we swap in org_cost in the demand matrix 
xnam<-c("dye_instrument","factor(quarter_year)",paste0("avg_labor:",names(working_data)[grep("^B_raw_[0-9]_", names(working_data))]))
xnam <- as.formula(paste0("~",paste0(paste0("factor(county):",xnam), collapse="+"),"-1"))
z_mm_1<-model.matrix(xnam, data=estim_matrix)


xnam2 <- paste0("factor(quarter_year):",names(working_data)[grep("^task_mix", names(working_data))])
xnam3 <- paste0("factor(county):avg_labor:",names(working_data)[grep("^E_raw_[0-9]", names(working_data))])
xnam4<-c(xnam2, xnam3)
xnam4 <- as.formula(paste0("~factor(county):org_cost+",paste0(xnam4, collapse="+"),"+factor(county):factor(quarter_year):avg_labor+factor(county):factor(quarter_year)-1"))
z_mm_2<-model.matrix(xnam4, data=estim_matrix)

### create s-index, E moments.
E_match<- data.frame(as.matrix(estim_matrix[,c("E_raw_2", "E_raw_3", "E_raw_4", "E_raw_5", "s_index")]),factor(estim_matrix[, "county"]))
colnames(E_match)<-c("E_2", "E_3", "E_4", "E_5","s_index", "county")
E_mat<-model.matrix(~county:E_2 + county:E_3+county:E_3+county:E_4+county:E_5+county:s_index-1, data=E_match)


##### compute the analytic estimator as a check
skip_to_next <- FALSE

tryCatch({
  beta<-solve(t(mm_1)%*%z_mm_1%*%solve(t(z_mm_1)%*%z_mm_1)%*%t(z_mm_1)%*%mm_1)%*%(t(mm_1)%*%z_mm_1%*%solve(t(z_mm_1)%*%z_mm_1)%*%t(z_mm_1)%*%as.matrix(estim_matrix[,"log_rel_mkt"]))
  p_adj<-estim_matrix[,"cust_price"]+estim_matrix[,"mk_piece"]*(1/beta[grep("6037:cust_price", rownames(beta))]*(estim_matrix$county=="6037")+
                                                                  1/beta[grep("36061:cust_price", rownames(beta))]*(estim_matrix$county=="36061")+
                                                                  1/beta[grep("17031:cust_price", rownames(beta))]*(estim_matrix$county=="17031"))
  beta_2<-solve(t(z_mm_2)%*%z_mm_2)%*%t(z_mm_2)%*%as.matrix(p_adj)
}, error = function(e) { skip_to_next <- TRUE})



if(skip_to_next) { next }   






objective_gmm<-function(theta,x){
  data<-x
  pre_parms<-theta
  names(pre_parms)<-names(beta_2_subset)
  
  
  tild_theta<-vector(mode='list', length=3)
  names(tild_theta)<-list("17031", "36061", "6037")
  
  for (cnty in names(tild_theta)){
    w_mat<-matrix(c(0,pre_parms[grep(paste0(cnty,":avg_labor:E"),names(pre_parms))]), ncol=5, nrow=5, byrow=FALSE)
    skills<-matrix(beta[grep(paste0(cnty,":avg_labor:B"),rownames(beta))], ncol=5, nrow=5, byrow=FALSE)
    rho<-beta[grep(paste0(cnty,":cust_price$"),rownames(beta))]
    tild_theta[[cnty]]<-w_mat+(rho)^(-1)*skills
    tild_theta[[cnty]]<-sweep(tild_theta[[cnty]],2,apply(tild_theta[[cnty]],2,min))
  }
  get_demands<-function(a1, a2, a3, a4, a5, county,s_index){
    alpha<-c(a1, a2, a3, a4,a5)
    B<-matrix(0, ncol=5, nrow=5)
    for (col in 1:5){
      B[which.min(tild_theta[[county]][,col]),col]<-alpha[col]
    }
    E<-rowSums(B)
    Brel<-t(t(B/E)/alpha)
    sbound<-sum(B*spec_log(Brel))
    # if the sindex is beyond the achievable bound, just return maximum specialization.
    if (s_index>sbound){
      return(E[2:5])
    } else{
      get_struct<-function(gamma){
        
        ## this function will return matrix given gamma
        A<-exp(-1/gamma*(tild_theta[[county]]) )
        E<-rep(0.2, 5)
        A[A>=Inf]<-1e16
        A[A<=0]<-1e-16
        fxpt<-function(p){
          C<-colSums(t(A)*alpha/colSums(A*p))
          return(p*C)
        }
        #for (counter in 1:1000000){
        #  E_old<-E
        #  E<-fxpt(E_old)
        #  if (all(abs(E-E_old)<innertol)) break
        #}
        E<-squarem(E,fixptfn = fxpt, control=list(maxiter=100000,tol=innertol) )$par
        B<-t(t(A)*alpha/colSums(A*E))*E
        B[abs(B)<1e-16]<-0
        Brel<-t(t(B/E)/alpha)
        return(s_index-sum(B*spec_log(Brel)) )
      }
      res_find<-bisection(get_struct, a=1, b=10000, ftol=outertol,xtol=outertol,n=10000)
      stopifnot(res_find$conv)
      gamma<-res_find$root
      A<-exp(-1/gamma*(tild_theta[[county]]) )
      
      E<-rep(0.2, 5)
      A[A>=Inf]<-1e16
      A[A<=0]<-1e-16
      fxpt<-function(p){
        C<-colSums(t(A)*alpha/colSums(A*p))
        return(p*C)
      }
      
      E<-squarem(E,fixptfn = fxpt, control=list(maxiter=100000,tol=innertol) )$par
      B<-t(t(A)*alpha/colSums(A*E))*E
      B[abs(B)<1e-16]<-0
      return(E[2:5])
    }
    
  }
  
  get_demands_outer<-function(x){
    return(get_demands(data$task_mix_1[x],data$task_mix_2[x],data$task_mix_3[x],
                       data$task_mix_4[x],data$task_mix_5[x],data$county[x], data$s_index[x]  ))
  }
  
  
  if (pl_on==TRUE){
    
    
    if (get_os()=="windows"){
      temp_res<-data.table(do.call(rbind,parLapply(clust,1:nrow(data),get_demands_outer)))
      
    } else{
      temp_res<-data.table(do.call(rbind,mclapply(1:nrow(data),get_demands_outer, mc.cores=core_count)))
    } 
  } else{
    temp_res<-data.table(do.call(rbind,lapply(1:nrow(data),get_demands_outer)))
    
  }
  
  # exclude E_1 so no collinearity.
  E_match<- data.frame((as.matrix(temp_res)-as.matrix(data[,c("E_raw_2", "E_raw_3", "E_raw_4", "E_raw_5")])),factor(data[, "county"]))
  colnames(E_match)<-c("E_2", "E_3", "E_4", "E_5", "county")
  E_mat<-model.matrix(~county:E_2 + county:E_3+county:E_3+county:E_4+county:E_5-1, data=E_match)
  
  return(E_mat)
  
}

objective_squared<-function(parms){
  res<-colMeans(objective_gmm(theta=parms, x=estim_matrix))
  return(sum(res^2))
}


eval_moments<-function(theta,x){
  data<-x
  pre_parms<-theta
  names(pre_parms)<-names(beta_2_subset)
  
  
  tild_theta<-vector(mode='list', length=3)
  names(tild_theta)<-list("17031", "36061", "6037")
  
  for (cnty in names(tild_theta)){
    w_mat<-matrix(c(0,pre_parms[grep(paste0(cnty,":avg_labor:E"),names(pre_parms))]), ncol=5, nrow=5, byrow=FALSE)
    skills<-matrix(beta[grep(paste0(cnty,":avg_labor:B"),rownames(beta))], ncol=5, nrow=5, byrow=FALSE)
    rho<-beta[grep(paste0(cnty,":cust_price$"),rownames(beta))]
    tild_theta[[cnty]]<-w_mat+(rho)^(-1)*skills
    tild_theta[[cnty]]<-sweep(tild_theta[[cnty]],2,apply(tild_theta[[cnty]],2,min))
  }
  get_demands<-function(a1, a2, a3, a4, a5, county,s_index){
    alpha<-c(a1, a2, a3, a4,a5)
    B<-matrix(0, ncol=5, nrow=5)
    for (col in 1:5){
      B[which.min(tild_theta[[county]][,col]),col]<-alpha[col]
    }
    E<-rowSums(B)
    Brel<-t(t(B/E)/alpha)
    sbound<-sum(B*spec_log(Brel))
    # if the sindex is beyond the achievable bound, just return maximum specialization.
    if (s_index>sbound){
      return(E[2:5])
    } else{
      get_struct<-function(gamma){
        
        ## this function will return matrix given gamma
        A<-exp(-1/gamma*(tild_theta[[county]]) )
        E<-rep(0.2, 5)
        A[A>=Inf]<-1e16
        A[A<=0]<-1e-16
        fxpt<-function(p){
          C<-colSums(t(A)*alpha/colSums(A*p))
          return(p*C)
        }
        #for (counter in 1:1000000){
        #  E_old<-E
        #  E<-fxpt(E_old)
        #  if (all(abs(E-E_old)<innertol)) break
        #}
        E<-squarem(E,fixptfn = fxpt, control=list(maxiter=100000,tol=innertol) )$par
        B<-t(t(A)*alpha/colSums(A*E))*E
        B[abs(B)<1e-16]<-0
        Brel<-t(t(B/E)/alpha)
        return(s_index-sum(B*spec_log(Brel)) )
      }
      res_find<-bisection(get_struct, a=1, b=10000, ftol=outertol,xtol=outertol,n=10000)
      stopifnot(res_find$conv)
      gamma<-res_find$root
      A<-exp(-1/gamma*(tild_theta[[county]]) )
      
      E<-rep(0.2, 5)
      A[A>=Inf]<-1e16
      A[A<=0]<-1e-16
      fxpt<-function(p){
        C<-colSums(t(A)*alpha/colSums(A*p))
        return(p*C)
      }
      
      E<-squarem(E,fixptfn = fxpt, control=list(maxiter=100000,tol=innertol) )$par
      B<-t(t(A)*alpha/colSums(A*E))*E
      B[abs(B)<1e-16]<-0
      return(E)
    }
    
  }
  
  get_demands_outer<-function(x){
    return(get_demands(data$task_mix_1[x],data$task_mix_2[x],data$task_mix_3[x],
                       data$task_mix_4[x],data$task_mix_5[x],data$county[x], data$s_index[x]  ))
  }
  
  
  if (pl_on==TRUE){
    
    
    if (get_os()=="windows"){
      temp_res<-data.table(do.call(rbind,parLapply(clust,1:nrow(data),get_demands_outer)))
      
    } else{
      temp_res<-data.table(do.call(rbind,mclapply(1:nrow(data),get_demands_outer, mc.cores=core_count)))
    } 
  } else{
    temp_res<-data.table(do.call(rbind,lapply(1:nrow(data),get_demands_outer)))
    
  }
  
  # exclude E_1 so no collinearity.
  E_match<- data.frame((as.matrix(data[,c("E_raw_1","E_raw_2", "E_raw_3", "E_raw_4", "E_raw_5")])),factor(data[, "county"]))
  colnames(E_match)<-c("E_1","E_2", "E_3", "E_4", "E_5", "county")
  E_raw<-model.matrix(~county:E_1+county:E_2 + county:E_3+county:E_3+county:E_4+county:E_5-1, data=E_match)
  
  
  E_match<- data.frame((as.matrix(temp_res)),factor(data[, "county"]))
  colnames(E_match)<-c("E_1","E_2", "E_3", "E_4", "E_5", "county")
  E_model<-model.matrix(~county:E_1+county:E_2 + county:E_3+county:E_3+county:E_4+county:E_5-1, data=E_match)
  
  
  return(cbind(colMeans(E_model), colMeans(E_raw)))
  
}

objective_vect<-function(parms){
  return(colMeans(objective_gmm(theta=parms, x=estim_matrix)))
}

get_gammas<-function(theta,x){
  data<-x
  pre_parms<-theta
  names(pre_parms)<-names(beta_2_subset)
  
  
  tild_theta<-vector(mode='list', length=3)
  names(tild_theta)<-list("17031", "36061", "6037")
  
  for (cnty in names(tild_theta)){
    w_mat<-matrix(c(0,pre_parms[grep(paste0(cnty,":avg_labor:E"),names(pre_parms))]), ncol=5, nrow=5, byrow=FALSE)
    skills<-matrix(beta[grep(paste0(cnty,":avg_labor:B"),rownames(beta))], ncol=5, nrow=5, byrow=FALSE)
    rho<-beta[grep(paste0(cnty,":cust_price$"),rownames(beta))]
    tild_theta[[cnty]]<-w_mat+(rho)^(-1)*skills
    tild_theta[[cnty]]<-sweep(tild_theta[[cnty]],2,apply(tild_theta[[cnty]],2,min))
  }
  get_demands<-function(a1, a2, a3, a4, a5, county,s_index){
    alpha<-c(a1, a2, a3, a4,a5)
    B<-matrix(0, ncol=5, nrow=5)
    for (col in 1:5){
      B[which.min(tild_theta[[county]][,col]),col]<-alpha[col]
    }
    E<-rowSums(B)
    Brel<-t(t(B/E)/alpha)
    sbound<-sum(B*spec_log(Brel))
    # if the sindex is beyond the achievable bound, just return 0
    if (s_index>sbound){
      return(0)
    } else{
      get_struct<-function(gamma){
        
        ## this function will return matrix given gamma
        A<-exp(-1/gamma*(tild_theta[[county]]) )
        E<-rep(0.2, 5)
        A[A>=Inf]<-1e16
        A[A<=0]<-1e-16
        fxpt<-function(p){
          C<-colSums(t(A)*alpha/colSums(A*p))
          return(p*C)
        }
        #for (counter in 1:1000000){
        #  E_old<-E
        #  E<-fxpt(E_old)
        #  if (all(abs(E-E_old)<innertol)) break
        #}
        E<-squarem(E,fixptfn = fxpt, control=list(maxiter=100000,tol=innertol) )$par
        B<-t(t(A)*alpha/colSums(A*E))*E
        B[abs(B)<1e-16]<-0
        Brel<-t(t(B/E)/alpha)
        return(s_index-sum(B*spec_log(Brel)) )
      }
      res_find<-bisection(get_struct, a=1, b=10000, ftol=outertol,xtol=outertol,n=10000)
      stopifnot(res_find$conv)
      gamma<-res_find$root
      
      return(gamma)
    }
    
  }
  
  get_demands_outer<-function(x){
    return(get_demands(data$task_mix_1[x],data$task_mix_2[x],data$task_mix_3[x],
                       data$task_mix_4[x],data$task_mix_5[x],data$county[x], data$s_index[x]  ))
  }
  
  
  if (pl_on==TRUE){
    
    
    if (get_os()=="windows"){
      temp_res<-data.table(do.call(rbind,parLapply(clust,1:nrow(data),get_demands_outer)))
      
    } else{
      temp_res<-data.table(do.call(rbind,mclapply(1:nrow(data),get_demands_outer, mc.cores=core_count)))
    } 
  } else{
    temp_res<-data.table(do.call(rbind,lapply(1:nrow(data),get_demands_outer)))
    
  }
  return(temp_res$V1)
  
}
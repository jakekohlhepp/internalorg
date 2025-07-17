## purpose: get substitution patterns
## make table

library('data.table')
library('lubridate')
library('stringr')
library('ggplot2')
library('gridExtra')
library('kableExtra')


get_prod<-function(a1, a2, a3, a4, a5, county,quarter_year,gamma){
  alpha<-c(a1, a2, a3, a4,a5)
  if (is.finite(gamma) & gamma>0){
    
    alpha<-c(a1, a2, a3, a4,a5)
    ## this function will return matrix given gamma
    A<-exp(-1/gamma*(tild_theta[[county]][[as.character(quarter_year)]]) )
    E<-rep(0.2, 5)
    A[A>=Inf]<-1e16
    A[A<=0]<-1e-16
    fxpt<-function(p){
      C<-colSums(t(A)*alpha/colSums(A*p))
      return(p*C)
    }
    for (i in 1:1000000){
      E_old<-E
      E<-fxpt(E_old)
      if (all(abs(E-E_old)<innertol)) break
    }
    B<-t(t(A)*alpha/colSums(A*E))*E
  } else if (gamma==0){
    # no frictions
    B<-matrix(0, ncol=5, nrow=5)
    for (col in 1:5){
      B[which.min(tild_theta[[county]][[as.character(quarter_year)]][,col]),col]<-alpha[col]
    }
  } else{
    # max frictions
    B<-matrix(0, ncol=5, nrow=5)
    B[which.min(rowSums(t(t(tild_theta[[county]][[as.character(quarter_year)]])*alpha ))),]<-alpha
  }
  B[abs(B)<1e-16]<-0
  E<-rowSums(B*new_theta[[county]])/rowSums(B)
  
  return(E)
}

get_demands<-function(a1, a2, a3, a4, a5, county,quarter_year,gamma){
  alpha<-c(a1, a2, a3, a4,a5)
  if (is.finite(gamma) & gamma>0){
    
    alpha<-c(a1, a2, a3, a4,a5)
    ## this function will return matrix given gamma
    A<-exp(-1/gamma*(tild_theta[[county]][[as.character(quarter_year)]]) )
    E<-rep(0.2, 5)
    A[A>=Inf]<-1e16
    A[A<=0]<-1e-16
    fxpt<-function(p){
      C<-colSums(t(A)*alpha/colSums(A*p))
      return(p*C)
    }
    for (i in 1:1000000){
      E_old<-E
      E<-fxpt(E_old)
      if (all(abs(E-E_old)<innertol)) break
    }
    B<-t(t(A)*alpha/colSums(A*E))*E
  } else if (gamma==0){
    # no frictions
    B<-matrix(0, ncol=5, nrow=5)
    for (col in 1:5){
      B[which.min(tild_theta[[county]][[as.character(quarter_year)]][,col]),col]<-alpha[col]
    }
  } else{
    # max frictions
    B<-matrix(0, ncol=5, nrow=5)
    B[which.min(rowSums(t(t(tild_theta[[county]][[as.character(quarter_year)]])*alpha ))),]<-alpha
  }
  B[abs(B)<1e-16]<-0
  E<-rowSums(B)
  
  return(E)
}

all_results<-readRDS('analysis_final/data/02_00_parameters.rds')
full_unsmoothed<-readRDS('analysis_final/data/02_04_withgammas.rds')

market_parms<-all_results$coefficients
names(market_parms)<-all_results$parm_name

new_theta<-vector(mode='list', length=3)
names(new_theta)<-list("17031", "36061", "6037")

tild_theta<-vector(mode='list', length=3)
names(tild_theta)<-list("17031", "36061", "6037")
for (cnty in names(tild_theta)){
  tild_theta[[cnty]]<-vector(mode='list', length=12)
  names(tild_theta[[cnty]])<-unique(full_unsmoothed$quarter_year)
}
## be careful - the sweep command helps numerically but needs to be undone for some outcomes
for (cnty in names(tild_theta)){
  for (qy in names(tild_theta[[cnty]])){
    w_mat<-matrix(c(0,market_parms[grep(paste0(cnty,":avg_labor:E"),names(market_parms))]), ncol=5, nrow=5, byrow=FALSE)
    ## add in base wage
    w_mat<-w_mat+market_parms[paste0("avg_labor:factor(county)",cnty,":factor(quarter_year)",qy)]
    skills<-matrix(market_parms[grep(paste0(cnty,":avg_labor:B"),names(market_parms))], ncol=5, nrow=5, byrow=FALSE)
    rho<-market_parms[grep(paste0(cnty,":cust_price$"),names(market_parms))]
    tild_theta[[cnty]][[qy]]<-w_mat+(rho)^(-1)*skills
    tild_theta[[cnty]][[qy]]<-sweep(tild_theta[[cnty]][[qy]],2,apply(tild_theta[[cnty]][[qy]],2,min))
    new_theta[[cnty]]<-skills-min(skills)
    
  }
}
org_tild_theta<-tild_theta
innertol<-1e-08
outertol<-1e-04
pl_on<-TRUE

for (i in 1:nrow(full_unsmoothed)){
  full_unsmoothed[i,c("l_1", "l_2", "l_3", "l_4", "l_5") := as.list(get_demands(task_mix_1, task_mix_2, task_mix_3, task_mix_4, task_mix_5, county,quarter_year, gamma_invert)) ]
  print(i)
}


for (i in 1:nrow(full_unsmoothed)){
  full_unsmoothed[i,c("E_1", "E_2", "E_3", "E_4", "E_5") := as.list(get_prod(task_mix_1, task_mix_2, task_mix_3, task_mix_4, task_mix_5, county,quarter_year, gamma_invert)) ]
  print(i)
}


#### productivity patterns

## shock wages




for (w in 1:5){
  for (cnty in names(tild_theta)){
    for (qy in names(tild_theta[[cnty]])){
      w_mat<-matrix(c(0,market_parms[grep(paste0(cnty,":avg_labor:E"),names(market_parms))]), ncol=5, nrow=5, byrow=FALSE)
      ## add in base wage
      w_mat<-w_mat+market_parms[paste0("avg_labor:factor(county)",cnty,":factor(quarter_year)",qy)]
      w_mat[w,]<-w_mat[w,]*1.01
      skills<-matrix(market_parms[grep(paste0(cnty,":avg_labor:B"),names(market_parms))], ncol=5, nrow=5, byrow=FALSE)
      rho<-market_parms[grep(paste0(cnty,":cust_price$"),names(market_parms))]
      tild_theta[[cnty]][[qy]]<-w_mat+(rho)^(-1)*skills
      tild_theta[[cnty]][[qy]]<-sweep(tild_theta[[cnty]][[qy]],2,apply(tild_theta[[cnty]][[qy]],2,min))
      
    }
  }
  
  for (i in 1:nrow(full_unsmoothed)){
    full_unsmoothed[i,paste0(c("E_1", "E_2", "E_3", "E_4", "E_5"),"_",w) := as.list(get_prod(task_mix_1, task_mix_2, task_mix_3, task_mix_4, task_mix_5, county, quarter_year,gamma_invert)) ]
    print(i)
  }
}

tild_theta<-org_tild_theta

for (enum in gsub("^E_","",names(full_unsmoothed)[grep("^E_[0-9]+$", names(full_unsmoothed))])){
  for (eshock in gsub("^E_","",names(full_unsmoothed)[grep("^E_[0-9]+$", names(full_unsmoothed))])){
    full_unsmoothed[get(paste0("l_",enum))>0.01 & get(paste0("l_",eshock))>0.01,(paste0("ediff_",enum,"_",eshock))  := (get(paste0("E_",enum,"_",eshock)) - get(paste0("E_",enum)))/get(paste0("E_",enum))]
  }
} 
full_unsmoothed[is.nan(gamma_invert), gamma_invert:=Inf]
full_unsmoothed[, gamma_quant:=ecdf(gamma_invert)(gamma_invert), by='county']

quantfunc<-function(x,q){
  return(format(round(quantile(x[!is.na(x)], q),digits=3),nsmall=3) )
}
sub_patterns<-full_unsmoothed[,lapply(.SD, quantfunc,q=0), by=c("county"), .SDcols=names(full_unsmoothed)[grep("ediff", names(full_unsmoothed))]]
sub_patterns[, stat_name:="Min."]
sub_patterns<-rbind(sub_patterns,full_unsmoothed[,lapply(.SD, quantfunc,q=1), by=c("county"), .SDcols=names(full_unsmoothed)[grep("ediff", names(full_unsmoothed))]], fill=TRUE)
sub_patterns[is.na(stat_name), stat_name:="Max."]

sub_patterns<-rbind(sub_patterns,full_unsmoothed[,lapply(.SD, quantfunc,q=0.5), by=c("county"), .SDcols=names(full_unsmoothed)[grep("ediff", names(full_unsmoothed))]], fill=TRUE)
sub_patterns[is.na(stat_name), stat_name:="Med."]

sub_patterns<-melt(sub_patterns, id.vars=c("county","stat_name"),measure = patterns("^ediff"))
sub_patterns[, enum:=str_replace_all(str_extract(variable   ,"_[0-9]_"), "_","")]
sub_patterns[, eshock:=str_replace_all(str_extract(variable   ,"_[0-9]$"), "_","")]
sub_patterns<-dcast(sub_patterns, county+enum~eshock+stat_name)

colnames(sub_patterns)<-c("County", "Skill Set", rep(c("Max.", "Med.", "Min."),5))
setkey(sub_patterns, "County", "Skill Set")
sub_patterns[County=='17031', County:="Cook"]
sub_patterns[County=='36061', County:="New York"]
sub_patterns[County=='6037', County:="Los Angeles"]

output<-kable(sub_patterns, "latex", align="c", booktabs=TRUE,linesep = c(""), escape = F, caption = NA, label=NA) 
output<-add_header_above(output,c(" "," ", "Skill Set 1" = 3, "Skill Set 2" = 3, "Skill Set 3" = 3, "Skill Set 4" = 3, "Skill Set 5" = 3))
cat(output, file = "analysis_final/out/tables/02_06b_substitute_prod.tex")

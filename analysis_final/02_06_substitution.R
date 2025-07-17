## purpose: get substitution patterns
## make table

library('data.table')
library('lubridate')
library('stringr')
library('ggplot2')
library('gridExtra')
library('kableExtra')


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
  }
}
org_tild_theta<-tild_theta
innertol<-1e-08
outertol<-1e-04
pl_on<-TRUE

for (i in 1:nrow(full_unsmoothed)){
  full_unsmoothed[i,c("E_1", "E_2", "E_3", "E_4", "E_5") := as.list(get_demands(task_mix_1, task_mix_2, task_mix_3, task_mix_4, task_mix_5, county,quarter_year, gamma_invert)) ]
  print(i)
}

##3 when NA fill in
full_unsmoothed[is.na(E_raw_1),E_raw_1:=E_1]
full_unsmoothed[is.na(E_raw_2),E_raw_2:=E_2]
full_unsmoothed[is.na(E_raw_3),E_raw_3:=E_3]
full_unsmoothed[is.na(E_raw_4),E_raw_4:=E_4]
full_unsmoothed[is.na(E_raw_5),E_raw_5:=E_5]
stopifnot(all(!is.na(full_unsmoothed[,c("E_raw_1", "E_raw_2", "E_raw_3", "E_raw_4", "E_raw_5")])))


#### substitution patterns

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
    full_unsmoothed[i,paste0(c("E_1", "E_2", "E_3", "E_4", "E_5"),"_",w) := as.list(get_demands(task_mix_1, task_mix_2, task_mix_3, task_mix_4, task_mix_5, county, quarter_year,gamma_invert)) ]
    print(i)
  }
}

tild_theta<-org_tild_theta

for (enum in gsub("^E_","",names(full_unsmoothed)[grep("^E_[0-9]+$", names(full_unsmoothed))])){
  for (eshock in gsub("^E_","",names(full_unsmoothed)[grep("^E_[0-9]+$", names(full_unsmoothed))])){
    full_unsmoothed[,(paste0("ediff_",enum,"_",eshock))  := get(paste0("E_",enum,"_",eshock)) - get(paste0("E_",enum))]
  }
} 
full_unsmoothed[is.nan(gamma_invert), gamma_invert:=Inf]
full_unsmoothed[, gamma_quant:=ecdf(gamma_invert)(gamma_invert), by='county']

quantfunc<-function(x,q){
  return(format(round(quantile(x, q),digits=3),nsmall=3) )
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
cat(output, file = "analysis_final/out/tables/02_06_substitute.tex")
  

list_todo<-c(which(full_unsmoothed$location_id=='03d9b605-d697-461a-abc4-b9fbc502fecc' & full_unsmoothed$quarter_year==2020.1),
             which(full_unsmoothed$location_id=='8310d870-fb13-414b-ba6c-2902eaf0276f' & full_unsmoothed$quarter_year==2020.4),
             which(full_unsmoothed$location_id=='6240c56b-a6fd-4c9a-bb4e-3d9aac3254e9' & full_unsmoothed$quarter_year==2020.4),
             which(full_unsmoothed$location_id=='c7ea80fe-c02f-4534-ab57-f7188317ea64' & full_unsmoothed$quarter_year==2018.1),
             which(full_unsmoothed$location_id=='5e616d9e-259e-4ace-a25a-e5037683c78f' & full_unsmoothed$quarter_year==2020.4))

             


total_res<-data.table()
for (i in list_todo){
  for (g in seq(from=0, to=1000, by=1)){
    
    res<-as.vector(get_demands(full_unsmoothed$task_mix_1[i], full_unsmoothed$task_mix_2[i], full_unsmoothed$task_mix_3[i], full_unsmoothed$task_mix_4[i], full_unsmoothed$task_mix_5[i], full_unsmoothed$county[i],full_unsmoothed$quarter_year[i], g))
    
    res<-data.table(location_id=full_unsmoothed$location_id[i],quarter_year=full_unsmoothed$quarter_year[i],
                    county=full_unsmoothed$county[i], gamma=g,
                    E_1=res[1], E_2=res[2], E_3=res[3], E_4=res[4], E_5=res[5])
    total_res<-rbind(total_res, res)
  }
  
}
 
total_res[, count_emp:= (E_1>1e-02) +(E_2>1e-02)+(E_3>1e-02)+(E_4>1e-02)+(E_5>1e-02) ]

setkey(total_res, "location_id","quarter_year", "gamma")

## fully decreasing
ggplot(data=total_res[location_id=='03d9b605-d697-461a-abc4-b9fbc502fecc' & quarter_year==2020.1 & gamma<=800])+geom_line(aes(x=gamma, y=count_emp))+
  ylab("# Skill Sets Employed")+xlab("Coordination Cost Parameter")+theme_bw() + theme(axis.text = element_text(size = 14),text = element_text(size = 20))+ 
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),panel.background = element_blank(), axis.line = element_line(colour = "black"))+
  geom_vline(xintercept =as.numeric(full_unsmoothed[location_id=='03d9b605-d697-461a-abc4-b9fbc502fecc' & quarter_year==2020.1,"gamma_invert"][1]), linetype="dashed", color = "blue", size=1)
ggsave("analysis_final/out/figures/02_06_middle_decreasing_count.png", width=12, height=8, units="in")

ggplot(data=total_res[location_id=='03d9b605-d697-461a-abc4-b9fbc502fecc' & quarter_year==2020.1 & gamma<=800])+geom_line(aes(x=gamma, y=count_emp), color="white")+
  ylab("# Skill Sets Employed")+xlab("Coordination Cost Parameter")+theme_bw() + theme(axis.text = element_text(size = 14),text = element_text(size = 20))+ 
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),panel.background = element_blank(), axis.line = element_line(colour = "black"))
ggsave("analysis_final/out/figures/02_06_middle_decreasing_count_blank.png", width=12, height=8, units="in")




## middle non-mons
ggplot(data=total_res[location_id=='8310d870-fb13-414b-ba6c-2902eaf0276f' & quarter_year==2020.4 & gamma<=800])+geom_line(aes(x=gamma, y=count_emp))+
  ylab("# Skill Sets Employed")+xlab("Coordination Cost Parameter")+theme_bw() + theme(axis.text = element_text(size = 14),text = element_text(size = 20))+ 
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),panel.background = element_blank(), axis.line = element_line(colour = "black"))+
  geom_vline(xintercept =as.numeric(full_unsmoothed[location_id=='8310d870-fb13-414b-ba6c-2902eaf0276f' & quarter_year==2020.4,"gamma_invert"][1]), linetype="dashed", color = "blue", size=1)
ggsave("analysis_final/out/figures/02_06_middle_non_mon1_count.png", width=6, height=4, units="in")



ggplot(data=total_res[location_id=='6240c56b-a6fd-4c9a-bb4e-3d9aac3254e9' & quarter_year==2020.4 & gamma<=800])+geom_line(aes(x=gamma, y=count_emp))+
  ylab("# Skill Sets Employed")+xlab("Coordination Cost Parameter")+theme_bw() + theme(axis.text = element_text(size = 14),text = element_text(size = 20))+ 
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),panel.background = element_blank(), axis.line = element_line(colour = "black"))+
  geom_vline(xintercept =as.numeric(full_unsmoothed[location_id=='6240c56b-a6fd-4c9a-bb4e-3d9aac3254e9' & quarter_year==2020.4,"gamma_invert"][1]), linetype="dashed", color = "blue", size=1)
ggsave("analysis_final/out/figures/02_06_middle_non_mon2_count.png", width=6, height=4, units="in")


ggplot(data=total_res[location_id=='c7ea80fe-c02f-4534-ab57-f7188317ea64' & quarter_year==2018.1 & gamma<=800])+geom_line(aes(x=gamma, y=count_emp))+
  ylab("# Skill Sets Employed")+xlab("Coordination Cost Parameter")+theme_bw() + theme(axis.text = element_text(size = 14),text = element_text(size = 20))+ 
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),panel.background = element_blank(), axis.line = element_line(colour = "black"))+
  geom_vline(xintercept =as.numeric(full_unsmoothed[location_id=='c7ea80fe-c02f-4534-ab57-f7188317ea64' & quarter_year==2018.1,"gamma_invert"][1]), linetype="dashed", color = "blue", size=1)
ggsave("analysis_final/out/figures/02_06_middle_non_mon3_count.png", width=6, height=4, units="in")



## increasing
ggplot(data=total_res[location_id=='5e616d9e-259e-4ace-a25a-e5037683c78f' & quarter_year==2020.4 & gamma<=800])+geom_line(aes(x=gamma, y=count_emp))+
  ylab("# Skill Sets Employed")+xlab("Coordination Cost Parameter")+theme_bw() + theme(axis.text = element_text(size = 14),text = element_text(size = 20))+ 
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),panel.background = element_blank(), axis.line = element_line(colour = "black"))+
  geom_vline(xintercept =as.numeric(full_unsmoothed[location_id=='5e616d9e-259e-4ace-a25a-e5037683c78f' & quarter_year==2020.4,"gamma_invert"][1]), linetype="dashed", color = "blue", size=1)
ggsave("analysis_final/out/figures/02_06_increasing_count.png", width=6, height=4, units="in")



### old version and path of expansion
if (FALSE){
  
  full_unsmoothed[, gamma_quant:=ecdf(gamma_invert)(gamma_invert), by='county']
  
  full_unsmoothed[, groupname:= as.character(ceiling(gamma_quant/0.25)), by="county"]
  sub_patterns<-copy(full_unsmoothed)
  sub_patterns[, groupname:="Total"]
  sub_patterns<-rbind(sub_patterns,full_unsmoothed )
  roundmean<-function(x){
    return(round(mean(x),digits=3))
  }
  sub_patterns<-sub_patterns[,lapply(.SD, roundmean), by=c("county","groupname"), .SDcols=names(full_unsmoothed)[grep("ediff", names(full_unsmoothed))]]
  
  sub_patterns<-melt(sub_patterns, id.vars=c("county","groupname"),measure = patterns("^ediff"))
  sub_patterns[, enum:=str_replace_all(str_extract(variable   ,"_[0-9]_"), "_","")]
  sub_patterns[, eshock:=str_replace_all(str_extract(variable   ,"_[0-9]$"), "_","")]
  sub_patterns<-dcast(sub_patterns, county+groupname+enum~eshock)
  setnames(sub_patterns, "groupname", "Org. Cost. Quartile")
  setkey(sub_patterns, "county", "enum", "Org. Cost. Quartile")
  kable(sub_patterns[county=='6037',-"county"], "latex", align="c", booktabs=TRUE,linesep = c(""), escape = F, caption = NA, label=NA) %>%
    cat(., file = "analysis_final/out/tables/02_06_substitute_6037.tex")
  kable(sub_patterns[county=='36061',-"county"], "latex", align="c", booktabs=TRUE,linesep = c(""), escape = F, caption = NA, label=NA) %>%
    cat(., file = "analysis_final/out/tables/02_06_substitute_36061.tex")
  kable(sub_patterns[county=='17031',-"county"], "latex", align="c", booktabs=TRUE,linesep = c(""), escape = F, caption = NA, label=NA) %>%
    cat(., file = "analysis_final/out/tables/02_06_substitute_17031.tex")
  
  
  
  
total_res<-data.table()
for (i in 1:nrow(full_unsmoothed)){
  
  
  for (g in seq(from=20, to=2000, by=1)){
    
    res<-as.vector(get_demands(full_unsmoothed$task_mix_1[i], full_unsmoothed$task_mix_2[i], full_unsmoothed$task_mix_3[i], full_unsmoothed$task_mix_4[i], full_unsmoothed$task_mix_5[i], full_unsmoothed$county[i], g))
    
    res<-data.table(location_id=full_unsmoothed$location_id[i],quarter_year=full_unsmoothed$quarter_year[i],
                    county=full_unsmoothed$county[i], gamma=g,
                    E_1=res[1], E_2=res[2], E_3=res[3], E_4=res[4], E_5=res[5])
    total_res<-rbind(total_res, res)
  }
 
}

total_res[, count_emp:= (E_1>innertol) +(E_2>innertol)+(E_3>innertol)+(E_4>innertol)+(E_5>innertol) ]
collapsed<-total_res[, .(avg_count=mean(count_emp)), by=c("county", "gamma")]

ggplot(data=collapsed[county=='17031'])+geom_line(aes(x=gamma, y=avg_count))
}

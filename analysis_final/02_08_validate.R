### note to future self - validate using distribution of specialization

library('data.table')
library('lubridate')
library('stringr')
library('ggplot2')
library('gridExtra')
library('kableExtra')
library('matrixStats')

spec_log<-function(x)  ifelse(x==0 | is.nan(x),0,log(x))

get_internals<-function(a1, a2, a3, a4, a5, county,gamma){
  alpha<-c(a1, a2, a3, a4,a5)
  if (is.finite(gamma) & gamma>0){
    
    alpha<-c(a1, a2, a3, a4,a5)
    ## this function will return matrix given gamma
    A<-exp(-1/gamma*(tild_theta[[county]]) )
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
      B[which.min(tild_theta[[county]][,col]),col]<-alpha[col]
    }
  } else{
    # max frictions
    B<-matrix(0, ncol=5, nrow=5)
    B[which.min(rowSums(t(t(tild_theta[[county]])*alpha ))),]<-alpha
  }
  #E<-rowSums(B)
  
  
  return(as.vector(B))
}



all_results<-readRDS('analysis_final/data/02_00_parameters.rds')

market_parms<-all_results$coefficients
names(market_parms)<-all_results$parm_name

tild_theta<-vector(mode='list', length=3)
names(tild_theta)<-list("17031", "36061", "6037")
## be careful - the sweep command helps numerically but needs to be undone for some outcomes
for (cnty in names(tild_theta)){
    w_mat<-matrix(c(0,market_parms[grep(paste0(cnty,":avg_labor:E"),names(market_parms))]), ncol=5, nrow=5, byrow=FALSE)
    ## add in base wage
    skills<-matrix(market_parms[grep(paste0(cnty,":avg_labor:B"),names(market_parms))], ncol=5, nrow=5, byrow=FALSE)
    rho<-market_parms[grep(paste0(cnty,":cust_price$"),names(market_parms))]
    tild_theta[[cnty]]<-w_mat+(rho)^(-1)*skills
    tild_theta[[cnty]]<-sweep(tild_theta[[cnty]],2,apply(tild_theta[[cnty]],2,min))

}



full_unsmoothed<-readRDS('analysis_final/data/02_04_withgammas.rds')


innertol<-1e-08
for (i in 1:nrow(full_unsmoothed)){
  full_unsmoothed[i,c("B_1_1", "B_1_2", "B_1_3", "B_1_4", "B_1_5",
                      "B_2_1", "B_2_2", "B_2_3", "B_2_4", "B_2_5",
                      "B_3_1", "B_3_2", "B_3_3", "B_3_4", "B_3_5",
                      "B_4_1", "B_4_2", "B_4_3", "B_4_4", "B_4_5",
                      "B_5_1", "B_5_2", "B_5_3", "B_5_4", "B_5_5") := as.list(get_internals(task_mix_1, task_mix_2, task_mix_3, task_mix_4, task_mix_5, county, gamma_invert)) ]
  print(i)
}

## save out with internals
saveRDS(full_unsmoothed, 'analysis_final/data/02_06_data_for_counterfactuals.rds')

model_people<-melt(full_unsmoothed,measure.vars=colnames(full_unsmoothed)[grep("^B_[0-9]_[0-9]",colnames(full_unsmoothed))])
model_people[, type_num:=str_extract(variable,"[0-9]$")]
model_people[, task_num:=paste0("job_",str_extract(str_replace(variable,"_[0-9]$",""),"[0-9]$"))]
model_people[, emp_duration:=sum(value), by=c("location_id", "quarter_year", "type_num")]
model_people<-model_people[emp_duration>innertol*10,]
model_people[, value:=value/emp_duration]
model_people[, emp_duration:=emp_duration*tot_duration]

attach_model_people<-dcast(model_people, location_id+quarter_year+emp_duration+type_num+county~task_num)

model_people[, task_num:=str_extract(task_num,"[0-9]$")]
model_people<-merge(model_people,attach_model_people, by=c("location_id","quarter_year","emp_duration","type_num","county" ))


data_people<-readRDS("analysis_final/data/01_00_staff_task_full.rds")
colnames(data_people)[grep("^E_", colnames(data_people))]<-str_replace(colnames(data_people)[grep("^E_", colnames(data_people))], "E_", "job_")
data_people<-data_people[, .SD, .SDcols=c(colnames(data_people)[grep("^job_", colnames(data_people))],
                                          "location_id", "quarter_year", "county","staff_num","emp_duration")]
data_people[county=='06037', county:="6037"]

attach_data_people<-copy(data_people[, .SD, .SDcols=c("location_id", "quarter_year", "county","staff_num", colnames(data_people)[grep("^job", colnames(data_people))])])
data_people<-melt(data_people, id.vars=c("location_id", "quarter_year", "county","staff_num","emp_duration"), pattern="^job")
data_people[, task_num:=str_extract(variable,"[0-9]$")]
data_people[,group:="Data"]
model_people[, group:="Model"]
data_people<-merge(data_people, attach_data_people,by=c("location_id", "quarter_year", "county","staff_num") )
setnames(data_people, "staff_num", "type_num")
quarter_list<-c(2018.1, 2018.2, 2018.3, 2018.4, 2019.1, 2019.2, 2019.3, 2019.4, 2020.1, 2020.4, 2021.1, 2021.2)

all_data<-rbind(data_people[county %in% c("6037", "36061", "17031") &
                              quarter_year %in% quarter_list,
                            .SD, .SDcols=c("group","location_id", "quarter_year", "county","emp_duration", "value","type_num", "task_num",
                                                                       colnames(model_people)[grep("^job", colnames(model_people))])]
                ,model_people[,.SD, .SDcols=c("group","location_id", "quarter_year", "county","emp_duration", "value","type_num", "task_num",
                                 colnames(model_people)[grep("^job", colnames(model_people))])])

get_cor<-function(x,y,wt){
  dat<-cbind(x,y)
  res<-cov.wt(cbind(x,y),wt=wt,cor=TRUE)$cor[2,1]
  return(as.character(format(round(res, 3), nsmall = 3)))
  }


out_sample_moments<-all_data[,.(Variance=weightedVar(value,w=emp_duration),
                                `Cor. Task 1`=get_cor(job_1, value, emp_duration),
                                `Cor. Task 2`=get_cor(job_2, value, emp_duration),
                                `Cor. Task 3`=get_cor(job_3, value, emp_duration),
                                `Cor. Task 4`=get_cor(job_4, value, emp_duration),
                                `Cor. Task 5`=get_cor(job_5, value, emp_duration)
                                ),by=c("group","task_num")]
setorder(out_sample_moments,"task_num", -"group")
setnames(out_sample_moments, "group", "")
setnames(out_sample_moments, "task_num", "Task")

# white out bottom triangular
out_sample_moments[3:.N, `Cor. Task 1`:=""]
out_sample_moments[5:.N, `Cor. Task 2`:=""]
out_sample_moments[7:.N, `Cor. Task 3`:=""]
out_sample_moments[9:.N, `Cor. Task 4`:=""]

out_sample_moments[,Variance:=format(round(Variance,digits=3),nsmall=3)]

output<-kable(out_sample_moments, "latex", align="c", booktabs=TRUE,linesep = c(""), escape = F, caption = NA, label=NA) 
cat(output, file = "analysis_final/out/tables/02_08_validate_corr.tex")


toplot<-all_data[, .(max_time=max(value)), by=c("group", "county", "type_num", "quarter_year", "emp_duration")]
ggplot(toplot)+geom_density(aes(x=max_time,color=group,fill=group, weight=emp_duration), position='identity',alpha=0.5)+
  xlab("Max. Time on Any 1 Task")+ylab("Density")+
  theme_bw() + theme(axis.text = element_text(size = 14),text = element_text(size = 20))+ 
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),panel.background = element_blank(), axis.line = element_line(colour = "black"))

toplot<-unique(all_data[, c("county", "group", "emp_duration",
                            "job_1", "job_2", "job_3", "job_4", "job_5","type_num")])
toplot[, comb_3:=job_3+job_4+job_5]
ggplot(toplot)+geom_density(aes(x=job_1,color=group,fill=group, weight=emp_duration), position='identity',alpha=0.5)+
  xlab("Time on Haircut/Shave")+ylab("Density")+
  theme_bw() + theme(axis.text = element_text(size = 14),text = element_text(size = 20))+ 
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),panel.background = element_blank(), axis.line = element_line(colour = "black"))
ggsave("analysis_final/out/figures/02_08_marginal_cut.png", width=6, height=4, units="in")


ggplot(toplot)+geom_density(aes(x=job_2,color=group,fill=group, weight=emp_duration), position='identity',alpha=0.5)+
  xlab("Time on Color/Highlight/Wash")+ylab("Density")+
  theme_bw() + theme(axis.text = element_text(size = 14),text = element_text(size = 20))+ 
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),panel.background = element_blank(), axis.line = element_line(colour = "black"))
ggsave("analysis_final/out/figures/02_08_marginal_color.png", width=6, height=4, units="in")

ggplot(toplot)+geom_density(aes(x=comb_3,color=group,fill=group, weight=emp_duration), position='identity',alpha=0.5)+
  xlab("Time on Other 3 Tasks")+ylab("Density (weighted by hours)")+
  theme_bw() + theme(axis.text = element_text(size = 14),text = element_text(size = 20))+ 
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),panel.background = element_blank(), axis.line = element_line(colour = "black"))
ggsave("analysis_final/out/figures/02_08_marginal_other3.png", width=6, height=4, units="in")


toplot<-toplot[,list(job_1=rep(job_1,round(emp_duration/60)),job_2=rep(job_2,round(emp_duration/60)),
                     job_3=rep(job_3,round(emp_duration/60)),job_4=rep(job_4,round(emp_duration/60)),
                     job_5=rep(job_5,round(emp_duration/60)),
                     comb_3=rep(job_3+job_4+job_5,round(emp_duration/60)),
                     group=rep(group,round(emp_duration/60)),county=rep(county,round(emp_duration/60)))]

ggplot(toplot)+geom_density_2d(aes(x=job_1,y=job_2),bins=50,color="black")+
  xlab("Time on Haircut/Shave")+ylab("Time on Color/Highlight/Wash")+
  theme_bw() + theme(axis.text = element_text(size = 14),text = element_text(size = 20))+ 
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),panel.background = element_blank(), axis.line = element_line(colour = "black"))+
  facet_wrap(~group)
ggsave("analysis_final/out/figures/02_08_bivariate_cut_color.png", width=10, height=6, units="in")


ggplot(toplot)+geom_density_2d(aes(x=job_1,y=comb_3),bins=50,color="black")+
  xlab("Time on Haircut/Shave")+ylab("Time on Other Three Tasks")+
  theme_bw() + theme(axis.text = element_text(size = 10),text = element_text(size = 20))+ 
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),panel.background = element_blank(), axis.line = element_line(colour = "black"))+
  facet_wrap(~group)

ggsave("analysis_final/out/figures/02_08_bivariate_cut_other3.png", width=10, height=6, units="in")


ggplot(toplot)+geom_density_2d(aes(x=job_2,y=comb_3),bins=50,color="black")+
  ylab("Time on Other Three Tasks")+xlab("Time Spent on Color/Highlight/Wash")+
  theme_bw() + theme(axis.text = element_text(size = 10),text = element_text(size = 20))+ 
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),panel.background = element_blank(), axis.line = element_line(colour = "black"))+
  facet_wrap(~group)
ggsave("analysis_final/out/figures/02_08_bivariate_color_other3.png", width=10, height=6, units="in")



## make estimate graphs
library('data.table')
library('gmm')
library('ggplot2')
library('spatstat')
theme_set(theme_bw(base_size=22))
############### import results
load('data/02_00_est_res_la.RData')

wage_bound<-Vectorize(function(x){
  return(195/(exp(-x)+1)+5)
})

###### create objects
tau<-1
core_count<-1
source('big_gmm_objective.R')

###temp chunk
load('data/02_00_progress.RData')
maxrefine<-res_store
rho<-coef(maxrefine)[1]
ny_objects<-ret_objects(coef(maxrefine), estim_matrix)
###############

### parameter table
library(knitr)
library(kableExtra)
sigfig <- function(vec, n=3){ 
  ### function to round values to N significant digits
  # input:   vec       vector of numeric
  #          n         integer is the required sigfig  
  # output:  outvec    vector of numeric rounded to N sigfig
  
  formatC(signif(vec,digits=n), digits=n,format="fg", flag="#") 
  
}      # end of function   sigfig
df <- data.frame(Parameter=c("Price Sensitivity", "Haircut/Shave Skill",
                             "Color/Highlight/Wash Skill", "Extensions Skill",
                             "Blowdry/Style/Treatment Skill", "Administrative Skill",
                             "Nail/Spa/Eye/Misc. Skill"),Symbol=c("$\\rho$","$S_1$", "$S_2$", "$S_3$", "$S_4$", "$S_5$", "$S_6$"), Estimate=sigfig (as.numeric(coef(maxrefine))[c(1,77:82)],4))
kable(df, "latex", align="c", booktabs=TRUE,linesep = c(""), escape = F, caption = 'Price Sensitivity and Worker Skill Levels') %>%
  cat(., file = "out/02_03_parms_la.tex")



#  footnote(general="\\\\small{This table displays the consumer price sensitivity and worker skill level estimates for Manhattan. These paramaters are assumed constant over the analysis period.}", 
#escape=FALSE, threeparttable = TRUE)%>%

### wages table
df <- data.frame(Type=rep(c("Haircut/Shave Specialist","Color/Highlight/Wash Specialist", "Extensions Specialist",
                             "Blowdry/Style/Treatment Specialist", "Administrative Specialist",
                             "Nail/Spa/Eye/Misc. Specialist"),4),Quarter=c(rep("2019-Q2",6),rep("2019-Q3",6),rep("2019-Q4",6),rep("2020-Q1",6)), 
                 Estimate=sigfig (wage_bound(as.numeric(coef(maxrefine))[4:27]),4 ))
kable(df, "latex", align="c", booktabs=TRUE,linesep = c("", "", "","","", '\\addlinespace'), escape = F, caption = 'Wage Estimates') %>%
  cat(., file = "out/02_03_wages_la.tex")

### plot gamma

ggplot(data=data.frame(gamma=ny_objects$gamma[estim_matrix[,1]>0]),aes(x=ny_objects$gamma[estim_matrix[,1]>0])) +
  geom_histogram(color="black", fill="lightblue", size=1)+ ylab("Establishment-Quarter Count") + xlab("Internal Organization Cost Parameter")+ theme(legend.position = "none")
ggsave("out/figures/02_03_gamma_la.png", width=12, heigh=6, units="in")


ggplot(data=data.frame(gamma=log(ny_objects$gamma[estim_matrix[,1]>0], base=10)),aes(x=log(ny_objects$gamma[estim_matrix[,1]>0],base=10))) +
  geom_histogram(color="black", fill="lightblue", size=1)+ ylab("Establishment-Quarter Count") + xlab("Log Base 10 Internal Organization Cost Parameter")+ theme(legend.position = "none")
ggsave("out/figures/02_03_log_gamma_la.png", width=12, heigh=6, units="in")


### moments
moments<-gmm_obj(coef(maxrefine), estim_matrix)

df <- data.frame(Moment=c("$\\mathbb{E}[\\phi]$","$\\mathbb{E}[\\nu]$", "$\\mathbb{E}[\\phi I]$", "$\\mathbb{E}[\\nu I]$"), Estimate=sigfig (moments[c(1,73,7,79)],4))
kable(df, "latex", align="c", booktabs=TRUE,linesep = c(""), escape = F, caption = 'Price Sensitivity and Worker Skill Levels') %>%
  cat(., file = "out/02_02_moments_ny.tex")

ggplot() +
  geom_point(aes(y=rank(ny_objects$quals[estim_matrix[,1]>0])/length(ny_objects$quals[estim_matrix[,1]>0])*100, x=rank(ny_objects$gammas[estim_matrix[,1]>0])/length(ny_objects$gammas[estim_matrix[,1]>0])*100 ),color="black")+ 
  ylab("Quality Percentile") + xlab("Gamma Percentile")+ theme(legend.position = "none")

ggplot() +
  geom_point(aes(y=rank(ny_objects$quals[estim_matrix[,1]>0])/length(ny_objects$quals[estim_matrix[,1]>0])*100, x=rank(estim_matrix[estim_matrix[,1]>0,1])/length(estim_matrix[estim_matrix[,1]>0,1])*100 ),color="black")+ 
  ylab("Quality Percentile") + xlab("Complexity Percentile")+ theme(legend.position = "none")


ggplot() +
  geom_point(aes(y=rank(ny_objects$gammas[estim_matrix[,1]>0])/length(ny_objects$gammas[estim_matrix[,1]>0])*100, x=rank(estim_matrix[estim_matrix[,1]>0,1])/length(estim_matrix[estim_matrix[,1]>0,1])*100 ),color="black")+ 
  ylab("Gamma Percentile") + xlab("Complexity Percentile")+ theme(legend.position = "none")


###job distribution - model based.
jobs_model<-c()
for (y in 1:length(ny_objects$bmats)){
  piece<-cbind(ny_objects$bmats[[y]]/rowSums(ny_objects$bmats[[y]]),rep(as.numeric(estim_sample[y,"cust_count"]*estim_sample[y,"avg_labor"]),6),rowSums(ny_objects$bmats[[y]]),
               estim_sample[y,]$quarter_year)
  jobs_model<-rbind(jobs_model,piece)
}
jobs_model<-data.table(jobs_model)
names(jobs_model)<-c("jobvect_1", "jobvect_2", "jobvect_3", "jobvect_4", "jobvect_5", "jobvect_6",
                     "tot_firm", "type_frac", "quarter_year")
jobs_model[,emp_tot:=tot_firm*type_frac]
jobs_model<-jobs_model[type_frac>1e-08]
jobs_model[,Source:="Model"]

### job distribution - empirical
jobs<-readRDS("data/00_00_job_quarter.rds")
helper<-readRDS("data/00_00_firm_quarter.rds")
helper<-unique(helper[,c("location_id", "county")])
jobs<-merge(jobs,helper,by="location_id", all.x=TRUE)
jobs<-jobs[quarter_year %in% c(2019.3,2019.4, 2021.1, 2021.2) & county %in% c(6037)]
jobs[, Source:="Observed"]

tokeep<-c("jobvect_1", "jobvect_2", "jobvect_3", "jobvect_4", "jobvect_5", "jobvect_6", "emp_tot", "Source", "quarter_year")

jobs_both<-rbind(jobs[,..tokeep], jobs_model[,..tokeep])
ggplot(jobs_both[quarter_year==2021.2],aes(x = jobvect_1, weight = emp_tot/sum(emp_tot), fill=Source) ) + geom_density(alpha=0.4)+
  labs(x="% Time Hair/Shave Task")+
  theme(legend.position="none")
ggsave("out/figures/02_02_jobdist_1.png", width=12, heigh=6, units="in")

ggplot(jobs_both[quarter_year==2021.2],aes(x = jobvect_2, weight = emp_tot/sum(emp_tot), fill=Source) ) + geom_density(alpha=0.4)+
  labs(x="% Time Color/Highlight/Wash Task")+
  theme(legend.position="none")
ggsave("out/figures/02_02_jobdist_2.png", width=12, heigh=6, units="in")

ggplot(jobs_both[quarter_year==2021.2],aes(x = jobvect_3, weight = emp_tot/sum(emp_tot), fill=Source) ) + geom_density(alpha=0.4)+
  labs(x="% Time Extension Task")+coord_cartesian(xlim=c(0,0.08))+
  theme(legend.position="none")
ggsave("out/figures/02_02_jobdist_3.png", width=12, heigh=6, units="in")


ggplot(jobs_both[quarter_year==2021.2],aes(x = jobvect_4, weight = emp_tot/sum(emp_tot), fill=Source) ) + geom_density(alpha=0.4)+
  labs(x="% Time Blowdry/Style/Treatment Task")+
  theme(legend.position="none")
ggsave("out/figures/02_02_jobdist_4.png", width=12, heigh=6, units="in")

ggplot(jobs_both[quarter_year==2021.2],aes(x = jobvect_5, weight = emp_tot/sum(emp_tot), fill=Source) ) + geom_density(alpha=0.4)+
  labs(x="% Time Administrative Task")+coord_cartesian(xlim=c(0,0.08))+
  theme(legend.position="none")
ggsave("out/figures/02_02_jobdist_5.png", width=12, heigh=6, units="in")

ggplot(jobs_both[quarter_year==2021.2],aes(x = jobvect_6, weight = emp_tot/sum(emp_tot), fill=Source) ) + geom_density(alpha=0.4)+
  labs(x="% Time Nail/Spa/Eye/Misc. Task")+coord_cartesian(xlim=c(0,0.15))+
  theme(legend.position="none")
ggsave("out/figures/02_02_jobdist_6.png", width=12, heigh=6, units="in")

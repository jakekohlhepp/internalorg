
## make estimate graphs
library('data.table')
library('gmm')
library('ggplot2')
library('spatstat')
library('knitr')
library('kableExtra')
library('stargazer')
theme_set(theme_bw(base_size=22))
############### import results
load('data/01_01_progress.RData')
#### general functions###########
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
wage_bound<-Vectorize(function(x){
  return(185/(exp(-x)+1)+15)
})
#################################

if (get_os()=="windows"){
  core_count<-1
} else{
  core_count<-32
} 

source('small_gmm_objective.R')

###temp chunk
maxrefine<-res_store
ny_objects<-ret_objects(coef(maxrefine), estim_matrix)
###############

### parameter table

sigfig <- function(vec, n=3){ 
  ### function to round values to N significant digits
  # input:   vec       vector of numeric
  #          n         integer is the required sigfig  
  # output:  outvec    vector of numeric rounded to N sigfig
  
  formatC(signif(vec,digits=n), digits=n,format="fg", flag="#") 
  
}      # end of function   sigfig

temp<-summary(res_store)
df <- data.frame(Parameter=c("Price Sensitivity", "Haircut/Shave Skill",
                             "Color/Highlight/Wash Skill", "Extensions Skill",
                             "Blowdry/Style/Treatment Skill", "Administrative Skill",
                             "Nail/Spa/Eye/Misc. Skill"),Symbol=c("$\\rho$","$S_1$", "$S_2$", "$S_3$", "$S_4$", "$S_5$", "$S_6$"), Estimate=sigfig (as.numeric(temp$coefficients[,1])[c(1,20:25)],4))

kable(df, "latex", align="c", booktabs=TRUE,linesep = c(""), escape = F, caption = 'Price Sensitivity and Worker Skill Levels') %>%
  cat(., file = "out/02_02_parms_ny.tex")
print(df)

## average own price elasticity is

mean(coef(res_store)[1]*tau*estim_sample$cust_price*(1-estim_sample$salon_share_subdiv))

#  footnote(general="\\\\small{This table displays the consumer price sensitivity and worker skill level estimates for Manhattan. These paramaters are assumed constant over the analysis period.}", 
#escape=FALSE, threeparttable = TRUE)%>%

### wages table
df <- data.frame(Type=rep(c("Haircut/Shave Specialist","Color/Highlight/Wash Specialist", "Extensions Specialist",
                             "Blowdry/Style/Treatment Specialist", "Administrative Specialist",
                             "Nail/Spa/Eye/Misc. Specialist")), 
                 Estimate= sigfig (wage_bound(as.numeric(temp$coefficients[,1])[4:9]),4 ))
kable(df, "latex", align="c", booktabs=TRUE,linesep = c("", "", "","","", '\\addlinespace'), escape = F, caption = 'Wage Estimates') %>%
  cat(., file = "out/02_02_wages_ny.tex")
print(df)
mod_stargazer <- function(...){
  output <- capture.output(stargazer(...))
  # The first three lines are the ones we want to remove...
  output <- output[5:(length(output)-1)]
  # cat out the results - this is essentially just what stargazer does too
  cat(paste(output, collapse = "\n"), "\n")
}

mod_stargazer(res_store, keep=c(1,4:9, 20:25),out.header=FALSE,header=FALSE,dep.var.caption="",
              covariate.labels=c("Price Sensitivity","Haircut/Shave Wage","Color/Highlight/Wash Wage", "Extensions Wage",
                                 "Blowdry/Style/Treatment Wage", "Administrative Wage",
                                 "Nail/Spa/Eye/Misc. Wage","Haircut/Shave Skill",
                                 "Color/Highlight/Wash Skill", "Extensions Skill",
                                 "Blowdry/Style/Treatment Skill", "Administrative Skill",
                                 "Nail/Spa/Eye/Misc. Skill"), out="out/tables/02_02_model_withse.tex")

########## In sample fit: market share and price by complexity.


estim_sample[, weight:=(1-outside_share)/(sum(salon_share_subdiv)), by=c("county","quarter_year")]


modelstuff<-data.table(price=as.numeric(1/(1-estim_matrix[,8])/coef(res_store)[1]/tau + ny_objects$C-ny_objects$phi),
                    share=as.numeric(-coef(res_store)[1]*tau*estim_matrix[,7] + ny_objects$Q-ny_objects$nu),
                    complexity=estim_matrix[,1],
                    taskmix2=estim_matrix[,2],
                    Source="Model")
modelstuff[, revenue:=price*exp(share)/(estim_sample$weight[1]*sum(exp(share))+1)*estim_sample$CSPOP[1]]
datastuff<-data.table(price=estim_matrix[,7],
                     share=log(estim_matrix[,8]/estim_matrix[,9]),
                     complexity=estim_matrix[,1],
                     taskmix2=estim_matrix[,2],
                     Source="Observed",
                     revenue=estim_sample$revenue)
forplot<-rbind(modelstuff,datastuff )

ggplot(data=forplot, aes(x=complexity, y=price, color=Source))+ geom_smooth(method = "loess", se=FALSE)
ggplot(data=forplot, aes(x=complexity, y=share, color=Source))+ geom_smooth(method = "loess", se=FALSE)

##########################################


#### get task-specialization
task_spec<-sapply(1:nrow(estim_sample),function(x) {sum(diag(ny_objects$bmats[[x]]))})

ggplot() +
  geom_point(aes(y=task_spec[estim_matrix[,1]>0], x=rank(ny_objects$gammas[estim_matrix[,1]>0])/length(ny_objects$gammas[estim_matrix[,1]>0])*100 ),color="black", size=3)+ 
  ylab("Task Specialization") + xlab("Organization Cost Percentile")+ theme(legend.position = "none")
ggsave("out/figures/02_02_taskspec_ny.png", width=12, heigh=6, units="in")
mean(task_spec[estim_matrix[,1]>0][rank(ny_objects$gammas[estim_matrix[,1]>0])/length(ny_objects$gammas[estim_matrix[,1]>0])*100>75 ])
mean(task_spec[estim_matrix[,1]>0][rank(ny_objects$gammas[estim_matrix[,1]>0])/length(ny_objects$gammas[estim_matrix[,1]>0])*100<25 ])

ggplot() +
  geom_point(aes(y=ny_objects$Q[estim_matrix[,1]>0], x=rank(ny_objects$gammas[estim_matrix[,1]>0])/length(ny_objects$gammas[estim_matrix[,1]>0])*100 ),color="black")+ 
  ylab("Product Quality") + xlab("Organization Cost Percentile")+ theme(legend.position = "none")
ggsave("out/figures/02_02_quality_gamma_ny.png", width=12, heigh=6, units="in")

ggplot() +
  geom_point(aes(y=ny_objects$C[estim_matrix[,1]>0], x=rank(ny_objects$gammas[estim_matrix[,1]>0])/length(ny_objects$gammas[estim_matrix[,1]>0])*100 ),color="black")+ 
  ylab("Marginal Cost ($)") + xlab("Organization Cost Percentile")+ theme(legend.position = "none")
ggsave("out/figures/02_02_cost_gamma_ny.png", width=12, heigh=6, units="in")

ggplot() +
  geom_point(aes(y=estim_sample$cust_price[estim_matrix[,1]>0], x=rank(ny_objects$gammas[estim_matrix[,1]>0])/length(ny_objects$gammas[estim_matrix[,1]>0])*100 ),color="black")+ 
  ylab("Product Price ($)") + xlab("Organization Cost Percentile")+ theme(legend.position = "none")
ggsave("out/figures/02_02_price_gamma_ny.png", width=12, heigh=6, units="in")


ggplot() +
  geom_point(aes(y=rank(ny_objects$gammas[estim_matrix[,1]>0])/length(ny_objects$gammas[estim_matrix[,1]>0])*100, x=rank(estim_matrix[estim_matrix[,1]>0,1])/length(estim_matrix[estim_matrix[,1]>0,1])*100 ),color="black", size=3)+ 
  ylab("Complexity Percentile") + xlab("Organization Cost Percentile")+ theme(legend.position = "none")
ggsave("out/figures/02_02_complexity_gamma.png", width=12, heigh=6, units="in")


### plot gamma

ggplot(data=data.frame(gamma=ny_objects$gamma[estim_matrix[,1]>0]),aes(x=ny_objects$gamma[estim_matrix[,1]>0])) +
  geom_histogram(color="black", fill="lightblue", size=1)+ ylab("Establishment-Quarter Count") + xlab("Internal Organization Cost Parameter")+ theme(legend.position = "none")
ggsave("out/figures/02_02_gamma_nycounty.png", width=12, heigh=6, units="in")


ggplot(data=data.frame(gamma=log(ny_objects$gamma[estim_matrix[,1]>0], base=10)),aes(x=log(ny_objects$gamma[estim_matrix[,1]>0],base=10))) +
  geom_histogram(color="black", fill="lightblue", size=1)+ ylab("Establishment-Quarter Count") + xlab("Log Base 10 Internal Organization Cost Parameter")+ theme(legend.position = "none")
ggsave("out/figures/02_02_log_gamma_nycounty.png", width=12, heigh=6, units="in")

### the cost of implementing the median organization structure


ggplot() +
  geom_point(aes(y=(ny_objects$gammas[ny_objects$gammas<2000]*median(estim_sample$s_index[ny_objects$gammas<2000])), x=(rank(ny_objects$gammas[ny_objects$gammas<2000])/length(ny_objects$gammas[ny_objects$gammas<2000])*100 )),size=3,color="black")+ 
  ylab("Hourly Cost of Median Org. ($)") + xlab("Firm Percentile")+ theme(legend.position = "none")
ggsave("out/figures/02_02_org_costs_median.png", width=12, heigh=6, units="in")

## org costs as fraction of marginal costs
summary(ny_objects$gammas*estim_sample$s_index*estim_sample$avg_labor/ny_objects$C)
eq_org<-ny_objects$gammas*estim_sample$s_index*estim_sample$avg_labor
summary(lm(ny_objects$C~eq_org))
var(eq_org)/var(ny_objects$C)
### moments
moments<-colMeans(res_store$gt)[c(1,14,7,20)]

df <- data.frame(Moment=c("$\\mathbb{E}[\\phi]$","$\\mathbb{E}[\\nu]$", "$\\mathbb{E}[\\phi I]$", "$\\mathbb{E}[\\nu I]$"), Estimate=sigfig (moments,4))
kable(df, "latex", align="c", booktabs=TRUE,linesep = c(""), escape = F, caption = 'Targeted Moments') %>%
  cat(., file = "out/02_02_moments_ny.tex")

###job distribution - model based.
jobs_model<-c()
for (y in 1:length(ny_objects$bmats)){
  piece<-cbind(ny_objects$bmats[[y]]/rowSums(ny_objects$bmats[[y]]),rep(as.numeric(estim_sample[y,"cust_count"]*estim_sample[y,"avg_labor"]),6),rowSums(ny_objects$bmats[[y]]),
               estim_sample[y,]$quarter_year, 1:6, estim_sample[y,"location_id"],ny_objects$gammas[y] )
  jobs_model<-rbind(jobs_model,piece)
}
jobs_model<-data.table(jobs_model)
names(jobs_model)<-c("jobvect_1", "jobvect_2", "jobvect_3", "jobvect_4", "jobvect_5", "jobvect_6",
                     "tot_firm", "type_frac", "quarter_year", "type", "location_id", "gamma")
jobs_model[,emp_tot:=tot_firm*type_frac]
jobs_model<-jobs_model[type_frac>1e-08]
jobs_model[,Source:="Model"]

### job distribution - empirical
jobs<-readRDS("data/00_00_job_quarter.rds")
jobs<-jobs[quarter_year %in% c(2021.2) & county %in% c("36061")]
jobs[, Source:="Observed"]
jobs[, gamma:=NA]
tokeep<-c("jobvect_1", "jobvect_2", "jobvect_3", "jobvect_4", "jobvect_5", "jobvect_6", "emp_tot", "Source", "quarter_year", "location_id", "gamma")

jobs_both<-rbind(jobs[,..tokeep], jobs_model[,..tokeep])

ggplot(jobs_both,aes(x = 100*jobvect_1, w = emp_tot, fill=Source) ) + geom_density(alpha=0.4)+
  labs(x="% Time Hair/Shave Task", y="Density")+
  theme(legend.position="none")
ggsave("out/figures/02_02_jobdist_1.png", width=12, heigh=6, units="in")

ggplot(jobs_both,aes(x = 100*jobvect_2, weight = emp_tot/sum(emp_tot), fill=Source) ) + geom_density(alpha=0.4)+
  labs(x="% Time Color/Highlight/Wash Task", y="Density")+
  theme(legend.position="none")
ggsave("out/figures/02_02_jobdist_2.png", width=12, heigh=6, units="in")

ggplot(jobs_both,aes(x = 100*jobvect_3, weight = emp_tot/sum(emp_tot), fill=Source) ) + geom_density(alpha=0.4)+
  labs(x="% Time Extension Task", y="Density")+coord_cartesian(xlim=c(0,10))+
  theme(legend.position="none")
ggsave("out/figures/02_02_jobdist_3.png", width=12, heigh=6, units="in")


ggplot(jobs_both,aes(x = 100*jobvect_4, weight = emp_tot/sum(emp_tot), fill=Source) ) + geom_density(alpha=0.4)+
  labs(x="% Time Blowdry/Style/Treatment Task", y="Density")+
  theme(legend.position="none")
ggsave("out/figures/02_02_jobdist_4.png", width=12, heigh=6, units="in")

ggplot(jobs_both,aes(x = 100*jobvect_5, weight = emp_tot/sum(emp_tot), fill=Source) ) + geom_density(alpha=0.4)+
  labs(x="% Time Administrative Task", y="Density")+coord_cartesian(xlim=c(0,10))+
  theme(legend.position="none")
ggsave("out/figures/02_02_jobdist_5.png", width=12, heigh=6, units="in")

ggplot(jobs_both,aes(x = 100*jobvect_6, weight = emp_tot/sum(emp_tot), fill=Source) ) + geom_density(alpha=0.4)+
  labs(x="% Time Nail/Spa/Eye/Misc. Task", y="Density")+coord_cartesian(xlim=c(0,20))+
  theme(legend.position="none")
ggsave("out/figures/02_02_jobdist_6.png", width=12, heigh=6, units="in")


ggplot(jobs_model[type==1] ) + geom_density(aes(x = 100*jobvect_1, weight = emp_tot/sum(emp_tot), fill="Cut"),alpha=0.4)+
  geom_density(aes(x = 100*jobvect_2, weight = emp_tot/sum(emp_tot), fill="Color/Highlight"),alpha=0.4)+
  geom_density(aes(x = 100*jobvect_4, weight = emp_tot/sum(emp_tot), fill="Blowdry"),alpha=0.4)+
  labs(x="% Time", y="Density")+
  theme(legend.position="bottom")
ggsave("out/figures/02_02_cut_specialist.png", width=12, heigh=8, units="in")


ggplot(jobs_model[type==2] ) + geom_density(aes(x = 100*jobvect_1, weight = emp_tot/sum(emp_tot), fill="Cut"),alpha=0.4)+
  geom_density(aes(x = 100*jobvect_2, weight = emp_tot/sum(emp_tot), fill="Color/Highlight"),alpha=0.4)+
  geom_density(aes(x = 100*jobvect_4, weight = emp_tot/sum(emp_tot), fill="Blowdry"),alpha=0.4)+
  labs(x="% Time", y="Density")+
  theme(legend.position="bottom")
ggsave("out/figures/02_02_color_specialist.png", width=12, heigh=8, units="in")


ggplot(jobs_model[type==4] ) + geom_density(aes(x = 100*jobvect_1, weight = emp_tot/sum(emp_tot), fill="Cut"),alpha=0.4)+
  geom_density(aes(x = 100*jobvect_2, weight = emp_tot/sum(emp_tot), fill="Color/Highlight"),alpha=0.4)+
  geom_density(aes(x = 100*jobvect_4, weight = emp_tot/sum(emp_tot), fill="Blowdry"),alpha=0.4)+
  labs(x="% Time", y="Density")+
  theme(legend.position="bottom")
ggsave("out/figures/02_02_blowdry_specialist.png", width=12, heigh=8, units="in")


cust_w_mean<-function(x,w){
  return(sum(x*w)/sum(w))
}

cust_w_var<-function(x,w){
  return(sum(w*(x-cust_w_mean(x,w))^2)/sum(w))
}

cust_var<-function(x){
  return(sum((x-mean(x))^2)/length(x))
}

colA = paste("jobvect", 1:6, sep = "_")

### model fit: tot variance in job task content, firm-based variance in job task-content.
jobs_both[, temp_id:=1:.N, by=c("Source", "location_id")]
fortable<-melt(jobs_both, id.vars=c("location_id", "Source", "emp_tot", "temp_id"), measure.vars=list(colA), value.name=c("jobvect"))
fortable[,task:=(gsub("jobvect_", "", variable)) ]
aug_table<-fortable[,.(tot_var=cust_w_var(jobvect, w=emp_tot)), by=c("task", "Source")]
fortable<-fortable[,.(firm_var=cust_w_mean(jobvect, w=emp_tot), tot=sum(emp_tot)) ,by=c("task","location_id", "Source")]
fortable<-fortable[,.(firm_var=cust_w_var(firm_var, w=tot)) ,by=c("task", "Source")]
fortable<-merge(aug_table,fortable, by=c("task", "Source"))
fortable<-dcast(fortable, task~Source, value.var=c("tot_var", "firm_var"))
fortable[, (names(fortable[,-1])):=lapply(.SD,round,digits=4), .SDcols=names(fortable[,-1])]
names(fortable)<-c("Task", "Model", "Observed", "Model", "Observed")
kable(fortable, "latex", align="c", booktabs=TRUE,linesep = c(""), escape = F, caption = 'Model vs. Observed Job Task Variance', label="variance") %>%
  add_header_above(., c(" ", "Total Variance" = 2, "Between Firm Variance" = 2)) %>%
  cat(., file = "out/tables/02_02_variance_validate.tex")


### model fit: mean, median of task content
jobs_both[, temp_id:=1:.N, by=c("Source", "location_id")]
fortable<-melt(jobs_both, id.vars=c("location_id", "Source", "emp_tot", "temp_id"), measure.vars=list(colA), value.name=c("jobvect"))
fortable[,task:=(gsub("jobvect_", "", variable)) ]
fortable<-fortable[,.(mean=cust_w_mean(jobvect, w=emp_tot),median=weighted.median(jobvect, w=emp_tot)), by=c("task", "Source")]


fortable<-dcast(fortable, task~Source, value.var=c("mean", "median"))
fortable[, (names(fortable[,-1])):=lapply(.SD,round,digits=4), .SDcols=names(fortable[,-1])]
names(fortable)<-c("Task", "Model", "Observed", "Model", "Observed")
kable(fortable, "latex", align="c", booktabs=TRUE,linesep = c(""), escape = F, caption = 'Model vs. Observed Job Task Mean and Median', label="median") %>%
  add_header_above(., c(" ", "Mean" = 2, "Median" = 2)) %>%
  cat(., file = "out/tables/02_02_median_validate.tex")



jobs_both[, temp_id:=1:.N, by=c("Source", "location_id")]
fortable<-melt(jobs_both, id.vars=c("location_id", "Source", "emp_tot", "temp_id"), measure.vars=list(colA), value.name=c("jobvect"))
fortable[,task:=(gsub("jobvect_", "", variable)) ]
fortable<-fortable[,.(p25=weighted.quantile(jobvect, w=emp_tot,p=0.25),p75=weighted.quantile(jobvect, w=emp_tot,p=0.75)), by=c("task", "Source")]


fortable<-dcast(fortable, task~Source, value.var=c("p25", "p75"))
fortable[, (names(fortable[,-1])):=lapply(.SD,round,digits=4), .SDcols=names(fortable[,-1])]
names(fortable)<-c("Task", "Model", "Observed", "Model", "Observed")
kable(fortable, "latex", align="c", booktabs=TRUE,linesep = c(""), escape = F, caption = 'Model vs. Observed Job Task IQR', label="median") %>%
  add_header_above(., c(" ", "p25" = 2, "p75" = 2)) %>%
  cat(., file = "out/tables/02_02_ptile_validate.tex")

### decompose variance into worker type vs firm.
### total is the same
### then variance across types.




fortable<-melt(jobs_model, id.vars=c("location_id", "type", "emp_tot"), measure.vars=list(colA), value.name=c("jobvect"))
aug_table<-fortable[,.(tot_var=cust_w_var(jobvect, w=emp_tot)), by="variable"]
fortable<-fortable[,.(firm_var=cust_w_var(jobvect, w=emp_tot),type_var=cust_w_mean(jobvect, w=emp_tot), tot=sum(emp_tot)) ,by=c("variable", "type")]
fortable<-fortable[,.(firm_var=sum(firm_var*tot)/sum(tot), type_var=cust_w_var(type_var, w=tot)) ,by=c("variable")]
fortable<-merge(aug_table,fortable, by="variable")
fortable[, firm_var:=firm_var/tot_var]
fortable[, type_var:=type_var/tot_var]
fortable[, tot_var:=NULL]
fortable[,variable:=(gsub("jobvect_", "", variable)) ]
fortable[, (names(fortable[,-1])):=lapply(.SD,round,digits=4), .SDcols=names(fortable[,-1])]
names(fortable)<-c("Task", "Firm", "Worker")
kable(fortable, "latex", align="c", booktabs=TRUE,linesep = c(""), escape = F, caption = 'Decomposition of Job Task-Content Variance', label="vardecomp") %>%
  add_header_above(., c(" ", "Share of Task-Content Variance" = 2)) %>%
  cat(., file = "out/tables/02_02_variance_decomp.tex")


## focal task
fortable<-melt(jobs_model, id.vars=c("location_id", "type", "emp_tot"), measure.vars=list(colA), value.name=c("jobvect"))
fortable[,task:=as.numeric(gsub("jobvect_", "", variable)) ]
fortable<-fortable[task==type]
aug_table<-fortable[,.(tot_var=cust_w_var(jobvect, w=emp_tot))]
fortable<-fortable[,.(firm_var=cust_w_var(jobvect, w=emp_tot),type_var=cust_w_mean(jobvect, w=emp_tot), tot=sum(emp_tot)) ,by=c("type")]
fortable<-fortable[,.(firm_var=sum(firm_var*tot)/sum(tot), type_var=cust_w_var(type_var, w=tot))]
fortable<-cbind(aug_table,fortable)
fortable[, firm_var:=firm_var/tot_var]
fortable[, type_var:=type_var/tot_var]
fortable[, tot_var:=NULL]

fortable[, (names(fortable[,-1])):=lapply(.SD,round,digits=4), .SDcols=names(fortable[,-1])]
print(fortable)


## create job space statistics and plots.

library('checkpoint')
#checkpoint('2020-07-16') activate to reproduce
library('data.table')
library('lubridate')
library('stringr')
library('zoo')
library('binsreg')
library('ggplot2')
library('lessR')
library('stargazer')
library('knitr')
library('kableExtra')
theme_set(theme_bw(base_size=22))
get_midpoint <- Vectorize(function(cut_label) {
  mean(as.numeric(unlist(strsplit(gsub("\\(|\\)|\\[|\\]", "", as.character(cut_label)), ","))))
})

range_val<-function(x){
  return(range(x)[2]-range(x)[1])
}

jobs<-readRDS("data/00_00_job_quarter.rds")
### collapse to firm
# exclude one salon in KY which has 22,000,000 in revenue.
jobs<-jobs[location_id!='fb686b3a-a166-469b-88ea-3467a68e2f53',]
# exclude partial quarter
jobs<-jobs[quarter_year!=2021.3,]

stopifnot(uniqueN(jobs[,c("staff_num", "location_id", "quarter_year")])==nrow(jobs))



## make scatter plot.
ggplot(data=jobs, aes(x=jobvect_1,y=jobvect_2, color=as.numeric(jobvect_4) ))+
  geom_point(size=1)+  ylab("% Time Color") + xlab("% Time Haircut")+ labs(color='% Time Blowdry')
ggsave("out/figures/00_02_job_scatter.png", width=12, heigh=6, units="in")

ggplot(data=jobs, aes(x=jobvect_3,y=jobvect_5 ))+
  geom_point(size=1)+  xlab("% Time Extensions") + ylab("% Time Admin.")+ labs(color='% Time Other') 
ggsave("out/figures/00_02_job_scatter_other.png", width=12, heigh=6, units="in")


## Make table
toshow<-jobs[,c("jobvect_1","jobvect_2","jobvect_3","jobvect_4", "jobvect_5" )]
names(toshow)<-c("Share Haircut/Shave", "Share Color/Highlight/Wash/Extensions", "Share Blowdry/Style/Treatment",
                 "Share Administrative", "Share Nail/Spa/Eye/Misc.")
stargazer(toshow, header=FALSE, out='out/tables/00_02_job_descriptives.tex')


##
jobs[, county_na:= .GRP, by=.(county, location_state)]

cust_w_mean<-function(x,w){
  return(sum(x*w)/sum(w))
}

cust_w_var<-function(x,w){
  return(sum(w*(x-cust_w_mean(x,w))^2)/sum(w))
}

cust_var<-function(x){
  return(sum((x-mean(x))^2)/length(x))
}

colA = paste("jobvect", 1:5, sep = "_")

fortable<-melt(jobs, id.vars=c("location_id", "emp_tot"), measure.vars=list(colA), value.name=c("jobvect"))
fortable<-fortable[!is.na(jobvect)]
aug_table<-fortable[,.(tot_var=cust_w_var(jobvect, w=emp_tot)), by="variable"]
fortable<-fortable[,.(firm_var=cust_w_mean(jobvect, w=emp_tot),within_var=cust_w_var(jobvect, w=emp_tot), tot=sum(emp_tot),
                      tot_hours=sum(jobvect*emp_tot)) ,by=c("variable", "location_id")]
fortable<-fortable[,.(within_var=sum(within_var*tot)/sum(tot), firm_var=cust_w_var(firm_var, w=tot), tot_hours=sum(tot_hours)) ,by=c("variable")]
fortable<-merge(aug_table,fortable, by="variable")
fortable[, firm_var:=firm_var/tot_var]
fortable[, within_var:=within_var/tot_var]
fortable[,tot_hours:=tot_hours/sum(tot_hours)]
fortable[, tot_var:=NULL]
fortable[,variable:=(gsub("jobvect_", "", variable)) ]
fortable[, (names(fortable[,-1])):=lapply(.SD,round,digits=4), .SDcols=names(fortable[,-1])]
setcolorder(fortable, c("variable", "tot_hours", "firm_var", "within_var"))
names(fortable)<-c("Task","Share of Labor", "Firm", "Within-Firm")
kable(fortable, "latex", align="c", booktabs=TRUE,linesep = c(""), escape = F, caption = 'Task-Content Variance Decomposition', label="firmdecomp") %>%
  add_header_above(., c(" "," ", "Share of Task-Content Variance" = 2)) %>%
  cat(., file = "out/tables/00_02_firmvariance_decomp.tex")

fortable<-melt(jobs, id.vars=c("quarter_year", "emp_tot"), measure.vars=list(colA), value.name=c("jobvect"))
fortable<-fortable[!is.na(jobvect)]
aug_table<-fortable[,.(tot_var=cust_w_var(jobvect, w=emp_tot)), by="variable"]
fortable<-fortable[,.(firm_var=cust_w_mean(jobvect, w=emp_tot),within_var=cust_w_var(jobvect, w=emp_tot), tot=sum(emp_tot),
                      tot_hours=sum(jobvect*emp_tot)) ,by=c("variable", "quarter_year")]
fortable<-fortable[,.(within_var=sum(within_var*tot)/sum(tot), firm_var=cust_w_var(firm_var, w=tot), tot_hours=sum(tot_hours)) ,by=c("variable")]
fortable<-merge(aug_table,fortable, by="variable")
fortable[, firm_var:=firm_var/tot_var]
fortable[, within_var:=within_var/tot_var]
fortable[,tot_hours:=tot_hours/sum(tot_hours)]
fortable[, tot_var:=NULL]
fortable[,variable:=(gsub("jobvect_", "", variable)) ]
fortable[, (names(fortable[,-1])):=lapply(.SD,round,digits=4), .SDcols=names(fortable[,-1])]
setcolorder(fortable, c("variable", "tot_hours", "firm_var", "within_var"))
names(fortable)<-c("Task","Share of Labor", "Quarter", "Within-Quarter")
kable(fortable, "latex", align="c", booktabs=TRUE,linesep = c(""), escape = F, caption = 'Task-Content Variance Decomposition', label="firmdecomp") %>%
  add_header_above(., c(" "," ", "Share of Task-Content Variance" = 2)) %>%
  cat(., file = "out/tables/00_02_timevariance_decomp.tex")


#### sindex
firms<-readRDS("data/00_00_firm_quarter.rds")
jobs<-merge(jobs, firms[,c("location_id", "quarter_year", "s_index")], by=c("location_id", "quarter_year"), all.x=TRUE)

jobs[, max_vect:=pmax(jobvect_1,jobvect_2, jobvect_3, jobvect_4, jobvect_5 )]
res0<-feols(max_vect~s_index, data=jobs)
res1<-feols(max_vect~s_index | quarter_year, data=jobs)
res2<-feols(max_vect~s_index | quarter_year+county_na, data=jobs)

esttex(res0, res1,res2, fitstat=~r2,se="cluster",dict=c(max_vect = "Worker Task Specialization", s_index="Organization Complexity", county_na="County" ,quarter_year="Quarter-Year"),
       cluster=jobs$location_id,keep = "!Intercept", file="out/tables/00_02_spec_sindex.tex", replace=TRUE,signifCode=c(`***`=0.001,`**`=0.01, `*`=0.05))


jobs[,r_sindex:=resid(feols(s_index~1| county_na+quarter_year, jobs))]
jobs[,r_maxvect:=resid(feols(max_vect~1| county_na+quarter_year, jobs))]

jobs[,round_resid_s_index:=get_midpoint(cut(r_sindex,9, include.lowest=TRUE))]
jobs[,round_s_index:=get_midpoint(cut(s_index,9, include.lowest=TRUE))]


summarySE <- function(data=NULL, measurevar, groupvars=NULL, na.rm=FALSE,
                      conf.interval=.95, .drop=TRUE) {
  library(plyr)
  
  # New version of length which can handle NA's: if na.rm==T, don't count them
  length2 <- function (x, na.rm=FALSE) {
    if (na.rm) sum(!is.na(x))
    else       length(x)
  }
  
  # This does the summary. For each group's data frame, return a vector with
  # N, mean, and sd
  datac <- ddply(data, groupvars, .drop=.drop,
                 .fun = function(xx, col) {
                   c(N    = length2(xx[[col]], na.rm=na.rm),
                     mean = mean   (xx[[col]], na.rm=na.rm),
                     sd   = sd     (xx[[col]], na.rm=na.rm)
                   )
                 },
                 measurevar
  )
  
  # Rename the "mean" column    
  datac <- rename(datac, c("mean" = measurevar))
  
  datac$se <- datac$sd / sqrt(datac$N)  # Calculate standard error of the mean
  
  # Confidence interval multiplier for standard error
  # Calculate t-statistic for confidence interval: 
  # e.g., if conf.interval is .95, use .975 (above/below), and use df=N-1
  ciMult <- qt(conf.interval/2 + .5, datac$N-1)
  datac$ci <- datac$se * ciMult
  
  return(datac)
}




# compelxity, task spec.
tgc <- summarySE(as.data.frame(jobs[!is.na(max_vect) & !is.na(s_index)]), measurevar="max_vect", groupvars=c("round_s_index"))
ggplot(tgc, aes(x=round_s_index, y=max_vect)) + 
  geom_errorbar(aes(ymin=max_vect-se, ymax=max_vect+se), width=.02,size=1) +
  geom_line(size=1) +
  geom_point(shape=21, size=3, fill="white")+ theme_bw(base_size=22)+
  xlab("Organization Complexity") + ylab("Most Worker Time Spent on a Single Task (%)")
ggsave("out/figures/00_02_sindex_spec_mean.png", width=12, heigh=6, units="in")

tgc <- summarySE(as.data.frame(jobs[!is.na(max_vect) & !is.na(s_index)]), measurevar="max_vect", groupvars=c("round_resid_s_index"))
ggplot(tgc, aes(x=round_resid_s_index, y=max_vect)) + 
  geom_errorbar(aes(ymin=max_vect-se, ymax=max_vect+se), width=.02,size=1) +
  geom_line(size=1) +
  geom_point(shape=21, size=3, fill="white")+ theme_bw(base_size=22)+
  xlab("Organization Complexity") + ylab("Most Worker Time Spent on a Single Task (%)")
ggsave("out/figures/00_02_sindex_spec_resid.png", width=12, heigh=6, units="in")




colA = "max_vect"

fortable<-copy(jobs)
fortable<-fortable[!is.na(max_vect)]
aug_table<-fortable[,.(tot_var=cust_w_var(max_vect, w=emp_tot))]
fortable<-fortable[,.(firm_var=cust_w_mean(max_vect, w=emp_tot),within_var=cust_w_var(max_vect, w=emp_tot), tot=sum(emp_tot),
                      tot_hours=sum(max_vect*emp_tot)) ,by=c("location_id")]
fortable<-fortable[,.(within_var=sum(within_var*tot)/sum(tot), firm_var=cust_w_var(firm_var, w=tot), tot_hours=sum(tot_hours)) ]
fortable<-cbind(aug_table,fortable)
fortable[, firm_var:=firm_var/tot_var]
fortable[, within_var:=within_var/tot_var]
fortable[,tot_hours:=tot_hours/sum(tot_hours)]



colA = "max_vect"

fortable<-copy(jobs)
fortable<-fortable[!is.na(max_vect)]
aug_table<-fortable[,.(tot_var=cust_w_var(max_vect, w=emp_tot))]
fortable<-fortable[,.(firm_var=cust_w_mean(max_vect, w=emp_tot),within_var=cust_w_var(max_vect, w=emp_tot), tot=sum(emp_tot),
                      tot_hours=sum(max_vect*emp_tot)) ,by=c("quarter_year")]
fortable<-fortable[,.(within_var=sum(within_var*tot)/sum(tot), firm_var=cust_w_var(firm_var, w=tot), tot_hours=sum(tot_hours)) ]
fortable<-cbind(aug_table,fortable)
fortable[, firm_var:=firm_var/tot_var]
fortable[, within_var:=within_var/tot_var]
fortable[,tot_hours:=tot_hours/sum(tot_hours)]


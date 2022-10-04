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

ggplot(data=jobs, aes(x=jobvect_3,y=jobvect_5, color=as.numeric(jobvect_6) ))+
  geom_point(size=1)+  xlab("% Time Extensions") + ylab("% Time Admin.")+ labs(color='% Time Other') 
ggsave("out/figures/00_02_job_scatter_other.png", width=12, heigh=6, units="in")


## Make table
toshow<-jobs[,c("jobvect_1","jobvect_2","jobvect_3","jobvect_4", "jobvect_5", "jobvect_6" )]
names(toshow)<-c("Share Haircut/Shave", "Share Color/Highlight/Wash", "Share Extensions", "Share Blowdry/Style/Treatment",
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

colA = paste("jobvect", 1:6, sep = "_")

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




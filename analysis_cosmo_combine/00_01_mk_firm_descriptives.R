### make descriptive tables of firm information

library('checkpoint')
#checkpoint('2020-07-16') activate to reproduce
library('data.table')
library('fixest')
library('binsreg')
library('ggplot2')
library('spatstat')

library('stargazer')
range_val<-function(x){
  return(range(x)[2]-range(x)[1])
}
rowMax <- function(data) apply(data,1, max, na.rm = TRUE)
rowMin <- function(data) apply(data,1, min, na.rm = TRUE)
theme_set(theme_bw(base_size=22))

firm_quarter<-readRDS("data/00_00_firm_quarter.rds")

#### attach measure of task-specialization
readRDS("data/00_00_job_quarter.rds")->jobs
jobs[, max_time:=rowMax(.SD), .SDcols=which(colnames(jobs) %like% "jobvect")]
jobs<-jobs[, .(max_time=sum(max_time*efrac)), by=c("quarter_year", "location_id")]
firm_quarter<-merge(firm_quarter,jobs,by=c("quarter_year", "location_id"), all.x=TRUE)

# exclude firm-quarters with no revenue.
firm_quarter<-firm_quarter[revenue>0 ,]
### collapse to firm
# exclude one salon in KY which has 22,000,000 in revenue.
#firm_quarter<-firm_quarter[location_id!='fb686b3a-a166-469b-88ea-3467a68e2f53',]
firm_quarter<-firm_quarter[location_id!='fb686b3a-a166-469b-88ea-3467a68e2f53',]
# exclude partial quarter
firm_quarter<-firm_quarter[quarter_year!=2021.3,]
firm_quarter[,return_rate:=return_count/cust_visits]
firm_quarter[, rev_per:=revenue/tot_duration]
# create residualized variables
firm_quarter[, county_na:= .GRP, by=.(county, location_state)]
firm_quarter[, rev_emp:=revenue/emps]

firm_quarter[,r_sindex:=resid(feols(s_index~task_mix2+task_mix3+task_mix4+task_mix5| county_na+quarter_year, firm_quarter))]
firm_quarter[,r_price:=resid(feols(cust_price~task_mix2+task_mix3+task_mix4+task_mix5| county_na+quarter_year, firm_quarter))]
firm_quarter[,r_rev:=resid(feols(revenue~task_mix2+task_mix3+task_mix4+task_mix5| county_na+quarter_year, firm_quarter))]
firm_quarter[,r_visits:=resid(feols(cust_visits~task_mix2+task_mix3+task_mix4+task_mix5| county_na+quarter_year, firm_quarter))]
firm_quarter[,r_duration:=resid(feols(tot_duration~task_mix2+task_mix3+task_mix4+task_mix5| county_na+quarter_year, firm_quarter))]
firm_quarter[,r_cust:=resid(feols(cust_count~task_mix2+task_mix3+task_mix4+task_mix5| county_na+quarter_year, firm_quarter))]
firm_quarter[,r_emp:=resid(feols(emps~task_mix2+task_mix3+task_mix4+task_mix5| county_na+quarter_year, firm_quarter))]
firm_quarter[,r_return_rate:=resid(feols(return_rate~task_mix2+task_mix3+task_mix4+task_mix5| county_na+quarter_year, firm_quarter))]
firm_quarter[,r_rev_per:=resid(feols(rev_per~task_mix2+task_mix3+task_mix4+task_mix5| county_na+quarter_year, firm_quarter))]
firm_quarter[,r_rev_emp:=resid(feols(rev_emp~task_mix2+task_mix3+task_mix4+task_mix5| county_na+quarter_year, firm_quarter))]


### firm table
firm_stats<-firm_quarter[,c("revenue", "cust_price","emps", "cust_count","service_types","avg_labor",
                            "s_index","task_mix1","task_mix2", "task_mix3", "task_mix4", "task_mix5")]


names(firm_stats)<-c("Revenue","Price","Employees", "Customers","Task Categories","Labor per. Customer", "Organization Complexity", 
                     "Share Haircut/Shave", "Share Color/Highlight/Wash", "Share Blowdry/Style/Treatment/Extensions",
                     "Admininstrative","Nail/Spa/Eye/Misc."
)
stargazer(firm_stats, header=FALSE, type='text')
stargazer(firm_stats, header=FALSE,digits=2, out='out/tables/00_01_firm_descriptives.tex',single.row = TRUE)

### task-mix scatter
ggplot(data=firm_quarter, aes(x=task_mix1,y=task_mix2, color=task_mix3))+
  geom_point()+  xlab("% Time Haircut") + ylab("% Time Color")+ labs(color='% Time Blowdry') 
ggsave("out/figures/00_01_firm_scatter.png", width=12, height=6, units="in")

ggplot(data=firm_quarter, aes(x=task_mix4,y=task_mix5))+
  geom_point()+  xlab("% Time Admin.") + ylab("% Time Nail/Spa/Misc.")+ labs(color='% Time Other') 
ggsave("out/figures/00_01_firm_scatter_other.png", width=12, height =6, units="in")

### Organization Complexity hist
ggplot(firm_quarter, aes(x=s_index)) +
  geom_histogram(color="black", fill="lightblue", size=1, bins = 40)+ ylab("Firm-Quarter Count") + xlab("Organization Complexity")+ theme(legend.position = "none")
ggsave("out/figures/00_01_sindex_hist.png", width=12, height=6, units="in")

firm_quarter[, s_norm:=ifelse(round(s_index,9)==0,0,s_norm) ]

ggplot(firm_quarter, aes(x=s_norm)) +
  geom_histogram(color="black", fill="lightblue", size=1, bins = 40)+ ylab("Firm-Quarter Count") + xlab("Normalized Organization Complexity")+ theme(legend.position = "none")
ggsave("out/figures/00_01_snorm_hist.png", width=12, height=6, units="in")


## regression

## regs

res2<-feols(s_norm~ 1 | quarter_year+location_id, data=firm_quarter)
locs<-data.table(loc_fe=fixef(res2)$location_id, location_id=names(fixef(res2)$location_id))
quarter<-data.table(quarter_fe=fixef(res2)$quarter_year, quarter_year=as.numeric(names(fixef(res2)$quarter_year)))
firm_quarter<-merge(firm_quarter, locs, by="location_id", all.x=TRUE)
firm_quarter<-merge(firm_quarter, quarter, by="quarter_year", all.x=TRUE)

firmpart<-var(firm_quarter$loc_fe)
quarterpart<-var(firm_quarter$quarter_fe)
covpart<-2*cov(firm_quarter$loc_fe, firm_quarter$quarter_fe)
residpart<-var(resid(res2))
total<-var(firm_quarter$s_norm)


get_midpoint <- Vectorize(function(cut_label) {
  mean(as.numeric(unlist(strsplit(gsub("\\(|\\)|\\[|\\]", "", as.character(cut_label)), ","))))
})



firm_quarter[,round_resid_s_index:=get_midpoint(cut(r_sindex,9, include.lowest=TRUE))]
firm_quarter[,round_s_index:=get_midpoint(cut(s_index,9, include.lowest=TRUE))]


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

tgc <- summarySE(as.data.frame(firm_quarter), measurevar="rev_emp", groupvars=c("round_s_index"))
ggplot(tgc, aes(x=round_s_index, y=rev_emp)) + 
  geom_errorbar(aes(ymin=rev_emp-se*qnorm(0.975), ymax=rev_emp+se*qnorm(0.975)), width=.02,size=1) +
  geom_line(size=1) +
  geom_point(shape=21, size=3, fill="white")+ theme_bw(base_size=22)+
  xlab("Organization Complexity") + ylab("Revenue per Employee ($)")

ggsave("out/figures/00_01_revemp_sindex_mean.png", width=12, heigh=6, units="in")


tgc <- summarySE(as.data.frame(firm_quarter), measurevar="r_rev_emp", groupvars=c("round_s_index"))
ggplot(tgc, aes(x=round_s_index, y=r_rev_emp)) + 
  geom_errorbar(aes(ymin=r_rev_emp-se*qnorm(0.975), ymax=r_rev_emp+se*qnorm(0.975)), width=.02,size=1) +
  geom_line(size=1) +
  geom_point(shape=21, size=3, fill="white")+ theme_bw(base_size=22)+
  xlab("Organization Complexity") + ylab("Revenue per Employee ($)")

ggsave("out/figures/00_01_revemp_sindex_resid.png", width=12, heigh=6, units="in")



### revenue
tgc <- summarySE(as.data.frame(firm_quarter), measurevar="revenue", groupvars=c("round_s_index"))
ggplot(tgc, aes(x=round_s_index, y=revenue)) + 
  geom_errorbar(aes(ymin=revenue-se*qnorm(0.975), ymax=revenue+se*qnorm(0.975)), width=.02,size=1) +
  geom_line(size=1) +
  geom_point(shape=21, size=3, fill="white")+ theme_bw(base_size=22)+
  xlab("Organization Complexity") + ylab("Revenue ($)")

ggsave("out/figures/00_01_rev_sindex_mean.png", width=12, heigh=6, units="in")
## resid revenue
tgc <- summarySE(as.data.frame(firm_quarter), measurevar="r_rev", groupvars=c("round_resid_s_index"))
ggplot(tgc, aes(x=round_resid_s_index, y=r_rev)) + 
  geom_errorbar(aes(ymin=r_rev-se*qnorm(0.975), ymax=r_rev+se*qnorm(0.975)), width=.02,size=1) +
  geom_line(size=1) +
  geom_point(shape=21, size=3, fill="white")+ theme_bw(base_size=22)+
  xlab("Residualized Organization Complexity") + ylab("Residualized Revenue")

ggsave("out/figures/00_01_rev_sindex_resid.png", width=12, heigh=6, units="in")




tgc <- summarySE(as.data.frame(firm_quarter[county==36061,]), measurevar="revenue", groupvars=c("round_s_index"))
ggplot(tgc, aes(x=round_s_index, y=revenue)) + 
  geom_errorbar(aes(ymin=revenue-se*qnorm(0.975), ymax=revenue+se*qnorm(0.975)), width=.02,size=1) +
  geom_line(size=1) +
  geom_point(shape=21, size=3, fill="white")+ theme_bw(base_size=22)+
  xlab("Organization Complexity") + ylab("Revenue ($)")

ggsave("out/figures/00_01_rev_sindex_mean_ny.png", width=12, heigh=6, units="in")


## regs
res0<-feols(revenue~s_index, data=firm_quarter)
res1<-feols(revenue~s_index | quarter_year, data=firm_quarter)
res2<-feols(revenue~s_index | quarter_year+county_na, data=firm_quarter)
res3<-feols(revenue~s_index+task_mix2+task_mix3+task_mix4+task_mix5 | quarter_year+county_na, data=firm_quarter)
esttex(res0, res1,res2,res3, fitstat=~r2,se="cluster",dict=c(revenue = "Revenue", s_index="Organization Complexity", sub_quarter="County Sub-Div." ,quarter_year="Quarter-Year"),
       cluster=firm_quarter$location_id, file="out/tables/00_01_reg_rev_sindex.tex", replace=TRUE,signifCode=c(`***`=0.001,`**`=0.01, `*`=0.05))


res0<-feols(revenue~max_time, data=firm_quarter)
res1<-feols(revenue~max_time | quarter_year, data=firm_quarter)
res2<-feols(revenue~max_time | quarter_year+county_na, data=firm_quarter)
res3<-feols(revenue~max_time+task_mix2+task_mix3+task_mix4+task_mix5 | quarter_year+county_na, data=firm_quarter)
esttex(res0, res1,res2,res3, fitstat=~r2,se="cluster",dict=c(revenue = "Revenue", max_time="Task Specialization", sub_quarter="County Sub-Div." ,quarter_year="Quarter-Year"),
       cluster=firm_quarter$location_id, file="out/tables/00_01_reg_rev_spec.tex", replace=TRUE,signifCode=c(`***`=0.001,`**`=0.01, `*`=0.05))




res_rev<-feols(revenue~s_index+task_mix2+task_mix3+task_mix4+task_mix5 | quarter_year+county_na, data=firm_quarter)
res_emp<-feols(emps~s_index+task_mix2+task_mix3+task_mix4+task_mix5 | quarter_year+county_na, data=firm_quarter)
res_labor<-feols(tot_duration~s_index+task_mix2+task_mix3+task_mix4+task_mix5 | quarter_year+county_na, data=firm_quarter)
res_cust<-feols(cust_count~s_index+task_mix2+task_mix3+task_mix4+task_mix5 | quarter_year+county_na, data=firm_quarter)
res_visits<-feols(cust_visits~s_index+task_mix2+task_mix3+task_mix4+task_mix5 | quarter_year+county_na, data=firm_quarter)

esttex(res_rev, res_emp,res_labor,res_cust,res_visits, fitstat=~r2,dict=c(revenue = "Revenue", s_index="Org. Complexity", loc_city_state="City" ,quarter_year="Quarter-Year",
                                                                          emps="Employees", tot_duration="Utilized Labor",cust_count="Customers", cust_visits="Visits",
                                                                          county_na="County"),
       cluster=firm_quarter$location_id,drop=c("task_mix2","task_mix3", "task_mix4", "task_mix5"), file="out/tables/00_01_reg_size_sindex.tex", replace=TRUE, se="cluster",signifCode=c(`***`=0.001,`**`=0.01, `*`=0.05))

res_rev<-feols(revenue~s_index+task_mix2+task_mix3+task_mix4+task_mix5 | quarter_year, data=firm_quarter[county==36061,])
res_emp<-feols(emps~s_index+task_mix2+task_mix3+task_mix4+task_mix5 | quarter_year, data=firm_quarter[county==36061,])
res_labor<-feols(tot_duration~s_index+task_mix2+task_mix3+task_mix4+task_mix5 | quarter_year, data=firm_quarter[county==36061,])
res_cust<-feols(cust_count~s_index+task_mix2+task_mix3+task_mix4+task_mix5 | quarter_year, data=firm_quarter[county==36061,])
res_visits<-feols(cust_visits~s_index+task_mix2+task_mix3+task_mix4+task_mix5 | quarter_year, data=firm_quarter[county==36061,])

esttex(res_rev, res_emp,res_labor,res_cust,res_visits, fitstat=~r2,dict=c(revenue = "Revenue", s_index="Org. Complexity", loc_city_state="City" ,quarter_year="Quarter-Year",
                                                                          emps="Employees", tot_duration="Utilized Labor",cust_count="Customers", cust_visits="Visits"),
       cluster=firm_quarter[county==36061,]$location_id,drop=c("task_mix2","task_mix3", "task_mix4", "task_mix5"), file="out/tables/00_01_reg_size_sindex_ny.tex", replace=TRUE, se="cluster",signifCode=c(`***`=0.001,`**`=0.01, `*`=0.05))

## more measures of size/market share


# customers
tgc <- summarySE(as.data.frame(firm_quarter), measurevar="cust_count", groupvars=c("round_s_index"))
ggplot(tgc, aes(x=round_s_index, y=cust_count)) + 
  geom_errorbar(aes(ymin=cust_count-se, ymax=cust_count+se), width=.02,size=1) +
  geom_line(size=1) +
  geom_point(shape=21, size=3, fill="white")+ theme_bw(base_size=22)+
  xlab("Organization Complexity") + ylab("Customer Count")

ggsave("out/figures/00_01_cust_sindex_mean.png", width=12, heigh=6, units="in")

tgc <- summarySE(as.data.frame(firm_quarter), measurevar="r_cust", groupvars=c("round_resid_s_index"))
ggplot(tgc, aes(x=round_resid_s_index, y=r_cust)) + 
  geom_errorbar(aes(ymin=r_cust-se, ymax=r_cust+se), width=.02,size=1) +
  geom_line(size=1) +
  geom_point(shape=21, size=3, fill="white")+ theme_bw(base_size=22)+
  xlab("Residualized Organization Complexity") + ylab("Residualized Customer Count")

ggsave("out/figures/00_01_cust_sindex_resid.png", width=12, heigh=6, units="in")



# visits

tgc <- summarySE(as.data.frame(firm_quarter), measurevar="cust_visits", groupvars=c("round_s_index"))
ggplot(tgc, aes(x=round_s_index, y=cust_visits)) + 
  geom_errorbar(aes(ymin=cust_visits-se*qnorm(0.975), ymax=cust_visits+se*qnorm(0.975)), width=.02,size=1) +
  geom_line(size=1) +
  geom_point(shape=21, size=3, fill="white")+ theme_bw(base_size=22)+
  xlab("Organization Complexity") + ylab("Customer Visits")

ggsave("out/figures/00_01_visits_sindex_mean.png", width=12, heigh=6, units="in")

tgc <- summarySE(as.data.frame(firm_quarter), measurevar="r_cust", groupvars=c("round_resid_s_index"))
ggplot(tgc, aes(x=round_resid_s_index, y=r_cust)) + 
  geom_errorbar(aes(ymin=r_cust-se*qnorm(0.975), ymax=r_cust+se*qnorm(0.975)), width=.02,size=1) +
  geom_line(size=1) +
  geom_point(shape=21, size=3, fill="white")+ theme_bw(base_size=22)+
  xlab("Residualized Organization Complexity") + ylab("Residualized Customer Visits")

ggsave("out/figures/00_01_visits_sindex_resid.png", width=12, heigh=6, units="in")


# total labor
tgc <- summarySE(as.data.frame(firm_quarter), measurevar="tot_duration", groupvars=c("round_s_index"))
ggplot(tgc, aes(x=round_s_index, y=tot_duration)) + 
  geom_errorbar(aes(ymin=tot_duration-se*qnorm(0.975), ymax=tot_duration+se*qnorm(0.975)), width=.02,size=1) +
  geom_line(size=1) +
  geom_point(shape=21, size=3, fill="white")+ theme_bw(base_size=22)+
  xlab("Organization Complexity") + ylab("Utilized Labor")

ggsave("out/figures/00_01_labor_sindex_mean.png", width=12, heigh=6, units="in")

tgc <- summarySE(as.data.frame(firm_quarter), measurevar="r_duration", groupvars=c("round_resid_s_index"))
ggplot(tgc, aes(x=round_resid_s_index, y=r_duration)) + 
  geom_errorbar(aes(ymin=r_duration-se*qnorm(0.975), ymax=r_duration+se*qnorm(0.975)), width=.02,size=1) +
  geom_line(size=1) +
  geom_point(shape=21, size=3, fill="white")+ theme_bw(base_size=22)+
  xlab("Residualized Organization Complexity") + ylab("Residualized Utilized Labor")

ggsave("out/figures/00_01_labor_sindex_resid.png", width=12, heigh=6, units="in")



# emps
tgc <- summarySE(as.data.frame(firm_quarter), measurevar="emps", groupvars=c("round_s_index"))
ggplot(tgc, aes(x=round_s_index, y=emps)) + 
  geom_errorbar(aes(ymin=emps-se*qnorm(0.975), ymax=emps+se*qnorm(0.975)), width=.02,size=1) +
  geom_line(size=1) +
  geom_point(shape=21, size=3, fill="white")+ theme_bw(base_size=22)+
  xlab("Organization Complexity") + ylab("Employee Count")

ggsave("out/figures/00_01_emps_sindex_mean.png", width=12, heigh=6, units="in")

tgc <- summarySE(as.data.frame(firm_quarter[county==36061,]), measurevar="emps", groupvars=c("round_s_index"))
ggplot(tgc, aes(x=round_s_index, y=emps)) + 
  geom_errorbar(aes(ymin=emps-se*qnorm(0.975), ymax=emps+se*qnorm(0.975)), width=.02,size=1) +
  geom_line(size=1) +
  geom_point(shape=21, size=3, fill="white")+ theme_bw(base_size=22)+
  xlab("Organization Complexity") + ylab("Employee Count")

ggsave("out/figures/00_01_emps_sindex_mean_ny.png", width=12, heigh=6, units="in")


tgc <- summarySE(as.data.frame(firm_quarter), measurevar="r_emp", groupvars=c("round_resid_s_index"))
ggplot(tgc, aes(x=round_resid_s_index, y=r_emp)) + 
  geom_errorbar(aes(ymin=r_emp-se*qnorm(0.975), ymax=r_emp+se*qnorm(0.975)), width=.02,size=1) +
  geom_line(size=1) +
  geom_point(shape=21, size=3, fill="white")+ theme_bw(base_size=22)+
  xlab("Residualized Organization Complexity") + ylab("Residualized Employee Count")

ggsave("out/figures/00_01_emps_sindex_resid.png", width=12, heigh=6, units="in")

# total rev per labor

tgc <- summarySE(as.data.frame(firm_quarter), measurevar="rev_per", groupvars=c("round_s_index"))
ggplot(tgc, aes(x=round_s_index, y=rev_per)) + 
  geom_errorbar(aes(ymin=rev_per-se*qnorm(0.975), ymax=rev_per+se*qnorm(0.975)), width=.02,size=1) +
  geom_line(size=1) +
  geom_point(shape=21, size=3, fill="white")+ theme_bw(base_size=22)+
  xlab("Organization Complexity") + ylab("Revenue per Minute of Labor")

ggsave("out/figures/00_01_revper_sindex_mean.png", width=12, heigh=6, units="in")

tgc <- summarySE(as.data.frame(firm_quarter), measurevar="r_rev_per", groupvars=c("round_resid_s_index"))
ggplot(tgc, aes(x=round_resid_s_index, y=r_rev_per)) + 
  geom_errorbar(aes(ymin=r_rev_per-se*qnorm(0.975), ymax=r_rev_per+se*qnorm(0.975)), width=.02,size=1) +
  geom_line(size=1) +
  geom_point(shape=21, size=3, fill="white")+ theme_bw(base_size=22)+
  xlab("Residualized Organization Complexity") + ylab("Residualized Revenue per Minute of Labor")

ggsave("out/figures/00_01_revper_sindex_resid.png", width=12, heigh=6, units="in")

# price
tgc <- summarySE(as.data.frame(firm_quarter), measurevar="cust_price", groupvars=c("round_s_index"))
ggplot(tgc, aes(x=round_s_index, y=cust_price)) + 
  geom_errorbar(aes(ymin=cust_price-se*qnorm(0.975), ymax=cust_price+se*qnorm(0.975)), width=.02,size=1) +
  geom_line(size=1) +
  geom_point(shape=21, size=3, fill="white")+ theme_bw(base_size=22)+
  xlab("Organization Complexity") + ylab("Price ($)")
ggsave("out/figures/00_01_custrev_sindex_mean.png", width=12, heigh=6, units="in")

tgc <- summarySE(as.data.frame(firm_quarter[county==36061,]), measurevar="cust_price", groupvars=c("round_s_index"))
ggplot(tgc, aes(x=round_s_index, y=cust_price)) + 
  geom_errorbar(aes(ymin=cust_price-se*qnorm(0.975), ymax=cust_price+se*qnorm(0.975)), width=.02,size=1) +
  geom_line(size=1) +
  geom_point(shape=21, size=3, fill="white")+ theme_bw(base_size=22)+
  xlab("Organization Complexity") + ylab("Price ($)")
ggsave("out/figures/00_01_custrev_sindex_mean_ny.png", width=12, heigh=6, units="in")


tgc <- summarySE(as.data.frame(firm_quarter), measurevar="r_price", groupvars=c("round_resid_s_index"))
ggplot(tgc, aes(x=round_resid_s_index, y=r_price)) + 
  geom_errorbar(aes(ymin=r_price-se*qnorm(0.975), ymax=r_price+se*qnorm(0.975)), width=.02,size=1) +
  geom_line(size=1) +
  geom_point(shape=21, size=3, fill="white")+ theme_bw(base_size=22)+
  xlab("Residualized Organization Complexity") + ylab("Residualized Price ($)")
ggsave("out/figures/00_01_custrev_sindex_resid.png", width=12, heigh=6, units="in")

# return rate

tgc <- summarySE(as.data.frame(firm_quarter), measurevar="return_rate", groupvars=c("round_s_index"))
ggplot(tgc, aes(x=round_s_index, y=return_rate)) + 
  geom_errorbar(aes(ymin=return_rate-se*qnorm(0.975), ymax=return_rate+se*qnorm(0.975)), width=.02,size=1) +
  geom_line(size=1) +
  geom_point(shape=21, size=3, fill="white")+ theme_bw(base_size=22)+
  xlab("Organization Complexity") + ylab("% Repeat Visits")
ggsave("out/figures/00_01_return_sindex_mean.png", width=12, heigh=6, units="in")

tgc <- summarySE(as.data.frame(firm_quarter[county==36061,]), measurevar="return_rate", groupvars=c("round_s_index"))
ggplot(tgc, aes(x=round_s_index, y=return_rate)) + 
  geom_errorbar(aes(ymin=return_rate-se*qnorm(0.975), ymax=return_rate+se*qnorm(0.975)), width=.02,size=1) +
  geom_line(size=1) +
  geom_point(shape=21, size=3, fill="white")+ theme_bw(base_size=22)+
  xlab("Organization Complexity") + ylab("% Repeat Visits")
ggsave("out/figures/00_01_return_sindex_mean_ny.png", width=12, heigh=6, units="in")



tgc <- summarySE(as.data.frame(firm_quarter), measurevar="r_return_rate", groupvars=c("round_resid_s_index"))
ggplot(tgc, aes(x=round_resid_s_index, y=r_return_rate)) + 
  geom_errorbar(aes(ymin=r_return_rate-se*qnorm(0.975), ymax=r_return_rate+se*qnorm(0.975)), width=.02,size=1) +
  geom_line(size=1) +
  geom_point(shape=21, size=3, fill="white")+ theme_bw(base_size=22)+
  xlab("Residualized Organization Complexity") + ylab("Residualized % Repeat Visits")
ggsave("out/figures/00_01_return_sindex_resid.png", width=12, heigh=6, units="in")


#firm_quarter[location_state=="NY",.(emps=mean(emps)), by=quarter_year]->timeseries
#
#ggplot(firm_quarter[quarter_year>=2019.3 & location_state=="CA"], aes(x=quarter_year, y=s_index,color=location_id)) + 
#  geom_line(size=1)+ theme(legend.position = "none")#
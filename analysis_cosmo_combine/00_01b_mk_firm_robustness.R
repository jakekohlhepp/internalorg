### make descriptive tables of firm information

library('checkpoint')
#checkpoint('2020-07-16') activate to reproduce
library('data.table')
library('fixest')
library('binsreg')
library('ggplot2')
library('stringr')
library('stargazer')
library('xtable')
range_val<-function(x){
  return(range(x)[2]-range(x)[1])
}
rowMax <- function(data) apply(data,1, max, na.rm = TRUE)
rowMin <- function(data) apply(data,1, min, na.rm = TRUE)
theme_set(theme_bw(base_size=22))



#### correlations between months within quarter.
firm_months<-readRDS("data/00_00b_firm_month.rds")
firm_months[, quarter_year:=floor(month_year/100)+floor((month_year-floor(month_year/100)*100)/3)/10]
firm_months[, month:=rank(month_year), by=c("location_id", "quarter_year")]
firm_months[, month:=paste0("sindex_",as.character(month))]
setnames(firm_months, "s_index", "sindex")
firm_casted<-dcast(firm_months, quarter_year+location_id~month, value.var=c("sindex"))
# flag data that has all 4 quarters.
firm_casted[, has_all:=!is.na(sindex_1)&   !is.na(sindex_2) &  !is.na(sindex_3)]

## general correlation
corstars <-function(x, method=c("pearson", "spearman"), removeTriangle=c("upper", "lower"),
                    result=c("none", "html", "latex")){
  #Compute correlation matrix
  require(Hmisc)
  x <- as.matrix(x)
  correlation_matrix<-rcorr(x, type=method[1])
  R <- correlation_matrix$r # Matrix of correlation coeficients
  p <- correlation_matrix$P # Matrix of p-value 
  
  ## Define notions for significance levels; spacing is important.
  mystars <- ifelse(p < .001, "***", ifelse(p < .01, "** ", ifelse(p < .05, "*  ", "")))
  
  ## trunctuate the correlation matrix to two decimal
  R <- format(round(cbind(rep(-1.11, ncol(x)), R), 3))[,-1]
  
  ## build a new matrix that includes the correlations with their apropriate stars
  Rnew <- matrix(paste(R, mystars, sep=""), ncol=ncol(x))
  diag(Rnew) <- paste(diag(R), " ", sep="")
  rownames(Rnew) <- colnames(x)
  colnames(Rnew) <- paste(colnames(x), "", sep="")
  
  ## remove upper triangle of correlation matrix
  if(removeTriangle[1]=="upper"){
    Rnew <- as.matrix(Rnew)
    #Rnew[upper.tri(Rnew, diag = FALSE)] <- ""
    Rnew <- as.data.frame(Rnew)
  }
  
  ## remove lower triangle of correlation matrix
  else if(removeTriangle[1]=="lower"){
    Rnew <- as.matrix(Rnew)
    #Rnew[lower.tri(Rnew, diag = FALSE)] <- ""
    Rnew <- as.data.frame(Rnew)
  }
  
  ## remove last column and return the correlation matrix
  Rnew <- cbind(Rnew[1:length(Rnew)])
  if (result[1]=="none") return(Rnew)
  else{
    if(result[1]=="html") print(xtable(Rnew), type="html")
    else print(xtable(Rnew), type="latex") 
  }
} 
for_corr<-firm_casted[has_all==1,c("sindex_1", "sindex_2", "sindex_3")]
mcor<-corstars(for_corr, method="pearson", result="latex")




## excluding 2020
cor(firm_casted[has_all==1 & floor(quarter_year)!=2020,c("sindex_1", "sindex_2", "sindex_3")])

## only 2020
cor(firm_casted[has_all==1 & floor(quarter_year)==2020,c("sindex_1", "sindex_2", "sindex_3")])

## only 2019
cor(firm_casted[has_all==1 & floor(quarter_year)==2019,c("sindex_1", "sindex_2", "sindex_3")])


### correlations between appointment requested rate and complexity.
firm_quarter<-readRDS("data/00_00_firm_quarter.rds")

firm_quarter[, any_req:=max(requested)>0, by=location_id ]
# first time observed requested
firm_quarter[requested>0, first_req:=min(quarter_year), by=location_id ]
firm_quarter[, first_req:=min(first_req, na.rm=TRUE), by=location_id ]
firm_quarter[, req_rate:=requested/task_count]
firm_quarter[,s_req_rate:=(req_rate-mean(req_rate, na.rm=TRUE))/sd(req_rate, na.rm=TRUE)]
firm_quarter[,s_s_index:=(s_index-mean(s_index, na.rm=TRUE))/sd(s_index, na.rm=TRUE)]

cor(firm_quarter[!is.na(s_index), ]$s_s_index, firm_quarter[!is.na(s_index), ]$s_req_rate)
plot(firm_quarter[!is.na(s_index), ]$s_s_index, firm_quarter[!is.na(s_index), ]$s_req_rate)
req_1<-feols(s_s_index~s_req_rate , data=firm_quarter)
summary(req_1,cluster=firm_quarter$location_id)

req_subset_1<-feols(s_s_index~s_req_rate , data=firm_quarter[any_req==1 & quarter_year>=first_req,])

summary(req_subset_1)

get_midpoint <- Vectorize(function(cut_label) {
  mean(as.numeric(unlist(strsplit(gsub("\\(|\\)|\\[|\\]", "", as.character(cut_label)), ","))))
})



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

tgc <- summarySE(as.data.frame(firm_quarter), measurevar="req_rate", groupvars=c("round_s_index"))
ggplot(tgc, aes(x=round_s_index, y=req_rate)) + 
  geom_errorbar(aes(ymin=req_rate-se*qnorm(0.975), ymax=req_rate+se*qnorm(0.975)), width=.02,size=1) +
  geom_line(size=1) +
  geom_point(shape=21, size=3, fill="white")+ theme_bw(base_size=22)+
  xlab("Organization Complexity") + ylab("Staff Requested Rate")

ggsave("out/figures/00_01b_req_complex_raw.png", width=12, heigh=6, units="in")

tgc <- summarySE(as.data.frame(firm_quarter[any_req==1 & quarter_year>=first_req]), measurevar="req_rate", groupvars=c("round_s_index"))
ggplot(tgc, aes(x=round_s_index, y=req_rate)) + 
  geom_errorbar(aes(ymin=req_rate-se*qnorm(0.975), ymax=req_rate+se*qnorm(0.975)), width=.02,size=1) +
  geom_line(size=1) +
  geom_point(shape=21, size=3, fill="white")+ theme_bw(base_size=22)+
  xlab("Organization Complexity") + ylab("Staff Requested Rate")

ggsave("out/figures/00_01b_req_complex_afterfirstreq.png", width=12, heigh=6, units="in")

ggplot(firm_quarter[any_req==1 & quarter_year>=first_req], aes(x=req_rate)) +
  geom_histogram(color="black", fill="lightblue", size=1, bins = 40)+ ylab("Firm-Quarter Count") + xlab("Staff Requested Rate")+ theme(legend.position = "none")
ggsave("out/figures/00_01b_request_hist_afterfirstreq.png", width=12, heigh=6, units="in")

ggplot(firm_quarter, aes(x=requested)) +
  geom_histogram(color="black", fill="lightblue", size=1, bins = 40)+ ylab("Firm-Quarter Count") + xlab("Count of Staff Requested")+ theme(legend.position = "none")

ggsave("out/figures/00_01b_request_hist.png", width=12, heigh=6, units="in")


quarter<-firm_quarter[, .(req_tot=sum(requested), task_tot=sum(task_count)), by=quarter_year]
quarter[, req_rate:=req_tot/task_tot]
ggplot(quarter, aes(y=req_rate, x=quarter_year))+geom_line()+xlab("Quarter-Year")+ylab("Staff Requested Rate")
ggsave("out/figures/00_01b_reqrate_time.png", width=12, heigh=6, units="in")


# request rate and revenue and employees.

firm_quarter[, county_na:= .GRP, by=.(county, location_state)]
firm_quarter[,r_rev:=resid(feols(revenue~task_mix2+task_mix3+task_mix4+task_mix5| county_na+quarter_year, firm_quarter))]
firm_quarter[,r_req_rate:=resid(feols(req_rate~task_mix2+task_mix3+task_mix4+task_mix5| county_na+quarter_year, firm_quarter))]
firm_quarter[,round_req_rate:=get_midpoint(cut(r_req_rate,9, include.lowest=TRUE))]
firm_quarter[,r_emp:=resid(feols(emps~task_mix2+task_mix3+task_mix4+task_mix5| county_na+quarter_year, firm_quarter))]

tgc <- summarySE(as.data.frame(firm_quarter), measurevar="r_rev", groupvars=c("round_req_rate"))
ggplot(tgc, aes(x=round_req_rate, y=r_rev)) + 
  geom_errorbar(aes(ymin=r_rev-se*qnorm(0.975), ymax=r_rev+se*qnorm(0.975)), width=.02,size=1) +
  geom_line(size=1) +
  geom_point(shape=21, size=3, fill="white")+ theme_bw(base_size=22)+
  xlab("Residualized Staff Requested Rate") + ylab("Residualized Revenue ($)")

ggsave("out/figures/00_01b_reqrate_rev_resid.png", width=12, heigh=6, units="in")

tgc <- summarySE(as.data.frame(firm_quarter), measurevar="r_emp", groupvars=c("round_req_rate"))
ggplot(tgc, aes(x=round_req_rate, y=r_emp)) + 
  geom_errorbar(aes(ymin=r_emp-se*qnorm(0.975), ymax=r_emp+se*qnorm(0.975)), width=.02,size=1) +
  geom_line(size=1) +
  geom_point(shape=21, size=3, fill="white")+ theme_bw(base_size=22)+
  xlab("Residualized Staff Requested Rate") + ylab("Residualized Employee Count")

ggsave("out/figures/00_01b_reqrate_emp_resid.png", width=12, heigh=6, units="in")

subset_firm_salon<-copy(firm_quarter[any_req==1 & quarter_year>=first_req,])

subset_firm_salon[,r_rev:=resid(feols(revenue~task_mix2+task_mix3+task_mix4+task_mix5| county_na+quarter_year, subset_firm_salon))]
subset_firm_salon[,r_req_rate:=resid(feols(req_rate~task_mix2+task_mix3+task_mix4+task_mix5| county_na+quarter_year, subset_firm_salon))]
subset_firm_salon[,round_req_rate:=get_midpoint(cut(r_req_rate,9, include.lowest=TRUE))]
subset_firm_salon[,r_emp:=resid(feols(emps~task_mix2+task_mix3+task_mix4+task_mix5| county_na+quarter_year, subset_firm_salon))]

tgc <- summarySE(as.data.frame(subset_firm_salon), measurevar="r_rev", groupvars=c("round_req_rate"))
ggplot(tgc, aes(x=round_req_rate, y=r_rev)) + 
  geom_errorbar(aes(ymin=r_rev-se*qnorm(0.975), ymax=r_rev+se*qnorm(0.975)), width=.02,size=1) +
  geom_line(size=1) +
  geom_point(shape=21, size=3, fill="white")+ theme_bw(base_size=22)+
  xlab("Residualized Staff Requested Rate") + ylab("Residualized Revenue ($)")

ggsave("out/figures/00_01b_reqrate_rev_resid_subset.png", width=12, heigh=6, units="in")

tgc <- summarySE(as.data.frame(subset_firm_salon), measurevar="r_emp", groupvars=c("round_req_rate"))
ggplot(tgc, aes(x=round_req_rate, y=r_emp)) + 
  geom_errorbar(aes(ymin=r_emp-se*qnorm(0.975), ymax=r_emp+se*qnorm(0.975)), width=.02,size=1) +
  geom_line(size=1) +
  geom_point(shape=21, size=3, fill="white")+ theme_bw(base_size=22)+
  xlab("Residualized Staff Requested Rate") + ylab("Residualized Employee Count")

ggsave("out/figures/00_01b_reqrate_emp_resid_subset.png", width=12, heigh=6, units="in")





---

######## regressions of firm size on complexity by employee count.


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
firm_quarter[, r_rev_emp:=revenue/emps]

#### regressions with employee count interacted

## revenue
res3<-feols(revenue~task_mix2+task_mix3+task_mix4+task_mix5+factor(emps)*s_index , data=firm_quarter[emps<=40])
point_ests<-coef(res3)
se_ests<-se(res3)
keep<-which(str_detect(names(point_ests), "factor\\(emps\\)(\\d+)\\:"))
forplot<-data.table(est=point_ests[keep], se=se_ests[keep])








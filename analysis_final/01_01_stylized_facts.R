### this script creates all sylized facts
### this script also creates some non-task variables.
library(qdapDictionaries)
library('data.table')
library('lubridate')
library('stringr')
library('zoo')
library('binsreg')
library('ggplot2')
library('lessR')
library('stats')
library('dendextend')
library('qgraph')
library('adespatial')
library('quanteda')
library('DescTools')
library('stringdist')
library('knitr')
library('kableExtra')
library('fixest')
library('pander')
library('stargazer')
library(showtext)

showtext_auto()
showtext_opts(dpi = 300)

my_style = style.df(depvar.title = "", fixef.title = "", 
                    fixef.suffix = " fixed effect", yesNo = "yes")
setFixest_etable(style.df = my_style, postprocess.df = pandoc.table.return)


is.word  <- Vectorize(function(x) x %in% GradyAugmented)

set.seed(588621)
rowMax <- function(data) apply(data,1, max, na.rm = TRUE)
rowMin <- function(data) apply(data,1, min, na.rm = TRUE)
spec_log<-function(x)  ifelse(x==0 | x==-Inf | is.nan(x),0,log(x))

working<-data.table(readRDS("mkdata/data/tasks_cosmo.rds"))

## merge extension task with blowdry task.
working[, clust:=ifelse(clust==3,4 ,clust)]
working[, rep_text_cluster:=ifelse(clust==4,"Blowdry/Style/Treatment/Extension" ,rep_text_cluster)]
working[, clust:=frank(clust, ties.method="dense")]
keytask<-unique(working[, c("clust", "rep_text_cluster")])
keytask[, task:=clust]
keytask[, type:=clust]
saveRDS(keytask, "analysis_final/data/01_00_keytask.rds")

#### data issue: drop 5 observations with negative time
stopifnot(nrow(working[duration<0])==5)
working<-working[duration>=0,]

## impute 0 time as average among quarter cluster (1% are imputed.)
nrow(working[duration==0,])/nrow(working)
working[, temp_dur:=ifelse(duration==0,NA,duration)]
working[, quarter_year:=year(date)+quarter(date)/10]
working[, year:=year(date)]
working[, temp_mean_dur:=mean(temp_dur, na.rm=TRUE), by=c("quarter_year", "clust")]
working[,duration:=ifelse(duration==0,temp_mean_dur,duration) ]
working[, c("temp_dur", "temp_mean_dur"):=list(NULL, NULL)]


### map zip codes to county
countypop<-fread('mkdata/raw/20220727_countypop/geocorr2022_2220806816.csv')[-1]
countypop[,CSPOP:=pop20]
stopifnot(uniqueN(countypop$county)==nrow(countypop))
data<-fread('mkdata/raw/20220727_countypop/geocorr2022_2220801561.csv')[-1]
data[, count:=uniqueN(county),by=zcta]
data<-data[afact>0.50 | count==1] # mapping only if more than 50 percent of zip is within county.
stopifnot(uniqueN(data$zcta)==nrow(data))
data<-merge(data, countypop[,c("county")], by="county")
stopifnot(uniqueN(data[zcta %in% unique(working$location_zip) ]$zcta)==nrow(data[zcta %in% unique(working$location_zip) ]))
data[, location_zip:=as.numeric(zcta)]
working<-merge(working,data[,c("location_zip", "county")], by="location_zip", all.x=TRUE)
stopifnot(uniqueN(working[is.na(county), location_id])==2)# one NA and one zip code not matched.

### get populations based on estimate for each year (except for census number in 2020)
working<-merge(working, readRDS("mkdata/data/county_census_pop.rds"), by=c("county", "year"),all.x=TRUE)



### drop firm-quarters that have 0 price
temp<-working[,.(rev=sum(price)),by=c("quarter_year", "location_id")]
temp[, is_zero:=rev<=0]
print(nrow(temp[is_zero==1]))
temp[,rev:=NULL]
working<-merge(working, temp, by=c("quarter_year", "location_id"), all.x=TRUE)
working<-working[is_zero==0,]
rm(temp)

###


## measures of within visit specialization and whether staff was requested.
setkey(working, location_id, customer_id, date, app_id)
visit_data<-working[, .(services_invisit=.N,staff_invisit=uniqueN(staff_id)),by=c("location_id","customer_id", "date", "quarter_year")]
visit_data[,multi_service:= services_invisit>1 ]
visit_data[,multi_staff:= staff_invisit>1]
visit_data<-visit_data[,.(multi_staff=sum(multi_staff), multi_service=sum(multi_service) ), by=c("location_id", "quarter_year")]
visit_data[multi_service>0, multi_rate:=multi_staff/multi_service]

## fraction of customers that are returners from a prior quarter.
setkey(working, location_id, customer_id, date, app_id)
working[, first_visit:=min(date), by=c("location_id", "customer_id")]
working[, last_visit:=max(date), by=c("location_id", "customer_id")]
working[, repeat_customer:= first_visit<quarter(date,type="date_first") ]
working[, pastrepeat_id:=  ifelse(repeat_customer, customer_id,NA)]
## fraction of customers in current quarter that come again in a future quarter.
working[, return_future:= last_visit>quarter(date,type="date_last") ]
working[, futurereturn_id:=  ifelse(return_future, customer_id,NA)]

## prebooking
working[prebooked==TRUE, prebook_app:=app_id ]
working[, prebook_date:=ifelse(prebooked, date,NA) ]

## was staff requested.
working[was_staff_requested==TRUE, staffreq_app:=app_id ]
working[, staffreq_date:=ifelse(was_staff_requested, date,NA) ]


## part of multi-establishment
working[, is_multi:= uniqueN(location_id)>1,by=c("business_id")]


## number of misspelling of 
service_text<-unique(working[,"service_performed"])
service_text[, clean_text:=gsub("[^A-Za-z0-9 ]","",service_performed)]
service_text[, clean_text:=str_to_lower(clean_text)][,clean_text:=gsub("'s","",clean_text)][,clean_text:=gsub('[[:punct:]]+',' ',clean_text)][,clean_text:=gsub('[[:digit:]]+', '', clean_text)]
##nrow(text_to_clean)-uniqueN(text_to_clean$clean_task)

allcorpus<-dfm(tokens(service_text$clean_text))
top_words<-names(topfeatures(allcorpus,n=125))
top_words<-top_words[str_length(top_words)>3]

mistake<-Vectorize(function(x){
  interim<-tokens(x)
  interim_length<-str_length(interim)
  is_word<-is.word(interim)
  res<-stringdistmatrix(interim,top_words)
  res<-res/interim_length
  res[which(is_word),]<-0
  ## any time edit length is greater than 25%, exclude
  res[res>0.25]<-0
  return(sum(res)>0)
  
})

service_text[clean_text!="", is_mistake:=mistake(clean_text) ]

working<-merge(working, service_text[,c("service_performed", "is_mistake")], by="service_performed", all.x=TRUE)
working[, mistake_count:=ifelse(is_mistake, service_performed,NA)]


## chair renters
chairrenter <- fread("C:/Users/jakek/blvd_dont_backup/20201204_chair_renters/staff_renters.csv")
working<-merge(working,chairrenter, by=c("staff_id", "business_id"),all.x=TRUE)

working[!is.na(chair_renter), count_chairrenter:=ifelse(chair_renter, staff_id,NA)]
working[is.na(chair_renter),count_chairrenter:=NA ]



## tips - earliest date tipping function is used

top <- fread("C:/Users/jakek/blvd_dont_backup/20200909_raw/Marketing Intern Data/x00.csv")
files<-list.files(path="C:/Users/jakek/blvd_dont_backup/20200909_raw/Marketing Intern Data/",pattern = "*.csv")
tip_data1 <- rbindlist(lapply(paste0("C:/Users/jakek/blvd_dont_backup/20200909_raw/Marketing Intern Data/",files[-1]), fread))
names(tip_data1)<-names(top)
tip_data1<-rbind(tip_data1, top)
rm(top)
# only 5 obs with tips - clearly some issue with this.

tip_data2<-fread('C:/Users/jakek/blvd_dont_backup/20201214_tip_more/bi_tip_lines_2017-2020.csv')

tip_data3<-fread('C:/Users/jakek/blvd_dont_backup/20210809_alldata_refresh_withzip/tip_amount_export.csv')

setnames(tip_data3, "APPOINTMENT_ID", "app_id")
setnames(tip_data2, "appointment_id", "app_id")

# the one with customers is unique by customer within app:
stopifnot(uniqueN(tip_data3[, c("CUSTOMER_ID", "app_id")])==uniqueN(tip_data3[, c("app_id")]))
# there are multiple rows but they convey different amounts. so sum up within customer-app:
tip_data3<-tip_data3[,.(tot_amount=sum(AMOUNT), customer_id=unique(CUSTOMER_ID)), by=c("app_id")]

# the one with staff has 300,000ish obs where multiple staff tipped. cast by appointment.
tip_data2[, tot_amount:=sum(amount), by="app_id"]
setkey(tip_data2, "app_id", "staff_id")
tip_data2[, num_staff:=1:.N, by="app_id"]
wide_tip_data2<-dcast(tip_data2, app_id+tot_amount~num_staff, value.var=c("amount", "staff_id"))
tip_merged<-merge(tip_data3, tip_data2, all=TRUE, by="app_id")

# tips are in cents in one and dollars in the other
tip_merged[, tot_amount.x:=tot_amount.x*100]

# how much overlap?
nrow(tip_merged[(!is.na(tot_amount.x) & !is.na(tot_amount.y)),])/nrow(tip_merged)

# among overlapping, how many disagreements? only 6 out of nearly 3 million
tip_merged[, tip_discrep:=abs(tot_amount.x-tot_amount.y)>1e-08 & (!is.na(tot_amount.x) & !is.na(tot_amount.y))]
nrow(tip_merged[tip_discrep==TRUE,])
# look at disagreements
# View(tip_merged[tip_discrep==1,])
# for these, all are within a cent. go with the data stored in cents. (tot_amount.y)
tip_merged[, tip_amount:=tot_amount.x]
tip_merged[is.na(tip_amount), tip_amount:=tot_amount.y]
tip_merged[tip_discrep==TRUE, tip_amount:=tot_amount.y]
stopifnot(nrow(tip_merged[is.na(tip_amount)])==0)
# make date separate tip date var for merge with all transactions.
setnames(tip_merged,"time", "tip_datetime")

tip_merged<-tip_merged[,.( tip=sum(tip_amount,na.rm=TRUE)), by="app_id"]

working<-merge(working, tip_merged, by="app_id", all.x=TRUE)
working[, tip_date:=ifelse(is.na(tip),NA,date)]
working[, tip_percent:=ifelse(is.na(tip) | price<=0,NA,tip/price)]


## compile
working[, first_prebook:=min(prebook_date,na.rm=TRUE), by="location_id" ]
working[, first_staffreq:=min(staffreq_date,na.rm=TRUE), by="location_id" ]
working[,first_tip:=min(tip_date,na.rm=TRUE), by="location_id" ]
working[, first_observed:=min(date), by="location_id"]
firm_quarter<-working[,.(cust_count=uniqueN(customer_id),pastrepeat_rate=uniqueN(pastrepeat_id, na.rm=TRUE)/uniqueN(customer_id),
                         futurereturn_rate=uniqueN(futurereturn_id, na.rm=TRUE)/uniqueN(customer_id),
                         uniq_desc=uniqueN(service_performed),
                         mistake_rate=uniqueN(mistake_count, na.rm=TRUE)/uniqueN(service_performed),
                         avg_tip_percent=mean(tip_percent,na.rm=TRUE),
                         renter_rate=uniqueN(count_chairrenter, na.rm=TRUE)/uniqueN(staff_id),
                         app_count=uniqueN(app_id), prebook_rate=uniqueN(prebook_app, na.rm=TRUE)/uniqueN(app_id),
                         staffreq_rate=uniqueN(staffreq_app, na.rm=TRUE)/uniqueN(app_id),
                         emps=uniqueN(staff_id),rev_labor=sum(price)/sum(duration), revenue=sum(price)
                         ),by=c("location_id", "quarter_year", "first_tip", "first_prebook","first_staffreq", "is_multi","first_observed",
                                "location_state")]

firm_quarter<-merge(firm_quarter, visit_data,by=c("location_id", "quarter_year"), all.x=TRUE)   

## add s-index.
full_unsmoothed<-readRDS("analysis_final/data/01_00_staff_task_full.rds")
full_unsmoothed<-unique(full_unsmoothed[,.SD, .SDcols=c(colnames(full_unsmoothed)[grep("^task_mix",colnames(full_unsmoothed))], "s_index", "location_id", "quarter_year", "county","location_zip")])
firm_quarter<-merge(full_unsmoothed, firm_quarter, by=c("location_id", "quarter_year"), all.x=TRUE)

saveRDS(firm_quarter, "analysis_final/data/01_01_stylied_facts_data.rds")

firm_quarter[, std_sindex:=s_index/sd(s_index, na.rm=TRUE)]
firm_quarter[, std_uniq_desc:=uniq_desc/sd(uniq_desc, na.rm=TRUE)]
firm_quarter[, std_mistake:=mistake_rate/sd(mistake_rate, na.rm=TRUE)]
firm_quarter[, std_tip:=avg_tip_percent/sd(avg_tip_percent, na.rm=TRUE)]
firm_quarter[, std_renter:=renter_rate/sd(renter_rate, na.rm=TRUE)]
firm_quarter[, std_futurereturn:=futurereturn_rate/sd(futurereturn_rate, na.rm=TRUE)]
firm_quarter[, std_pastrepeat:=pastrepeat_rate/sd(pastrepeat_rate, na.rm=TRUE)]
firm_quarter[, std_prebook:=prebook_rate/sd(prebook_rate, na.rm=TRUE)]
firm_quarter[, std_staffreq:=staffreq_rate/sd(staffreq_rate, na.rm=TRUE)]
firm_quarter[,std_emps:=emps/sd(emps, na.rm=TRUE)]
firm_quarter[,std_rev_labor:=rev_labor/sd(rev_labor, na.rm=TRUE)]
firm_quarter[, rev_cust:=revenue/cust_count]
firm_quarter[, std_rev_cust:=rev_cust/sd(rev_cust, na.rm=TRUE)]
firm_quarter[, std_cust:=cust_count/sd(cust_count, na.rm=TRUE)]

### county - one zip code not mapped to county
### manually code as orange county based on la times article: https://www.latimes.com/archives/la-xpm-1996-04-20-me-18603-story.html
firm_quarter[location_zip=='92681', county:='06059']
stopifnot(nrow(firm_quarter[!is.na(location_zip) & is.na(county)])==0)
### one missing zip code: set to be its own category for regressions
stopifnot(uniqueN(firm_quarter[is.na(location_zip)]$location_id)==1)
firm_quarter[is.na(location_zip),location_zip:= "-9999" ]
firm_quarter[location_zip=="-9999" ,county:= "-9999" ]

firm_data<-firm_quarter[,.(avg_sindex=mean(s_index), has_renter=max(renter_rate, na.rm=TRUE)>0, avg_tm_2=mean(task_mix_2),avg_tm_3=mean(task_mix_3),
                           avg_tm_4=mean(task_mix_4),avg_tm_5=mean(task_mix_5)),
                        by=c("location_id", "first_observed", "first_tip", "first_prebook", "first_staffreq","is_multi",
                             "county", "location_zip")]
firm_data[, first_observed:=date(first_observed)-min(date(first_observed))]
firm_data[, std_first:=as.numeric(first_observed/sd(first_observed))]
stopifnot(nrow(firm_data)==uniqueN(firm_data$location_id))

firm_data[, prebook_time:=as.numeric(as_date(first_prebook)-as_date(min(first_prebook,na.rm=TRUE) ))]
firm_data[is.finite(prebook_time), std_prebook_time:=prebook_time/sd(prebook_time)]
firm_data[, staffreq_time:=as.numeric(as_date(first_staffreq)-as_date(min(first_staffreq,na.rm=TRUE)))]
firm_data[is.finite(staffreq_time), std_staffreq_time:=staffreq_time/sd(staffreq_time)]

firm_data[, tip_time:=as.numeric(as_date(first_tip)-as_date(min(first_tip,na.rm=TRUE))) ]
firm_data[is.finite(tip_time), std_tip_time:=tip_time/sd(tip_time)]
firm_data[, std_sindex:=avg_sindex/sd(avg_sindex)]
firm_data[,uses_tip:=is.finite(first_tip)]
firm_data[,uses_prebook:=is.finite(first_prebook)]


## display number of single establishments vs multi.
firm_quarter<-merge(firm_quarter, unique(working[,c("business_id", "location_id")]), by="location_id", all.x=TRUE)
firm_quarter[, locs_bizid:=uniqueN(location_id), by="business_id"]
print(uniqueN(firm_quarter[locs_bizid==1,"business_id"])/uniqueN(firm_quarter[,"business_id"]))

## discount sophistication

product_data1<-fread('C:/Users/jakek/blvd_dont_backup/20210809_alldata_refresh_withzip/product_sales_export.csv')

product_data2<-fread('C:/Users/jakek/blvd_dont_backup/2017_2020_product_sales/2017_2020_product_sales.csv')
colnames(product_data1)<-str_to_lower(colnames(product_data1))
product_data2[,date:=date(ymd_hms(report_at_date)) ]
product_data1[,date:=date(ymd(report_at_date)) ]

colstokeep<-c("date", "location_id","product_name", "discount_amount")
product_data<-rbind(product_data1[,.SD, .SDcols=colstokeep], product_data2[,.SD, .SDcols=colstokeep], fill=TRUE)
product_data[, quarter_year:=year(date)+quarter(date)/10]
product_data<-unique(product_data[, c("location_id", "quarter_year", 
                                      "discount_amount", "product_name")])
product_data<-product_data[, .(uniq_discounts=.N, uniq_products=uniqueN(product_name)),
             by=c("quarter_year", "location_id")]

firm_quarter<-merge(firm_quarter, product_data, by=c("quarter_year", "location_id"), all.x=TRUE)

firm_quarter[, has_productdata:=!is.na(uniq_products) & uniq_products>0,]
firm_quarter[, std_uniq_discounts:=uniq_discounts/sd(uniq_discounts, na.rm=TRUE)]
firm_quarter[, std_multi_rate:=multi_rate/sd(multi_rate, na.rm=TRUE)]

## exclusions
# there is one salon in KY which is an anomaly
firm_quarter<-firm_quarter[location_id!='fb686b3a-a166-469b-88ea-3467a68e2f53',]
# exclude partial quarter
firm_quarter<-firm_quarter[quarter_year!=2021.3,]

firm_quarter[,s_max:=-task_mix_1*spec_log(task_mix_1)-task_mix_2*spec_log(task_mix_2)-task_mix_3*spec_log(task_mix_3)-task_mix_4*spec_log(task_mix_4)-task_mix_5*spec_log(task_mix_5)]

## summary stats for full sample.


firm_stats<-firm_quarter[,c("revenue","emps","cust_count","task_mix_1","task_mix_2", "task_mix_3", "task_mix_4", "task_mix_5")]


names(firm_stats)<-c("Revenue","Employees","Customers",
                     "Share Haircut/Shave", "Share Color/Highlight/Wash", "Share Blowdry/Style/Treatment/Extensions",
                     "Share Admininstrative","Share Nail/Spa/Eye/Misc."
)
stargazer(firm_stats, header=FALSE, type='text')
stargazer(firm_stats, header=FALSE,digits=2, out='analysis_final/out/tables/01_01_summary_stats.tex',single.row = TRUE)




## fact 1: varies greatly across firms, even among firms with the same number of emps
s_index_breaks<-seq(from=min(firm_quarter$s_index), to=max(firm_quarter$s_index), by=0.05)
# use equal spacing now.

## syverson style facts: firm-quarter 
res_fe<-feols(s_index~1|quarter_year+location_id, data=firm_quarter)
quantile(firm_quarter$s_index, c(0.1, 0.9))
quantile(firm_quarter$s_index, c(0.2, 0.8))

quantile(firm_quarter$s_index, c(0.05, 0.95))
quantile(firm_quarter$s_index, c(0.9))/quantile(firm_quarter$s_index, c(0.1))
quantile(firm_quarter$s_index, c(0.75))/quantile(firm_quarter$s_index, c(0.25))
# 12.85 times more specialized.
sd(resid(feols(s_index~task_mix_2+task_mix_3+task_mix_4+task_mix_5|county+quarter_year, firm_quarter)))/sd(firm_quarter$s_index)



# strikingly similar to syverson.
quantile(firm_quarter$rev_labor, c(0.75))/quantile(firm_quarter$rev_labor, c(0.25))
quantile(firm_quarter$rev_labor, c(0.9))/quantile(firm_quarter$rev_labor, c(0.1))
quantile(firm_quarter$rev_labor, c(0.95))/quantile(firm_quarter$rev_labor, c(0.05))

sd(resid(feols(rev_labor~task_mix_2+task_mix_3+task_mix_4+task_mix_5|county+quarter_year+emps, firm_quarter)))/sd(firm_quarter$rev_labor)

temp<-firm_quarter[, .(avg_sindex=mean(s_index)), by=location_id]

quantile(temp$avg_sindex, c(0.1, 0.9))
quantile(temp$avg_sindex, c(0.2, 0.8))

quantile(temp$avg_sindex, c(0.05, 0.95))
quantile(temp$avg_sindex, c(0.9))/quantile(temp$avg_sindex, c(0.1))
quantile(temp$avg_sindex, c(0.75))/quantile(temp$avg_sindex, c(0.25))



temp<-firm_quarter[, .(avg_rev_labor=mean(rev_labor)), by=location_id]

quantile(temp$avg_rev_labor, c(0.1, 0.9))
quantile(temp$avg_rev_labor, c(0.2, 0.8))

quantile(temp$avg_rev_labor, c(0.05, 0.95))
quantile(temp$avg_rev_labor, c(0.9))/quantile(temp$avg_rev_labor, c(0.1))
quantile(temp$avg_rev_labor, c(0.75))/quantile(temp$avg_rev_labor, c(0.25))

temp<-firm_quarter[, .(sindex_p25=quantile(s_index, 0.25),
                       sindex_p75=quantile(s_index, 0.75),
                       rev_labor_p25=quantile(rev_labor, 0.25),
                       rev_labor_p75=quantile(rev_labor,0.75)), by=quarter_year]

ggplot(temp)+geom_line(aes(x=quarter_year, y=sindex_p25), color="blue")+geom_line(aes(x=quarter_year, y=sindex_p75), color="red")+theme_bw() + 
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),panel.background = element_blank(), axis.line = element_line(colour = "black"))

# firm level - 10 times more specialized.
firm_stats<-firm_quarter[,c("rev_labor","s_index")]


names(firm_stats)<-c("Labor Productivity","S-index")
stargazer(firm_stats,summary.stat=c("N","mean","min","p25", "median", "p75","max"), header=FALSE, type='text')
stargazer(firm_stats,summary.stat=c("N","mean","min","p25", "median", "p75","max"), header=FALSE,digits=2, out='analysis_final/out/tables/01_01_dispersion.tex',single.row = TRUE)

# the most productive quartile of firms are more than twice as specialized
summary(firm_quarter[rev_labor<=quantile(firm_quarter$rev_labor, 0.25)]$s_index)
summary(firm_quarter[rev_labor>=quantile(firm_quarter$rev_labor, 0.75)]$s_index)

##histogram
ggplot(firm_quarter, aes(x=s_index)) +
  geom_histogram(color="black", fill="lightblue", size=0.5, bins = 40)+ ylab("Salon-Quarter Count") + xlab("Task Specialization")+ theme(legend.position = "none")+
theme_bw()+ theme(axis.text = element_text(size = 14))+
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),panel.background = element_blank(), axis.line = element_line(colour = "black"))

ggsave("analysis_final/out/figures/01_01_sindex_hist.png", width=4, height=4, units="in")

firm_quarter[, round_emps:=cut(emps, quantile(emps, seq(from=0, to=1, length=13)))]


ggplot(firm_quarter[emps>1,], aes(x=s_index)) +
  geom_histogram(color="black", fill="lightblue", size=0.5, bins = 20)+ ylab("Salon-Quarter Count") + xlab("Task Specialization")+ theme(legend.position = "none")+
  scale_x_continuous(breaks=c(0,0.5,1))+theme_bw() + theme(axis.text = element_text(size = 10))+
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),panel.background = element_blank(), axis.line = element_line(colour = "black"))+
facet_wrap(~ round_emps) 
ggsave("analysis_final/out/figures/01_01_sindex_hist_byemps.png", width=4, height=4, units="in")

## persistence of both - use ar(1) persistence with and without fixed effect
setorder(firm_quarter, "location_id", "quarter_year")
firm_quarter[, quarter_num:=(quarter_year-floor(quarter_year)-0.1)*10*0.25]
firm_quarter[, gap:=floor(quarter_year)+quarter_num-floor(shift(quarter_year))-shift(quarter_num), by="location_id"]
firm_quarter[,l_sindex:=shift(s_index) ,by="location_id"]
firm_quarter[,l_rev_labor:=shift(rev_labor) ,by="location_id"]
summary(feols(s_index~l_sindex, data=firm_quarter[round(gap,6)==0.25]), cluster=~location_id)
summary(feols(s_index~l_sindex|location_id, data=firm_quarter[round(gap,6)==0.25]), cluster=~location_id)
summary(feols(rev_labor~l_rev_labor, data=firm_quarter[round(gap,6)==0.25]), cluster=~location_id)
summary(feols(rev_labor~l_rev_labor|location_id, data=firm_quarter[round(gap,6)==0.25]), cluster=~location_id)



  ## fact 2 - productivity and s-index correlation.
  
  res0<-feols(std_rev_labor~std_sindex,cluster=~location_id, data=firm_quarter)
  res1<-feols(std_rev_labor~std_sindex+task_mix_2+task_mix_3+task_mix_4+task_mix_5,cluster=~location_id, data=firm_quarter)
  res2<-feols(std_rev_labor~std_sindex+task_mix_2+task_mix_3+task_mix_4+task_mix_5|emps,cluster=~location_id, data=firm_quarter)
  res3<-feols(std_rev_labor~std_sindex+task_mix_2+task_mix_3+task_mix_4+task_mix_5 | county+emps,cluster=~location_id, data=firm_quarter)
  res4<-feols(std_rev_labor~std_sindex+task_mix_2+task_mix_3+task_mix_4+task_mix_5 | quarter_year+county+emps,cluster=~location_id, data=firm_quarter)
  res5<-feols(std_rev_labor~std_sindex+task_mix_2+task_mix_3+task_mix_4+task_mix_5 | quarter_year+location_zip+emps,cluster=~location_id, data=firm_quarter)
  res6<-feols(std_rev_labor~std_sindex+task_mix_2+task_mix_3+task_mix_4+task_mix_5 | quarter_year+location_zip^emps,cluster=~location_id, data=firm_quarter)
  
  
  etable(res0, res1,res2,res3,res4,res5,res6, fitstat=~n+r2,keep="!Constant",dict=c(county="County",std_rev_labor = "Revenue per Minute (standardized)", std_sindex="S-Index", location_zip="Zip" ,quarter_year="Quarter-Year", emps="Firm Size",
                                                                    task_mix_2="Color Task Mix",task_mix_3="Blowdry Task Mix",
                                                                    task_mix_4="Admin. Task Mix", task_mix_5="Nail Task Mix", location_id="Establishment"),
         file="analysis_final/out/tables/01_01_productivity_sindex.tex", replace=TRUE,signifCode=c(`***`=0.001,`**`=0.01, `*`=0.05))
  

  
  ## do graphs unconditional and conditional on firm size.
  ggplot(data = firm_quarter, aes( x = s_index, y = rev_labor)) + geom_smooth(method='lm',color = "red", se=FALSE)+ylab("Revenue per Minute")+xlab("Task-Specialization (S-Index)")+
    stat_summary_bin(fun.y = mean, breaks=s_index_breaks, geom = "point")+theme_bw() + theme(axis.text = element_text(size = 14))+
    theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),panel.background = element_blank(), axis.line = element_line(colour = "black"))
  
  ggsave("analysis_final/out/figures/01_01_sindex_prod_all.png", width=4, height=4, units="in")
  
  scaleFUN <- function(x) formatC(signif(x, digits=1), digits=1, format="fg", flag="#")
  
  ggplot(data = firm_quarter[!is.na(round_emps)], aes( x = s_index, y = rev_labor)) + geom_smooth(method='lm',color = "red", se=FALSE)+
    stat_summary_bin(fun.y = mean, breaks=quantile(firm_quarter$s_index,seq(from=0.05, to=0.95,by=0.05)), geom = "point")+ xlab("Task Specialization")+ ylab("Revenue per Minute")+theme_bw() + theme(axis.text = element_text(size = 10))+
    theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),panel.background = element_blank(), axis.line = element_line(colour = "black"))+scale_x_continuous(labels=scaleFUN,breaks=c(0,0.5, 1))+
  facet_wrap(~round_emps) 
  
  ggsave("analysis_final/out/figures/01_01_sindex_prod_byemps.png", width=4, height=4, units="in")
  
  
  # the most specialized quartile of firms on averagegenerate $1.08 more revenue per minute
  # than least specialized quartile. this is 68% more productive.
  summary(firm_quarter[s_index<=quantile(firm_quarter$s_index, 0.25)]$rev_labor)
  summary(firm_quarter[s_index>=quantile(firm_quarter$s_index, 0.75)]$rev_labor)
  
  
  ##  driven by revenue per customer rather than number of customers

  res1<-feols(std_cust~std_sindex+task_mix_2+task_mix_3+task_mix_4+task_mix_5 | quarter_year+location_zip+emps,cluster=~location_id, data=firm_quarter)
  
  res2<-feols(std_rev_cust~std_sindex+task_mix_2+task_mix_3+task_mix_4+task_mix_5 | quarter_year+location_zip+emps,cluster=~location_id, data=firm_quarter)
  res3<-feols(std_futurereturn~std_sindex+task_mix_2+task_mix_3+task_mix_4+task_mix_5 | quarter_year+location_zip+emps,cluster=~location_id, data=firm_quarter)
  
  
  
  etable( res1,res2,res3, fitstat=~n+r2,keep="!Constant",dict=c(std_rev_cust = "Revenue per Customer", std_sindex="S-Index", location_zip="Zip" ,quarter_year="Quarter-Year", emps="Firm Size",
                                                                                          task_mix_2="Color Task Mix",task_mix_3="Blowdry Task Mix",
                                                                                          task_mix_4="Admin. Task Mix", task_mix_5="Nail Task Mix", location_id="Establishment",
                                                              std_cust="Customer Count", std_rev_cust="Rev. per Customer",
                                                              std_futurereturn="Customer Return Rate"
                                                              ),
         file="analysis_final/out/tables/01_01_rev_link_decomp.tex", replace=TRUE,signifCode=c(`***`=0.001,`**`=0.01, `*`=0.05))
  

    ### uncorrelated variance share
    
    ## r2 for the two
    print(r2(feols(s_index~1|emps, data=firm_quarter))['r2'])
    
    uvs<-(r2(feols(std_rev_labor~std_sindex|emps, data=firm_quarter))['r2']-r2(feols(std_rev_labor~1|emps, data=firm_quarter))['r2'])
    print(uvs/r2(feols(std_rev_labor~std_sindex|emps, data=firm_quarter))['r2'])
  
    ## partial r2
    print(uvs/(1-r2(feols(std_rev_labor~std_sindex|emps, data=firm_quarter))['r2']))
    
    ## uvs for size.
    uvs_size<-(r2(feols(std_rev_labor~std_sindex|emps, data=firm_quarter))['r2']-r2(feols(std_rev_labor~s_index, data=firm_quarter))['r2'])
    print(uvs_size/r2(feols(std_rev_labor~std_sindex|emps, data=firm_quarter))['r2'])
    
    ### most aggressive uvs
    uvs_aggr<-(r2(feols(std_rev_labor~std_sindex+task_mix_2+task_mix_3+task_mix_4+task_mix_5 | county+emps, data=firm_quarter))['r2']-r2(feols(std_rev_labor~task_mix_2+task_mix_3+task_mix_4+task_mix_5 | county+emps, data=firm_quarter))['r2'])
    print(uvs_aggr/r2(feols(std_rev_labor~std_sindex+task_mix_2+task_mix_3+task_mix_4+task_mix_5 | county+emps, data=firm_quarter))['r2'])
    
  ## fact 4: connected to other active management practices.
  
  ## robust to using teamwork within visit specialization)
  
  res0<-feols(std_rev_labor~std_multi_rate,cluster=~location_id, data=firm_quarter)
  res1<-feols(std_rev_labor~std_multi_rate+task_mix_2+task_mix_3+task_mix_4+task_mix_5,cluster=~location_id, data=firm_quarter)
  res2<-feols(std_rev_labor~std_multi_rate+task_mix_2+task_mix_3+task_mix_4+task_mix_5 | location_zip,cluster=~location_id, data=firm_quarter)
  res3<-feols(std_rev_labor~std_multi_rate+task_mix_2+task_mix_3+task_mix_4+task_mix_5 | quarter_year+location_zip,cluster=~location_id, data=firm_quarter)
  res4<-feols(std_rev_labor~std_multi_rate+task_mix_2+task_mix_3+task_mix_4+task_mix_5 | quarter_year+location_zip+emps,cluster=~location_id, data=firm_quarter)
  res5<-feols(std_rev_labor~std_multi_rate+task_mix_2+task_mix_3+task_mix_4+task_mix_5 | quarter_year+location_zip^emps,cluster=~location_id, data=firm_quarter)
  
  
  
  etable(res0, res1,res2,res3,res4,res5, fitstat=~n+r2,keep="!Constant",dict=c(std_rev_labor = "Revenue per Minute", std_multi_rate="Teamwork", location_zip="Zip" ,quarter_year="Quarter-Year", emps="Firm Size",
                                                                             task_mix_2="Color Task Mix",task_mix_3="Blowdry Task Mix",
                                                                             task_mix_4="Admin. Task Mix", task_mix_5="Nail Task Mix", location_id="Establishment"),
         file="analysis_final/out/tables/01_01_productivity_teamwork.tex", replace=TRUE,signifCode=c(`***`=0.001,`**`=0.01, `*`=0.05))
  
  ggplot(data = firm_quarter, aes( x = s_index, y = multi_rate)) + geom_smooth(method='lm',color = "red", se=FALSE)+
    stat_summary_bin(fun.y = mean, breaks=s_index_breaks, geom = "point")+theme_bw() + 
    theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),panel.background = element_blank(), axis.line = element_line(colour = "black"))+
    theme(axis.text = element_text(size = 14))+
    xlab("Task Specialization (S-Index)")+ylab("Teamwork")
  
  ggsave("analysis_final/out/figures/01_01_sindex_teamwork.png", width=4, height=4, units="in")
  
  

  res0<-feols(std_multi_rate~std_sindex+task_mix_2+task_mix_3+task_mix_4+task_mix_5 | location_zip+quarter_year,cluster=~location_id, data=firm_quarter)
  res1<-feols(std_uniq_desc~std_sindex+task_mix_2+task_mix_3+task_mix_4+task_mix_5 | location_zip+quarter_year,cluster=~location_id, data=firm_quarter)
  res2<-feols(std_uniq_discounts~std_sindex+task_mix_2+task_mix_3+task_mix_4+task_mix_5 | location_zip+quarter_year,cluster=~location_id, data=firm_quarter)
  res3<-feols(std_first~std_sindex,cluster=~location_id, data=firm_data)
  res4<-feols(std_tip_time~std_sindex,cluster=~location_id, data=firm_data)

  res5<-feols(std_prebook_time~std_sindex,cluster=~location_id, data=firm_data)
  res6<-feols(std_staffreq_time~std_sindex,cluster=~location_id, data=firm_data)
  
  
  
  etable(res0, res1,res2,res3,res4,res5,res6, fitstat=~n+r2,keep="!Constant",dict=c(std_cust = "Customer Count", std_sindex="S-Index", location_zip="Zip" ,quarter_year="Quarter-Year", emps="Firm Size",
                                                                             task_mix_2="Color Task Mix",task_mix_3="Blowdry Task Mix",
                                                                             task_mix_4="Admin. Task Mix", task_mix_5="Nail Task Mix", location_id="Establishment",
                        std_multi_rate="Teamwork", std_uniq_desc="Service Descriptions",
                        std_uniq_discounts="Product Discounts",std_tip_time="Tip Feature",
                        std_prebook_time="Prebook Feature",std_staffreq_time="Request Feature",
                        std_first="Software Adopted"),
         file="analysis_final/out/tables/01_01_management_practices.tex", replace=TRUE,signifCode=c(`***`=0.001,`**`=0.01, `*`=0.05))
  

  ## customer utilization of features.
  res1<-feols(std_prebook~std_sindex+task_mix_2+task_mix_3+task_mix_4+task_mix_5 | location_zip+quarter_year,cluster=~location_id, data=firm_quarter[quarter_year>=year(as_date(first_prebook))+quarter(as_date(first_prebook))/10])
  res2<-feols(std_tip~std_sindex+task_mix_2+task_mix_3+task_mix_4+task_mix_5 | county+quarter_year,cluster=~location_id, data=firm_quarter[quarter_year>=year(as_date(first_tip))+quarter(as_date(first_tip))/10])
  
  res3<-feols(std_staffreq~std_sindex+task_mix_2+task_mix_3+task_mix_4+task_mix_5 | location_zip+quarter_year,cluster=~location_id, data=firm_quarter[quarter_year>=year(as_date(first_staffreq))+quarter(as_date(first_staffreq))/10])
  
  
  
  ## mediation analysis for staff req.
  
  model.M <- lm(std_staffreq ~ std_sindex, firm_quarter[quarter_year>=year(as_date(first_staffreq))+quarter(as_date(first_staffreq))/10])
  summary(model.M)
  model.Y <- lm(std_rev_labor ~ std_sindex+std_staffreq, firm_quarter[quarter_year>=year(as_date(first_staffreq))+quarter(as_date(first_staffreq))/10])
  summary(model.Y)
  
  library(mediation)
  results <- mediate(model.M, model.Y, treat='std_sindex', mediator='std_staffreq',
                     boot=TRUE, sims=500)
  summary(results)
  
  summary(firm_quarter[quarter_year>=year(as_date(first_staffreq))+quarter(as_date(first_staffreq))/10]$s_index)
  
  summary(firm_quarter[quarter_year>=year(as_date(first_staffreq))+quarter(as_date(first_staffreq))/10]$staffreq_rate)
  

### testable implication
  firm_quarter[, year:=floor(quarter_year)]
  all_years<-data.table()
library('tidycensus')
  for (y in 2011:2021){
    zcta_income = get_acs(
      geography = "zcta",
      variables = "B19013_001",
      year      = y)
    zcta_income<-data.table(zcta_income)
    setnames(zcta_income, "GEOID", "location_zip")
    zcta_income[,location_zip:=as.integer(location_zip) ]
    zcta_income[, year:=y]
    all_years<-rbind(zcta_income,all_years)
    
  }

  firm_quarter<-merge(firm_quarter, all_years, by=c("location_zip", "year"), all.x=TRUE)
firm_quarter[, s_estimate:=estimate/sd(estimate, na.rm=TRUE)]
feols(std_sindex~s_estimate, data=firm_quarter)
feols(std_sindex~s_estimate|year, data=firm_quarter)


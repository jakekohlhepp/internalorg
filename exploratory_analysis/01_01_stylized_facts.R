### explore hypotheses on specialization
library('checkpoint')
#checkpoint('2020-07-16') activate to reproduce
library('data.table')
library('ggplot2')
library('lubridate')
library('stringr')
library('survminer')
library('fixest') ## using newest version
library('DescTools')
library('zoo')
library('eulerr')
library('binsreg')
setwd('C:/Users/jakek/Google Drive/Working Documents/econ_phd/jmp/exploratory_analysis/')
theme_set(theme_bw(base_size=22))

firm_week<-readRDS('data/01_01_spec_metric.rds')

firm_week[, first_observed:=min(week_start_date), by=c("location_id")]
firm_week[,last_observed:=max(week_start_date), by=c("location_id")]

all_combos<-data.table(expand.grid(week_start_date=
                                     lubridate::ymd(as.character(seq(min(firm_week$week_start_date),date('2021-09-06'), by = '1 week'))), location_id=unique(firm_week$location_id)))
all_combos[, tag:=1]
firm_week<-merge(firm_week,all_combos,by=c("location_id", "week_start_date"), all=TRUE)
stopifnot(nrow(firm_week[is.na(tag)])==0)

setkey(firm_week, location_id, first_observed,week_start_date)
firm_week[, first_observed:=min(first_observed, na.rm=TRUE), by=c("location_id")]
firm_week[,last_observed:=max(last_observed, na.rm=TRUE), by=c("location_id")]
firm_week<-firm_week[week_start_date>=first_observed & week_start_date<=last_observed,]

## reattach location, etc.
firm_week[, location_state:=min(location_state, na.rm=TRUE), by=c("location_id")]
firm_week[,location_city:=max(location_city, na.rm=TRUE), by=c("location_id")]
firm_week[,location_zip:=min(location_zip, na.rm=TRUE), by=c("location_id")]
firm_week[,business_id:=max(business_id, na.rm=TRUE), by=c("location_id")]

## dist, services and emps all 0 when na
firm_week[is.na(dist_equal), dist_equal:=0]
firm_week[is.na(emps), emps:=0]
firm_week[is.na(service_types), service_types:=0]

firm_week[,emp_serv_id:=paste0(as.character(service_types), "-", as.character(emps))]
firm_week[,city_state:=paste0(as.character(location_city), "-", as.character(location_state))]

a<-100
b<- -11
## fact -1: neoclassical story seems inadequate.
locs<-c("3dc261ed-c8e3-40d1-801f-35cfe0eeccd2","ceed9b97-200e-4ffd-b4b0-64dab92ee26b",
        "a1e07ff3-51d5-4312-8313-e35583a53563","e63671a0-2164-4f4a-a254-c13b65043ec6")

locs<-c("a1e07ff3-51d5-4312-8313-e35583a53563","e63671a0-2164-4f4a-a254-c13b65043ec6")

examples<-firm_week[week_start_date<date('2020-01-01') & week_start_date>=date('2014-01-01') & location_id %in% locs,]
setkey(examples, location_id, week_start_date)
examples[, r_emps:=frollmean(emps, 8)]
examples[, r_dist:=frollmean(dist_equal, 8)]
# "e63671a0-2164-4f4a-a254-c13b65043ec6"
examples[, firmnum:=paste("Firm", as.numeric(factor(location_id)))]
p<-ggplot(data=examples, aes(x=week_start_date)) +
  geom_line(aes(y=r_dist, linetype="S-Index Lower Bound"))+ scale_x_date(date_labels = "%Y")+
  geom_line(aes(y=(r_emps-b)/a, linetype="Employees"))+
  ylab("") + xlab("")+ labs(linetype = element_blank())+ theme(legend.position="bottom")+ 
  scale_y_continuous(
    "Specialization", 
    sec.axis = sec_axis(~ . * a+b, name = "Employees")
  )

p+facet_wrap(vars(firmnum), scales="free")
ggsave("out/01_01_empgrowth_examples.png", width=16, heigh=6, units="in")

## fact 0: specialization does not vary much across service counts
## bin into twenty bins
firm_week[, r_emp:=floor(emps/5)*5]
color_map<-firm_week[!is.na(dist_equal) & !is.na(mean_haircut), .(firms=uniqueN(location_id),obs=.N,mean_dist=mean(dist_equal, na.rm=TRUE), cor_dist=cor(dist_equal, mean_haircut)), by=c("r_emp", "service_types")]

ggplot(color_map, aes(r_emp, service_types, fill= mean_dist)) + 
  geom_tile() +scale_fill_gradient2(
    low = "white",
    high = "blue")+ylab("Task Count")+xlab("Employee Count")+ labs(fill='Spec. Index') 
ggsave("out/01_01_index_grid.png", width=12, heigh=6, units="in")

bin_price_specialization<-binsreg(firm_week$dist_equal, 
                                  firm_week$emps, 
                                  nbins=40)
bin_price_specialization$bins_plot + theme_bw(base_size=22)+
  ylab("S-index Lower Bound") + xlab("Employee Count")
ggsave("out/01_01_binreg_emps_specialization.png", width=12, heigh=6, units="in")

bin_price_specialization<-binsreg(firm_week[location_city=="New York"]$dist_equal, 
                                  firm_week[location_city=="New York"]$emps, 
                                  nbins=40)
bin_price_specialization$bins_plot + theme_bw(base_size=22)+
  ylab("S-index Lower Bound") + xlab("Employee Count")
ggsave("out/01_01_binreg_emps_specialization_nyc.png", width=12, heigh=6, units="in")


bin_price_specialization<-binsreg(firm_week[location_city=="Hollywood"]$dist_equal, 
                                  firm_week[location_city=="Hollywood"]$emps, 
                                  nbins=40)
bin_price_specialization$bins_plot + theme_bw(base_size=22)+
  ylab("S-index Lower Bound") + xlab("Employee Count")
ggsave("out/01_01_binreg_emps_specialization_hollywood.png", width=12, heigh=6, units="in")


bin_price_specialization<-binsreg(firm_week[location_city=="Los Angeles"]$dist_equal, 
                                  firm_week[location_city=="Los Angeles"]$emps, 
                                  nbins=40)
bin_price_specialization$bins_plot + theme_bw(base_size=22)+
  ylab("S-index Lower Bound") + xlab("Employee Count")
ggsave("out/01_01_binreg_emps_specialization_la.png", width=12, heigh=6, units="in")



res_dist<-resid(feols(dist_equal~factor(city_state)|factor(week_start_date)+service_types, data=firm_week))
res_emps<-resid(feols(emps~factor(city_state)|factor(week_start_date)+service_types, data=firm_week))

bin_price_specialization<-binsreg(y=res_dist, 
                                  x=res_emps,
                                  nbins=40)
bin_price_specialization$bins_plot +
  theme_bw(base_size=22) + ylab("Residualized S-Index Lower Bound")+
  xlab("Residualized Employee Count")
ggsave("out/01_01_binreg_emps_specialization_controlled.png", width=12, heigh=6, units="in")


## Fact 1: Specialization Strategy Varies Greatly Across Salons
firm<-firm_week[,.(avg_dist=mean(dist_equal)), by=location_id]
ggplot(firm, aes(x=avg_dist)) +
  geom_histogram(color="darkblue", fill="lightblue")+ ylab("Frequency") + xlab("Firm Average S-Index Lower Bound")+ theme(legend.position = "none")
ggsave("out/01_01_heterogenous_hist.png", width=12, heigh=6, units="in")

## Fact 2: Specialization Strategy is Time Invariant and Firm Specific
model<-lm(dist_equal~ factor(week_start_date)+factor(location_id), data=firm_week)
tot<-predict(model)

hold<-firm_week[emps>0 & service_types>0 & week_start_date<=date('2020-03-01'),]
hold[, mean_loc:=mean(dist_equal,na.rm=TRUE), by=location_id]

hold<-hold[, .(count=.N, withinpiece=sum((dist_equal-mean_loc)^2,na.rm=TRUE) ), by=c("location_id","mean_loc")]

among<-sum(hold$count*(hold$mean_loc-mean(firm_week[emps>0,]$dist_equal, na.rm=TRUE))^2/sum(hold$count))
within<-sum(hold$withinpiece)/sum(hold$count)

locmodel<-predict(lm(dist_equal~ factor(location_id), data=firm_week[emps>0,]))
datemodel<-predict(lm(dist_equal~ factor(week_start_date), data=firm_week[emps>0,]))

diagram<-euler(c("Firm" = round((var(tot)-var(datemodel))/var(firm_week[emps>0,]$dist_equal),2),
               "Time" = round((var(tot)-var(locmodel))/var(firm_week[emps>0,]$dist_equal),2),
               "Firm&Time" =  round((var(tot) -(var(tot)-var(datemodel)) -
                                        (var(tot)-var(locmodel)))/var(firm_week[emps>0,]$dist_equal) ,2)))
png("out/01_01_venn.png", width = 800, height = 800) 
plot(diagram, quantities = list(fontsize = 25), fills = list(fill=c("dodgerblue4", "darkgoldenrod1", "cornsilk4"), alpha=0.7),
     labels=NULL, legend = list(labels = c("Firm", "Time"), cex=3))
dev.off()
stopifnot(nrow(firm_week[is.na(location_state)])==0)
ggplot(firm_week[week_start_date>=date("2017-01-01") & location_state=="NY"     ], aes(x = week_start_date, y = roll_dist_equal,color=factor(location_id) )) +
  geom_line(size=0.5) + ylab("Rolling Average S-Index Lower Bound") + xlab("Week")+ theme(legend.position = "none")
ggsave("out/01_01_spec_timeseries_ny.png", width=12, heigh=6, units="in")

## even the covid-19 pandemic does not impact.
covid<-firm_week[ first_observed<=date('2019-01-01') & last_observed>=date('2021-06-01'),]
covid<-covid[, .(avg_dist=mean(dist_equal,na.rm=TRUE),
                 avg_emps=mean(emps, na.rm=TRUE)), by=c("week_start_date", "location_state")]
ggplot(data=covid[week_start_date>=date('2019-01-01') & location_state %in% c("NY", "CA")], aes(x=week_start_date, y=avg_dist, linetype=location_state)) +
  geom_line()+
  ylab("Avg. S-Index Lower Bound") + xlab("")+ labs(linetype = element_blank())+ theme(legend.position = "bottom")
ggsave("out/01_01_covid_spec.png", width=9, heigh=6, units="in")
ggplot(data=covid[week_start_date>=date('2019-01-01') & location_state %in% c("NY", "CA")], aes(x=week_start_date, y=avg_emps, linetype=location_state)) +
  geom_line()+
  ylab("Avg. Employees") + xlab("")+ labs(linetype = element_blank())+ theme(legend.position = "bottom")
ggsave("out/01_01_covid_emp.png", width=9, heigh=6, units="in")
#cutD <- function(x,n) {
#  cut(x, breaks=c(unique(quantile(x, probs = seq(0, 1, by = 1/n),na.rm = T, include.lowest=TRUE))))
#}

stopifnot(nrow(firm_week[is.na(location_state)])==0)


### Fact 2: Specialization Rises with Price
ggplot(firm_week[mean_haircut<300], aes(x = mean_haircut, y = dist_equal,color=location_id )) +  
  geom_point(size=1) + ylab("S-Index Lower Bound") + xlab("Average Weekly Haircut Price ($)")+ theme(legend.position = "none")
ggsave("out/01_01_all_price_specialization.png", width=12, heigh=6, units="in")

bin_price_specialization<-binsreg(firm_week$dist_equal, 
                                  firm_week$mean_haircut, 
                                 nbins=40)
bin_price_specialization$bins_plot + theme_bw(base_size=22)+xlim(0, 200)+
  ylab("S-Index Lower Bound") + xlab("Average Weekly Haircut Price ($)")
ggsave("out/01_01_binreg_price_specialization.png", width=12, heigh=6, units="in")

bin_price_specialization<-binsreg(firm_week[location_city=="New York"]$dist_equal, 
                                  firm_week[location_city=="New York"]$mean_haircut, 
                                  nbins=40)
bin_price_specialization$bins_plot + theme_bw(base_size=22)+
  ylab("S-Index Lower Bound") + xlab("Average Weekly Haircut Price ($)")
ggsave("out/01_01_binreg_price_specialization_nyc.png", width=12, heigh=6, units="in")

bin_price_specialization<-binsreg(firm_week[location_city=="Los Angeles"]$dist_equal, 
                                  firm_week[location_city=="Los Angeles"]$mean_haircut, 
                                  nbins=40)
bin_price_specialization$bins_plot + theme_bw(base_size=22)+
  ylab("S-Index Lower Bound") + xlab("Average Weekly Haircut Price ($)")
ggsave("out/01_01_binreg_price_specialization_la.png", width=12, heigh=6, units="in")


bin_price_specialization<-binsreg(firm_week[location_city=="Hollywood"]$dist_equal, 
                                  firm_week[location_city=="Hollywood"]$mean_haircut, 
                                  nbins=40)
bin_price_specialization$bins_plot + theme_bw(base_size=22)+
  ylab("S-Index Lower Bound") + xlab("Average Weekly Haircut Price ($)")
ggsave("out/01_01_binreg_price_specialization_hollywood.png", width=12, heigh=6, units="in")

## Fact 3: even controlling for employee-task counts, there is a strong positive relationship between price and specialization
ggplot(color_map, aes(r_emp, service_types, fill= cor_dist)) + 
  geom_tile() +scale_fill_gradient2(
    low = "red",
    mid = "white",
    high = "dark green",
    midpoint = 0  )+ylab("Task Count")+xlab("Employee Count")+ labs(fill='Correlation')+
  geom_text(aes(label=obs))
ggsave("out/01_01_corr_grid.png", width=12, height=6, units="in")





#ggplot(color_map, aes(r_emp, service_types, fill= cor_dist,alpha=exp(logp(obs/max(color_map$obs)))) ) + 
#  geom_tile() +scale_fill_gradient2(
#    low = "red",
#    mid = "white",
#    high = "blue",
#    midpoint = 0  )+ylab("Task Count")+xlab("Employee Count")+ labs(fill='Correlation')



res_dist<-resid(feols(dist_equal~factor(city_state)|factor(week_start_date), data=firm_week))
res_price<-resid(feols(mean_haircut~factor(city_state)|factor(week_start_date), data=firm_week))

bin_price_specialization<-binsreg(y=res_dist, 
                                  x=res_price,
                                  nbins=40)
bin_price_specialization$bins_plot +
  theme_bw(base_size=22) + ylab("Residualized Specialization Index Lower Bound")+
  xlab("Residualized Average Weekly Haircut Price ($)")+xlim(-100, 100)
ggsave("out/01_01_binreg_price_specialization_controlled.png", width=12, heigh=6, units="in")

res_dist<-resid(feols(dist_equal~factor(city_state)|factor(week_start_date)+factor(emp_serv_id), data=firm_week))
res_price<-resid(feols(mean_haircut~factor(city_state)|factor(week_start_date)+factor(emp_serv_id), data=firm_week))

bin_price_specialization<-binsreg(y=res_dist, 
                                  x=res_price,
                                  nbins=40)
bin_price_specialization$bins_plot +
  theme_bw(base_size=22) + ylab("Residualized S-Index Lower Bound")+
  xlab("Residualized Average Weekly Haircut Price ($)")+xlim(-100, 100)
ggsave("out/01_01_binreg_price_specialization_controlled_more.png", width=12, heigh=6, units="in")



## summarize facts in regression table.
firm_week[,s_dist_equal:=dist_equal/sd(dist_equal, na.rm=TRUE)]
firm_week[,s_mean_haircut:=mean_haircut/sd(mean_haircut, na.rm=TRUE)]
res0<-feols(s_dist_equal~s_mean_haircut, data=firm_week)
res1<-feols(s_dist_equal~s_mean_haircut | city_state, data=firm_week)
res2<-feols(s_dist_equal~s_mean_haircut | city_state+week_start_date, data=firm_week)
res3<-feols(s_dist_equal~s_mean_haircut | city_state+week_start_date+emp_serv_id, data=firm_week)
esttex(res0, res1,res2,res3, fitstat=~r2, cluster=firm_week$location_id, file="out/01_01_reg_price_spec.tex", replace=TRUE, se="cluster")



### specialization by survival.
## define survival as existing before and after covid
covid<-firm_week[first_observed<=date('2020-01-01') & last_observed>=date('2020-01-01'),]
covid[,closed:=last_observed<=date('2021-06-01')]
# pre-period is first quarter 2020
# post period is first quarter 2021
covid[, pre:=week_start_date>=date('2020-01-01') & week_start_date<=date('2020-03-01')]
covid[, post:=week_start_date>=date('2021-01-01') & week_start_date<=date('2021-03-01')]
covid<-covid[,.(pre_dist=sum(pre*dist_equal, na.rm=TRUE)/sum(pre*!is.na(dist_equal)),
                post_dist=sum(post*dist_equal, na.rm=TRUE)/sum(post*!is.na(dist_equal)),
                post_emp=sum(post*emps, na.rm=TRUE)/sum(post*!is.na(emps)),
                pre_emp=sum(pre*emps, na.rm=TRUE)/sum(pre*!is.na(emps))), by=c("closed", "location_id", "city_state", "location_zip", "last_observed")]
summary(feols(closed~pre_dist+pre_emp|city_state, data=covid))

### pre-covid dist is highly predictive of survival.
summary(feols(closed~pre_dist+pre_emp|city_state, data=covid))

### plot survival curve
covid[, time:=last_observed-date('2020-01-01')]
covid[,spec:=pre_dist>=mean(pre_dist) ]
fit<-survfit(Surv(time,closed)~spec, data=covid)
ggsurvplot(fit, data=covid)








model<-lm(emps~ factor(week_start_date)+factor(location_id), data=firm_week[emps>0,])
tot<-predict(model)


locmodel<-predict(lm(emps~ factor(location_id), data=firm_week[emps>0,]))
datemodel<-predict(lm(emps~ factor(week_start_date), data=firm_week[emps>0,]))

diagram<-euler(c("Firm" = round((var(tot)-var(datemodel))/var(firm_week[emps>0,]$emps),2),
                 "Time" = round((var(tot)-var(locmodel))/var(firm_week[emps>0,]$emps),2),
                 "Firm&Time" =  round((var(tot) -(var(tot)-var(datemodel)) -
                                         (var(tot)-var(locmodel)))/var(firm_week[emps>0,]$emps) ,2)))
plot(diagram, quantities = list(fontsize = 25), fills = list(fill=c("dodgerblue4", "darkgoldenrod1", "cornsilk4"), alpha=0.7),
     labels=NULL, legend = list(labels = c("Firm", "Time"), cex=3))




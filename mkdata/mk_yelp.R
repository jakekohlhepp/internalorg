## Read in and process Yelp data.
library('data.table')
library('lubridate')
library('stringr')

yelp_biz<-fread('mkdata/raw/20211006_yelp/data_UCLA_beauty__biz_details__by_business (2).csv')
stopifnot(all(!is.na(yelp_biz$created_date)))
yelp_weekly<-fread('mkdata/raw/20211006_yelp/data_UCLA_beauty_ratings_weekly__by_business-1.csv')
setnames(yelp_weekly,"business_id_encid", "id_encid")

# floyd 99: jyDNsKE6Z_GDKkQNmPc0hw

### clean variables.
### for 1970 start date, set date to 2010-01-01
### for missing end, set to last observed data change.
### fill in zeros and ratings for the gaps.
yelp_biz[,num_created_date:= date(parse_date_time(created_date, orders="ymd HMS"))]
yelp_biz[,num_closure_date:= date(parse_date_time(closure_date, orders="ymd HMS"))]
yelp_weekly[,num_week:= date(parse_date_time(week, orders="ymd"))]

yelp_biz[num_created_date==date("1970-01-01"),num_created_date:=date("2010-01-01")]
max_date<-max(yelp_weekly$num_week, na.rm=TRUE)
yelp_biz[is.na(num_closure_date),num_closure_date:=max_date]

### expand biz data to create an observation on closure date and start date.
helper<-yelp_biz[,c("num_closure_date", "num_created_date", "id_encid") ]
### expand to fill between
pad<-data.table(expand.grid(num_week=seq( ymd("2010-01-01"),max_date, by = "week"),id_encid=unique(yelp_biz$id_encid) ))
helper<-merge(helper, pad, by="id_encid", all=TRUE)
rm(pad)
helper<-helper[num_week %within% interval(num_created_date,num_closure_date) ,  ]
## merge this with the weekly data. now we have an ob for each week.
yelp_weekly<-merge(yelp_weekly,helper[,-c("num_closure_date", "num_created_date")], by=c("num_week", "id_encid"), all=TRUE )

yelp<-merge(yelp_biz,yelp_weekly, all=TRUE,by="id_encid" )
stopifnot(all(!is.na(yelp$created_date))) ## all weekly data should merge.
table(unique(yelp[,c("dollar_signs", "id_encid")])$dollar_signs)
table(unique(yelp[,c("closure_date", "id_encid")])$closure_date)
rm(helper, yelp_biz, yelp_weekly)

### fill in 0 for weekly rating count when missing.
yelp[is.na(weekly_count_rating),weekly_count_rating:=0]
### fill down sum cumulative, count cumulative,rating_AT_week_cumulative
stopifnot(nrow(yelp)==uniqueN(yelp[, c("id_encid", "num_week")])) ## unique by week id.
setkey(yelp, id_encid,num_week )
yelp[,sum_cumulative:=nafill(sum_cumulative,type="locf") ,by=id_encid]
yelp[,count_cumulative:=nafill(count_cumulative,type="locf") ,by=id_encid]
yelp[,rating_AT_week_cumulative:=nafill(rating_AT_week_cumulative,type="locf") ,by=id_encid]
yelp[,flag:=str_detect(str_to_lower(name), "hair|salon|barber|cut")]


### compute delta as city specific. use time period 2017 through 2021.
for (m in c(1,3,6)){
  delta<-yelp[flag==1 & week>=date(paste0('2016-',m,'-01')) & week<=date('2020-01-01')]
  delta[,weekly_mean:=sum(weekly_sum_rating, na.rm=TRUE)/sum(weekly_count_rating, na.rm=TRUE) , by=c("id_encid", "week")]
  # normalize ratings within period.
  delta[,weekly_mean:=(weekly_mean-mean(weekly_mean,na.rm=TRUE)), by=c("week")]
  delta[, longterm_mean:=sum(weekly_mean*weekly_count_rating, na.rm=TRUE)/sum(weekly_count_rating, na.rm=TRUE),by=c("id_encid") ]
  delta[, above_below:= as.numeric(weekly_mean>longterm_mean )
        , by=c("id_encid", "week") ]
  print(paste("Year: ", y))
  print(mean(delta[city=="Los Angeles" & week>=date('2019-06-01')]$above_below,na.rm=TRUE))
  print(mean(delta[city=="New York"]$above_below,na.rm=TRUE))
}
## convergence requires a lot of data.

## data ready.


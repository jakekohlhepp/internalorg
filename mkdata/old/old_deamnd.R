## use safegraph to compute total demand
## old demand method where we tried to get demand at each salon.
library(tidycensus)
library('SafeGraphR')
library('lubridate')
library('data.table')
library('stringr')


### Step 0: Retrieve aggregate statistics about safegraph.
filelist <- list.files(recursive=TRUE,path = "mkdata/raw/20211021_safegraph_patterns/", pattern = "*home_panel_summary.csv", full.names = TRUE)

aggstats<-NULL
for (f in filelist) {
  piece<-fread(f, colClasses = 'character', data.table = FALSE)
  aggstats<-rbind(aggstats, piece)
}
aggstats<-data.table(aggstats)
aggstats[, year:=as.integer(year)]
aggstats[, month:=as.integer(month)]
aggstats[, quarter_year:=as.integer(year)+ceiling(month/4)/10]
aggstats[, number_devices_residing:=as.integer(number_devices_residing)]
aggstats[, county:=substr(census_block_group,1,5)]
### step 0.5: for each cbg find total daytime
daytime_tot<-aggstats[,.(tot_daytime_devices=sum(as.integer(number_devices_primary_daytime))), 
                      by=c("county", "year", "month") ]


### Step 1: Retrieve census information.
# source: 
census<-fread('mkdata/raw/20220404_cbg_pop/cbg_2019.csv')
census[, year:=2019]
census<-rbind(census, fread('mkdata/raw/20220404_cbg_pop/cbg_2020.csv')[-1,], fill=TRUE)
census[, year:=ifelse(is.na(year),2020,year)]
census<-census[2:nrow(census),c("year", "B01001_001E","NAME", "GEO_ID")]
census[, census_block_group:=sub(".*US", "", GEO_ID)    ]
census[, county:=substr(census_block_group,1,5)    ]
census[, total_pop:=as.integer(B01001_001E)]
census[, county_pop:=sum(total_pop), by=c("county", "year")]
### Step 2: Compute weights for each CBG and year.
aggstats<-merge(aggstats, census,by=c("year", "census_block_group"), all.x=TRUE)
stopifnot(nrow(aggstats[region %in% c("ny", "ca") & is.na(B01001_001E) & year %in% c(2019),])==0)
# weight will be cbg pop divided by number of devices.
aggstats[, weight:=as.integer(B01001_001E)/as.integer(number_devices_residing)]
weight_file<-aggstats[region %in% c("ny", "ca") & year==2019, c("weight", "census_block_group", 
                                                                "number_devices_residing", "B01001_001E",
                                                                "quarter_year", "month")]
rm(aggstats)
### Step 3: Use weights to compute visits.
# for now use 2019 weights because cbgs changed.

data<-readRDS('mkdata/data/safegraph_patterns.rds')
# save out place details, with most recent name
place_details<-data[,.(max_date=max(date(date_range_end))),by=c("region", "city", "postal_code", "street_address", "location_name", "placekey")]
place_details[,last_date:=max(max_date), by=placekey]
place_details<-place_details[last_date==max_date]
stopifnot(uniqueN(place_details$placekey)==nrow(place_details))

data[,range_start:=date(date_range_start)]
data[,range_end:=date(date_range_end)]
data[, year:=year(range_start)]
data[, month:=month(range_start)]
data[, quarter_year:=year+quarter(range_start)/10]
data[, county:=substr(as.character(poi_cbg),1,5)]
data<-data[region %in% c("NY", "CA") & year %in% 2019]
home_cbg_data_df <- expand_cat_json(data,
                                    expand = 'visitor_home_cbgs',
                                    index = 'origin_cbg',
                                    by = c("placekey", "month", "year", "county"))
setnames(home_cbg_data_df, "visitor_home_cbgs", "cbg_visitor_count")
home_cbg_data_df[,cbg_visitor_count:=ifelse(cbg_visitor_count==4,2,cbg_visitor_count) ] # insert minimum from cbg which is 2 because rounded to 4.
tot_visits<-data[,.(tot_visits=sum(raw_visitor_counts)), by=c("placekey", "month", "year", "county")]
tot_visits[, tot_flag:=1]
# add row which is the missing visitors that have no cbg
home_cbg_data_df<-rbind(home_cbg_data_df,tot_visits , fill=TRUE)
home_cbg_data_df[, tot_visitor_withcbg:=sum(cbg_visitor_count,na.rm=TRUE), by=c("placekey", "month", "year")]
home_cbg_data_df[,cbg_visitor_count:=ifelse(!is.na(tot_flag),tot_visits-tot_visitor_withcbg,cbg_visitor_count) ]
setnames(home_cbg_data_df, "origin_cbg", "census_block_group")

weight_file[, year:=floor(quarter_year)]
qt_salon_demand<-merge(home_cbg_data_df,weight_file, by=c("census_block_group","year", "month"), all.x=TRUE)
stopifnot(nrow(qt_salon_demand[is.na(weight) & tot_flag!=1])==0)

qt_salon_demand[is.na(tot_flag), scaled_visitors:=cbg_visitor_count*weight]
# for the devices with no cbg, assume scaling is the same.
# sum up all visitors from known cbg, then divide by sum of all devices in these cbgs.
qt_salon_demand[, cbg_scaled_visitors_sum:=sum(scaled_visitors, na.rm=TRUE),
                by=c("year", "month", "county")]
qt_salon_demand[, cbg_tot_devices:=sum(number_devices_residing, na.rm=TRUE),
                by=c("year", "month", "county")]
qt_salon_demand[,overall_scale:=cbg_scaled_visitors_sum/cbg_tot_devices ]

qt_salon_demand[tot_flag==1,scaled_visitors:=overall_scale*cbg_visitor_count ]
# there are some remaining places in CBGs with no data. use same scaling procedure
qt_salon_demand[is.na(tot_flag) & is.na(B01001_001E),scaled_visitors:=overall_scale*cbg_visitor_count ]
## there are around 20 place-times where  weight is not defined and county weight is noted defined.
## finally sum up demand
qt_salon_demand<-qt_salon_demand[, .(adj_visitors=sum(scaled_visitors)),
                                 by=c("placekey","year", "month", "county", "overall_scale")]

## use county pop as baseline.



### reattach details
qt_salon_demand<-merge(qt_salon_demand,place_details, by="placekey", all.x=TRUE)



saveRDS(qt_salon_demand,"mkdata/data/demand.rds")




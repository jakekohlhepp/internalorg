## use safegraph get county demand for hair cuts and zip code demand for hair cuts.
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
aggstats[, quarter_year:=as.integer(year)+ceiling(month/3)/10]
aggstats[, number_devices:=as.integer(number_devices_residing)]
aggstats[, county:=substr(census_block_group,1,5)]
### step 0.5: for each cbg find total daytime
daytime_tot<-aggstats[,.(tot_daytime_devices=sum(as.integer(number_devices_primary_daytime)),
                         tot_nighttime_devices=sum(as.integer(number_devices_residing))), 
                      by=c("county", "year", "month") ]


### Step 1: Retrieve census information.
# source: 
census<-fread('mkdata/raw/20220404_cbg_pop/cbg_2019.csv')
census[, year:=2019] # mark 2019 as 2021 for now.
#census<-rbind(census, fread('mkdata/raw/20220404_cbg_pop/cbg_2020.csv')[-1,], fill=TRUE)
#census[, year:=ifelse(is.na(year),2020,year)]
census<-census[2:nrow(census),c("year", "B01001_001E","NAME", "GEO_ID")]
census[, census_block_group:=sub(".*US", "", GEO_ID)    ]
census[, county:=substr(census_block_group,1,5)    ]
census[, total_pop:=as.integer(B01001_001E)]
census[, county_pop:=sum(total_pop), by=c("county", "year")]
### Step 2: Compute weights for each CBG and year. for now use 2019 for both 2019 and 2020 first quarter.
aggstats<-merge(aggstats, census,by=c("census_block_group"), all.x=TRUE)
stopifnot(nrow(aggstats[region %in% c("ny", "ca") & is.na(B01001_001E),])==0)
weight_file<-aggstats[region %in% c("ny", "ca") & quarter_year %between% c(2019.1, 2020.1), c("census_block_group", "county_pop",
                                                                "number_devices", "B01001_001E",
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
data<-data[region %in% c("NY", "CA") & quarter_year %between% c(2019.1, 2020.1)]
home_cbg_data_df <- expand_cat_json(data,
                                   expand = 'visitor_home_cbgs',
                                   index = 'origin_cbg',
                                    by = c("placekey", "month", "year", "county"))
setnames(home_cbg_data_df, "visitor_home_cbgs", "cbg_visitor_count")


## roughly 1.48 million devices stay in county for haircut, 377k leave.
## for now, we assume that the people who go outside their county are taking the outside option.
## this amounts to dropping them in the program.
home_cbg_data_df[, origin_county:=substr(as.character(origin_cbg),1,5)]
home_cbg_data_df<-home_cbg_data_df[origin_county==county, ]
#home_cbg_data_df[,cbg_visitor_count:=ifelse(cbg_visitor_count==4,2,cbg_visitor_count) ] # insert minimum from cbg which is 2 because rounded to 4.
setnames(home_cbg_data_df, "origin_cbg", "census_block_group")
# for now we do not use devices where home CBG could not be determined.
weight_file[, year:=floor(quarter_year)]
qt_salon_demand<-merge(home_cbg_data_df,weight_file, by=c("census_block_group","year", "month"), all.x=TRUE)
stopifnot(nrow(qt_salon_demand[is.na(B01001_001E)])==0)
qt_salon_demand[, cbg_pop:=as.integer(B01001_001E)]
qt_salon_demand<-merge(qt_salon_demand,place_details, by="placekey", all.x=TRUE)
qt_salon_demand[,quarter_year:=year+ceiling(month/3)/10]
### get demand by zip code-quarter.
demand_shares<-qt_salon_demand[, .(zip_demand=sum(cbg_visitor_count*cbg_pop/number_devices)), 
                               by=c("postal_code","quarter_year", "county", "county_pop")]
demand_shares[,county:=as.integer(county)]

### Step 4: Get list of all zips in counties.
library('noncensus')
data(zip_codes)
zips<-data.table(zip_codes)
setnames(zips, "fips", "county")
zips<-zips[state %in% c("NY", "CA"),c("zip", "county")]
zips[,postal_code:=as.integer(zip)]
zips[,holder:=1,]
zips<-zips[data.table(quarter_year=c(2019.1, 2019.2, 2019.3, 2019.4, 2020.1),holder=1 ),on="holder", allow.cartesian=TRUE]
full_demand<-merge(zips, demand_shares, by=c("postal_code", "quarter_year", "county"), all.x=TRUE)
full_demand[is.na(zip_demand),zip_demand:=0 ]
full_demand[, county_demand:=sum(zip_demand), by=c("county", "quarter_year" )]
full_demand[, zip_count:=.N, by=c("county", "quarter_year" )]
# there will be some zipcodes with no demand (because no devices observed going here.
# to deal with this we use smoothing, invoke the rule of succession and set smoothing to be alpha =1
# this means we add 4 to all demand, then add number of zips to denominator.
# 4 because this is the minimum increment in safegraph
full_demand[, zip_frac_smooth:= (zip_demand+4)/(county_demand+zip_count)]

### Step 5: compute fraction who do not get a haircut using CEX.
cex<-readRDS('../mkdata/data/cex_outside.rds')
keepers<-data.table(PSU=c('S12A', 'S49A','S49B'), county=c(36061,06037 ,06075))
cex<-merge(cex, keepers, by='PSU')
final_demand<-merge(cex,full_demand, by=c("county", "quarter_year") , all.x=TRUE)
final_demand[, outside_share:=nohc_count /count_sample ]
# then zip fraction will be fraction among customers multiplied by fraction of people who get a haircut.
# implicitly we are assuming base is county population.
final_demand[, zip_share:=(1-outside_share)*zip_frac_smooth]
saveRDS(final_demand,"mkdata/data/demand.rds")




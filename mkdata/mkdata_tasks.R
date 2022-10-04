## compile data into one data base.
## 20210211 update: now we have all of 2020 in one file.
## 20210809  update: we now have a file for 01-0-01-2020 through 07-31-2021
## we now supplement our original file with the 2021 file and do not use the october extract or the 2020 file.
# we now keep 0 price transactions.
library('data.table')
library('stringr')
library('lubridate')

#### process original pull
setwd('C:/Users/jakek/blvd_dont_backup/20200909_raw/Marketing Intern Data/')
top <- fread("x00.csv")
files<-list.files(pattern = "*.csv")
compiled_txn <- rbindlist(lapply(files[-1], fread))
names(compiled_txn)<-names(top)
full_transactions<-rbind(compiled_txn, top)
rm(compiled_txn)
rm(top)
## process some of the variables.
# none of these are missing
for (v in names(full_transactions)) {
  if (all(!is.na(full_transactions[,get(v)]))) {
    print(paste(v, "is ok and is a", class(full_transactions[,get(v)])))
    
  } else{
    print(paste(v, "is MISSING STUFF and is a",class(full_transactions[,get(v)])))
    
  }
  
}

# discount amount has an issue.
length(full_transactions$discount_amount=="(\"\"\"\",0,0)")
full_transactions[,discount_amount:=NULL]

#### process 2021 pull.
setwd('C:/Users/jakek/blvd_dont_backup/20210809_alldata_refresh_withzip/')
all2020 <- fread("appointment_export.csv")
names(all2020)<-str_to_lower(names(all2020))
for (v in names(all2020)) {
  if (all(!is.na(all2020[,get(v)]))) {
    print(paste(v, "is ok and is a", class(all2020[,get(v)])))
    
  } else{
    print(paste(v, "is MISSING STUFF and is a",class(all2020[,get(v)])))
    
  }
  
}

# all2020 missing location_state and total_app_time
summary(all2020$price)

#### combine the data
stopifnot(nrow(full_transactions)-uniqueN(full_transactions)==0) #no exact dups in original pull

## take location information from the new pull.
full_transactions[,location_city:=NULL][,location_state:=NULL]

# duplicates due to overlap in the pulls need to be dropped.
# also we remove duplicates in terms of the main variables.
cols<-colnames(all2020)
cols<-cols[cols!="discount_amount"]
### remove the location vars.
cols<-cols[!(cols %in% c("location_city", "location_state", "location_zip","business_id", 
                   "industry", "first_location_app_datetime", "first_business_app_datetime"))]

uniq_2020<-unique(all2020[,cols, with=FALSE])
uniq_main<-unique(full_transactions[,c(cols,"total_app_time"), with=FALSE])

stopifnot(nrow(uniq_main)==uniqueN(uniq_main))
uniq_main[,main_src:=1]
uniq_combined<-rbind(uniq_2020, uniq_main, fill=TRUE)
setorderv(uniq_combined, c(cols, "main_src"))
# fix date format - all should be 26 characters
stopifnot(all(str_length(uniq_combined$app_datetime) %in% c(26,29)))
uniq_combined[,app_datetime:=str_sub(app_datetime, 1, 26)]
uniq_combined[,dup:=.N>1, by=cols]
# prioritize the first data pull when dup.
uniq_combined<-uniq_combined[dup==0 | (dup==1 & main_src==1), ]
stopifnot(nrow(uniq_combined)==uniqueN(uniq_combined[,-c( "main_src")])) # now unique
rm(full_transactions, uniq_2020, uniq_main)
## check date coverage.

# convert date times.
uniq_combined[,num_datetime:= parse_date_time(app_datetime, orders="ymd HMS")]

## acquire location information from all2020.
biz_locs_states<-unique(all2020[,c("location_id","location_city", "location_state", "location_zip","business_id", 
                            "industry", "first_location_app_datetime", "first_business_app_datetime")])
# biz_locs_states[, mults:=.N, by=location_id]
# one location has mults. only due to different first_bus_app. use earlier time.
# biz_locs_states[mults>1,]
biz_locs_states[,first_business_app_datetime:=min(first_business_app_datetime), by=location_id]
biz_locs_states<-unique(biz_locs_states)
stopifnot(uniqueN(biz_locs_states$location_id)==nrow(biz_locs_states))
# fix two prominent typos
biz_locs_states[,location_city:=str_trim(location_city)]
biz_locs_states[location_city %in% c("Los Angelas"),location_city:="Los Angeles"]
biz_locs_states[location_city %in% c("nyc"),location_city:="New York"]
biz_locs_states[location_city %in%c("West Los Angeles" ,"East Los Angeles", "Hollywood", "West Hollywood"), location_city:="Hollywood"]
uniq_combined<-merge(uniq_combined,biz_locs_states,by="location_id", all.x=TRUE)

### additional limitations
uniq_combined[,date:=date(num_datetime)]
#limit to final transactions with non-zero price
uniq_combined<-uniq_combined[app_stage=="final",]

# there are few obs with location city of denver but no state. fill in with colorado
uniq_combined[location_city=="Denver",location_state:="CO" ]
# there are few obs with location city of stockton but no state. fill in with ca
uniq_combined[location_city=="Stockton",location_state:="CA" ]
# austin but not state. fill in with tx
uniq_combined[location_city=="Austin",location_state:="TX" ]
# 5 remain. drop these. they appear to be video chat services?
#stopifnot(nrow(uniq_combined[is.na(location_city)|is.na(location_state) | location_state=="" | location_city=="",])==57)
#uniq_combined<-uniq_combined[!(location_id %in% c("e46c623f-c283-487a-9185-e96bf28052bb", "2da0db30-647b-4693-88f9-0ad2ae048bf9", "e78f4ab5-f9b6-4b22-a650-ab543cd902dd")),]
#stopifnot(nrow(uniq_combined[is.na(location_city)|is.na(location_state) | location_state=="" | location_city=="",])==0)


# in one case app_id has two dates. this appears to be an error. correct it by changing app_id
uniq_combined[,first_date:=min(date), by="app_id"]
uniq_combined[app_id=="7f388e8a-4c7c-4b4c-8f3b-b3dbda35e5f7" &date!=first_date,app_id:="7f388e8a-4c7c-4b4c-8f3b-b3dbda35e5f7-change" ]
uniq_combined[app_id=="7f388e8a-4c7c-4b4c-8f3b-b3dbda35e5f7",first_date:=min(date)]
stopifnot(all(uniq_combined$first_date==uniq_combined$date))

## per Sean 4/23/2021: this business_id id is fake for internal purposes. it should not be present.
stopifnot(nrow(uniq_combined[business_id=="0b8bc512-705d-4be9-90e4-2fad62a4e4f4",])==0)

setwd('C:/Users/jakek/blvd_dont_backup/data')
saveRDS(uniq_combined,file="compiled_trxns.rds")

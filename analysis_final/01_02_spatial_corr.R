# plot spatial correlation of productivity, s-index and repeat customers.
# regressions first
library('fixest')
library('data.table')
library('sf')
library('ggplot2')
library('stringr')

## use data from 
working_data<-data.table(readRDS("analysis_final/data/01_01_stylied_facts_data.rds"))


summary(feols(data=working_data, rev_labor~s_index|location_zip, cluster=~location_id))
state_data_count<-working_data[,.(salons=uniqueN(location_id),count=.N
),by="location_state"]
quarter_data_count<-working_data[,.(salons=uniqueN(location_id),count=.N,s_index=mean(s_index)
),by="quarter_year"]


get_state<-Vectorize(function(y){
  if (y %in% state.abb){
    return(str_to_lower(state.name[which(y == state.abb)]))
  }else if (y =="DC"){
    return('district of columbia')
  }else{
    return(NA)
  }
})

state_data_count[, region:=get_state(location_state)]
expand_it<-data.table(region=get_state(state.abb))
state_data_count<-merge(expand_it, state_data_count, all.x=TRUE)
state_data_count[is.na(count), count:=0]
state_data_count[is.na(salons), salons:=0]
working_data<-working_data[,.(return_cust=mean(pastrepeat_rate),rev_labor=mean(rev_labor),s_index=mean(s_index),salons=uniqueN(location_id),count=.N
                              ),by="location_zip"]
setnames(working_data, "location_zip", "ZCTA5CE10")
working_data[, coverage:=1]
### map zip codes to county
countypop<-fread('mkdata/raw/20220727_countypop/geocorr2022_2220806816.csv')[-1]
countypop[,CSPOP:=pop20]
stopifnot(uniqueN(countypop$county)==nrow(countypop))
data<-fread('mkdata/raw/20220727_countypop/geocorr2022_2220801561.csv')[-1]
data[, count:=uniqueN(county),by=zcta]
data<-data[afact>0.50 | count==1] # mapping only if more than 50 percent of zip is within county.
stopifnot(uniqueN(data$zcta)==nrow(data))
data<-merge(data, countypop[,c("county")], by="county")
data[, ZCTA5CE10:=as.character(zcta)]
data[, county:=as.character(as.integer(county))]


# pull shape files from 2018 version of https://www.census.gov/geographies/mapping-files/time-series/geo/carto-boundary-file.html


my_sf <- read_sf("mkdata/raw/20240415_census_zcta_shapefiles/cb_2018_us_zcta510_500k.shp")
my_sf<-merge(my_sf, data[,c("ZCTA5CE10","county")], by='ZCTA5CE10', all.x=TRUE)


my_sf<-merge(my_sf, working_data, by='ZCTA5CE10', all.x=TRUE)
my_colors <- c("white",colorRampPalette(c('white', "black"))(uniqueN(my_sf[my_sf$county=='36061',]$rev_labor)-1))
cats<-frank(my_sf[my_sf$county=='36061'& my_sf$coverage==1,]$rev_labor,na.last=FALSE, ties.method="dense")


my_colors <- my_colors[cats]
png("analysis_final/out/figures/01_01_spatial_cor_36061_rev.png",width=700, height=700)
plot(st_geometry(my_sf[my_sf$county=='36061'& my_sf$coverage==1 ,]), col = my_colors, bg = "white", lwd = 0.25
)
dev.off()

my_colors <- c("white",colorRampPalette(c('white', "black"))(uniqueN(my_sf[my_sf$county=='36061',]$s_index)-1))
cats<-frank(my_sf[my_sf$county=='36061' & my_sf$coverage==1,]$s_index,na.last=FALSE, ties.method="dense")
my_colors <- my_colors[cats]

png("analysis_final/out/figures/01_01_spatial_cor_36061_sindex.png",width=700, height=700 )
plot(st_geometry(my_sf[my_sf$county=='36061'& my_sf$coverage==1 ,]), col = my_colors, bg = "white", lwd = 0.25
)
dev.off()

my_colors <- c("white",colorRampPalette(c('white', "black"))(uniqueN(my_sf[my_sf$county=='36061',]$return_cust)-1))
cats<-frank(my_sf[my_sf$county=='36061' & my_sf$coverage==1,]$return_cust,na.last=FALSE, ties.method="dense")
my_colors <- my_colors[cats]

png("analysis_final/out/figures/01_01_spatial_cor_36061_return_rate.png",width=700, height=700 )
plot(st_geometry(my_sf[my_sf$county=='36061'& my_sf$coverage==1 ,]), col = my_colors, bg = "white", lwd = 0.25
)
dev.off()


my_colors <- c("white",colorRampPalette(c('white', "black"))(uniqueN(my_sf[my_sf$county=='6037',]$rev_labor)-1))
cats<-frank(my_sf[my_sf$county=='6037'& my_sf$coverage==1,]$rev_labor,na.last=FALSE, ties.method="dense")
my_colors <- my_colors[cats]

png("analysis_final/out/figures/01_01_spatial_cor_6037_rev.png",width=700, height=700)
plot(st_geometry(my_sf[my_sf$county=='6037'& my_sf$coverage==1 ,]), col = my_colors, bg = "white", lwd = 0.25
)
dev.off()



my_colors <- c("white",colorRampPalette(c('white', "black"))(uniqueN(my_sf[my_sf$county=='6037',]$s_index)-1))
cats<-frank(my_sf[my_sf$county=='6037' & my_sf$coverage==1,]$s_index,na.last=FALSE, ties.method="dense")
my_colors <- my_colors[cats]

png("analysis_final/out/figures/01_01_spatial_cor_6037_sindex.png", width=700, height=700)
plot(st_geometry(my_sf[my_sf$county=='6037'& my_sf$coverage==1 ,]), col = my_colors, bg = "white", lwd = 0.25
)
dev.off()

my_colors <- c("white",colorRampPalette(c('white', "black"))(uniqueN(my_sf[my_sf$county=='6037',]$return_cust)-1))
cats<-frank(my_sf[my_sf$county=='6037'& my_sf$coverage==1,]$return_cust,na.last=FALSE, ties.method="dense")
my_colors <- my_colors[cats]

png("analysis_final/out/figures/01_01_spatial_cor_6037_return_rate.png",width=700, height=700)
plot(st_geometry(my_sf[my_sf$county=='6037'& my_sf$coverage==1 ,]), col = my_colors, bg = "white", lwd = 0.25
)
dev.off()





my_colors <- c("white",colorRampPalette(c('white', "black"))(uniqueN(my_sf[my_sf$county=='17031',]$rev_labor)-1))
cats<-frank(my_sf[my_sf$county=='17031'& my_sf$coverage==1,]$rev_labor,na.last=FALSE, ties.method="dense")
my_colors <- my_colors[cats]

png("analysis_final/out/figures/01_01_spatial_cor_17031_rev.png",width=700, height=700)
plot(st_geometry(my_sf[my_sf$county=='17031'& my_sf$coverage==1 ,]), col = my_colors, bg = "white", lwd = 0.25
)
dev.off()



my_colors <- c("white",colorRampPalette(c('white', "black"))(uniqueN(my_sf[my_sf$county=='17031',]$s_index)-1))
cats<-frank(my_sf[my_sf$county=='17031' & my_sf$coverage==1,]$s_index,na.last=FALSE, ties.method="dense")
my_colors <- my_colors[cats]

png("analysis_final/out/figures/01_01_spatial_cor_17031_sindex.png", width=700, height=700)
plot(st_geometry(my_sf[my_sf$county=='17031'& my_sf$coverage==1 ,]), col = my_colors, bg = "white", lwd = 0.25
)
dev.off()

my_colors <- c("white",colorRampPalette(c('white', "black"))(uniqueN(my_sf[my_sf$county=='17031',]$return_cust)-1))
cats<-frank(my_sf[my_sf$county=='17031'& my_sf$coverage==1,]$return_cust,na.last=FALSE, ties.method="dense")
my_colors <- my_colors[cats]

png("analysis_final/out/figures/01_01_spatial_cor_17031_return_rate.png",width=700, height=700)
plot(st_geometry(my_sf[my_sf$county=='17031'& my_sf$coverage==1 ,]), col = my_colors, bg = "white", lwd = 0.25
)
dev.off()

## coverage of data.

states_map <- map_data("state")
library('ggthemes')
ggplot(state_data_count, aes(map_id = region)) + 
  geom_map(aes(fill = salons), map = states_map,color="black")+
  scale_fill_gradientn(colours=c("white","black")) + 
  expand_limits(x = states_map$long, y = states_map$lat)+
  theme_map()+theme(legend.text = element_text(size = 18), legend.title=element_text(size = 18))
ggsave("analysis_final/out/figures/01_01_coverage.png", width=12, heigh=6, units="in")



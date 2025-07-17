# mk ppi data

library('data.table')
library('lubridate')

ppi<-fread('mkdata/raw/20231227_ppi_cost/file.csv')
setnames(ppi, "Value", "ppi_inputs")
ppi[,month:=as.numeric(gsub("M","",Period))][,quarter:=quarter(month)][,quarter_year:=as.numeric(Year)+quarter/10]
ppi[,helper:=frank(month), by=c("quarter_year")]
# use starting month price
ppi<-ppi[helper==1,c("ppi_inputs", "quarter_year")]
stopifnot(nrow(ppi)==uniqueN(ppi))
saveRDS(ppi, "mkdata/data/ppi.rds")
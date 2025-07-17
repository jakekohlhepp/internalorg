### get qcew wage information for two counties.
library('data.table')
source('mkdata/raw/20220427_qcew_code/qcew_rscript_example.R')

puzzle<-c()

for (y in 2014:2021){
  for (q in 1:4){
    piece<-qcewGetIndustryData(y, q, 812112)
    puzzle<-rbind(puzzle,piece)
  }
}
puzzle<-data.table(puzzle)
puzzle[,quarter_year:=year+qtr/10]
puzzle[,county:=as.numeric(area_fips)]

saveRDS(puzzle, "mkdata/data/qcew_county.rds")




## construct additional weighting information.
setwd("C:/Users/jakek/Google Drive/Working Documents/econ_phd/jmp/mkdata")

## oews cosmetologist wage distribution
oews<-readRDS("data/oews_msa.rds")
oews<-oews[OCC_CODE=='39-5012']


## qcew
source('raw/20220427_qcew_code/qcew_rscript_example.R')

puzzle<-data.table()
for (county in c("36061", "06037")){
for (y in c(2018, 2019, 2020, 2021)){
for (q in c(1,2,3,4)){
if (q==4 & y==2021) next
piece<-data.table(qcewGetAreaData(y,q, county))
piece<-piece[industry_code=='812112']
puzzle<-rbind(puzzle, piece)
}
}
}
qcew<-puzzle
qcew[, quarter_year:=year+qtr/10]
qcew[, avg_size:=(month2_emplvl+month2_emplvl+month2_emplvl)/3/qtrly_estabs]
qcew[, avg_wagebill:=total_qtrly_wages/qtrly_estabs]

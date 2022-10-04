## process bls oews data
## goal: annual wage distribution for cosmetologists
library("readxl")
## append them all
setwd("C:/Users/jakek/Google Drive/Working Documents/econ_phd/jmp/mkdata/raw")
files<-list.files(pattern = "*.xlsx")
group1 <- rbindlist(lapply(files[-10],function(x) {cbind(read_excel(x),YEAR=as.numeric(unlist(regmatches(x, regexec("[0-9]{4}", x)))))}), fill=TRUE)
twenty19 <- rbindlist(lapply(files[10],function(x) {cbind(read_excel(x),YEAR=as.numeric(unlist(regmatches(x, regexec("[0-9]{4}", x)))))}), fill=TRUE)
names(twenty19)<-str_to_upper(names(twenty19))
together<-rbind(group1,twenty19,fill=TRUE )
# keep only year, msa, occs hairstylist and all, then percentiles of wage
pull<-data.table(together)
changeCols <- colnames(pull)[which(str_detect(colnames(pull), "PCT|MEDIAN|MEAN"))]
pull[,CLEAN_AREA_NAME:=ifelse(is.na(AREA_TITLE),AREA_NAME,AREA_TITLE)]
pull[,(changeCols):= lapply(.SD, as.numeric), .SDcols = changeCols]
pull[,A_IQR:=A_PCT75-A_PCT25]
pull[,A_IDR:=A_PCT90-A_PCT10]

saveRDS(pull,file="../data/oews_msa.rds")

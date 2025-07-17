# purpose: create crosswalk from msa to county
# for attaching cex to data.

library('data.table')
library('stringr')

cex_codebook<-fread('mkdata/raw/20231023_county_msa_crosswalk/cex_cleaned_codebook.csv')
colnames(cex_codebook)<-str_replace_all(str_to_lower(colnames(cex_codebook)), fixed(" "), "")
cex_codebook<-cex_codebook[variable=="PSU" & str_detect(codevalue, "S")>0 & survey=="INTERVIEW",c("codevalue", "codedescription")]
stopifnot(uniqueN(cex_codebook)==nrow(cex_codebook))


msa_xwalk<-fread('mkdata/raw/20231023_county_msa_crosswalk/qcew-county-msa-csa-crosswalk.csv')
colnames(msa_xwalk)<-str_replace_all(str_to_lower(colnames(msa_xwalk)), fixed(" "), "")
msa_xwalk[,codedescription:=str_trim(str_replace_all(msatitle," MSA","")) ]
msa_xwalk<-msa_xwalk[,c("countycode", "codedescription")]
stopifnot(uniqueN(msa_xwalk)==nrow(msa_xwalk))

cex_codebook<-merge(cex_codebook,msa_xwalk, by="codedescription",all.x=TRUE)
cex_codebook<-cex_codebook[,county:=countycode][,PSU:=codevalue]
cex_codebook<-cex_codebook[,c("county", "PSU")]

saveRDS(cex_codebook,file="mkdata/data/county_msa_xwalk.rds")
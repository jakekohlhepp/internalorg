## create summary statistics for estimation sample.
library('data.table')
library('stargazer')

full_unsmoothed<-readRDS("analysis_final/data/01_00_staff_task_full.rds")

## exclusions
# there is one salon in KY which is an anomaly
full_unsmoothed<-full_unsmoothed[location_id!='fb686b3a-a166-469b-88ea-3467a68e2f53',]

full_unsmoothed<-unique(full_unsmoothed[,.SD, .SDcols=c(colnames(full_unsmoothed)[grep("^task_mix",colnames(full_unsmoothed))], "s_index", "location_id","county", "cust_count", "quarter_year", "emps","cust_count", "revenue")])

## restrict to the three counties and quarters
quarter_list<-c(2018.1, 2018.2, 2018.3, 2018.4, 2019.1, 2019.2, 2019.3, 2019.4, 2020.1, 2020.4, 2021.1, 2021.2)
full_unsmoothed<-full_unsmoothed[county %in% c("17031", "36061", "06037") & quarter_year %in% quarter_list,]

### firm table
firm_stats<-full_unsmoothed[,c("revenue","emps","cust_count","s_index","task_mix_1","task_mix_2", "task_mix_3", "task_mix_4", "task_mix_5")]

names(firm_stats)<-c("Revenue","Employees","Customers","S-Index",
                     "Share Haircut/Shave", "Share Color/Highlight/Wash", "Share Blowdry/Style/Treatment/Extensions",
                     "Share Admininstrative","Share Nail/Spa/Eye/Misc."
)
stargazer(firm_stats, header=FALSE, type='text')
stargazer(firm_stats, header=FALSE,digits=2, out='analysis_final/out/tables/01_03_summary_stats_structural.tex',single.row = TRUE)

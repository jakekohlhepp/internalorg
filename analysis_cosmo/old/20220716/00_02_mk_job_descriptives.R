## create job space statistics and plots.

library('checkpoint')
#checkpoint('2020-07-16') activate to reproduce
library('data.table')
library('lubridate')
library('stringr')
library('zoo')
library('binsreg')
library('ggplot2')
library('lessR')
library('stargazer')
theme_set(theme_bw(base_size=22))


jobs<-readRDS("data/00_00_job_quarter.rds")
stopifnot(uniqueN(jobs[,c("staff_num", "location_id", "quarter_year")])==nrow(jobs))

## make scatter plot.
ggplot(data=jobs, aes(x=jobvect_1,y=jobvect_2, color=as.numeric(jobvect_4) ))+
  geom_point(size=1)+  ylab("% Time Color") + xlab("% Time Haircut")+ labs(color='% Time Blowdry')
ggsave("out/figures/00_02_job_scatter.png", width=12, heigh=6, units="in")

## Make table
toshow<-jobs[,c("jobvect_1","jobvect_2","jobvect_3","jobvect_4", "jobvect_5", "jobvect_6" )]
names(toshow)<-c("Share Haircut/Shave", "Share Color/Highlight/Wash", "Share Extensions", "Share Blowdry/Style/Treatment",
                 "Share Administrative", "Share Nail/Spa/Eye/Misc.")
stargazer(toshow, header=FALSE, out='out/tables/00_02_job_descriptives.tex')


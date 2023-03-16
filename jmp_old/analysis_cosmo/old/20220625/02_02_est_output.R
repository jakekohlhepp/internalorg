
## make estimate graphs
library('data.table')
library('gmm')
library('ggplot2')


############### import results
load('data/02_01_est_res_ny.RData')
###### create objects
tau<-1.045
core_count<-1
source('main_gmm_objective.R')
ny_obects<-ret_objects(coef(maxrefine), estim_matrix)
rho<-coef(maxrefine)[1]
###############

### parameter table
library(knitr)
library(kableExtra)
sigfig <- function(vec, n=3){ 
  ### function to round values to N significant digits
  # input:   vec       vector of numeric
  #          n         integer is the required sigfig  
  # output:  outvec    vector of numeric rounded to N sigfig
  
  formatC(signif(vec,digits=n), digits=n,format="fg", flag="#") 
  
}      # end of function   sigfig
df <- data.frame(Paramater=c("Price Sensitivity", "Haircut Skill",
                             "Color/Highlight/Wash Skill", "Extensions Skill",
                             "Blowdry/Style/Treatment Skill", "Administrative Skill",
                             "Nail/Spa/Eye/Misc. Skill"),Symbol=c("$\\rho$","$S_1$", "$S_2$", "$S_3$", "$S_4$", "$S_5$", "$S_6$"), Estimate=sigfig (as.numeric(coef(maxrefine))[c(1,30:35)],4))
kable(df, "latex", align="c", booktabs=TRUE, escape = F, caption = 'Price Sensitivity and Worker Skill Levels') %>%
  cat(., file = "out/02_02_parms_ny.tex")



#  footnote(general="\\\\small{This table displays the consumer price sensitivity and worker skill level estimates for Manhattan. These paramaters are assumed constant over the analysis period.}", 
#escape=FALSE, threeparttable = TRUE)%>%

### wages table
df <- data.frame(Type=rep(c("Color/Highlight/Wash Specialist", "Extensions Specialist",
                             "Blowdry/Style/Treatment Specialist", "Administrative Specialist",
                             "Nail/Spa/Eye/Misc. Specialist"),4),Quarter=c(rep("2019-Q2",5),rep("2019-Q3",5),rep("2019-Q4",5),rep("2020-Q1",5)), 
                 Estimate=sigfig (as.numeric(coef(maxrefine))[c(4:23)],4))
kable(df, "latex", align="c", booktabs=TRUE, escape = F, caption = 'Wage Estimates') %>%
  cat(., file = "out/02_02_wages_ny.tex")

### plot gamma

ggplot(data=data.frame(gamma=ny_obects$gamma[estim_matrix[,1]>0]),aes(x=ny_obects$gamma[estim_matrix[,1]>0])) +
  geom_histogram(color="black", fill="lightblue", size=1)+ ylab("Establishment-Quarter Count") + xlab("Internal Organization Cost Parameter")+ theme(legend.position = "none")
ggsave("out/figures/02_02_gamma_nycounty.png", width=12, heigh=6, units="in")



### moments
moments<-gmm_obj(coef(maxrefine), estim_matrix)



###



profit<-1/rho*estim_sample$CSPOP*estim_sample$salon_share_subdiv/(1-estim_sample$salon_share_subdiv)
qual_cost<-ny_obects$quals - rho*tau*(ny_obects$wagebills+ny_obects$gammas*estim_matrix[,1])

ggplot() +
  geom_point(aes(x=rank(ny_obects$gammas[estim_matrix[,1]>0])/length(ny_obects$gammas[estim_matrix[,1]>0])*100, y=qual_cost[estim_matrix[,1]>0] ),color="black")+ ylab("Quality-Adjusted Cost Percentile") + xlab("Internal Organization Cost Percentile")+ theme(legend.position = "none")
ggsave("out/figures/02_02_prank_rank_nycounty.png", width=12, heigh=6, units="in")



### validation.


#findbmax<-function(x){colMax(finda(x)[[1]]/rowSums(finda(x)[[1]]))}
#findbmin<-function(x){colMin(finda(x)[[1]]/rowSums(finda(x)[[1]]))}
#colMean <- function(data) apply(data,2, mean, na.rm = TRUE)
#bmax_model<-colMean(t(sapply(1:nrow(estim_sample),findbmax )))
#bmax_real<-colMean(estim_sample[,c("max_jobvect_1","max_jobvect_2","max_jobvect_3", "max_jobvect_4", "max_jobvect_5", "max_jobvect_6")])
#
#bmin_model<-colMean(t(sapply(1:nrow(estim_sample),findbmin )))
#bmin_real<-colMean(estim_sample[,c("min_jobvect_1","min_jobvect_2","min_jobvect_3", "min_jobvect_4", "min_jobvect_5", "min_jobvect_6")])


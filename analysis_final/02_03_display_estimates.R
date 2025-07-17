### Purpose: display all point estimates

library('data.table')
library('lubridate')
library('stringr')
library('knitr')
library('kableExtra')
library('gmm')
spec_log<-function(x)  ifelse(x==0 | is.nan(x),0,log(x))

key_task<-data.table(readRDS("analysis_final/data/01_00_keytask.rds"))
key_task[clust==5, rep_text_cluster :="Nail/Misc."]
key_task[clust==3, rep_text_cluster :="Blowdry/Style/Etc."]
all_results<-readRDS('analysis_final/data/02_00_parameters.rds')
point_estimates<-all_results$coefficients
names(point_estimates)<-all_results$parm_name
point_estimates_demand_flag<-all_results$demand
all_results[,county:=str_extract(parm_name, "(?i)(?<=factor\\(county\\))[0-9]*")]
all_results[,quarter_year:=str_extract(parm_name, "(?i)(?<=factor\\(quarter_year\\))\\d+\\.*\\d*")]


all_results[,other_part:=paste0("factor\\(county\\)",county,"\\:")]
all_results[!is.na(quarter_year),other_part:=paste0(other_part,"factor\\(quarter_year\\)",quarter_year)]
all_results[, other_part:=str_replace(parm_name, other_part,"")]  
all_results[str_detect(parm_name,"task_mix"), task :=as.numeric(str_extract(parm_name, "(?i)(?<=task_mix_)[0-9]"))]
all_results[str_detect(parm_name,"B_raw_[0-9]_[0-9]"), task :=as.numeric(str_extract(parm_name, "(?i)(?<=B_raw_)[0-9]"))]
all_results[str_detect(parm_name,"E_raw_"), worker_type :=as.numeric(str_extract(parm_name, "(?i)(?<=E_raw_)[0-9]"))]
all_results[str_detect(parm_name,"B_raw_[0-9]_[0-9]"), worker_type :=as.numeric(str_sub(parm_name, start=-1,))]
all_results<-merge(all_results, key_task[,c("task", "rep_text_cluster")], by="task", all.x=TRUE)
all_results[, county_name:=ifelse(county=="17031", "Cook",ifelse(county=="36061", "New York", "Los Angeles"))]


### create se


files <- list.files(path="analysis_final/data/",pattern = "03_[0-9]+_bootstrap.rds")

## if statement for before the bootstrapping is done.
if (length(files)>0){
  all_bootreps<-data.table()
  for (f in files) {
    all_bootreps<-rbind(all_bootreps,readRDS(paste0("analysis_final/data/",f)))
  }
  colnames(all_bootreps)[-1]<-names(point_estimates)
  all_se<-all_bootreps[, lapply(.SD, sd), .SDcols= colnames(all_bootreps)[-1]]
  all_se<-rbind(all_se)
  all_se[,id_var:=1]
  
  all_se<-melt(all_se,id=c("id_var"))
  all_se[,id_var:=NULL]
  all_se[, demand:=point_estimates_demand_flag]
  setnames(all_se, "value", "se")
  setnames(all_se, "variable", "parm_name")
  
  all_results<-merge(all_results, all_se, by=c("parm_name", "demand"))
  stopifnot(nrow(all_results)==nrow(all_se))
  all_results[,se:=str_replace(as.character(paste0("(",format(round(se, 3), nsmall = 3),")"))," 0.","."),]
} else{
  all_results[,se:=as.character(0.001),]
  
}
## blanks are county-quarter fixed effects in the cost and quality equations.

## format point estimates and se
# flip sign of rho for display
all_results[other_part=="cust_price", coefficients:= -coefficients]
all_results[,coefficients:=as.character(format(round(coefficients, 3), nsmall = 3)),]

## melt so se is lower
all_results<-melt(all_results,id.vars=c("parm_name","other_part", "county_name","rep_text_cluster","quarter_year","worker_type", "demand") , measure.vars =c("se", "coefficients") )
all_results[,variable:=as.character(variable)]
setkey(all_results, "county_name","quarter_year", "other_part","variable","rep_text_cluster")
setnames(all_results, "county_name", "County")
## nice names
all_results[other_part=="cust_price",nice_name:="Price Sensitivity"]
all_results[other_part=="org_cost",nice_name:="Reference Org. Cost"]
all_results[other_part=="avg_labor:",nice_name:="Wage Level"]
all_results[str_detect(other_part, "task_mix_[0-9]$"),nice_name:=paste("Material Cost", rep_text_cluster)]
all_results[other_part=="" & demand==TRUE,nice_name:="Demand Level"]
all_results[other_part=="" & demand==FALSE,nice_name:="Cost Level"]

## order by county, quarter year,task, coefficient than se

### table 1: org costs and price sensitivity for all markets
output<-all_results[other_part %in% c("cust_price", "org_cost"),c("County","nice_name","variable","value")]
output<-dcast(output, County+variable~nice_name)
output[which(!as.logical(1:nrow(output) %% 2)),County:="" ]
kable(output[,-"variable"], "latex", align="c", booktabs=TRUE,linesep = c(""), escape = F, caption = NA, label=NA) %>%
  cat(., file = "analysis_final/out/tables/02_03_org_price.tex")


### table 2: wages and skills (one table per market)


for (cnty in unique(all_results$County)){
  print(cnty)
  if (is.na(cnty)) next
  output<-all_results[County==cnty & !is.na(worker_type),c("worker_type","value", "rep_text_cluster","variable")]
  output[, rep_text_cluster:=ifelse(is.na(rep_text_cluster),"Wage",rep_text_cluster)]
  output<-dcast(output, worker_type+variable~rep_text_cluster, value.var="value")
  output[is.na(Wage) & variable=="coefficients", Wage:="-"]
  output[is.na(Wage) & variable=="se", Wage:="-"]
  setcolorder(output, c("worker_type", "variable","Wage"))
  setnames(output, "worker_type", "Worker Skill Set")
  print(output[,-"variable"])
  kable(output[,-"variable"], "latex", align="c", booktabs=TRUE,linesep = c(""), escape = F, caption = NA, label=NA) %>%
    cat(., file = paste0("analysis_final/out/tables/02_03_wages_skills_",gsub(" ","",cnty),".tex"))
  

}

### table 3: material costs, wage time fixed effect, demand time fixed effect
output<-all_results[nice_name %in% c("Demand Level", "Wage Level", "Cost Level") | str_detect(nice_name, "Material Cost"),c("nice_name","County","quarter_year","value","variable")]
output[is.na(County), County:="All"]
output[, quarter_year:=gsub("\\.", "Q", quarter_year)]
output<-dcast(output, County+nice_name+variable~quarter_year, value.var="value")
output[is.na(`2018Q1`), `2018Q1`:="-"]
setnames(output, "nice_name", "Parameter")
kable(output[,-"variable"], "latex", align="c", booktabs=TRUE,linesep = c(""), escape = F, caption = NA, label=NA) %>%
  cat(., file = "analysis_final/out/tables/02_03_time_effects.tex")




#### display targeted moments and the model output.
rm(list = setdiff(ls(all.names = TRUE), "point_estimates"))
spec_log<-function(x)  ifelse(x==0 | is.nan(x),0,log(x))




working_data<-data.table(readRDS('analysis_final/data/01_01_working.rds'))
source('analysis_final/preamble.R')

beta_2_subset<-point_estimates[(nrow(beta)+1):(nrow(beta)+12)]
names(beta_2_subset)<-names(point_estimates)[(nrow(beta)+1):(nrow(beta)+12)]

## coefficients for last step
final_coefs<-point_estimates[(nrow(beta)+13):length(point_estimates)]
names(final_coefs)<-names(point_estimates)[(nrow(beta)+13):length(point_estimates)]
 



## set innertolerance, and choose if we want to accelerate the fixedpoint
innertol<-1e-08
outertol<-1e-04
obj_tol<-1e-02
accel_on<-TRUE
pl_on<-FALSE

if (get_os()=="windows"){
  clust <- makeCluster(core_count)
  clusterExport(clust, ls())
  clusterEvalQ(clust, library("SQUAREM"))
}

##### initialize

moments_part1<-data.table(moment_name=colnames(z_mm_1), model_interact="Log Market Share", raw_moments=colMeans( matrix(estim_matrix$log_rel_mkt, nrow=nrow(z_mm_1),ncol=ncol(z_mm_1), byrow=FALSE)*z_mm_1),
                     model_moments=colMeans( matrix(mm_1%*%beta, nrow=nrow(z_mm_1),ncol=ncol(z_mm_1), byrow=FALSE)*z_mm_1))


moments_part2<-eval_moments(theta=beta_2_subset, x=estim_matrix)
moments_part2<-cbind(data.table(moments_part2), rownames(moments_part2))
colnames(moments_part2)<-c("model_moments", "raw_moments", "moment_name")
moments_part2[, model_interact:="Labor Demand"]


working_data[, wb_2:=beta_2_subset[paste0("factor(county)", county, ":avg_labor:E_raw_2")]*E_raw_2*avg_labor]
working_data[, wb_3:=beta_2_subset[paste0("factor(county)", county, ":avg_labor:E_raw_3")]*E_raw_3*avg_labor]
working_data[, wb_4:=beta_2_subset[paste0("factor(county)", county, ":avg_labor:E_raw_4")]*E_raw_4*avg_labor]
working_data[, wb_5:=beta_2_subset[paste0("factor(county)", county, ":avg_labor:E_raw_5")]*E_raw_5*avg_labor]
working_data[, gamma_invert:=get_gammas(beta_2_subset, estim_matrix)]
working_data[,p_adj:=cust_price-wb_2-wb_3-wb_4-wb_5-gamma_invert*s_index+mk_piece/beta[paste0("factor(county)",county, ":cust_price"),]]
xnam <- as.formula("~avg_labor:factor(county):factor(quarter_year)+factor(quarter_year):factor(county)+factor(quarter_year):(task_mix_2+task_mix_3+task_mix_4+task_mix_5)-1")
mod_mm_2<-model.matrix(xnam, data=working_data)



moments_part3<-data.table(model_interact="Price",moment_name=colnames(mod_mm_2) ,raw_moments=colMeans( matrix(working_data$p_adj, nrow=nrow(mod_mm_2),ncol=ncol(mod_mm_2), byrow=FALSE)*mod_mm_2),
                          model_moments=colMeans( matrix(mod_mm_2%*%final_coefs, nrow=nrow(mod_mm_2),ncol=ncol(mod_mm_2), byrow=FALSE)*mod_mm_2))

moment_mat<-rbind(moments_part1,moments_part2,moments_part3 )

moment_mat[,county:=str_extract(moment_name, "(?i)(?<=factor\\(county\\))[0-9]*")]
moment_mat[, county_name:=ifelse(county=="17031", "Chicago",ifelse(county=="36061", "Manhattan", "Los Angeles"))]
moment_mat[,quarter_year:=str_extract(moment_name, "(?i)(?<=factor\\(quarter_year\\))\\d+\\.*\\d*")]

moment_mat[, other_part:=""]
moment_mat[!is.na(county),other_part:=paste0("factor\\(county\\)",county,"\\:")]
moment_mat[!is.na(quarter_year),other_part:=paste0(other_part,"factor\\(quarter_year\\)",quarter_year)]
moment_mat[, other_part:=str_replace(moment_name, other_part,"")]  
moment_mat[is.na(other_part), other_part:=""]


# drop so not collinear
moment_mat<-moment_mat[!str_detect(other_part, "E_raw_1") ]

moment_mat[, other_part:=str_replace(other_part, "E_raw_[0-9]","E_raw")]
moment_mat[, other_part:=str_replace(other_part, "B_raw_[0-9]_[0-9]","B_raw")]
moment_mat[, other_part:=str_replace(other_part, "task_mix_[0-9]","task_mix")]

summary_moments<-moment_mat[, .(count=.N,mean_model=mean(model_moments),mean_data=mean(raw_moments),
                                r2=1-sum((raw_moments-model_moments)^2)/sum((raw_moments-mean(raw_moments))^2) ), by=c("model_interact","other_part" )]

summary_moments[model_interact=="Labor Demand", other_part:="County-Skill Set"]
summary_moments[model_interact=="Price" & other_part=="", other_part:="County-Quarter"]
summary_moments[model_interact=="Log Market Share" & other_part=="", other_part:="County-Quarter"]
summary_moments[model_interact=="Price" & other_part==":task_mix", other_part:="Quarter-Task Mix"]
summary_moments[model_interact=="Price" & other_part=="avg_labor:E_raw", other_part:="Labor Demand"]
summary_moments[model_interact=="Price" & other_part=="avg_labor:", other_part:="County-Quarter-Labor"]
summary_moments[model_interact=="Log Market Share" & other_part=="dye_instrument", other_part:="County-Dye Instrument"]
summary_moments[model_interact=="Log Market Share" & other_part=="avg_labor:B_raw", other_part:="County-Task Assignments"]
stopifnot(nrow(summary_moments[is.na(other_part) | other_part=="",])==0)

summary_moments[,mean_model:=as.character(format(round(mean_model   , 2), nsmall = 2)),]
summary_moments[,mean_data:=as.character(format(round(mean_data, 2), nsmall = 2)),]
summary_moments[,r2:=as.character(format(round(r2, 3), nsmall = 3)),]
setnames(summary_moments, old=c("model_interact", "other_part","count","mean_model", "mean_data", "r2"),
         new=c("Equation", "Instrument","Count","Avg. Model", "Avg. Data", "R2"))
kable(summary_moments, "latex", align="c", booktabs=TRUE,linesep = c(""), escape = F, caption = NA, label=NA) %>%
  cat(., file = "analysis_final/out/tables/02_03_model_fit.tex")













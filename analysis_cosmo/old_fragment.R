## variation in task content - within vs across firms
tot_var<-jobs_both[,.(var_tot_1=weighted.var(jobvect_1, w=emp_tot)[1,1],
                      var_tot_2=weighted.var(jobvect_2, w=emp_tot)[1,1],
                      var_tot_3=weighted.var(jobvect_3, w=emp_tot)[1,1],
                      var_tot_4=weighted.var(jobvect_4, w=emp_tot)[1,1],
                      var_tot_5=weighted.var(jobvect_5, w=emp_tot)[1,1],
                      var_tot_6=weighted.var(jobvect_6, w=emp_tot)[1,1]),
                   by=c("Source")]
byfirm<-jobs_both[,.(var_tot_1=weighted.mean(jobvect_1, w=emp_tot),
                     var_tot_2=weighted.mean(jobvect_2, w=emp_tot),
                     var_tot_3=weighted.mean(jobvect_3, w=emp_tot),
                     var_tot_4=weighted.mean(jobvect_4, w=emp_tot),
                     var_tot_5=weighted.mean(jobvect_5, w=emp_tot),
                     var_tot_6=weighted.mean(jobvect_6, w=emp_tot)),
                  by=c("Source", "location_id")]
byfirm<-byfirm[,.(within_var_1=var(var_tot_1),
                  within_var_2=var(var_tot_2),
                  within_var_3=var(var_tot_3),
                  within_var_4=var(var_tot_4),
                  within_var_5=var(var_tot_5),
                  within_var_6=var(var_tot_6)),
               by=c("Source")]

bytype<-jobs_model[,.(var_tot_1=weighted.mean(jobvect_1, w=emp_tot),
                      var_tot_2=weighted.mean(jobvect_2, w=emp_tot),
                      var_tot_3=weighted.mean(jobvect_3, w=emp_tot),
                      var_tot_4=weighted.mean(jobvect_4, w=emp_tot),
                      var_tot_5=weighted.mean(jobvect_5, w=emp_tot),
                      var_tot_6=weighted.mean(jobvect_6, w=emp_tot)),
                   by=c("Source", "type")]
bytype<-bytype[,.(within_var_1=var(var_tot_1),
                  within_var_2=var(var_tot_2),
                  within_var_3=var(var_tot_3),
                  within_var_4=var(var_tot_4),
                  within_var_5=var(var_tot_5),
                  within_var_6=var(var_tot_6)),
               by=c("Source")]

library('fixest')
reg_res<-feols(jobvect_1~1|location_id,weights=~emp_tot, data=jobs_model)
locmodel<-fixef(reg_res)

reg_res<-feols(jobvect_1~1|type,weights=~emp_tot, data=jobs_model)
typemodel<-fixef(reg_res)

reg_res<-feols(jobvect_1~1|location_id,weights=~emp_tot, data=jobs)
locobserved<-fixef(reg_res)

loc_fe<-data.table(location_id=names(locmodel$location_id), locfe_1=locmodel$location_id)
loc_obs_fe<-data.table(location_id=names(locobserved$location_id), loc_obs_fe_1=locobserved$location_id)
type_fe<-data.table(type=as.numeric(names(typemodel$type)), typefe_1=typemodel$type)

jobs_model<-merge(jobs_model, loc_fe, by="location_id", all.x=TRUE)
jobs_model<-merge(jobs_model, type_fe, by="type", all.x=TRUE)
jobs_model<-merge(jobs_model, loc_obs_fe, by="location_id", all.x=TRUE)

reg_res<-feols(jobvect_2~1|location_id,weights=~emp_tot, data=jobs_model)
locmodel<-fixef(reg_res)

reg_res<-feols(jobvect_2~1|type,weights=~emp_tot, data=jobs_model)
typemodel<-fixef(reg_res)

reg_res<-feols(jobvect_2~1|location_id,weights=~emp_tot, data=jobs)
locobserved<-fixef(reg_res)

loc_fe<-data.table(location_id=names(locmodel$location_id), locfe_2=locmodel$location_id)
loc_obs_fe<-data.table(location_id=names(locobserved$location_id), loc_obs_fe_2=locobserved$location_id)
type_fe<-data.table(type=as.numeric(names(typemodel$type)), typefe_2=typemodel$type)

jobs_model<-merge(jobs_model, loc_fe, by="location_id", all.x=TRUE)
jobs_model<-merge(jobs_model, type_fe, by="type", all.x=TRUE)
jobs_model<-merge(jobs_model, loc_obs_fe, by="location_id", all.x=TRUE)

reg_res<-feols(jobvect_3~1|location_id,weights=~emp_tot, data=jobs_model)
locmodel<-fixef(reg_res)

reg_res<-feols(jobvect_3~1|type,weights=~emp_tot, data=jobs_model)
typemodel<-fixef(reg_res)

reg_res<-feols(jobvect_3~1|location_id,weights=~emp_tot, data=jobs)
locobserved<-fixef(reg_res)

loc_fe<-data.table(location_id=names(locmodel$location_id), locfe_3=locmodel$location_id)
loc_obs_fe<-data.table(location_id=names(locobserved$location_id), loc_obs_fe_3=locobserved$location_id)
type_fe<-data.table(type=as.numeric(names(typemodel$type)), typefe_3=typemodel$type)

jobs_model<-merge(jobs_model, loc_fe, by="location_id", all.x=TRUE)
jobs_model<-merge(jobs_model, type_fe, by="type", all.x=TRUE)
jobs_model<-merge(jobs_model, loc_obs_fe, by="location_id", all.x=TRUE)

reg_res<-feols(jobvect_4~1|location_id,weights=~emp_tot, data=jobs_model)
locmodel<-fixef(reg_res)

reg_res<-feols(jobvect_4~1|type,weights=~emp_tot, data=jobs_model)
typemodel<-fixef(reg_res)

reg_res<-feols(jobvect_4~1|location_id,weights=~emp_tot, data=jobs)
locobserved<-fixef(reg_res)

loc_fe<-data.table(location_id=names(locmodel$location_id), locfe_4=locmodel$location_id)
loc_obs_fe<-data.table(location_id=names(locobserved$location_id), loc_obs_fe_4=locobserved$location_id)
type_fe<-data.table(type=as.numeric(names(typemodel$type)), typefe_4=typemodel$type)

jobs_model<-merge(jobs_model, loc_fe, by="location_id", all.x=TRUE)
jobs_model<-merge(jobs_model, type_fe, by="type", all.x=TRUE)
jobs_model<-merge(jobs_model, loc_obs_fe, by="location_id", all.x=TRUE)

reg_res<-feols(jobvect_5~1|location_id,weights=~emp_tot, data=jobs_model)
locmodel<-fixef(reg_res)

reg_res<-feols(jobvect_5~1|type,weights=~emp_tot, data=jobs_model)
typemodel<-fixef(reg_res)

reg_res<-feols(jobvect_5~1|location_id,weights=~emp_tot, data=jobs)
locobserved<-fixef(reg_res)

loc_fe<-data.table(location_id=names(locmodel$location_id), locfe_5=locmodel$location_id)
loc_obs_fe<-data.table(location_id=names(locobserved$location_id), loc_obs_fe_5=locobserved$location_id)
type_fe<-data.table(type=as.numeric(names(typemodel$type)), typefe_5=typemodel$type)

jobs_model<-merge(jobs_model, loc_fe, by="location_id", all.x=TRUE)
jobs_model<-merge(jobs_model, type_fe, by="type", all.x=TRUE)
jobs_model<-merge(jobs_model, loc_obs_fe, by="location_id", all.x=TRUE)

reg_res<-feols(jobvect_6~1|location_id,weights=~emp_tot, data=jobs_model)
locmodel<-fixef(reg_res)

reg_res<-feols(jobvect_6~1|type,weights=~emp_tot, data=jobs_model)
typemodel<-fixef(reg_res)

reg_res<-feols(jobvect_6~1|location_id,weights=~emp_tot, data=jobs)
locobserved<-fixef(reg_res)

loc_fe<-data.table(location_id=names(locmodel$location_id), locfe_6=locmodel$location_id)
loc_obs_fe<-data.table(location_id=names(locobserved$location_id), loc_obs_fe_6=locobserved$location_id)
type_fe<-data.table(type=as.numeric(names(typemodel$type)), typefe_6=typemodel$type)

jobs_model<-merge(jobs_model, loc_fe, by="location_id", all.x=TRUE)
jobs_model<-merge(jobs_model, type_fe, by="type", all.x=TRUE)
jobs_model<-merge(jobs_model, loc_obs_fe, by="location_id", all.x=TRUE)



colA = paste("jobvect", 1:6, sep = "_")
colB = paste("loc_obs_fe", 1:6, sep = "_")
colC = paste("locfe", 1:6, sep = "_")
colD = paste("typefe", 1:6, sep = "_")

fortable<-melt(jobs_model, id.vars=c("location_id", "type", "emp_tot"), measure.vars=list(colA, colB, colC, colD), value.name=c("jobvect", "loc_obs_fe", "locfe", "typefe"))
aug_table<-melt(jobs,id.vars=c("location_id", "staff_num", "emp_tot"),measure.vars=patterns("^jobvect_" ), value.name = "jobvect")
aug_table[, variable:=as.factor(gsub("jobvect_", "", variable))]
aug_table<-aug_table[,.(tot_var_obs=cust_w_var(jobvect, w=emp_tot)[1,1]), by=variable]
fortable<-fortable[,.(tot_var=weighted.var(jobvect, w=emp_tot)[1,1],
                      obs_firm_var=weighted.var(loc_obs_fe, w=emp_tot)[1,1],          
                      firm_var=weighted.var(locfe, w=emp_tot)[1,1],
                      type_var=weighted.var(typefe, w=emp_tot)[1,1]
) ,by="variable"]

#,  cov=-2*cov.wt(.SD, wt=emp_tot)$cov[1,2]
fortable<-merge(fortable,aug_table, by="variable")


##### purpose: perform the clustering procedure
##### can modify this directly to create the bootstrap program


# we want threshold to stop grouping to be low for small firms and high for big firms.
within_firm_clust<-function(mat,cut_level){
  # if only one employee, return one cluster
  if (nrow(mat)<=1){
    return(c(1))
  }else{
    clust_res<-hclust(dist(mat, method="euclidean"),method='complete')
    clust_res<-cutree(clust_res,h=cut_level)
    return(clust_res)
  }
}

  
  
  # for each market, set the cut_level to be such that all firms have 5 or less types.
  max(staff_task[county=='17031']$min_cutlevel)
max(staff_task[county=='36061']$min_cutlevel)
max(staff_task[county=='06037']$min_cutlevel)

staff_task[,county_cutlevel:=max(min_cutlevel), by=county ]
staff_task[,type_within_firm:=within_firm_clust(as.matrix(.SD),county_cutlevel[1]), by=c("location_id", "quarter_year"), .SDcols=colnames(staff_task)[colnames(staff_task) %like% "^Btilde_raw_"]]
staff_task[,types_observed_firm:=max(type_within_firm), by=c("location_id", "quarter_year")]
hist(unique(staff_task[,c("types_observed_firm","location_id", "quarter_year")])$types_observed_firm)
print(table(unique(staff_task[, c("types_observed_firm", "location_id", "quarter_year")])$types_observed_firm))


for (col in gsub("^B_","",names(staff_task)[grep("^B_", names(staff_task))])) staff_task[,(paste0("Btilde_raw_",col)):=NULL]
for (col in gsub("^duration_","",names(staff_task)[grep("^duration_", names(staff_task))])) staff_task[,smooth_e_frac  := smooth_e_frac+get(paste0("B_",col))]
for (col in gsub("^B_","",names(staff_task)[grep("^B_", names(staff_task))])) staff_task[,(paste0("Btilde_",col)):=get(paste0("B_",col))/smooth_e_frac ]
stopifnot(max(staff_task$types_observed_firm)<=5 )

# sample(unique(staff_task[type_within_firm==7]$location_id),1)
#View(staff_task[location_id==sample(unique(staff_task[type_within_firm==6]$location_id),1),.SD,.SDcols=c(colnames(staff_task)[colnames(staff_task) %like% "^Btilde"],"type_within_firm")])

# unique id across time and space for a merged person.
staff_task[, loc_person:= .GRP, by=c("location_id", "quarter_year", "type_within_firm")]
staffnum_xwalk<-merge(staffnum_xwalk,unique(staff_task[,c("loc_person", "quarter_year", "location_id", "staff_num")]), by=c("location_id", "quarter_year", "staff_num"))



#### Step 5: Merge clusters within firm
staff_merged_within<-staff_task[, lapply(.SD,sum),.SDcols=colnames(staff_task)[colnames(staff_task) %like% "^smooth_duration_"], by=c("county","location_id", "quarter_year", "type_within_firm", "service_types","types_observed_firm", "loc_person","service_mix_id","emps")]
staff_merged_within[, smooth_tot_duration:=0]
for (col in gsub("^smooth_duration_","",names(staff_merged_within)[grep("^smooth_duration_", names(staff_merged_within))])) staff_merged_within[,smooth_tot_duration  := smooth_tot_duration+sum(get(paste0("smooth_duration_",col))), by=c("location_id", "quarter_year")]
for (col in gsub("^smooth_duration_","",names(staff_merged_within)[grep("^smooth_duration_", names(staff_merged_within))])) staff_merged_within[,(paste0("B_",col))  := get(paste0("smooth_duration_",col))/smooth_tot_duration]
staff_merged_within[,smooth_e_frac:=0]
for (col in gsub("^smooth_duration_","",names(staff_merged_within)[grep("^smooth_duration_", names(staff_merged_within))])) staff_merged_within[,smooth_e_frac  := smooth_e_frac+get(paste0("B_",col))]
for (col in gsub("^B_","",names(staff_merged_within)[grep("^B_", names(staff_merged_within))])) staff_merged_within[,(paste0("Btilde_",col)):=get(paste0("B_",col))/smooth_e_frac ]
## service_mix_id gives which tasks are performed with positive probability in a quarter-location

#### AT THIS POINT, STAFF_MERGED_WITHIN IS MUTUALLY EXCLUSIVE BY TYPE WITHIN FIRM BUT NOT ACROSS FIRMS.
## firms with only one worker type must be put aside for now
## firms can only be compared within market, within quarter, within service_mix_id
## firms must have two worker types.

### start with just firms with 5 tasks observed, at least two worker types
all_tasks<-""
for (col in gsub("^B_","",names(staff_merged_within)[grep("^Btilde_", names(staff_merged_within))])) all_tasks<-paste0(all_tasks, "1")
staff_comparable<-staff_merged_within[types_observed_firm>1 & service_mix_id== all_tasks]


## choose one reference firm-quarter from each county with 5 types. use the largest firm in terms of emp count
staff_comparable_fivers<-staff_merged_within[types_observed_firm==5 & service_mix_id== all_tasks ]
staff_comparable_fivers[,ref_firm:=location_id[which.max(emps)], by="county"]
staff_comparable_fivers[,ref_quarter:=quarter_year[which.max(emps)], by="county"]
staff_comparable_fivers<-staff_comparable_fivers[location_id==ref_firm & quarter_year==ref_quarter,]
stopifnot(nrow(staff_comparable_fivers)==15)


## create function which finds minimum distance of all possible combinations.
## use reference firm based on county.

staff_comparable[, year_loc:=paste0(location_id, " - ", quarter_year) ]
label_final<-data.table()

for (cur_loc_quarter in unique(staff_comparable$year_loc)){
  cur_county<-unique(staff_comparable[year_loc==cur_loc_quarter,]$county)
  mat<-as.matrix(staff_comparable[year_loc==cur_loc_quarter, .SD, .SDcols=grep("^Btilde_", names(staff_comparable))])
  mat_comp<-as.matrix(staff_comparable_fivers[county==cur_county, .SD, .SDcols=grep("^Btilde_", names(staff_comparable_fivers))])
  allcombos<-combn(1:5, nrow(mat), simplify=FALSE)
  allpairs<-combn(1:nrow(mat), 2, simplify=FALSE)
  
  
  res<-c()
  for (i in 1:length(allcombos)){
    tot<-0
    for (pair in allpairs){
      current<-log(mat[pair[1],]/mat[pair[2],])
      current<-current/sqrt(sum(current^2))
      comp<-log(mat_comp[allcombos[[i]][pair[1]],]/mat_comp[allcombos[[i]][pair[2]],])
      comp<-comp/sqrt(sum(comp^2))
      tot<-tot+sum(abs(current-comp))
    }
    res[i]<-tot
  }
  stopifnot(length(which.min(res))==1)
  --
  label_final<-rbind(label_final, t(c(cur_loc_quarter,allcombos[[which.min(res)]] )), fill=TRUE)
}

# check that type within firm is ordered correctly.
stopifnot(all(staff_comparable[,type_within_firm==1:.N, by=year_loc]$V1))
colnames(label_final)<-c("year_loc", paste0("type_within_firm",1:5))
label_final<-melt(label_final,id.vars="year_loc", measure=patterns("^type_within_firm"), value.names="type_within_firm")
label_final[, variable:=as.numeric(sub("type_within_firm", "",variable))]
setnames(label_final, old=c("variable","value"), new=c("type_within_firm", "worker_type"))
label_final[, worker_type:=as.numeric(worker_type)]

working_data<-merge(staff_comparable,label_final,by=c("year_loc","type_within_firm"), all.x=TRUE )
stopifnot(nrow(working_data[is.na(worker_type),])==0)


############################### was 01_02 before:
###########################################################################################
###########################################################################################
###########################################################################################
###########################################################################################
###########################################################################################

# attach workerid
working_data<-merge(working_data,staffnum_xwalk[,-c("quarter_year", "location_id")], all.x=TRUE, by=c("loc_person"))


################# checks across staff id
uniqueN(working_data[,c("staff_id", "quarter_year")])
unique(working_data$quarter_year)
check<-working_data[, .(worker_type=max(worker_type), count=.N), by=c("staff_id", "quarter_year")]
check<-check[count==1,]
check<-check[, .(count_types=uniqueN(worker_type), count=.N), by=c("staff_id")]
# of the staffids observed multiple quarters, how many are classified as the same type the whole time
uniqueN(check[count>1 & count_types==1]$staff_id)/uniqueN(check[count>1]$staff_id)
# of the staffid-quarters observed more than once, how many are classified as more than one type?
nrow(check[count>1 & count_types>1])/nrow(check[count>1])
hist(check[count>1]$count_types)
check<-working_data[, .(worker_type=max(worker_type), count=.N), by=c("staff_id", "quarter_year")]
check<-check[count==1,]
check[, count:=sum(count), by=staff_id]
setkey(check, "staff_id", "quarter_year")
check[, same_flag:= data.table::shift(worker_type)==worker_type, by=staff_id]
check[, first:=min(quarter_year), by=staff_id]
mean(check[count>1 & quarter_year>first,]$same_flag)
check<-working_data[, .(count_type=uniqueN(worker_type), count=.N), by=c("staff_id", "quarter_year")]
check<-check[count>1,]
table(check$count_type)
###################

### Attach types back to data to get internal org.
# bring back in raw data
staff_task<-merge(staff_task, working_data[,c("location_id","staff_num", "county", "quarter_year","worker_type"),],by=c("location_id","staff_num", "county", "quarter_year"),all.x=TRUE)
uniqueN(staff_task[!is.na(worker_type), c("location_id", "quarter_year")])/uniqueN(staff_task[, c("location_id", "quarter_year")])
staff_task<-staff_task[!is.na(worker_type),]

# collapse to types within firm.
cols_tokeep<-c("location_id", "county", "quarter_year", "location_state", "location_city", "location_zip", "business_id", "smooth_tot_duration", "emps", "service_types", "revenue", "tot_duration", "cust_count", "CSPOP", "s_index")
cols_tokeep<-c(cols_tokeep, names(staff_task)[grep("^task_mix_", names(staff_task))])
stopifnot(nrow(unique(staff_task[,.SD, .SDcols=cols_tokeep]) )==uniqueN(staff_task[,c("quarter_year", "location_id")]) )
staff_task[,smooth_emp_duration:=0]
for (col in gsub("^smooth_duration_","",names(staff_task)[grep("^smooth_duration_", names(staff_task))])) staff_task[,(paste0("smooth_emp_duration"))  := get(paste0("smooth_emp_duration"))+get(paste0("smooth_duration_", col)) ]
staff_merged_within<-staff_task[, lapply(.SD,sum),.SDcols=c(colnames(staff_task)[colnames(staff_task) %like% "^smooth_duration_"],"smooth_emp_duration",colnames(staff_task)[colnames(staff_task) %like% "^duration_"], "emp_duration"), by=c(cols_tokeep,"worker_type")]
setkey(staff_merged_within, "location_id","quarter_year","worker_type")
# unique by merged type.
stopifnot(uniqueN(staff_merged_within[,c("location_id","quarter_year","worker_type")])==nrow(staff_merged_within))

# expand so that 5 types
expanded_data<-data.table()
for (i in 1:(length(colnames(staff_task)[colnames(staff_task) %like% "^smooth_duration_"])) ) expanded_data<-rbind(expanded_data,unique(staff_merged_within[, .SD, .SDcols=c(cols_tokeep) ]))
expanded_data[, worker_type:=1:.N, by=cols_tokeep]
expanded_data<-merge(expanded_data, staff_merged_within, by=c(cols_tokeep,"worker_type"), all.x=TRUE)
for (col in c(names(expanded_data)[grep("^smooth_duration_", names(expanded_data))],"smooth_emp_duration")) expanded_data[,(col)  := ifelse(is.na(get(col)), 0,get(col))  ]
for (col in c(names(expanded_data)[grep("^duration_", names(expanded_data))],"emp_duration")) expanded_data[,(col)  := ifelse(is.na(get(col)), 0,get(col))  ]
for (col in gsub("^smooth_duration_","",names(expanded_data)[grep("^smooth_duration_", names(expanded_data))])) expanded_data[,(paste0("B_",col)):=get(paste0("smooth_duration_",col))/smooth_tot_duration]
for (col in gsub("^smooth_duration_","",names(expanded_data)[grep("^smooth_duration_", names(expanded_data))])) expanded_data[,(paste0("BdivE_",col)):=get(paste0("smooth_duration_",col))/smooth_emp_duration]
expanded_data[, E:=smooth_emp_duration/smooth_tot_duration]

## raw versions
expanded_data[, E_raw:=emp_duration/tot_duration]
for (col in gsub("^duration_","",names(expanded_data)[grep("^duration_", names(expanded_data))])) expanded_data[,(paste0("B_raw_",col)):=get(paste0("duration_",col))/tot_duration]

# cast to fully wide for regressions and gamma.
# note that labeling B_1_2 means task 1, worker 2
fmla <- as.formula(paste(paste(names(expanded_data)[grep("^task_mix_", names(expanded_data))], collapse= "+"), "+location_id+quarter_year+county+tot_duration+smooth_tot_duration+revenue+cust_count+CSPOP+s_index~worker_type"))
verywide_expanded<-dcast(expanded_data, fmla, value.var=c(names(expanded_data)[grep("^B_", names(expanded_data))], "E", names(expanded_data)[grep("^B_raw_", names(expanded_data))], "E_raw") )
stopifnot(all(verywide_expanded[,.(.N), by=location_id]$N<=12))
# expand so that we have every pairwise division
# first every type
expanded_data<-data.table()
for (i in 1:5 ) expanded_data<-rbind(expanded_data,unique(staff_merged_within[, .SD, .SDcols=c(cols_tokeep) ]))
expanded_data[, worker_type:=1:.N, by=cols_tokeep]

temp<-copy(expanded_data)
for (i in 1:3 ) temp<-rbind(temp,expanded_data)
expanded_data<-copy(temp)
rm(temp)
expanded_data[, compare_type:=setdiff(1:5,worker_type), by=c(cols_tokeep, "worker_type")]
stopifnot(nrow(expanded_data)==uniqueN(expanded_data[,.SD, .SDcols=c(cols_tokeep, "worker_type", "compare_type")]))
expanded_data<-merge(expanded_data, staff_merged_within, by=c(cols_tokeep,"worker_type"), all.x=TRUE)
temp<-copy(staff_merged_within)
colnames(temp)[colnames(temp) %like% "^smooth_duration_"]<-paste0("compare_duration_", 1:length(colnames(temp)[colnames(temp) %like% "^smooth_duration_"]))
setnames(temp, "smooth_emp_duration", "compare_emp_duration")
setnames(temp, "worker_type", "compare_type")
expanded_data<-merge(expanded_data, temp, by=c(cols_tokeep,"compare_type"), all.x=TRUE)
rm(temp)
for (col in gsub("^smooth_duration_","",names(expanded_data)[grep("^smooth_duration_", names(expanded_data))])) expanded_data[,(paste0("BdivE_",col)):=get(paste0("smooth_duration_",col))/smooth_emp_duration]
for (col in gsub("^smooth_duration_","",names(expanded_data)[grep("^smooth_duration_", names(expanded_data))])) expanded_data[,(paste0("compare_BdivE_",col)):=get(paste0("compare_duration_",col))/compare_emp_duration]
for (col in gsub("^smooth_duration_","",names(expanded_data)[grep("^smooth_duration_", names(expanded_data))])) expanded_data[,(paste0("ratio_",col)):=log(get(paste0("compare_BdivE_",col))/get(paste0("BdivE_",col))) ]
for (col in gsub("^smooth_duration_","",names(expanded_data)[grep("^smooth_duration_", names(expanded_data))])) expanded_data[,(paste0("raw_fracs_",col)):=pmin(get(paste0("compare_BdivE_",col)),get(paste0("BdivE_",col)))]

# for any BdivE that is small set ratio to NA
for (col in gsub("^ratio_","",names(expanded_data)[grep("^ratio", names(expanded_data))])) expanded_data[,(paste0("ratio_",col)):=ifelse(round(get(paste0("compare_BdivE_",col)),3)==0, NA,get(paste0("ratio_",col))) ]
for (col in gsub("^ratio_","",names(expanded_data)[grep("^ratio", names(expanded_data))])) expanded_data[,(paste0("ratio_",col)):=ifelse(round(get(paste0("BdivE_",col)),3)==0, NA,get(paste0("ratio_",col))) ]

forgamma_verywide<-dcast(expanded_data, location_id+quarter_year+county~worker_type+compare_type, value.var=names(expanded_data)[grep("^ratio_", names(expanded_data))] )
# new variables read as "task_numeratortype_denomtype
# set all nas to 0 (unused)
for (col in gsub("^ratio_","",names(forgamma_verywide)[grep("^log_ratio_", names(forgamma_verywide))])) forgamma_verywide[,(paste0("ratio_",col)):=ifelse(is.na(get(paste0("ratio_",col))), 0,get(paste0("ratio_",col))) ]
# set all infinites to 0 
for (col in gsub("^ratio_","",names(forgamma_verywide)[grep("^ratio_", names(forgamma_verywide))])) forgamma_verywide[,(paste0("ratio_",col)):=ifelse(is.finite(get(paste0("ratio_",col))), get(paste0("ratio_",col)),0) ]

# this is just to reference the raw fractions.
rawfracs_verywide<-dcast(expanded_data, location_id+quarter_year+county~worker_type+compare_type, value.var=names(expanded_data)[grep("^raw_fracs_", names(expanded_data))] )
# set all nas to 0 (unused)
for (col in gsub("^raw_fracs_","",names(rawfracs_verywide)[grep("^raw_fracs_", names(rawfracs_verywide))])) rawfracs_verywide[,(paste0("raw_fracs_",col)):=ifelse(is.na(get(paste0("raw_fracs_",col))), 0,get(paste0("raw_fracs_",col))) ]
# set all infinites to 0 
for (col in gsub("^raw_fracs_","",names(rawfracs_verywide)[grep("^raw_fracs_", names(rawfracs_verywide))])) rawfracs_verywide[,(paste0("raw_fracs_",col)):=ifelse(is.finite(get(paste0("raw_fracs_",col))), get(paste0("raw_fracs_",col)),0) ]


### recover gamma
# need to find a path through all firms each quarter within market
# create location-quarter id
forgamma_verywide[, loc_quarter:=paste0(location_id, "-", as.character(quarter_year))]
verywide_expanded[, loc_quarter:=paste0(location_id, "-", as.character(quarter_year))]
stopifnot(uniqueN(forgamma_verywide[,'loc_quarter'])==nrow(forgamma_verywide))

# for display reasons only
for (cnty in c("06037", "36061","17031")){
  
  # create a matrix that fills in all squares that are non-zero to be 1
  vect_mat<-as.matrix(forgamma_verywide[county==cnty & round(quarter_year)==2019,][,-c("location_id", "quarter_year", "county","loc_quarter")])
  vect_mat[round(vect_mat,3)==0]<-0
  vect_mat[vect_mat!=0]<-1
  dist_mat<-data.table(expand.grid(1:nrow(vect_mat),1:nrow(vect_mat) ))
  
  
  # two are similar if two match
  simil_function<-function(vec1,vec2){
    return(sum(vec1[vec1==vec2]))
  }
  
  wrapper_func<-Vectorize(function(loc1, loc2){
    v1<-as.numeric(vect_mat[loc1,])
    v2<-as.numeric(vect_mat[loc2,])
    return(simil_function(v1, v2))
  })
  
  
  dist_mat[, match_types:=wrapper_func(Var1, Var2)]
  dist_mat<-dcast(dist_mat, Var1~Var2, value.var=c("match_types"))
  qgraph_mat<-as.matrix(dist_mat[,-c("Var1")])
  rownames(qgraph_mat)<-rep(1:nrow(dist_mat))
  colnames(qgraph_mat)<-rep(1:nrow(dist_mat))
  qgraph_mat[qgraph_mat<2]<-0
  qgraph_mat[qgraph_mat>=2]<-1
  if (!exists("bootreps")){
  png(paste0("analysis_final/out/01_02_network_2019_",cnty,".png"), pointsize=10, width=1400, height=1200, res=300)
  qgraph(qgraph_mat, layout="spring", esize=1, edge.color="black")
  dev.off()
  }
  # get paths between all firms.
  centrality(qgraph(qgraph_mat, layout="spring"), all.shortest.paths = TRUE)->paths_betweenfirms
}

for (cnty in c("06037", "36061","17031")){
  
  # create a matrix that fills in all squares that are non-zero to be 1
  vect_mat<-as.matrix(forgamma_verywide[county==cnty][,-c("location_id", "quarter_year", "county","loc_quarter")])
  vect_mat[round(vect_mat,3)==0]<-0
  vect_mat[vect_mat!=0]<-1
  dist_mat<-data.table(expand.grid(1:nrow(vect_mat),1:nrow(vect_mat) ))
  
  
  # two are similar if two match
  simil_function<-function(vec1,vec2){
    return(sum(vec1[vec1==vec2]))
  }
  
  wrapper_func<-Vectorize(function(loc1, loc2){
    v1<-as.numeric(vect_mat[loc1,])
    v2<-as.numeric(vect_mat[loc2,])
    return(simil_function(v1, v2))
  })
  
  
  dist_mat[, match_types:=wrapper_func(Var1, Var2)]
  dist_mat<-dcast(dist_mat, Var1~Var2, value.var=c("match_types"))
  qgraph_mat<-as.matrix(dist_mat[,-c("Var1")])
  rownames(qgraph_mat)<-rep(1:nrow(dist_mat))
  colnames(qgraph_mat)<-rep(1:nrow(dist_mat))
  qgraph_mat[qgraph_mat<2]<-0
  qgraph_mat[qgraph_mat>=2]<-1
  #png(paste0("analysis_final/out/01_02_network_",cnty,".png"), pointsize=10, width=1400, height=960, res=300)
  qgraph(qgraph_mat, layout="spring", esize=1, edge.color="blue")
  #dev.off()
  # get paths between all firms.
  centrality(qgraph(qgraph_mat, layout="spring"), all.shortest.paths = TRUE)->paths_betweenfirms
  
  # this tells me the order of rows to lookup.
  
  # get the ratio relative to the largest firm in the market
  # set of firms in all quarters.
  largest_firm<-staff_merged_within[county==cnty,.(count_qt=uniqueN(quarter_year),emp_max=max(emps)), by="location_id"][count_qt==uniqueN(working_data$quarter_year),][max(emp_max)==emp_max,]$location_id[1]
  lg_num<-which(forgamma_verywide[county==cnty ]$location_id==largest_firm & forgamma_verywide[county==cnty ]$quarter_year==2021.1 )
  # for bootstrap parts, this may fail. in that case choose firm in the most quarters, then with the most emps.
  if (is.na(largest_firm)){
    largest_firm<-staff_merged_within[county==cnty,.(count_qt=uniqueN(quarter_year),emp_max=max(emps)), by="location_id"]
    setorder(largest_firm, -"count_qt",-"emp_max")
    largest_firm<-largest_firm[1]$location_id
    lg_num<-which.max((forgamma_verywide[county==cnty ]$location_id==largest_firm)*forgamma_verywide[county==cnty ]$quarter_year)[1]
  }
  
  
  # returns location_id
  get_loc<-function(x){
    return(forgamma_verywide[county==cnty]$loc_quarter[x])
  }
  
  find_vecmax<-function(x,y){
    
    
    v1<-as.numeric(forgamma_verywide[county==cnty][loc_quarter==get_loc(x),-c("location_id","loc_quarter", "quarter_year", "county")])
    v2<-as.numeric(forgamma_verywide[county==cnty][loc_quarter==get_loc(y),-c("location_id","loc_quarter", "quarter_year", "county")])
    
    # index of shared.
    shared<-which(as.logical((round(abs(v1),8)>0)*(round(abs(v2),8)>0)))
    res<-v1[shared]/v2[shared]
    
    return(max(res ))
  }
  # recurse down to the start
  recurse_down<-function(x){
    if (length(x)==2) {
      return(find_vecmax(x[1], x[2]))
    } else{
      return(find_vecmax(x[1], x[2])*recurse_down(x[-1]))
    }
  }
  
  outer_check<-Vectorize(function(loc){
    x<-which(forgamma_verywide[county==cnty]$loc_quarter==loc)
    # if connected
    if (length(paths_betweenfirms$ShortestPaths[x,lg_num][[1]])>0){
      part<-0
      iter<-0
      for (p in 1:length(paths_betweenfirms$ShortestPaths[x,lg_num][[1]])){
        path_goal<-paths_betweenfirms$ShortestPaths[x,lg_num][[1]][[p]]
        
        holdval<-recurse_down(path_goal)
        if (holdval>0){
          part<-part+(holdval)^(-1)
          iter<-iter+1
        }
      }
      #return(part/length(paths_betweenfirms$ShortestPaths[x,lg_num][[1]]))
      return(part/iter)
      # if unconnected
    } else{
      if (lg_num==x){
        return(1)
      } else{
        return(NA)
      }
    }
  })
  
  verywide_expanded[county==cnty, gamma_normalized:=outer_check(loc_quarter)]
  
}

# should always be connected in the main sample
# for bootstrap sample, drop the disconnected part.
if (!exists("bootreps")){
  stopifnot(nrow(verywide_expanded[is.na(gamma_normalized)])==0)
} else{
  if (nrow(verywide_expanded[is.na(gamma_normalized)])!=0) print("Some disconnected")
  verywide_expanded<-verywide_expanded[!is.na(gamma_normalized),]
}


######################## MAKE AUXILLIARY PARAMETERS
verywide_expanded[, cust_price:=revenue/cust_count]
verywide_expanded[, avg_labor:=tot_duration/cust_count/60]

verywide_expanded[, CSPOP:=as.numeric(CSPOP)]
verywide_expanded[, county:=as.numeric(county)]
verywide_expanded[, salon_share_subdiv:=cust_count/CSPOP]

### get the fraction of non-purchasers from the CEX
cex<-readRDS('mkdata/data/cex_outside.rds')
cex[, outside_share:=nohc_count /count_sample ]
cex<-cex[str_detect(PSU,"S")>0,]
cex<-unique(cex)
stopifnot(nrow(cex)==uniqueN(cex[,c("quarter_year", "PSU")]))
# get psu to county xwalk
cex<-merge(cex,data.table(readRDS("mkdata/data/county_msa_xwalk.rds")),by="PSU", all.x=TRUE, allow.cartesian = TRUE)
stopifnot(nrow(cex)==uniqueN(cex[,c("quarter_year", "county")]))


verywide_expanded<-merge(verywide_expanded,cex[,c("county", "quarter_year", "outside_share") ], by=c("county", "quarter_year"),all.x=TRUE )
stopifnot(nrow(verywide_expanded[is.na(outside_share)])==0)

### get qcew for wages.
qcew<-readRDS('mkdata/data/qcew_county.rds')
# unique after we keep only private establishments
qcew<-qcew[own_code==5 & !is.na(county),]
stopifnot(nrow(qcew)==uniqueN(qcew[,c("county", "quarter_year")]))
qcew[,avg_wage_qtr:=total_qtrly_wages/qtrly_estabs]
verywide_expanded<-merge(verywide_expanded, qcew[,c("county", "quarter_year", "avg_wage_qtr", "avg_wkly_wage")], by=c("county", "quarter_year"),
                         all.x=TRUE)
stopifnot(nrow(verywide_expanded[(county %in% c("36061", "6037", "17031")) & is.na(avg_wage_qtr)])==0)




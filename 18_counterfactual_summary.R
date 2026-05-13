## Counterfactual summary tables.
## Reads the wage and productivity panels written by 13-17 and aggregates
## them into the LaTeX comparison tables consumed by the manuscript.
source("config.R")
source("utils/counterfactuals_core.R")

innertol <- CONFIG$counterfactual_innertol
outertol <- CONFIG$counterfactual_outertol

counterfactual_context <- load_counterfactual_context(extra_packages = c("kableExtra"))
working_data    <- counterfactual_context$working_data
initial_wages   <- counterfactual_context$initial_wages
all_results     <- counterfactual_context$all_results
market_parms    <- counterfactual_context$market_parms
total_labor     <- counterfactual_context$total_labor
total_labor_orig<- counterfactual_context$total_labor_orig
initial_guess   <- counterfactual_context$initial_guess
rho             <- counterfactual_context$rho
tild_theta      <- counterfactual_context$tild_theta

n_worker_types <- CONFIG$n_worker_types

#### initial
wage_vect_initial<-read_counterfactual_rds(
  "13_initial_wages.rds",
  legacy_filenames = "13_initial_wages.rds",
  description = "baseline counterfactual wages"
)
prod_data_initial<-read_counterfactual_rds(
  "15_prod_initial.rds",
  legacy_filenames = "15_prod_initial.rds",
  description = "baseline counterfactual productivity panel"
)


prod_data_initial<-melt(prod_data_initial,id.vars=c("location_id", "avg_labor", "new_share","CSPOP", "county", "s_index","sol_type", "weight"), measure.vars=patterns("^B_[0-9]") )
prod_data_initial[, multiplier:=avg_labor*CSPOP*new_share*weight]
prod_data_initial[, worker_type:=str_replace(variable, "^B_[0-9]_","")]

firm_initial<-prod_data_initial[,.(tot_prod=sum(value)), c('s_index', "multiplier","location_id", "county", "sol_type")]
firm_initial<-firm_initial[, .(s_avg=weighted.mean(s_index, multiplier),labor_prod=sum(tot_prod*multiplier)), by=c("county", "sol_type")]


prod_data_initial<-prod_data_initial[, .(tot_prod=sum(multiplier*value)), by=c("county", "worker_type","sol_type")]


prod_data_initial<-dcast(prod_data_initial, county+sol_type~worker_type, value.var="tot_prod")
prod_data_initial[, version:="Initial"]


#### sales tax
wage_vect_salestax<-read_counterfactual_rds(
  "15_wages_salestax.rds",
  legacy_filenames = "15_wages_salestax.rds",
  description = "sales-tax counterfactual wages"
)
prod_data_salestax<-read_counterfactual_rds(
  "15_prod_salestax.rds",
  legacy_filenames = "15_prod_salestax.rds",
  description = "sales-tax counterfactual productivity panel"
)


prod_data_salestax<-melt(prod_data_salestax,id.vars=c("location_id", "avg_labor", "new_share","CSPOP", "county", "s_index","sol_type", "weight"), measure.vars=patterns("^B_[0-9]") )
prod_data_salestax[, multiplier:=avg_labor*CSPOP*new_share*weight]
prod_data_salestax[, worker_type:=str_replace(variable, "^B_[0-9]_","")]

firm_salestax<-prod_data_salestax[,.(tot_prod=sum(value)), c('s_index', "multiplier","location_id", "county", "sol_type")]
firm_salestax<-firm_salestax[, .(s_avg=weighted.mean(s_index, multiplier),labor_prod=sum(tot_prod*multiplier)), by=c("county", "sol_type")]
firm_salestax[, version:="Sales Tax"]


prod_data_salestax<-prod_data_salestax[, .(tot_prod=sum(multiplier*value)), by=c("county", "worker_type","sol_type")]


prod_data_salestax<-dcast(prod_data_salestax, county+sol_type~worker_type, value.var="tot_prod")
prod_data_salestax[, version:="Sales Tax"]

#### management diffusion
wage_vect_diffusion<-read_counterfactual_rds(
  "14_wages_diffusion.rds",
  legacy_filenames = "14_wages_diffusion.rds",
  description = "diffusion counterfactual wages"
)
prod_data_diffusion<-read_counterfactual_rds(
  "14_prod_diffusion.rds",
  legacy_filenames = "14_prod_diffusion.rds",
  description = "diffusion counterfactual productivity panel"
)


prod_data_diffusion<-melt(prod_data_diffusion,id.vars=c("location_id", "avg_labor", "new_share","CSPOP", "county", "s_index","sol_type", "weight"), measure.vars=patterns("^B_[0-9]") )
prod_data_diffusion[, multiplier:=avg_labor*CSPOP*new_share*weight]
prod_data_diffusion[, worker_type:=str_replace(variable, "^B_[0-9]_","")]

firm_diffusion<-prod_data_diffusion[,.(tot_prod=sum(value)), c('s_index', "multiplier","location_id", "county", "sol_type")]
firm_diffusion<-firm_diffusion[, .(s_avg=weighted.mean(s_index, multiplier),labor_prod=sum(tot_prod*multiplier)), by=c("county", "sol_type")]
firm_diffusion[, version:="Management Diffusion"]



prod_data_diffusion<-prod_data_diffusion[, .(tot_prod=sum(multiplier*value)), by=c("county", "worker_type","sol_type")]


prod_data_diffusion<-dcast(prod_data_diffusion, county+sol_type~worker_type, value.var="tot_prod")
prod_data_diffusion[, version:="Management Diffusion"]

#### immigration
wage_vect_immigration<-read_counterfactual_rds(
  "16_wages_immigration.rds",
  legacy_filenames = "16_wages_immigration.rds",
  description = "immigration counterfactual wages"
)
prod_data_immigration<-read_counterfactual_rds(
  "16_prod_immigration.rds",
  legacy_filenames = "16_prod_immigration.rds",
  description = "immigration counterfactual productivity panel"
)


prod_data_immigration<-melt(prod_data_immigration,id.vars=c("location_id", "avg_labor", "new_share","CSPOP", "county", "s_index","sol_type", "weight"), measure.vars=patterns("^B_[0-9]") )
prod_data_immigration[, multiplier:=avg_labor*CSPOP*new_share*weight]
prod_data_immigration[, worker_type:=str_replace(variable, "^B_[0-9]_","")]

## to get labor productivity need to adjust by 1.1 the value for the immigration group
prod_data_immigration[worker_type==1 & county==6037, value:=value/1.1]
prod_data_immigration[worker_type==5 & county==17031, value:=value/1.1]
prod_data_immigration[worker_type==3 & county==36061, value:=value/1.1]

firm_immigration<-prod_data_immigration[,.(tot_prod=sum(value)), c('s_index', "multiplier","location_id", "county", "sol_type")]
firm_immigration<-firm_immigration[, .(s_avg=weighted.mean(s_index, multiplier),labor_prod=sum(tot_prod*multiplier)), by=c("county", "sol_type")]
firm_immigration[, version:="Immigration"]



prod_data_immigration<-prod_data_immigration[, .(tot_prod=sum(multiplier*value)), by=c("county", "worker_type","sol_type")]

prod_data_immigration<-dcast(prod_data_immigration, county+sol_type~worker_type, value.var="tot_prod")
prod_data_immigration[, version:="Immigration"]


#### merger
wage_vect_merger<-read_counterfactual_rds(
  "17_wages_merger.rds",
  legacy_filenames = "17_wages_merger.rds",
  description = "merger counterfactual wages"
)
prod_data_merger<-read_counterfactual_rds(
  "17_prod_merger.rds",
  legacy_filenames = "17_prod_merger.rds",
  description = "merger counterfactual productivity panel"
)


prod_data_merger<-melt(prod_data_merger,id.vars=c("location_id", "avg_labor", "new_share","CSPOP", "county", "s_index","sol_type", "weight"), measure.vars=patterns("^B_[0-9]") )
prod_data_merger[, multiplier:=avg_labor*CSPOP*new_share*weight]
prod_data_merger[, worker_type:=str_replace(variable, "^B_[0-9]_","")]

firm_merger<-prod_data_merger[,.(tot_prod=sum(value)), c('s_index', "multiplier","location_id", "county", "sol_type")]
firm_merger<-firm_merger[, .(s_avg=weighted.mean(s_index, multiplier),labor_prod=sum(tot_prod*multiplier)), by=c("county", "sol_type")]
firm_merger[, version:="Incr. Concentration"]



prod_data_merger<-prod_data_merger[, .(tot_prod=sum(multiplier*value)), by=c("county", "worker_type","sol_type")]

prod_data_merger<-dcast(prod_data_merger, county+sol_type~worker_type, value.var="tot_prod")
prod_data_merger[, version:="Incr. Concentration"]


## make table based on percentages.
tot_table<-rbind(firm_salestax, firm_diffusion, firm_immigration, firm_merger)
colnames(firm_initial)<-c("county", "sol_type", "initial_s", "initial_prod")

tot_table<-merge(tot_table, firm_initial[, -"sol_type"], by="county")
tot_table[, pct_sindex:=(s_avg-initial_s)/initial_s]
tot_table[, pct_prod:=(labor_prod-initial_prod)/initial_prod]

tot_table<-dcast(tot_table, version+county~sol_type, value.var=c("pct_sindex", "pct_prod"))
setcolorder(tot_table, c("version", "county", "pct_sindex_realloc", "pct_prod_realloc"))
cols<-colnames(tot_table)[-c(1,2)]
tot_table[,(cols) := lapply(.SD, function(x) as.character(format(round(x   , 3), nsmall = 3))),.SDcols=cols]




tot_table[, county_name:=ifelse(county=="17031", "Cook",ifelse(county=="36061", "New York", "Los Angeles"))]
setcolorder(tot_table, "county_name")
colnames(tot_table)[-c(1,2,3)]<-c("S-Index Change", "Prod. Change", "S-Index Change", "Prod. Change")
setnames(tot_table, "county_name", "County")
setnames(tot_table, "version", "Counterfactual")

output<-kable(tot_table[,-"county"], "latex", align="c", booktabs=TRUE,linesep = c(""), escape = F, caption = NA, label=NA)
output<-add_header_above(output,c(" "," ", "Reallocation" = 2, "Reorganization" = 2))
write_counterfactual_text(
  output,
  "18_tot_counterfactuals.tex",
  type = "tables",
  legacy_filename = "18_tot_counterfactuals.tex"
)

## changes by worker type.
wage_vect_merger[, version:="Incr. Concentration"]
wage_vect_salestax[, version:="Sales Tax"]
wage_vect_immigration[,version:="Immigration"]
wage_vect_diffusion[, version:="Management Diffusion"]
wage_changes<-rbind(wage_vect_merger,wage_vect_salestax,wage_vect_immigration,wage_vect_diffusion)
wage_changes<-merge(wage_changes, wage_vect_initial, by=c("county", "quarter_year"))
for (idx in seq_len(n_worker_types)) {
  new_col <- paste0("wage", idx)
  x_col <- paste0("w", idx, ".x")
  y_col <- paste0("w", idx, ".y")
  wage_changes[, (new_col) := (get(x_col) - get(y_col)) / get(y_col)]
}

prod_data_merger[, version:="Incr. Concentration"]
prod_data_salestax[, version:="Sales Tax"]
prod_data_immigration[,version:="Immigration"]
prod_data_diffusion[, version:="Management Diffusion"]
prod_changes<-rbind(prod_data_merger,prod_data_salestax,prod_data_immigration,prod_data_diffusion)
prod_changes<-merge(prod_changes, prod_data_initial[,-c("sol_type", "version")], by=c("county"))
for (idx in seq_len(n_worker_types)) {
  new_col <- paste0("prod", idx)
  x_col <- paste0(idx, ".x")
  y_col <- paste0(idx, ".y")
  prod_changes[, (new_col) := (get(x_col) - get(y_col)) / get(y_col)]
}

het_prod<-merge(prod_changes, wage_changes[is.finite(w1.x),], by=c("county", "sol_type", "version"))
prod_wage_pairs <- as.vector(rbind(paste0("prod", seq_len(n_worker_types)),
                                   paste0("wage", seq_len(n_worker_types))))
het_prod[, c("county","version" ,"sol_type", prod_wage_pairs), with = FALSE]
cols <- prod_wage_pairs
het_prod[,(cols) := lapply(.SD, function(x) as.character(format(round(x   , 3), nsmall = 3))),.SDcols=cols]
het_prod[, county_name:=ifelse(county=="17031", "Cook",ifelse(county=="36061", "New York", "Los Angeles"))]
setnames(het_prod, "county_name", "County")
setnames(het_prod, "version", "Counterfactual")
setorder(het_prod, "Counterfactual", "County")
het_prod <- het_prod[sol_type == "reorg",
                     c("Counterfactual", "County", prod_wage_pairs),
                     with = FALSE]
setnames(het_prod,
         old = prod_wage_pairs,
         new = rep(c("Prod.","Wage"), n_worker_types))
output<-kable(het_prod, "latex", align="c", booktabs=TRUE,linesep = c(""), escape = F, caption = NA, label=NA)
header_skill_sets <- setNames(rep(2, n_worker_types),
                              paste0("Skill Set ", seq_len(n_worker_types)))
output<-add_header_above(output, c(" ", " ", header_skill_sets))
write_counterfactual_text(
  output,
  "18_bytype_counterfactuals.tex",
  type = "tables",
  legacy_filename = "18_bytype_counterfactuals.tex"
)

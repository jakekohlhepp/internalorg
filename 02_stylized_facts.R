#' =============================================================================
#' STEP 02: Stylized Facts and Non-Task Variables (Full National Sample)
#' =============================================================================
#' Produces the stylized-facts evidence reported in the paper: dispersion in
#' specialization and productivity, productivity-specialization correlations,
#' management practices, service description quality, and customer return
#' rates. This script runs on the FULL national sample, not the three-county
#' estimation sample -- stylized facts are intended to describe the industry
#' broadly.
#'
#' Pipeline:
#'   1. Load cleaned transactions from 00_tasks_cosmo.rds.
#'   2. Build visit-level customer-behavior variables.
#'   3. Compute service-description misspelling rates.
#'   4. Merge three auxiliary data pulls (chair renters, tips, products).
#'   5. Collapse to a firm-quarter panel; join s_index and task_mix from the
#'      pre-filter 01_staff_task_full.rds.
#'   6. Save the firm-quarter panel for use by 03_spatial_corr.R.
#'   7. Run the stylized-facts regressions and save tables / figures.
#'
#' Inputs:
#'   - mkdata/data/00_tasks_cosmo.rds        (cleaned transactions)
#'   - mkdata/data/01_staff_task_full.rds    (pre-filter firm-quarter panel)
#'   - CONFIG$raw_data_base/<sub-paths>      (chair renters, tip pulls, products)
#'
#' Outputs:
#'   - results/data/02_stylized_facts_data.rds  (firm-quarter panel used by 03)
#'   - results/out/tables/02_*.tex               (7 tex files)
#'   - results/out/figures/02_*.png              (8 png files; each binscatter
#'     is drawn on equal-width bins and again on equal-count bins, *_qbins.png)
#'   - logs/02_stylized_facts_console.log        (every number printed below)
#' =============================================================================

# -----------------------------------------------------------------------------
# Libraries
# -----------------------------------------------------------------------------
library('qdapDictionaries')
library('data.table')
library('lubridate')
library('stringr')
library('zoo')
library('binsreg')
library('ggplot2')
library('lessR')
library('stats')
library('dendextend')
library('qgraph')
library('adespatial')
library('quanteda')
library('DescTools')
library('stringdist')
library('knitr')
library('kableExtra')
library('fixest')
library('pander')
library('stargazer')
library('showtext')
library('mediation')

# -----------------------------------------------------------------------------
# Configuration
# -----------------------------------------------------------------------------
source('config.R')

ensure_directory("results/data")
ensure_directory("results/out/tables")
ensure_directory("results/out/figures")
ensure_directory(CONFIG$log_dir)

# -----------------------------------------------------------------------------
# Console log
# -----------------------------------------------------------------------------
# The Fact 1-4 numbers below are reported with cat()/print() and nothing else
# captures them: logs/02_stylized_facts.log is written by run_all.R's wrapper
# and records only the start/finish bookkeeping, so every printed statistic is
# lost when the session ends. Tee stdout to its own file (split = TRUE keeps the
# console echo) so the numbers cited in the manuscript stay recoverable.
#
# The sink is opened and closed here rather than via on.exit(): source() gives
# each top-level expression its own short-lived frame, so an on.exit()
# registered here would fire the instant this expression finishes, tearing the
# sink down before anything is logged. That leaves the failure path uncovered --
# an error before the closing sink() would strand it, and split = TRUE means
# the console keeps echoing, so the rest of the session would silently append
# to this log with no visible symptom. run_all.R's STEP 2b drops a stranded
# sink in its finally block; the reset here covers a standalone re-run.
while (sink.number() > 0) sink()
console_log_path <- file.path(CONFIG$log_dir, "02_stylized_facts_console.log")
console_log_con <- file(console_log_path, open = "wt")
sink(console_log_con, split = TRUE)

if (!nzchar(CONFIG$raw_data_base)) {
  stop("CONFIG$raw_data_base must be set to run 02_stylized_facts.R")
}

task_data_path <- project_path(CONFIG$prep_output_dir, "00_tasks_cosmo.rds")
staff_task_full_path <- project_path(CONFIG$prep_output_dir, "01_staff_task_full.rds")
chairrenter_path <- file.path(CONFIG$raw_data_base, "20201204_chair_renters/staff_renters.csv")
tip_dir <- file.path(CONFIG$raw_data_base, "20200909_raw", "Marketing Intern Data")
tip_header_path <- file.path(tip_dir, "x00.csv")
tip_data2_path <- file.path(CONFIG$raw_data_base, "20201214_tip_more/bi_tip_lines_2017-2020.csv")
tip_data3_path <- file.path(CONFIG$raw_data_base, "20210809_alldata_refresh_withzip/tip_amount_export.csv")
product_data1_path <- file.path(CONFIG$raw_data_base, "20210809_alldata_refresh_withzip/product_sales_export.csv")
product_data2_path <- file.path(CONFIG$raw_data_base, "2017_2020_product_sales/2017_2020_product_sales.csv")

assert_required_files(c(
  task_data_path,
  staff_task_full_path,
  chairrenter_path,
  tip_header_path,
  tip_data2_path,
  tip_data3_path,
  product_data1_path,
  product_data2_path
))

showtext_auto()
showtext_opts(dpi = 300)

my_style = style.df(depvar.title = "", fixef.title = "",
                    fixef.suffix = " fixed effect", yesNo = "yes")
setFixest_etable(style.df = my_style, postprocess.df = pandoc.table.return)

is.word  <- Vectorize(function(x) x %in% GradyAugmented)

set.seed(588621)
rowMax <- function(data) apply(data, 1, max, na.rm = TRUE)
rowMin <- function(data) apply(data, 1, min, na.rm = TRUE)
spec_log <- function(x) ifelse(x == 0 | x == -Inf | is.nan(x), 0, log(x))

scaleFUN <- function(x) formatC(signif(x, digits=1), digits=1, format="fg", flag="#")

#' Binned scatter of a firm-quarter outcome against the s-index.
#'
#' Every binscatter in this script goes through here, so the panels cannot drift
#' apart on binning rule or theme -- which is how one of them ended up on a
#' quantile grid while the other two used equal-width bins.
#'
#' @param breaks Bin edges. Pass s_index_breaks for equal-width bins (an
#'   interpretable x-axis, but bin counts run from ~1,500 near zero to single
#'   digits in the sparse right tail) or s_index_qbreaks for equal-count bins
#'   (~225 salon-quarters each, at the cost of a bin width that varies ~10x).
#' @param facet Whether to facet by round_emps (the establishment-size groups).
binscatter_sindex <- function(dt, yvar, ylab_text, breaks, xlab_text,
                              text_size, facet = FALSE) {
  p <- ggplot(data = dt, aes(x = s_index, y = .data[[yvar]])) +
    geom_smooth(method = 'lm', color = "red", se = FALSE) +
    stat_summary_bin(fun = mean, breaks = breaks, geom = "point") +
    xlab(xlab_text) + ylab(ylab_text) +
    theme_bw() + theme(axis.text = element_text(size = text_size)) +
    theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
          panel.background = element_blank(), axis.line = element_line(colour = "black"))
  if (facet) {
    p <- p + scale_x_continuous(labels = scaleFUN, breaks = c(0, 0.5, 1)) +
      facet_wrap(~round_emps)
  }
  p
}

#' -----------------------------------------------------------------------------
#' LOAD AND PREPARE TASK DATA
#' -----------------------------------------------------------------------------

working <- data.table(readRDS(task_data_path))

## verify upstream cleaning was applied
stopifnot("county" %in% colnames(working))
stopifnot("quarter_year" %in% colnames(working))
stopifnot(nrow(working[duration == 0, ]) == 0)


#' -----------------------------------------------------------------------------
#' VISIT-LEVEL SPECIALIZATION AND CUSTOMER BEHAVIOR
#' -----------------------------------------------------------------------------

## measures of within visit specialization and whether staff was requested
setkey(working, location_id, customer_id, date, app_id)
visit_data<-working[, .(services_invisit=.N,staff_invisit=uniqueN(staff_id)),by=c("location_id","customer_id", "date", "quarter_year")]
visit_data[,multi_service:= services_invisit>1 ]
visit_data[,multi_staff:= staff_invisit>1]
visit_data<-visit_data[,.(multi_staff=sum(multi_staff), multi_service=sum(multi_service) ), by=c("location_id", "quarter_year")]
visit_data[multi_service>0, multi_rate:=multi_staff/multi_service]

## fraction of customers that are returners from a prior quarter.
setkey(working, location_id, customer_id, date, app_id)
working[, first_visit:=min(date), by=c("location_id", "customer_id")]
working[, last_visit:=max(date), by=c("location_id", "customer_id")]
working[, repeat_customer:= first_visit<quarter(date,type="date_first") ]
working[, pastrepeat_id:=  ifelse(repeat_customer, customer_id,NA)]
## fraction of customers in current quarter that come again in a future quarter.
working[, return_future:= last_visit>quarter(date,type="date_last") ]
working[, futurereturn_id:=  ifelse(return_future, customer_id,NA)]

## prebooking
working[prebooked==TRUE, prebook_app:=app_id ]
working[, prebook_date:=ifelse(prebooked, date,NA) ]

## was staff requested.
working[was_staff_requested==TRUE, staffreq_app:=app_id ]
working[, staffreq_date:=ifelse(was_staff_requested, date,NA) ]


## part of multi-establishment
working[, is_multi:= uniqueN(location_id)>1,by=c("business_id")]


#' -----------------------------------------------------------------------------
#' SERVICE DESCRIPTION TEXT ANALYSIS
#' -----------------------------------------------------------------------------

## number of misspellings
service_text<-unique(working[,"service_performed"])
service_text[, clean_text:=gsub("[^A-Za-z0-9 ]","",service_performed)]
service_text[, clean_text:=str_to_lower(clean_text)][,clean_text:=gsub("'s","",clean_text)][,clean_text:=gsub('[[:punct:]]+',' ',clean_text)][,clean_text:=gsub('[[:digit:]]+', '', clean_text)]
##nrow(text_to_clean)-uniqueN(text_to_clean$clean_task)

allcorpus<-dfm(tokens(service_text$clean_text))
top_words<-names(topfeatures(allcorpus,n=125))
top_words<-top_words[str_length(top_words)>3]

mistake<-Vectorize(function(x){
  interim<-tokens(x)
  interim_length<-str_length(interim)
  is_word<-is.word(interim)
  res<-stringdistmatrix(interim,top_words)
  res<-res/interim_length
  res[which(is_word),]<-0
  ## any time edit length is greater than 25%, exclude
  res[res>0.25]<-0
  return(sum(res)>0)
  
})

service_text[clean_text!="", is_mistake:=mistake(clean_text) ]

working<-merge(working, service_text[,c("service_performed", "is_mistake")], by="service_performed", all.x=TRUE)
working[, mistake_count:=ifelse(is_mistake, service_performed,NA)]


#' -----------------------------------------------------------------------------
#' CHAIR RENTERS
#' -----------------------------------------------------------------------------
chairrenter <- fread(chairrenter_path)
working<-merge(working,chairrenter, by=c("staff_id", "business_id"),all.x=TRUE)

working[!is.na(chair_renter), count_chairrenter:=ifelse(chair_renter, staff_id,NA)]
working[is.na(chair_renter),count_chairrenter:=NA ]



#' -----------------------------------------------------------------------------
#' TIPS DATA
#' -----------------------------------------------------------------------------

## tips - earliest date tipping function is used

top <- fread(tip_header_path)
files <- list.files(path = tip_dir, pattern = "*.csv", full.names = TRUE)
stopifnot(length(files) > 1)
tip_data1 <- rbindlist(lapply(files[!grepl("x00\\.csv$", basename(files))], fread))
names(tip_data1)<-names(top)
tip_data1<-rbind(tip_data1, top)
rm(top)
# only 5 obs with tips - clearly some issue with this.

tip_data2<-fread(tip_data2_path)

tip_data3<-fread(tip_data3_path)

setnames(tip_data3, "APPOINTMENT_ID", "app_id")
setnames(tip_data2, "appointment_id", "app_id")

# the one with customers is unique by customer within app:
stopifnot(uniqueN(tip_data3[, c("CUSTOMER_ID", "app_id")])==uniqueN(tip_data3[, c("app_id")]))
# there are multiple rows but they convey different amounts. so sum up within customer-app:
tip_data3<-tip_data3[,.(tot_amount=sum(AMOUNT), customer_id=unique(CUSTOMER_ID)), by=c("app_id")]

# the one with staff has 300,000ish obs where multiple staff tipped. cast by appointment.
tip_data2[, tot_amount:=sum(amount), by="app_id"]
setkey(tip_data2, "app_id", "staff_id")
tip_data2[, num_staff:=1:.N, by="app_id"]
wide_tip_data2<-dcast(tip_data2, app_id+tot_amount~num_staff, value.var=c("amount", "staff_id"))
# merge on the one-row-per-appointment cast (wide_tip_data2); merging on the long
# per-staff tip_data2 duplicates each appointment's app-level tot_amount across its
# staff rows, and the sum(tip_amount) by app_id below then multiplies the tip by the
# number of staff tip lines.
tip_merged<-merge(tip_data3, wide_tip_data2, all=TRUE, by="app_id")

# tips are in cents in one and dollars in the other
tip_merged[, tot_amount.x:=tot_amount.x*100]

# how much overlap?
nrow(tip_merged[(!is.na(tot_amount.x) & !is.na(tot_amount.y)),])/nrow(tip_merged)

# among overlapping, how many disagreements? only 6 out of nearly 3 million
tip_merged[, tip_discrep:=abs(tot_amount.x-tot_amount.y)>1e-08 & (!is.na(tot_amount.x) & !is.na(tot_amount.y))]
nrow(tip_merged[tip_discrep==TRUE,])
# look at disagreements
# View(tip_merged[tip_discrep==1,])
# for these, all are within a cent. go with the data stored in cents. (tot_amount.y)
tip_merged[, tip_amount:=tot_amount.x]
tip_merged[is.na(tip_amount), tip_amount:=tot_amount.y]
tip_merged[tip_discrep==TRUE, tip_amount:=tot_amount.y]
stopifnot(nrow(tip_merged[is.na(tip_amount)])==0)
# tip_date is derived from the transaction `date` below; the raw per-staff tip
# timestamp is not carried through (the wide cast keeps one row per appointment and
# the by-app collapse dropped tip_datetime anyway).
tip_merged<-tip_merged[,.( tip=sum(tip_amount,na.rm=TRUE)), by="app_id"]

working<-merge(working, tip_merged, by="app_id", all.x=TRUE)
working[, tip_date:=ifelse(is.na(tip),NA,date)]
working[, tip_percent:=ifelse(is.na(tip) | price<=0,NA,tip/price)]


#' -----------------------------------------------------------------------------
#' COMPILE FIRM-QUARTER PANEL
#' -----------------------------------------------------------------------------

working[, first_prebook:=min(prebook_date,na.rm=TRUE), by="location_id" ]
working[, first_staffreq:=min(staffreq_date,na.rm=TRUE), by="location_id" ]
working[,first_tip:=min(tip_date,na.rm=TRUE), by="location_id" ]
working[, first_observed:=min(date), by="location_id"]
firm_quarter<-working[,.(cust_count=uniqueN(customer_id),pastrepeat_rate=uniqueN(pastrepeat_id, na.rm=TRUE)/uniqueN(customer_id),
                         futurereturn_rate=uniqueN(futurereturn_id, na.rm=TRUE)/uniqueN(customer_id),
                         uniq_desc=uniqueN(service_performed),
                         mistake_rate=uniqueN(mistake_count, na.rm=TRUE)/uniqueN(service_performed),
                         avg_tip_percent=mean(tip_percent,na.rm=TRUE),
                         renter_rate=uniqueN(count_chairrenter, na.rm=TRUE)/uniqueN(staff_id),
                         app_count=uniqueN(app_id), prebook_rate=uniqueN(prebook_app, na.rm=TRUE)/uniqueN(app_id),
                         staffreq_rate=uniqueN(staffreq_app, na.rm=TRUE)/uniqueN(app_id),
                         emps=uniqueN(staff_id),rev_labor=sum(price)/sum(duration), revenue=sum(price)
                         ),by=c("location_id", "quarter_year", "first_tip", "first_prebook","first_staffreq", "is_multi","first_observed",
                                "location_state")]

firm_quarter<-merge(firm_quarter, visit_data,by=c("location_id", "quarter_year"), all.x=TRUE)   

## add s-index.
full_unsmoothed<-readRDS(staff_task_full_path)
full_unsmoothed<-unique(full_unsmoothed[,.SD, .SDcols=c(colnames(full_unsmoothed)[grep("^task_mix",colnames(full_unsmoothed))], "s_index", "location_id", "quarter_year", "county","location_zip")])
# Keep only matched firm-quarters so stale pre-transition rows from legacy-import
# histories do not survive through the merge when the task panel has been cleaned.
firm_quarter<-merge(full_unsmoothed, firm_quarter, by=c("location_id", "quarter_year"))

## exclude partial quarters (see CONFIG$excluded_quarters_analysis) BEFORE any
## standard deviations are computed. Otherwise the std_* variables below are
## scaled by an sd that still contains the partial quarter, which distorts the
## units of every standardized output once that quarter is dropped.
firm_quarter <- firm_quarter[!quarter_year %in% CONFIG$excluded_quarters_analysis, ]

firm_quarter[, std_sindex:=s_index/sd(s_index, na.rm=TRUE)]
firm_quarter[, std_uniq_desc:=uniq_desc/sd(uniq_desc, na.rm=TRUE)]
firm_quarter[, std_mistake:=mistake_rate/sd(mistake_rate, na.rm=TRUE)]
firm_quarter[, std_tip:=avg_tip_percent/sd(avg_tip_percent, na.rm=TRUE)]
firm_quarter[, std_renter:=renter_rate/sd(renter_rate, na.rm=TRUE)]
firm_quarter[, std_futurereturn:=futurereturn_rate/sd(futurereturn_rate, na.rm=TRUE)]
firm_quarter[, std_pastrepeat:=pastrepeat_rate/sd(pastrepeat_rate, na.rm=TRUE)]
firm_quarter[, std_prebook:=prebook_rate/sd(prebook_rate, na.rm=TRUE)]
firm_quarter[, std_staffreq:=staffreq_rate/sd(staffreq_rate, na.rm=TRUE)]
firm_quarter[,std_emps:=emps/sd(emps, na.rm=TRUE)]
firm_quarter[,std_rev_labor:=rev_labor/sd(rev_labor, na.rm=TRUE)]
firm_quarter[, rev_cust:=revenue/cust_count]
firm_quarter[, std_rev_cust:=rev_cust/sd(rev_cust, na.rm=TRUE)]
firm_quarter[, std_cust:=cust_count/sd(cust_count, na.rm=TRUE)]

#' -----------------------------------------------------------------------------
#' COUNTY AND ZIP CODE FIXES
#' -----------------------------------------------------------------------------

## one zip code not mapped to county
## manually code as orange county based on la times article: https://www.latimes.com/archives/la-xpm-1996-04-20-me-18603-story.html
firm_quarter[location_zip=='92681', county:='06059']
stopifnot(nrow(firm_quarter[!is.na(location_zip) & is.na(county)])==0)
## one missing zip code: set to be its own category for regressions
stopifnot(uniqueN(firm_quarter[is.na(location_zip)]$location_id)==1)
firm_quarter[is.na(location_zip),location_zip:= "-9999" ]
firm_quarter[location_zip=="-9999" ,county:= "-9999" ]

firm_data<-firm_quarter[,.(avg_sindex=mean(s_index), has_renter=max(renter_rate, na.rm=TRUE)>0, avg_tm_2=mean(task_mix_2),avg_tm_3=mean(task_mix_3),
                           avg_tm_4=mean(task_mix_4),avg_tm_5=mean(task_mix_5)),
                        by=c("location_id", "first_observed", "first_tip", "first_prebook", "first_staffreq","is_multi",
                             "county", "location_zip")]
firm_data[, first_observed:=date(first_observed)-min(date(first_observed))]
firm_data[, std_first:=as.numeric(first_observed/sd(first_observed))]
stopifnot(nrow(firm_data)==uniqueN(firm_data$location_id))

firm_data[, prebook_time:=as.numeric(as_date(first_prebook)-as_date(min(first_prebook,na.rm=TRUE) ))]
firm_data[is.finite(prebook_time), std_prebook_time:=prebook_time/sd(prebook_time)]
firm_data[, staffreq_time:=as.numeric(as_date(first_staffreq)-as_date(min(first_staffreq,na.rm=TRUE)))]
firm_data[is.finite(staffreq_time), std_staffreq_time:=staffreq_time/sd(staffreq_time)]

firm_data[, tip_time:=as.numeric(as_date(first_tip)-as_date(min(first_tip,na.rm=TRUE))) ]
firm_data[is.finite(tip_time), std_tip_time:=tip_time/sd(tip_time)]
firm_data[, std_sindex:=avg_sindex/sd(avg_sindex)]
firm_data[,uses_tip:=is.finite(first_tip)]
firm_data[,uses_prebook:=is.finite(first_prebook)]


## display number of single establishments vs multi.
firm_quarter<-merge(firm_quarter, unique(working[,c("business_id", "location_id")]), by="location_id", all.x=TRUE)
firm_quarter[, locs_bizid:=uniqueN(location_id), by="business_id"]
cat(sprintf("\nShare of business_ids that are single-establishment: %.4f\n",
            as.numeric(uniqueN(firm_quarter[locs_bizid==1,"business_id"])/uniqueN(firm_quarter[,"business_id"]))))

#' -----------------------------------------------------------------------------
#' PRODUCT DATA AND DISCOUNT SOPHISTICATION
#' -----------------------------------------------------------------------------

product_data1<-fread(product_data1_path)

product_data2<-fread(product_data2_path)
colnames(product_data1)<-str_to_lower(colnames(product_data1))
product_data2[,date:=date(ymd_hms(report_at_date)) ]
product_data1[,date:=date(ymd(report_at_date)) ]

colstokeep<-c("date", "location_id","product_name", "discount_amount")
product_data<-rbind(product_data1[,.SD, .SDcols=colstokeep], product_data2[,.SD, .SDcols=colstokeep], fill=TRUE)
product_data[, quarter_year:=year(date)+quarter(date)/10]
product_data<-unique(product_data[, c("location_id", "quarter_year", 
                                      "discount_amount", "product_name")])
product_data<-product_data[, .(uniq_discounts=.N, uniq_products=uniqueN(product_name)),
             by=c("quarter_year", "location_id")]

firm_quarter<-merge(firm_quarter, product_data, by=c("quarter_year", "location_id"), all.x=TRUE)

firm_quarter[, has_productdata:=!is.na(uniq_products) & uniq_products>0,]
firm_quarter[, std_uniq_discounts:=uniq_discounts/sd(uniq_discounts, na.rm=TRUE)]
firm_quarter[, std_multi_rate:=multi_rate/sd(multi_rate, na.rm=TRUE)]

saveRDS(firm_quarter, "results/data/02_stylized_facts_data.rds")

firm_quarter[,s_max:=-task_mix_1*spec_log(task_mix_1)-task_mix_2*spec_log(task_mix_2)-task_mix_3*spec_log(task_mix_3)-task_mix_4*spec_log(task_mix_4)-task_mix_5*spec_log(task_mix_5)]

#' -----------------------------------------------------------------------------
#' SUMMARY STATISTICS
#' -----------------------------------------------------------------------------

firm_stats <- firm_quarter[, c("revenue", "emps", "cust_count", "task_mix_1", "task_mix_2", "task_mix_3", "task_mix_4", "task_mix_5")]


names(firm_stats)<-c("Revenue","Employees","Customers",
                     "Share Haircut/Shave", "Share Color/Highlight/Wash", "Share Blowdry/Style/Treatment/Extensions",
                     "Share Administrative","Share Nail/Spa/Eye/Misc."
)
stargazer(firm_stats, header=FALSE, type='text')
stargazer(firm_stats, header=FALSE,digits=2, out='results/out/tables/02_summary_stats.tex',single.row = TRUE)




#' -----------------------------------------------------------------------------
#' FACT 1: DISPERSION IN SPECIALIZATION AND PRODUCTIVITY
#' -----------------------------------------------------------------------------
s_index_breaks<-seq(from=min(firm_quarter$s_index), to=max(firm_quarter$s_index), by=0.05)
# use equal spacing now.

# Equal-COUNT companion grid: every binscatter below is also drawn on these
# breaks, as a *_qbins.png variant, so the equal-width panels can be read
# against a version whose bins carry the same number of salon-quarters. The two
# rules trade off against each other and neither dominates: equal-width bins
# give an interpretable x-axis but their counts run from ~1,500 near zero down
# to single digits in the sparse right tail, so the rightmost points are noisy;
# equal-count bins hold that noise fixed at ~225 salon-quarters per point but
# their width varies ~10x, which compresses the right tail.
# unique() is required, not defensive: 5.4% of salon-quarters sit at exactly
# s_index == 0 (a salon whose staff all do the same task mix), so the 0% and 5%
# quantiles are both 0 and stat_summary_bin errors on duplicate breaks. The
# collapse leaves 19 bins rather than 20, and folds that zero mass into a first
# bin of ~450.
s_index_qbreaks<-unique(quantile(firm_quarter$s_index, seq(from=0, to=1, by=0.05)))

## syverson style facts: firm-quarter
res_fe<-feols(s_index~1|quarter_year+location_id, data=firm_quarter)

cat("\n===== FACT 1A: S-INDEX DISPERSION (firm-quarter) =====\n")
cat("s_index P10, P90:\n"); print(quantile(firm_quarter$s_index, c(0.1, 0.9)))
cat("s_index P20, P80:\n"); print(quantile(firm_quarter$s_index, c(0.2, 0.8)))
cat("s_index P5, P95:\n"); print(quantile(firm_quarter$s_index, c(0.05, 0.95)))
cat(sprintf("s_index P90/P10 ratio: %.3f\n",
            as.numeric(quantile(firm_quarter$s_index, 0.9)/quantile(firm_quarter$s_index, 0.1))))
cat(sprintf("s_index P75/P25 ratio (manuscript reports ~12.85): %.3f\n",
            as.numeric(quantile(firm_quarter$s_index, 0.75)/quantile(firm_quarter$s_index, 0.25))))
cat(sprintf("s_index raw SD: %.4f\n", sd(firm_quarter$s_index)))
cat(sprintf("s_index residual SD share after county+quarter FE w/ task mix: %.4f\n",
            sd(resid(feols(s_index~task_mix_2+task_mix_3+task_mix_4+task_mix_5|county+quarter_year, firm_quarter)))/sd(firm_quarter$s_index)))
## Size + county + quarter FE, no task mix.
cat(sprintf("s_index residual SD after emps+county+quarter FE (no task mix): %.4f\n",
            sd(resid(feols(s_index~1|emps+county+quarter_year, firm_quarter)))))
cat(sprintf("s_index residual SD share after emps+county+quarter FE (no task mix): %.4f\n",
            sd(resid(feols(s_index~1|emps+county+quarter_year, firm_quarter)))/sd(firm_quarter$s_index)))
## Manuscript spec: the paper residualizes the s-index on "the task mix,
## establishment-size fixed effects, county fixed effects and quarter fixed
## effects" -- the same four controls it uses for rev_labor in FACT 1B, so the
## two shares it reports side by side net out the same variation. Neither spec
## above does that: the first drops the size FE, the second drops the task mix.
## The task mix is not an incidental control here -- s_index is the worker-task
## mutual information, bounded above by the entropy of the task mix (s_max,
## computed above), so the mix mechanically caps how specialized a salon can be.
res_sindex_full <- feols(s_index~task_mix_2+task_mix_3+task_mix_4+task_mix_5|county+quarter_year+emps, firm_quarter)
cat(sprintf("s_index residual SD after county+quarter+emps FE w/ task mix (manuscript spec): %.4f\n",
            sd(resid(res_sindex_full))))
cat(sprintf("s_index residual SD share after county+quarter+emps FE w/ task mix (manuscript spec): %.4f\n",
            sd(resid(res_sindex_full))/sd(firm_quarter$s_index)))

cat("\n===== FACT 1B: REVENUE PER MINUTE DISPERSION (firm-quarter; Syverson comparison) =====\n")
cat(sprintf("rev_labor P75/P25 ratio: %.3f\n",
            as.numeric(quantile(firm_quarter$rev_labor, 0.75)/quantile(firm_quarter$rev_labor, 0.25))))
cat(sprintf("rev_labor P90/P10 ratio: %.3f\n",
            as.numeric(quantile(firm_quarter$rev_labor, 0.9)/quantile(firm_quarter$rev_labor, 0.1))))
cat(sprintf("rev_labor P95/P5 ratio:  %.3f\n",
            as.numeric(quantile(firm_quarter$rev_labor, 0.95)/quantile(firm_quarter$rev_labor, 0.05))))
cat(sprintf("rev_labor residual SD share after county+quarter+emps FE w/ task mix: %.4f\n",
            sd(resid(feols(rev_labor~task_mix_2+task_mix_3+task_mix_4+task_mix_5|county+quarter_year+emps, firm_quarter)))/sd(firm_quarter$rev_labor)))

cat("\n===== FACT 1C: FIRM-LEVEL S-INDEX DISPERSION (avg over quarters) =====\n")
temp<-firm_quarter[, .(avg_sindex=mean(s_index)), by=location_id]
cat("avg_sindex P10, P90:\n"); print(quantile(temp$avg_sindex, c(0.1, 0.9)))
cat("avg_sindex P20, P80:\n"); print(quantile(temp$avg_sindex, c(0.2, 0.8)))
cat("avg_sindex P5, P95:\n"); print(quantile(temp$avg_sindex, c(0.05, 0.95)))
cat(sprintf("avg_sindex P90/P10 ratio: %.3f\n",
            as.numeric(quantile(temp$avg_sindex, 0.9)/quantile(temp$avg_sindex, 0.1))))
cat(sprintf("avg_sindex P75/P25 ratio (manuscript reports ~10x at firm level): %.3f\n",
            as.numeric(quantile(temp$avg_sindex, 0.75)/quantile(temp$avg_sindex, 0.25))))

cat("\n===== FACT 1D: FIRM-LEVEL REV-PER-MINUTE DISPERSION (avg over quarters) =====\n")
temp<-firm_quarter[, .(avg_rev_labor=mean(rev_labor)), by=location_id]
cat("avg_rev_labor P10, P90:\n"); print(quantile(temp$avg_rev_labor, c(0.1, 0.9)))
cat("avg_rev_labor P20, P80:\n"); print(quantile(temp$avg_rev_labor, c(0.2, 0.8)))
cat("avg_rev_labor P5, P95:\n"); print(quantile(temp$avg_rev_labor, c(0.05, 0.95)))
cat(sprintf("avg_rev_labor P90/P10 ratio: %.3f\n",
            as.numeric(quantile(temp$avg_rev_labor, 0.9)/quantile(temp$avg_rev_labor, 0.1))))
cat(sprintf("avg_rev_labor P75/P25 ratio: %.3f\n",
            as.numeric(quantile(temp$avg_rev_labor, 0.75)/quantile(temp$avg_rev_labor, 0.25))))

temp<-firm_quarter[, .(sindex_p25=quantile(s_index, 0.25),
                       sindex_p75=quantile(s_index, 0.75),
                       rev_labor_p25=quantile(rev_labor, 0.25),
                       rev_labor_p75=quantile(rev_labor,0.75)), by=quarter_year]

ggplot(temp)+geom_line(aes(x=quarter_year, y=sindex_p25), color="blue")+geom_line(aes(x=quarter_year, y=sindex_p75), color="red")+theme_bw() + 
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),panel.background = element_blank(), axis.line = element_line(colour = "black"))

# firm level - 10 times more specialized.
firm_stats<-firm_quarter[,c("rev_labor","s_index")]


names(firm_stats)<-c("Labor Productivity","S-index")
stargazer(firm_stats,summary.stat=c("N","mean","min","p25", "median", "p75","max"), header=FALSE, type='text')
stargazer(firm_stats,summary.stat=c("N","mean","min","p25", "median", "p75","max"), header=FALSE,digits=2, out='results/out/tables/02_dispersion.tex',single.row = TRUE)

# the most productive quartile of firms are more than twice as specialized
cat("\n===== FACT 2: S-INDEX BY REVENUE-PER-MINUTE QUARTILE (manuscript: top quartile >2x as specialized) =====\n")
cat("s_index distribution among bottom rev_labor quartile (Q1):\n")
print(summary(firm_quarter[rev_labor<=quantile(firm_quarter$rev_labor, 0.25)]$s_index))
cat("s_index distribution among top rev_labor quartile (Q4):\n")
print(summary(firm_quarter[rev_labor>=quantile(firm_quarter$rev_labor, 0.75)]$s_index))
cat(sprintf("Ratio of top/bottom mean s_index: %.3f\n",
            mean(firm_quarter[rev_labor>=quantile(firm_quarter$rev_labor, 0.75)]$s_index) /
            mean(firm_quarter[rev_labor<=quantile(firm_quarter$rev_labor, 0.25)]$s_index)))

##histogram
ggplot(firm_quarter, aes(x=s_index)) +
  geom_histogram(color="black", fill="lightblue", size=0.5, bins = 40)+ ylab("Salon-Quarter Count") + xlab("Task Specialization")+ theme(legend.position = "none")+
theme_bw()+ theme(axis.text = element_text(size = 14))+
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),panel.background = element_blank(), axis.line = element_line(colour = "black"))

ggsave("results/out/figures/02_sindex_hist.png", width=4, height=4, units="in")

firm_quarter[, round_emps:=cut(emps, quantile(emps, seq(from=0, to=1, length=13)))]


ggplot(firm_quarter[emps>1,], aes(x=s_index)) +
  geom_histogram(color="black", fill="lightblue", size=0.5, bins = 20)+ ylab("Salon-Quarter Count") + xlab("Task Specialization")+ theme(legend.position = "none")+
  scale_x_continuous(breaks=c(0,0.5,1))+theme_bw() + theme(axis.text = element_text(size = 10))+
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),panel.background = element_blank(), axis.line = element_line(colour = "black"))+
facet_wrap(~ round_emps) 
ggsave("results/out/figures/02_sindex_hist_byemps.png", width=4, height=4, units="in")

## persistence of both - use ar(1) persistence with and without fixed effect
setorder(firm_quarter, "location_id", "quarter_year")
firm_quarter[, quarter_num:=(quarter_year-floor(quarter_year)-0.1)*10*0.25]
firm_quarter[, gap:=floor(quarter_year)+quarter_num-floor(shift(quarter_year))-shift(quarter_num), by="location_id"]
firm_quarter[,l_sindex:=shift(s_index) ,by="location_id"]
firm_quarter[,l_rev_labor:=shift(rev_labor) ,by="location_id"]
## print() explicitly: run_all.R source()s this script with print.eval left at
## its FALSE default, so a bare summary() at top level is evaluated and thrown
## away. The manuscript quotes these four AR(1) coefficients.
cat("\n===== FACT 1E: AR(1) PERSISTENCE (manuscript reports s_index 0.955 / 0.570, rev_labor 0.851 / 0.791) =====\n")
print(summary(feols(s_index~l_sindex, data=firm_quarter[round(gap,6)==0.25]), cluster=~location_id))
print(summary(feols(s_index~l_sindex|location_id, data=firm_quarter[round(gap,6)==0.25]), cluster=~location_id))
print(summary(feols(rev_labor~l_rev_labor, data=firm_quarter[round(gap,6)==0.25]), cluster=~location_id))
print(summary(feols(rev_labor~l_rev_labor|location_id, data=firm_quarter[round(gap,6)==0.25]), cluster=~location_id))



  ## fact 2 - productivity and s-index correlation.
  
  res0<-feols(std_rev_labor~std_sindex,cluster=~location_id, data=firm_quarter)
  res1<-feols(std_rev_labor~std_sindex+task_mix_2+task_mix_3+task_mix_4+task_mix_5,cluster=~location_id, data=firm_quarter)
  res2<-feols(std_rev_labor~std_sindex+task_mix_2+task_mix_3+task_mix_4+task_mix_5|emps,cluster=~location_id, data=firm_quarter)
  res3<-feols(std_rev_labor~std_sindex+task_mix_2+task_mix_3+task_mix_4+task_mix_5 | county+emps,cluster=~location_id, data=firm_quarter)
  res4<-feols(std_rev_labor~std_sindex+task_mix_2+task_mix_3+task_mix_4+task_mix_5 | quarter_year+county+emps,cluster=~location_id, data=firm_quarter)
  res5<-feols(std_rev_labor~std_sindex+task_mix_2+task_mix_3+task_mix_4+task_mix_5 | quarter_year+location_zip+emps,cluster=~location_id, data=firm_quarter)
  res6<-feols(std_rev_labor~std_sindex+task_mix_2+task_mix_3+task_mix_4+task_mix_5 | quarter_year+location_zip^emps,cluster=~location_id, data=firm_quarter)
  
  
  etable(res0, res1,res2,res3,res4,res5,res6, fitstat=~n+r2,keep="!Constant",dict=c(county="County",std_rev_labor = "Revenue per Minute (standardized)", std_sindex="S-Index", location_zip="Zip" ,quarter_year="Quarter-Year", emps="Firm Size",
                                                                    task_mix_2="Color Task Mix",task_mix_3="Blowdry Task Mix",
                                                                    task_mix_4="Admin. Task Mix", task_mix_5="Nail Task Mix", location_id="Establishment"),
         file="results/out/tables/02_productivity_sindex.tex", replace=TRUE,signifCode=c(`***`=0.001,`**`=0.01, `*`=0.05))
  

  
  ## do graphs unconditional and conditional on firm size.
  ## Each is drawn twice, once per binning rule (see s_index_qbreaks above).
  binscatter_sindex(firm_quarter, "rev_labor", "Revenue per Minute",
                    s_index_breaks, "Task-Specialization (S-Index)", 14)
  ggsave("results/out/figures/02_sindex_prod_all.png", width=4, height=4, units="in")

  binscatter_sindex(firm_quarter, "rev_labor", "Revenue per Minute",
                    s_index_qbreaks, "Task-Specialization (S-Index)", 14)
  ggsave("results/out/figures/02_sindex_prod_all_qbins.png", width=4, height=4, units="in")

  ## Binned on s_index_breaks, the same equal-width grid as the plot above. This
  ## previously binned on the P5-P95 quantiles of s_index, which both varied the
  ## bin width across the x-axis and silently dropped the top and bottom 5% of
  ## salon-quarters; the *_qbins variant below is the deliberate equal-count
  ## version, on a grid that covers the full support.
  binscatter_sindex(firm_quarter[!is.na(round_emps)], "rev_labor", "Revenue per Minute",
                    s_index_breaks, "Task Specialization", 10, facet = TRUE)
  ggsave("results/out/figures/02_sindex_prod_byemps.png", width=4, height=4, units="in")

  binscatter_sindex(firm_quarter[!is.na(round_emps)], "rev_labor", "Revenue per Minute",
                    s_index_qbreaks, "Task Specialization", 10, facet = TRUE)
  ggsave("results/out/figures/02_sindex_prod_byemps_qbins.png", width=4, height=4, units="in")
  
  
  # the most specialized quartile of firms on average generate $1.08 more revenue per minute
  # than least specialized quartile. this is 68% more productive.
  cat("\n===== FACT 2B: REVENUE-PER-MINUTE BY S-INDEX QUARTILE (manuscript: top quartile ~$1.08 higher, ~68% more productive) =====\n")
  cat("rev_labor distribution among bottom s_index quartile (Q1):\n")
  print(summary(firm_quarter[s_index<=quantile(firm_quarter$s_index, 0.25)]$rev_labor))
  cat("rev_labor distribution among top s_index quartile (Q4):\n")
  print(summary(firm_quarter[s_index>=quantile(firm_quarter$s_index, 0.75)]$rev_labor))
  cat(sprintf("Top - bottom mean rev_labor (dollars per minute): %.3f\n",
              mean(firm_quarter[s_index>=quantile(firm_quarter$s_index, 0.75)]$rev_labor) -
              mean(firm_quarter[s_index<=quantile(firm_quarter$s_index, 0.25)]$rev_labor)))
  cat(sprintf("Top/bottom mean rev_labor ratio (percent more productive): %.1f%%\n",
              100*(mean(firm_quarter[s_index>=quantile(firm_quarter$s_index, 0.75)]$rev_labor) /
                   mean(firm_quarter[s_index<=quantile(firm_quarter$s_index, 0.25)]$rev_labor) - 1)))
  
  
#' -----------------------------------------------------------------------------
#' DECOMPOSITION: REVENUE PER CUSTOMER VS CUSTOMER COUNT
#' -----------------------------------------------------------------------------

  res1<-feols(std_cust~std_sindex+task_mix_2+task_mix_3+task_mix_4+task_mix_5 | quarter_year+location_zip+emps,cluster=~location_id, data=firm_quarter)
  
  res2<-feols(std_rev_cust~std_sindex+task_mix_2+task_mix_3+task_mix_4+task_mix_5 | quarter_year+location_zip+emps,cluster=~location_id, data=firm_quarter)
  res3<-feols(std_futurereturn~std_sindex+task_mix_2+task_mix_3+task_mix_4+task_mix_5 | quarter_year+location_zip+emps,cluster=~location_id, data=firm_quarter)
  
  
  
  etable( res1,res2,res3, fitstat=~n+r2,keep="!Constant",dict=c(std_rev_cust = "Revenue per Customer", std_sindex="S-Index", location_zip="Zip" ,quarter_year="Quarter-Year", emps="Firm Size",
                                                                                          task_mix_2="Color Task Mix",task_mix_3="Blowdry Task Mix",
                                                                                          task_mix_4="Admin. Task Mix", task_mix_5="Nail Task Mix", location_id="Establishment",
                                                              std_cust="Customer Count", std_rev_cust="Rev. per Customer",
                                                              std_futurereturn="Customer Return Rate"
                                                              ),
         file="results/out/tables/02_rev_link_decomp.tex", replace=TRUE,signifCode=c(`***`=0.001,`**`=0.01, `*`=0.05))
  

#' -----------------------------------------------------------------------------
#' UNCORRELATED VARIANCE SHARE
#' -----------------------------------------------------------------------------

    cat("\n===== FACT 3: UNCORRELATED VARIANCE SHARE OF S-INDEX vs REV-PER-MINUTE =====\n")
    cat(sprintf("R^2 of s_index ~ emps FE (share of s_index explained by firm size): %.4f\n",
                as.numeric(r2(feols(s_index~1|emps, data=firm_quarter))['r2'])))

    uvs<-(r2(feols(std_rev_labor~std_sindex|emps, data=firm_quarter))['r2']-r2(feols(std_rev_labor~1|emps, data=firm_quarter))['r2'])
    cat(sprintf("UVS share (s_index contribution / total R^2 with s_index+emps FE): %.4f\n",
                as.numeric(uvs/r2(feols(std_rev_labor~std_sindex|emps, data=firm_quarter))['r2'])))

    ## partial r2
    cat(sprintf("Partial R^2 of s_index (uvs / (1 - R^2 of s_index+emps FE)): %.4f\n",
                as.numeric(uvs/(1-r2(feols(std_rev_labor~std_sindex|emps, data=firm_quarter))['r2']))))

    ## uvs for size.
    uvs_size<-(r2(feols(std_rev_labor~std_sindex|emps, data=firm_quarter))['r2']-r2(feols(std_rev_labor~s_index, data=firm_quarter))['r2'])
    cat(sprintf("Firm-size UVS share (emps contribution beyond s_index alone): %.4f\n",
                as.numeric(uvs_size/r2(feols(std_rev_labor~std_sindex|emps, data=firm_quarter))['r2'])))

    ## most aggressive uvs
    uvs_aggr<-(r2(feols(std_rev_labor~std_sindex+task_mix_2+task_mix_3+task_mix_4+task_mix_5 | county+emps, data=firm_quarter))['r2']-r2(feols(std_rev_labor~task_mix_2+task_mix_3+task_mix_4+task_mix_5 | county+emps, data=firm_quarter))['r2'])
    cat(sprintf("Aggressive UVS share (s_index beyond task_mix + county + emps FE): %.4f\n",
                as.numeric(uvs_aggr/r2(feols(std_rev_labor~std_sindex+task_mix_2+task_mix_3+task_mix_4+task_mix_5 | county+emps, data=firm_quarter))['r2'])))
    
#' -----------------------------------------------------------------------------
#' FACT 4: MANAGEMENT PRACTICES AND TEAMWORK
#' -----------------------------------------------------------------------------
  
  ## robust to using teamwork within visit specialization)
  
  res0<-feols(std_rev_labor~std_multi_rate,cluster=~location_id, data=firm_quarter)
  res1<-feols(std_rev_labor~std_multi_rate+task_mix_2+task_mix_3+task_mix_4+task_mix_5,cluster=~location_id, data=firm_quarter)
  res2<-feols(std_rev_labor~std_multi_rate+task_mix_2+task_mix_3+task_mix_4+task_mix_5 | location_zip,cluster=~location_id, data=firm_quarter)
  res3<-feols(std_rev_labor~std_multi_rate+task_mix_2+task_mix_3+task_mix_4+task_mix_5 | quarter_year+location_zip,cluster=~location_id, data=firm_quarter)
  res4<-feols(std_rev_labor~std_multi_rate+task_mix_2+task_mix_3+task_mix_4+task_mix_5 | quarter_year+location_zip+emps,cluster=~location_id, data=firm_quarter)
  res5<-feols(std_rev_labor~std_multi_rate+task_mix_2+task_mix_3+task_mix_4+task_mix_5 | quarter_year+location_zip^emps,cluster=~location_id, data=firm_quarter)
  
  
  
  etable(res0, res1,res2,res3,res4,res5, fitstat=~n+r2,keep="!Constant",dict=c(std_rev_labor = "Revenue per Minute", std_multi_rate="Teamwork", location_zip="Zip" ,quarter_year="Quarter-Year", emps="Firm Size",
                                                                             task_mix_2="Color Task Mix",task_mix_3="Blowdry Task Mix",
                                                                             task_mix_4="Admin. Task Mix", task_mix_5="Nail Task Mix", location_id="Establishment"),
         file="results/out/tables/02_productivity_teamwork.tex", replace=TRUE,signifCode=c(`***`=0.001,`**`=0.01, `*`=0.05))
  
  binscatter_sindex(firm_quarter, "multi_rate", "Teamwork",
                    s_index_breaks, "Task Specialization (S-Index)", 14)
  ggsave("results/out/figures/02_sindex_teamwork.png", width=4, height=4, units="in")

  binscatter_sindex(firm_quarter, "multi_rate", "Teamwork",
                    s_index_qbreaks, "Task Specialization (S-Index)", 14)
  ggsave("results/out/figures/02_sindex_teamwork_qbins.png", width=4, height=4, units="in")
  
  

  res0<-feols(std_multi_rate~std_sindex+task_mix_2+task_mix_3+task_mix_4+task_mix_5 | location_zip+quarter_year,cluster=~location_id, data=firm_quarter)
  res1<-feols(std_uniq_desc~std_sindex+task_mix_2+task_mix_3+task_mix_4+task_mix_5 | location_zip+quarter_year,cluster=~location_id, data=firm_quarter)
  res2<-feols(std_uniq_discounts~std_sindex+task_mix_2+task_mix_3+task_mix_4+task_mix_5 | location_zip+quarter_year,cluster=~location_id, data=firm_quarter)
  res3<-feols(std_first~std_sindex,cluster=~location_id, data=firm_data)
  res4<-feols(std_tip_time~std_sindex,cluster=~location_id, data=firm_data)

  res5<-feols(std_prebook_time~std_sindex,cluster=~location_id, data=firm_data)
  res6<-feols(std_staffreq_time~std_sindex,cluster=~location_id, data=firm_data)
  
  
  
  etable(res0, res1,res2,res3,res4,res5,res6, fitstat=~n+r2,keep="!Constant",dict=c(std_cust = "Customer Count", std_sindex="S-Index", location_zip="Zip" ,quarter_year="Quarter-Year", emps="Firm Size",
                                                                             task_mix_2="Color Task Mix",task_mix_3="Blowdry Task Mix",
                                                                             task_mix_4="Admin. Task Mix", task_mix_5="Nail Task Mix", location_id="Establishment",
                        std_multi_rate="Teamwork", std_uniq_desc="Service Descriptions",
                        std_uniq_discounts="Product Discounts",std_tip_time="Tip Feature",
                        std_prebook_time="Prebook Feature",std_staffreq_time="Request Feature",
                        std_first="Software Adopted"),
         file="results/out/tables/02_management_practices.tex", replace=TRUE,signifCode=c(`***`=0.001,`**`=0.01, `*`=0.05))
  

#' -----------------------------------------------------------------------------
#' ADDITIONAL AUXILIARY MEASURES
#' -----------------------------------------------------------------------------
  res0<-feols(std_mistake~std_sindex+task_mix_2+task_mix_3+task_mix_4+task_mix_5 | location_zip+quarter_year,cluster=~location_id, data=firm_quarter)
  res1<-feols(std_renter~std_sindex+task_mix_2+task_mix_3+task_mix_4+task_mix_5 | location_zip+quarter_year,cluster=~location_id, data=firm_quarter)
  res2<-feols(std_tip~std_sindex+task_mix_2+task_mix_3+task_mix_4+task_mix_5 | county+quarter_year,cluster=~location_id, data=firm_quarter[quarter_year>=year(as_date(first_tip))+quarter(as_date(first_tip))/10])
  res3<-feols(has_productdata~std_sindex+task_mix_2+task_mix_3+task_mix_4+task_mix_5 | location_zip+quarter_year,cluster=~location_id, data=firm_quarter)
  res4<-feols(has_renter~std_sindex|county,cluster=~location_id, data=firm_data)
  res5<-feols(uses_tip~std_sindex|county,cluster=~location_id, data=firm_data)

  etable(res0, res1,res2,res3,res4,res5, fitstat=~n+r2,keep="!Constant",dict=c(std_sindex="S-Index", location_zip="Zip" ,quarter_year="Quarter-Year", emps="Firm Size",
                                                                             task_mix_2="Color Task Mix",task_mix_3="Blowdry Task Mix",
                                                                             task_mix_4="Admin. Task Mix", task_mix_5="Nail Task Mix", location_id="Establishment",
                        std_mistake="Misspellings", std_renter="Chair Renters", std_tip="Tip Percent",
                        has_productdata="Has Product Data", has_renter="Has Chair Renter", uses_tip="Uses Tip Feature",
                        county="County"),
         file="results/out/tables/02_auxiliary_practices.tex", replace=TRUE,signifCode=c(`***`=0.001,`**`=0.01, `*`=0.05))


#' -----------------------------------------------------------------------------
#' CUSTOMER UTILIZATION OF FEATURES
#' -----------------------------------------------------------------------------
  res1<-feols(std_prebook~std_sindex+task_mix_2+task_mix_3+task_mix_4+task_mix_5 | location_zip+quarter_year,cluster=~location_id, data=firm_quarter[quarter_year>=year(as_date(first_prebook))+quarter(as_date(first_prebook))/10])
  res2<-feols(std_tip~std_sindex+task_mix_2+task_mix_3+task_mix_4+task_mix_5 | county+quarter_year,cluster=~location_id, data=firm_quarter[quarter_year>=year(as_date(first_tip))+quarter(as_date(first_tip))/10])
  
  res3<-feols(std_staffreq~std_sindex+task_mix_2+task_mix_3+task_mix_4+task_mix_5 | location_zip+quarter_year,cluster=~location_id, data=firm_quarter[quarter_year>=year(as_date(first_staffreq))+quarter(as_date(first_staffreq))/10])
  
  
  
  ## mediation analysis for staff req.
  
  cat("\n===== MEDIATION: S-INDEX -> STAFF-REQUEST RATE -> REVENUE PER MINUTE =====\n")
  model.M <- lm(std_staffreq ~ std_sindex, firm_quarter[quarter_year>=year(as_date(first_staffreq))+quarter(as_date(first_staffreq))/10])
  print(summary(model.M))
  model.Y <- lm(std_rev_labor ~ std_sindex+std_staffreq, firm_quarter[quarter_year>=year(as_date(first_staffreq))+quarter(as_date(first_staffreq))/10])
  print(summary(model.Y))

  results <- mediate(model.M, model.Y, treat='std_sindex', mediator='std_staffreq',
                     boot=TRUE, sims=500)
  print(summary(results))
  
  cat("\n===== POST-STAFFREQ-ADOPTION SAMPLE SUMMARIES =====\n")
  cat("s_index distribution among firm-quarters at/after first_staffreq:\n")
  print(summary(firm_quarter[quarter_year>=year(as_date(first_staffreq))+quarter(as_date(first_staffreq))/10]$s_index))
  cat("staffreq_rate distribution among firm-quarters at/after first_staffreq:\n")
  print(summary(firm_quarter[quarter_year>=year(as_date(first_staffreq))+quarter(as_date(first_staffreq))/10]$staffreq_rate))

sink()
close(console_log_con)
message("02: console output written to ", console_log_path)
message("02: complete")



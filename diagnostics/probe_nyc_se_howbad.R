## How bad are the NYC SEs if left as-is (raw sd over status==ok, as 08 does),
## and how do robust alternatives compare? READ-ONLY.
suppressPackageStartupMessages(library(data.table))
source("config.R")
files <- list.files("results/data/bootstrap_reps", pattern="^boot_res_.*\\.rds$", full.names=TRUE)
combined <- rbindlist(lapply(files, readRDS), fill=TRUE)
nonparm <- c("iteration","wage_convergence","price_convergence","status","error_message")
pcols <- setdiff(names(combined), nonparm)
for (c in pcols) if (!is.numeric(combined[[c]])) combined[, (c) := as.numeric(get(c))]
pt <- as.data.table(readRDS("results/data/06_parameters.rds")); point <- setNames(pt$coefficients, pt$parm_name)
ok <- combined[status=="ok"]

robust <- function(x){x<-x[is.finite(x)]; list(
  sd=sd(x), mad=1.4826*mad(x), tsd=sd(x[x>=quantile(x,.05)&x<=quantile(x,.95)]),
  iqr=IQR(x), p=length(x))}
signflip <- function(x,p){x<-x[is.finite(x)]; if(p>=0) mean(x<0) else mean(x>0)}
loo_drop <- function(x, iters, drop){ keep <- !(iters %in% drop); sd(x[keep], na.rm=TRUE)}

row <- function(cn, suf, lab, drop=integer(0)){
  c <- sprintf("factor(county)%s:%s", cn, suf); x <- ok[[c]]; p <- as.numeric(point[[c]]); r <- robust(x)
  data.table(param=lab, point=round(p,4), raw_SE=round(r$sd,3), t_raw=round(p/r$sd,2),
    robSE_MAD=round(r$mad,3), robSE_trim=round(r$tsd,3), t_rob=round(p/r$mad,2),
    SE_drop=if(length(drop)) round(loo_drop(x, ok$iteration, drop),3) else NA,
    pct_signflip=round(100*signflip(x,p)), IQR=round(r$iqr,1))
}
cat("============ NYC (36061) structural params: raw vs robust SE ============\n")
nyc <- rbindlist(list(
  row("36061","avg_labor:E_raw_2","wage E_raw_2", drop=109L),
  row("36061","avg_labor:E_raw_3","wage E_raw_3"),
  row("36061","avg_labor:E_raw_4","wage E_raw_4"),
  row("36061","avg_labor:E_raw_5","wage E_raw_5"),
  row("36061","cust_price","price rho", drop=198L)
))
print(nyc)
cat("\n(SE_drop: SE after removing the documented contaminating rep — iter109 wrong-basin for E_raw_2, iter198 rho outlier)\n")

cat("\n============ context: same table for Cook & LA wages + rho ============\n")
ctx <- rbindlist(lapply(c(Cook="17031", LA="6037"), function(cn) rbindlist(lapply(
  c("avg_labor:E_raw_2","avg_labor:E_raw_3","avg_labor:E_raw_4","avg_labor:E_raw_5","cust_price"),
  function(s) cbind(county=cn, row(cn, s, sub("avg_labor:","",s)))))), use.names=TRUE)
print(ctx[, .(county, param, point, raw_SE, t_raw, pct_signflip)])

cat("\n============ how many params are 'significant' (|t|>1.96 vs 0)? ============\n")
allcols <- function(cn) grep(sprintf("factor\\(county\\)%s:(avg_labor:(E_raw|B_raw)|cust_price)", cn), names(ok), value=TRUE)
for (nm in c(Cook="17031", NYC="36061", LA="6037")){
  cn <- nm; lab <- names(which(c(Cook="17031",NYC="36061",LA="6037")==cn))
  cs <- allcols(cn); tt <- sapply(cs, function(c){p<-as.numeric(point[[c]]); s<-sd(ok[[c]],na.rm=TRUE); p/s})
  cat(sprintf("  %s (%s): %d structural params, |t|>1.96: %d (%.0f%%);  median|t|=%.2f\n",
    lab, cn, length(tt), sum(abs(tt)>1.96,na.rm=TRUE), 100*mean(abs(tt)>1.96,na.rm=TRUE), median(abs(tt),na.rm=TRUE)))
}
cat("\nn ok reps =", nrow(ok), "\n")

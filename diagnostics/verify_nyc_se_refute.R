#!/usr/bin/env Rscript
# Independent re-computation & refutation of NYC SE claims.
suppressWarnings(suppressMessages({source("renv/activate.R"); library(data.table)}))
options(width=160)

repdir <- "results/data/bootstrap_reps"
files  <- list.files(repdir, pattern="^boot_res_.*\\.rds$", full.names=TRUE)
cat("N rep files on disk:", length(files), "\n")

# file mod times (completion order)
mt <- file.info(files)$mtime
ord <- order(mt)
files <- files[ord]; mt <- mt[ord]

# read all reps -> long matrix of params (coerce char->numeric)
read_one <- function(f){
  x <- readRDS(f)
  x <- as.data.frame(x)
  list(row=x)
}
reps <- lapply(files, function(f) as.data.frame(readRDS(f)))
nm <- colnames(reps[[1]])

book <- c("iteration","wage_convergence","price_convergence","status","error_message")
parmcols <- setdiff(nm, book)

# Build numeric matrix: rows=reps, cols=params
mat <- matrix(NA_real_, nrow=length(reps), ncol=length(parmcols),
              dimnames=list(NULL, parmcols))
status <- character(length(reps))
wconv  <- rep(NA, length(reps)); pconv <- rep(NA, length(reps))
iternum <- rep(NA_integer_, length(reps))
for(i in seq_along(reps)){
  r <- reps[[i]]
  status[i] <- as.character(r[["status"]])
  if("wage_convergence" %in% colnames(r)) wconv[i] <- suppressWarnings(as.numeric(r[["wage_convergence"]]))
  if("price_convergence" %in% colnames(r)) pconv[i] <- suppressWarnings(as.numeric(r[["price_convergence"]]))
  if("iteration" %in% colnames(r)) iternum[i] <- suppressWarnings(as.integer(r[["iteration"]]))
  for(cc in parmcols){
    v <- r[[cc]]
    mat[i,cc] <- suppressWarnings(as.numeric(as.character(v)))
  }
}
cat("status table:\n"); print(table(status))

# helper to grab a county's wage / B / rho columns
wcols <- function(fips) grep(paste0("factor\\(county\\)", fips, ":avg_labor:E_raw_"), parmcols, value=TRUE)
bcols <- function(fips) grep(paste0("factor\\(county\\)", fips, ":avg_labor:B_raw_"), parmcols, value=TRUE)
rcol  <- function(fips) grep(paste0("factor\\(county\\)", fips, ":cust_price$"), parmcols, value=TRUE)

cnty <- c(Cook="17031", NYC="36061", LA="6037")

# point estimates
p <- as.data.frame(readRDS("results/data/06_parameters.rds"))
pe <- setNames(p$coefficients, p$parm_name)

okidx <- which(status=="ok")
allidx <- which(status!="error")  # all non-error
cat("\nN ok:", length(okidx), " N non-error:", length(allidx), "\n\n")

relSE <- function(sdv, point){
  ifelse(abs(point) < 1e-8, NA, sdv/abs(point))
}

#### CLAIM 1: NYC wage SDs vs LA/Cook ####
cat("================ CLAIM 1: WAGE (E_raw) SDs by county ================\n")
for(set in list(ok=okidx, nonerror=allidx)){
  setname <- if(identical(set,okidx)) "OK-only" else "ALL-non-error"
  cat("\n--- subset:", setname, " (n=",length(set),") ---\n")
  for(nmc in names(cnty)){
    cs <- wcols(cnty[nmc])
    sds <- apply(mat[set,cs,drop=FALSE], 2, sd, na.rm=TRUE)
    pts <- pe[ sub("^factor", "factor", cs) ]
    pts <- pe[cs]
    rs  <- relSE(sds, pts)
    cat(sprintf("%-5s wage: SD range [%.1f, %.1f] median %.1f | relSE median %.2f (range %.2f-%.2f)\n",
        nmc, min(sds), max(sds), median(sds), median(rs,na.rm=TRUE), min(rs,na.rm=TRUE), max(rs,na.rm=TRUE)))
    cat("       SDs:", paste(sprintf("%.1f",sds), collapse=", "),
        "| pts:", paste(sprintf("%.1f",pts),collapse=", "),"\n")
  }
}

#### CLAIM 2: skill-B relSE and rho relSE ####
cat("\n================ CLAIM 2: skill-B relSE & rho relSE ================\n")
for(nmc in names(cnty)){
  cs <- bcols(cnty[nmc])
  sds <- apply(mat[okidx,cs,drop=FALSE],2,sd,na.rm=TRUE)
  pts <- pe[cs]
  rs  <- relSE(sds,pts)
  rc  <- rcol(cnty[nmc])
  rsd <- sd(mat[okidx,rc],na.rm=TRUE); rpt <- pe[rc]; rrel <- relSE(rsd,rpt)
  cat(sprintf("%-5s : skill-B relSE median %.2f (max %.2f) | rho point %.4f sd %.4f relSE %.2f\n",
      nmc, median(rs,na.rm=TRUE), max(rs,na.rm=TRUE), rpt, rsd, rrel))
}

#### CLAIM 3: bimodality of NYC wages ####
cat("\n================ CLAIM 3: NYC wage bimodality ================\n")
nyc_w <- mat[okidx, wcols("36061"), drop=FALSE]
cat("NYC wage columns:", paste(colnames(nyc_w),collapse=", "),"\n")
cat("\nQuantiles per wage param (OK reps):\n")
print(round(apply(nyc_w,2,quantile,probs=c(0,.1,.25,.5,.75,.9,1),na.rm=TRUE),1))
cat("\nskew/kurtosis-ish (mean,median,sd):\n")
print(round(rbind(mean=apply(nyc_w,2,mean,na.rm=TRUE),
                  median=apply(nyc_w,2,median,na.rm=TRUE),
                  sd=apply(nyc_w,2,sd,na.rm=TRUE)),1))
# kmeans k=2 on standardized wage vector
nyc_w_cc <- nyc_w[complete.cases(nyc_w),,drop=FALSE]
sc <- scale(nyc_w_cc)
set.seed(1)
km <- kmeans(sc, centers=2, nstart=25)
cat("\nkmeans k=2 cluster sizes:", paste(km$size,collapse=" / "),
    " -> high-basin fraction (smaller cluster):", round(min(km$size)/sum(km$size),3),"\n")
cat("cluster centers (orig scale):\n")
ctr <- aggregate(nyc_w_cc, by=list(cluster=km$cluster), FUN=mean)
print(round(ctr,1))
# also: how many reps have E_raw_3 (the big +165 param) below say 100 vs above
e3 <- nyc_w[,grep("E_raw_3", colnames(nyc_w))]
cat("\nE_raw_3 (point=164.6): n<100 =", sum(e3<100,na.rm=TRUE), " n>=500 =", sum(e3>=500,na.rm=TRUE),
    " of", sum(!is.na(e3)),"\n")
cat("E_raw_3 sorted:", paste(sprintf("%.0f",sort(e3)),collapse=" "),"\n")

#### CLAIM 4: does status==ok reduce SE? ####
cat("\n================ CLAIM 4: OK filter vs all-non-error SE ================\n")
for(nmc in names(cnty)){
  cs <- wcols(cnty[nmc])
  sd_ok  <- apply(mat[okidx,cs,drop=FALSE],2,sd,na.rm=TRUE)
  sd_all <- apply(mat[allidx,cs,drop=FALSE],2,sd,na.rm=TRUE)
  cat(sprintf("%-5s wage SD ok vs all: %s  |  %s\n", nmc,
      paste(sprintf("%.0f",sd_ok),collapse=","), paste(sprintf("%.0f",sd_all),collapse=",")))
}
cat("(if ok<=all consistently, filtering does NOT help)\n")

#### CLAIM 5: too few reps -- running SD by completion order + bootstrap-of-reps ####
cat("\n================ CLAIM 5: running SD (completion order) ================\n")
# focus columns: NYC wages + NYC rho
focus <- c(wcols("36061"), rcol("36061"))
# running SD over OK reps in completion(mtime) order
ok_in_order <- okidx  # okidx already in mtime order since files sorted by mtime
cat("OK reps in completion order, n=",length(ok_in_order),"\n")
runSD <- function(v){ sapply(seq_along(v), function(k) if(k<2) NA else sd(v[1:k],na.rm=TRUE)) }
for(cc in focus){
  v <- mat[ok_in_order, cc]
  rs <- runSD(v)
  # report SD at n=10,20,30,final and last-10-step change
  ns <- c(10,20,30,length(v))
  vals <- sapply(ns, function(n) if(n<=length(rs)) rs[n] else NA)
  finalSD <- rs[length(rs)]
  # pct change over last 10 reps
  if(length(rs)>=11){
    pc <- (rs[length(rs)] - rs[length(rs)-10]) / rs[length(rs)-10] *100
  } else pc <- NA
  cat(sprintf("%-50s running SD @n=%s: %s | last-10-rep change %.1f%%\n",
      sub("factor\\(county\\)36061:","",cc),
      paste(ns,collapse="/"), paste(sprintf("%.1f",vals),collapse="/"), pc))
}

cat("\n--- analytic se(sd) = sd/sqrt(2(n-1)) at current n and projected n ---\n")
n_now <- length(okidx)
for(cc in focus){
  s <- sd(mat[okidx,cc],na.rm=TRUE)
  se_now <- s/sqrt(2*(n_now-1))
  se_1100 <- s/sqrt(2*(1100*43/60-1))  # ok-rate ~43/60 of 1100
  se_1100b <- s/sqrt(2*(789-1))
  cat(sprintf("%-50s sd=%.1f  se(sd)@n=%d: %.1f (%.0f%%)  proj@n=789: %.1f (%.1f%%)\n",
      sub("factor\\(county\\)36061:","",cc), s, n_now, se_now, 100*se_now/s, se_1100b, 100*se_1100b/s))
}

cat("\n--- nonparametric bootstrap-of-the-reps: CI on the SE itself (B=2000) ---\n")
set.seed(42)
B <- 2000
for(cc in focus){
  v <- mat[okidx,cc]; v <- v[!is.na(v)]
  n <- length(v)
  bs <- replicate(B, sd(sample(v, n, replace=TRUE)))
  ci <- quantile(bs, c(.025,.5,.975))
  cat(sprintf("%-50s SE=%.1f  boot CI [%.1f, %.1f] (median %.1f) width/SE=%.2f\n",
      sub("factor\\(county\\)36061:","",cc), sd(v), ci[1], ci[3], ci[2], (ci[3]-ci[1])/sd(v)))
}

cat("\nDONE\n")

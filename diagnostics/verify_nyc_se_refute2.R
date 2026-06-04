#!/usr/bin/env Rscript
suppressWarnings(suppressMessages({source("renv/activate.R"); library(data.table)}))
options(width=170)
repdir <- "results/data/bootstrap_reps"
files  <- list.files(repdir, pattern="^boot_res_.*\\.rds$", full.names=TRUE)
mt <- file.info(files)$mtime
ord <- order(mt); files <- files[ord]; mt <- mt[ord]
reps <- lapply(files, function(f) as.data.frame(readRDS(f)))
nm <- colnames(reps[[1]])
book <- c("iteration","wage_convergence","price_convergence","status","error_message")
parmcols <- setdiff(nm, book)
mat <- matrix(NA_real_, nrow=length(reps), ncol=length(parmcols), dimnames=list(NULL,parmcols))
status <- character(length(reps)); iternum <- rep(NA_integer_,length(reps))
for(i in seq_along(reps)){ r<-reps[[i]]; status[i]<-as.character(r[["status"]])
  iternum[i]<-suppressWarnings(as.integer(r[["iteration"]]))
  for(cc in parmcols) mat[i,cc]<-suppressWarnings(as.numeric(as.character(r[[cc]]))) }
wcols <- function(fips) grep(paste0("factor\\(county\\)",fips,":avg_labor:E_raw_"),parmcols,value=TRUE)
rcol  <- function(fips) grep(paste0("factor\\(county\\)",fips,":cust_price$"),parmcols,value=TRUE)
okidx <- which(status=="ok")

cat("=== Completion order: iter# and mtime of OK reps ===\n")
fb <- basename(files)
df <- data.frame(pos=seq_along(files), file=fb, iter=iternum, status=status,
                 mtime=format(mt,"%m-%d %H:%M"))
print(df[df$status=="ok",], row.names=FALSE)

cat("\n=== Is iteration order == completion order? cor(pos, iter) ===\n")
cat("Spearman cor(completion pos, iteration#):", round(cor(seq_along(files), iternum, method="spearman"),3),"\n")
cat("(low/negative => on-disk subset is a biased fast-completing set, NOT first iterations)\n")

# Key wage param E_raw_3 (point 164.6) and E_raw_5
e3col <- wcols("36061")[2]; e5col <- wcols("36061")[4]
cat("\n=== When did the HIGH-BASIN reps arrive? (E_raw_3 > 500) ===\n")
ok_df <- data.frame(pos=okidx, iter=iternum[okidx], mtime=format(mt[okidx],"%m-%d %H:%M"),
                    E_raw_3=round(mat[okidx,e3col],0), E_raw_5=round(mat[okidx,e5col],0))
ok_df$high <- ok_df$E_raw_3>500
ok_df$ok_order <- seq_len(nrow(ok_df))
print(ok_df, row.names=FALSE)
cat("\nHigh-basin reps at ok-order positions:", paste(ok_df$ok_order[ok_df$high],collapse=","),
    "of",nrow(ok_df),"\n")
cat("Mean ok-order pos of high vs low:", round(mean(ok_df$ok_order[ok_df$high]),1),"vs",
    round(mean(ok_df$ok_order[!ok_df$high]),1),"\n")

cat("\n=== SENSITIVITY: SD with vs without high-basin reps (trim test) ===\n")
focus <- c(wcols("36061"), rcol("36061"))
lowidx <- okidx[!ok_df$high]   # only low-basin
for(cc in focus){
  full <- sd(mat[okidx,cc],na.rm=TRUE)
  low  <- sd(mat[lowidx,cc],na.rm=TRUE)
  # trimmed 10% each tail
  v <- mat[okidx,cc]; v<-v[!is.na(v)]; q<-quantile(v,c(.1,.9)); tr<-sd(v[v>=q[1]&v<=q[2]])
  cat(sprintf("%-50s full SD %.1f | low-basin-only SD %.1f (%.0f%%) | 10%%-trimmed SD %.1f (%.0f%%)\n",
      sub("factor\\(county\\)36061:","",cc), full, low, 100*low/full, tr, 100*tr/full))
}
cat("(If SD collapses when dropping high-basin/tails => SE is driven by a FEW basin-switch reps,\n",
    " i.e. basin-switching, NOT a sample-size-will-fix-it problem. More reps add MORE such reps.)\n")

cat("\n=== Fraction of variance from top reps (NYC E_raw_5) ===\n")
for(cc in focus){
  v<-mat[okidx,cc]; v<-v[!is.na(v)]; m<-mean(v); dev2<-(v-m)^2
  o<-order(dev2,decreasing=TRUE)
  cat(sprintf("%-50s top-1 rep = %.0f%% of var; top-3 = %.0f%% of var\n",
      sub("factor\\(county\\)36061:","",cc), 100*dev2[o[1]]/sum(dev2), 100*sum(dev2[o[1:3]])/sum(dev2)))
}

cat("\n=== Project: what does the running SD look like over CUMULATIVE OK reps with bands? ===\n")
runSD <- function(v) sapply(seq_along(v), function(k) if(k<2) NA else sd(v[1:k],na.rm=TRUE))
for(cc in c(e3col,e5col,rcol("36061"))){
  v <- mat[okidx,cc]; rs<-runSD(v)
  cat(sub("factor\\(county\\)36061:","",cc),"running SD every 5:\n  ")
  idx<-seq(5,length(rs),by=5)
  cat(paste(sprintf("n%d=%.1f",idx,rs[idx]),collapse="  "),"\n")
}
cat("\nDONE2\n")

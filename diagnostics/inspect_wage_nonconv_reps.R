## Investigate why bootstrap reps are still wage_nonconverged under the new
## (weak-inequality) gate. For each .rds in bootstrap_reps/, print iteration,
## status, wage_convergence, price_convergence, error_message (if any).
files <- list.files("results/data/bootstrap_reps", pattern="\\.rds$", full.names=TRUE)
for (f in files) {
  x <- tryCatch(readRDS(f), error=function(e) NULL)
  if (is.null(x)) next
  st <- as.character(x$status[[1]])
  if (st == "ok") next
  cat("file=", basename(f),
      " status=", st,
      " wage_conv=", as.character(x$wage_convergence[[1]]),
      " price_conv=", as.character(x$price_convergence[[1]]),
      "\n  iter=", as.character(x$iteration[[1]]),
      "\n  err=", as.character(x$error_message[[1]]),
      "\n", sep="")
}

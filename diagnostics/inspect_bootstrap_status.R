files <- list.files("results/data/bootstrap_reps", pattern="\\.rds$", full.names=TRUE)
s <- vapply(files, function(f) tryCatch(as.character(readRDS(f)$status[[1]]), error=function(e) "err"), character(1))
print(table(s, useNA="ifany"))

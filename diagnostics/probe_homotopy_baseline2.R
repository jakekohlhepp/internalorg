## Probe the apply_shock(0) mechanism directly for merger.
suppressPackageStartupMessages(library(data.table))
source("config.R")
source("utils/counterfactuals_core.R")

cf_script <- "17_counterfactual_merger.R"
env <- new.env(parent = globalenv())
exprs <- parse(cf_script)
.is_for <- function(e) is.call(e) && length(e) > 0 && as.character(e[[1]]) == "for"
.first_for <- which(vapply(seq_along(exprs),
                           function(i) .is_for(exprs[[i]]), logical(1)))[1]
for (i in seq_len(.first_for - 1L)) eval(exprs[[i]], envir = env)
env$cnty <- "6037"; env$qy <- 2021.2

wd <- env$working_data; od <- env$orig_data
cat("Before any apply_shock:\n")
cat("  identical(wd, env$working_data):", identical(wd, env$working_data), "\n")
cat("  data.table::address(env$working_data):", data.table::address(env$working_data), "\n")
cat("  data.table::address(wd):                ", data.table::address(wd), "\n")
cat("  names(wd):", paste(names(wd), collapse=", "), "\n")
cat("  wd weight LA (first 3):", paste(head(wd[county == "6037" & quarter_year == 2021.2, weight], 3), collapse=", "), "\n")
cat("  od weight LA (first 3):", paste(head(od[county == "6037" & quarter_year == 2021.2, weight], 3), collapse=", "), "\n")

wd[, orig_weight := od$weight]
cat("\nAfter wd[, orig_weight := od$weight]:\n")
cat("  names(wd):", paste(names(wd), collapse=", "), "\n")
cat("  wd orig_weight LA (first 3):", paste(head(wd[county == "6037" & quarter_year == 2021.2, orig_weight], 3), collapse=", "), "\n")
cat("  wd weight LA      (first 3):", paste(head(wd[county == "6037" & quarter_year == 2021.2, weight], 3), collapse=", "), "\n")

## Apply shock manually as if lam=0
wd[, weight := orig_weight * (1 - 0 / 2)]
cat("\nAfter wd[, weight := orig_weight * 1]:\n")
cat("  wd weight LA      (first 3):", paste(head(wd[county == "6037" & quarter_year == 2021.2, weight], 3), collapse=", "), "\n")
cat("  env$working_data weight LA (first 3):", paste(head(env$working_data[county == "6037" & quarter_year == 2021.2, weight], 3), collapse=", "), "\n")

## Now do it via a function (closure capture)
wd[, weight := orig_weight / 2]  ## reset to halved
cat("\nReset wd weight to orig/2 again (first 3):",
    paste(head(wd[county == "6037" & quarter_year == 2021.2, weight], 3), collapse=", "), "\n")

apply_shock_fn <- function(lam) {
  wd[, weight := orig_weight * (1 - lam / 2)]
}
apply_shock_fn(0)
cat("After apply_shock_fn(0) (first 3):",
    paste(head(wd[county == "6037" & quarter_year == 2021.2, weight], 3), collapse=", "), "\n")

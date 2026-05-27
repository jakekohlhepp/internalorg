## Track data.table addresses through apply_shock to find where wd and
## env$working_data diverge.
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

addrs <- function(tag) {
  cat(sprintf("[%s] addr(env$working_data)=%s addr(wd)=%s identical=%s ncol=%d truelength=%d\n",
              tag, data.table::address(env$working_data), data.table::address(wd),
              identical(wd, env$working_data), ncol(wd), truelength(wd)))
}

wd <- env$working_data
od <- env$orig_data
addrs("after wd <- env$working_data")

wd[, orig_weight := od$weight]
addrs("after wd[, orig_weight := od$weight]")

wd[, weight := orig_weight * 1]
addrs("after wd[, weight := orig_weight * 1]")

cat("wd weight LA first 3:           ",
    paste(head(wd[county == "6037" & quarter_year == 2021.2, weight], 3), collapse=", "), "\n")
cat("env$working_data weight LA first 3:",
    paste(head(env$working_data[county == "6037" & quarter_year == 2021.2, weight], 3), collapse=", "), "\n")

## Re-bind wd to env$working_data and try again
wd <- env$working_data
addrs("after wd <- env$working_data (re-bind)")
cat("wd weight LA first 3:", paste(head(wd[county == "6037" & quarter_year == 2021.2, weight], 3), collapse=", "), "\n")

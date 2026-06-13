#' 1-D scans of the wage objective with and without the interior-share
#' penalty (config: wage_interior_penalty_*; docs/wage_interior_penalty_proposal.md).
#'
#' For the two priced-out coordinates (Cook w3, LA w5; see project memory
#' "priced-out wage coords"), holds all other wage parameters at the 06
#' estimates and traces, along a grid in the focal coordinate:
#'   - the county's raw 4-moment ssq (the current minimizer objective),
#'   - the interior penalty term,
#'   - the penalized total (what the minimizer would see with the option on),
#'   - the focal type's county-mean model share.
#' Demonstrates that the raw objective prefers the extinct plateau while the
#' penalized objective has an interior minimum inside the cliff window.
#'
#' Output: diagnostics/out/wage_interior_penalty_scan.rds (+ printed tables).
#' Runtime: ~25-30 min serial (24 county-restricted equilibrium passes).

suppressMessages(library(data.table))
source("config.R")
source("preamble.R")

scan_config <- CONFIG
scan_config$use_staged_solver_tolerances <- FALSE
scan_config$use_solver_warm_starts <- FALSE
scan_config$pl_on <- FALSE
scan_config$wage_interior_penalty_enabled <- TRUE

es <- readRDS(file.path(CONFIG$prep_output_dir, "04_estimation_sample.rds"))
working_data <- data.table(es$working_data)
estim_matrix <- es$estim_matrix
setup <- suppressWarnings(build_estimation_setup(working_data, estim_matrix, config = scan_config))
beta_hat <- setup$beta
beta_2 <- setup$beta_2
p06 <- as.data.table(readRDS("results/data/06_parameters.rds"))
wage_terms <- paste0(":avg_labor:E_raw_", 2:scan_config$n_worker_types, "$")
wage_idx <- Reduce(`|`, lapply(wage_terms, grepl, rownames(beta_2)))
wage_names <- rownames(beta_2)[wage_idx]
wr <- p06[parm_name %in% wage_names]
wage_hat <- setNames(wr$coefficients[match(wage_names, wr$parm_name)], wage_names)

scan_coord <- function(fips, coord_name, values) {
  sub <- estim_matrix[estim_matrix$county == fips, , drop = FALSE]
  obs_means <- wage_interior_obs_means(sub, scan_config)
  k_focal <- as.integer(sub("^.*E_raw_", "", coord_name))
  out <- rbindlist(lapply(values, function(v) {
    w <- wage_hat
    w[coord_name] <- v
    moments <- weighted_col_means(objective_gmm(
      theta = unname(w), x = sub, beta = beta_hat,
      beta_2_subset = w, config = scan_config
    ))
    cm <- grepl(paste0("county", fips, ":E_"), names(moments), fixed = TRUE)
    mv <- as.numeric(moments[cm])
    pen <- wage_interior_penalty_county(mv, obs_means, scan_config)
    shares <- mv + obs_means
    out_row <- data.table(
      value = v,
      raw_ssq = sum(mv^2),
      penalty = pen,
      total = sum(mv^2) +
        scan_config$wage_interior_penalty_weight * pen,
      share_focal = shares[k_focal - 1L],
      obs_focal = obs_means[k_focal - 1L]
    )
    out_row[, paste0("share_", 2:scan_config$n_worker_types) := as.list(shares)]
    out_row
  }))
  cat("\n== scan", fips, coord_name, "(obs share",
      round(out$obs_focal[1], 4), ") ==\n")
  print(out[, .(value, raw_ssq = signif(raw_ssq, 4), penalty = signif(penalty, 4),
                total = signif(total, 4), share_focal = signif(share_focal, 3))])
  out[, `:=`(county = fips, coord = coord_name)]
  out
}

la5 <- "factor(county)6037:avg_labor:E_raw_5"
ck3 <- "factor(county)17031:avg_labor:E_raw_3"
res <- rbind(
  scan_coord("6037", la5, c(754.1, 400, 300, 275, 250, 240, 230, 220, 210, 200, 175, 150)),
  scan_coord("17031", ck3, c(525.9, 300, 150, 100, 90, 80, 70, 60, 55, 50, 45, 40))
)
ensure_directory(file.path("diagnostics", "out"))
saveRDS(res, file.path("diagnostics", "out", "wage_interior_penalty_scan.rds"))
cat("\nSaved diagnostics/out/wage_interior_penalty_scan.rds\n")

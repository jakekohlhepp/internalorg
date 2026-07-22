# Unit tests for the wage_fallbacks helpers added in commit 6487528:
# - wage_fallback_format_county_par: per-county wage vector formatter for logs
# - wage_fallback_checkpoint_write: atomic checkpoint of the joint wage vector
#
# Both were introduced to make L1/L3/L4 accept paths recoverable after a
# 06_estimation.R timeout. Tests cover formatting, atomic writes via .tmp
# rename, the "none"/empty-path no-op, and error tolerance.

context("wage_fallbacks helpers")

PROJECT_ROOT <- if (file.exists("config.R")) "." else file.path("..", "..")
source(file.path(PROJECT_ROOT, "config.R"))
source(file.path(PROJECT_ROOT, "preamble.R"), chdir = TRUE)

test_that("wage_fallback_format_county_par formats a 4-vector with comma-separated 3-decimal values", {
  result <- wage_fallback_format_county_par(c(-22.456, 498.123, 150.9, 1249.99))
  expect_equal(result, "-22.456, 498.123, 150.900, 1249.990")
})

test_that("wage_fallback_format_county_par strips names from the input", {
  v <- c(E_2 = 1.5, E_3 = -2.5, E_4 = 3, E_5 = 4)
  expect_equal(wage_fallback_format_county_par(v), "1.500, -2.500, 3.000, 4.000")
})

test_that("wage_fallback_format_county_par handles single-element vector", {
  expect_equal(wage_fallback_format_county_par(3.14159), "3.142")
})

test_that("wage_fallback_format_county_par handles arbitrary-length vectors", {
  expect_equal(wage_fallback_format_county_par(c(1, 2, 3, 4, 5, 6)),
               "1.000, 2.000, 3.000, 4.000, 5.000, 6.000")
})

test_that("wage_fallback_checkpoint_write writes a readable rds with par/label/timestamp", {
  tmp_path <- tempfile(fileext = ".rds")
  on.exit(unlink(tmp_path), add = TRUE)
  config <- list(
    wage_fallback_checkpoint_path = tmp_path,
    wage_fallback_verbose = FALSE
  )
  par <- c("a:E_raw_2" = -22, "a:E_raw_3" = 498, "a:E_raw_4" = 150, "a:E_raw_5" = 122)
  wage_fallback_checkpoint_write(par, config, "test-L1-county-36061")

  expect_true(file.exists(tmp_path))
  loaded <- readRDS(tmp_path)
  expect_named(loaded, c("par", "label", "timestamp"), ignore.order = TRUE)
  expect_equal(loaded$par, par)
  expect_equal(loaded$label, "test-L1-county-36061")
  expect_s3_class(loaded$timestamp, "POSIXct")
})

test_that("wage_fallback_checkpoint_write uses atomic .tmp rename (no .tmp left behind)", {
  tmp_path <- tempfile(fileext = ".rds")
  on.exit(unlink(c(tmp_path, paste0(tmp_path, ".tmp"))), add = TRUE)
  config <- list(
    wage_fallback_checkpoint_path = tmp_path,
    wage_fallback_verbose = FALSE
  )
  wage_fallback_checkpoint_write(c(1, 2, 3), config, "atomic-test")
  expect_true(file.exists(tmp_path))
  expect_false(file.exists(paste0(tmp_path, ".tmp")))
})

test_that("wage_fallback_checkpoint_write overwrites prior checkpoint", {
  tmp_path <- tempfile(fileext = ".rds")
  on.exit(unlink(tmp_path), add = TRUE)
  config <- list(
    wage_fallback_checkpoint_path = tmp_path,
    wage_fallback_verbose = FALSE
  )
  wage_fallback_checkpoint_write(c(1, 2), config, "first")
  Sys.sleep(0.01)
  wage_fallback_checkpoint_write(c(99, 100), config, "second")

  loaded <- readRDS(tmp_path)
  expect_equal(loaded$par, c(99, 100))
  expect_equal(loaded$label, "second")
})

test_that("wage_fallback_checkpoint_write is a silent no-op when path is 'none'", {
  config <- list(
    wage_fallback_checkpoint_path = "none",
    wage_fallback_verbose = FALSE
  )
  ## Should not error and should not create any file.
  expect_silent(wage_fallback_checkpoint_write(c(1, 2, 3), config, "no-op"))
})

test_that("wage_fallback_checkpoint_write is a no-op when path is empty string", {
  config <- list(
    wage_fallback_checkpoint_path = "",
    wage_fallback_verbose = FALSE
  )
  expect_silent(wage_fallback_checkpoint_write(c(1, 2, 3), config, "empty"))
})

test_that("wage_fallback_checkpoint_write falls back to <prep_output_dir>/06_wage_fb_checkpoint.rds when checkpoint_path is NULL", {
  tmp_dir <- tempfile()
  dir.create(tmp_dir)
  on.exit(unlink(tmp_dir, recursive = TRUE), add = TRUE)
  config <- list(
    prep_output_dir = tmp_dir,
    wage_fallback_verbose = FALSE
    ## wage_fallback_checkpoint_path deliberately absent
  )
  wage_fallback_checkpoint_write(c(7, 8, 9), config, "fallback-path")
  default_path <- file.path(tmp_dir, "06_wage_fb_checkpoint.rds")
  expect_true(file.exists(default_path))
  expect_equal(readRDS(default_path)$par, c(7, 8, 9))
})

test_that("wage_fallback_checkpoint_write does not raise on unwritable path", {
  ## Pointing into a non-existent directory; saveRDS should fail and the
  ## tryCatch should swallow the error rather than crashing the caller.
  config <- list(
    wage_fallback_checkpoint_path = "/nonexistent_dir_xyz/cp.rds",
    wage_fallback_verbose = FALSE
  )
  expect_error(wage_fallback_checkpoint_write(c(1, 2), config, "bad-path"),
               NA)
})

# ---------------------------------------------------------------------------
# Layer 0 short circuit (added after 59610108 burned its 2-day wall running the
# full ladder on three counties that were already at 6.2e-16 / 5.5e-15 / 5.0e-12).
# ---------------------------------------------------------------------------

test_that("wage_fallback_converged_ssq_tol defaults to 1e-8", {
  expect_equal(CONFIG$wage_fallback_converged_ssq_tol, 1e-8)
})

# Drives apply_wage_fallback_layers with a stubbed slice objective: county
# 17031 sits at machine-zero, county 6037 does not.
local_ladder_fixture <- function(ssq_by_county, tol) {
  par_names <- unlist(lapply(names(ssq_by_county), function(c)
    paste0("factor(county)", c, ":avg_labor:E_raw_", 2:5)))
  result <- list(par = setNames(rep(1, length(par_names)), par_names))
  x <- data.frame(county = names(ssq_by_county), stringsAsFactors = FALSE)

  # objective_gmm hands the county slice through; weighted_col_means turns it
  # into the named moment vector the slice objective greps for.
  assign("objective_gmm", function(theta, x, ...) x, envir = globalenv())
  assign("weighted_col_means", function(m, weights = NULL) {
    cnty <- as.character(m$county[1])
    setNames(sqrt(ssq_by_county[[cnty]]), paste0("county", cnty, ":E_raw_2"))
  }, envir = globalenv())

  config <- list(
    counties = names(ssq_by_county),
    wage_fallback_converged_ssq_tol = tol,
    wage_fallback_post_polish_enable = TRUE,
    wage_fallback_hessian_enable = FALSE,
    wage_fallback_multistart_enable = FALSE,
    wage_fallback_repso_max_iter = 0L,
    wage_fallback_verbose = FALSE,
    wage_fallback_checkpoint_path = "none"
  )
  apply_wage_fallback_layers(result, x, beta = NULL, beta_2_subset = NULL,
                             config = config, clust = NULL,
                             solver_state = NULL, moment_weights = NULL)
}

test_that("layer 0 skips a county already below the tolerance and runs the ladder otherwise", {
  orig <- mget(c("objective_gmm", "weighted_col_means"), envir = globalenv(),
               ifnotfound = list(NULL, NULL))
  on.exit({
    for (nm in names(orig)) {
      if (is.null(orig[[nm]])) rm(list = nm, envir = globalenv())
      else assign(nm, orig[[nm]], envir = globalenv())
    }
  }, add = TRUE)

  out <- local_ladder_fixture(list("17031" = 1e-20, "6037" = 4.0), tol = 1e-8)
  rep1 <- out$fallback_layers[[1]]

  # Converged county: short-circuited before layer 1.
  expect_true(rep1[["17031"]]$layer0$skipped_ladder)
  expect_equal(rep1[["17031"]]$layer0$ssq, 1e-20)
  expect_null(rep1[["17031"]]$layer1)

  # Non-converged county: layer 0 records it but lets the ladder proceed.
  expect_false(rep1[["6037"]]$layer0$skipped_ladder)
  expect_false(is.null(rep1[["6037"]]$layer1))
})

test_that("setting the tolerance to 0 restores the always-run-the-ladder behaviour", {
  orig <- mget(c("objective_gmm", "weighted_col_means"), envir = globalenv(),
               ifnotfound = list(NULL, NULL))
  on.exit({
    for (nm in names(orig)) {
      if (is.null(orig[[nm]])) rm(list = nm, envir = globalenv())
      else assign(nm, orig[[nm]], envir = globalenv())
    }
  }, add = TRUE)

  out <- local_ladder_fixture(list("17031" = 1e-20), tol = 0)
  rep1 <- out$fallback_layers[[1]]
  expect_null(rep1[["17031"]]$layer0)
  expect_false(is.null(rep1[["17031"]]$layer1))
})

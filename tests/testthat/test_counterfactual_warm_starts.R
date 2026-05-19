# Unit tests for the smoke-derived warm-start wage table that 13_ optionally
# overrides its initial guess with.

context("Counterfactual warm-start wages")

PROJECT_ROOT <- if (file.exists("config.R")) "." else file.path("..", "..")
source(file.path(PROJECT_ROOT, "config.R"))
source(file.path(PROJECT_ROOT, "utils", "counterfactuals_core.R"), chdir = TRUE)
suppressPackageStartupMessages(library(data.table))

test_that("read_counterfactual_warm_starts returns NULL when the file is missing", {
  tmp_config <- modifyList(CONFIG, list(
    counterfactual_data_dir = tempfile("warmstart_missing_")
  ))
  dir.create(tmp_config$counterfactual_data_dir, recursive = TRUE)
  out <- read_counterfactual_warm_starts(tmp_config)
  expect_null(out)
})

test_that("counterfactual_warm_start_for round-trips a hand-rolled table", {
  warm <- data.table(
    county       = c("17031", "36061", "6037"),
    quarter_year = c(2021.2,  2021.2,  2021.2),
    w1 = c(40.293,  81.9985,  61.085),
    w2 = c(114.810, 65.7976,  79.494),
    w3 = c(109.010, 319.5385, 78.719),
    w4 = c(52.413,  159.225,  54.673),
    w5 = c(93.154,  211.708,  315.928)
  )
  setkey(warm, county, quarter_year)

  ## Match by character qy (mirrors how 13_ calls the helper via
  ## `as.character(qy)`), and by numeric qy directly.
  for (qy_in in list("2021.2", 2021.2)) {
    nyc <- counterfactual_warm_start_for(warm, "36061", qy_in)
    expect_equal(length(nyc), 5L)
    expect_equal(nyc, c(81.9985, 65.7976, 319.5385, 159.225, 211.708),
                 tolerance = 1e-9)

    la <- counterfactual_warm_start_for(warm, "6037", qy_in)
    expect_equal(la, c(61.085, 79.494, 78.719, 54.673, 315.928),
                 tolerance = 1e-9)
  }

  ## Unknown markets fall through to NULL.
  expect_null(counterfactual_warm_start_for(warm, "99999", "2021.2"))
  expect_null(counterfactual_warm_start_for(warm, "17031", "1999.4"))

  ## Empty / missing tables are treated as "no warm-start registered".
  expect_null(counterfactual_warm_start_for(NULL, "17031", "2021.2"))
  expect_null(counterfactual_warm_start_for(warm[0L], "17031", "2021.2"))
})

test_that("the shipped 13_warm_start_wages.rds round-trips for all three counties", {
  warm <- read_counterfactual_warm_starts()
  skip_if(is.null(warm),
          "13_warm_start_wages.rds not built yet (run compile_warm_start_wages.R)")
  expect_setequal(unique(warm$county), c("17031", "36061", "6037"))
  for (cnty in unique(warm$county)) {
    w <- counterfactual_warm_start_for(warm, cnty, "2021.2")
    expect_equal(length(w), 5L)
    expect_true(all(is.finite(w)))
    expect_true(all(w > 0))
  }
})

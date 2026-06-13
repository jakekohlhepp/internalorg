## ============================================================================
## Unit tests for the interior-share wage penalty (PROPOSAL, off by default).
## config: wage_interior_penalty_*; helpers in utils/structural_solver.R;
## design: docs/wage_interior_penalty_proposal.md.
## ============================================================================
library(testthat); library(data.table)
if (!file.exists("config.R")) setwd(normalizePath(file.path(getwd(), "..", "..")))
suppressWarnings(suppressMessages({ source("config.R"); source("preamble.R") }))

cfg <- CONFIG
cfg$n_worker_types <- 3L
cfg$wage_interior_penalty_enabled <- TRUE
cfg$wage_interior_penalty_weight <- 1
cfg$wage_interior_penalty_min_share <- 1e-3

test_that("county penalty enforces interiority only: zero above min_share, log^2 toward extinction", {
  obs <- c(0.20, 0.04)                       # observed county-mean shares, types 2..3
  ## model exactly matches observed -> no penalty
  expect_equal(wage_interior_penalty_county(c(0, 0), obs, cfg), 0)
  ## model FAR below observed but interior (1% vs 4% observed) -> still ZERO:
  ## the penalty never pushes a share toward the observed level
  expect_equal(wage_interior_penalty_county(c(0, 0.01 - 0.04), obs, cfg), 0)
  ## model at exactly min_share -> zero
  expect_equal(wage_interior_penalty_county(c(0, 1e-3 - 0.04), obs, cfg), 0)
  ## model above observed -> zero (one-sided)
  expect_equal(wage_interior_penalty_county(c(0.1, 0.1), obs, cfg), 0)
  ## just below min_share -> small positive
  p_small <- wage_interior_penalty_county(c(0, 9e-4 - 0.04), obs, cfg)
  expect_gt(p_small, 0)
  expect_lt(p_small, 0.05)
  ## extinction: share at numeric floor -> log(min_share/1e-16)^2, dominates fit scale
  p_ext <- wage_interior_penalty_county(c(0, 1e-16 - 0.04), obs, cfg)
  expect_equal(p_ext, log(1e-3 / 1e-16)^2, tolerance = 1e-6)
  expect_gt(p_ext, 800)
  ## monotone: lower share -> larger penalty
  shares <- c(9e-4, 1e-4, 1e-8, 1e-12)
  pens <- vapply(shares, function(s)
    wage_interior_penalty_county(c(0, s - 0.04), obs, cfg), numeric(1))
  expect_true(all(diff(pens) > 0))
  ## the floor is ABSOLUTE -- no cap at the observed share: a type observed
  ## below min_share is still held to min_share
  obs_small <- c(0.20, 5e-4)
  expect_gt(wage_interior_penalty_county(c(0, 0), obs_small, cfg), 0)       # model = obs = 5e-4 < 1e-3
  expect_equal(wage_interior_penalty_county(c(0, 1e-3 - 5e-4), obs_small, cfg), 0)
})

## toy two-county frame: county A has 3 rows, county B has 1 row
toy_x <- data.frame(
  county = c("A", "A", "A", "B"),
  E_raw_1 = c(0.5, 0.6, 0.7, 0.2),
  E_raw_2 = c(0.3, 0.3, 0.2, 0.5),
  E_raw_3 = c(0.2, 0.1, 0.1, 0.3)
)
cfg_toy <- cfg
cfg_toy$counties <- c("A", "B")

test_that("obs-means helper reproduces (weighted) county means", {
  expect_equal(wage_interior_obs_means(toy_x[toy_x$county == "A", ], cfg_toy),
               c(mean(c(0.3, 0.3, 0.2)), mean(c(0.2, 0.1, 0.1))))
  w <- c(2, 1, 1)
  expect_equal(
    wage_interior_obs_means(toy_x[toy_x$county == "A", ], cfg_toy, weights = w),
    c(weighted.mean(c(0.3, 0.3, 0.2), w), weighted.mean(c(0.2, 0.1, 0.1), w))
  )
})

test_that("joint-vector wrapper undoes the off-county dilution exactly", {
  ## model shares we want to encode: A = (0.25, 0.05), B = (0.10, 0.30)
  model_A <- c(0.25, 0.05); model_B <- c(0.10, 0.30)
  obs_A <- wage_interior_obs_means(toy_x[toy_x$county == "A", ], cfg_toy)
  obs_B <- wage_interior_obs_means(toy_x[toy_x$county == "B", ], cfg_toy)
  ## joint moment means over all 4 rows (off-county rows contribute zeros):
  ## m_ck = (model_ck - obs_ck) * n_c / N
  mv <- c(
    "countyA:E_2" = (model_A[1] - obs_A[1]) * 3 / 4,
    "countyB:E_2" = (model_B[1] - obs_B[1]) * 1 / 4,
    "countyA:E_3" = (model_A[2] - obs_A[2]) * 3 / 4,
    "countyB:E_3" = (model_B[2] - obs_B[2]) * 1 / 4
  )
  got <- wage_interior_penalty_terms(mv, toy_x, cfg_toy)
  want <- wage_interior_penalty_county(model_A - obs_A, obs_A, cfg_toy) +
    wage_interior_penalty_county(model_B - obs_B, obs_B, cfg_toy)
  expect_equal(got, want, tolerance = 1e-12)
  ## B's E_3 model share 0.30 = obs 0.30: fine; drive A's E_3 extinct:
  mv2 <- mv; mv2["countyA:E_3"] <- (1e-16 - obs_A[2]) * 3 / 4
  expect_gt(wage_interior_penalty_terms(mv2, toy_x, cfg_toy), 800)
})

test_that("unnamed vectors: single-county assumed in type order, multi-county skipped", {
  xa <- toy_x[toy_x$county == "A", ]
  obs_A <- wage_interior_obs_means(xa, cfg_toy)
  model_A <- c(0.25, 1e-16)
  unnamed <- unname(model_A - obs_A)
  expect_equal(wage_interior_penalty_terms(unnamed, xa, cfg_toy),
               wage_interior_penalty_county(model_A - obs_A, obs_A, cfg_toy))
  expect_warning(out <- wage_interior_penalty_terms(unname(c(0, 0, 0, 0)), toy_x, cfg_toy),
                 "unnamed multi-county")
  expect_equal(out, 0)
})

test_that("gate score adds weight * penalty only when the flag is on", {
  xa <- toy_x[toy_x$county == "A", ]
  obs_A <- wage_interior_obs_means(xa, cfg_toy)
  mv <- setNames(c(0.25, 1e-16) - obs_A, c("countyA:E_2", "countyA:E_3"))
  cfg_on <- cfg_toy; cfg_on$structural_bound_guard_enabled <- FALSE
  cfg_off <- cfg_on; cfg_off$wage_interior_penalty_enabled <- FALSE
  s_off <- structural_wage_objective_score(mv, theta = NULL, x = xa, beta = NULL,
                                           beta_2_subset = NULL, config = cfg_off)
  expect_equal(s_off, sum(mv^2))
  cfg_w <- cfg_on; cfg_w$wage_interior_penalty_weight <- 2.5
  s_on <- structural_wage_objective_score(mv, theta = NULL, x = xa, beta = NULL,
                                          beta_2_subset = NULL, config = cfg_w)
  expect_equal(s_on, sum(mv^2) +
                 2.5 * wage_interior_penalty_terms(mv, xa, cfg_w),
               tolerance = 1e-12)
})

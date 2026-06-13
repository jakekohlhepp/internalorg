## ============================================================================
## Validation of the Petrin-Train first-stage machinery (07_bootstrap.R).
##
## Reference (Petrin & Train, NBER WP 9452 (2003) / JMR (2010);
## docs/bootstrap_petrin_train.md):
##   - first stage: ANALYTICAL CR1 cluster-robust 2SLS vcov, clustered at
##     location_id; reported SEs are sqrt(diag(V)).
##   - second stage: each rep draws beta^(r) ~ N(beta_hat, V_clustered) and
##     re-estimates the wage/price stage; SEs are the across-rep SD.
##
## These tests exercise the ACTUAL functions parsed out of 07_bootstrap.R and
## the shared helpers in utils/estimation_pipeline.R, run on the real
## estimation sample and config (seed/reps), so they validate production.
## ============================================================================
library(testthat); library(data.table)
## testthat runs from tests/testthat/; hop to the project root so the relative
## source()/readRDS() paths below resolve.
if (!file.exists("config.R")) setwd(normalizePath(file.path(getwd(), "..", "..")))
suppressWarnings(suppressMessages({ source("config.R"); source("preamble.R") }))

## --- pull the real draw functions out of 07_bootstrap.R without running it ---
extract_defs <- function(path, names) {
  env <- new.env(parent = globalenv())
  for (e in as.list(parse(path))) {
    if (is.call(e) && identical(e[[1]], as.name("<-")) && is.name(e[[2]]) &&
        as.character(e[[2]]) %in% names) eval(e, env)
  }
  env
}
B <- extract_defs("07_bootstrap.R",
                  c("build_bootstrap_parameter_draws",
                    "load_or_create_parameter_draws",
                    "bootstrap_draw_beta"))

## ---------------------------------------------------------------------------
## Formula validation on synthetic data: cluster_vcov_2sls must reproduce
## sandwich::vcovCL(fit, cluster, type = "HC1") on an equivalent ivreg fit
## (the SE convention 05_iv_spec_comparison.R reports).
## ---------------------------------------------------------------------------
test_that("cluster_vcov_2sls == sandwich::vcovCL HC1 on an overidentified ivreg fit", {
  skip_if_not_installed("ivreg"); skip_if_not_installed("sandwich")
  set.seed(42)
  G <- 40L; n_per <- 8L; n <- G * n_per
  cl <- rep(seq_len(G), each = n_per)
  u_g <- rnorm(G)[cl]                                       # cluster shock
  z1 <- rnorm(n); z2 <- rnorm(n); x_exog <- rnorm(n)
  price <- 0.5 * z1 - 0.4 * z2 + 0.3 * x_exog + u_g / 2 + rnorm(n)
  y <- 1 - 0.8 * price + 0.6 * x_exog + u_g + rnorm(n)
  df <- data.frame(y, price, x_exog, z1, z2, cl)

  fit <- ivreg::ivreg(y ~ price + x_exog | x_exog + z1 + z2, data = df)
  X <- model.matrix(~ price + x_exog, df)
  Z <- model.matrix(~ x_exog + z1 + z2, df)
  beta <- rank_aware_2sls(X, Z, df$y)
  expect_equal(unname(beta[, 1]), unname(coef(fit)[colnames(X)]), tolerance = 1e-8)

  ours <- cluster_vcov_2sls(X, Z, df$y, beta, df$cl)
  ref <- sandwich::vcovCL(fit, cluster = df$cl, type = "HC1")
  expect_equal(unname(ours$vcov), unname(ref[colnames(X), colnames(X)]),
               tolerance = 1e-8)
  expect_equal(unname(ours$se),
               unname(sqrt(diag(ref))[colnames(X)]), tolerance = 1e-8)
  expect_equal(ours$n_clusters, G)
  expect_equal(ours$n_obs, n)
})

test_that("cluster_vcov_2sls guards degenerate inputs", {
  set.seed(7)
  n <- 60L
  z1 <- rnorm(n); x2 <- rnorm(n)
  z <- cbind(1, z1, x2); colnames(z) <- c("c", "z1", "x2")
  x <- cbind(1, 0.7 * z1 + rnorm(n), x2); colnames(x) <- c("c", "x1", "x2")
  y <- x %*% c(1, -1, 0.5) + rnorm(n)
  beta <- rank_aware_2sls(x, z, y)
  expect_error(cluster_vcov_2sls(x, z, y, beta, rep(1L, n)),
               "at least 2 clusters")
  ## 2 clusters < 3 parameters -> singular vcov warning
  expect_warning(cluster_vcov_2sls(x, z, y, beta, rep(1:2, length.out = n)),
                 "fewer clusters")
})

## ---------------------------------------------------------------------------
## Production first stage: real sample, real spec, cluster = location_id.
## ---------------------------------------------------------------------------
es <- readRDS(file.path(CONFIG$prep_output_dir, "04_estimation_sample.rds"))
wd <- data.table(es$working_data)
em <- es$estim_matrix
setup <- suppressWarnings(build_estimation_setup(wd, em, config = CONFIG))
beta_hat <- setup$beta
fs <- cluster_vcov_2sls(
  x = setup$mm_1, z = setup$z_mm_1, y = em[, "log_rel_mkt"],
  beta = beta_hat, cluster = em[, "location_id"],
  context = "demand IV clustered vcov"
)
reps <- CONFIG$bootstrap_reps

test_that("production clustered vcov is a valid covariance for the demand 2SLS", {
  expect_identical(rownames(fs$vcov), rownames(beta_hat))
  expect_identical(colnames(fs$vcov), rownames(beta_hat))
  expect_equal(fs$vcov, t(fs$vcov))                         # symmetric
  expect_true(all(is.finite(fs$se)) && all(fs$se >= 0))
  expect_gt(min(fs$se[grep("cust_price", names(fs$se))]), 0)  # rho SEs strictly positive
  expect_equal(fs$n_clusters, uniqueN(em[, "location_id"])) # clustered at location level
  expect_equal(fs$n_obs, nrow(em))
  ev <- eigen(fs$vcov, symmetric = TRUE, only.values = TRUE)$values
  expect_gt(min(ev), -1e-8 * max(ev))                       # PSD up to fp noise
})

test_that("first stage is centered on the production 06 demand estimates", {
  skip_if_not(file.exists("results/data/06_parameters.rds"))
  p06 <- as.data.table(readRDS("results/data/06_parameters.rds"))
  demand_06 <- p06[demand == TRUE]
  expect_equal(sort(rownames(beta_hat)), sort(demand_06$parm_name))
  expect_equal(as.numeric(beta_hat[demand_06$parm_name, 1]),
               demand_06$coefficients, tolerance = 1e-6)
})

test_that("production SEs cross-check against ivreg + vcovCL on shared coefficients", {
  skip_if_not_installed("ivreg"); skip_if_not_installed("sandwich")
  fit <- ivreg::ivreg(build_demand_iv_formula(as.data.frame(wd), CONFIG),
                      data = as.data.frame(wd))
  shared <- intersect(names(coef(fit))[!is.na(coef(fit))], rownames(beta_hat))
  expect_gt(length(shared), 50)
  expect_equal(unname(coef(fit)[shared]), unname(beta_hat[shared, 1]),
               tolerance = 1e-6)
  ref_se <- sqrt(diag(sandwich::vcovCL(fit, cluster = wd$location_id, type = "HC1")))
  expect_equal(unname(fs$se[shared]), unname(ref_se[shared]), tolerance = 1e-4)
})

## ---------------------------------------------------------------------------
## First-stage draws: the matrix every array task regenerates from the seed.
## ---------------------------------------------------------------------------
D <- B$build_bootstrap_parameter_draws(beta_hat, fs$vcov, CONFIG)

test_that("draw matrix covers every rep and every demand parameter", {
  expect_true(is.matrix(D))
  expect_equal(nrow(D), reps)
  expect_identical(colnames(D), rownames(beta_hat))
  expect_true(all(is.finite(D)))
})

test_that("draws are N(beta_hat, V): centered, with matching spread and correlation", {
  se_mc <- fs$se / sqrt(reps)
  off <- abs(colMeans(D) - as.numeric(beta_hat))
  expect_true(all(off <= 5 * se_mc + 1e-10))                # mean = beta_hat (MC error)
  live <- fs$se > 1e-8                                      # skip numerically-zero directions
  expect_true(all(abs(apply(D[, live], 2, sd) / fs$se[live] - 1) < 0.15))
  ## correlation structure: compare a handful of the largest off-diagonals
  cv <- fs$vcov[live, live]
  cr <- cov2cor(cv)
  big <- which(abs(cr) > 0.5 & row(cr) < col(cr), arr.ind = TRUE)
  if (nrow(big) > 0) {
    big <- big[seq_len(min(20L, nrow(big))), , drop = FALSE]
    emp <- cor(D[, live])
    expect_lt(max(abs(emp[big] - cr[big])), 0.15)
  }
})

test_that("generation is deterministic from the seed (concurrent array tasks agree)", {
  expect_identical(B$build_bootstrap_parameter_draws(beta_hat, fs$vcov, CONFIG), D)
  cfg2 <- CONFIG; cfg2$bootstrap_seed <- CONFIG$bootstrap_seed + 1L
  expect_false(isTRUE(all.equal(
    B$build_bootstrap_parameter_draws(beta_hat, fs$vcov, cfg2), D)))
  expect_equal(anyDuplicated(D), 0L)                        # reps are distinct draws
})

test_that("bootstrap_draw_beta returns a rank_aware_2sls-shaped 1-column matrix", {
  bd <- B$bootstrap_draw_beta(7L, D)
  expect_true(is.matrix(bd) && ncol(bd) == 1L)
  expect_identical(rownames(bd), rownames(beta_hat))
  expect_equal(as.numeric(bd), unname(D[7L, ]))
  ## downstream indexing patterns must behave like the 06 beta
  rho_idx <- grep("cust_price$", rownames(bd))
  expect_equal(length(rho_idx), length(CONFIG$counties))
  expect_true(all(is.finite(bd[rho_idx])))
  expect_error(B$bootstrap_draw_beta(0L, D), "No first-stage draw")
  expect_error(B$bootstrap_draw_beta(nrow(D) + 1L, D), "No first-stage draw")
})

test_that("load_or_create_parameter_draws persists, reloads, and invalidates stale files", {
  tmp <- tempfile(fileext = ".rds")
  on.exit(unlink(tmp), add = TRUE)
  got <- B$load_or_create_parameter_draws(beta_hat, fs$vcov, tmp, CONFIG, persist = TRUE)
  expect_identical(got, D)
  stored <- readRDS(tmp)
  expect_identical(stored$seed, CONFIG$bootstrap_seed)
  expect_identical(stored$draws, D)
  ## reload path returns the stored matrix
  expect_identical(B$load_or_create_parameter_draws(beta_hat, fs$vcov, tmp, CONFIG), D)
  ## a 06 re-run (shifted point estimates) must invalidate the cache
  beta_shift <- beta_hat + 1e-3
  expect_message(
    got2 <- B$load_or_create_parameter_draws(beta_shift, fs$vcov, tmp, CONFIG,
                                             persist = FALSE),
    "regenerating")
  expect_false(isTRUE(all.equal(got2, D)))
  ## persist = FALSE leaves the file untouched
  expect_identical(readRDS(tmp)$draws, D)
})

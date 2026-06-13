## ============================================================================
## Validation of the Murphy-Topel building blocks (07_vcov.R):
##   - cluster_vcov_2sls(return_scores = TRUE): per-cluster scores/influence
##     reproduce the sandwich exactly (vcov == adjustment * crossprod(influence))
##   - assemble_triangular_mt_vcov(): blockwise influence assembly equals the
##     brute-force joint sandwich J^{-1} S J^{-1}' of the stacked triangular
##     system on arbitrary (random) inputs.
## See docs/murphy_topel_proposal.md.
## ============================================================================
library(testthat); library(data.table)
if (!file.exists("config.R")) setwd(normalizePath(file.path(getwd(), "..", "..")))
suppressWarnings(suppressMessages({ source("config.R"); source("preamble.R") }))

test_that("return_scores: influence rows reproduce the clustered vcov exactly", {
  set.seed(99)
  G <- 25L; n_per <- 6L; n <- G * n_per
  cl <- rep(seq_len(G), each = n_per)
  u_g <- rnorm(G)[cl]
  z1 <- rnorm(n); x_exog <- rnorm(n)
  price <- 0.6 * z1 + 0.2 * x_exog + u_g / 2 + rnorm(n)
  y <- 2 - price + 0.5 * x_exog + u_g + rnorm(n)
  X <- cbind(1, price, x_exog); colnames(X) <- c("c", "price", "x_exog")
  Z <- cbind(1, z1, x_exog); colnames(Z) <- c("c", "z1", "x_exog")
  beta <- rank_aware_2sls(X, Z, y)

  fs <- cluster_vcov_2sls(X, Z, y, beta, cl, return_scores = TRUE)
  expect_equal(dim(fs$scores), c(G, ncol(X)))
  expect_identical(rownames(fs$scores), rownames(fs$influence))
  ## 2SLS FOC: scores sum to ~0 across clusters
  expect_lt(max(abs(colSums(fs$scores))), 1e-7 * max(abs(fs$scores)))
  ## sandwich identity: V = c * crossprod(A^{-1} s_g rows)
  expect_equal(unname(fs$vcov),
               unname(fs$adjustment * crossprod(fs$influence)),
               tolerance = 1e-10)
  ## default call returns no scores
  fs0 <- cluster_vcov_2sls(X, Z, y, beta, cl)
  expect_null(fs0$scores)
  expect_equal(fs0$vcov, fs$vcov)
})

test_that("triangular assembly equals the brute-force joint sandwich", {
  set.seed(123)
  G <- 30L; K1 <- 4L; K2 <- 3L; K3 <- 5L
  gnames <- paste0("g", seq_len(G))
  s1 <- matrix(rnorm(G * K1), G, dimnames = list(gnames, paste0("b", 1:K1)))
  s2 <- matrix(rnorm(G * K2), G, dimnames = list(gnames, paste0("w", 1:K2)))
  s3 <- matrix(rnorm(G * K3), G, dimnames = list(gnames, paste0("p", 1:K3)))
  A <- crossprod(matrix(rnorm(20 * K1), 20)) + diag(K1)
  J_2w <- matrix(rnorm(K2 * K2), K2) + 3 * diag(K2)   # well-conditioned, NOT symmetric
  J_2b <- matrix(rnorm(K2 * K1), K2)
  J_3w <- matrix(rnorm(K3 * K2), K3)
  J_3b <- matrix(rnorm(K3 * K1), K3)
  MtM <- crossprod(matrix(rnorm(20 * K3), 20)) + diag(K3)
  adjustment <- 1.37

  psi_b <- t(solve(A, t(s1)))
  dimnames(psi_b) <- list(gnames, paste0("b", 1:K1))
  mt <- assemble_triangular_mt_vcov(psi_b, s2, s3, J_2w, J_2b, J_3w, J_3b, MtM,
                                    adjustment = adjustment)

  ## brute force: theta_hat - theta = -J^{-1} sum_g s_g per cluster
  J <- rbind(
    cbind(-A, matrix(0, K1, K2), matrix(0, K1, K3)),
    cbind(J_2b, J_2w, matrix(0, K2, K3)),
    cbind(J_3b, J_3w, -MtM)
  )
  S <- crossprod(cbind(s1, s2, s3))
  Jinv <- solve(J)
  V_ref <- adjustment * Jinv %*% S %*% t(Jinv)

  expect_equal(unname(mt$vcov), unname(V_ref), tolerance = 1e-10)
  expect_equal(unname(mt$se), unname(sqrt(diag(V_ref))), tolerance = 1e-10)
  expect_identical(colnames(mt$vcov),
                   c(paste0("b", 1:K1), paste0("w", 1:K2), paste0("p", 1:K3)))
  ## beta block is untouched by the later stages (triangularity)
  expect_equal(unname(mt$vcov[1:K1, 1:K1]),
               unname(adjustment * crossprod(psi_b)), tolerance = 1e-10)
})

test_that("third-block sandwich is invariant to a fixed nonsingular transform (KKT system)", {
  ## 07c uses the gradient/KKT system h = [(M'M) gbar3]_free = 0 for the price
  ## block. With no binding bound that is a square nonsingular transform C of
  ## the plain moment system, and V must be unchanged: scores -> s3 C',
  ## J_3* -> C J_3*, J_3p = -MtM -> -C MtM.
  set.seed(7)
  G <- 30L; K1 <- 3L; K2 <- 2L; K3 <- 4L
  gnames <- paste0("g", seq_len(G))
  s1 <- matrix(rnorm(G * K1), G, dimnames = list(gnames, paste0("b", 1:K1)))
  s2 <- matrix(rnorm(G * K2), G, dimnames = list(gnames, paste0("w", 1:K2)))
  s3 <- matrix(rnorm(G * K3), G, dimnames = list(gnames, paste0("p", 1:K3)))
  A <- crossprod(matrix(rnorm(15 * K1), 15)) + diag(K1)
  J_2w <- matrix(rnorm(K2 * K2), K2) + 3 * diag(K2)
  J_2b <- matrix(rnorm(K2 * K1), K2)
  J_3w <- matrix(rnorm(K3 * K2), K3)
  J_3b <- matrix(rnorm(K3 * K1), K3)
  MtM <- crossprod(matrix(rnorm(15 * K3), 15)) + diag(K3)
  psi_b <- t(solve(A, t(s1)))
  dimnames(psi_b) <- list(gnames, paste0("b", 1:K1))

  v0 <- assemble_triangular_mt_vcov(psi_b, s2, s3, J_2w, J_2b, J_3w, J_3b, MtM)

  C <- MtM  # the actual transform 07c applies (square here: nothing binds)
  s3t <- s3 %*% t(C); colnames(s3t) <- colnames(s3)
  v1 <- assemble_triangular_mt_vcov(psi_b, s2, s3t,
                                    J_2w, J_2b, C %*% J_3w, C %*% J_3b, C %*% MtM)
  expect_equal(unname(v1$vcov), unname(v0$vcov), tolerance = 1e-9)
})

test_that("wage-block score transform with slack penalty (D = 0) leaves the sandwich unchanged", {
  ## 07c's penalized wage FOC reduces, when the penalty is slack, to
  ## transforming the wage equations by the fixed nonsingular J_2w':
  ## scores -> s2 %*% J_2w, J_2w -> J_2w' J_2w, J_2b -> J_2w' J_2b.
  set.seed(31)
  G <- 25L; K1 <- 3L; K2 <- 4L; K3 <- 3L
  gnames <- paste0("g", seq_len(G))
  s1 <- matrix(rnorm(G * K1), G, dimnames = list(gnames, paste0("b", 1:K1)))
  s2 <- matrix(rnorm(G * K2), G, dimnames = list(gnames, paste0("w", 1:K2)))
  s3 <- matrix(rnorm(G * K3), G, dimnames = list(gnames, paste0("p", 1:K3)))
  A <- crossprod(matrix(rnorm(15 * K1), 15)) + diag(K1)
  J_2w <- matrix(rnorm(K2 * K2), K2) + 3 * diag(K2)
  J_2b <- matrix(rnorm(K2 * K1), K2)
  J_3w <- matrix(rnorm(K3 * K2), K3)
  J_3b <- matrix(rnorm(K3 * K1), K3)
  MtM <- crossprod(matrix(rnorm(15 * K3), 15)) + diag(K3)
  psi_b <- t(solve(A, t(s1)))
  dimnames(psi_b) <- list(gnames, paste0("b", 1:K1))

  v0 <- assemble_triangular_mt_vcov(psi_b, s2, s3, J_2w, J_2b, J_3w, J_3b, MtM)
  s2h <- s2 %*% J_2w; colnames(s2h) <- colnames(s2)
  v1 <- assemble_triangular_mt_vcov(psi_b, s2h, s3,
                                    t(J_2w) %*% J_2w, t(J_2w) %*% J_2b,
                                    J_3w, J_3b, MtM)
  expect_equal(unname(v1$vcov), unname(v0$vcov), tolerance = 1e-9)
})

test_that("assembly validates cluster alignment and dimensions", {
  G <- 5L
  gn <- paste0("g", 1:G)
  psi_b <- matrix(0, G, 2, dimnames = list(gn, NULL))
  s2 <- matrix(0, G, 2, dimnames = list(rev(gn), NULL))
  s3 <- matrix(0, G, 2, dimnames = list(gn, NULL))
  expect_error(
    assemble_triangular_mt_vcov(psi_b, s2, s3,
                                diag(2), matrix(0, 2, 2),
                                matrix(0, 2, 2), matrix(0, 2, 2), diag(2)),
    "identical"
  )
})

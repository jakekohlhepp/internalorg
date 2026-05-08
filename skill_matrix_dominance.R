library("data.table")
library("stringr")

params <- as.data.table(readRDS(file.path("results", "data", "06_parameters.rds")))
skill <- params[str_detect(parm_name, "B_raw_[0-9]+_[0-9]+")]
skill[, county := str_extract(parm_name, "(?<=factor\\(county\\))[0-9]+")]
skill[, task        := as.integer(str_extract(parm_name, "(?<=B_raw_)[0-9]+"))]
skill[, worker_type := as.integer(str_sub(parm_name, -1L, -1L))]

county_label <- c(`17031` = "Chicago", `36061` = "Manhattan", `6037` = "Los Angeles")

build_mat <- function(cnty) {
  d <- skill[county == cnty]
  M <- matrix(NA_real_, 5, 5)
  for (i in seq_len(nrow(d))) M[d$task[i], d$worker_type[i]] <- d$coefficients[i]
  M
}

all_perms <- function(x) {
  if (length(x) == 1) return(matrix(x, 1, 1))
  out <- NULL
  for (i in seq_along(x)) {
    sub <- all_perms(x[-i])
    out <- rbind(out, cbind(x[i], sub, deparse.level = 0))
  }
  out
}

PERMS <- all_perms(1:5)

iso_l2 <- function(v) as.numeric(stats::isoreg(v)$yf)

proj_l2 <- function(M) apply(M, 2, iso_l2)

frob_dist <- function(M) {
  best <- Inf; best_pi <- NULL; best_fit <- NULL
  for (i in seq_len(nrow(PERMS))) {
    pi <- PERMS[i, ]
    Mp <- M[pi, , drop = FALSE]
    Mfit <- proj_l2(Mp)
    d <- sqrt(sum((Mp - Mfit)^2))
    if (d < best) { best <- d; best_pi <- pi; best_fit <- Mfit }
  }
  list(dist = best, perm = best_pi, fit = best_fit)
}

viol_l1 <- function(M) {
  v <- 0
  n <- nrow(M)
  for (i in 1:(n - 1)) for (j in (i + 1):n) {
    v <- v + sum(pmax(0, M[i, ] - M[j, ]))
  }
  v
}

l1_dist <- function(M) {
  best <- Inf; best_pi <- NULL
  for (i in seq_len(nrow(PERMS))) {
    pi <- PERMS[i, ]
    v <- viol_l1(M[pi, , drop = FALSE])
    if (v < best) { best <- v; best_pi <- pi }
  }
  list(viol = best, perm = best_pi)
}

report <- function(label, M) {
  cat("\n############################################################\n")
  cat("# ", label, "\n", sep = "")
  cat("############################################################\n")
  cat("Matrix:\n")
  print(round(M, 3))
  fnorm <- sqrt(sum(M^2))
  cat(sprintf("Frobenius norm of M: %.3f\n", fnorm))

  fr <- frob_dist(M)
  cat("\n[Frobenius / isotonic-L2 projection]\n")
  cat("  min ||M - M'||_F           : ", sprintf("%.4f", fr$dist), "\n")
  cat("  relative (||.||_F / ||M||_F): ", sprintf("%.4f", fr$dist / fnorm), "\n")
  cat("  best row permutation       : ", paste(fr$perm, collapse = " -> "), "\n")
  cat("  projected matrix (rows in best perm order, columns non-decreasing):\n")
  print(round(fr$fit, 3))

  v <- l1_dist(M)
  cat("\n[Sum of pairwise componentwise violations (L1)]\n")
  cat("  min total violation        : ", sprintf("%.4f", v$viol), "\n")
  cat("  best row permutation       : ", paste(v$perm, collapse = " -> "), "\n")
}

for (cnty in c("17031", "36061", "6037")) {
  M <- build_mat(cnty)
  report(paste0("County ", cnty, " — ", county_label[cnty],
                " (rows = tasks, cols = worker types)"), M)
  report(paste0("County ", cnty, " — ", county_label[cnty],
                " (rows = worker types, cols = tasks) [transposed]"), t(M))
}

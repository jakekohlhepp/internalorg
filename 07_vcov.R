#' =============================================================================
#' STEP 07c: Murphy-Topel Standard Errors for the Structural Stage
#' =============================================================================
#' Full two-step asymptotic variance (Murphy & Topel 1985; Newey & McFadden
#' 1994 6) for the wage/price parameters, treating the pipeline as a
#' block-triangular stacked GMM system clustered at location_id:
#'
#'   g1_i(beta)       2SLS normal equations  (111)   -> analytical A, scores
#'   g2_i(w; beta)    wage moments           (12)    -> objective_gmm rows
#'   g3_i(p; w,beta)  price moments          (120)   -> (p_adj - M p) * M
#'
#' Per-cluster influence functions are assembled blockwise
#' (assemble_triangular_mt_vcov in utils/estimation_pipeline.R); the result
#' contains the conditional second-stage variance, the first-stage propagation
#' term, and the same-sample cross-covariances. Design and caveats:
#' docs/murphy_topel_proposal.md.
#'
#' Inputs:
#'   - mkdata/data/04_estimation_sample.rds
#'   - results/data/06_parameters.rds
#'   - results/data/07_first_stage_vcov.rds  (cross-checked if present)
#'
#' Outputs:
#'   - results/data/07_first_stage_vcov.rds   (analytical clustered 2SLS demand
#'     vcov; this script is the producer, replacing the retired 07_bootstrap.R)
#'   - results/data/07_murphy_topel_vcov.rds
#'
#' Runtime: ~367 cold moment-evaluation passes (central differences over 12
#' wage + 78 relevant demand coordinates, 2 evals each, plus base/faithfulness
#' passes) at ~3-4 min per pass; the Jacobian passes parallelize across
#' JMP_MT_WORKERS. Submit per repo convention, e.g.:
#'   sbatch -p general -n 1 --cpus-per-task=12 --mem=16g -t 1-00:00:00 \
#'     -J 07_vcov -o logs/mt_%j.out \
#'     --wrap 'module load r/4.4.0; export JMP_MT_WORKERS=12 OMP_NUM_THREADS=1 \
#'             OPENBLAS_NUM_THREADS=1 MKL_NUM_THREADS=1; \
#'             Rscript -e "source(\"renv/activate.R\"); source(\"07_vcov.R\")"'
#' =============================================================================

library("data.table")

source("config.R")
source("preamble.R")

script_start <- Sys.time()

estimation_sample_path <- file.path(CONFIG$prep_output_dir, "04_estimation_sample.rds")
parameters_06_path <- file.path("results", "data", "06_parameters.rds")
first_stage_path <- file.path("results", "data", "07_first_stage_vcov.rds")
output_path <- file.path("results", "data", "07_murphy_topel_vcov.rds")
assert_required_files(c(estimation_sample_path, parameters_06_path))
ensure_directory(file.path("results", "data"))

if (!identical(CONFIG$skill_monotone_orientation, "none")) {
  stop("07c_murphy_topel.R requires skill_monotone_orientation = 'none'; ",
       "the 2SLS sandwich does not apply to the constrained demand estimator.")
}

## Fine, un-staged solver tolerances throughout: finite differences need the
## moment function to be as smooth as the inner solvers allow. Warm starts are
## OFF: a warm-started re-solve converges within tolerance but not
## bit-for-bit, which (i) breaks the g2-vs-objective_gmm faithfulness check,
## (ii) injects tolerance-level noise into the central differences, and
## (iii) makes the +h/-h evaluations asymmetric (the -h solve would warm-start
## from the +h solution). Cold solves are deterministic in their inputs, so
## the only finite-difference error left is truncation. Inner row-level
## parallelism is also off; parallelize across Jacobian columns with
## JMP_MT_WORKERS instead (whole passes are independent).
mt_config <- CONFIG
mt_config$use_staged_solver_tolerances <- FALSE
mt_config$use_solver_warm_starts <- FALSE
mt_config$pl_on <- FALSE

estimation_sample <- readRDS(estimation_sample_path)
working_data <- data.table(estimation_sample$working_data)
estim_matrix <- estimation_sample$estim_matrix
min_wage_levels <- data.table(estimation_sample$min_wage_levels)
n_w <- mt_config$n_worker_types

## ---------------------------------------------------------------------------
## Stage 1: 2SLS + clustered scores/influence (analytical).
## ---------------------------------------------------------------------------
setup <- build_estimation_setup(working_data, estim_matrix, config = mt_config)
beta_hat <- setup$beta
beta_2 <- setup$beta_2

first_stage <- cluster_vcov_2sls(
  x = setup$mm_1,
  z = setup$z_mm_1,
  y = estim_matrix[, "log_rel_mkt"],
  beta = beta_hat,
  cluster = estim_matrix[, "location_id"],
  context = "demand IV clustered vcov",
  return_scores = TRUE
)
rm(setup)

if (file.exists(first_stage_path)) {
  fs_file <- readRDS(first_stage_path)
  dev <- max(abs(fs_file$vcov - first_stage$vcov)) / max(abs(first_stage$vcov))
  message("Stage 1 vcov vs ", first_stage_path, ": max rel deviation ", signif(dev, 3))
  stopifnot(dev < 1e-8)
}

## This script is now the producer of 07_first_stage_vcov.rds (the analytical
## clustered 2SLS demand vcov), replacing the retired 07_bootstrap.R. 08 reads
## first_stage$se / cluster_variable / n_clusters for the demand-parameter SEs.
## Field names mirror the legacy bootstrap output for drop-in compatibility.
first_stage_out <- list(
  beta = beta_hat[colnames(first_stage$vcov), , drop = FALSE],
  vcov = first_stage$vcov,
  se = first_stage$se,
  cluster_variable = "location_id",
  n_clusters = first_stage$n_clusters,
  n_obs = first_stage$n_obs,
  rank = first_stage$rank,
  small_sample_adjustment = first_stage$adjustment,
  type = "analytical_clustered_2sls",
  estimator = "2sls"
)
saveRDS(first_stage_out, first_stage_path)
message("Wrote analytical first-stage 2SLS vcov to ", first_stage_path)

## ---------------------------------------------------------------------------
## Point estimates from 06 (wage in beta_2 row order; price in file order,
## which is the fit_price_parameters / lm coefficient order).
## ---------------------------------------------------------------------------
parameters_06 <- as.data.table(readRDS(parameters_06_path))
demand_06 <- parameters_06[demand == TRUE]
stopifnot(setequal(demand_06$parm_name, rownames(beta_hat)))
beta_dev <- max(abs(beta_hat[demand_06$parm_name, 1] - demand_06$coefficients))
message("Recomputed beta vs 06 file: max abs deviation ", signif(beta_dev, 3))
stopifnot(beta_dev < 1e-6 * (1 + max(abs(demand_06$coefficients))))

wage_terms <- paste0(":avg_labor:E_raw_", 2:n_w, "$")
wage_idx <- Reduce(`|`, lapply(wage_terms, grepl, rownames(beta_2)))
wage_names <- rownames(beta_2)[wage_idx]
wage_rows <- parameters_06[parm_name %in% wage_names]
stopifnot(nrow(wage_rows) == length(wage_names), length(wage_names) > 0)
wage_hat <- setNames(
  wage_rows$coefficients[match(wage_names, wage_rows$parm_name)],
  wage_names
)

price_rows <- parameters_06[demand == FALSE & !parm_name %in% wage_names]
p_hat <- setNames(price_rows$coefficients, price_rows$parm_name)
stopifnot(length(p_hat) > 0, all(is.finite(p_hat)), all(is.finite(wage_hat)))
message("Parameters: ", nrow(beta_hat), " demand, ", length(wage_hat),
        " wage, ", length(p_hat), " price.")

## ---------------------------------------------------------------------------
## Shared evaluation objects.
## ---------------------------------------------------------------------------
rows <- prepare_equilibrium_rows(estim_matrix, mt_config)
wd <- as.data.frame(working_data)
task_mix_formula_str <- build_task_mix_sum(mt_config)
xnam <- as.formula(paste0(
  "~avg_labor:factor(county):factor(quarter_year) + ",
  "factor(quarter_year):factor(county) + ",
  "factor(quarter_year):(", task_mix_formula_str, ") - 1"
))
M <- stats::model.matrix(xnam, data = wd)
stopifnot(nrow(M) == nrow(wd), ncol(M) == length(p_hat))
rho_lookup <- paste0("factor(county)", wd$county, ":cust_price")
e_raw_mat <- as.matrix(wd[, paste0("E_raw_", 2:n_w)])
wb_name_mat <- vapply(
  2:n_w,
  function(k) paste0("factor(county)", wd$county, ":avg_labor:E_raw_", k),
  character(nrow(wd))
)

solver_state <- get_default_solver_state()

## One equilibrium pass serving both moment blocks: g2 (wage) per-row moments
## exactly as objective_gmm builds them, and g3 (price) per-row moments
## (p_adj - M p_hat) * M with p_adj assembled as in add_price_adjustment.
## g3 is always evaluated at p_hat: its Jacobian in p is the analytic -M'M.
stage2_moment_rows <- function(beta_mat, wage_vec) {
  tild_theta <- build_cost_matrix(wage_vec, beta_mat, mt_config)
  solved <- solve_worker_rows(
    tild_theta, rows, mt_config,
    clust = NULL, state = solver_state,
    return_full_E = FALSE, return_gamma = TRUE
  )
  g2 <- build_worker_moment_matrix(
    solved$E - rows$observed_E, rows$county, mt_config,
    worker_indices = 2:n_w
  )
  wb_sum <- rowSums(
    matrix(wage_vec[wb_name_mat], nrow = nrow(wd)) * e_raw_mat
  ) * wd$avg_labor
  p_adj <- wd$cust_price - wb_sum - solved$gamma * wd$s_index +
    wd$mk_piece / beta_mat[rho_lookup, 1]
  g3 <- as.numeric(p_adj - M %*% p_hat) * M
  list(g2 = g2, g3 = g3)
}

message("Base moment evaluation at the point estimates...")
base_t0 <- Sys.time()
base <- stage2_moment_rows(beta_hat, wage_hat)
pass_secs <- as.numeric(difftime(Sys.time(), base_t0, units = "secs"))
message("  one pass: ", round(pass_secs, 1), "s")

## Faithfulness checks against the production code paths.
g2_ref <- objective_gmm(
  theta = unname(wage_hat), x = estim_matrix, beta = beta_hat,
  beta_2_subset = wage_hat, config = mt_config, solver_state = solver_state
)
stopifnot(max(abs(base$g2 - g2_ref)) < 1e-6)
padj_ref <- add_price_adjustment(
  working_data, estim_matrix, beta_hat, wage_hat, wage_hat,
  config = mt_config, clust = NULL, solver_state = solver_state
)$p_adj
## recover p_adj from the stored g3 (g3_ij = resid_i * M_ij, and every row has
## at least its county-quarter FE column, so rowSums(M^2) > 0)
resid_full <- rowSums(base$g3 * M) / rowSums(M * M)
padj_mine <- resid_full + as.numeric(M %*% p_hat)
padj_dev <- max(abs(padj_mine - padj_ref))
message("p_adj reconstruction vs add_price_adjustment: max abs dev ", signif(padj_dev, 3))
stopifnot(padj_dev < 1e-6 * (1 + max(abs(padj_ref))))

## FOC diagnostics (sample means of the estimating equations).
g2_mean <- colMeans(base$g2)
g3_mean <- colMeans(base$g3)
message("FOC norms: ||mean g2|| = ", signif(sqrt(sum(g2_mean^2)), 4),
        " (wage moment floor), max|mean g3| = ", signif(max(abs(g3_mean)), 4))

## ---------------------------------------------------------------------------
## Price-step lower bounds: coordinates of p_hat sitting on their min-wage
## bound have no interior FOC and are excluded from the p block (NA SE).
## Reconstructs fit_price_parameters' bound logic; the lm fit also verifies
## that the saved price-parameter order is the design-matrix column order.
## ---------------------------------------------------------------------------
first_try_formula <- as.formula(paste0(
  "p_adj ~ avg_labor:factor(county):factor(quarter_year) + ",
  "factor(county):factor(quarter_year) + ",
  "factor(quarter_year):(", task_mix_formula_str, ") - 1"
))
bound_data <- data.table(wd)
bound_data[, p_adj := padj_ref]
first_try <- stats::coef(stats::lm(first_try_formula, data = bound_data))
stopifnot(identical(names(first_try), names(p_hat)))

temp <- data.table(V1 = names(first_try), position = seq_along(first_try))
temp[, quarter_year := as.numeric(stringr::str_extract(V1, "\\b\\d{4}\\.\\d\\b"))][
  , county := as.numeric(gsub(
    "factor\\(county\\)", "",
    stringr::str_extract(V1, "factor\\(county\\)\\s*(\\d+)")
  ))]
temp <- temp[grep("^avg_labor", V1), ]
temp2 <- data.table(V1 = names(wage_hat), value = wage_hat)
temp2 <- rbind(
  temp2,
  data.table(
    V1 = unique(stringr::str_replace(names(wage_hat), "E_raw_[0-9]+", "E_raw_1")),
    value = 0
  )
)
temp2[, county := as.numeric(gsub(
  "factor\\(county\\)", "",
  stringr::str_extract(V1, "factor\\(county\\)\\s*(\\d+)")
))]
min_wage_bounds <- data.table::copy(min_wage_levels)
min_wage_bounds[, county := as.numeric(county)]
temp <- merge(temp2, temp, by = "county", allow.cartesian = TRUE)
temp <- merge(temp, min_wage_bounds, by = c("county", "quarter_year"), all.x = TRUE)
stopifnot(nrow(temp[is.na(min_wage)]) == 0)
temp_bounds <- temp[, .(lb = max(min_wage - value)), by = "position"]
lower_bound <- rep(-Inf, length(p_hat))
lower_bound[temp_bounds$position] <- temp_bounds$lb

binding <- which(is.finite(lower_bound) & (p_hat - lower_bound) < 1e-6)
free_p <- setdiff(seq_along(p_hat), binding)
if (length(binding) > 0L) {
  message("Price coordinates ON their lower bound (excluded from the p block, NA SE): ",
          paste(names(p_hat)[binding], collapse = ", "))
} else {
  message("No price coordinates bind their lower bound; full p block retained.")
}

## Price-block estimating equations: the KKT/gradient system the bounded
## L-BFGS-B step actually solves, h = [(M'M) gbar3]_free = 0. When a bound
## binds, gbar3 itself is NOT zero (the binding coordinate's moment leaks into
## the others through M'M; observed max|mean g3| can be O(1)), so expanding
## around gbar3_free = 0 would be the wrong system. When nothing binds, h is a
## fixed nonsingular transform of g3 and the sandwich is exactly invariant
## (unit-tested in test_murphy_topel.R). Per-row h contributions are
## C_free g3_i; the Jacobian in p_free is -[(M'M)(M'M)]_{free,free}.
MtM_full <- crossprod(M)
C_free <- MtM_full[free_p, , drop = FALSE]
MtM_h <- (MtM_full %*% MtM_full)[free_p, free_p]
kkt <- as.numeric(C_free %*% g3_mean)
message("Price KKT gradient on free coords: max|.| = ", signif(max(abs(kkt)), 4),
        " vs raw max|mean g3| = ", signif(max(abs(g3_mean)), 4),
        " (nonzero raw moments are expected when a bound binds)")

## ---------------------------------------------------------------------------
## Numerical Jacobian columns (central differences). beta: only the 78
## coordinates that enter stage 2 (skills + rho); the demand county-quarter
## FEs have identically zero columns -- asserted below with one spot check.
## ---------------------------------------------------------------------------
beta_relevant <- sort(unique(c(
  grep(":avg_labor:B_raw", rownames(beta_hat)),
  grep(":cust_price$", rownames(beta_hat))
)))
beta_irrelevant <- setdiff(seq_len(nrow(beta_hat)), beta_relevant)
message("Relevant beta coordinates: ", length(beta_relevant), " of ", nrow(beta_hat))

parscale_for <- function(name) {
  cnty <- stringr::str_extract(name, "(?<=factor\\(county\\))[0-9]+")
  override <- mt_config$min_optim_parscale_wage_by_county[[cnty]]
  if (is.null(override)) mt_config$min_optim_parscale_wage else override
}
h_wage <- mt_config$mt_step_scale * 1e-3 *
  pmax(abs(wage_hat), vapply(names(wage_hat), parscale_for, numeric(1)))
se_rel <- first_stage$se[beta_relevant]
stopifnot(all(se_rel > 0))
h_beta <- mt_config$mt_step_scale * 1e-2 * se_rel

jacobian_task <- function(kind, idx, h) {
  beta_p <- beta_hat; beta_m <- beta_hat
  wage_p <- wage_hat; wage_m <- wage_hat
  if (kind == "wage") {
    wage_p[idx] <- wage_p[idx] + h
    wage_m[idx] <- wage_m[idx] - h
  } else {
    beta_p[idx, 1] <- beta_p[idx, 1] + h
    beta_m[idx, 1] <- beta_m[idx, 1] - h
  }
  plus <- stage2_moment_rows(beta_p, wage_p)
  minus <- stage2_moment_rows(beta_m, wage_m)
  list(
    d_g2 = (colSums(plus$g2) - colSums(minus$g2)) / (2 * h),
    d_g3 = (colSums(plus$g3) - colSums(minus$g3)) / (2 * h)
  )
}

## Zero-column spot check: perturbing a demand county-quarter FE must leave
## the stage-2 moments untouched.
if (length(beta_irrelevant) > 0L) {
  spot <- jacobian_task("beta", beta_irrelevant[1L],
                        1e-2 * first_stage$se[beta_irrelevant[1L]])
  spot_max <- max(abs(c(spot$d_g2, spot$d_g3)))
  message("Zero-column spot check (", rownames(beta_hat)[beta_irrelevant[1L]],
          "): max |dg| = ", signif(spot_max, 3))
  stopifnot(spot_max < 1e-6)
}

tasks <- c(
  lapply(seq_along(wage_hat), function(j) list(kind = "wage", idx = j, h = h_wage[j])),
  lapply(seq_along(beta_relevant), function(j) {
    list(kind = "beta", idx = beta_relevant[j], h = h_beta[j])
  })
)
message("Computing ", length(tasks), " central-difference Jacobian columns (",
        2 * length(tasks), " passes, ~", round(pass_secs, 0), "s each, ",
        mt_config$mt_workers, " worker(s))...")

run_tasks <- function(task_list) {
  if (mt_config$mt_workers > 1L && !identical(get_os(), "windows")) {
    parallel::mclapply(task_list, function(tk) jacobian_task(tk$kind, tk$idx, tk$h),
                       mc.cores = mt_config$mt_workers, mc.preschedule = FALSE)
  } else {
    lapply(task_list, function(tk) jacobian_task(tk$kind, tk$idx, tk$h))
  }
}

if (isTRUE(mt_config$mt_smoke)) {
  message("JMP_MT_SMOKE=true: running one wage and one beta Jacobian column, then exiting.")
  smoke_cols <- run_tasks(list(tasks[[1L]], tasks[[length(wage_hat) + 1L]]))
  message("  wage col 1: max|d_g2| = ", signif(max(abs(smoke_cols[[1]]$d_g2)), 3),
          ", max|d_g3| = ", signif(max(abs(smoke_cols[[1]]$d_g3)), 3))
  message("  beta col 1 (", rownames(beta_hat)[beta_relevant[1L]],
          "): max|d_g2| = ", signif(max(abs(smoke_cols[[2]]$d_g2)), 3),
          ", max|d_g3| = ", signif(max(abs(smoke_cols[[2]]$d_g3)), 3))
  message("Smoke complete; no output written.")
  quit(save = "no", status = 0)
}

jac_t0 <- Sys.time()
cols <- run_tasks(tasks)
failed <- vapply(cols, function(cc) inherits(cc, "try-error") || is.null(cc$d_g2), logical(1))
if (any(failed)) {
  stop("Jacobian column(s) failed: ", paste(which(failed), collapse = ", "))
}
message("Jacobian columns done in ",
        round(as.numeric(difftime(Sys.time(), jac_t0, units = "mins")), 1), " min.")
## Checkpoint the expensive part so a downstream failure (assembly, save)
## never costs a recompute; the file is overwritten on each successful run.
saveRDS(list(tasks = tasks, cols = cols, h_wage = h_wage, h_beta = h_beta,
             free_p = free_p, binding = binding),
        file.path("results", "data", "07_murphy_topel_jacobian_checkpoint.rds"))

n_wage <- length(wage_hat)
J_2w <- matrix(0, n_wage, n_wage,
               dimnames = list(names(wage_hat), names(wage_hat)))
J_3w <- matrix(0, length(p_hat), n_wage,
               dimnames = list(names(p_hat), names(wage_hat)))
J_2b <- matrix(0, n_wage, nrow(beta_hat),
               dimnames = list(names(wage_hat), rownames(beta_hat)))
J_3b <- matrix(0, length(p_hat), nrow(beta_hat),
               dimnames = list(names(p_hat), rownames(beta_hat)))
for (j in seq_along(tasks)) {
  tk <- tasks[[j]]
  if (tk$kind == "wage") {
    J_2w[, tk$idx] <- cols[[j]]$d_g2
    J_3w[, tk$idx] <- cols[[j]]$d_g3
  } else {
    J_2b[, tk$idx] <- cols[[j]]$d_g2
    J_3b[, tk$idx] <- cols[[j]]$d_g3
  }
}
## Rownames of the g2 blocks come from build_worker_moment_matrix's
## county:E_k labels; relabel to the moment names for readability.
rownames(J_2w) <- colnames(base$g2)
rownames(J_2b) <- colnames(base$g2)

kappa_J2w <- kappa(J_2w)
message("J_2w condition number: ", signif(kappa_J2w, 4))

if (isTRUE(mt_config$mt_step_check)) {
  message("Step-size robustness probe (first 3 columns of each block at 2h)...")
  for (j in c(1:3, n_wage + (1:3))) {
    tk <- tasks[[j]]
    redo <- jacobian_task(tk$kind, tk$idx, 2 * tk$h)
    ref <- cols[[j]]
    rel <- max(abs(c(redo$d_g2 - ref$d_g2, redo$d_g3 - ref$d_g3))) /
      max(abs(c(ref$d_g2, ref$d_g3, 1e-12)))
    message("  ", tk$kind, " col ", tk$idx, ": rel disagreement at 2h = ", signif(rel, 3))
  }
}

## ---------------------------------------------------------------------------
## Cluster sums and the sandwich. g2/g3 contributions are demeaned (the wage
## FOC sits at a basin floor, not exactly zero); g1 stays raw so the beta
## block reproduces the analytical first stage exactly.
## ---------------------------------------------------------------------------
cl_chr <- as.character(estim_matrix[, "location_id"])
s2 <- rowsum(sweep(base$g2, 2, g2_mean), cl_chr)
g3h <- base$g3 %*% t(C_free)
colnames(g3h) <- names(p_hat)[free_p]
s3 <- rowsum(sweep(g3h, 2, colMeans(g3h)), cl_chr)
ord <- rownames(first_stage$scores)
stopifnot(setequal(rownames(s2), ord))
s2 <- s2[ord, , drop = FALSE]
s3 <- s3[ord, , drop = FALSE]
psi_b <- first_stage$influence

## ---------------------------------------------------------------------------
## Wage-block estimating equations. Without the interior penalty the wage
## stage solves gbar2 = 0 and the raw moment system is used. With the penalty
## (JMP_WAGE_INTERIOR_PENALTY must MATCH the 06 estimation run), the wage
## minimizer solves the penalized score
##   h(w, beta) = G' [ gbar2 + (lambda/2) u ] = 0,  u_ck = pen'(share_ck) N/n_c,
## an analytic function of the same sample means, so the system is a
## transform of objects already computed:
##   scores : s2h_g = J_2w' [ (I + D) s2_g + D sE_g ]
##   J_hw   = J_2w' (I + D) J_2w,   J_hb = J_2w' (I + D) J_2b,
##   D      = (lambda/2) diag(pen''(share_ck) (N/n_c)^2),
## where sE_g are cluster sums of the observed-share rows (model shares carry
## sampling noise through BOTH gbar2 and the observed county means), and
## pen'(s) = -2 gap/s, pen''(s) = 2(1+gap)/s^2 on the active branch
## (gap = log(min_share) - log(s) > 0), zero when slack. When the penalty is
## slack at every moment, D = 0 and u = 0, so this is a fixed nonsingular
## transform (J_2w') of the raw system and the sandwich is IDENTICAL
## (invariance unit-tested). dG/dw second-order terms are dropped, consistent
## with the rest of the Jacobian treatment (docs/murphy_topel_proposal.md).
## ---------------------------------------------------------------------------
message("Interior penalty in the MT wage FOC: ",
        isTRUE(mt_config$wage_interior_penalty_enabled),
        " (must match the 06 estimation run)")
if (isTRUE(mt_config$wage_interior_penalty_enabled)) {
  eobs_rows <- build_worker_moment_matrix(
    rows$observed_E, rows$county, mt_config,
    worker_indices = 2:n_w
  )
  stopifnot(identical(colnames(eobs_rows), colnames(base$g2)))
  mom_fips <- sub("^county([0-9]+):E_[0-9]+$", "\\1", colnames(base$g2))
  n_by_mom <- as.numeric(table(rows$county)[mom_fips])
  N_rows <- nrow(base$g2)
  obs_mean_mom <- colSums(eobs_rows) / n_by_mom
  share_mom <- g2_mean * (N_rows / n_by_mom) + obs_mean_mom

  lam <- mt_config$wage_interior_penalty_weight
  floor_share <- max(mt_config$wage_interior_penalty_min_share, mt_config$numeric_floor)
  s_safe <- pmax(share_mom, mt_config$numeric_floor)
  gap <- pmax(0, log(floor_share) - log(s_safe))
  active <- gap > 0
  pen2 <- ifelse(active, 2 * (1 + gap) / s_safe^2, 0)
  u <- ifelse(active, -2 * gap / s_safe, 0) * (N_rows / n_by_mom)
  Dvec <- (lam / 2) * pen2 * (N_rows / n_by_mom)^2

  h_mean <- as.numeric(t(J_2w) %*% (g2_mean + (lam / 2) * u))
  message("Penalized wage FOC: ||h|| = ", signif(sqrt(sum(h_mean^2)), 4),
          " (raw ||mean g2|| = ", signif(sqrt(sum(g2_mean^2)), 4), "); ",
          sum(active), " active penalty moment(s)",
          if (any(active)) paste0(": ",
            paste0(colnames(base$g2)[active], " (share=",
                   signif(share_mom[active], 3), ")", collapse = ", ")) else "")
  near_kink <- !active & share_mom < 1.05 * floor_share
  if (any(near_kink)) {
    message("NOTE: moment(s) within 5% above the penalty floor (one-sided ",
            "kink nearby; treat the corresponding SEs with care): ",
            paste(colnames(base$g2)[near_kink], collapse = ", "))
  }

  sE <- rowsum(sweep(eobs_rows, 2, colMeans(eobs_rows)), cl_chr)[ord, , drop = FALSE]
  s2_use <- (sweep(s2, 2, 1 + Dvec, "*") + sweep(sE, 2, Dvec, "*")) %*% J_2w
  colnames(s2_use) <- names(wage_hat)
  J_2w_use <- t(J_2w) %*% ((1 + Dvec) * J_2w)
  J_2b_use <- t(J_2w) %*% ((1 + Dvec) * J_2b)
  dimnames(J_2w_use) <- list(names(wage_hat), names(wage_hat))
  dimnames(J_2b_use) <- list(names(wage_hat), rownames(beta_hat))
  wage_penalty_record <- list(
    enabled = TRUE, lambda = lam, min_share = floor_share,
    share = setNames(share_mom, colnames(base$g2)),
    gap = setNames(gap, colnames(base$g2)),
    active = colnames(base$g2)[active],
    u = u, Dvec = Dvec, h_norm = sqrt(sum(h_mean^2))
  )
} else {
  s2_use <- s2
  J_2w_use <- J_2w
  J_2b_use <- J_2b
  wage_penalty_record <- list(enabled = FALSE)
}

kappa_used <- kappa(J_2w_use)
message("Wage-block system condition number (as used): ", signif(kappa_used, 4))
if (!is.finite(kappa_used) || kappa_used > 1e10) {
  stop("Wage-block estimating-equation Jacobian is numerically singular ",
       "(condition ", signif(kappa_used, 3), "). One or more wage coordinates ",
       "are locally unidentified at the point estimates; see ",
       "docs/wage_interior_penalty_proposal.md and the priced-out-coords memory.")
}

J_3w_h <- C_free %*% J_3w
J_3b_h <- C_free %*% J_3b
rownames(J_3w_h) <- names(p_hat)[free_p]
rownames(J_3b_h) <- names(p_hat)[free_p]

n_obs <- first_stage$n_obs
n_clusters <- first_stage$n_clusters
k_total <- nrow(beta_hat) + n_wage + length(free_p)
adjustment <- (n_clusters / (n_clusters - 1)) * ((n_obs - 1) / (n_obs - k_total))
adjustment_cluster_only <- n_clusters / (n_clusters - 1)

mt <- assemble_triangular_mt_vcov(
  psi_b = psi_b, s2 = s2_use, s3 = s3,
  J_2w = J_2w_use, J_2b = J_2b_use, J_3w = J_3w_h, J_3b = J_3b_h, MtM = MtM_h,
  adjustment = adjustment
)

## Validation: the beta block must equal the analytical clustered vcov up to
## the (known) ratio of small-sample adjustments.
beta_block <- mt$vcov[seq_len(nrow(beta_hat)), seq_len(nrow(beta_hat))]
ratio <- adjustment / first_stage$adjustment
block_dev <- max(abs(beta_block / ratio - first_stage$vcov)) / max(abs(first_stage$vcov))
message("Beta block vs analytical stage-1 vcov (after adjustment ratio ",
        signif(ratio, 4), "): max rel dev ", signif(block_dev, 3))
stopifnot(block_dev < 1e-8)

## Propagation-only decomposition for comparison with the draw-based reps:
## Var(theta_hat) from first-stage noise alone is (dw/dbeta') V1 (dw/dbeta')'.
## Uses the estimating-equation system actually solved (penalized when on).
dw_dbeta <- -solve(J_2w_use, J_2b_use)
prop_wage <- dw_dbeta %*% first_stage$vcov %*% t(dw_dbeta)
se_wage_prop <- sqrt(pmax(diag(prop_wage), 0))

## Assemble the full named SE vector in parameter_table order; binding price
## coordinates get NA. NB: demand county:quarter fixed effects share parameter
## NAMES with their price/B-equation twins (2018.2-2021.2), so the SE vector is
## assembled POSITIONALLY. A name-keyed assignment (`se_all[name] <- ...`)
## writes both twins to the first (demand) match and leaves the structural twin
## NA, which silently drops 33 structural SEs and breaks the 08 MT merge.
n_beta <- nrow(beta_hat)
se_all <- setNames(rep(NA_real_, n_beta + n_wage + length(p_hat)),
                   c(rownames(beta_hat), names(wage_hat), names(p_hat)))
se_all[seq_len(n_beta)] <- mt$se[seq_len(n_beta)]
se_all[n_beta + seq_len(n_wage)] <- mt$se[n_beta + seq_len(n_wage)]
se_all[n_beta + n_wage + free_p] <- mt$se[n_beta + n_wage + seq_along(free_p)]

## Structural-only SE vector (wage + price block). Its names are unique -- no
## demand twins -- so 08 merges on this to avoid the name collision above.
se_structural <- se_all[n_beta + seq_len(n_wage + length(p_hat))]

wage_summary <- data.table(
  parm_name = names(wage_hat),
  estimate = unname(wage_hat),
  se_mt_total = unname(se_all[names(wage_hat)]),
  se_first_stage_propagation = unname(se_wage_prop)
)
message("Wage-parameter MT SEs (total | first-stage propagation only):")
for (i in seq_len(nrow(wage_summary))) {
  message("  ", wage_summary$parm_name[i], ": ",
          signif(wage_summary$se_mt_total[i], 4), " | ",
          signif(wage_summary$se_first_stage_propagation[i], 4))
}

saveRDS(list(
  vcov = mt$vcov,
  vcov_cluster_only = mt$vcov * (adjustment_cluster_only / adjustment),
  se = se_all,
  se_structural = se_structural,
  wage_summary = wage_summary,
  blocks = list(J_2w = J_2w, J_2b = J_2b, J_3w = J_3w, J_3b = J_3b,
                J_2w_use = J_2w_use, J_2b_use = J_2b_use,
                C_free = C_free, MtM_h = MtM_h, MtM_full = MtM_full,
                dw_dbeta = dw_dbeta),
  wage_penalty = wage_penalty_record,
  foc = list(g2_mean = g2_mean, g3_mean = g3_mean, price_kkt = kkt),
  binding_price_coords = names(p_hat)[binding],
  steps = list(h_wage = h_wage, h_beta = h_beta),
  cluster_variable = "location_id",
  n_obs = n_obs,
  n_clusters = n_clusters,
  k_total = k_total,
  adjustment = adjustment,
  adjustment_cluster_only = adjustment_cluster_only,
  beta_block_rel_dev = block_dev,
  J_2w_condition = kappa_J2w,
  J_2w_use_condition = kappa_used,
  created_at = Sys.time()
), output_path)
message("Saved Murphy-Topel vcov to ", output_path)

script_end <- Sys.time()
message("Script finished at ", script_end, " (Duration: ",
        round(difftime(script_end, script_start, units = "mins"), 2), " minutes)")

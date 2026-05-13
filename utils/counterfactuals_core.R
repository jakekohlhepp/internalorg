if (!exists("CONFIG")) {
  source("config.R")
}

if (!exists("pso_solve") || !exists("solver_value")) {
  source(project_path("utils", "structural_solver.R"))
}

load_counterfactual_packages <- function(extra_packages = NULL) {
  required_packages <- unique(c(
    "data.table",
    "lubridate",
    "rootSolve",
    "stringr",
    "lamW",
    "pracma",
    "BB",
    "SQUAREM",
    "nleqslv",
    extra_packages
  ))

  missing_packages <- required_packages[
    !vapply(required_packages, requireNamespace, logical(1), quietly = TRUE)
  ]

  if (length(missing_packages) > 0) {
    stop(
      "Missing required package(s): ",
      paste(missing_packages, collapse = ", "),
      "\nInstall them before running the counterfactual pipeline."
    )
  }

  invisible(lapply(required_packages, function(pkg) {
    suppressPackageStartupMessages(
      library(pkg, character.only = TRUE)
    )
  }))
}

counterfactual_data_path <- function(filename, config = CONFIG) {
  project_path(config$counterfactual_data_dir, filename)
}

legacy_counterfactual_data_path <- function(filename, config = CONFIG) {
  project_path(config$legacy_counterfactual_data_dir, filename)
}

counterfactual_output_path <- function(filename, type = c("tables", "figures"),
                                       config = CONFIG) {
  type <- match.arg(type)
  base_dir <- if (type == "tables") {
    config$counterfactual_table_dir
  } else {
    config$counterfactual_figure_dir
  }

  project_path(base_dir, filename)
}

legacy_counterfactual_output_path <- function(filename,
                                              type = c("tables", "figures"),
                                              config = CONFIG) {
  type <- match.arg(type)
  base_dir <- if (type == "tables") {
    config$legacy_counterfactual_table_dir
  } else {
    config$legacy_counterfactual_figure_dir
  }

  project_path(base_dir, filename)
}

ensure_counterfactual_dirs <- function(config = CONFIG) {
  dirs <- c(
    project_path(config$counterfactual_data_dir),
    project_path(config$counterfactual_table_dir),
    project_path(config$counterfactual_figure_dir)
  )

  invisible(lapply(dirs, ensure_directory))
}

first_existing_path <- function(paths, description = "path") {
  clean_paths <- unique(paths[!is.na(paths) & nzchar(paths)])
  matches <- clean_paths[file.exists(clean_paths)]

  if (length(matches) == 0) {
    stop(
      "Missing ", description, ". Checked:\n  ",
      paste(clean_paths, collapse = "\n  ")
    )
  }

  matches[[1]]
}

read_first_existing_rds <- function(paths, description = "RDS input") {
  readRDS(first_existing_path(paths, description))
}

read_counterfactual_parameters <- function(config = CONFIG) {
  read_first_existing_rds(
    c(
      project_path("results", "data", "06_parameters.rds"),
      legacy_counterfactual_data_path("02_00_parameters.rds", config)
    ),
    description = "counterfactual parameter file"
  )
}

read_counterfactual_rds <- function(filename, legacy_filenames = filename,
                                    description = filename, config = CONFIG) {
  read_first_existing_rds(
    c(
      counterfactual_data_path(filename, config),
      legacy_counterfactual_data_path(legacy_filenames, config)
    ),
    description = description
  )
}

save_counterfactual_rds <- function(object, filename, legacy_filename = NULL,
                                    config = CONFIG) {
  ensure_counterfactual_dirs(config)

  standard_path <- counterfactual_data_path(filename, config)
  ensure_directory(dirname(standard_path))
  saveRDS(object, standard_path)

  if (!is.null(legacy_filename)) {
    legacy_path <- legacy_counterfactual_data_path(legacy_filename, config)
    ensure_directory(dirname(legacy_path))
    saveRDS(object, legacy_path)
  }

  invisible(standard_path)
}

write_counterfactual_text <- function(text, filename, type = c("tables", "figures"),
                                      legacy_filename = NULL, config = CONFIG) {
  type <- match.arg(type)
  ensure_counterfactual_dirs(config)

  output_path <- counterfactual_output_path(filename, type, config)
  ensure_directory(dirname(output_path))
  cat(text, file = output_path)

  if (!is.null(legacy_filename)) {
    legacy_path <- legacy_counterfactual_output_path(legacy_filename, type, config)
    ensure_directory(dirname(legacy_path))
    cat(text, file = legacy_path)
  }

  invisible(output_path)
}

save_counterfactual_plot <- function(filename, legacy_filename = NULL,
                                     config = CONFIG, ...) {
  ensure_counterfactual_dirs(config)

  output_path <- counterfactual_output_path(filename, "figures", config)
  ensure_directory(dirname(output_path))
  ggplot2::ggsave(output_path, ...)

  if (!is.null(legacy_filename)) {
    legacy_path <- legacy_counterfactual_output_path(
      legacy_filename,
      "figures",
      config
    )
    ensure_directory(dirname(legacy_path))
    ggplot2::ggsave(legacy_path, ...)
  }

  invisible(output_path)
}

get_counterfactual_market_parms <- function(all_results) {
  market_parms <- all_results$coefficients
  names(market_parms) <- all_results$parm_name
  market_parms
}

#' Names of the per-firm worker-share columns: E_1, E_2, ..., E_n.
counterfactual_e_field_names <- function(config = CONFIG) {
  paste0("E_", seq_len(config$n_worker_types))
}

#' Names of the per-firm B (assignment) fields, in column-major order over
#' the (n_worker_types x n_task_types) matrix. The downstream pattern
#' "B_<task>_<worker>" matches a flattened B[worker, task] keeping
#' as.vector(B) aligned with these names.
counterfactual_b_field_names <- function(config = CONFIG) {
  as.vector(vapply(
    seq_len(config$n_task_types),
    function(task_id) paste0("B_", task_id, "_", seq_len(config$n_worker_types)),
    character(config$n_worker_types)
  ))
}

#' Names of the per-market total-labor columns: tot_1, tot_2, ..., tot_n.
counterfactual_tot_labor_field_names <- function(config = CONFIG) {
  paste0("tot_", seq_len(config$n_worker_types))
}

compute_counterfactual_total_labor <- function(working_data, config = CONFIG) {
  working_data[, {
    totals <- lapply(seq_len(config$n_worker_types), function(idx) {
      sum(weight * salon_share_subdiv * CSPOP * get(paste0("E_", idx)) * avg_labor)
    })
    setNames(totals, paste0("tot_", seq_len(config$n_worker_types)))
  }, by = c("county", "quarter_year")]
}

build_counterfactual_initial_guess <- function(working_data, market_parms,
                                               config = CONFIG) {
  counties <- config$counties
  quarter_years <- as.character(unique(working_data$quarter_year))
  n_markets <- length(counties) * length(quarter_years)
  n_types <- config$n_worker_types

  initial_guess <- vector(mode = "numeric", length = n_markets * n_types)
  lab_wages <- vector(mode = "character", length = n_markets * n_types)
  rho <- setNames(numeric(length(counties)), counties)

  i <- 1
  for (cnty in counties) {
    rho[cnty] <- market_parms[grep(paste0(cnty, ":cust_price$"), names(market_parms))]
    for (qy in quarter_years) {
      idx <- i:(i + n_types - 1)
      initial_guess[idx] <- c(
        0,
        market_parms[grep(paste0(cnty, ":avg_labor:E"), names(market_parms))]
      ) + market_parms[
        paste0("avg_labor:factor(county)", cnty, ":factor(quarter_year)", qy)
      ]
      lab_wages[idx] <- paste0(cnty, "-", qy, "-", seq_len(n_types))
      i <- i + n_types
    }
  }

  names(initial_guess) <- lab_wages

  list(initial_guess = initial_guess, rho = rho)
}

build_counterfactual_tild_theta <- function(working_data, market_parms, rho,
                                            config = CONFIG) {
  tild_theta <- vector(mode = "list", length = length(config$counties))
  names(tild_theta) <- config$counties

  for (cnty in names(tild_theta)) {
    county_quarters <- as.character(unique(working_data[county == cnty]$quarter_year))
    tild_theta[[cnty]] <- vector(mode = "list", length = length(county_quarters))
    names(tild_theta[[cnty]]) <- county_quarters

    for (qy in county_quarters) {
      w_mat <- matrix(
        c(0, market_parms[grep(paste0(cnty, ":avg_labor:E"), names(market_parms))]),
        ncol = config$n_task_types,
        nrow = config$n_worker_types,
        byrow = FALSE
      )
      w_mat <- w_mat + market_parms[
        paste0("avg_labor:factor(county)", cnty, ":factor(quarter_year)", qy)
      ]

      skills <- matrix(
        market_parms[grep(paste0(cnty, ":avg_labor:B"), names(market_parms))],
        ncol = config$n_task_types,
        nrow = config$n_worker_types,
        byrow = FALSE
      )

      county_theta <- w_mat + (rho[cnty])^(-1) * skills
      tild_theta[[cnty]][[qy]] <- sweep(
        county_theta,
        2,
        apply(county_theta, 2, min),
        FUN = "-"
      )
    }
  }

  tild_theta
}

build_counterfactual_market_context <- function(working_data, all_results,
                                                config = CONFIG) {
  market_parms <- get_counterfactual_market_parms(all_results)
  ## Prefer the saved baseline-equilibrium total labor written by 13_
  ## (model_labor at each market's final wages). If the file isn't there
  ## yet (first-time run before 13_ has been re-executed), fall back to
  ## the data-anchored aggregate. Downstream counterfactuals should target
  ## the baseline equilibrium, not the data anchor, because for markets
  ## where 13_ used WLS (Tier 2) the data target is structurally
  ## unreachable.
  saved_total_labor_path <- counterfactual_data_path("13_total_labor.rds", config)
  if (file.exists(saved_total_labor_path)) {
    total_labor <- data.table::as.data.table(readRDS(saved_total_labor_path))
    message("Loaded baseline-equilibrium total_labor from ", saved_total_labor_path)
  } else {
    total_labor <- compute_counterfactual_total_labor(working_data, config)
    message("No 13_total_labor.rds found; using data-anchored ",
            "compute_counterfactual_total_labor() (re-run 13_ to refresh).")
  }
  guess_setup <- build_counterfactual_initial_guess(working_data, market_parms, config)
  tild_theta <- build_counterfactual_tild_theta(
    working_data,
    market_parms,
    guess_setup$rho,
    config
  )

  list(
    market_parms = market_parms,
    total_labor = total_labor,
    total_labor_orig = data.table::copy(total_labor),
    initial_guess = guess_setup$initial_guess,
    rho = guess_setup$rho,
    tild_theta = tild_theta
  )
}

load_counterfactual_context <- function(extra_packages = NULL, config = CONFIG) {
  load_counterfactual_packages(extra_packages)
  ensure_counterfactual_dirs(config)

  working_data <- read_counterfactual_rds(
    "13_working_data.rds",
    legacy_filenames = "13_working_data.rds",
    description = "baseline counterfactual working data",
    config = config
  )
  initial_wages <- read_counterfactual_rds(
    "13_initial_wages.rds",
    legacy_filenames = "13_initial_wages.rds",
    description = "baseline counterfactual wages",
    config = config
  )
  all_results <- read_counterfactual_parameters(config)

  c(
    list(
      working_data = working_data,
      initial_wages = initial_wages,
      all_results = all_results
    ),
    build_counterfactual_market_context(working_data, all_results, config)
  )
}

attach_counterfactual_context <- function(context, env = parent.frame()) {
  warning(
    "attach_counterfactual_context() is deprecated because it hides variable ",
    "provenance. Assign the needed context fields explicitly, e.g. ",
    "working_data <- context$working_data.",
    call. = FALSE
  )
  invisible(context)
}

get_counterfactual_focus_quarter <- function(config = CONFIG) {
  if (!is.null(config$counterfactual_focus_quarters) &&
      length(config$counterfactual_focus_quarters) > 0) {
    return(config$counterfactual_focus_quarters[[1]])
  }

  max(config$estimation_quarters)
}

new_counterfactual_wages_grid <- function(quarter_years, include_solution_type = TRUE,
                                          config = CONFIG) {
  if (include_solution_type) {
    wage_grid <- data.table(
      expand.grid(
        county = config$counties,
        quarter_year = quarter_years,
        sol_type = c("reorg", "realloc")
      )
    )
  } else {
    wage_grid <- data.table(
      expand.grid(
        county = config$counties,
        quarter_year = quarter_years
      )
    )
  }

  wage_grid[, fval := Inf]
  wage_grid[, converged := FALSE]
  wage_grid[, method := NA_character_]
  wage_grid[, termcd := NA_integer_]
  wage_grid[, message := NA_character_]
  wage_grid[, target_tol := NA_real_]
  for (idx in seq_len(config$n_worker_types)) {
    wage_grid[, (paste0("w", idx)) := Inf]
    wage_grid[, (paste0("resid_", idx)) := Inf]
  }

  wage_grid
}

build_counterfactual_structure_snapshot <- function(get_everything_fn, wage_table,
                                                    solution_type = NULL,
                                                    quarter_year = NULL,
                                                    config = CONFIG) {
  if (is.null(quarter_year)) {
    quarter_year <- get_counterfactual_focus_quarter(config)
  }
  target_quarter_year <- quarter_year

  structures <- vector(mode = "list", length = length(config$counties))
  names(structures) <- config$counties

  wage_cols <- paste0("w", seq_len(config$n_worker_types))
  wage_table <- data.table(wage_table)
  wage_table[, county := as.character(county)]

  for (cnty in config$counties) {
    slice <- wage_table[county == cnty & quarter_year == target_quarter_year]
    if (!is.null(solution_type)) {
      slice <- slice[sol_type == solution_type]
    }

    stopifnot(nrow(slice) == 1)
    wage_guess <- as.numeric(slice[, ..wage_cols])
    structures[[cnty]] <- get_everything_fn(wage_guess, cnty, quarter_year)
  }

  structures
}

counterfactual_assignment <- function(cost_matrix, alpha, gamma,
                                      innertol = CONFIG$innertol,
                                      config = CONFIG) {
  gamma <- counterfactual_effective_gamma(gamma, config)
  if (is.finite(gamma) && gamma > 0) {
    A <- exp(-cost_matrix / gamma)
    E <- rep(1 / config$n_worker_types, config$n_worker_types)
    A[A >= Inf] <- config$numeric_ceiling
    A[A <= 0] <- config$numeric_floor

    fxpt <- function(p) {
      C <- colSums(t(A) * alpha / colSums(A * p))
      p * C
    }

    E <- SQUAREM::squarem(
      E,
      fixptfn = fxpt,
      control = list(maxiter = config$fixedpoint_max_iter, tol = innertol)
    )$par

    B <- t(t(A) * alpha / colSums(A * E)) * E
  } else if (identical(gamma, 0) || (!is.na(gamma) && gamma == 0)) {
    B <- matrix(0, ncol = config$n_task_types, nrow = config$n_worker_types)
    for (col in seq_len(config$n_task_types)) {
      B[which.min(cost_matrix[, col]), col] <- alpha[col]
    }
  } else {
    B <- matrix(0, ncol = config$n_task_types, nrow = config$n_worker_types)
    B[which.min(rowSums(t(t(cost_matrix) * alpha))), ] <- alpha
  }

  E <- rowSums(B)
  B[abs(B) < config$B_zero_threshold] <- 0

  list(B = B, E = E)
}

#' Spec-log helper used by the counterfactual scripts. Mirrors preamble.R's
#' definition so 13-19 don't need to redefine it locally.
counterfactual_spec_log <- function(x) {
  ifelse(x == 0 | is.nan(x), 0, log(x))
}

#' Build a per-firm summary record from a wage / theta / gamma triple.
#'
#' Wraps `counterfactual_assignment()` and computes the c_endog / q_endog /
#' s_index / E_i / B_<task>_<worker> fields the counterfactual scripts emit.
#' Optional flags select which fields appear in the result so 13-18 can use
#' the same helper despite their different output schemas.
#'
#' @param cost_matrix wage-adjusted skill cost matrix (worker x task).
#' @param alpha task-mix vector, length n_task_types.
#' @param gamma scalar coordination cost; pass `Inf` / `NA` for max frictions.
#' @param wage_guess wage vector, length n_worker_types.
#' @param new_theta optional task-skill matrix; required when `with_q = TRUE`
#'   or `with_swept_b = TRUE`.
#' @param innertol fixed-point tolerance.
#' @param with_s_index include the entropy index in the output.
#' @param with_b include `B_<task>_<worker>` columns from the assignment.
#' @param with_swept_b like `with_b`, but multiplies B by the column-min-
#'   subtracted theta to produce the productivity-output panel that
#'   `get_prod()` writes in 14-17.
#' @param config configuration list controlling dimensions and tolerances.
#' @return Named list ready to drop into a data.table assignment.
counterfactual_org_outputs <- function(cost_matrix, alpha, gamma, wage_guess,
                                       new_theta = NULL,
                                       innertol = CONFIG$innertol,
                                       with_s_index = FALSE,
                                       with_b = FALSE,
                                       with_swept_b = FALSE,
                                       config = CONFIG) {
  if ((with_swept_b || isTRUE(attr(new_theta, "needs_q"))) && is.null(new_theta)) {
    stop("counterfactual_org_outputs() requires new_theta when q is requested.")
  }

  assign <- counterfactual_assignment(cost_matrix, alpha, gamma, innertol, config)
  B <- assign$B
  E <- assign$E
  Brel <- t(t(B / E) / alpha)
  s_index <- sum(B * counterfactual_spec_log(Brel))
  cendog <- sum(E * wage_guess) +
    ifelse(is.finite(gamma), gamma * s_index, 0)
  qendog <- if (is.null(new_theta)) NA_real_ else sum(B * new_theta)

  out <- list(c_endog = cendog, q_endog = qendog)
  if (with_s_index) out$s_index <- s_index
  out <- c(out, setNames(as.list(E), counterfactual_e_field_names(config)))

  if (with_b || with_swept_b) {
    if (with_swept_b) {
      swept_theta <- sweep(new_theta, 2, apply(new_theta, 2, min), FUN = "-")
      B_out <- B * swept_theta
    } else {
      B_out <- B
    }
    out <- c(out,
             setNames(as.list(as.vector(B_out)),
                      counterfactual_b_field_names(config)))
  }

  out
}

#' Reconstruct an org summary from a saved B matrix (no fresh assignment).
#'
#' Implements the realloc branch of 14-17's `solve_reloc`: pull a saved
#' firm-level B (e.g. from `orig_struct`), recompute the cost / quality
#' summary at the new wage / gamma without re-solving the assignment.
counterfactual_org_outputs_from_b <- function(B, alpha, gamma, wage_guess,
                                              new_theta,
                                              with_s_index = FALSE,
                                              with_b = FALSE,
                                              with_swept_b = FALSE,
                                              config = CONFIG) {
  E <- rowSums(B)
  Brel <- t(t(B / E) / colSums(B))
  s_index <- sum(B * counterfactual_spec_log(Brel))
  cendog <- sum(E * wage_guess) +
    ifelse(is.finite(gamma), gamma * s_index, 0)
  qendog <- sum(B * new_theta)

  out <- list(c_endog = cendog, q_endog = qendog)
  if (with_s_index) out$s_index <- s_index
  out <- c(out, setNames(as.list(E), counterfactual_e_field_names(config)))

  if (with_b || with_swept_b) {
    if (with_swept_b) {
      swept_theta <- sweep(new_theta, 2, apply(new_theta, 2, min), FUN = "-")
      B_out <- B * swept_theta
    } else {
      B_out <- B
    }
    out <- c(out,
             setNames(as.list(as.vector(B_out)),
                      counterfactual_b_field_names(config)))
  }

  out
}

counterfactual_effective_gamma <- function(gamma, config = CONFIG) {
  gamma_floor <- config$counterfactual_zero_gamma_floor
  if (is.null(gamma_floor) || !is.finite(gamma_floor) || gamma_floor <= 0) {
    return(gamma)
  }

  ifelse(!is.na(gamma) & gamma == 0, gamma_floor, gamma)
}

counterfactual_last_price_status <- new.env(parent = emptyenv())

counterfactual_bounded_exp <- function(x, config = CONFIG) {
  exp(pmin(
    pmax(x, log(config$numeric_floor)),
    log(config$numeric_ceiling)
  ))
}

counterfactual_lambertW0_exp <- function(log_x, config = CONFIG) {
  log_x[is.na(log_x)] <- log(config$numeric_floor)
  log_x[is.infinite(log_x) & log_x > 0] <- 1e6
  log_x[is.infinite(log_x) & log_x < 0] <- log(config$numeric_floor)

  out <- numeric(length(log_x))
  tiny <- log_x <= log(config$numeric_floor)
  regular <- !tiny & log_x < log(.Machine$double.xmax)
  large <- !tiny & !regular

  out[tiny] <- exp(log_x[tiny])
  out[regular] <- lamW::lambertW0(exp(log_x[regular]))

  if (any(large)) {
    L1 <- log_x[large]
    L2 <- log(L1)
    out[large] <- L1 - L2 + L2 / L1 +
      L2 * (-2 + L2) / (2 * L1^2)
  }

  pmax(out, config$numeric_floor)
}

counterfactual_logit_shares <- function(Q, price, wgt, rho, config = CONFIG) {
  utility <- as.numeric(Q + rho * price)
  if (!all(is.finite(c(utility, wgt)))) {
    return(rep(NA_real_, length(utility)))
  }

  shift <- max(c(0, utility))
  numerator <- exp(pmax(utility - shift, log(config$numeric_floor)))
  denominator <- exp(-shift) + sum(wgt * numerator)
  numerator / pmax(denominator, config$numeric_floor)
}

counterfactual_best_response_prices <- function(p0, Q, C, wgt, rho,
                                                outertol = CONFIG$outertol,
                                                label = NULL,
                                                config = CONFIG) {
  old_p <- p0
  converged <- FALSE
  nonfinite <- FALSE
  max_iter <- config$counterfactual_price_max_iter

  for (iter in seq_len(max_iter)) {
    if (!all(is.finite(c(old_p, Q, C, wgt, rho)))) {
      nonfinite <- TRUE
      new_p <- p0
      break
    }

    demand_index <- counterfactual_bounded_exp(Q + rho * old_p, config)
    denominator <- 1 + sum(wgt * demand_index) - demand_index
    denominator <- pmax(denominator, config$numeric_floor)
    log_lambert_arg <- -1 + Q + rho * C - log(denominator)

    new_p <- -1 / rho + C -
      counterfactual_lambertW0_exp(log_lambert_arg, config) / rho

    if (!all(is.finite(new_p))) {
      nonfinite <- TRUE
      new_p <- old_p
      break
    }

    if (all(abs(new_p - old_p) < outertol)) {
      converged <- TRUE
      break
    }

    old_p <- new_p
  }

  status <- list(
    converged = converged,
    iterations = iter,
    max_iter = max_iter,
    nonfinite = nonfinite,
    label = label
  )
  counterfactual_last_price_status$status <- status

  if (!converged || nonfinite) {
    warning(
      "Counterfactual price solver did not converge",
      if (!is.null(label)) paste0(" for ", label),
      ": iterations=", iter,
      ", nonfinite=", nonfinite,
      call. = FALSE
    )
  }

  attr(new_p, "price_status") <- status
  new_p
}

counterfactual_labor_gap <- function(new_total_labor, total_labor, cnty, qy,
                                     scale = "log", config = CONFIG) {
  target_labor <- total_labor[
    county == cnty & quarter_year == qy,
    -c("county", "quarter_year")
  ]
  stopifnot(nrow(new_total_labor) == 1)
  stopifnot(nrow(target_labor) == 1)

  new_labor <- as.numeric(as.matrix(new_total_labor))
  target_labor <- as.numeric(as.matrix(target_labor))

  if (identical(scale, "log")) {
    labor_clearing <- log(pmax(new_labor, config$numeric_floor)) -
      log(pmax(target_labor, config$numeric_floor))
  } else if (identical(scale, "relative")) {
    labor_clearing <- (new_labor - target_labor) /
      pmax(abs(target_labor), config$numeric_floor)
  } else if (identical(scale, "raw")) {
    labor_clearing <- new_labor - target_labor
  } else {
    stop("Unknown counterfactual labor-gap scale: ", scale)
  }

  names(labor_clearing) <- paste0(
    cnty,
    "-",
    qy,
    "-",
    seq_len(config$n_worker_types)
  )

  labor_clearing
}

counterfactual_residual_norm <- function(residual) {
  residual <- as.numeric(residual)
  if (!all(is.finite(residual))) {
    return(Inf)
  }

  max(abs(residual))
}

counterfactual_evaluate_wage_solution <- function(fn, par) {
  if (is.null(par) || !all(is.finite(par)) || any(par <= 0)) {
    residual <- rep(Inf, CONFIG$n_worker_types)
    return(list(norm = Inf, residual = residual))
  }

  residual <- tryCatch(as.numeric(fn(par)), error = function(e) rep(Inf, CONFIG$n_worker_types))
  list(norm = counterfactual_residual_norm(residual), residual = residual)
}

counterfactual_safe_wage_fn <- function(fn, config = CONFIG) {
  force(fn)
  function(wages) {
    if (is.null(wages) ||
        length(wages) != config$n_worker_types ||
        !all(is.finite(wages)) ||
        any(wages <= 0)) {
      return(rep(1e6, config$n_worker_types))
    }

    residual <- tryCatch(
      as.numeric(fn(wages)),
      error = function(e) rep(1e6, config$n_worker_types)
    )

    if (length(residual) != config$n_worker_types ||
        !all(is.finite(residual))) {
      return(rep(1e6, config$n_worker_types))
    }

    residual
  }
}

counterfactual_candidate_result <- function(fn, par, method, message = NULL,
                                           termcd = NA_integer_) {
  eval <- counterfactual_evaluate_wage_solution(fn, par)
  list(
    par = as.numeric(par),
    residual = eval$norm,
    residual_components = eval$residual,
    method = method,
    termcd = termcd,
    message = message
  )
}

counterfactual_better_result <- function(candidate, incumbent) {
  if (is.null(incumbent)) {
    return(TRUE)
  }
  is.finite(candidate$residual) && candidate$residual < incumbent$residual
}

counterfactual_multistarts <- function(start, additional_starts = list(),
                                       config = CONFIG) {
  start <- pmax(as.numeric(start), config$numeric_floor)
  starts <- c(
    list(start),
    list(start * 0.75, start * 1.25),
    list(pmax(start + c(-5, -2, 0, 2, 5), config$numeric_floor)),
    additional_starts
  )

  unique_starts <- list()
  seen <- character(0)
  for (candidate in starts) {
    candidate <- pmax(as.numeric(candidate), config$numeric_floor)
    key <- paste(round(candidate, 8), collapse = "|")
    if (!key %in% seen && length(candidate) == config$n_worker_types) {
      unique_starts[[length(unique_starts) + 1]] <- candidate
      seen <- c(seen, key)
    }
  }

  unique_starts
}

counterfactual_protected_worker_starts <- function(..., target_labor,
                                                   config = CONFIG) {
  starts <- list(...)
  starts <- starts[!vapply(starts, is.null, logical(1))]
  starts <- lapply(starts, as.numeric)
  starts <- starts[vapply(starts, function(x) {
    length(x) == config$n_worker_types && all(is.finite(x)) && all(x > 0)
  }, logical(1))]

  if (length(starts) == 0) {
    return(list())
  }

  target_labor <- as.numeric(target_labor)
  positive_types <- which(target_labor > config$numeric_floor)
  zero_types <- setdiff(seq_len(config$n_worker_types), positive_types)
  if (length(positive_types) == 0 || length(zero_types) == 0) {
    return(list())
  }

  base <- Reduce(pmax, starts)
  max_wage <- max(base, na.rm = TRUE)
  protected_starts <- list()
  for (positive_type in positive_types) {
    for (positive_multiplier in c(0.4, 0.6)) {
      for (zero_multiplier in c(1.0, 1.5)) {
        candidate <- base
        candidate[positive_type] <- max(
          candidate[positive_type],
          max_wage * positive_multiplier
        )
        candidate[zero_types] <- pmax(
          candidate[zero_types],
          max_wage * zero_multiplier + seq_along(zero_types) - 1
        )
        protected_starts[[length(protected_starts) + 1]] <- candidate
      }
    }
  }

  protected_starts
}

counterfactual_solve_wage_market <- function(fn, start, label = NULL,
                                             additional_starts = list(),
                                             target_tol = CONFIG$counterfactual_wage_tol,
                                             nleqslv_control = list(),
                                             bb_control = list(),
                                             use_homotopy = TRUE,
                                             config = CONFIG) {
  fn <- counterfactual_safe_wage_fn(fn, config)
  starts <- counterfactual_multistarts(start, additional_starts, config)
  best_result <- NULL

  for (idx in seq_along(starts)) {
    positive_start <- starts[[idx]]
    start_label <- if (is.null(label)) {
      paste0("start ", idx)
    } else {
      paste0(label, " start ", idx)
    }

    start_result <- counterfactual_candidate_result(fn, positive_start, "start", start_label)
    if (counterfactual_better_result(start_result, best_result)) {
      best_result <- start_result
    }

    solve_log_wages <- function(log_wages) {
      fn(exp(log_wages))
    }
    control <- modifyList(
      list(
        xtol = 1e-10,
        ftol = 1e-8,
        maxit = config$counterfactual_nleqslv_maxit,
        cndtol = 1e-12,
        allowSingular = TRUE,
        trace = 0L
      ),
      nleqslv_control
    )

    nleqslv_result <- tryCatch(
      nleqslv::nleqslv(
        x = log(positive_start),
        fn = solve_log_wages,
        method = "Broyden",
        global = "dbldog",
        control = control
      ),
      error = function(e) {
        warning("nleqslv failed", if (!is.null(label)) paste0(" for ", label), ": ",
                conditionMessage(e), call. = FALSE)
        NULL
      }
    )

    if (!is.null(nleqslv_result)) {
      candidate <- counterfactual_candidate_result(
        fn,
        exp(nleqslv_result$x),
        "nleqslv",
        nleqslv_result$message,
        nleqslv_result$termcd
      )
      if (counterfactual_better_result(candidate, best_result)) {
        best_result <- candidate
      }
    }
  }

  if (is.null(best_result)) {
    best_result <- counterfactual_candidate_result(fn, start, "failed", "no feasible start")
  }

  if (use_homotopy &&
      (!is.finite(best_result$residual) || best_result$residual > target_tol)) {
    anchor <- pmax(as.numeric(start), config$numeric_floor)
    homotopy_start <- pmax(best_result$par, config$numeric_floor)

    for (lambda in c(0.25, 0.5, 0.75, 1)) {
      homotopy_fn <- function(wages) {
        safe_wages <- pmax(as.numeric(wages), config$numeric_floor)
        lambda * fn(safe_wages) + (1 - lambda) * (log(safe_wages) - log(anchor))
      }
      homotopy_result <- counterfactual_solve_wage_market(
        homotopy_fn,
        homotopy_start,
        label = paste(label, "homotopy", lambda),
        additional_starts = list(),
        target_tol = target_tol,
        nleqslv_control = nleqslv_control,
        bb_control = bb_control,
        use_homotopy = FALSE,
        config = config
      )

      true_candidate <- counterfactual_candidate_result(
        fn,
        homotopy_result$par,
        paste0("homotopy_", lambda, "+", homotopy_result$method),
        "generic continuation toward full labor-clearing equation"
      )
      if (counterfactual_better_result(true_candidate, best_result)) {
        best_result <- true_candidate
        homotopy_start <- pmax(true_candidate$par, config$numeric_floor)
      } else {
        homotopy_start <- pmax(homotopy_result$par, config$numeric_floor)
      }

      if (is.finite(best_result$residual) && best_result$residual <= target_tol) {
        break
      }
    }
  }

  if (!is.finite(best_result$residual) || best_result$residual > target_tol) {
    bb_control <- modifyList(
      list(
        maxit = config$counterfactual_bbsolve_maxit,
        trace = FALSE,
        NM = TRUE,
        noimp = 100,
        tol = min(target_tol, 1e-8),
        M = c(10, 2, 15, 5, 20)
      ),
      bb_control
    )

    bb_result <- tryCatch(
      BB::BBsolve(best_result$par, fn, control = bb_control),
      error = function(e) {
        warning("BBsolve fallback failed",
                if (!is.null(label)) paste0(" for ", label), ": ",
                conditionMessage(e), call. = FALSE)
        NULL
      }
    )

    if (!is.null(bb_result) &&
        !is.null(bb_result$par) &&
        all(is.finite(bb_result$par)) &&
        all(bb_result$par > 0)) {
      candidate <- counterfactual_candidate_result(
        fn,
        bb_result$par,
        "BBsolve_fallback",
        "BBsolve fallback after residual above target"
      )
      if (counterfactual_better_result(candidate, best_result)) {
        best_result <- candidate
      }
    }
  }

  if (!is.finite(best_result$residual) || best_result$residual > target_tol) {
    broyden_result <- tryCatch(
      pracma::broyden(fn, best_result$par, maxiter = config$counterfactual_broyden_maxit),
      error = function(e) {
        warning("broyden polish failed",
                if (!is.null(label)) paste0(" for ", label), ": ",
                conditionMessage(e), call. = FALSE)
        NULL
      }
    )

    if (!is.null(broyden_result) &&
        !is.null(broyden_result$zero) &&
        all(is.finite(broyden_result$zero)) &&
        all(broyden_result$zero > 0)) {
      candidate <- counterfactual_candidate_result(
        fn,
        broyden_result$zero,
        paste(best_result$method, "broyden_polish", sep = "+"),
        "broyden polish after residual above target"
      )
      if (counterfactual_better_result(candidate, best_result)) {
        best_result <- candidate
      }
    }
  }

  best_result$converged <- is.finite(best_result$residual) &&
    best_result$residual <= target_tol
  best_result$target_tol <- target_tol
  best_result
}

#' PSO + Nelder-Mead wage solver for the counterfactual pipeline.
#'
#' Mirrors `estimate_wage_parameters_pso` (utils/structural_solver.R) but for
#' a single (county, quarter) labor-clearing problem rather than a per-county
#' GMM moment sweep:
#'   - search runs in log-wage space (wages are levels and must be positive);
#'   - cold-start PSO over [-halfwidth, +halfwidth]^n_worker_types;
#'   - two Nelder-Mead polishes: one from the PSO best, one from the seed;
#'   - candidate = lowest of {pso, polish_pso, polish_seed};
#'   - acceptance gate: if the candidate's sum-of-squared residuals exceeds the
#'     seed's, revert to the seed (the "minimizer cannot make things worse than
#'     the start" guarantee from the estimation analogue).
#'
#' Returns the same record shape as `counterfactual_solve_wage_market` so it is
#' a drop-in replacement at the `counterfactual_store_wage_solution` boundary.
counterfactual_solve_wage_market_pso <- function(fn, start, label = NULL,
                                                 target_tol = CONFIG$counterfactual_wage_tol,
                                                 config = CONFIG) {
  start <- pmax(as.numeric(start), config$numeric_floor)
  d <- length(start)
  failure_penalty <- solver_value(config, "optimizer_failure_penalty", 1e6)

  fn_safe <- counterfactual_safe_wage_fn(fn, config)

  objective_ssq <- function(log_w) {
    if (!all(is.finite(log_w))) return(failure_penalty)
    w <- exp(log_w)
    if (!all(is.finite(w)) || any(w <= 0)) return(failure_penalty)
    residual <- tryCatch(as.numeric(fn_safe(w)), error = function(e) NULL)
    if (is.null(residual) || length(residual) != d || any(!is.finite(residual))) {
      return(failure_penalty)
    }
    sum(residual^2)
  }

  start_log <- log(start)
  start_objective <- objective_ssq(start_log)

  n_particles <- solver_value(config, "pso_n_particles", 40L)
  n_iter <- solver_value(config, "pso_n_iter", 100L)
  halfwidth <- solver_value(config, "counterfactual_pso_log_halfwidth", 6)
  parscale_log <- solver_value(config, "counterfactual_pso_log_parscale", 1)
  w_inertia <- solver_value(config, "pso_w", 0.7)
  c1 <- solver_value(config, "pso_c1", 1.5)
  c2 <- solver_value(config, "pso_c2", 1.5)
  pso_trace <- solver_value(config, "min_optim_trace", 0L)
  polish_method <- solver_value(config, "pso_polish_method", "Nelder-Mead")
  polish_maxit <- solver_value(config, "min_optim_maxit", 5000L)
  reltol <- solver_value(config, "min_optim_reltol", config$obj_tol)

  lower <- rep(-halfwidth, d); upper <- rep(halfwidth, d)
  pso_label <- if (is.null(label)) "counterfactual" else label
  message(sprintf(
    "PSO wage solve %s: %d particles x %d iter, log search box +/- %g, cold start.",
    pso_label, n_particles, n_iter, halfwidth
  ))

  pso_res <- pso_solve(
    objective_ssq, lower, upper,
    seed_particle = NULL,
    n_particles = n_particles, n_iter = n_iter,
    w = w_inertia, c1 = c1, c2 = c2, trace = pso_trace,
    label = pso_label
  )

  polish_pso <- tryCatch(
    stats::optim(
      par = pso_res$par, fn = objective_ssq,
      method = polish_method,
      control = list(parscale = rep(parscale_log, d),
                     maxit = polish_maxit, reltol = reltol,
                     trace = pso_trace)
    ),
    error = function(e) {
      warning("PSO->polish failed", if (!is.null(label)) paste0(" for ", label), ": ",
              conditionMessage(e), call. = FALSE)
      list(par = pso_res$par, value = pso_res$value, convergence = -1L)
    }
  )

  polish_seed <- tryCatch(
    stats::optim(
      par = start_log, fn = objective_ssq,
      method = polish_method,
      control = list(parscale = rep(parscale_log, d),
                     maxit = polish_maxit, reltol = reltol,
                     trace = pso_trace)
    ),
    error = function(e) {
      warning("seed->polish failed", if (!is.null(label)) paste0(" for ", label), ": ",
              conditionMessage(e), call. = FALSE)
      list(par = start_log, value = start_objective, convergence = -1L)
    }
  )

  candidates <- list(
    pso         = list(par = pso_res$par,     value = pso_res$value),
    polish_pso  = list(par = polish_pso$par,  value = polish_pso$value),
    polish_seed = list(par = polish_seed$par, value = polish_seed$value)
  )
  cand_values <- vapply(candidates, function(x) x$value, numeric(1))
  best_label <- names(cand_values)[which.min(cand_values)]
  best_par_log <- candidates[[best_label]]$par
  best_value <- candidates[[best_label]]$value

  message(sprintf(
    "PSO wage solve %s done: pso=%.6g  polish_pso=%.6g  polish_seed=%.6g  best=%.6g (%s)",
    pso_label, pso_res$value, polish_pso$value, polish_seed$value,
    best_value, best_label
  ))

  accepted <- is.finite(best_value) &&
    best_value <= start_objective * (1 + sqrt(.Machine$double.eps))
  if (!accepted) {
    warning(
      "Rejected PSO counterfactual wage solve ", pso_label,
      " because the minimum sum-of-squared residuals did not improve on the start. ",
      "start=", signif(start_objective, 6),
      ", candidate=", signif(best_value, 6),
      call. = FALSE
    )
    best_par_log <- start_log
    best_value <- start_objective
    best_label <- "start_revert"
  }

  best_par <- exp(best_par_log)
  best_par <- pmax(best_par, config$numeric_floor)
  result <- counterfactual_candidate_result(
    fn_safe, best_par,
    method = best_label,
    message = sprintf("PSO+polish (ssq=%.6g, start_ssq=%.6g, accepted=%s)",
                      best_value, start_objective, accepted),
    termcd = if (isTRUE(accepted)) 0L else 1L
  )
  result$pso_value <- pso_res$value
  result$polish_pso_value <- polish_pso$value
  result$polish_seed_value <- polish_seed$value
  result$start_objective <- start_objective
  result$candidate_objective <- best_value
  result$accepted <- accepted
  result$converged <- is.finite(result$residual) && result$residual <= target_tol
  result$target_tol <- target_tol
  result
}

#' BBsolve-based wage solver for the counterfactual labor-clearing problem.
#'
#' Tries BB::BBsolve twice from the same seed: once without Nelder-Mead polish
#' (works for markets where the spectral step finds the root cleanly --
#' empirically 17031 converges to ~1e-9 in ~2s with this) and once with NM
#' polish (works for markets where the spectral residual gets stuck near the
#' basin but NM polish closes the gap -- empirically 36061 converges to
#' ~1e-8 with this). Returns whichever attempt has the smaller residual norm.
#'
#' For markets where neither attempt converges to target_tol (empirically:
#' 6037 in the JMP), the residual vector is reported so callers can apply a
#' partial-alignment fall-back (drop the worst residual component, fix the
#' corresponding wage, and re-anchor that target to the model's prediction
#' at the converged wages).
counterfactual_solve_wage_market_bbsolve <- function(fn, start, label = NULL,
                                                     target_tol = CONFIG$counterfactual_wage_tol,
                                                     config = CONFIG) {
  start <- pmax(as.numeric(start), config$numeric_floor)
  fn_safe <- counterfactual_safe_wage_fn(fn, config)

  start_label <- if (is.null(label)) "initial seed" else paste0(label, " initial seed")
  best_result <- counterfactual_candidate_result(fn_safe, start, "start", start_label)

  for (use_NM in c(FALSE, TRUE)) {
    res <- tryCatch(
      BB::BBsolve(
        par = start,
        fn = fn_safe,
        control = list(
          maxit = config$counterfactual_bbsolve_maxit,
          tol = min(target_tol, 1e-8),
          trace = FALSE,
          NM = use_NM,
          noimp = 100
        ),
        quiet = TRUE
      ),
      error = function(e) {
        warning(
          "BBsolve (NM=", use_NM, ") failed",
          if (!is.null(label)) paste0(" for ", label), ": ",
          conditionMessage(e),
          call. = FALSE
        )
        NULL
      }
    )

    if (!is.null(res) &&
        !is.null(res$par) &&
        all(is.finite(res$par)) &&
        all(res$par > 0)) {
      cand <- counterfactual_candidate_result(
        fn_safe,
        res$par,
        method = paste0("bbsolve_NM=", use_NM),
        message = if (is.null(res$message)) NA_character_ else as.character(res$message),
        termcd = as.integer(if (is.null(res$convergence)) NA_integer_ else res$convergence)
      )
      if (counterfactual_better_result(cand, best_result)) {
        best_result <- cand
      }
    }
  }

  best_result$converged <- is.finite(best_result$residual) &&
    best_result$residual <= target_tol
  best_result$target_tol <- target_tol
  best_result
}

counterfactual_store_wage_solution <- function(wage_table, result, cnty, qy,
                                               solution_type = NULL,
                                               config = CONFIG) {
  wage_cols <- paste0("w", seq_len(config$n_worker_types))
  residual_cols <- paste0("resid_", seq_len(config$n_worker_types))
  selector <- wage_table$county == cnty & wage_table$quarter_year == qy
  if (!is.null(solution_type)) {
    selector <- selector & wage_table$sol_type == solution_type
  }
  rows <- which(selector)

  for (idx in seq_along(wage_cols)) {
    data.table::set(wage_table, rows, wage_cols[idx], result$par[idx])
  }
  data.table::set(wage_table, rows, "fval", result$residual)
  data.table::set(wage_table, rows, "converged", result$converged)
  data.table::set(wage_table, rows, "method", result$method)
  data.table::set(wage_table, rows, "termcd", result$termcd)
  data.table::set(wage_table, rows, "message", result$message)
  data.table::set(wage_table, rows, "target_tol", result$target_tol)
  for (idx in seq_along(residual_cols)) {
    data.table::set(
      wage_table,
      rows,
      residual_cols[idx],
      result$residual_components[idx]
    )
  }

  invisible(wage_table)
}

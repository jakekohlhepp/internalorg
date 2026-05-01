if (!exists("CONFIG")) {
  source("config.R")
}

ensure_counterfactual_solver_functions <- function() {
  required_functions <- c(
    "spec_log",
    "compute_corner_solution",
    "solve_equilibrium"
  )

  missing_functions <- required_functions[
    !vapply(required_functions, exists, logical(1), mode = "function")
  ]

  if (length(missing_functions) > 0) {
    source(project_path("preamble.R"), local = .GlobalEnv)
  }

  invisible(TRUE)
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

  ensure_counterfactual_solver_functions()
  install_counterfactual_BBsolve_wrapper()

  invisible(TRUE)
}

install_counterfactual_BBsolve_wrapper <- function() {
  if (identical(getOption("counterfactual.BBsolve.wrapper", FALSE), TRUE)) {
    return(invisible(TRUE))
  }

  wrapper <- function(par, fn, ..., control = list()) {
    maxit_cap <- suppressWarnings(as.integer(
      Sys.getenv("JMP_COUNTERFACTUAL_BBSOLVE_MAXIT", unset = NA_character_)
    ))
    noimp_cap <- suppressWarnings(as.integer(
      Sys.getenv("JMP_COUNTERFACTUAL_BBSOLVE_NOIMP", unset = NA_character_)
    ))

    if (!is.na(maxit_cap)) {
      current_maxit <- if (is.null(control$maxit)) maxit_cap else control$maxit
      control$maxit <- min(current_maxit, maxit_cap)
    }
    if (!is.na(noimp_cap)) {
      current_noimp <- if (is.null(control$noimp)) noimp_cap else control$noimp
      control$noimp <- min(current_noimp, noimp_cap)
    }

    tryCatch(
      BB::BBsolve(par, fn, ..., control = control),
      error = function(e) {
        warning("BBsolve failed: ", conditionMessage(e), call. = FALSE)
        list(par = par, residual = Inf, convergence = FALSE)
      }
    )
  }

  assign("BBsolve", wrapper, envir = .GlobalEnv)
  options(counterfactual.BBsolve.wrapper = TRUE)

  invisible(TRUE)
}

counterfactual_entropy <- function(B, alpha = NULL, config = CONFIG) {
  if (is.null(alpha)) {
    alpha <- colSums(B)
  }

  E_safe <- pmax(rowSums(B), config$numeric_floor)
  alpha_safe <- pmax(as.numeric(alpha), config$numeric_floor)
  B_rel <- B / E_safe
  B_rel <- t(t(B_rel) / alpha_safe)

  sum(B * spec_log(B_rel))
}

counterfactual_assignment <- function(cost_matrix, alpha, gamma,
                                      innertol = config$innertol,
                                      config = CONFIG) {
  ensure_counterfactual_solver_functions()

  alpha <- as.numeric(alpha)
  if (length(alpha) != config$n_task_types) {
    stop("alpha must have length ", config$n_task_types)
  }

  if (is.finite(gamma) && gamma > 0) {
    result <- solve_equilibrium(
      cost_matrix,
      alpha,
      gamma,
      innertol,
      config = config
    )
    return(list(E = result$E, B = result$B, s_index = result$entropy))
  }

  if (is.finite(gamma) && gamma == 0) {
    corner <- compute_corner_solution(cost_matrix, alpha, config)
    return(list(E = corner$E, B = corner$B, s_index = corner$entropy_bound))
  }

  B <- matrix(0, ncol = config$n_task_types, nrow = config$n_worker_types)
  B[which.min(rowSums(t(t(cost_matrix) * alpha))), ] <- alpha
  B[abs(B) < config$B_zero_threshold] <- 0

  list(E = rowSums(B), B = B, s_index = counterfactual_entropy(B, alpha, config))
}

counterfactual_assignment_vector <- function(alpha, cost_matrix, gamma,
                                             innertol = config$innertol,
                                             include_s_index = FALSE,
                                             config = CONFIG) {
  allocation <- counterfactual_assignment(cost_matrix, alpha, gamma, innertol, config)
  values <- as.vector(allocation$B)

  if (include_s_index) {
    values <- c(allocation$E, allocation$s_index)
  }

  values
}

counterfactual_skill_matrices <- function(wage_guess, cnty, market_parms, rho,
                                          config = CONFIG) {
  theta <- matrix(
    market_parms[grep(paste0(cnty, ":avg_labor:B"), names(market_parms))],
    ncol = config$n_task_types,
    nrow = config$n_worker_types,
    byrow = FALSE
  )
  wage_matrix <- matrix(
    wage_guess,
    ncol = config$n_task_types,
    nrow = config$n_worker_types,
    byrow = FALSE
  )
  cost_matrix <- wage_matrix + (rho[cnty])^(-1) * theta
  cost_matrix <- sweep(cost_matrix, 2, apply(cost_matrix, 2, min), FUN = "-")

  list(theta = theta, cost_matrix = cost_matrix)
}

counterfactual_org_values <- function(alpha, gamma, cost_matrix, theta,
                                      wage_guess, innertol = config$innertol,
                                      include_s_index = FALSE,
                                      include_B = FALSE,
                                      scale_B = NULL,
                                      config = CONFIG) {
  allocation <- counterfactual_assignment(cost_matrix, alpha, gamma, innertol, config)
  E <- allocation$E
  B <- allocation$B

  B_out <- if (is.null(scale_B)) B else B * scale_B
  out <- list(
    c_endog = sum(E * wage_guess) + ifelse(is.finite(gamma), gamma * allocation$s_index, 0),
    q_endog = sum(B * theta)
  )

  if (include_s_index) {
    out$s_index <- allocation$s_index
  }

  for (idx in seq_len(config$n_worker_types)) {
    out[[paste0("E_", idx)]] <- E[idx]
  }

  if (include_B) {
    for (task_idx in seq_len(config$n_task_types)) {
      for (worker_idx in seq_len(config$n_worker_types)) {
        out[[paste0("B_", task_idx, "_", worker_idx)]] <- B_out[worker_idx, task_idx]
      }
    }
  }

  out
}

counterfactual_existing_org_values <- function(B, gamma, theta, wage_guess,
                                               include_s_index = FALSE,
                                               include_B = FALSE,
                                               scale_B = NULL,
                                               config = CONFIG) {
  E <- rowSums(B)
  s_index <- counterfactual_entropy(B, colSums(B), config)
  B_out <- if (is.null(scale_B)) B else B * scale_B

  out <- list(
    c_endog = sum(E * wage_guess) + ifelse(is.finite(gamma), gamma * s_index, 0),
    q_endog = sum(B * theta)
  )

  if (include_s_index) {
    out$s_index <- s_index
  }

  for (idx in seq_len(config$n_worker_types)) {
    out[[paste0("E_", idx)]] <- E[idx]
  }

  if (include_B) {
    for (task_idx in seq_len(config$n_task_types)) {
      for (worker_idx in seq_len(config$n_worker_types)) {
        out[[paste0("B_", task_idx, "_", worker_idx)]] <- B_out[worker_idx, task_idx]
      }
    }
  }

  out
}

counterfactual_best_response_prices <- function(p0, Q, C, wgt, rho,
                                                outertol = CONFIG$outertol) {
  old_p <- p0
  max_iter <- if (!is.null(CONFIG$counterfactual_price_max_iter)) {
    CONFIG$counterfactual_price_max_iter
  } else {
    10000L
  }

  for (iter in seq_len(max_iter)) {
    demand_index <- exp(Q + rho * old_p)
    denominator <- 1 + sum(wgt * demand_index) - demand_index
    denominator <- pmax(denominator, CONFIG$numeric_floor)
    lambert_arg <- exp(-1 + Q + rho * C) / denominator
    lambert_arg <- pmax(lambert_arg, CONFIG$numeric_floor)

    new_p <- -1 / rho + C - lambertW0(lambert_arg) / rho

    if (!all(is.finite(new_p))) {
      return(old_p)
    }

    if (all(abs(new_p - old_p) < outertol)) {
      break
    }
    old_p <- new_p
  }

  new_p
}

counterfactual_safe_broyden <- function(fn, start, maxiter = 1000, ...) {
  maxiter_cap <- suppressWarnings(as.integer(
    Sys.getenv("JMP_COUNTERFACTUAL_BROYDEN_MAXITER", unset = NA_character_)
  ))
  if (!is.na(maxiter_cap)) {
    maxiter <- min(maxiter, maxiter_cap)
  }

  tryCatch(
    broyden(fn, start, maxiter = maxiter, ...),
    error = function(e) {
      warning("broyden refinement failed: ", conditionMessage(e), call. = FALSE)
      list(zero = start, fnorm = Inf)
    }
  )
}

counterfactual_solver_residual <- function(result) {
  if (!is.null(result$residual) && length(result$residual) == 1) {
    return(as.numeric(result$residual))
  }

  if (!is.null(result$fnorm) && length(result$fnorm) == 1) {
    return(as.numeric(result$fnorm))
  }

  Inf
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
    return(Inf)
  }

  tryCatch(
    counterfactual_residual_norm(fn(par)),
    error = function(e) Inf
  )
}

counterfactual_solve_wage_market <- function(fn, start, label = NULL,
                                             fallback = TRUE,
                                             nleqslv_control = list(),
                                             bb_control = list()) {
  if (!requireNamespace("nleqslv", quietly = TRUE)) {
    stop("Package 'nleqslv' is required for counterfactual wage solving.")
  }

  start <- as.numeric(start)
  if (length(start) != CONFIG$n_worker_types) {
    stop("start must have length ", CONFIG$n_worker_types)
  }

  positive_start <- pmax(start, CONFIG$numeric_floor)
  start_residual <- counterfactual_evaluate_wage_solution(fn, positive_start)
  best_result <- list(
    par = positive_start,
    residual = start_residual,
    method = "start",
    termcd = NA_integer_,
    message = "initial point"
  )

  nleqslv_maxit_cap <- suppressWarnings(as.integer(
    Sys.getenv("JMP_COUNTERFACTUAL_NLEQSLV_MAXIT", unset = NA_character_)
  ))
  nleqslv_trace <- suppressWarnings(as.integer(
    Sys.getenv("JMP_COUNTERFACTUAL_NLEQSLV_TRACE", unset = NA_character_)
  ))

  control <- modifyList(
    list(
      xtol = 1e-10,
      ftol = 1e-8,
      maxit = 200L,
      cndtol = 1e-12,
      allowSingular = TRUE,
      trace = 1L
    ),
    nleqslv_control
  )
  if (!is.na(nleqslv_maxit_cap)) {
    control$maxit <- min(control$maxit, nleqslv_maxit_cap)
  }
  if (!is.na(nleqslv_trace)) {
    control$trace <- nleqslv_trace
  }

  solve_log_wages <- function(log_wages) {
    fn(exp(log_wages))
  }

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
    nleqslv_par <- exp(nleqslv_result$x)
    nleqslv_residual <- counterfactual_evaluate_wage_solution(fn, nleqslv_par)
    if (
      all(is.finite(nleqslv_par)) &&
        nleqslv_residual < counterfactual_solver_residual(best_result)
    ) {
      best_result <- list(
        par = nleqslv_par,
        residual = nleqslv_residual,
        method = "nleqslv",
        termcd = nleqslv_result$termcd,
        message = nleqslv_result$message
      )
    }
  }

  if (isTRUE(fallback) && !identical(best_result$method, "nleqslv")) {
    bb_control <- modifyList(
      list(maxit = 10000, trace = TRUE, NM = TRUE, noimp = 50, tol = 1e-8),
      bb_control
    )
    bb_result <- tryCatch(
      BBsolve(best_result$par, fn, control = bb_control),
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
      bb_residual <- counterfactual_evaluate_wage_solution(fn, bb_result$par)
      if (bb_residual < counterfactual_solver_residual(best_result)) {
        best_result <- list(
          par = bb_result$par,
          residual = bb_residual,
          method = "BBsolve_fallback",
          termcd = NA_integer_,
          message = "BBsolve fallback improved nleqslv"
        )
      }
    }
  }

  best_result
}

counterfactual_polish_bad_bbsolve <- function(fn, bb_result, maxiter = 1000,
                                              trigger = NULL, ...) {
  if (is.null(trigger)) {
    trigger <- suppressWarnings(as.numeric(
      Sys.getenv("JMP_COUNTERFACTUAL_BROYDEN_TRIGGER", unset = NA_character_)
    ))
    if (is.na(trigger)) {
      trigger <- 1000
    }
  }

  bb_residual <- counterfactual_solver_residual(bb_result)
  if (is.finite(bb_residual) && bb_residual <= trigger) {
    bb_result$broyden_polished <- FALSE
    bb_result$broyden_reason <- "bbsolve residual below trigger"
    return(bb_result)
  }

  if (is.null(bb_result$par) || !all(is.finite(bb_result$par))) {
    bb_result$broyden_polished <- FALSE
    bb_result$broyden_reason <- "missing or non-finite BBsolve parameters"
    return(bb_result)
  }

  broyden_result <- counterfactual_safe_broyden(fn, bb_result$par, maxiter, ...)
  broyden_residual <- counterfactual_solver_residual(broyden_result)
  if (
    !is.null(broyden_result$zero) &&
      all(is.finite(broyden_result$zero)) &&
      is.finite(broyden_residual) &&
      (!is.finite(bb_residual) || broyden_residual < bb_residual)
  ) {
    bb_result$par <- broyden_result$zero
    bb_result$residual <- broyden_residual
    bb_result$broyden_polished <- TRUE
    bb_result$broyden_reason <- "broyden improved bad BBsolve residual"
    return(bb_result)
  }

  bb_result$broyden_polished <- FALSE
  bb_result$broyden_reason <- "broyden did not improve BBsolve residual"
  bb_result
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
  total_labor <- compute_counterfactual_total_labor(working_data, config)
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
    "05_00_working_data.rds",
    legacy_filenames = "05_00_working_data.rds",
    description = "baseline counterfactual working data",
    config = config
  )
  initial_wages <- read_counterfactual_rds(
    "05_00_initial_wages.rds",
    legacy_filenames = "05_00_initial_wages.rds",
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
  for (idx in seq_len(config$n_worker_types)) {
    wage_grid[, (paste0("w", idx)) := Inf]
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

  for (cnty in config$counties) {
    slice <- wage_table[county == cnty & quarter_year == target_quarter_year]
    if (!is.null(solution_type)) {
      slice <- slice[sol_type == solution_type]
    }

    stopifnot(nrow(slice) == 1)
    wage_guess <- as.numeric(slice[, ..wage_cols])
    structures[[cnty]] <- get_everything_fn(wage_guess, cnty, target_quarter_year)
  }

  structures
}

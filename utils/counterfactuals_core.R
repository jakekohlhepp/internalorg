if (!exists("CONFIG")) {
  source("config.R")
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

  structures <- vector(mode = "list", length = length(config$counties))
  names(structures) <- config$counties

  wage_cols <- paste0("w", seq_len(config$n_worker_types))
  wage_table <- data.table(wage_table)

  for (cnty in config$counties) {
    slice <- wage_table[county == cnty & quarter_year == quarter_year]
    if (!is.null(solution_type)) {
      slice <- slice[sol_type == solution_type]
    }

    stopifnot(nrow(slice) == 1)
    wage_guess <- as.numeric(slice[, ..wage_cols])
    structures[[cnty]] <- get_everything_fn(wage_guess, cnty, quarter_year)
  }

  structures
}

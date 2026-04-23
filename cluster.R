##### purpose: reusable worker clustering and working-data construction helpers

#' Return columns whose names start with a prefix
prefixed_cols <- function(dt, prefix) {
  grep(paste0("^", prefix), names(dt), value = TRUE)
}

#' Return suffixes for columns whose names start with a prefix
prefixed_suffixes <- function(dt, prefix) {
  gsub(paste0("^", prefix), "", prefixed_cols(dt, prefix))
}

#' Safe logarithm with log(0) = 0 convention
spec_log <- function(x) ifelse(x == 0 | x == -Inf | is.nan(x), 0, log(x))

#' Apply a column-wise transformation using a prefix-to-prefix mapping
transform_cols <- function(dt, in_prefix, out_prefix, fn, by = NULL) {
  cols <- prefixed_cols(dt, in_prefix)
  suffixes <- gsub(paste0("^", in_prefix), "", cols)
  for (s in suffixes) {
    local_s <- s
    if (is.null(by)) {
      dt[, (paste0(out_prefix, local_s)) := fn(dt, local_s)]
    } else {
      dt[, (paste0(out_prefix, local_s)) := fn(.SD, local_s), by = by]
    }
  }
  invisible(dt)
}

#' Sum prefixed columns within each group into one target column
compute_group_total_from_prefix <- function(dt, source_prefix, target_col, by) {
  dt[, (target_col) := 0]
  for (source_col in prefixed_cols(dt, source_prefix)) {
    dt[, (target_col) := get(target_col) + sum(get(source_col)), by = by]
  }
  invisible(dt)
}

#' Sum prefixed columns row-wise into one target column
compute_row_total_from_prefix <- function(dt, source_prefix, target_col) {
  cols <- prefixed_cols(dt, source_prefix)
  if (!length(cols)) {
    dt[, (target_col) := 0]
    return(invisible(dt))
  }
  dt[, (target_col) := rowSums(.SD), .SDcols = cols]
  invisible(dt)
}

#' Build the binary service-mix signature for task support
build_service_mix_id <- function(dt, mix_prefix = "task_mix_") {
  dt[, service_mix_id := ""]
  for (suffix in prefixed_suffixes(dt, mix_prefix)) {
    mix_col <- paste0(mix_prefix, suffix)
    dt[, service_mix_id := paste0(service_mix_id, ifelse(round(get(mix_col), 8) == 0, 0, 1))]
  }
  invisible(dt)
}

#' Reshape wide local-type assignments into a long worker-type lookup
reshape_label_assignments <- function(label_final, n_worker_types) {
  label_final <- data.table::melt(
    data.table::copy(label_final),
    id.vars = "year_loc",
    measure.vars = paste0("type_within_firm", seq_len(n_worker_types)),
    variable.name = "type_within_firm",
    value.name = "worker_type"
  )
  label_final[, type_within_firm := as.numeric(sub("type_within_firm", "", type_within_firm))]
  label_final[, worker_type := as.numeric(worker_type)]
  label_final
}

#' Smallest cut level that yields at most k clusters
within_firm_min <- function(mat, k) {
  if (nrow(mat) <= k) {
    return(c(0))
  }
  clust_res <- hclust(dist(mat, method = "euclidean"), method = "complete")
  clust_res$height[nrow(mat) - k]
}

#' Cluster workers within a firm-quarter
within_firm_clust <- function(mat, cut_level) {
  if (nrow(mat) <= 1) {
    return(c(1))
  }
  clust_res <- hclust(dist(mat, method = "euclidean"), method = "complete")
  cutree(clust_res, h = cut_level)
}

#' Normalize cluster build options for baseline vs bootstrap runs
normalize_cluster_options <- function(mode = c("baseline", "bootstrap"), options = list()) {
  mode <- match.arg(mode)
  defaults <- list(
    render_network_plots = identical(mode, "baseline"),
    allow_disconnected_gamma = identical(mode, "bootstrap")
  )
  utils::modifyList(defaults, options)
}

#' Expand one row per firm-quarter into one row per worker type
expand_to_all_worker_types <- function(dt, cols_tokeep, n_worker_types) {
  base_rows <- unique(dt[, ..cols_tokeep])
  if (!nrow(base_rows)) {
    base_rows[, worker_type := integer()]
    return(base_rows)
  }
  expanded <- base_rows[rep(seq_len(nrow(base_rows)), each = n_worker_types)]
  expanded[, worker_type := rep(seq_len(n_worker_types), times = nrow(base_rows))]
  expanded
}

#' Resolve the saved baseline lookup artifact or a raw lookup table
resolve_worker_type_lookup <- function(worker_type_lookup) {
  if (is.list(worker_type_lookup) && !inherits(worker_type_lookup, "data.table")) {
    if (!("worker_type_lookup" %in% names(worker_type_lookup))) {
      stop("worker_type_lookup list must contain a worker_type_lookup element.")
    }
    worker_type_lookup <- worker_type_lookup$worker_type_lookup
  }
  worker_type_lookup <- data.table::as.data.table(data.table::copy(worker_type_lookup))
  required_cols <- c("location_id", "quarter_year", "staff_id", "worker_type")
  if (!all(required_cols %in% names(worker_type_lookup))) {
    stop("worker_type_lookup must contain: ", paste(required_cols, collapse = ", "))
  }
  stopifnot(data.table::uniqueN(worker_type_lookup[, ..required_cols]) == nrow(worker_type_lookup))
  worker_type_lookup[, worker_type := as.numeric(worker_type)]
  worker_type_lookup
}

#' Build the graph object and shortest paths used for gamma normalization
build_qgraph_paths <- function(county_gamma_dt, plot_path = NULL, edge_color = "blue") {
  if (!requireNamespace("qgraph", quietly = TRUE)) {
    stop("Package qgraph is required to build gamma normalization.")
  }

  value_cols <- setdiff(names(county_gamma_dt), c("location_id", "quarter_year", "county", "loc_quarter"))
  vect_mat <- as.matrix(county_gamma_dt[, ..value_cols])
  vect_mat[round(vect_mat, 3) == 0] <- 0
  vect_mat[vect_mat != 0] <- 1

  dist_mat <- data.table::data.table(expand.grid(seq_len(nrow(vect_mat)), seq_len(nrow(vect_mat))))

  simil_function <- function(vec1, vec2) {
    sum(vec1[vec1 == vec2])
  }

  wrapper_func <- Vectorize(function(loc1, loc2) {
    v1 <- as.numeric(vect_mat[loc1, ])
    v2 <- as.numeric(vect_mat[loc2, ])
    simil_function(v1, v2)
  })

  dist_mat[, match_types := wrapper_func(Var1, Var2)]
  dist_mat <- data.table::dcast(dist_mat, Var1 ~ Var2, value.var = "match_types")
  qgraph_mat <- as.matrix(dist_mat[, -c("Var1")])
  rownames(qgraph_mat) <- rep(seq_len(nrow(dist_mat)), nrow(dist_mat) > 0)
  colnames(qgraph_mat) <- rep(seq_len(nrow(dist_mat)), nrow(dist_mat) > 0)
  qgraph_mat[qgraph_mat < 2] <- 0
  qgraph_mat[qgraph_mat >= 2] <- 1

  if (!is.null(plot_path)) {
    dir.create(dirname(plot_path), recursive = TRUE, showWarnings = FALSE)
    grDevices::png(plot_path, pointsize = 10, width = 1400, height = 1200, res = 300)
    on.exit(grDevices::dev.off(), add = TRUE)
    graph_obj <- qgraph::qgraph(qgraph_mat, layout = "spring", esize = 1, edge.color = edge_color)
  } else {
    graph_obj <- qgraph::qgraph(
      qgraph_mat,
      layout = "spring",
      esize = 1,
      edge.color = edge_color,
      DoNotPlot = TRUE
    )
  }

  list(
    graph = graph_obj,
    paths = qgraph::centrality(graph_obj, all.shortest.paths = TRUE),
    matrix = qgraph_mat
  )
}

#' Add gamma normalization to the wide working data
add_gamma_normalization <- function(
  forgamma_verywide,
  verywide_expanded,
  staff_merged_within,
  supported_quarter_count,
  config,
  cluster_options
) {
  if (isTRUE(cluster_options$render_network_plots)) {
    for (cnty in config$counties_padded) {
      county_2019 <- forgamma_verywide[county == cnty & round(quarter_year) == 2019]
      if (!nrow(county_2019)) {
        next
      }
      build_qgraph_paths(
        county_2019,
        plot_path = file.path("results", "out", "figures", paste0("01_network_2019_", cnty, ".png")),
        edge_color = "black"
      )
    }
  }

  for (cnty in config$counties_padded) {
    county_gamma <- forgamma_verywide[county == cnty]
    if (!nrow(county_gamma)) {
      next
    }

    graph_info <- build_qgraph_paths(county_gamma)
    paths_betweenfirms <- graph_info$paths
    gamma_value_cols <- setdiff(names(county_gamma), c("location_id", "quarter_year", "county", "loc_quarter"))

    largest_firm <- staff_merged_within[
      county == cnty,
      .(count_qt = data.table::uniqueN(quarter_year), emp_max = max(emps)),
      by = "location_id"
    ][count_qt == supported_quarter_count][max(emp_max) == emp_max, ]$location_id[1]

    if (is.na(largest_firm)) {
      largest_firm <- staff_merged_within[
        county == cnty,
        .(count_qt = data.table::uniqueN(quarter_year), emp_max = max(emps)),
        by = "location_id"
      ]
      data.table::setorder(largest_firm, -count_qt, -emp_max)
      largest_firm <- largest_firm[1]$location_id
    }

    preferred_anchor <- which(county_gamma$location_id == largest_firm & county_gamma$quarter_year == 2021.1)
    if (length(preferred_anchor)) {
      lg_num <- preferred_anchor[1]
    } else {
      lg_num <- which.max((county_gamma$location_id == largest_firm) * county_gamma$quarter_year)[1]
    }

    get_loc <- function(x) {
      county_gamma$loc_quarter[x]
    }

    find_vecmax <- function(x, y) {
      v1 <- as.numeric(county_gamma[loc_quarter == get_loc(x), ..gamma_value_cols])
      v2 <- as.numeric(county_gamma[loc_quarter == get_loc(y), ..gamma_value_cols])
      shared <- which(as.logical((round(abs(v1), 8) > 0) * (round(abs(v2), 8) > 0)))
      res <- v1[shared] / v2[shared]
      max(res)
    }

    recurse_down <- function(x) {
      if (length(x) == 2) {
        return(find_vecmax(x[1], x[2]))
      }
      find_vecmax(x[1], x[2]) * recurse_down(x[-1])
    }

    outer_check <- Vectorize(function(loc) {
      x <- which(county_gamma$loc_quarter == loc)
      if (length(paths_betweenfirms$ShortestPaths[x, lg_num][[1]]) > 0) {
        part <- 0
        iter <- 0
        for (p in seq_along(paths_betweenfirms$ShortestPaths[x, lg_num][[1]])) {
          path_goal <- paths_betweenfirms$ShortestPaths[x, lg_num][[1]][[p]]
          holdval <- recurse_down(path_goal)
          if (holdval > 0) {
            part <- part + (holdval)^(-1)
            iter <- iter + 1
          }
        }
        return(part / iter)
      }
      if (lg_num == x) {
        return(1)
      }
      NA_real_
    })

    verywide_expanded[county == cnty, gamma_normalized := outer_check(loc_quarter)]
  }

  if (!isTRUE(cluster_options$allow_disconnected_gamma)) {
    stopifnot(nrow(verywide_expanded[is.na(gamma_normalized)]) == 0)
  } else {
    if (nrow(verywide_expanded[is.na(gamma_normalized)]) != 0) {
      message("Some disconnected")
    }
    verywide_expanded <- verywide_expanded[!is.na(gamma_normalized)]
  }

  verywide_expanded
}

#' Build the staff-task feature tables used by the clustering pipeline
build_staff_task_features <- function(working, config, smooth_parm = NULL) {
  working <- data.table::as.data.table(data.table::copy(working))

  required_cols <- c(
    "customer_id", "staff_id", "location_id", "CSPOP", "county",
    "rep_text_cluster", "quarter_year", "business_id", "clust",
    "location_state", "location_city", "location_zip", "price", "duration"
  )
  if (!all(required_cols %in% names(working))) {
    stop("working is missing required columns: ",
         paste(setdiff(required_cols, names(working)), collapse = ", "))
  }

  stopifnot(nrow(working[is.na(price)]) == 0)

  if (is.null(smooth_parm)) {
    smooth_parm <- mean(working$duration)
  }

  working[, cust_count := data.table::uniqueN(customer_id), by = c("location_id", "quarter_year")]

  staff_task <- working[
    ,
    .(count = .N, duration = as.double(sum(duration)), revenue = sum(price)),
    by = c(
      "staff_id", "location_id", "CSPOP", "county", "rep_text_cluster",
      "quarter_year", "business_id", "clust", "location_state",
      "location_city", "location_zip", "cust_count"
    )
  ]

  staff_task[, staff_num := data.table::frank(staff_id, ties.method = "dense"), by = c("location_id", "quarter_year")]
  staffnum_xwalk <- unique(staff_task[, .(location_id, quarter_year, staff_num, staff_id)])

  staff_task[
    ,
    c("staff_revenue", "emp_duration") := list(sum(revenue), sum(duration)),
    by = c("location_id", "quarter_year", "staff_num")
  ]
  stopifnot(data.table::uniqueN(staff_task[, c("staff_id", "location_id", "clust", "quarter_year")]) == nrow(staff_task))
  staff_task[
    ,
    c("service_types", "mean_duration_firm", "tot_duration", "emps", "revenue") :=
      list(
        data.table::uniqueN(clust),
        mean(duration),
        sum(duration),
        data.table::uniqueN(staff_id),
        sum(revenue)
      ),
    by = c("location_id", "quarter_year")
  ]

  staff_task <- data.table::dcast(
    staff_task,
    location_state + location_city + location_zip + CSPOP + county + location_id +
      quarter_year + business_id + staff_id + staff_num + tot_duration +
      emp_duration + staff_revenue + emps + service_types + revenue +
      mean_duration_firm + cust_count ~ clust,
    value.var = c("rep_text_cluster", "duration"),
    fill = 0
  )

  transform_cols(
    staff_task, "duration_", "firm_duration_",
    function(d, s) sum(d[[paste0("duration_", s)]]),
    by = c("location_id", "quarter_year")
  )
  transform_cols(
    staff_task, "firm_duration_", "task_mix_",
    function(d, s) d[[paste0("firm_duration_", s)]] / d$tot_duration
  )
  staff_task[, e_frac := emp_duration / tot_duration]

  build_service_mix_id(staff_task, "task_mix_")
  staff_task[, worker_spec := duration_1 / emp_duration]
  staff_task[, staff_rev_perhour := (staff_revenue / emp_duration / 60)]
  staff_task[, std_worker_spec := worker_spec / sd(worker_spec)]
  staff_task[, std_staff_rev_perhour := staff_rev_perhour / sd(staff_rev_perhour)]

  transform_cols(
    staff_task, "duration_", "E_",
    function(d, s) d[[paste0("duration_", s)]] / d$emp_duration
  )
  transform_cols(
    staff_task, "duration_", "B_raw_",
    function(d, s) d[[paste0("duration_", s)]] / d$tot_duration
  )
  transform_cols(
    staff_task, "B_raw_", "mipart_",
    function(d, s) {
      d[[paste0("B_raw_", s)]] *
        spec_log(d[[paste0("B_raw_", s)]] / d[[paste0("task_mix_", s)]] / d$e_frac)
    }
  )

  B <- as.matrix(staff_task[, .SD, .SDcols = grep("^mipart", names(staff_task))])
  stopifnot(all(!is.na(B)))
  stopifnot(all(!is.nan(B)))
  stopifnot(all(is.finite(B)))
  staff_task[, s_index := rowSums(B)]
  staff_task[, s_index := sum(s_index), by = c("quarter_year", "location_id")]
  staff_task <- staff_task[, .SD, .SDcols = -grep("^mipart", names(staff_task))]
  rm(B)

  staff_task_full <- data.table::copy(staff_task)

  staff_task <- staff_task[
    quarter_year %in% config$estimation_quarters &
      county %in% config$counties_padded
  ]

  transform_cols(
    staff_task, "duration_", "smooth_duration_",
    function(d, s) d[[paste0("duration_", s)]] + smooth_parm
  )
  compute_group_total_from_prefix(staff_task, "smooth_duration_", "smooth_tot_duration", c("location_id", "quarter_year"))
  transform_cols(
    staff_task, "smooth_duration_", "B_",
    function(d, s) d[[paste0("smooth_duration_", s)]] / d$smooth_tot_duration
  )

  staff_task_full_smoothed <- data.table::copy(staff_task)

  staff_task[, smooth_e_frac := 0]
  transform_cols(
    staff_task, "B_raw_", "Btilde_raw_",
    function(d, s) d[[paste0("B_raw_", s)]] / d$e_frac
  )

  raw_btilde_cols <- prefixed_cols(staff_task, "Btilde_raw_")
  staff_task[
    ,
    min_cutlevel := within_firm_min(as.matrix(.SD), config$n_worker_types),
    by = c("location_id", "quarter_year"),
    .SDcols = raw_btilde_cols
  ]

  list(
    staff_task_full = staff_task_full,
    staff_task_full_smoothed = staff_task_full_smoothed,
    staff_task = staff_task,
    staffnum_xwalk = staffnum_xwalk,
    smooth_parm = smooth_parm
  )
}

#' Assign baseline worker types once on the unweighted sample
assign_worker_types_baseline <- function(staff_task, staffnum_xwalk, config) {
  staff_task <- data.table::as.data.table(data.table::copy(staff_task))
  staffnum_xwalk <- data.table::as.data.table(data.table::copy(staffnum_xwalk))

  raw_btilde_cols <- prefixed_cols(staff_task, "Btilde_raw_")

  if (isTRUE(config$verbose_logging)) {
    for (cnty in config$counties_padded) {
      print(max(staff_task[county == cnty]$min_cutlevel))
    }
  }

  staff_task[, county_cutlevel := max(min_cutlevel), by = county]
  staff_task[
    ,
    type_within_firm := within_firm_clust(as.matrix(.SD), county_cutlevel[1]),
    by = c("location_id", "quarter_year"),
    .SDcols = raw_btilde_cols
  ]
  staff_task[, types_observed_firm := max(type_within_firm), by = c("location_id", "quarter_year")]

  within_firm_assignments <- unique(
    staff_task[, .(location_id, quarter_year, staff_id, staff_num, type_within_firm)]
  )

  if (isTRUE(config$verbose_logging)) {
    hist(unique(staff_task[, c("types_observed_firm", "location_id", "quarter_year")])$types_observed_firm)
    print(table(unique(staff_task[, c("types_observed_firm", "location_id", "quarter_year")])$types_observed_firm))
  }

  if (length(raw_btilde_cols)) {
    staff_task[, (raw_btilde_cols) := NULL]
  }
  compute_row_total_from_prefix(staff_task, "B_", "smooth_e_frac")
  transform_cols(
    staff_task, "B_", "Btilde_",
    function(d, s) d[[paste0("B_", s)]] / d$smooth_e_frac
  )
  stopifnot(max(staff_task$types_observed_firm) <= config$n_worker_types)

  staff_merged_within <- staff_task[
    ,
    lapply(.SD, sum),
    .SDcols = prefixed_cols(staff_task, "smooth_duration_"),
    by = c(
      "county", "location_id", "quarter_year", "type_within_firm",
      "service_types", "types_observed_firm", "service_mix_id", "emps"
    )
  ]

  compute_group_total_from_prefix(
    staff_merged_within,
    "smooth_duration_",
    "smooth_tot_duration",
    c("location_id", "quarter_year")
  )
  transform_cols(
    staff_merged_within, "smooth_duration_", "B_",
    function(d, s) d[[paste0("smooth_duration_", s)]] / d$smooth_tot_duration
  )
  compute_row_total_from_prefix(staff_merged_within, "B_", "smooth_e_frac")
  transform_cols(
    staff_merged_within, "B_", "Btilde_",
    function(d, s) d[[paste0("B_", s)]] / d$smooth_e_frac
  )

  all_tasks <- paste(rep("1", length(prefixed_cols(staff_merged_within, "Btilde_"))), collapse = "")
  staff_comparable <- staff_merged_within[types_observed_firm > 1 & service_mix_id == all_tasks]

  staff_comparable_fivers <- staff_merged_within[
    types_observed_firm == config$n_worker_types & service_mix_id == all_tasks
  ]
  reference_firms <- unique(staff_comparable_fivers[
    ,
    .(
      county,
      ref_firm = location_id[which.max(emps)],
      ref_quarter = quarter_year[which.max(emps)]
    ),
    by = county
  ][, .(county, ref_firm, ref_quarter)])

  staff_comparable_fivers <- merge(staff_comparable_fivers, reference_firms, by = "county")
  staff_comparable_fivers <- staff_comparable_fivers[location_id == ref_firm & quarter_year == ref_quarter]
  stopifnot(nrow(staff_comparable_fivers) == config$n_worker_types * length(config$counties_padded))

  staff_comparable[, year_loc := paste0(location_id, " - ", quarter_year)]
  label_final <- data.table::data.table()

  comparable_btilde_cols <- prefixed_cols(staff_comparable, "Btilde_")
  reference_btilde_cols <- prefixed_cols(staff_comparable_fivers, "Btilde_")

  for (cur_loc_quarter in unique(staff_comparable$year_loc)) {
    cur_county <- unique(staff_comparable[year_loc == cur_loc_quarter]$county)
    mat <- as.matrix(staff_comparable[year_loc == cur_loc_quarter, ..comparable_btilde_cols])
    mat_comp <- as.matrix(staff_comparable_fivers[county == cur_county, ..reference_btilde_cols])
    allcombos <- combn(seq_len(config$n_worker_types), nrow(mat), simplify = FALSE)
    allpairs <- combn(seq_len(nrow(mat)), 2, simplify = FALSE)

    res <- c()
    for (i in seq_along(allcombos)) {
      tot <- 0
      for (pair in allpairs) {
        current <- log(mat[pair[1], ] / mat[pair[2], ])
        current <- current / sqrt(sum(current^2))
        comp <- log(mat_comp[allcombos[[i]][pair[1]], ] / mat_comp[allcombos[[i]][pair[2]], ])
        comp <- comp / sqrt(sum(comp^2))
        tot <- tot + sum(abs(current - comp))
      }
      res[i] <- tot
    }
    stopifnot(length(which.min(res)) == 1)
    label_final <- rbind(label_final, t(c(cur_loc_quarter, allcombos[[which.min(res)]])), fill = TRUE)
  }

  stopifnot(all(staff_comparable[, type_within_firm == 1:.N, by = year_loc]$V1))
  colnames(label_final) <- c("year_loc", paste0("type_within_firm", seq_len(config$n_worker_types)))
  label_final <- reshape_label_assignments(label_final, config$n_worker_types)

  staff_task[, year_loc := paste0(location_id, " - ", quarter_year)]
  staff_task_labeled <- merge(staff_task, label_final, by = c("year_loc", "type_within_firm"), all.x = TRUE)

  worker_type_lookup <- unique(
    staff_task_labeled[!is.na(worker_type), .(
      location_id,
      quarter_year,
      staff_id,
      staff_num,
      county,
      worker_type
    )]
  )

  xwalk_check <- merge(
    unique(worker_type_lookup[, .(location_id, quarter_year, staff_num, staff_id)]),
    staffnum_xwalk,
    by = c("location_id", "quarter_year", "staff_num", "staff_id")
  )
  stopifnot(nrow(xwalk_check) == nrow(unique(worker_type_lookup[, .(location_id, quarter_year, staff_num, staff_id)])))

  if (isTRUE(config$verbose_logging)) {
    check <- unique(worker_type_lookup[, .(staff_id, quarter_year, worker_type)])
    check[, count := .N, by = staff_id]
    check[, first := min(quarter_year), by = staff_id]
    data.table::setkey(check, "staff_id", "quarter_year")
    check[, same_flag := data.table::shift(worker_type) == worker_type, by = staff_id]
    print(mean(check[count > 1 & quarter_year > first]$same_flag))
  }

  list(
    staff_task_labeled = staff_task_labeled,
    worker_type_lookup = worker_type_lookup,
    supported_staff_keys = unique(worker_type_lookup[, .(location_id, quarter_year, staff_id)]),
    reference_firms = reference_firms,
    county_cutlevels = unique(staff_task[, .(county, county_cutlevel)]),
    within_firm_assignments = within_firm_assignments
  )
}

#' Build the final working data from fixed worker-type labels
build_working_from_labels <- function(
  staff_task,
  worker_type_lookup,
  aux_data,
  config,
  mode = c("baseline", "bootstrap"),
  options = list()
) {
  mode <- match.arg(mode)
  cluster_options <- normalize_cluster_options(mode, options)
  staff_task <- data.table::as.data.table(data.table::copy(staff_task))

  supported_staff_keys <- NULL
  if (is.list(worker_type_lookup) && !inherits(worker_type_lookup, "data.table")) {
    supported_staff_keys <- worker_type_lookup$supported_staff_keys
  }
  worker_type_lookup <- resolve_worker_type_lookup(worker_type_lookup)

  if (is.null(supported_staff_keys)) {
    supported_staff_keys <- unique(worker_type_lookup[, .(location_id, quarter_year, staff_id)])
  } else {
    supported_staff_keys <- data.table::as.data.table(data.table::copy(supported_staff_keys))
    required_key_cols <- c("location_id", "quarter_year", "staff_id")
    if (!all(required_key_cols %in% names(supported_staff_keys))) {
      stop("supported_staff_keys must contain: ", paste(required_key_cols, collapse = ", "))
    }
    stopifnot(data.table::uniqueN(supported_staff_keys[, ..required_key_cols]) == nrow(supported_staff_keys))

    support_lookup <- merge(
      supported_staff_keys,
      worker_type_lookup[, .(location_id, quarter_year, staff_id, worker_type)],
      by = c("location_id", "quarter_year", "staff_id"),
      all.x = TRUE
    )
    if (nrow(support_lookup[is.na(worker_type)]) > 0) {
      stop("worker_type_lookup is missing one or more supported staff keys.")
    }
  }

  required_aux <- c("cex", "county_msa_xwalk", "qcew")
  if (!all(required_aux %in% names(aux_data))) {
    stop("aux_data must contain: ", paste(required_aux, collapse = ", "))
  }

  supported_firms <- unique(supported_staff_keys[, .(location_id, quarter_year)])
  staff_task_supported <- merge(staff_task, supported_firms, by = c("location_id", "quarter_year"))

  if ("worker_type" %in% names(staff_task_supported)) {
    staff_task_supported[, worker_type := NULL]
  }

  current_keys <- unique(staff_task_supported[, .(location_id, quarter_year, staff_id)])
  input_presence <- merge(
    supported_staff_keys,
    current_keys[, present := TRUE],
    by = c("location_id", "quarter_year", "staff_id"),
    all.x = TRUE
  )
  if (nrow(input_presence[is.na(present)]) > 0) {
    stop("Current staff_task is missing baseline worker_type_lookup keys. Positive-weight bootstrap support was not preserved.")
  }

  staff_task_labeled <- merge(
    staff_task_supported,
    worker_type_lookup[, .(location_id, quarter_year, staff_id, worker_type)],
    by = c("location_id", "quarter_year", "staff_id"),
    all = FALSE
  )

  cols_tokeep <- c(
    "location_id", "county", "quarter_year", "location_state", "location_city",
    "location_zip", "business_id", "smooth_tot_duration", "emps", "service_types",
    "revenue", "tot_duration", "cust_count", "CSPOP", "s_index"
  )
  cols_tokeep <- c(cols_tokeep, prefixed_cols(staff_task_labeled, "task_mix_"))
  stopifnot(
    nrow(unique(staff_task_labeled[, .SD, .SDcols = cols_tokeep])) ==
      data.table::uniqueN(staff_task_labeled[, c("quarter_year", "location_id")])
  )

  compute_row_total_from_prefix(staff_task_labeled, "smooth_duration_", "smooth_emp_duration")
  staff_merged_within <- staff_task_labeled[
    ,
    lapply(.SD, sum),
    .SDcols = c(
      prefixed_cols(staff_task_labeled, "smooth_duration_"),
      "smooth_emp_duration",
      prefixed_cols(staff_task_labeled, "duration_"),
      "emp_duration"
    ),
    by = c(cols_tokeep, "worker_type")
  ]
  data.table::setkey(staff_merged_within, "location_id", "quarter_year", "worker_type")
  stopifnot(data.table::uniqueN(staff_merged_within[, c("location_id", "quarter_year", "worker_type")]) == nrow(staff_merged_within))

  expanded_data <- expand_to_all_worker_types(staff_merged_within, cols_tokeep, config$n_worker_types)
  expanded_data <- merge(expanded_data, staff_merged_within, by = c(cols_tokeep, "worker_type"), all.x = TRUE)
  for (col in c(prefixed_cols(expanded_data, "smooth_duration_"), "smooth_emp_duration")) {
    expanded_data[, (col) := ifelse(is.na(get(col)), 0, get(col))]
  }
  for (col in c(prefixed_cols(expanded_data, "duration_"), "emp_duration")) {
    expanded_data[, (col) := ifelse(is.na(get(col)), 0, get(col))]
  }

  transform_cols(
    expanded_data, "smooth_duration_", "B_",
    function(d, s) d[[paste0("smooth_duration_", s)]] / d$smooth_tot_duration
  )
  transform_cols(
    expanded_data, "smooth_duration_", "BdivE_",
    function(d, s) d[[paste0("smooth_duration_", s)]] / d$smooth_emp_duration
  )
  expanded_data[, E := smooth_emp_duration / smooth_tot_duration]

  expanded_data[, E_raw := emp_duration / tot_duration]
  transform_cols(
    expanded_data, "duration_", "B_raw_",
    function(d, s) d[[paste0("duration_", s)]] / d$tot_duration
  )

  fmla <- stats::as.formula(
    paste(
      paste(prefixed_cols(expanded_data, "task_mix_"), collapse = "+"),
      "+location_id+quarter_year+location_zip+county+tot_duration+smooth_tot_duration+revenue+cust_count+CSPOP+s_index~worker_type"
    )
  )
  verywide_expanded <- data.table::dcast(
    expanded_data,
    fmla,
    value.var = c(prefixed_cols(expanded_data, "B_"), "E", prefixed_cols(expanded_data, "B_raw_"), "E_raw")
  )
  stopifnot(all(verywide_expanded[, .(.N), by = location_id]$N <= 12))

  expanded_data <- expand_to_all_worker_types(staff_merged_within, cols_tokeep, config$n_worker_types)
  temp <- data.table::copy(expanded_data)
  for (i in seq_len(config$n_worker_types - 2)) {
    temp <- rbind(temp, expanded_data)
  }
  expanded_data <- data.table::copy(temp)
  rm(temp)
  expanded_data[, compare_type := setdiff(seq_len(config$n_worker_types), worker_type), by = c(cols_tokeep, "worker_type")]
  stopifnot(nrow(expanded_data) == data.table::uniqueN(expanded_data[, .SD, .SDcols = c(cols_tokeep, "worker_type", "compare_type")]))
  expanded_data <- merge(expanded_data, staff_merged_within, by = c(cols_tokeep, "worker_type"), all.x = TRUE)
  temp <- data.table::copy(staff_merged_within)
  data.table::setnames(temp, prefixed_cols(temp, "smooth_duration_"), paste0("compare_duration_", seq_along(prefixed_cols(temp, "smooth_duration_"))))
  data.table::setnames(temp, "smooth_emp_duration", "compare_emp_duration")
  data.table::setnames(temp, "worker_type", "compare_type")
  expanded_data <- merge(expanded_data, temp, by = c(cols_tokeep, "compare_type"), all.x = TRUE)
  rm(temp)

  transform_cols(
    expanded_data, "smooth_duration_", "BdivE_",
    function(d, s) d[[paste0("smooth_duration_", s)]] / d$smooth_emp_duration
  )
  transform_cols(
    expanded_data, "smooth_duration_", "compare_BdivE_",
    function(d, s) d[[paste0("compare_duration_", s)]] / d$compare_emp_duration
  )
  transform_cols(
    expanded_data, "smooth_duration_", "ratio_",
    function(d, s) log(d[[paste0("compare_BdivE_", s)]] / d[[paste0("BdivE_", s)]])
  )
  transform_cols(
    expanded_data, "smooth_duration_", "raw_fracs_",
    function(d, s) pmin(d[[paste0("compare_BdivE_", s)]], d[[paste0("BdivE_", s)]])
  )
  transform_cols(
    expanded_data, "ratio_", "ratio_",
    function(d, s) ifelse(round(d[[paste0("compare_BdivE_", s)]], 3) == 0, NA, d[[paste0("ratio_", s)]])
  )
  transform_cols(
    expanded_data, "ratio_", "ratio_",
    function(d, s) ifelse(round(d[[paste0("BdivE_", s)]], 3) == 0, NA, d[[paste0("ratio_", s)]])
  )

  forgamma_verywide <- data.table::dcast(
    expanded_data,
    location_id + quarter_year + county ~ worker_type + compare_type,
    value.var = prefixed_cols(expanded_data, "ratio_")
  )
  transform_cols(
    forgamma_verywide, "ratio_", "ratio_",
    function(d, s) ifelse(is.finite(d[[paste0("ratio_", s)]]), d[[paste0("ratio_", s)]], 0)
  )

  rawfracs_verywide <- data.table::dcast(
    expanded_data,
    location_id + quarter_year + county ~ worker_type + compare_type,
    value.var = prefixed_cols(expanded_data, "raw_fracs_")
  )
  transform_cols(
    rawfracs_verywide, "raw_fracs_", "raw_fracs_",
    function(d, s) ifelse(is.na(d[[paste0("raw_fracs_", s)]]), 0, d[[paste0("raw_fracs_", s)]])
  )
  transform_cols(
    rawfracs_verywide, "raw_fracs_", "raw_fracs_",
    function(d, s) ifelse(is.finite(d[[paste0("raw_fracs_", s)]]), d[[paste0("raw_fracs_", s)]], 0)
  )

  forgamma_verywide[, loc_quarter := paste0(location_id, "-", as.character(quarter_year))]
  verywide_expanded[, loc_quarter := paste0(location_id, "-", as.character(quarter_year))]
  stopifnot(data.table::uniqueN(forgamma_verywide[, "loc_quarter"]) == nrow(forgamma_verywide))

  verywide_expanded <- add_gamma_normalization(
    forgamma_verywide = forgamma_verywide,
    verywide_expanded = verywide_expanded,
    staff_merged_within = staff_merged_within,
    supported_quarter_count = data.table::uniqueN(staff_task_labeled$quarter_year),
    config = config,
    cluster_options = cluster_options
  )

  verywide_expanded[, cust_price := revenue / cust_count]
  verywide_expanded[, avg_labor := tot_duration / cust_count / 60]
  verywide_expanded[, CSPOP := as.numeric(CSPOP)]
  verywide_expanded[, county := as.numeric(county)]
  verywide_expanded[, salon_share_subdiv := cust_count / CSPOP]

  cex <- data.table::as.data.table(data.table::copy(aux_data$cex))
  county_msa_xwalk <- data.table::as.data.table(data.table::copy(aux_data$county_msa_xwalk))
  qcew <- data.table::as.data.table(data.table::copy(aux_data$qcew))

  cex[, outside_share := nohc_count / count_sample]
  cex <- cex[stringr::str_detect(PSU, "S") > 0]
  cex <- unique(cex)
  stopifnot(nrow(cex) == data.table::uniqueN(cex[, c("quarter_year", "PSU")]))
  cex <- merge(cex, county_msa_xwalk, by = "PSU", all.x = TRUE, allow.cartesian = TRUE)
  stopifnot(nrow(cex) == data.table::uniqueN(cex[, c("quarter_year", "county")]))

  verywide_expanded <- merge(
    verywide_expanded,
    cex[, c("county", "quarter_year", "outside_share")],
    by = c("county", "quarter_year"),
    all.x = TRUE
  )
  stopifnot(nrow(verywide_expanded[is.na(outside_share)]) == 0)

  qcew <- qcew[own_code == 5 & !is.na(county)]
  stopifnot(nrow(qcew) == data.table::uniqueN(qcew[, c("county", "quarter_year")]))
  qcew[, avg_wage_qtr := total_qtrly_wages / qtrly_estabs]
  verywide_expanded <- merge(
    verywide_expanded,
    qcew[, c("county", "quarter_year", "avg_wage_qtr", "avg_wkly_wage")],
    by = c("county", "quarter_year"),
    all.x = TRUE
  )
  stopifnot(nrow(verywide_expanded[(county %in% config$counties) & is.na(avg_wage_qtr)]) == 0)

  list(
    verywide_expanded = verywide_expanded,
    staff_task_labeled = staff_task_labeled,
    staff_merged_within = staff_merged_within,
    worker_type_lookup = worker_type_lookup
  )
}





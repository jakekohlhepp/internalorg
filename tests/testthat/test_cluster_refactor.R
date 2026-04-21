context('Cluster refactor')

if (file.exists('cluster.R')) {
  source('cluster.R', local = TRUE)
} else {
  source(file.path('..', '..', 'cluster.R'), local = TRUE)
}

TEST_CLUSTER_CONFIG <- list(
  counties = c('6037'),
  counties_padded = c('06037'),
  estimation_quarters = c(2021.1),
  n_worker_types = 2,
  verbose_logging = FALSE
)

make_cluster_fixture <- function(location_weights = c(L1 = 1, L2 = 1)) {
  base <- data.table::rbindlist(list(
    data.table::data.table(
      customer_id = 'c1', staff_id = 's1', location_id = 'L1', CSPOP = 1000,
      county = '06037', rep_text_cluster = 'task1', quarter_year = 2021.1,
      business_id = 'B1', clust = 1, location_state = 'CA', location_city = 'LA',
      location_zip = '90001', price = 120, duration = 60
    ),
    data.table::data.table(
      customer_id = 'c2', staff_id = 's1', location_id = 'L1', CSPOP = 1000,
      county = '06037', rep_text_cluster = 'task2', quarter_year = 2021.1,
      business_id = 'B1', clust = 2, location_state = 'CA', location_city = 'LA',
      location_zip = '90001', price = 15, duration = 5
    ),
    data.table::data.table(
      customer_id = 'c3', staff_id = 's2', location_id = 'L1', CSPOP = 1000,
      county = '06037', rep_text_cluster = 'task1', quarter_year = 2021.1,
      business_id = 'B1', clust = 1, location_state = 'CA', location_city = 'LA',
      location_zip = '90001', price = 20, duration = 10
    ),
    data.table::data.table(
      customer_id = 'c4', staff_id = 's2', location_id = 'L1', CSPOP = 1000,
      county = '06037', rep_text_cluster = 'task2', quarter_year = 2021.1,
      business_id = 'B1', clust = 2, location_state = 'CA', location_city = 'LA',
      location_zip = '90001', price = 100, duration = 50
    ),
    data.table::data.table(
      customer_id = 'c5', staff_id = 's3', location_id = 'L2', CSPOP = 1200,
      county = '06037', rep_text_cluster = 'task1', quarter_year = 2021.1,
      business_id = 'B2', clust = 1, location_state = 'CA', location_city = 'LA',
      location_zip = '90002', price = 110, duration = 55
    ),
    data.table::data.table(
      customer_id = 'c6', staff_id = 's3', location_id = 'L2', CSPOP = 1200,
      county = '06037', rep_text_cluster = 'task2', quarter_year = 2021.1,
      business_id = 'B2', clust = 2, location_state = 'CA', location_city = 'LA',
      location_zip = '90002', price = 25, duration = 15
    ),
    data.table::data.table(
      customer_id = 'c7', staff_id = 's4', location_id = 'L2', CSPOP = 1200,
      county = '06037', rep_text_cluster = 'task1', quarter_year = 2021.1,
      business_id = 'B2', clust = 1, location_state = 'CA', location_city = 'LA',
      location_zip = '90002', price = 30, duration = 15
    ),
    data.table::data.table(
      customer_id = 'c8', staff_id = 's4', location_id = 'L2', CSPOP = 1200,
      county = '06037', rep_text_cluster = 'task2', quarter_year = 2021.1,
      business_id = 'B2', clust = 2, location_state = 'CA', location_city = 'LA',
      location_zip = '90002', price = 110, duration = 55
    )
  ))

  base[, weight := unname(location_weights[location_id])]
  base[, `:=`(duration = duration * weight, price = price * weight, weight = NULL)]
  base
}

make_cluster_aux_data <- function() {
  list(
    cex = data.table::data.table(
      quarter_year = 2021.1,
      PSU = 'S_TEST',
      nohc_count = 20,
      count_sample = 100
    ),
    county_msa_xwalk = data.table::data.table(
      PSU = 'S_TEST',
      county = 6037
    ),
    qcew = data.table::data.table(
      county = 6037,
      quarter_year = 2021.1,
      own_code = 5,
      total_qtrly_wages = 100000,
      qtrly_estabs = 10,
      avg_wkly_wage = 1000
    )
  )
}

ordered_feature_slice <- function(staff_task) {
  cols <- c(
    'location_id', 'quarter_year', 'staff_id', 'service_mix_id', 'e_frac', 's_index', 'min_cutlevel',
    grep('^task_mix_', names(staff_task), value = TRUE),
    grep('^B_raw_', names(staff_task), value = TRUE),
    grep('^Btilde_raw_', names(staff_task), value = TRUE)
  )
  data.table::copy(staff_task[, ..cols])[order(location_id, quarter_year, staff_id)]
}

ordered_lookup_slice <- function(worker_type_lookup) {
  slice <- data.table::copy(worker_type_lookup[, .(location_id, quarter_year, staff_id, worker_type)])[order(location_id, quarter_year, staff_id)]
  data.table::setkey(slice, NULL)
  slice
}

test_that('positive location weights leave raw clustering inputs unchanged', {
  baseline <- build_staff_task_features(make_cluster_fixture(), TEST_CLUSTER_CONFIG)
  weighted <- build_staff_task_features(make_cluster_fixture(c(L1 = 2, L2 = 3)), TEST_CLUSTER_CONFIG)

  expect_equal(
    ordered_feature_slice(weighted$staff_task),
    ordered_feature_slice(baseline$staff_task),
    tolerance = 1e-10
  )
})

test_that('within-firm clustering is unchanged under positive location weights', {
  baseline <- build_staff_task_features(make_cluster_fixture(), TEST_CLUSTER_CONFIG)
  weighted <- build_staff_task_features(make_cluster_fixture(c(L1 = 2, L2 = 3)), TEST_CLUSTER_CONFIG)

  baseline_assignment <- assign_worker_types_baseline(
    baseline$staff_task,
    baseline$staffnum_xwalk,
    TEST_CLUSTER_CONFIG
  )
  weighted_assignment <- assign_worker_types_baseline(
    weighted$staff_task,
    weighted$staffnum_xwalk,
    TEST_CLUSTER_CONFIG
  )

  expect_equal(
    baseline_assignment$within_firm_assignments[order(location_id, quarter_year, staff_id)],
    weighted_assignment$within_firm_assignments[order(location_id, quarter_year, staff_id)]
  )
})

test_that('bootstrap-mode rebuild reuses baseline worker labels exactly', {
  skip_if_not_installed('qgraph')
  skip_if_not_installed('stringr')

  baseline <- build_staff_task_features(make_cluster_fixture(), TEST_CLUSTER_CONFIG)
  baseline_assignment <- assign_worker_types_baseline(
    baseline$staff_task,
    baseline$staffnum_xwalk,
    TEST_CLUSTER_CONFIG
  )
  weighted <- build_staff_task_features(make_cluster_fixture(c(L1 = 2, L2 = 3)), TEST_CLUSTER_CONFIG)

  rebuilt <- build_working_from_labels(
    weighted$staff_task,
    baseline_assignment$worker_type_lookup,
    make_cluster_aux_data(),
    TEST_CLUSTER_CONFIG,
    mode = 'bootstrap'
  )

  expect_equal(
    ordered_lookup_slice(rebuilt$staff_task_labeled),
    ordered_lookup_slice(baseline_assignment$worker_type_lookup)
  )
})

test_that('build_working_from_labels fails on missing supported keys', {
  baseline <- build_staff_task_features(make_cluster_fixture(), TEST_CLUSTER_CONFIG)
  baseline_assignment <- assign_worker_types_baseline(
    baseline$staff_task,
    baseline$staffnum_xwalk,
    TEST_CLUSTER_CONFIG
  )
  broken_lookup_artifact <- list(
    worker_type_lookup = data.table::copy(baseline_assignment$worker_type_lookup[-1]),
    supported_staff_keys = baseline_assignment$supported_staff_keys
  )

  expect_error(
    suppressWarnings(build_working_from_labels(
      baseline$staff_task,
      broken_lookup_artifact,
      make_cluster_aux_data(),
      TEST_CLUSTER_CONFIG,
      mode = 'bootstrap'
    )),
    'missing|support'
  )
})



context('Prep pipeline helpers')

if (file.exists('config.R')) {
  source('config.R', local = TRUE)
  source(file.path('utils', 'logging.R'), local = TRUE)
} else {
  source(file.path('..', '..', 'config.R'), local = TRUE)
  source(file.path('..', '..', 'utils', 'logging.R'), local = TRUE)
}

helper_env <- environment(get_log_dir)

test_that('project path helpers resolve within the repo', {
  root_dir <- get_project_root(file.path(getwd(), 'tests', 'testthat'))

  expect_true(file.exists(file.path(root_dir, 'config.R')))
  expect_equal(
    normalizePath(project_path('config.R'), winslash = '/'),
    normalizePath(file.path(root_dir, 'config.R'), winslash = '/')
  )
})

test_that('required file and column assertions fail clearly', {
  sample_dt <- data.table::data.table(alpha = 1, beta = 2)

  expect_no_error(assert_required_files(project_path('config.R')))
  expect_error(assert_required_files(project_path('definitely_missing.file')), 'Missing required file')

  expect_no_error(assert_required_columns(sample_dt, c('alpha', 'beta'), 'sample_dt'))
  expect_error(assert_required_columns(sample_dt, c('alpha', 'gamma'), 'sample_dt'), 'gamma')
})

test_that('run_with_logging records warnings without failing', {
  old_log_dir <- helper_env$CONFIG$log_dir
  old_verbose <- helper_env$CONFIG$verbose_logging
  temp_log_dir <- project_path('tests', 'temp_logs')
  temp_script <- project_path('temp_warning_script.R')

  if (dir.exists(temp_log_dir)) {
    unlink(temp_log_dir, recursive = TRUE)
  }

  dir.create(temp_log_dir, recursive = TRUE)
  writeLines(c(
    "warning('test warning')",
    "message('after warning')"
  ), temp_script)

  helper_env$CONFIG$log_dir <- temp_log_dir
  helper_env$CONFIG$verbose_logging <- FALSE

  on.exit({
    if (file.exists(temp_script)) {
      file.remove(temp_script)
    }
    if (dir.exists(temp_log_dir)) {
      unlink(temp_log_dir, recursive = TRUE)
    }
    helper_env$CONFIG$log_dir <- old_log_dir
    helper_env$CONFIG$verbose_logging <- old_verbose
  }, add = TRUE)

  result <- run_with_logging(temp_script, force = TRUE)
  log_path <- file.path(temp_log_dir, 'temp_warning_script.log')
  log_lines <- readLines(log_path)

  expect_true(result$success)
  expect_true(file.exists(log_path))
  expect_true(any(grepl('WARNING: test warning', log_lines, fixed = TRUE)))
})

test_that('write_pipeline_summary honors the requested summary file name', {
  old_log_dir <- helper_env$CONFIG$log_dir
  old_verbose <- helper_env$CONFIG$verbose_logging
  temp_log_dir <- project_path('tests', 'temp_summary_logs')

  if (dir.exists(temp_log_dir)) {
    unlink(temp_log_dir, recursive = TRUE)
  }

  dir.create(temp_log_dir, recursive = TRUE)
  helper_env$CONFIG$log_dir <- temp_log_dir
  helper_env$CONFIG$verbose_logging <- FALSE

  on.exit({
    if (dir.exists(temp_log_dir)) {
      unlink(temp_log_dir, recursive = TRUE)
    }
    helper_env$CONFIG$log_dir <- old_log_dir
    helper_env$CONFIG$verbose_logging <- old_verbose
  }, add = TRUE)

  write_pipeline_summary(
    list(prep = list(success = TRUE, skipped = FALSE, duration = 1, error = NULL)),
    Sys.time(),
    summary_name = 'custom_prep_summary.log',
    title = 'CUSTOM PREP SUMMARY'
  )

  summary_path <- file.path(temp_log_dir, 'custom_prep_summary.log')
  summary_lines <- readLines(summary_path)

  expect_true(file.exists(summary_path))
  expect_true(any(grepl('CUSTOM PREP SUMMARY', summary_lines, fixed = TRUE)))
})

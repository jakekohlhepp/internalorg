# Unit tests for get_os function
# get_os detects the operating system for parallel processing setup

context("get_os function")

# Copy the get_os function for isolated testing
get_os <- function() {
  sysinf <- Sys.info()
  if (!is.null(sysinf)) {
    os <- sysinf['sysname']
    if (os == 'Darwin')
      os <- "osx"
  } else {
    os <- .Platform$OS.type
    if (grepl("^darwin", R.version$os))
      os <- "osx"
    if (grepl("linux-gnu", R.version$os))
      os <- "linux"
  }
  tolower(os)
}

test_that("get_os returns a character string", {
  result <- get_os()
  expect_type(result, "character")
})

test_that("get_os returns lowercase result", {
  result <- get_os()
  expect_equal(result, tolower(result))
})

test_that("get_os returns a recognized OS type", {
  result <- get_os()
  valid_os <- c("windows", "linux", "osx", "unix", "darwin")
  expect_true(result %in% valid_os,
              info = paste("Unexpected OS:", result))
})

test_that("get_os is deterministic", {
  result1 <- get_os()
  result2 <- get_os()
  expect_equal(result1, result2)
})

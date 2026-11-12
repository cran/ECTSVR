context("Test stationarity functions")

test_that("test_stationarity runs without errors", {
  set.seed(123)
  fruit <- rnorm(100, 100, 10)
  oil <- rnorm(100, 50, 5)

  expect_output(results <- suppressWarnings(test_stationarity(oil, fruit, verbose = FALSE)), NA)
  expect_type(results, "list")
  expect_true(all(c("jb_oil", "jb_fruit", "adf_oil", "kpss_oil", "adf_fruit", "kpss_fruit") %in% names(results)))
})

test_that("test_stationarity validates input lengths", {
  fruit <- rnorm(100, 100, 10)
  oil <- rnorm(90, 50, 5)

  expect_error(test_stationarity(oil, fruit), "must have the same length")
})

test_that("stationarity tests return expected structure", {
  set.seed(123)
  fruit <- rnorm(100, 100, 10)
  oil <- rnorm(100, 50, 5)

  results <- suppressWarnings(test_stationarity(oil, fruit, verbose = FALSE))

  # Check that all test results have p-values
  expect_true(is.numeric(results$jb_oil$p.value))
  expect_true(is.numeric(results$adf_oil$p.value))
  expect_true(is.numeric(results$kpss_oil$p.value))
})

test_that("non-stationary series produce expected test results", {
  # Generate random walk (non-stationary)
  set.seed(123)
  oil <- cumsum(rnorm(100, 0, 1))
  fruit <- cumsum(rnorm(100, 0, 1))

  results <- suppressWarnings(test_stationarity(oil, fruit, verbose = FALSE))

  # ADF test should have high p-value for non-stationary series
  expect_true(results$adf_oil$p.value > 0.05)
  expect_true(results$adf_fruit$p.value > 0.05)
})

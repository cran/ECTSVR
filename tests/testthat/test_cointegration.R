context("Test cointegration functions")

test_that("test_cointegration identifies cointegrated series", {
  set.seed(123)
  n <- 200
  trend <- cumsum(rnorm(n, 0, 1))

  oil <- 50 + trend + rnorm(n, 0, 0.5)
  fruit <- 2 * oil + rnorm(n, 0, 1) + 100

  results <- test_cointegration(fruit, oil, verbose = FALSE)

  expect_type(results, "list")
  expect_true(all(c("johansen_test", "beta", "ECT") %in% names(results)))
  expect_length(results$beta, 3)  # Cointegrating vector with constant
})

test_that("test_cointegration handles non-cointegrated series", {
  set.seed(123)
  fruit <- cumsum(rnorm(100, 0, 1))
  oil <- cumsum(rnorm(100, 0, 1))

  results <- test_cointegration(fruit, oil, verbose = FALSE)

  # Should still return structure even if not cointegrated
  expect_type(results, "list")
  expect_true(all(c("johansen_test", "beta", "ECT") %in% names(results)))
})

test_that("test_cointegration validates input lengths", {
  fruit <- rnorm(100, 100, 10)
  oil <- rnorm(90, 50, 5)

  expect_error(test_cointegration(fruit, oil), "must have the same length")
})

test_that("ECT is properly constructed", {
  set.seed(123)
  n <- 100
  trend <- cumsum(rnorm(n, 0, 1))
  oil <- 50 + trend + rnorm(n, 0, 0.5)
  fruit <- 2 * oil + rnorm(n, 0, 1) + 100

  results <- test_cointegration(fruit, oil, verbose = FALSE)

  # ECT should be mean-reverting (cointegrated series)
  expect_true(abs(mean(results$ECT)) < sd(results$ECT))
})

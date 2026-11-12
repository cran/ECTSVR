context("Test calculate_metrics function")

test_that("calculate_metrics returns correct values for perfect predictions", {
  actual <- c(10, 20, 30, 40, 50)
  predicted <- c(10, 20, 30, 40, 50)

  metrics <- calculate_metrics(actual, predicted)

  expect_equal(unname(metrics["RMSE"]), 0)
  expect_equal(unname(metrics["MAE"]), 0)
  expect_equal(unname(metrics["MAPE"]), 0)
  expect_equal(unname(metrics["R2"]), 1)
})

test_that("calculate_metrics handles imperfect predictions", {
  actual <- c(10, 20, 30, 40, 50)
  predicted <- c(12, 22, 28, 42, 48)

  metrics <- calculate_metrics(actual, predicted)

  expect_true(metrics["RMSE"] > 0)
  expect_true(metrics["MAE"] > 0)
  expect_true(metrics["MAPE"] > 0)
  expect_true(metrics["R2"] < 1)
  expect_true(metrics["R2"] > 0)
})

test_that("calculate_metrics handles different scales", {
  actual_small <- c(1, 2, 3, 4, 5)
  actual_large <- c(100, 200, 300, 400, 500)
  predicted <- actual_small + 0.1  # Small error
  predicted_large <- actual_large + 10  # Larger absolute error but similar relative

  metrics_small <- calculate_metrics(actual_small, predicted)
  metrics_large <- calculate_metrics(actual_large, predicted_large)

  # MAPE should be similar regardless of scale
  expect_true(abs(metrics_small["MAPE"] - metrics_large["MAPE"]) < 1)
})

test_that("calculate_metrics handles edge cases", {
  actual <- c(1, 1, 1, 1, 1)
  predicted <- c(1, 1, 1, 1, 1)

  metrics <- calculate_metrics(actual, predicted)

  expect_equal(unname(metrics["RMSE"]), 0)
  expect_equal(unname(metrics["MAE"]), 0)
  expect_equal(unname(metrics["MAPE"]), 0)
  expect_equal(unname(metrics["R2"]), 1)  # Perfect correlation with constant
})

test_that("calculate_metrics validates inputs", {
  actual <- c(1, 2, 3)
  predicted <- c(1, 2)

  expect_error(calculate_metrics(actual, predicted))
})

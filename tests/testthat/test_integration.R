context("Integration tests for complete workflow")

test_that("Complete ECT-SVR workflow runs without errors", {
  # Generate test data
  set.seed(123)
  n <- 150
  trend <- cumsum(rnorm(n, 0, 1))
  oil <- 50 + trend + rnorm(n, 0, 0.5)
  fruit <- 2 * oil + rnorm(n, 0, 1) + 100

  # Run complete analysis
  results <- run_ectsvr_analysis(
    fruit = fruit,
    oil = oil,
    train_ratio = 0.8,
    kernels = c("linear", "radial"),
    verbose = FALSE,
    plot_results = FALSE
  )

  # Check structure
  expect_type(results, "list")
  expect_true(all(c("stationarity", "cointegration", "data",
                    "std_svr", "ect_svr", "evaluation", "best_model") %in% names(results)))

  # Check that models trained successfully
  expect_true(nrow(results$std_svr$results) > 0)
  expect_true(nrow(results$ect_svr$results) > 0)

  # Check that evaluation produced results
  expect_true(nrow(results$evaluation$combined_results) > 0)
})

test_that("ECT-SVR improves over standard SVR for cointegrated data", {
  set.seed(123)
  n <- 200
  trend <- cumsum(rnorm(n, 0, 1))
  oil <- 50 + trend + rnorm(n, 0, 0.5)
  fruit <- 2 * oil + rnorm(n, 0, 1) + 100

  results <- run_ectsvr_analysis(
    fruit, oil,
    train_ratio = 0.8,
    kernels = c("radial"),
    verbose = FALSE,
    plot_results = FALSE
  )

  # Extract test RMSE for best models
  std_rmse <- min(results$std_svr$results$Test_RMSE)
  ect_rmse <- min(results$ect_svr$results$Test_RMSE)

  # ECT-SVR should perform better (lower RMSE) for cointegrated data
  expect_true(ect_rmse < std_rmse)
})

test_that("Workflow handles different input sizes", {
  set.seed(123)

  # Small dataset
  fruit_small <- rnorm(50, 100, 10)
  oil_small <- rnorm(50, 50, 5)

  expect_output(suppressWarnings(
    run_ectsvr_analysis(fruit_small, oil_small, verbose = FALSE, plot_results = FALSE)
  ), NA)

  # Larger dataset
  fruit_large <- rnorm(500, 100, 10)
  oil_large <- rnorm(500, 50, 5)

  expect_output(suppressWarnings(
    run_ectsvr_analysis(fruit_large, oil_large, verbose = FALSE, plot_results = FALSE)
  ), NA)
})

test_that("All kernel types work in integration", {
  set.seed(123)
  n <- 100
  fruit <- rnorm(n, 100, 10)
  oil <- rnorm(n, 50, 5)

  kernels <- c("linear", "polynomial", "radial", "sigmoid")

  for (kernel in kernels) {
    expect_output(suppressWarnings(
      run_ectsvr_analysis(
        fruit, oil,
        kernels = kernel,
        verbose = FALSE,
        plot_results = FALSE
      )
    ), NA)
  }
})

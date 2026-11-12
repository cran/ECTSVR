context("Test SVR model training")

# Helper function to generate test data
generate_test_data <- function(n = 100) {
  set.seed(123)
  trend <- cumsum(rnorm(n, 0, 1))
  oil <- 50 + trend + rnorm(n, 0, 0.5)
  fruit <- 2 * oil + rnorm(n, 0, 1) + 100

  data <- prepare_ectsvr_data(fruit, oil, train_ratio = 0.8)
  return(data)
}

test_that("train_svr_models works with standard features", {
  data <- generate_test_data()

  results <- train_svr_models(
    data$X_train_std, data$X_test_std,
    data$y_train, data$y_test,
    "Standard SVR",
    kernels = c("linear", "radial"), verbose = FALSE
  )

  expect_type(results, "list")
  expect_true(all(c("results", "predictions", "models") %in% names(results)))
  expect_equal(nrow(results$results), 2)  # Two kernels
})

test_that("train_svr_models works with ECT features", {
  data <- generate_test_data()

  results <- train_svr_models(
    data$X_train_ect, data$X_test_ect,
    data$y_train, data$y_test,
    "ECT-SVR",
    kernels = c("linear", "radial"), verbose = FALSE
  )

  expect_type(results, "list")
  expect_true(all(c("results", "predictions", "models") %in% names(results)))
})

test_that("train_svr_models handles all kernel types", {
  data <- generate_test_data()

  results <- train_svr_models(
    data$X_train_std, data$X_test_std,
    data$y_train, data$y_test,
    "Test SVR",
    kernels = c("linear", "polynomial", "radial", "sigmoid"), verbose = FALSE
  )

  expect_equal(nrow(results$results), 4)
  expect_true(all(results$results$Kernel %in% c("linear", "polynomial", "radial", "sigmoid")))
})

test_that("train_svr_models returns valid metrics", {
  data <- generate_test_data()

  results <- train_svr_models(
    data$X_train_std, data$X_test_std,
    data$y_train, data$y_test,
    "Test SVR",
    kernels = c("radial"), verbose = FALSE
  )

  metrics <- results$results

  # Check metric ranges
  expect_true(metrics$Train_RMSE >= 0)
  expect_true(metrics$Test_RMSE >= 0)
  expect_true(metrics$Train_MAE >= 0)
  expect_true(metrics$Test_MAE >= 0)
  expect_true(metrics$Train_MAPE >= 0)
  expect_true(metrics$Test_MAPE >= 0)
  expect_true(metrics$Train_R2 <= 1)
  expect_true(metrics$Test_R2 <= 1)
})

test_that("train_svr_models validates input dimensions", {
  data <- generate_test_data()

  # Mismatched dimensions
  expect_error(
    train_svr_models(
      data$X_train_std, data$X_test_std,
      data$y_train[1:10], data$y_test,  # Wrong length
      "Test SVR"
    ),
    "must equal length"
  )
})

test_that("train_svr_models rejects unknown kernels", {
  data <- generate_test_data()
  expect_error(
    train_svr_models(data$X_train_std, data$X_test_std, data$y_train, data$y_test,
                     "Test SVR", kernels = "rbf", verbose = FALSE),
    "Unknown kernel"
  )
})

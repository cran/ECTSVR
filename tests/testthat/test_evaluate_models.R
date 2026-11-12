context("Test model evaluation functions")

mock_results <- function(rmse_scale = 1) {
  data.frame(
    Kernel = c("linear", "radial"),
    Train_RMSE = c(2, 1.5) * rmse_scale, Train_MAE = c(1.8, 1.3) * rmse_scale,
    Train_MAPE = c(10, 8) * rmse_scale, Train_R2 = c(0.8, 0.85),
    Test_RMSE = c(2.5, 2.0) * rmse_scale, Test_MAE = c(2.2, 1.8) * rmse_scale,
    Test_MAPE = c(12, 9) * rmse_scale, Test_R2 = c(0.75, 0.82),
    stringsAsFactors = FALSE
  )
}

test_that("evaluate_models combines results correctly", {
  comparison <- evaluate_models(list(results = mock_results()),
                                list(results = mock_results(0.8)), verbose = FALSE)
  expect_equal(nrow(comparison$combined_results), 4)
  expect_true(all(comparison$combined_results$Model %in% c("Standard SVR", "ECT-SVR")))
})

test_that("evaluate_models identifies best models", {
  comparison <- evaluate_models(list(results = mock_results()),
                                list(results = mock_results(0.8)), verbose = FALSE)
  expect_equal(comparison$best_models$best_rmse$Model, "ECT-SVR")
  expect_equal(comparison$best_models$best_rmse$Kernel, "radial")
  expect_false(is.null(comparison$best_models$best_r2))
})

test_that("evaluate_models is silent when verbose = FALSE", {
  expect_silent(evaluate_models(list(results = mock_results()),
                                list(results = mock_results(0.8)), verbose = FALSE))
})

context("Test data preparation functions")

test_that("prepare_ectsvr_data creates correct structure", {
  set.seed(123)
  n <- 200
  trend <- cumsum(rnorm(n, 0, 1))
  oil <- 50 + trend + rnorm(n, 0, 0.5)
  fruit <- 2 * oil + rnorm(n, 0, 1) + 100

  data <- prepare_ectsvr_data(fruit, oil, train_ratio = 0.8)

  expect_type(data, "list")
  expect_true(all(c("train", "test", "X_train_std", "X_test_std",
                    "X_train_ect", "X_test_ect", "y_train", "y_test") %in% names(data)))
})

test_that("prepare_ectsvr_data handles different train ratios", {
  set.seed(123)
  n <- 200
  fruit <- rnorm(n, 100, 10)
  oil <- rnorm(n, 50, 5)

  data_70 <- prepare_ectsvr_data(fruit, oil, train_ratio = 0.7)
  data_80 <- prepare_ectsvr_data(fruit, oil, train_ratio = 0.8)
  data_90 <- prepare_ectsvr_data(fruit, oil, train_ratio = 0.9)

  expect_true(nrow(data_70$train) < nrow(data_80$train))
  expect_true(nrow(data_80$train) < nrow(data_90$train))
  expect_true(nrow(data_70$test) > nrow(data_80$test))
  expect_true(nrow(data_80$test) > nrow(data_90$test))
})

test_that("prepare_ectsvr_data validates train_ratio", {
  fruit <- rnorm(100, 100, 10)
  oil <- rnorm(100, 50, 5)

  expect_error(prepare_ectsvr_data(fruit, oil, train_ratio = 0), "between 0 and 1")
  expect_error(prepare_ectsvr_data(fruit, oil, train_ratio = 1), "between 0 and 1")
  expect_error(prepare_ectsvr_data(fruit, oil, train_ratio = 1.5), "between 0 and 1")
})

test_that("ECT is included in enhanced features", {
  set.seed(123)
  n <- 200
  trend <- cumsum(rnorm(n, 0, 1))
  oil <- 50 + trend + rnorm(n, 0, 0.5)
  fruit <- 2 * oil + rnorm(n, 0, 1) + 100

  data <- prepare_ectsvr_data(fruit, oil)

  expect_true("ect" %in% colnames(data$X_train_ect))
  expect_true("ect" %in% colnames(data$X_test_ect))
  expect_false("ect" %in% colnames(data$X_train_std))
  expect_false("ect" %in% colnames(data$X_test_std))
})

test_that("lag features are truly lagged (no leakage of y into features)", {
  set.seed(1)
  fruit <- cumsum(rnorm(60)) + 100
  oil <- cumsum(rnorm(60)) + 50
  d <- prepare_ectsvr_data(fruit, oil, train_ratio = 0.8, lags = 2)
  full <- rbind(d$train, d$test)

  expect_equal(full$y, fruit[3:60])
  expect_equal(full$lag1, fruit[2:59])
  expect_equal(full$lag2, fruit[1:58])
  expect_equal(full$ect, d$ECT[2:59])
  expect_false(isTRUE(all.equal(full$y, full$lag1)))
})

test_that("lags argument controls number of features", {
  set.seed(1)
  fruit <- cumsum(rnorm(60)) + 100
  oil <- cumsum(rnorm(60)) + 50
  d <- prepare_ectsvr_data(fruit, oil, lags = 3)
  expect_equal(colnames(d$X_train_std), c("lag1", "lag2", "lag3"))
  expect_equal(colnames(d$X_train_ect), c("lag1", "lag2", "lag3", "ect"))
})

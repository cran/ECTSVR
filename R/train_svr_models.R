#' Train SVR Models with Different Kernels
#'
#' Trains Support Vector Regression models with different kernel functions.
#' Features are centred and scaled using training-set statistics only.
#'
#' @param X_train A data frame or matrix of training features
#' @param X_test A data frame or matrix of test features
#' @param y_train A numeric vector of training target values
#' @param y_test A numeric vector of test target values
#' @param model_name A character string naming the model type (e.g., "Standard SVR")
#' @param kernels Kernel types: "linear", "polynomial", "radial", "sigmoid"
#'   (default: all four)
#' @param cost Cost parameter C (default: 100)
#' @param gamma Gamma parameter (default: 0.1)
#' @param epsilon Epsilon parameter (default: 0.1)
#' @param verbose Logical; print progress messages (default: TRUE)
#'
#' @return A list containing:
#'   \item{results}{A data frame with performance metrics for each kernel}
#'   \item{predictions}{A list of train/test predictions for each kernel}
#'   \item{models}{A list of trained SVR model objects for each kernel}
#'
#' @examples
#' \dontrun{
#' data <- prepare_ectsvr_data(fruit, oil)
#' results <- train_svr_models(data$X_train_std, data$X_test_std,
#'                             data$y_train, data$y_test, "Standard SVR")
#' print(results$results)
#' }
#'
#' @importFrom caret preProcess
#' @importFrom stats predict
#' @importFrom e1071 svm
#' @export
train_svr_models <- function(X_train, X_test, y_train, y_test,
                             model_name,
                             kernels = c("linear", "polynomial", "radial", "sigmoid"),
                             cost = 100, gamma = 0.1, epsilon = 0.1,
                             verbose = TRUE) {

  if (nrow(X_train) != length(y_train)) {
    stop("Number of rows in X_train must equal length of y_train")
  }
  if (nrow(X_test) != length(y_test)) {
    stop("Number of rows in X_test must equal length of y_test")
  }
  valid <- c("linear", "polynomial", "radial", "sigmoid")
  bad <- setdiff(kernels, valid)
  if (length(bad) > 0) {
    stop("Unknown kernel(s): ", paste(bad, collapse = ", "),
         ". Choose from: ", paste(valid, collapse = ", "))
  }

  # Scaling (training statistics only); identical for all kernels
  pre <- caret::preProcess(X_train, method = c("center", "scale"))
  X_train_scaled <- stats::predict(pre, X_train)
  X_test_scaled <- stats::predict(pre, X_test)

  results <- data.frame()
  predictions_list <- list()
  models_list <- list()

  for (kernel in kernels) {
    if (verbose) cat("\nTraining", model_name, "with", kernel, "kernel...\n")

    svr_model <- switch(kernel,
      linear = e1071::svm(x = X_train_scaled, y = y_train, type = "eps-regression",
                          kernel = "linear", cost = cost, epsilon = epsilon),
      polynomial = e1071::svm(x = X_train_scaled, y = y_train, type = "eps-regression",
                              kernel = "polynomial", cost = cost, gamma = gamma,
                              coef0 = 0, degree = 3, epsilon = epsilon),
      radial = e1071::svm(x = X_train_scaled, y = y_train, type = "eps-regression",
                          kernel = "radial", cost = cost, gamma = gamma,
                          epsilon = epsilon),
      sigmoid = e1071::svm(x = X_train_scaled, y = y_train, type = "eps-regression",
                           kernel = "sigmoid", cost = cost, gamma = gamma,
                           coef0 = 0, epsilon = epsilon)
    )

    pred_train <- as.numeric(stats::predict(svr_model, X_train_scaled))
    pred_test <- as.numeric(stats::predict(svr_model, X_test_scaled))

    predictions_list[[kernel]] <- list(train = pred_train, test = pred_test)
    models_list[[kernel]] <- svr_model

    tr <- calculate_metrics(y_train, pred_train)
    te <- calculate_metrics(y_test, pred_test)

    results <- rbind(results, data.frame(
      Kernel = kernel,
      Train_RMSE = unname(tr["RMSE"]), Train_MAE = unname(tr["MAE"]),
      Train_MAPE = unname(tr["MAPE"]), Train_R2 = unname(tr["R2"]),
      Test_RMSE = unname(te["RMSE"]), Test_MAE = unname(te["MAE"]),
      Test_MAPE = unname(te["MAPE"]), Test_R2 = unname(te["R2"]),
      stringsAsFactors = FALSE
    ))
  }

  list(results = results, predictions = predictions_list, models = models_list)
}

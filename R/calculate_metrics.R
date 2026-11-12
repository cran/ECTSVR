#' Calculate Performance Metrics
#'
#' Calculates RMSE, MAE, MAPE and R-squared (squared Pearson correlation
#' between actual and predicted values).
#'
#' @param actual A numeric vector of actual values
#' @param predicted A numeric vector of predicted values
#'
#' @return A named numeric vector containing:
#'   \item{RMSE}{Root Mean Square Error}
#'   \item{MAE}{Mean Absolute Error}
#'   \item{MAPE}{Mean Absolute Percentage Error (in percent)}
#'   \item{R2}{Squared correlation between actual and predicted values}
#'
#' @examples
#' actual <- c(10, 12, 15, 18, 20)
#' predicted <- c(11, 13, 14, 17, 21)
#' metrics <- calculate_metrics(actual, predicted)
#' print(metrics)
#'
#' @export
calculate_metrics <- function(actual, predicted) {
  if (length(actual) != length(predicted)) {
    stop("actual and predicted must have the same length")
  }
  actual <- as.numeric(actual)
  predicted <- as.numeric(predicted)

  rmse_val <- Metrics::rmse(actual, predicted)
  mae_val <- Metrics::mae(actual, predicted)
  mape_val <- mean(abs((actual - predicted) / actual)) * 100

  # Squared correlation is undefined when either series is constant
  if (stats::sd(actual) == 0 || stats::sd(predicted) == 0) {
    r2_val <- if (isTRUE(all.equal(rmse_val, 0))) 1 else NA_real_
  } else {
    r2_val <- stats::cor(actual, predicted)^2
  }

  c(RMSE = rmse_val, MAE = mae_val, MAPE = mape_val, R2 = r2_val)
}

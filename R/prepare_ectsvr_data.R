#' Prepare Data for ECT-SVR Analysis
#'
#' Performs a Johansen cointegration test, constructs the error correction
#' term (ECT), creates lagged features, and splits the data chronologically
#' into training and test sets.
#'
#' @param fruit A numeric vector of fruit prices
#' @param oil A numeric vector of crude oil prices
#' @param train_ratio A number in (0, 1): proportion of observations used for
#'   training (default: 0.8)
#' @param lags Number of lags of \code{fruit} used as features (default: 2).
#'   Must be at least 1.
#' @param ect_sample Sample used to estimate the cointegrating vector:
#'   \code{"train"} (default; avoids look-ahead into the test period) or
#'   \code{"full"}.
#'
#' @return A list containing:
#'   \item{train}{Training dataset as data frame}
#'   \item{test}{Test dataset as data frame}
#'   \item{X_train_std}{Standard features for training (lags only)}
#'   \item{X_test_std}{Standard features for testing (lags only)}
#'   \item{X_train_ect}{ECT-enhanced features for training}
#'   \item{X_test_ect}{ECT-enhanced features for testing}
#'   \item{y_train}{Training target values}
#'   \item{y_test}{Test target values}
#'   \item{ECT}{Error correction term vector (length of the input series)}
#'   \item{johansen_test}{Johansen test result object}
#'   \item{beta}{Cointegrating vector}
#'
#' @examples
#' \dontrun{
#' fruit <- rnorm(100, 100, 10)
#' oil <- rnorm(100, 50, 5)
#' data <- prepare_ectsvr_data(fruit, oil)
#' str(data)
#' }
#'
#' @importFrom urca ca.jo
#' @export
prepare_ectsvr_data <- function(fruit, oil, train_ratio = 0.8, lags = 2,
                                ect_sample = c("train", "full")) {

  ect_sample <- match.arg(ect_sample)
  fruit <- as.numeric(fruit)
  oil <- as.numeric(oil)

  # Input validation
  if (length(fruit) != length(oil)) {
    stop("fruit and oil vectors must have the same length")
  }
  if (train_ratio <= 0 || train_ratio >= 1) {
    stop("train_ratio must be between 0 and 1")
  }
  if (length(lags) != 1 || lags < 1 || lags != round(lags)) {
    stop("lags must be a single positive integer")
  }
  n_obs <- length(fruit)
  n <- n_obs - lags                      # usable rows after lagging
  train_size <- floor(train_ratio * n)
  if (train_size < 2 || n - train_size < 1) {
    stop("not enough observations for the requested lags and train_ratio")
  }

  # Johansen test / cointegrating vector (training sample only by default,
  # so no information from the test period enters the ECT)
  n_est <- if (ect_sample == "train") lags + train_size else n_obs
  data_mat <- cbind(fruit = fruit[seq_len(n_est)], oil = oil[seq_len(n_est)])
  johansen_test <- urca::ca.jo(data_mat, type = "trace", ecdet = "const", K = 2)
  beta <- johansen_test@V[, 1]

  # Error correction term for the whole sample
  ECT <- beta[1] * fruit + beta[2] * oil + beta[3]

  # Lagged features. NOTE: stats::lag() on plain vectors only shifts the time
  # attribute, so data.frame() would silently return unshifted values
  # (lag1 == y). Lag explicitly by indexing instead.
  idx <- (lags + 1):n_obs
  lag_data <- data.frame(y = fruit[idx])
  for (k in seq_len(lags)) {
    lag_data[[paste0("lag", k)]] <- fruit[idx - k]
  }
  lag_data$ect <- ECT[idx - 1]

  # Chronological split
  train <- lag_data[seq_len(train_size), ]
  test <- lag_data[(train_size + 1):n, ]

  std_cols <- paste0("lag", seq_len(lags))
  ect_cols <- c(std_cols, "ect")

  list(
    train = train,
    test = test,
    X_train_std = train[, std_cols, drop = FALSE],
    X_test_std = test[, std_cols, drop = FALSE],
    X_train_ect = train[, ect_cols, drop = FALSE],
    X_test_ect = test[, ect_cols, drop = FALSE],
    y_train = train$y,
    y_test = test$y,
    ECT = ECT,
    johansen_test = johansen_test,
    beta = beta
  )
}

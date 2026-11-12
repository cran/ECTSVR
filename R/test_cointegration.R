#' Perform Johansen Cointegration Test
#'
#' Performs the Johansen cointegration test on fruit and oil price series and
#' extracts the cointegrating vector and error correction term.
#'
#' @param fruit A numeric vector of fruit prices
#' @param oil A numeric vector of crude oil prices
#' @param verbose Logical; print the test summary and cointegrating vector
#'   (default: TRUE)
#'
#' @return A list containing:
#'   \item{johansen_test}{The Johansen test result object from urca::ca.jo}
#'   \item{beta}{The first cointegrating vector}
#'   \item{ECT}{The error correction term calculated from the cointegrating vector}
#'
#' @examples
#' \dontrun{
#' fruit <- rnorm(100, 100, 10)
#' oil <- rnorm(100, 50, 5)
#' coint <- test_cointegration(fruit, oil)
#' print(coint$beta)
#' plot(coint$ECT, type = "l")
#' }
#'
#' @importFrom urca ca.jo
#' @importMethodsFrom urca summary
#' @export
test_cointegration <- function(fruit, oil, verbose = TRUE) {

  if (length(fruit) != length(oil)) {
    stop("fruit and oil vectors must have the same length")
  }

  data_mat <- cbind(fruit = as.numeric(fruit), oil = as.numeric(oil))
  johansen_test <- urca::ca.jo(data_mat, type = "trace", ecdet = "const", K = 2)

  beta <- johansen_test@V[, 1]
  ECT <- beta[1] * fruit + beta[2] * oil + beta[3]

  if (verbose) {
    cat("\n=== Johansen Co-integration Test ===\n")
    # summary() is not auto-printed inside a function: print explicitly
    print(summary(johansen_test))
    cat("\nCointegrating Vector:\n")
    print(beta)
  }

  list(johansen_test = johansen_test, beta = beta, ECT = ECT)
}

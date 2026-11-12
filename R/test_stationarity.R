#' Perform Stationarity Tests on Time Series
#'
#' Performs Jarque-Bera, Augmented Dickey-Fuller (ADF) and KPSS tests on the
#' oil and fruit price series.
#'
#' @param oil A numeric vector of oil prices
#' @param fruit A numeric vector of fruit prices
#' @param verbose Logical; print the test results (default: TRUE)
#'
#' @return A list containing test results:
#'   \item{jb_oil}{Jarque-Bera test result for oil (tseries::jarque.bera.test)}
#'   \item{jb_fruit}{Jarque-Bera test result for fruit}
#'   \item{adf_oil}{ADF test result for oil (tseries::adf.test)}
#'   \item{kpss_oil}{KPSS test result for oil (tseries::kpss.test)}
#'   \item{adf_fruit}{ADF test result for fruit}
#'   \item{kpss_fruit}{KPSS test result for fruit}
#'
#' @examples
#' \dontrun{
#' fruit <- rnorm(100, 100, 10)
#' oil <- rnorm(100, 50, 5)
#' results <- test_stationarity(oil, fruit)
#' print(results$adf_oil)
#' }
#'
#' @importFrom tseries jarque.bera.test adf.test kpss.test
#' @export
test_stationarity <- function(oil, fruit, verbose = TRUE) {

  if (length(oil) != length(fruit)) {
    stop("oil and fruit vectors must have the same length")
  }
  oil <- as.numeric(oil)
  fruit <- as.numeric(fruit)

  jb_oil <- tseries::jarque.bera.test(oil)
  jb_fruit <- tseries::jarque.bera.test(fruit)
  adf_oil <- tseries::adf.test(oil)
  kpss_oil <- tseries::kpss.test(oil)
  adf_fruit <- tseries::adf.test(fruit)
  kpss_fruit <- tseries::kpss.test(fruit)

  if (verbose) {
    cat("\n=== Stationarity Tests ===\n")
    cat("Jarque-Bera Test (Oil):\n");   print(jb_oil)
    cat("\nJarque-Bera Test (Fruit):\n"); print(jb_fruit)
    cat("\nADF Test (Oil):\n");         print(adf_oil)
    cat("\nKPSS Test (Oil):\n");        print(kpss_oil)
    cat("\nADF Test (Fruit):\n");       print(adf_fruit)
    cat("\nKPSS Test (Fruit):\n");      print(kpss_fruit)
  }

  list(jb_oil = jb_oil, jb_fruit = jb_fruit,
       adf_oil = adf_oil, kpss_oil = kpss_oil,
       adf_fruit = adf_fruit, kpss_fruit = kpss_fruit)
}

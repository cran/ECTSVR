#' Main Function for ECT-SVR Analysis
#'
#' This function performs a complete ECT-SVR analysis including stationarity tests,
#' cointegration tests, model training with multiple kernels, and comprehensive
#' evaluation. It serves as the main entry point for the package.
#'
#' @param fruit A numeric vector of fruit prices
#' @param oil A numeric vector of crude oil prices
#' @param train_ratio A numeric value between 0 and 1 for training proportion
#'   (default: 0.8)
#' @param kernels A character vector of kernel types to use. Options are
#'   "linear", "polynomial", "radial", and "sigmoid" (default: all four)
#' @param verbose A logical value indicating whether to print detailed output
#'   (default: TRUE)
#' @param plot_results A logical value indicating whether to generate comparison
#'   plots (default: TRUE)
#' @param lags Number of lags of the fruit series used as features (default: 2)
#'
#' @return A list containing all analysis results:
#'   \item{stationarity}{Results from stationarity tests}
#'   \item{cointegration}{Results from cointegration tests}
#'   \item{data}{Prepared data for modeling}
#'   \item{std_svr}{Standard SVR model results}
#'   \item{ect_svr}{ECT-SVR model results}
#'   \item{evaluation}{Model evaluation and comparison results}
#'   \item{best_model}{Information about the best performing model}
#'
#' @examples
#' \dontrun{
#' # Load your data
#' fruit <- c(100, 102, 101, 105, 108, 107, 110, 112, 111, 115)
#' oil <- c(50, 51, 50.5, 52, 51.5, 53, 52.5, 54, 53.5, 55)
#'
#' # Run complete analysis
#' results <- run_ectsvr_analysis(fruit, oil)
#'
#' # View best model
#' print(results$best_model)
#'
#' # View comparison table
#' print(results$evaluation$comparison_table)
#' }
#'
#' @export
run_ectsvr_analysis <- function(fruit, oil, train_ratio = 0.8,
                                kernels = c("linear", "polynomial", "radial", "sigmoid"),
                                verbose = TRUE, plot_results = TRUE, lags = 2) {

  if (verbose) {
    cat("\n", paste(rep("=", 80), collapse = ""), "\n")
    cat("ECT-SVR ANALYSIS STARTED\n")
    cat(paste(rep("=", 80), collapse = ""), "\n")
  }

  # Step 1: Stationarity tests
  if (verbose) cat("\nStep 1: Performing Stationarity Tests...\n")
  stationarity_results <- test_stationarity(oil, fruit, verbose = verbose)

  # Step 2: Cointegration test
  if (verbose) cat("\nStep 2: Performing Cointegration Test...\n")
  cointegration_results <- test_cointegration(fruit, oil, verbose = verbose)

  # Step 3: Prepare data
  if (verbose) cat("\nStep 3: Preparing Data for SVR...\n")
  data <- prepare_ectsvr_data(fruit, oil, train_ratio, lags = lags)

  # Step 4: Train Standard SVR models
  if (verbose) cat("\nStep 4: Training Standard SVR Models...\n")
  std_svr <- train_svr_models(data$X_train_std, data$X_test_std,
                              data$y_train, data$y_test,
                              "Standard SVR", kernels, verbose = verbose)

  # Step 5: Train ECT-SVR models
  if (verbose) cat("\nStep 5: Training ECT-SVR Models...\n")
  ect_svr <- train_svr_models(data$X_train_ect, data$X_test_ect,
                              data$y_train, data$y_test,
                              "ECT-SVR", kernels, verbose = verbose)

  # Step 6: Evaluate and compare models
  if (verbose) cat("\nStep 6: Evaluating Models...\n")
  evaluation <- evaluate_models(std_svr, ect_svr, verbose = verbose)

  # Step 7: Generate plots
  if (plot_results) {
    if (verbose) cat("\nStep 7: Generating Plots...\n")
    plot_comparisons(data$y_test, std_svr$predictions, ect_svr$predictions, kernels)
  }

  # Compile results
  results <- list(
    stationarity = stationarity_results,
    cointegration = cointegration_results,
    data = data,
    std_svr = std_svr,
    ect_svr = ect_svr,
    evaluation = evaluation,
    best_model = evaluation$best_models$best_rmse
  )

  if (verbose) {
    cat("\n", paste(rep("=", 80), collapse = ""), "\n")
    cat("ECT-SVR ANALYSIS COMPLETED\n")
    cat(paste(rep("=", 80), collapse = ""), "\n")
  }

  return(results)
}

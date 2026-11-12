#' Compare and Evaluate Models
#'
#' This function compares standard SVR and ECT-SVR models, identifies the best
#' performing models based on different metrics, and creates a comparison table.
#'
#' @param std_results A list containing results from standard SVR models
#' @param ect_results A list containing results from ECT-SVR models
#' @param format Output format for the table when using knitr. Options:
#'   "simple", "markdown", "latex", "html", or "pipe" (default: "simple")
#' @param verbose Logical; print the summary and table (default: TRUE)
#'
#' @return A list containing:
#'   \item{combined_results}{Data frame with combined results from both model types}
#'   \item{best_models}{List of best models by RMSE, MAE, MAPE, and R2}
#'   \item{comparison_table}{Formatted kable table for display (if knitr available)}
#'
#' @examples
#' \dontrun{
#' # Assuming you have trained models
#' comparison <- evaluate_models(std_results, ect_results)
#' print(comparison$combined_results)
#' print(comparison$best_models$best_rmse)
#'
#' # With markdown format
#' comparison <- evaluate_models(std_results, ect_results, format = "markdown")
#' }
#'
#' @importFrom knitr kable
#' @export
evaluate_models <- function(std_results, ect_results, format = "simple",
                            verbose = TRUE) {

  # Combine results
  std_results$results$Model <- "Standard SVR"
  ect_results$results$Model <- "ECT-SVR"

  combined_results <- rbind(std_results$results, ect_results$results)
  rownames(combined_results) <- NULL
  combined_results <- combined_results[, c("Model", "Kernel",
                                           "Train_RMSE", "Train_MAE", "Train_MAPE", "Train_R2",
                                           "Test_RMSE", "Test_MAE", "Test_MAPE", "Test_R2")]

  # Find best models
  best_test_rmse <- combined_results[which.min(combined_results$Test_RMSE), ]
  best_test_mae <- combined_results[which.min(combined_results$Test_MAE), ]
  best_test_mape <- combined_results[which.min(combined_results$Test_MAPE), ]
  best_test_r2 <- combined_results[which.max(combined_results$Test_R2), ]

  best_models <- list(
    best_rmse = best_test_rmse,
    best_mae = best_test_mae,
    best_mape = best_test_mape,
    best_r2 = best_test_r2
  )

  # Formatted table (knitr is in Imports)
  comparison_table <- knitr::kable(combined_results, digits = 4, format = format,
                                   row.names = FALSE)

  if (verbose) {
    cat("\n=== BEST MODELS BASED ON TEST METRICS ===\n")
    cat("\nBest Test RMSE:", best_test_rmse$Model, "with", best_test_rmse$Kernel,
        "kernel:", round(best_test_rmse$Test_RMSE, 4))
    cat("\nBest Test MAE:", best_test_mae$Model, "with", best_test_mae$Kernel,
        "kernel:", round(best_test_mae$Test_MAE, 4))
    cat("\nBest Test MAPE:", best_test_mape$Model, "with", best_test_mape$Kernel,
        "kernel:", round(best_test_mape$Test_MAPE, 2), "%")
    cat("\nBest Test R-squared:", best_test_r2$Model, "with", best_test_r2$Kernel,
        "kernel:", round(best_test_r2$Test_R2, 4), "\n\n")
    print(comparison_table)
  }

  list(
    combined_results = combined_results,
    best_models = best_models,
    comparison_table = comparison_table
  )
}

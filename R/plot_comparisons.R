#' Plot Model Comparisons
#'
#' Plots actual values against standard SVR and ECT-SVR test predictions, one
#' panel per kernel.
#'
#' @param y_test A numeric vector of actual test values
#' @param std_predictions A list of standard SVR predictions per kernel
#'   (each element has a \code{test} component)
#' @param ect_predictions A list of ECT-SVR predictions per kernel
#' @param kernels Kernel types to plot (default: all four)
#' @param ylab Label of the y axis (default: "Fruit Price")
#'
#' @return No return value, creates plots in the current graphics device
#'
#' @examples
#' \dontrun{
#' plot_comparisons(data$y_test, std_results$predictions, ect_results$predictions)
#' }
#'
#' @importFrom graphics par plot lines legend
#' @export
plot_comparisons <- function(y_test, std_predictions, ect_predictions,
                             kernels = c("linear", "polynomial", "radial", "sigmoid"),
                             ylab = "Fruit Price") {

  # Only plot kernels that were actually trained
  kernels <- intersect(kernels, intersect(names(std_predictions), names(ect_predictions)))
  if (length(kernels) == 0) stop("No matching kernels found in the predictions")

  nr <- ceiling(length(kernels) / 2)
  nc <- if (length(kernels) > 1) 2 else 1
  old_par <- graphics::par(mfrow = c(nr, nc))
  on.exit(graphics::par(old_par))

  for (kernel in kernels) {
    std_pred <- std_predictions[[kernel]]$test
    ect_pred <- ect_predictions[[kernel]]$test
    # Common y-range so predictions outside the actual range are not clipped
    yl <- range(y_test, std_pred, ect_pred, na.rm = TRUE)

    graphics::plot(y_test, type = "l", col = "black", lwd = 2, ylim = yl,
                   main = paste("Forecast Comparison -", kernel, "Kernel"),
                   ylab = ylab, xlab = "Time")
    graphics::lines(std_pred, col = "blue", lwd = 2, lty = 2)
    graphics::lines(ect_pred, col = "red", lwd = 2)
    graphics::legend("topleft",
                     legend = c("Actual", "Standard SVR", "ECT-SVR"),
                     col = c("black", "blue", "red"),
                     lty = c(1, 2, 1), lwd = 2, cex = 0.8)
  }
  invisible(NULL)
}

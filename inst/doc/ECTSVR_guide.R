## ----include = FALSE----------------------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>"
)

## ----setup--------------------------------------------------------------------
# Examples: How The cointegration based support vector regression model can be applied  
library(ECTSVR)
## Generate sample data
set.seed(123)
n <- 100
oil <- cumsum(rnorm(n, 0, 1)) + 50
fruit <- 2 * oil + rnorm(n, 0, 1) + 100

# Run analysis (quiet, no plots)
results <- run_ectsvr_analysis(fruit, oil, verbose = FALSE, plot_results = FALSE)

# View best model
print(results$best_model)


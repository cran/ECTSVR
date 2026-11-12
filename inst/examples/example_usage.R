# Example usage of ECTSVR package

# Load required libraries
library(ECTSVR)

# Simulate example data
set.seed(123)
n <- 200
time <- 1:n

# Generate cointegrated series
oil <- cumsum(rnorm(n, 0, 1)) + 50
fruit <- 2 * oil + rnorm(n, 0, 0.5) + 100

# Method 1: Run complete analysis
results <- run_ectsvr_analysis(fruit, oil, verbose = TRUE, plot_results = TRUE)

# Access best model
print(results$best_model)

# View evaluation table
print(results$evaluation$comparison_table)

# Method 2: Step-by-step analysis
# Stationarity tests
stationarity <- test_stationarity(oil, fruit)

# Cointegration test
cointegration <- test_cointegration(fruit, oil)

# Prepare data
data <- prepare_ectsvr_data(fruit, oil, train_ratio = 0.8)

# Train standard SVR
std_results <- train_svr_models(data$X_train_std, data$X_test_std,
                                data$y_train, data$y_test, "Standard SVR")

# Train ECT-SVR
ect_results <- train_svr_models(data$X_train_ect, data$X_test_ect,
                                data$y_train, data$y_test, "ECT-SVR")

# Compare models
comparison <- evaluate_models(std_results, ect_results)

# Method 3: Custom parameters
custom_results <- run_ectsvr_analysis(
  fruit = fruit,
  oil = oil,
  train_ratio = 0.7,
  kernels = c("radial", "linear"),
  verbose = TRUE,
  plot_results = TRUE
)

# Save results
saveRDS(custom_results, "ectsvr_analysis_results.rds")

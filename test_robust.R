# Test Robust Synthetic Control

library(dplyr)
source("functions/data_functions.R")
source("functions/synthetic_control_robust.R")

# Load data
data <- read.csv("example_data.csv")

# Test parameters
unit_var <- "state"
time_var <- "year"
outcome_var <- "gdp"
treated_unit <- "California"
treatment_year <- 2005
predictor_vars <- c("population", "education_spending")

cat("=== Testing Robust Synthetic Control ===\n")

tryCatch({
  results <- run_synthetic_control_robust(
    data = data,
    unit_var = unit_var,
    time_var = time_var,
    outcome_var = outcome_var,
    treated_unit = treated_unit,
    treatment_year = treatment_year,
    predictor_vars = predictor_vars
  )

  cat("\n✓ Robust analysis successful!\n")
  cat("Summary:\n")
  cat("- RMSPE:", round(results$rmspe, 3), "\n")
  cat("- Converged:", results$converged, "\n")
  cat("- Weights sum:", round(sum(results$weights), 3), "\n")
  cat("- Outcome path rows:", nrow(results$outcome_path), "\n")

}, error = function(e) {
  cat("✗ Robust analysis failed:", e$message, "\n")
  cat("Full error:\n")
  print(e)
})
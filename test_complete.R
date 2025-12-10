# Test Complete Workflow

library(dplyr)
source("functions/data_functions.R")
source("functions/synthetic_control_functions.R")
source("functions/plotting_functions.R")

# Load data
data <- read.csv("example_data.csv")

cat("=== Testing Complete Synthetic Control Workflow ===\n")

# Test parameters
unit_var <- "state"
time_var <- "year"
outcome_var <- "gdp"
treated_unit <- "California"
treatment_year <- 2005
predictor_vars <- c("population", "education_spending")

cat("1. Testing main synthetic control analysis...\n")

tryCatch({
  results <- run_synthetic_control(
    data = data,
    unit_var = unit_var,
    time_var = time_var,
    outcome_var = outcome_var,
    treated_unit = treated_unit,
    treatment_year = treatment_year,
    predictor_vars = predictor_vars
  )

  cat("✓ Main analysis successful!\n")
  cat("  - RMSPE:", round(results$rmspe, 3), "\n")
  cat("  - Converged:", results$converged, "\n")
  cat("  - Weights sum:", round(sum(results$weights), 3), "\n")

}, error = function(e) {
  cat("✗ Main analysis failed:", e$message, "\n")
  return()
})

cat("\n2. Testing placebo analysis...\n")

tryCatch({
  placebo_results <- run_placebo_tests(
    data = data,
    unit_var = unit_var,
    time_var = time_var,
    outcome_var = outcome_var,
    treated_unit = treated_unit,
    treatment_year = treatment_year,
    predictor_vars = predictor_vars
  )

  cat("✓ Placebo tests successful!\n")
  cat("  - Ranking:", round(placebo_results$ranking, 3), "\n")
  cat("  - Total placebo units:", length(unique(placebo_results$placebo_gaps$unit)), "\n")

}, error = function(e) {
  cat("✗ Placebo tests failed:", e$message, "\n")
})

cat("\n3. Testing plotting functions...\n")

tryCatch({
  # Test plots
  p1 <- plot_actual_vs_synthetic(results$outcome_path, results$treatment_year, results$treated_unit)
  p2 <- plot_treatment_gap(results$outcome_path, results$treatment_year)
  p3 <- plot_donor_weights(results$weights)

  cat("✓ All plots created successfully!\n")

}, error = function(e) {
  cat("✗ Plotting failed:", e$message, "\n")
})

cat("\n=== All Tests Complete ===\n")
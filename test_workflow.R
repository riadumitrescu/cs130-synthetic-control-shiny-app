# Test Synthetic Control Workflow
# This script tests the complete analysis pipeline

# Load required libraries
library(dplyr)
library(readr)

# Source functions
source("functions/data_functions.R")
source("functions/synthetic_control_functions.R")
source("functions/plotting_functions.R")

# Load example data
cat("Loading example data...\n")
data <- read.csv("example_data.csv")

cat("Data dimensions:", dim(data), "\n")
cat("Variables:", names(data), "\n")

# Test the analysis workflow
cat("\n=== Testing Analysis Workflow ===\n")

# Set parameters (matching the example data)
unit_var <- "state"
time_var <- "year"
outcome_var <- "gdp"
treated_unit <- "California"
treatment_year <- 2005
predictor_vars <- c("population", "education_spending")

cat("Configuration:\n")
cat("- Unit variable:", unit_var, "\n")
cat("- Time variable:", time_var, "\n")
cat("- Outcome variable:", outcome_var, "\n")
cat("- Treated unit:", treated_unit, "\n")
cat("- Treatment year:", treatment_year, "\n")
cat("- Predictors:", paste(predictor_vars, collapse = ", "), "\n")

# Run the analysis
cat("\nRunning synthetic control analysis...\n")
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

  cat("✓ Analysis completed successfully!\n")
  cat("\nResults summary:\n")
  cat("- Donor units:", length(results$donor_units), "\n")
  cat("- RMSPE:", round(results$rmspe, 3), "\n")
  cat("- Converged:", results$converged, "\n")

  # Show weights
  cat("\nDonor weights:\n")
  for(i in 1:length(results$weights)) {
    cat("-", names(results$weights)[i], ":", round(results$weights[i], 3), "\n")
  }

  # Show treatment effect summary
  post_effects <- results$outcome_path[results$outcome_path$post_treatment, "gap"]
  cat("\nTreatment effects:\n")
  cat("- Average post-treatment effect:", round(mean(post_effects), 2), "\n")
  cat("- Max effect:", round(max(post_effects), 2), "\n")
  cat("- Min effect:", round(min(post_effects), 2), "\n")

}, error = function(e) {
  cat("✗ Analysis failed:", e$message, "\n")
})

cat("\n=== Test completed ===\n")
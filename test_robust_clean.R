# Test Robust Function with Clean Data

library(dplyr)
library(tidyr)
source("functions/synthetic_control_robust.R")

# Load clean test data
data <- read.csv("test_data_clean.csv", stringsAsFactors = FALSE)

cat("=== Testing Robust Synthetic Control with Clean Data ===\n")

tryCatch({
  results <- run_synthetic_control_robust(
    data = data,
    unit_var = "unit",
    time_var = "year",
    outcome_var = "outcome",
    treated_unit = "A",
    treatment_year = 2005,
    predictor_vars = c("predictor1", "predictor2")
  )

  cat("\n✓ Robust analysis with clean data successful!\n")
  cat("Summary:\n")
  cat("- RMSPE:", round(results$rmspe, 4), "\n")
  cat("- Converged:", results$converged, "\n")
  cat("- Weights sum:", round(sum(results$weights), 6), "\n")
  cat("- Number of weights:", length(results$weights), "\n")
  cat("- Outcome path dimensions:", dim(results$outcome_path), "\n")

  # Test a few key columns
  cat("- Post-treatment values:", sum(results$outcome_path$post_treatment), "\n")
  cat("- Pre-treatment values:", sum(!results$outcome_path$post_treatment), "\n")

}, error = function(e) {
  cat("✗ Robust analysis failed:", e$message, "\n")
  cat("Full error details:\n")
  print(traceback())
  print(e)
})
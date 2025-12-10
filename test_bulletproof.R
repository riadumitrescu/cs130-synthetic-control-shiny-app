# Test Bulletproof Function

library(dplyr)
source("functions/synthetic_control_bulletproof.R")

# Test with clean data
data <- read.csv("test_data_clean.csv", stringsAsFactors = FALSE)

cat("=== TESTING BULLETPROOF FUNCTION ===\n")

tryCatch({
  results <- run_synthetic_control_bulletproof(
    data = data,
    unit_var = "unit",
    time_var = "year",
    outcome_var = "outcome",
    treated_unit = "A",
    treatment_year = 2005,
    predictor_vars = c("predictor1", "predictor2")
  )

  cat("\n🎉 SUCCESS! Bulletproof analysis worked!\n")
  cat("Key results:\n")
  cat("- RMSPE:", results$rmspe, "\n")
  cat("- Weights sum:", sum(results$weights), "\n")
  cat("- Outcome path length:", nrow(results$outcome_path), "\n")
  cat("- Post-treatment periods:", sum(results$outcome_path$post_treatment), "\n")

}, error = function(e) {
  cat("❌ STILL FAILED:", e$message, "\n")
  print(traceback())
})
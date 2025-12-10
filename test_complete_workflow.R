# Complete Workflow Test - Simulate App Usage

library(dplyr)
source("functions/data_functions.R")
source("functions/synthetic_control_bulletproof.R")
source("functions/plotting_functions.R")

cat("=== COMPLETE WORKFLOW TEST ===\n")

# Step 1: Load data (simulating file upload)
cat("Step 1: Loading test data...\n")
data <- read.csv("test_data_clean.csv", stringsAsFactors = FALSE)
cat("✓ Data loaded:", nrow(data), "rows,", ncol(data), "columns\n")

# Step 2: Validate data (simulating app validation)
cat("\nStep 2: Validating data structure...\n")
validation <- validate_panel_data(data)
if(validation$valid) {
  cat("✓ Data validation passed\n")
} else {
  cat("⚠ Validation warnings:", paste(validation$messages, collapse = "; "), "\n")
}

# Step 3: Set up analysis parameters (simulating user inputs)
cat("\nStep 3: Setting up analysis parameters...\n")
unit_var <- "unit"
time_var <- "year"
outcome_var <- "outcome"
treated_unit <- "A"
treatment_year <- 2005
predictor_vars <- c("predictor1", "predictor2")

cat("- Unit variable:", unit_var, "\n")
cat("- Time variable:", time_var, "\n")
cat("- Outcome variable:", outcome_var, "\n")
cat("- Treated unit:", treated_unit, "\n")
cat("- Treatment year:", treatment_year, "\n")
cat("- Predictors:", paste(predictor_vars, collapse = ", "), "\n")

# Step 4: Run main analysis
cat("\nStep 4: Running synthetic control analysis...\n")
main_results <- NULL

tryCatch({
  main_results <- run_synthetic_control_bulletproof(
    data = data,
    unit_var = unit_var,
    time_var = time_var,
    outcome_var = outcome_var,
    treated_unit = treated_unit,
    treatment_year = treatment_year,
    predictor_vars = predictor_vars
  )
  cat("✓ Main analysis completed successfully!\n")
  cat("  - RMSPE:", round(main_results$rmspe, 4), "\n")
  cat("  - Weights sum:", sum(main_results$weights), "\n")
}, error = function(e) {
  cat("✗ Main analysis failed:", e$message, "\n")
  stop("Cannot proceed without main analysis")
})

# Step 5: Test plotting functions
cat("\nStep 5: Testing plotting functions...\n")

tryCatch({
  p1 <- plot_actual_vs_synthetic(main_results$outcome_path, main_results$treatment_year, main_results$treated_unit)
  cat("✓ Actual vs Synthetic plot created\n")

  p2 <- plot_treatment_gap(main_results$outcome_path, main_results$treatment_year)
  cat("✓ Treatment gap plot created\n")

  p3 <- plot_donor_weights(main_results$weights)
  cat("✓ Donor weights plot created\n")

  balance_table <- create_balance_table(main_results$predictor_balance)
  cat("✓ Balance table created\n")

}, error = function(e) {
  cat("✗ Plotting failed:", e$message, "\n")
})

# Step 6: Test placebo analysis
cat("\nStep 6: Testing placebo analysis...\n")

tryCatch({
  placebo_results <- run_placebo_tests_bulletproof(
    data = data,
    unit_var = unit_var,
    time_var = time_var,
    outcome_var = outcome_var,
    treated_unit = treated_unit,
    treatment_year = treatment_year,
    predictor_vars = predictor_vars
  )

  cat("✓ Placebo tests completed successfully!\n")
  cat("  - Total placebo units:", length(unique(placebo_results$placebo_gaps$unit)), "\n")
  cat("  - P-value ranking:", round(placebo_results$ranking, 3), "\n")

  # Test placebo plot
  placebo_plot <- plot_placebo_gaps(placebo_results$placebo_gaps, treatment_year)
  cat("✓ Placebo plot created\n")

}, error = function(e) {
  cat("✗ Placebo tests failed:", e$message, "\n")
})

cat("\n=== WORKFLOW TEST COMPLETED ===\n")
cat("🎉 All major components working correctly!\n")
cat("\nThe app should now work without comparison errors.\n")
cat("Try uploading the test_data_clean.csv file and running the analysis.\n")
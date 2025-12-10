# Debug Step by Step

library(dplyr)
library(tidyr)

cat("=== Loading and checking data ===\n")

# Load clean test data
data <- read.csv("test_data_clean.csv", stringsAsFactors = FALSE)
print(str(data))
print(head(data))

# Set parameters
unit_var <- "unit"
time_var <- "year"
outcome_var <- "outcome"
treated_unit <- "A"
treatment_year <- 2005
predictor_vars <- c("predictor1", "predictor2")

cat("\nParameters:\n")
cat("- Unit var:", unit_var, "\n")
cat("- Time var:", time_var, "\n")
cat("- Outcome var:", outcome_var, "\n")
cat("- Treated unit:", treated_unit, "\n")
cat("- Treatment year:", treatment_year, "\n")
cat("- Predictor vars:", paste(predictor_vars, collapse = ", "), "\n")

cat("\n=== Step 1: Data type conversions ===\n")

# Convert data types explicitly
data[[time_var]] <- as.numeric(data[[time_var]])
treatment_year <- as.numeric(treatment_year)
data[[outcome_var]] <- as.numeric(data[[outcome_var]])

for(var in predictor_vars) {
  data[[var]] <- as.numeric(data[[var]])
}

print("Data types after conversion:")
print(str(data))

cat("\n=== Step 2: Check units ===\n")

all_units <- unique(data[[unit_var]])
cat("All units:", paste(all_units, collapse = ", "), "\n")

donor_units <- setdiff(all_units, treated_unit)
cat("Donor units:", paste(donor_units, collapse = ", "), "\n")

cat("\n=== Step 3: Pre-treatment filtering ===\n")

# Test comparison operations one by one
cat("Testing time comparisons:\n")
cat("- data[[time_var]] class:", class(data[[time_var]]), "\n")
cat("- treatment_year class:", class(treatment_year), "\n")
cat("- Sample time values:", head(data[[time_var]]), "\n")

# Try the comparison that's failing
tryCatch({
  cat("Testing: data[[time_var]] < treatment_year\n")
  comparison_result <- data[[time_var]] < treatment_year
  cat("- Comparison successful, length:", length(comparison_result), "\n")

  pre_data <- data[comparison_result, ]
  cat("- Pre-treatment data rows:", nrow(pre_data), "\n")

}, error = function(e) {
  cat("✗ Comparison failed:", e$message, "\n")
  print(e)
})

cat("\n=== Step 4: Test predictor calculation ===\n")

tryCatch({
  # Try the group_by operation
  cat("Testing group_by summarise...\n")

  predictor_means <- data[data[[time_var]] < treatment_year, ] %>%
    group_by(!!sym(unit_var)) %>%
    summarise(across(all_of(predictor_vars), ~ mean(.x, na.rm = TRUE)), .groups = "drop")

  cat("✓ Predictor means calculated successfully\n")
  print(predictor_means)

}, error = function(e) {
  cat("✗ Predictor calculation failed:", e$message, "\n")
  print(e)
})

cat("\n=== Debugging complete ===\n")
# Test Simple Synthetic Control

library(dplyr)
source("functions/synthetic_control_simple.R")

# Load data
data <- read.csv("example_data.csv")

# Test parameters
unit_var <- "state"
time_var <- "year"
outcome_var <- "gdp"
treated_unit <- "California"
treatment_year <- 2005
predictor_vars <- c("population", "education_spending")

cat("Testing simple synthetic control implementation...\n")

results <- run_synthetic_control_simple(
  data = data,
  unit_var = unit_var,
  time_var = time_var,
  outcome_var = outcome_var,
  treated_unit = treated_unit,
  treatment_year = treatment_year,
  predictor_vars = predictor_vars
)

cat("\nResults Summary:\n")
cat("- RMSPE:", round(results$rmspe, 3), "\n")
cat("- Converged:", results$converged, "\n")
cat("- Donor weights:\n")
for(i in 1:length(results$weights)) {
  cat("  ", names(results$weights)[i], ":", round(results$weights[i], 3), "\n")
}

# Treatment effects
post_effects <- results$outcome_path[results$outcome_path$post_treatment, "gap"]
cat("\nTreatment Effects:\n")
cat("- Average:", round(mean(post_effects), 2), "\n")
cat("- Range:", round(min(post_effects), 2), "to", round(max(post_effects), 2), "\n")
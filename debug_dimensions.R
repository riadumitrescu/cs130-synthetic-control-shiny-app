# Debug Matrix Dimensions Issue

library(dplyr)
source("functions/data_functions.R")

# Load data
data <- read.csv("example_data.csv")
cat("=== Data Info ===\n")
cat("Dimensions:", dim(data), "\n")
cat("Variables:", names(data), "\n")
print(head(data))

# Set parameters
unit_var <- "state"
time_var <- "year"
outcome_var <- "gdp"
treated_unit <- "California"
treatment_year <- 2005
predictor_vars <- c("population", "education_spending")

cat("\n=== Testing Predictor Calculation ===\n")

# Step 1: Prepare data
prepared_data <- prepare_sc_data(data, unit_var, time_var, outcome_var,
                                treated_unit, treatment_year, predictor_vars)
cat("Prepared data dimensions:", dim(prepared_data), "\n")

# Step 2: Calculate predictor means
predictor_means <- calculate_predictor_means(prepared_data, unit_var, time_var,
                                           predictor_vars, treatment_year)

cat("Predictor means matrix:\n")
print(predictor_means)
cat("Predictor means dimensions:", dim(predictor_means), "\n")

# Step 3: Check units
all_units <- unique(data[[unit_var]])
donor_units <- setdiff(all_units, treated_unit)
cat("All units:", all_units, "\n")
cat("Donor units:", donor_units, "\n")
cat("Treated unit:", treated_unit, "\n")

# Step 4: Extract predictors
cat("\n=== Extracting Predictors ===\n")
treated_predictors <- predictor_means[treated_unit, , drop = TRUE]
cat("Treated predictors:", treated_predictors, "\n")
cat("Treated predictors length:", length(treated_predictors), "\n")

available_donors <- intersect(donor_units, rownames(predictor_means))
cat("Available donors:", available_donors, "\n")

if(length(available_donors) > 0) {
  donor_predictors <- predictor_means[available_donors, , drop = FALSE]
  cat("Donor predictors matrix dimensions:", dim(donor_predictors), "\n")
  print(donor_predictors)
} else {
  cat("No available donors found!\n")
}
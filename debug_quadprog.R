# Debug Quadratic Programming Issue

library(quadprog)
library(dplyr)
source("functions/data_functions.R")

# Get the matrices from previous debug
data <- read.csv("example_data.csv")
unit_var <- "state"
time_var <- "year"
outcome_var <- "gdp"
treated_unit <- "California"
treatment_year <- 2005
predictor_vars <- c("population", "education_spending")

prepared_data <- prepare_sc_data(data, unit_var, time_var, outcome_var,
                                treated_unit, treatment_year, predictor_vars)
predictor_means <- calculate_predictor_means(prepared_data, unit_var, time_var,
                                           predictor_vars, treatment_year)

treated_predictors <- predictor_means[treated_unit, , drop = TRUE]
donor_units <- setdiff(unique(data[[unit_var]]), treated_unit)
available_donors <- intersect(donor_units, rownames(predictor_means))
donor_predictors <- predictor_means[available_donors, , drop = FALSE]

cat("=== Testing Quadratic Programming ===\n")
cat("Treated predictors:", treated_predictors, "\n")
cat("Donor predictors:\n")
print(donor_predictors)

# Check dimensions
n_donors <- nrow(donor_predictors)
n_predictors <- ncol(donor_predictors)

cat("Number of donors:", n_donors, "\n")
cat("Number of predictors:", n_predictors, "\n")

# Test the quadratic programming matrices
cat("\n=== Building QP Matrices ===\n")

# Create objective matrices
D <- t(donor_predictors) %*% donor_predictors
cat("D matrix dimensions:", dim(D), "\n")
print(D)

d <- as.numeric(t(donor_predictors) %*% treated_predictors)
cat("d vector length:", length(d), "\n")
cat("d vector:", d, "\n")

# Constraint matrices
Aeq <- matrix(1, nrow = 1, ncol = n_donors)
cat("Aeq matrix dimensions:", dim(Aeq), "\n")

beq <- 1
cat("beq value:", beq, "\n")

# For quadprog, constraints are Amat' %*% w >= bvec
Amat <- rbind(Aeq, diag(n_donors))
cat("Amat matrix dimensions:", dim(Amat), "\n")

bvec <- c(beq, rep(0, n_donors))
cat("bvec vector length:", length(bvec), "\n")

cat("\n=== Testing solve.QP ===\n")
tryCatch({
  result <- solve.QP(Dmat = D, dvec = d, Amat = t(Amat), bvec = bvec, meq = 1)
  cat("✓ QP solved successfully!\n")
  cat("Solution:", result$solution, "\n")
  cat("Sum of weights:", sum(result$solution), "\n")
}, error = function(e) {
  cat("✗ QP failed:", e$message, "\n")
})
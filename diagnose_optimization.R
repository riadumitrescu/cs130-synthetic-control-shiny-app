# Diagnostic Script for Synthetic Control Optimization
library(quadprog)
library(dplyr)

# Load test data
data <- read.csv("test_data_clean.csv", stringsAsFactors = FALSE)

cat("=== SYNTHETIC CONTROL OPTIMIZATION DIAGNOSIS ===\n")

# Convert data types
clean_data <- data.frame(
  unit = as.character(data$unit),
  time = as.numeric(as.character(data$year)),
  outcome = as.numeric(as.character(data$outcome)),
  pred_1 = as.numeric(as.character(data$predictor1)),
  pred_2 = as.numeric(as.character(data$predictor2)),
  stringsAsFactors = FALSE
)

# Parameters
treated_unit <- "A"
treatment_year <- 2005
all_units <- unique(clean_data$unit)
donor_units <- all_units[all_units != treated_unit]

cat("Units:", paste(all_units, collapse = ", "), "\n")
cat("Donor units:", paste(donor_units, collapse = ", "), "\n")

# Get pre-treatment data
pre_data <- clean_data[clean_data$time < treatment_year, ]

# Calculate predictor means
predictor_means <- data.frame()
for(unit in all_units) {
  unit_data <- pre_data[pre_data$unit == unit, ]
  if(nrow(unit_data) > 0) {
    row_data <- data.frame(
      unit = unit,
      pred_1 = mean(unit_data$pred_1, na.rm = TRUE),
      pred_2 = mean(unit_data$pred_2, na.rm = TRUE),
      stringsAsFactors = FALSE
    )
    predictor_means <- rbind(predictor_means, row_data)
  }
}

cat("\n=== PREDICTOR MEANS ===\n")
print(predictor_means)

# Extract matrices
treated_predictors <- as.numeric(predictor_means[predictor_means$unit == treated_unit, c("pred_1", "pred_2")])
donor_predictors <- as.matrix(predictor_means[predictor_means$unit %in% donor_units, c("pred_1", "pred_2")])
rownames(donor_predictors) <- predictor_means$unit[predictor_means$unit %in% donor_units]

cat("\n=== OPTIMIZATION MATRICES ===\n")
cat("Treated predictors:\n")
print(treated_predictors)
cat("\nDonor predictors:\n")
print(donor_predictors)

# Check matrix properties
cat("\n=== MATRIX DIAGNOSTICS ===\n")
cat("Donor matrix dimensions:", dim(donor_predictors), "\n")
cat("Treated vector length:", length(treated_predictors), "\n")

# Setup optimization
n_donors <- nrow(donor_predictors)
cat("Number of donors:", n_donors, "\n")

# Calculate H matrix (quadratic term)
H <- 2 * (t(donor_predictors) %*% donor_predictors)
cat("H matrix dimensions:", dim(H), "\n")
cat("H matrix:\n")
print(H)

# Calculate f vector (linear term)
f <- -2 * (t(donor_predictors) %*% treated_predictors)
f <- as.numeric(f)
cat("f vector:\n")
print(f)

# Check for singularity
cat("\nMatrix condition number:", kappa(H), "\n")
cat("Matrix determinant:", det(H), "\n")

# Add regularization
H_reg <- H + diag(ncol(H)) * 1e-8
cat("Regularized matrix determinant:", det(H_reg), "\n")

# Setup constraints
Amat <- cbind(rep(1, n_donors), diag(n_donors))
bvec <- c(1, rep(0, n_donors))

cat("\n=== CONSTRAINT MATRICES ===\n")
cat("Amat dimensions:", dim(Amat), "\n")
cat("bvec length:", length(bvec), "\n")

# Try optimization
cat("\n=== OPTIMIZATION ATTEMPT ===\n")
tryCatch({
  solution <- solve.QP(Dmat = H_reg, dvec = f, Amat = Amat, bvec = bvec, meq = 1)
  weights <- as.numeric(solution$solution)
  names(weights) <- rownames(donor_predictors)

  cat("Optimization successful!\n")
  cat("Raw weights:\n")
  print(weights)
  cat("Weights sum:", sum(weights), "\n")
  cat("Negative weights:", sum(weights < 0), "\n")

  # Normalize
  weights <- pmax(weights, 0)
  weights <- weights / sum(weights)
  cat("Final weights:\n")
  print(weights)

  # Check fit
  fitted_predictors <- as.numeric(t(weights) %*% donor_predictors)
  cat("Fitted predictors:\n")
  print(fitted_predictors)
  cat("Target predictors:\n")
  print(treated_predictors)
  cat("Differences:\n")
  print(treated_predictors - fitted_predictors)

}, error = function(e) {
  cat("Optimization failed:", e$message, "\n")
  print(traceback())
})
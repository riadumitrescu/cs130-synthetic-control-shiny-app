# Synthetic Control Analysis Functions
library(dplyr)
library(tidyr)

#' Compute synthetic control weights
#' @param treated_predictors Vector of treated unit predictor values
#' @param donor_predictors Matrix of donor unit predictor values (donors x predictors)
#' @return List containing weights and optimization details
compute_sc_weights <- function(treated_predictors, donor_predictors) {

  # Ensure inputs are matrices/vectors
  if(is.null(donor_predictors) || nrow(donor_predictors) < 2) {
    stop("Need at least 2 donor units for synthetic control")
  }

  if(length(treated_predictors) != ncol(donor_predictors)) {
    stop("Dimension mismatch between treated unit and donor predictors")
  }

  # Number of donors
  n_donors <- nrow(donor_predictors)

  # Set up quadratic programming problem
  # min 0.5 * w' * D * w - d' * w
  # subject to: sum(w) = 1, w >= 0

  # Create the objective function matrix (D) and vector (d)
  # We want to minimize ||X_1 - X_0 * w||^2
  # This becomes w' * (X_0' * X_0) * w - 2 * (X_1' * X_0) * w

  D <- t(donor_predictors) %*% donor_predictors
  d <- t(donor_predictors) %*% treated_predictors

  # Constraint matrices
  # Equality constraint: sum(w) = 1
  Aeq <- matrix(1, nrow = 1, ncol = n_donors)
  beq <- 1

  # Inequality constraints: w >= 0 (handled by bounds)
  # For quadprog, we need Amat and bvec where Amat' %*% w >= bvec
  Amat <- rbind(Aeq, diag(n_donors))  # First row: sum constraint, then identity for bounds
  bvec <- c(beq, rep(0, n_donors))    # b = 1 for sum, 0 for lower bounds

  # Use quadprog to solve
  tryCatch({
    result <- solve.QP(Dmat = D, dvec = d, Amat = t(Amat), bvec = bvec, meq = 1)
    weights <- result$solution

    # Check convergence
    converged <- TRUE

  }, error = function(e) {
    # Fallback to simple equal weights if optimization fails
    warning("Optimization failed, using equal weights")
    weights <- rep(1/n_donors, n_donors)
    converged <- FALSE
  })

  # Ensure weights sum to 1 and are non-negative
  weights[weights < 0] <- 0
  weights <- weights / sum(weights)

  # Calculate fit statistics
  fitted_predictors <- as.numeric(donor_predictors %*% weights)
  rmspe <- sqrt(mean((treated_predictors - fitted_predictors)^2))

  return(list(
    weights = weights,
    rmspe = rmspe,
    converged = converged,
    fitted_predictors = fitted_predictors,
    treated_predictors = treated_predictors
  ))
}

#' Compute synthetic control outcome path
#' @param data The dataset
#' @param unit_var Unit identifier column name
#' @param time_var Time variable column name
#' @param outcome_var Outcome variable column name
#' @param weights Vector of donor weights
#' @param donor_units Vector of donor unit names (in same order as weights)
#' @return Data frame with synthetic outcome path
compute_synthetic_outcome <- function(data, unit_var, time_var, outcome_var,
                                     weights, donor_units) {

  # Filter to donor units only
  donor_data <- data[data[[unit_var]] %in% donor_units, ]

  # Create outcome matrix (time x donors)
  outcome_matrix <- donor_data %>%
    select(all_of(c(unit_var, time_var, outcome_var))) %>%
    pivot_wider(names_from = !!sym(unit_var), values_from = !!sym(outcome_var)) %>%
    arrange(!!sym(time_var))

  time_periods <- outcome_matrix[[time_var]]
  outcome_matrix <- outcome_matrix[, donor_units, drop = FALSE]

  # Compute weighted average (synthetic outcome)
  synthetic_outcome <- as.numeric(as.matrix(outcome_matrix) %*% weights)

  # Return as data frame
  data.frame(
    time = time_periods,
    synthetic_outcome = synthetic_outcome
  )
}

#' Run full synthetic control analysis
#' @param data The dataset
#' @param unit_var Unit identifier column name
#' @param time_var Time variable column name
#' @param outcome_var Outcome variable column name
#' @param treated_unit The treated unit
#' @param treatment_year The treatment year
#' @param predictor_vars Vector of predictor variable names
#' @return List with all analysis results
run_synthetic_control <- function(data, unit_var, time_var, outcome_var,
                                 treated_unit, treatment_year, predictor_vars = NULL) {

  # Prepare data
  prepared_data <- prepare_sc_data(data, unit_var, time_var, outcome_var,
                                  treated_unit, treatment_year, predictor_vars)

  # Get donor units
  donor_units <- unique(prepared_data[prepared_data$donor, unit_var])

  # If no predictor variables specified, use pre-treatment outcome means
  if(is.null(predictor_vars)) {
    predictor_vars <- outcome_var
  }

  # Calculate predictor means
  predictor_means <- calculate_predictor_means(prepared_data, unit_var, time_var,
                                              predictor_vars, treatment_year)

  # Extract treated unit predictors
  treated_predictors <- predictor_means[treated_unit, , drop = TRUE]

  # Extract donor predictors
  donor_predictors <- predictor_means[donor_units, , drop = FALSE]

  # Compute weights
  weight_results <- compute_sc_weights(treated_predictors, donor_predictors)
  names(weight_results$weights) <- donor_units

  # Compute synthetic outcome path
  synthetic_path <- compute_synthetic_outcome(prepared_data, unit_var, time_var,
                                             outcome_var, weight_results$weights, donor_units)

  # Get treated unit outcome path
  treated_path <- prepared_data[prepared_data[[unit_var]] == treated_unit,
                               c(time_var, outcome_var)]
  names(treated_path) <- c("time", "treated_outcome")

  # Merge paths and calculate gap
  combined_path <- merge(treated_path, synthetic_path, by = "time")
  combined_path$gap <- combined_path$treated_outcome - combined_path$synthetic_outcome

  # Mark treatment period
  combined_path$post_treatment <- combined_path$time >= treatment_year

  return(list(
    weights = weight_results$weights,
    rmspe = weight_results$rmspe,
    converged = weight_results$converged,
    outcome_path = combined_path,
    predictor_balance = data.frame(
      predictor = names(treated_predictors),
      treated = treated_predictors,
      synthetic = weight_results$fitted_predictors,
      difference = treated_predictors - weight_results$fitted_predictors
    ),
    treatment_year = treatment_year,
    treated_unit = treated_unit,
    donor_units = donor_units
  ))
}
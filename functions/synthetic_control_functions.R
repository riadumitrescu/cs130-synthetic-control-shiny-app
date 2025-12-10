# Synthetic Control Analysis Functions
library(dplyr)
library(tidyr)

#' Compute synthetic control weights
#' @param treated_predictors Vector of treated unit predictor values (length K)
#' @param donor_predictors Matrix of donor unit predictor values (J donors x K predictors)
#' @return List containing weights and optimization details
compute_sc_weights <- function(treated_predictors, donor_predictors) {

  # Ensure inputs are matrices/vectors
  if(is.null(donor_predictors) || nrow(donor_predictors) < 2) {
    stop("Need at least 2 donor units for synthetic control")
  }

  n_predictors <- length(treated_predictors)
  n_donors <- nrow(donor_predictors)

  if(n_predictors != ncol(donor_predictors)) {
    stop("Dimension mismatch between treated unit and donor predictors")
  }

  # Synthetic control minimizes ||X1 - X0*W||^2
  # where X1 = treated predictors (K x 1), X0 = donor predictors transposed (K x J), W = weights (J x 1)
  #
  # Our donor_predictors is (J x K), so X0 = t(donor_predictors) is (K x J)
  # Synthetic predictor = X0 * W = t(donor_predictors) %*% W
  #
  # Expanding: ||X1 - X0*W||^2 = W' * (X0'X0) * W - 2 * (X0'X1)' * W + const
  # QP form: min 0.5 * W' * D * W - d' * W
  # So: D = X0'X0 = donor_predictors %*% t(donor_predictors)  (J x J)
  #     d = X0'X1 = donor_predictors %*% X1                    (J x 1)

  # Compute D (J x J) and d (J x 1)
  D <- donor_predictors %*% t(donor_predictors)
  d <- as.numeric(donor_predictors %*% treated_predictors)

  # Add small ridge to D for numerical stability (positive definiteness)
  D <- D + diag(1e-6, n_donors)

  # Constraint matrices for quadprog
  # Equality: sum(w) = 1
  # Inequality: w_j >= 0 for all j
  # quadprog: Amat' %*% w >= bvec
  Amat <- cbind(rep(1, n_donors), diag(n_donors))  # J x (1 + J)
  bvec <- c(1, rep(0, n_donors))                    # sum=1, then lower bounds

  weights <- NULL
  converged <- FALSE

  tryCatch({
    result <- quadprog::solve.QP(Dmat = D, dvec = d, Amat = Amat, bvec = bvec, meq = 1)
    weights <- result$solution
    converged <- TRUE
  }, error = function(e) {
    warning(paste("QP optimization failed:", e$message, "- using equal weights"))
  })

  # Fallback to equal weights if optimization failed

  if(is.null(weights)) {
    weights <- rep(1/n_donors, n_donors)
    converged <- FALSE
  }

  # Clean up weights: enforce non-negativity and sum to 1
  weights[weights < 1e-8] <- 0
  weights <- weights / sum(weights)

  # Compute fitted synthetic predictors: X0 * W = t(donor_predictors) %*% W
  fitted_predictors <- as.numeric(t(donor_predictors) %*% weights)

  # RMSPE: root mean squared prediction error over predictors
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

  # Ensure columns are in the same order as donor_units (weights)
  # Some units might be missing from the outcome matrix
  available_columns <- intersect(donor_units, names(outcome_matrix))
  outcome_matrix <- outcome_matrix[, available_columns, drop = FALSE]

  # Reorder weights to match available columns
  matched_weights <- weights[available_columns]

  # Compute weighted average (synthetic outcome)
  synthetic_outcome <- as.numeric(as.matrix(outcome_matrix) %*% matched_weights)

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

  # Extract donor predictors (make sure they exist in the matrix)
  available_donors <- intersect(donor_units, rownames(predictor_means))
  if(length(available_donors) < 2) {
    stop("Need at least 2 donor units with complete predictor data")
  }
  donor_predictors <- predictor_means[available_donors, , drop = FALSE]

  # Compute weights
  weight_results <- compute_sc_weights(treated_predictors, donor_predictors)
  names(weight_results$weights) <- available_donors

  # Compute synthetic outcome path
  synthetic_path <- compute_synthetic_outcome(prepared_data, unit_var, time_var,
                                             outcome_var, weight_results$weights, available_donors)

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
    donor_units = available_donors
  ))
}
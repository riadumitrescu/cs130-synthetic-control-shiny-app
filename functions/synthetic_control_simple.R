# Simplified Synthetic Control Functions with Better Error Handling

library(dplyr)
library(tidyr)

#' Simple synthetic control analysis
#' @param data The dataset
#' @param unit_var Unit identifier column name
#' @param time_var Time variable column name
#' @param outcome_var Outcome variable column name
#' @param treated_unit The treated unit
#' @param treatment_year The treatment year
#' @param predictor_vars Vector of predictor variable names
#' @return List with analysis results
run_synthetic_control_simple <- function(data, unit_var, time_var, outcome_var,
                                        treated_unit, treatment_year, predictor_vars = NULL) {

  # Use outcome variable if no predictors specified
  if(is.null(predictor_vars)) {
    predictor_vars <- outcome_var
  }

  cat("Step 1: Preparing data...\n")

  # Get all units and donor pool
  all_units <- unique(data[[unit_var]])
  donor_units <- setdiff(all_units, treated_unit)

  if(length(donor_units) < 2) {
    stop("Need at least 2 donor units")
  }

  # Filter to pre-treatment period for predictor calculation
  pre_data <- data[data[[time_var]] < treatment_year, ]

  cat("Step 2: Calculating predictor means...\n")

  # Calculate pre-treatment means for each predictor
  predictor_means <- pre_data %>%
    group_by(!!sym(unit_var)) %>%
    summarise(across(all_of(predictor_vars), ~ mean(.x, na.rm = TRUE)), .groups = "drop")

  # Extract treated unit predictors
  treated_row <- predictor_means[predictor_means[[unit_var]] == treated_unit, ]
  if(nrow(treated_row) == 0) {
    stop(paste("Treated unit", treated_unit, "not found in data"))
  }
  treated_predictors <- as.numeric(treated_row[, predictor_vars])

  # Extract donor predictors
  donor_rows <- predictor_means[predictor_means[[unit_var]] %in% donor_units, ]
  if(nrow(donor_rows) < 2) {
    stop("Need at least 2 donor units with complete data")
  }

  donor_predictors <- as.matrix(donor_rows[, predictor_vars])
  rownames(donor_predictors) <- donor_rows[[unit_var]]

  cat("Step 3: Computing synthetic control weights...\n")

  # Simple optimization: minimize sum of squared differences
  n_donors <- nrow(donor_predictors)
  n_predictors <- ncol(donor_predictors)

  cat("  - Donors:", n_donors, "\n")
  cat("  - Predictors:", n_predictors, "\n")

  # Use quadratic programming if available, otherwise use simple method
  tryCatch({
    library(quadprog)

    # Set up QP problem: minimize ||X1 - X0*w||^2 subject to sum(w)=1, w>=0
    Dmat <- 2 * (t(donor_predictors) %*% donor_predictors)
    dvec <- 2 * (t(donor_predictors) %*% matrix(treated_predictors, ncol=1))
    dvec <- as.numeric(dvec)

    # Constraints: Amat^T * w >= bvec
    # First constraint: sum(w) = 1 (equality)
    # Other constraints: w_i >= 0
    Amat <- cbind(rep(1, n_donors), diag(n_donors))
    bvec <- c(1, rep(0, n_donors))

    solution <- solve.QP(Dmat = Dmat, dvec = dvec, Amat = Amat, bvec = bvec, meq = 1)
    weights <- solution$solution
    converged <- TRUE

  }, error = function(e) {
    cat("  - QP failed, using equal weights\n")
    weights <- rep(1/n_donors, n_donors)
    converged <- FALSE
  })

  # Ensure weights are valid
  weights[weights < 0] <- 0
  weights <- weights / sum(weights)
  names(weights) <- rownames(donor_predictors)

  cat("Step 4: Computing synthetic outcome path...\n")

  # Create outcome matrix (time x units)
  outcome_wide <- data %>%
    select(all_of(c(unit_var, time_var, outcome_var))) %>%
    pivot_wider(names_from = !!sym(unit_var), values_from = !!sym(outcome_var)) %>%
    arrange(!!sym(time_var))

  time_periods <- outcome_wide[[time_var]]

  # Get outcomes for treated unit and donors
  treated_outcomes <- outcome_wide[[treated_unit]]

  # Get donor outcomes in same order as weights
  donor_names <- names(weights)
  donor_outcome_matrix <- as.matrix(outcome_wide[, donor_names])

  # Compute synthetic outcomes
  synthetic_outcomes <- as.numeric(donor_outcome_matrix %*% weights)

  # Calculate gaps
  gaps <- treated_outcomes - synthetic_outcomes

  # Create results data frame
  outcome_path <- data.frame(
    time = time_periods,
    treated_outcome = treated_outcomes,
    synthetic_outcome = synthetic_outcomes,
    gap = gaps,
    post_treatment = time_periods >= treatment_year
  )

  cat("Step 5: Computing fit statistics...\n")

  # RMSPE in pre-treatment period
  pre_period <- outcome_path[!outcome_path$post_treatment, ]
  rmspe <- sqrt(mean(pre_period$gap^2, na.rm = TRUE))

  # Predictor balance
  fitted_predictors <- as.numeric(t(weights) %*% donor_predictors)
  predictor_balance <- data.frame(
    predictor = predictor_vars,
    treated = treated_predictors,
    synthetic = fitted_predictors,
    difference = treated_predictors - fitted_predictors
  )

  cat("✓ Analysis completed successfully!\n")

  return(list(
    weights = weights,
    rmspe = rmspe,
    converged = converged,
    outcome_path = outcome_path,
    predictor_balance = predictor_balance,
    treatment_year = treatment_year,
    treated_unit = treated_unit,
    donor_units = donor_names
  ))
}
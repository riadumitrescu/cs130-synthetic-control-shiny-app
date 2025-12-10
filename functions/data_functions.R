# Data Processing and Validation Functions
library(dplyr)

#' Validate panel data structure
#' @param data The uploaded dataset
#' @return List with validation results
validate_panel_data <- function(data) {
  messages <- character(0)
  valid <- TRUE

  # Check if data exists and has rows
  if(is.null(data) || nrow(data) == 0) {
    return(list(valid = FALSE, messages = "No data found"))
  }

  # Check minimum number of columns
  if(ncol(data) < 3) {
    messages <- c(messages, "Dataset should have at least 3 columns (unit, time, outcome)")
    valid <- FALSE
  }

  # Check for completely empty columns
  empty_cols <- sapply(data, function(x) all(is.na(x) | x == ""))
  if(any(empty_cols)) {
    messages <- c(messages, paste("Empty columns found:", paste(names(data)[empty_cols], collapse = ", ")))
  }

  # Check for reasonable number of observations
  if(nrow(data) < 10) {
    messages <- c(messages, "Dataset has very few observations. Synthetic control typically requires more data.")
  }

  return(list(valid = valid, messages = messages))
}

#' Check panel balance
#' @param data The dataset
#' @param unit_var Unit identifier column name
#' @param time_var Time variable column name
#' @return List with balance information
check_panel_balance <- function(data, unit_var, time_var) {
  if(is.null(data) || is.null(unit_var) || is.null(time_var)) {
    return(NULL)
  }

  # Count observations per unit
  unit_counts <- data %>%
    group_by(!!sym(unit_var)) %>%
    summarise(n_periods = n(), .groups = "drop")

  # Check for missing unit-time combinations
  all_units <- unique(data[[unit_var]])
  all_times <- unique(data[[time_var]])
  expected_obs <- length(all_units) * length(all_times)
  actual_obs <- nrow(data)

  list(
    balanced = actual_obs == expected_obs,
    expected_obs = expected_obs,
    actual_obs = actual_obs,
    unit_counts = unit_counts,
    missing_share = (expected_obs - actual_obs) / expected_obs
  )
}

#' Prepare data for synthetic control
#' @param data The dataset
#' @param unit_var Unit identifier column name
#' @param time_var Time variable column name
#' @param outcome_var Outcome variable column name
#' @param treated_unit The treated unit
#' @param treatment_year The treatment year
#' @param predictor_vars Vector of predictor variable names
#' @return Prepared dataset with additional columns
prepare_sc_data <- function(data, unit_var, time_var, outcome_var,
                           treated_unit, treatment_year, predictor_vars = NULL) {

  # Create post-treatment indicator
  data$post_treatment <- data[[time_var]] >= treatment_year

  # Create treated unit indicator
  data$treated <- data[[unit_var]] == treated_unit

  # Create donor pool indicator
  data$donor <- !data$treated

  # Filter to remove treated unit from post-treatment period if needed
  # (keeping it for now as we need it for gap calculation)

  return(data)
}

#' Calculate pre-treatment predictor means
#' @param data The prepared dataset
#' @param unit_var Unit identifier column name
#' @param time_var Time variable column name
#' @param predictor_vars Vector of predictor variable names
#' @param treatment_year The treatment year
#' @return Matrix of pre-treatment predictor means by unit
calculate_predictor_means <- function(data, unit_var, time_var, predictor_vars, treatment_year) {
  if(is.null(predictor_vars) || length(predictor_vars) == 0) {
    return(NULL)
  }

  # Filter to pre-treatment period
  pre_data <- data[data[[time_var]] < treatment_year, ]

  # Calculate means by unit
  predictor_means <- pre_data %>%
    group_by(!!sym(unit_var)) %>%
    summarise(across(all_of(predictor_vars), ~ mean(.x, na.rm = TRUE)), .groups = "drop")

  # Convert to matrix with units as rownames
  means_matrix <- as.matrix(predictor_means[, predictor_vars, drop = FALSE])
  rownames(means_matrix) <- predictor_means[[unit_var]]

  return(means_matrix)
}
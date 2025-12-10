# Robust Synthetic Control Analysis with Special Predictors
library(dplyr)
library(tidyr)

#' Enhanced synthetic control analysis with special predictors
#' @param data The dataset
#' @param unit_var Unit identifier column name
#' @param time_var Time variable column name
#' @param outcome_var Outcome variable column name
#' @param treated_unit The treated unit
#' @param treatment_year The treatment year
#' @param predictor_vars Vector of regular predictor variable names (REQUIRED)
#' @param special_predictors List of special predictors (optional)
#' @return List with analysis results
run_synthetic_control_robust <- function(data, unit_var, time_var, outcome_var,
                                        treated_unit, treatment_year, predictor_vars,
                                        special_predictors = NULL) {

  # Validate inputs
  if(is.null(predictor_vars) || length(predictor_vars) == 0) {
    stop("Predictor variables are required for synthetic control analysis")
  }

  # Convert to ensure proper data types
  data[[time_var]] <- as.numeric(data[[time_var]])
  treatment_year <- as.numeric(treatment_year)
  data[[outcome_var]] <- as.numeric(data[[outcome_var]])

  # Convert predictor columns to numeric
  for(var in predictor_vars) {
    data[[var]] <- as.numeric(data[[var]])
  }

  cat("Starting robust synthetic control analysis...\n")
  cat("- Treated unit:", treated_unit, "\n")
  cat("- Treatment year:", treatment_year, "\n")
  cat("- Predictors:", paste(predictor_vars, collapse = ", "), "\n")

  # Get all units and validate
  all_units <- unique(data[[unit_var]])
  if(!(treated_unit %in% all_units)) {
    stop(paste("Treated unit", treated_unit, "not found in data"))
  }

  donor_units <- setdiff(all_units, treated_unit)
  if(length(donor_units) < 2) {
    stop("Need at least 2 donor units")
  }

  # Filter to pre-treatment period
  pre_data <- data[data[[time_var]] < treatment_year, ]
  if(nrow(pre_data) == 0) {
    stop("No pre-treatment data available")
  }

  cat("- Pre-treatment periods:", length(unique(pre_data[[time_var]])), "\n")
  cat("- Donor units:", length(donor_units), "\n")

  # Calculate regular predictor means
  cat("Calculating predictor means...\n")

  predictor_means <- pre_data %>%
    group_by(!!sym(unit_var)) %>%
    summarise(across(all_of(predictor_vars), ~ mean(.x, na.rm = TRUE)), .groups = "drop")

  # Handle special predictors if provided
  if(!is.null(special_predictors) && length(special_predictors) > 0) {
    cat("Processing special predictors...\n")

    for(i in seq_along(special_predictors)) {
      special <- special_predictors[[i]]
      var_name <- special$variable
      periods <- special$periods
      operation <- special$operation %||% "mean"

      # Filter data for special predictor
      special_data <- data[data[[time_var]] %in% periods, ]

      if(nrow(special_data) > 0) {
        special_means <- special_data %>%
          group_by(!!sym(unit_var)) %>%
          summarise(
            !!paste0(var_name, "_", paste(range(periods), collapse = "_")) :=
              get(operation)(!!sym(var_name), na.rm = TRUE),
            .groups = "drop"
          )

        # Merge with main predictor means
        predictor_means <- merge(predictor_means, special_means, by = unit_var, all = TRUE)
      }
    }
  }

  # Extract treated unit predictors
  treated_row <- predictor_means[predictor_means[[unit_var]] == treated_unit, ]
  if(nrow(treated_row) == 0) {
    stop(paste("No predictor data found for treated unit:", treated_unit))
  }

  # Get numeric predictor columns only
  numeric_cols <- sapply(predictor_means, is.numeric)
  predictor_cols <- names(predictor_means)[numeric_cols]

  treated_predictors <- as.numeric(treated_row[, predictor_cols])
  names(treated_predictors) <- predictor_cols

  # Extract donor predictors
  donor_rows <- predictor_means[predictor_means[[unit_var]] %in% donor_units, ]
  if(nrow(donor_rows) < 2) {
    stop("Need at least 2 donor units with complete predictor data")
  }

  donor_predictors <- as.matrix(donor_rows[, predictor_cols])
  rownames(donor_predictors) <- donor_rows[[unit_var]]

  cat("- Predictor matrix dimensions:", dim(donor_predictors), "\n")

  # Compute synthetic control weights using quadratic programming
  cat("Computing optimal weights...\n")

  n_donors <- nrow(donor_predictors)

  tryCatch({
    library(quadprog)

    # Set up optimization: minimize ||X1 - X0*w||^2 subject to sum(w)=1, w>=0
    H <- 2 * (t(donor_predictors) %*% donor_predictors)
    f <- 2 * (t(donor_predictors) %*% treated_predictors)

    # Add small regularization to diagonal for numerical stability
    H <- H + diag(ncol(H)) * 1e-8

    # Constraints: sum(w) = 1, w >= 0
    Amat <- cbind(rep(1, n_donors), diag(n_donors))
    bvec <- c(1, rep(0, n_donors))

    solution <- solve.QP(Dmat = H, dvec = f, Amat = Amat, bvec = bvec, meq = 1)
    weights <- solution$solution
    converged <- TRUE

    cat("- Optimization converged successfully\n")

  }, error = function(e) {
    cat("- Optimization failed, using equal weights:", e$message, "\n")
    weights <- rep(1/n_donors, n_donors)
    converged <- FALSE
  })

  # Ensure weights are valid and atomic
  weights <- as.numeric(weights)
  weights[weights < 0] <- 0
  if(sum(weights) == 0 || any(is.na(weights))) {
    weights <- rep(1/n_donors, n_donors)
  }
  weights <- weights / sum(weights)
  names(weights) <- rownames(donor_predictors)

  cat("- Weight sum:", round(sum(weights), 6), "\n")

  # Create outcome paths
  cat("Computing synthetic outcomes...\n")

  # Reshape data to wide format
  outcome_wide <- data %>%
    select(all_of(c(unit_var, time_var, outcome_var))) %>%
    pivot_wider(names_from = !!sym(unit_var), values_from = !!sym(outcome_var)) %>%
    arrange(!!sym(time_var))

  time_periods <- outcome_wide[[time_var]]

  # Get treated outcomes
  if(!(treated_unit %in% names(outcome_wide))) {
    stop(paste("Treated unit", treated_unit, "not found in outcome data"))
  }
  treated_outcomes <- outcome_wide[[treated_unit]]

  # Get donor outcomes
  donor_names <- names(weights)
  missing_donors <- setdiff(donor_names, names(outcome_wide))
  if(length(missing_donors) > 0) {
    stop(paste("Donor units not found in outcome data:", paste(missing_donors, collapse = ", ")))
  }

  donor_outcome_matrix <- as.matrix(outcome_wide[, donor_names, drop = FALSE])
  synthetic_outcomes <- as.numeric(donor_outcome_matrix %*% weights)

  # Calculate treatment effects
  gaps <- treated_outcomes - synthetic_outcomes

  # Create comprehensive results
  outcome_path <- data.frame(
    time = time_periods,
    treated_outcome = treated_outcomes,
    synthetic_outcome = synthetic_outcomes,
    gap = gaps,
    post_treatment = time_periods >= treatment_year,
    stringsAsFactors = FALSE
  )

  # Calculate fit statistics
  pre_period <- outcome_path[!outcome_path$post_treatment, ]
  rmspe <- sqrt(mean(pre_period$gap^2, na.rm = TRUE))

  # Predictor balance
  fitted_predictors <- as.numeric(t(weights) %*% donor_predictors)
  names(fitted_predictors) <- predictor_cols

  predictor_balance <- data.frame(
    predictor = predictor_cols,
    treated = treated_predictors,
    synthetic = fitted_predictors,
    difference = treated_predictors - fitted_predictors,
    stringsAsFactors = FALSE
  )

  cat("✓ Analysis completed successfully!\n")
  cat("- RMSPE:", round(rmspe, 4), "\n")

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

#' Run placebo tests with robust implementation
run_placebo_tests_robust <- function(data, unit_var, time_var, outcome_var,
                                   treated_unit, treatment_year, predictor_vars,
                                   special_predictors = NULL) {

  all_units <- unique(data[[unit_var]])
  placebo_results <- list()

  cat("Running placebo tests...\n")

  for(unit in all_units) {
    cat("- Testing unit:", unit, "\n")

    tryCatch({
      result <- run_synthetic_control_robust(
        data = data,
        unit_var = unit_var,
        time_var = time_var,
        outcome_var = outcome_var,
        treated_unit = unit,
        treatment_year = treatment_year,
        predictor_vars = predictor_vars,
        special_predictors = special_predictors
      )

      gap_data <- result$outcome_path[, c("time", "gap")]
      gap_data$unit <- unit
      gap_data$is_treated <- (unit == treated_unit)

      placebo_results[[unit]] <- gap_data

    }, error = function(e) {
      cat("  - Skipped due to error:", e$message, "\n")
    })
  }

  # Combine results
  all_gaps <- do.call(rbind, placebo_results)

  # Calculate statistical ranking
  post_gaps <- all_gaps[all_gaps$time >= treatment_year, ]
  if(nrow(post_gaps) > 0) {
    avg_gaps <- aggregate(gap ~ unit, data = post_gaps, FUN = mean)
    treated_gap <- avg_gaps[avg_gaps$unit == treated_unit, "gap"]

    if(length(treated_gap) > 0) {
      ranking <- sum(abs(avg_gaps$gap) >= abs(treated_gap)) / nrow(avg_gaps)
    } else {
      ranking <- NA
    }
  } else {
    ranking <- NA
  }

  return(list(
    placebo_gaps = all_gaps,
    ranking = ranking
  ))
}
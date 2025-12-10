# Bulletproof Synthetic Control - No More Comparison Errors
library(dplyr)
library(tidyr)

#' Bulletproof synthetic control analysis
run_synthetic_control_bulletproof <- function(data, unit_var, time_var, outcome_var,
                                            treated_unit, treatment_year, predictor_vars) {

  cat("=== BULLETPROOF SYNTHETIC CONTROL ===\n")

  # STEP 1: Validate all inputs
  if(is.null(predictor_vars) || length(predictor_vars) == 0) {
    stop("Predictor variables are required")
  }

  cat("Step 1: Input validation passed\n")

  # STEP 2: Force all data to correct types
  cat("Step 2: Converting data types...\n")

  # Make a clean copy of the data
  clean_data <- data.frame(
    unit = as.character(data[[unit_var]]),
    time = as.numeric(as.character(data[[time_var]])),  # Double conversion to handle factors
    outcome = as.numeric(as.character(data[[outcome_var]])),
    stringsAsFactors = FALSE
  )

  # Add predictors as numeric
  for(i in seq_along(predictor_vars)) {
    var_name <- predictor_vars[i]
    clean_data[[paste0("pred_", i)]] <- as.numeric(as.character(data[[var_name]]))
  }

  # Convert treatment year
  treatment_year <- as.numeric(as.character(treatment_year))

  cat("- Data dimensions:", nrow(clean_data), "x", ncol(clean_data), "\n")
  cat("- Time range:", min(clean_data$time, na.rm=TRUE), "to", max(clean_data$time, na.rm=TRUE), "\n")
  cat("- Treatment year:", treatment_year, "\n")

  # STEP 3: Get units
  all_units <- unique(clean_data$unit)
  if(!(treated_unit %in% all_units)) {
    stop(paste("Treated unit not found:", treated_unit))
  }

  donor_units <- all_units[all_units != treated_unit]
  if(length(donor_units) < 2) {
    stop("Need at least 2 donor units")
  }

  cat("- All units:", paste(all_units, collapse=", "), "\n")
  cat("- Donor units:", paste(donor_units, collapse=", "), "\n")

  # STEP 4: Pre-treatment data (SAFE comparison)
  cat("Step 3: Filtering pre-treatment data...\n")

  # Use which() to avoid any comparison issues
  pre_treatment_rows <- which(clean_data$time < treatment_year)
  if(length(pre_treatment_rows) == 0) {
    stop("No pre-treatment data found")
  }

  pre_data <- clean_data[pre_treatment_rows, ]
  cat("- Pre-treatment rows:", nrow(pre_data), "\n")

  # STEP 5: Calculate predictor means
  cat("Step 4: Calculating predictor means...\n")

  predictor_cols <- paste0("pred_", 1:length(predictor_vars))

  # Manual calculation to avoid any dplyr issues
  predictor_means <- data.frame(unit = character(0), stringsAsFactors = FALSE)
  for(col in predictor_cols) {
    predictor_means[[col]] <- numeric(0)
  }

  for(unit in all_units) {
    unit_data <- pre_data[pre_data$unit == unit, ]
    if(nrow(unit_data) > 0) {
      row_data <- list(unit = unit)
      for(col in predictor_cols) {
        row_data[[col]] <- mean(unit_data[[col]], na.rm = TRUE)
      }
      predictor_means <- rbind(predictor_means, data.frame(row_data, stringsAsFactors = FALSE))
    }
  }

  cat("- Predictor means calculated for", nrow(predictor_means), "units\n")

  # STEP 6: Extract predictors safely
  treated_row_idx <- which(predictor_means$unit == treated_unit)
  if(length(treated_row_idx) != 1) {
    stop("Could not find unique treated unit in predictor means")
  }

  treated_predictors <- as.numeric(predictor_means[treated_row_idx, predictor_cols])

  donor_row_idx <- which(predictor_means$unit %in% donor_units)
  if(length(donor_row_idx) < 2) {
    stop("Not enough donor units with predictor data")
  }

  donor_predictors <- as.matrix(predictor_means[donor_row_idx, predictor_cols])
  rownames(donor_predictors) <- predictor_means$unit[donor_row_idx]

  cat("- Treated predictors:", paste(round(treated_predictors, 2), collapse=", "), "\n")
  cat("- Donor matrix:", nrow(donor_predictors), "x", ncol(donor_predictors), "\n")

  # STEP 7: Simple weight calculation (avoid quadprog issues)
  cat("Step 5: Computing weights...\n")

  n_donors <- nrow(donor_predictors)

  # Try quadprog first, but with robust fallback
  weights <- NULL
  converged <- FALSE

  tryCatch({
    library(quadprog, quietly = TRUE)

    # Set up optimization
    H <- 2 * (t(donor_predictors) %*% donor_predictors)
    f <- 2 * (t(donor_predictors) %*% treated_predictors)
    f <- as.numeric(f)  # Ensure it's a vector

    # Add small regularization
    H <- H + diag(ncol(H)) * 1e-8

    # Constraints: sum(w) = 1, w >= 0
    Amat <- cbind(rep(1, n_donors), diag(n_donors))
    bvec <- c(1, rep(0, n_donors))

    solution <- solve.QP(Dmat = H, dvec = f, Amat = Amat, bvec = bvec, meq = 1)
    weights <- as.numeric(solution$solution)  # FORCE to numeric
    converged <- TRUE
    cat("- Quadprog optimization successful\n")

  }, error = function(e) {
    cat("- Quadprog failed, using equal weights\n")
    weights <- rep(1/n_donors, n_donors)
    converged <- FALSE
  })

  # BULLETPROOF weight validation
  if(is.null(weights) || !is.numeric(weights) || length(weights) != n_donors) {
    weights <- rep(1/n_donors, n_donors)
  }

  # Safe comparison using which()
  negative_idx <- which(weights < 0)
  if(length(negative_idx) > 0) {
    weights[negative_idx] <- 0
  }

  if(sum(weights) <= 0 || any(is.na(weights))) {
    weights <- rep(1/n_donors, n_donors)
  }

  weights <- weights / sum(weights)
  names(weights) <- rownames(donor_predictors)

  cat("- Final weights sum:", sum(weights), "\n")

  # STEP 8: Compute synthetic outcomes
  cat("Step 6: Computing synthetic outcomes...\n")

  # Reshape outcome data manually
  time_periods <- sort(unique(clean_data$time))

  outcome_matrix <- matrix(NA, nrow = length(time_periods), ncol = length(all_units))
  rownames(outcome_matrix) <- as.character(time_periods)
  colnames(outcome_matrix) <- all_units

  for(i in 1:nrow(clean_data)) {
    time_idx <- which(time_periods == clean_data$time[i])
    unit_idx <- which(all_units == clean_data$unit[i])
    outcome_matrix[time_idx, unit_idx] <- clean_data$outcome[i]
  }

  # Get outcomes
  treated_outcomes <- outcome_matrix[, treated_unit]
  donor_outcome_matrix <- outcome_matrix[, names(weights), drop = FALSE]

  synthetic_outcomes <- as.numeric(donor_outcome_matrix %*% weights)
  gaps <- treated_outcomes - synthetic_outcomes

  # Create final data frame with SAFE comparisons
  post_treatment <- rep(FALSE, length(time_periods))
  post_idx <- which(time_periods >= treatment_year)
  if(length(post_idx) > 0) {
    post_treatment[post_idx] <- TRUE
  }

  outcome_path <- data.frame(
    time = time_periods,
    treated_outcome = treated_outcomes,
    synthetic_outcome = synthetic_outcomes,
    gap = gaps,
    post_treatment = post_treatment,
    stringsAsFactors = FALSE
  )

  # Calculate RMSPE
  pre_idx <- which(!outcome_path$post_treatment)
  if(length(pre_idx) > 0) {
    rmspe <- sqrt(mean(outcome_path$gap[pre_idx]^2, na.rm = TRUE))
  } else {
    rmspe <- NA
  }

  # Predictor balance
  fitted_predictors <- as.numeric(t(weights) %*% donor_predictors)

  predictor_balance <- data.frame(
    predictor = predictor_vars,
    treated = treated_predictors,
    synthetic = fitted_predictors,
    difference = treated_predictors - fitted_predictors,
    stringsAsFactors = FALSE
  )

  cat("✓ Analysis completed successfully!\n")
  cat("- RMSPE:", round(rmspe, 4), "\n")
  cat("- Weights:", paste(round(weights, 3), collapse=", "), "\n")

  return(list(
    weights = weights,
    rmspe = rmspe,
    converged = converged,
    outcome_path = outcome_path,
    predictor_balance = predictor_balance,
    treatment_year = treatment_year,
    treated_unit = treated_unit,
    donor_units = names(weights)
  ))
}

#' Bulletproof placebo tests
run_placebo_tests_bulletproof <- function(data, unit_var, time_var, outcome_var,
                                        treated_unit, treatment_year, predictor_vars) {

  all_units <- unique(data[[unit_var]])
  placebo_results <- list()

  cat("Running bulletproof placebo tests...\n")

  for(unit in all_units) {
    cat("- Testing unit:", unit, "\n")

    tryCatch({
      result <- run_synthetic_control_bulletproof(
        data = data,
        unit_var = unit_var,
        time_var = time_var,
        outcome_var = outcome_var,
        treated_unit = unit,
        treatment_year = treatment_year,
        predictor_vars = predictor_vars
      )

      gap_data <- data.frame(
        time = result$outcome_path$time,
        gap = result$outcome_path$gap,
        unit = unit,
        is_treated = (unit == treated_unit),
        stringsAsFactors = FALSE
      )

      placebo_results[[unit]] <- gap_data

    }, error = function(e) {
      cat("  - Skipped:", e$message, "\n")
    })
  }

  # Combine results
  if(length(placebo_results) > 0) {
    all_gaps <- do.call(rbind, placebo_results)

    # Calculate ranking with safe comparisons
    post_idx <- which(all_gaps$time >= treatment_year)
    if(length(post_idx) > 0) {
      post_gaps <- all_gaps[post_idx, ]
      avg_gaps <- aggregate(gap ~ unit, data = post_gaps, FUN = mean)
      treated_gap_idx <- which(avg_gaps$unit == treated_unit)

      if(length(treated_gap_idx) == 1) {
        treated_gap <- avg_gaps$gap[treated_gap_idx]
        ranking <- sum(abs(avg_gaps$gap) >= abs(treated_gap)) / nrow(avg_gaps)
      } else {
        ranking <- NA
      }
    } else {
      ranking <- NA
    }
  } else {
    all_gaps <- data.frame()
    ranking <- NA
  }

  return(list(
    placebo_gaps = all_gaps,
    ranking = ranking
  ))
}
# Wrapper functions for the Synth package
# Uses dataprep() and synth() from the official Synth package by Abadie et al.

library(Synth)
library(dplyr)

#' Run synthetic control analysis using the Synth package
#' 
#' @param data Panel dataset
#' @param unit_var Name of unit identifier column
#' @param time_var Name of time variable column
#' @param outcome_var Name of outcome variable column
#' @param treated_unit The treated unit identifier
#' @param treatment_year The year treatment begins
#' @param predictor_vars Vector of predictor variable names (uses pre-treatment mean)
#' @param special_predictors_config List of special predictor configs, each with:
#'   - var: variable name
#'   - start: start year of time window
#'   - end: end year of time window
#'   - op: aggregation operator ("mean", "median", etc.)
#' @return List containing synth results and extracted data for plotting
run_synth_analysis <- function(data, unit_var, time_var, outcome_var,
                                treated_unit, treatment_year, 
                                predictor_vars = NULL,
                                special_predictors_config = NULL) {
  
  # Convert to data.frame
  data <- as.data.frame(data)
  
  # Get all unique units
  all_units <- unique(data[[unit_var]])
  n_units <- length(all_units)
  
  # Create numeric unit IDs (Synth requires numeric identifiers)
  unit_to_id <- setNames(seq_len(n_units), all_units)
  id_to_unit <- setNames(all_units, seq_len(n_units))
  
  # Add numeric unit ID column to data
  data$unit_num <- unit_to_id[as.character(data[[unit_var]])]
  
  # Get treated and control IDs
  treated_id <- unit_to_id[[as.character(treated_unit)]]
  control_ids <- setdiff(seq_len(n_units), treated_id)
  control_names <- id_to_unit[as.character(control_ids)]
  
  # Get time periods
  all_times <- sort(unique(data[[time_var]]))
  pre_times <- all_times[all_times < treatment_year]
  post_times <- all_times[all_times >= treatment_year]
  
  if(length(pre_times) < 2) {
    stop("Need at least 2 pre-treatment periods")
  }
  if(length(control_ids) < 2) {
    stop("Need at least 2 control units in donor pool")
  }
  
  # Build special.predictors list from config
  # Each element: list(varname, time_range, operator)
  special_predictors <- list()
  
  if(!is.null(special_predictors_config) && length(special_predictors_config) > 0) {
    for(i in seq_along(special_predictors_config)) {
      cfg <- special_predictors_config[[i]]
      var_name <- cfg$var
      start_year <- cfg$start
      end_year <- cfg$end
      op <- if(!is.null(cfg$op)) cfg$op else "mean"
      
      # Create time range within pre-treatment period
      time_range <- seq(start_year, end_year)
      # Filter to only include times that exist in data and are pre-treatment
      time_range <- intersect(time_range, pre_times)
      
      if(length(time_range) > 0) {
        special_predictors[[length(special_predictors) + 1]] <- list(
          var_name,
          time_range,
          op
        )
      }
    }
  }
  
  # If no special predictors configured, use outcome at each pre-treatment time point
  if(length(special_predictors) == 0) {
    # Default: use outcome variable at each pre-treatment period
    for(t in pre_times) {
      special_predictors[[length(special_predictors) + 1]] <- list(
        outcome_var,
        t,
        "mean"
      )
    }
  }
  
  message(paste("Configured", length(special_predictors), "special predictors"))
  if(!is.null(predictor_vars) && length(predictor_vars) > 0) {
    message(paste("Regular predictors:", paste(predictor_vars, collapse = ", ")))
  }
  
  # Prepare data using Synth::dataprep()
  message("Running dataprep()...")
  dataprep_out <- tryCatch({
    dataprep(
      foo = data,
      predictors = predictor_vars,          # Regular predictors (pre-treatment mean)
      predictors.op = "mean",
      time.predictors.prior = pre_times,
      special.predictors = special_predictors,  # Special predictors with time windows
      dependent = outcome_var,
      unit.variable = "unit_num",
      time.variable = time_var,
      treatment.identifier = treated_id,
      controls.identifier = control_ids,
      time.optimize.ssr = pre_times,
      time.plot = all_times
    )
  }, error = function(e) {
    stop(paste("dataprep() failed:", e$message))
  })
  
  # Run synth() optimization directly
  message("Running synth() optimization...")
  synth_out <- tryCatch({
    synth(data.prep.obj = dataprep_out, method = "BFGS")
  }, error = function(e) {
    stop(paste("synth() optimization failed:", e$message))
  })
  
  # Extract results using synth.tab()
  synth_tables <- synth.tab(dataprep.res = dataprep_out, synth.res = synth_out)
  
  # Extract ALL weights with proper unit names
  weights_vec <- as.numeric(synth_out$solution.w)
  names(weights_vec) <- control_names
  
  # Keep all weights for donor_units count
  all_donor_names <- control_names
  # Filter for display (non-trivial weights only)
  display_weights <- weights_vec[weights_vec > 0.001]
  
  # Extract outcome paths from dataprep object
  treated_outcome <- as.numeric(dataprep_out$Y1plot)
  synthetic_outcome <- as.numeric(dataprep_out$Y0plot %*% synth_out$solution.w)
  
  outcome_path <- data.frame(
    time = all_times,
    treated_outcome = treated_outcome,
    synthetic_outcome = synthetic_outcome,
    stringsAsFactors = FALSE
  )
  outcome_path$gap <- outcome_path$treated_outcome - outcome_path$synthetic_outcome
  outcome_path$post_treatment <- outcome_path$time >= treatment_year
  
  # Calculate pre-treatment RMSPE
  pre_gaps <- outcome_path$gap[!outcome_path$post_treatment]
  rmspe <- sqrt(mean(pre_gaps^2))
  
  # Extract predictor balance from synth.tab
  tab_pred <- synth_tables$tab.pred
  predictor_balance <- data.frame(
    predictor = rownames(tab_pred),
    treated = as.numeric(tab_pred[, 1]),
    synthetic = as.numeric(tab_pred[, 2]),
    stringsAsFactors = FALSE
  )
  predictor_balance$difference <- predictor_balance$treated - predictor_balance$synthetic
  rownames(predictor_balance) <- NULL
  
  message(paste("Synth completed. RMSPE:", round(rmspe, 4)))
  message(paste("Donor pool size:", length(all_donor_names)))
  message(paste("Non-zero weight donors:", length(display_weights)))
  
  # Return structured results
  list(
    # Core Synth objects (for native plotting if needed)
    dataprep_out = dataprep_out,
    synth_out = synth_out,
    synth_tables = synth_tables,
    
    # Extracted data for custom plotting
    weights = display_weights,
    all_weights = weights_vec,
    outcome_path = outcome_path,
    predictor_balance = predictor_balance,
    rmspe = rmspe,
    
    # Metadata
    treated_unit = treated_unit,
    treatment_year = treatment_year,
    donor_units = all_donor_names,
    converged = TRUE,
    
    # Unit mappings
    unit_to_id = unit_to_id,
    id_to_unit = id_to_unit
  )
}


#' Run placebo tests using the Synth package
#' 
#' Runs synthetic control on each donor unit as if it were treated
#' 
#' @param data Panel dataset
#' @param unit_var Name of unit identifier column
#' @param time_var Name of time variable column
#' @param outcome_var Name of outcome variable column
#' @param treated_unit The actual treated unit identifier
#' @param treatment_year The year treatment begins
#' @param predictor_vars Vector of regular predictor variable names
#' @param special_predictors_config List of special predictor configs
#' @return List with placebo gaps and ranking
run_synth_placebo <- function(data, unit_var, time_var, outcome_var,
                               treated_unit, treatment_year, 
                               predictor_vars = NULL,
                               special_predictors_config = NULL) {
  
  data <- as.data.frame(data)
  
  all_units <- unique(data[[unit_var]])
  all_times <- sort(unique(data[[time_var]]))
  
  # Store gaps for each unit (including treated)
  all_gaps <- list()
  rmspe_ratios <- c()
  
  message(paste("Running placebo tests for", length(all_units), "units..."))
  
  for(i in seq_along(all_units)) {
    unit <- all_units[i]
    message(paste0("[", i, "/", length(all_units), "] Testing: ", unit))
    
    tryCatch({
      # Run synth with this unit as "treated"
      result <- run_synth_analysis(
        data = data,
        unit_var = unit_var,
        time_var = time_var,
        outcome_var = outcome_var,
        treated_unit = unit,
        treatment_year = treatment_year,
        predictor_vars = predictor_vars,
        special_predictors_config = special_predictors_config
      )
      
      # Extract gap series
      gap_df <- data.frame(
        time = result$outcome_path$time,
        gap = result$outcome_path$gap,
        unit = unit,
        is_treated = (unit == treated_unit),
        stringsAsFactors = FALSE
      )
      all_gaps[[unit]] <- gap_df
      
      # Calculate RMSPE ratio (post/pre)
      pre_rmspe <- result$rmspe
      post_data <- result$outcome_path[result$outcome_path$post_treatment, ]
      post_rmspe <- sqrt(mean(post_data$gap^2))
      
      if(pre_rmspe > 0.001) {
        rmspe_ratios[unit] <- post_rmspe / pre_rmspe
      }
      
    }, error = function(e) {
      message(paste("  Placebo failed for", unit, ":", e$message))
    })
  }
  
  # Combine all gaps
  if(length(all_gaps) > 0) {
    placebo_gaps <- do.call(rbind, all_gaps)
    rownames(placebo_gaps) <- NULL
  } else {
    placebo_gaps <- data.frame(time = numeric(), gap = numeric(), 
                               unit = character(), is_treated = logical())
  }
  
  # Calculate p-value (rank of treated unit)
  if(treated_unit %in% names(rmspe_ratios) && length(rmspe_ratios) > 1) {
    treated_ratio <- rmspe_ratios[treated_unit]
    ranking <- mean(rmspe_ratios >= treated_ratio, na.rm = TRUE)
  } else {
    ranking <- NA
  }
  
  message(paste("Placebo tests completed.", length(all_gaps), "successful."))
  
  list(
    placebo_gaps = placebo_gaps,
    rmspe_ratios = rmspe_ratios,
    ranking = ranking
  )
}


#' Generate path plot using Synth package
#' 
#' @param synth_result Result from run_synth_analysis()
#' @return NULL (plots to current device)
plot_synth_path <- function(synth_result) {
  path.plot(
    synth.res = synth_result$synth_out,
    dataprep.res = synth_result$dataprep_out,
    Ylab = "Outcome",
    Xlab = "Time",
    Legend = c(synth_result$treated_unit, paste("Synthetic", synth_result$treated_unit)),
    Legend.position = "bottomleft"
  )
  abline(v = synth_result$treatment_year, lty = 2, col = "red")
}


#' Generate gaps plot using Synth package
#' 
#' @param synth_result Result from run_synth_analysis()
#' @return NULL (plots to current device)
plot_synth_gaps <- function(synth_result) {
  gaps.plot(
    synth.res = synth_result$synth_out,
    dataprep.res = synth_result$dataprep_out,
    Ylab = "Gap (Treatment Effect)",
    Xlab = "Time"
  )
  abline(v = synth_result$treatment_year, lty = 2, col = "red")
  abline(h = 0, lty = 1, col = "gray50")
}

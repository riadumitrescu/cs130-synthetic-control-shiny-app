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
  data[["unit_num"]] <- unit_to_id[as.character(data[[unit_var]])]
  
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
      var_name <- cfg[["var"]]
      start_year <- cfg[["start"]]
      end_year <- cfg[["end"]]
      op <- if(!is.null(cfg[["op"]])) cfg[["op"]] else "mean"
      
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
  
  # Only create default special predictors if BOTH regular predictors AND special predictors are empty
  # This ensures we match standard Synth package usage where users can use ONLY regular predictors
  if(length(special_predictors) == 0 && (is.null(predictor_vars) || length(predictor_vars) == 0)) {
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

  # Pass NULL instead of empty list for special.predictors if not configured
  # This matches standard Synth package behavior
  special_pred_arg <- if(length(special_predictors) > 0) special_predictors else NULL

  # Prepare data using Synth::dataprep()
  message("Running dataprep()...")
  dataprep_out <- tryCatch({
    result <- dataprep(
      foo = data,
      predictors = predictor_vars,          # Regular predictors (pre-treatment mean)
      predictors.op = "mean",
      time.predictors.prior = pre_times,
      special.predictors = special_pred_arg,  # Special predictors with time windows (NULL if not configured)
      dependent = outcome_var,
      unit.variable = "unit_num",
      time.variable = time_var,
      treatment.identifier = treated_id,
      controls.identifier = control_ids,
      time.optimize.ssr = pre_times,
      time.plot = all_times
    )
    
    # Validate dataprep result
    if(is.null(result)) {
      stop("dataprep() returned NULL")
    }
    if(!is.list(result)) {
      stop(paste("dataprep() returned unexpected type:", class(result)[1]))
    }
    if(!"Y1plot" %in% names(result) || !"Y0plot" %in% names(result)) {
      stop("dataprep() result missing required Y1plot or Y0plot elements")
    }
    
    result
  }, error = function(e) {
    stop(paste("dataprep() failed:", e$message))
  })
  
  # Run synth() optimization directly
  message("Running synth() optimization...")
  synth_out <- tryCatch({
    result <- synth(data.prep.obj = dataprep_out, method = "BFGS")
    
    # Validate synth result
    if(is.null(result)) {
      stop("synth() returned NULL")
    }
    if(!is.list(result)) {
      stop(paste("synth() returned unexpected type:", class(result)[1]))
    }
    if(!"solution.w" %in% names(result)) {
      stop("synth() result missing required solution.w element")
    }
    
    result
  }, error = function(e) {
    stop(paste("synth() optimization failed:", e$message))
  })
  
  # Extract results using synth.tab()
  synth_tables <- tryCatch({
    result <- synth.tab(dataprep.res = dataprep_out, synth.res = synth_out)
    if(is.null(result) || !is.list(result)) {
      stop(paste("synth.tab() returned unexpected type:", class(result)[1]))
    }
    result
  }, error = function(e) {
    stop(paste("synth.tab() failed:", e$message))
  })
  
  # Extract ALL weights with proper unit names - use [[ for safety
  weights_vec <- tryCatch({
    w <- as.numeric(synth_out[["solution.w"]])
    if(length(w) == 0 || any(is.na(w))) {
      stop("solution.w is empty or contains NA values")
    }
    w
  }, error = function(e) {
    stop(paste("Failed to extract weights:", e$message))
  })
  names(weights_vec) <- control_names
  
  # Keep all weights for donor_units count
  all_donor_names <- control_names
  # Filter for display (non-trivial weights only)
  display_weights <- weights_vec[weights_vec > 0.001]
  
  # Extract outcome paths from dataprep object - use [[ for safety
  treated_outcome <- tryCatch({
    as.numeric(dataprep_out[["Y1plot"]])
  }, error = function(e) {
    stop(paste("Failed to extract Y1plot:", e$message))
  })
  
  synthetic_outcome <- tryCatch({
    y0 <- dataprep_out[["Y0plot"]]
    if(is.null(y0)) {
      stop("Y0plot is NULL")
    }
    as.numeric(y0 %*% synth_out[["solution.w"]])
  }, error = function(e) {
    stop(paste("Failed to calculate synthetic outcome:", e$message))
  })
  
  outcome_path <- data.frame(
    time = all_times,
    treated_outcome = treated_outcome,
    synthetic_outcome = synthetic_outcome,
    stringsAsFactors = FALSE
  )
  # Add gap and post_treatment columns using [[ for safety
  outcome_path[["gap"]] <- outcome_path[["treated_outcome"]] - outcome_path[["synthetic_outcome"]]
  outcome_path[["post_treatment"]] <- outcome_path[["time"]] >= treatment_year
  
  # Calculate pre-treatment RMSPE
  post_treatment_vec <- outcome_path[["post_treatment"]]
  pre_gaps <- outcome_path[["gap"]][!post_treatment_vec]
  rmspe <- sqrt(mean(pre_gaps^2))
  
  # Extract predictor balance from synth.tab - use [[ for safety
  tab_pred <- tryCatch({
    pred <- synth_tables[["tab.pred"]]
    if(is.null(pred)) {
      stop("tab.pred is NULL")
    }
    pred
  }, error = function(e) {
    stop(paste("Failed to extract tab.pred:", e$message))
  })
  
  predictor_balance <- data.frame(
    predictor = rownames(tab_pred),
    treated = as.numeric(tab_pred[, 1]),
    synthetic = as.numeric(tab_pred[, 2]),
    stringsAsFactors = FALSE
  )
  predictor_balance[["difference"]] <- predictor_balance[["treated"]] - predictor_balance[["synthetic"]]
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
  failed_units <- c()
  
  total_units <- length(all_units)
  message(paste("Running placebo tests for", total_units, "units..."))
  
  for(i in seq_along(all_units)) {
    unit <- all_units[i]
    message(paste0("[", i, "/", total_units, "] Testing: ", unit))
    
    result <- tryCatch({
      # Run synth with this unit as "treated"
      synth_result <- tryCatch({
        run_synth_analysis(
          data = data,
          unit_var = unit_var,
          time_var = time_var,
          outcome_var = outcome_var,
          treated_unit = unit,
          treatment_year = treatment_year,
          predictor_vars = predictor_vars,
          special_predictors_config = special_predictors_config
        )
      }, error = function(e) {
        error_msg <- tryCatch({
          conditionMessage(e)
        }, error = function(e2) {
          as.character(e)
        })
        stop(paste("run_synth_analysis failed:", error_msg))
      })
      
      # Validate result structure - check type BEFORE accessing elements
      if(is.null(synth_result)) {
        stop("Analysis returned NULL")
      }
      
      # Check if it's a list/data frame before using $ operator
      if(!is.list(synth_result) && !is.data.frame(synth_result)) {
        result_type <- class(synth_result)[1]
        result_str <- if(length(synth_result) <= 10) {
          paste(synth_result, collapse = ", ")
        } else {
          paste(paste(head(synth_result, 5), collapse = ", "), "... (truncated)")
        }
        stop(paste("Analysis returned unexpected type:", result_type, "- Value:", result_str))
      }
      
      # Now safe to check for outcome_path
      if(!"outcome_path" %in% names(synth_result)) {
        stop("Analysis result missing 'outcome_path' element")
      }
      
      outcome_path <- synth_result[["outcome_path"]]  # Use [[ instead of $ for safety
      
      if(is.null(outcome_path)) {
        stop("outcome_path is NULL")
      }
      
      if(!is.data.frame(outcome_path)) {
        stop(paste("outcome_path is not a data frame, got:", class(outcome_path)[1]))
      }
      
      # Extract gap series - use [[ for safer access and validate columns exist
      if(!"time" %in% names(outcome_path)) {
        stop("outcome_path missing 'time' column")
      }
      if(!"gap" %in% names(outcome_path)) {
        stop("outcome_path missing 'gap' column")
      }
      
      # Extract columns safely
      time_col <- outcome_path[["time"]]
      gap_col <- outcome_path[["gap"]]
      
      # Validate they're vectors
      if(!is.vector(time_col) || !is.vector(gap_col)) {
        stop(paste("time or gap columns are not vectors. time:", class(time_col)[1], "gap:", class(gap_col)[1]))
      }
      
      # Ensure they have the same length
      if(length(time_col) != length(gap_col)) {
        stop(paste("time and gap columns have different lengths:", length(time_col), "vs", length(gap_col)))
      }
      
      gap_df <- data.frame(
        time = as.numeric(time_col),
        gap = as.numeric(gap_col),
        unit = unit,
        is_treated = (unit == treated_unit),
        stringsAsFactors = FALSE
      )
      
      # Validate gap data
      if(nrow(gap_df) == 0) {
        stop("Gap data is empty")
      }
      
      if(any(is.na(gap_df[["gap"]]))) {
        message(paste("  Warning: Gap data contains NA values for", unit))
      }
      
      all_gaps[[unit]] <- gap_df
      
      # Calculate RMSPE ratio (post/pre) - optional, don't fail if this doesn't work
      tryCatch({
        pre_rmspe <- synth_result[["rmspe"]]  # Use [[ instead of $
        if(!is.null(pre_rmspe) && !is.na(pre_rmspe)) {
          # Safely access post_treatment column
          if("post_treatment" %in% names(outcome_path)) {
            post_treatment_idx <- outcome_path[["post_treatment"]]  # Use [[ instead of $
            if(any(post_treatment_idx, na.rm = TRUE)) {
              post_data <- outcome_path[post_treatment_idx, , drop = FALSE]
              if(nrow(post_data) > 0 && "gap" %in% names(post_data)) {
                post_rmspe <- sqrt(mean(post_data[["gap"]]^2, na.rm = TRUE))  # Use [[ instead of $
                
                if(!is.na(pre_rmspe) && pre_rmspe > 0.001 && !is.na(post_rmspe)) {
                  rmspe_ratios[unit] <- post_rmspe / pre_rmspe
                }
              }
            }
          }
        }
      }, error = function(e) {
        error_msg <- tryCatch({
          conditionMessage(e)
        }, error = function(e2) {
          as.character(e)
        })
        message(paste("  Warning: Could not calculate RMSPE ratio for", unit, ":", error_msg))
      })
      
      return(TRUE)
      
    }, error = function(e) {
      error_msg <- tryCatch({
        paste("  Placebo failed for", unit, ":", conditionMessage(e))
      }, error = function(e2) {
        paste("  Placebo failed for", unit, ":", as.character(e))
      })
      message(error_msg)
      cat("ERROR:", error_msg, "\n")
      return(FALSE)
    })
    
    if(is.null(result) || !result) {
      failed_units <- c(failed_units, unit)
    }
  }
  
  # Combine all gaps
  if(length(all_gaps) > 0) {
    placebo_gaps <- do.call(rbind, all_gaps)
    rownames(placebo_gaps) <- NULL
  } else {
    stop("No successful placebo tests - all units failed. Check your data and configuration.")
  }
  
  # Calculate p-value (rank of treated unit)
  if(treated_unit %in% names(rmspe_ratios) && length(rmspe_ratios) > 1) {
    treated_ratio <- rmspe_ratios[treated_unit]
    ranking <- mean(rmspe_ratios >= treated_ratio, na.rm = TRUE)
  } else {
    ranking <- NA
    if(length(rmspe_ratios) == 0) {
      message("Warning: No RMSPE ratios calculated - cannot compute ranking")
    } else if(!treated_unit %in% names(rmspe_ratios)) {
      message("Warning: Treated unit not in RMSPE ratios - cannot compute ranking")
    }
  }
  
  successful_count <- length(all_gaps)
  message(paste("Placebo tests completed.", successful_count, "/", total_units, "successful."))
  if(length(failed_units) > 0) {
    message(paste("Failed units:", paste(failed_units, collapse = ", ")))
  }
  
  list(
    placebo_gaps = placebo_gaps,
    rmspe_ratios = rmspe_ratios,
    ranking = ranking,
    successful_units = successful_count,
    total_units = total_units,
    failed_units = failed_units
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

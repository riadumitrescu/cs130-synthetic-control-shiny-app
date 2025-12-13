# Advanced Placebo Test Functions for Synthetic Control Analysis
# Implements in-space and in-time placebo tests using the Synth package

library(dplyr)
library(tidyr)
library(ggplot2)

#' Wrapper function to standardize synth analysis calls for placebo tests
#'
#' @param data Panel dataset
#' @param treated_unit_id The unit to treat
#' @param treat_time_point The treatment time
#' @param pre_periods Vector of pre-treatment periods
#' @param post_periods Vector of post-treatment periods
#' @param donor_pool Vector of donor unit IDs
#' @param unit_var Name of unit identifier column
#' @param time_var Name of time variable column
#' @param outcome_var Name of outcome variable column
#' @param predictor_vars Vector of predictor variable names
#' @param special_predictors_config List of special predictor configs
#' @return List with time, Y_treated, Y_synth, gap, and metadata
synth_runner <- function(data, treated_unit_id, treat_time_point,
                        pre_periods, post_periods, donor_pool,
                        unit_var, time_var, outcome_var,
                        predictor_vars = NULL, special_predictors_config = NULL) {

  tryCatch({
    # Filter data to only include relevant units and time periods
    all_periods <- c(pre_periods, post_periods)
    filtered_data <- data[data[[unit_var]] %in% c(treated_unit_id, donor_pool) &
                         data[[time_var]] %in% all_periods, ]

    if(nrow(filtered_data) == 0) {
      stop("No data after filtering")
    }

    # Run synthetic control analysis
    synth_result <- run_synth_analysis(
      data = filtered_data,
      unit_var = unit_var,
      time_var = time_var,
      outcome_var = outcome_var,
      treated_unit = treated_unit_id,
      treatment_year = treat_time_point,
      predictor_vars = predictor_vars,
      special_predictors_config = special_predictors_config
    )

    # Extract outcome paths
    outcome_path <- synth_result$outcome_path

    # Return standardized format
    list(
      time = outcome_path$time,
      Y_treated = outcome_path$treated_outcome,
      Y_synth = outcome_path$synthetic_outcome,
      gap = outcome_path$gap,
      rmspe = synth_result$rmspe,
      converged = synth_result$converged,
      weights = synth_result$weights
    )

  }, error = function(e) {
    # Return empty result on error
    list(
      time = numeric(0),
      Y_treated = numeric(0),
      Y_synth = numeric(0),
      gap = numeric(0),
      rmspe = NA,
      converged = FALSE,
      weights = NULL,
      error = e$message
    )
  })
}


#' In-space placebo test (permutation test across units)
#'
#' Tests whether the treatment effect is unusually large compared to
#' what we would observe if we applied the same analysis to control units
#'
#' @param data Panel dataset in long format
#' @param outcome_var Name of outcome variable
#' @param unit_var Name of unit identifier column
#' @param time_var Name of time variable column
#' @param treated_unit ID of the real treated unit
#' @param treat_time Numeric time of intervention
#' @param donor_units Vector of donor unit IDs (if NULL, uses all units except treated)
#' @param pre_period Vector of pre-treatment periods (if NULL, auto-detected)
#' @param post_period Vector of post-treatment periods (if NULL, auto-detected)
#' @param predictor_vars Vector of predictor variable names
#' @param special_predictors_config List of special predictor configs
#' @param exclude_treated_from_donor Whether to exclude real treated unit from donor pools in placebos
#' @return List with gap_df (long format gaps) and summary_df (unit-level summaries)
in_space_placebo <- function(data, outcome_var, unit_var, time_var,
                            treated_unit, treat_time,
                            donor_units = NULL, pre_period = NULL, post_period = NULL,
                            predictor_vars = NULL, special_predictors_config = NULL,
                            exclude_treated_from_donor = TRUE) {

  # Data validation
  if(!all(c(unit_var, time_var, outcome_var) %in% names(data))) {
    stop("Required variables not found in data")
  }

  # Auto-detect periods if not provided
  all_times <- sort(unique(data[[time_var]]))
  if(is.null(pre_period)) {
    pre_period <- all_times[all_times < treat_time]
  }
  if(is.null(post_period)) {
    post_period <- all_times[all_times >= treat_time]
  }

  # Auto-detect donor units if not provided
  if(is.null(donor_units)) {
    all_units <- unique(data[[unit_var]])
    donor_units <- setdiff(all_units, treated_unit)
  }

  # Validate we have enough units and periods
  if(length(donor_units) < 2) {
    stop("Need at least 2 donor units for synthetic control")
  }
  if(length(pre_period) < 2) {
    stop("Need at least 2 pre-treatment periods")
  }
  if(length(post_period) < 1) {
    stop("Need at least 1 post-treatment period")
  }

  message(paste("Running in-space placebo test with", length(donor_units), "donor units"))
  message(paste("Pre-treatment periods:", length(pre_period), "Post-treatment periods:", length(post_period)))

  results_list <- list()

  # 1. Real treated unit
  message(paste("1. Analyzing real treated unit:", treated_unit))
  res_treated <- synth_runner(
    data = data,
    treated_unit_id = treated_unit,
    treat_time_point = treat_time,
    pre_periods = pre_period,
    post_periods = post_period,
    donor_pool = donor_units,
    unit_var = unit_var,
    time_var = time_var,
    outcome_var = outcome_var,
    predictor_vars = predictor_vars,
    special_predictors_config = special_predictors_config
  )

  if(length(res_treated$time) == 0) {
    stop(paste("Failed to analyze treated unit:", treated_unit))
  }

  # Calculate RMSPE ratios
  gap_treated <- res_treated$gap
  pre_gaps_treated <- gap_treated[res_treated$time %in% pre_period]
  post_gaps_treated <- gap_treated[res_treated$time %in% post_period]

  RMSPE_pre_treated <- sqrt(mean(pre_gaps_treated^2, na.rm = TRUE))
  RMSPE_post_treated <- sqrt(mean(post_gaps_treated^2, na.rm = TRUE))
  ratio_treated <- if(RMSPE_pre_treated > 0) RMSPE_post_treated / RMSPE_pre_treated else NA

  results_list[[length(results_list) + 1]] <- list(
    unit = treated_unit,
    time = res_treated$time,
    gap = gap_treated,
    RMSPE_pre = RMSPE_pre_treated,
    RMSPE_post = RMSPE_post_treated,
    ratio = ratio_treated,
    is_real_treated = TRUE,
    converged = res_treated$converged
  )

  # 2. Placebo tests for each donor unit
  successful_placebos <- 0
  for(i in seq_along(donor_units)) {
    u <- donor_units[i]
    message(paste0("2.", i, " Placebo test for donor unit: ", u, " (", i, "/", length(donor_units), ")"))

    # Define donor pool for this placebo
    if(exclude_treated_from_donor) {
      donor_u <- setdiff(donor_units, u)  # Exclude both u and treated_unit
    } else {
      donor_u <- setdiff(c(treated_unit, donor_units), u)  # Include treated_unit, exclude u
    }

    # If still insufficient, try including treated unit
    if(length(donor_u) < 2 && exclude_treated_from_donor) {
      donor_u <- setdiff(c(treated_unit, donor_units), u)
      message(paste("  Including treated unit in donor pool for", u, "due to insufficient donors"))
    }

    if(length(donor_u) < 2) {
      message(paste("  Skipping", u, "- insufficient donor pool"))
      next
    }

    res_u <- synth_runner(
      data = data,
      treated_unit_id = u,
      treat_time_point = treat_time,
      pre_periods = pre_period,
      post_periods = post_period,
      donor_pool = donor_u,
      unit_var = unit_var,
      time_var = time_var,
      outcome_var = outcome_var,
      predictor_vars = predictor_vars,
      special_predictors_config = special_predictors_config
    )

    if(length(res_u$time) == 0 || !is.null(res_u$error)) {
      message(paste("  Failed for unit", u, ":", res_u$error))
      next
    }

    # Calculate RMSPE ratios
    gap_u <- res_u$gap
    pre_gaps_u <- gap_u[res_u$time %in% pre_period]
    post_gaps_u <- gap_u[res_u$time %in% post_period]

    RMSPE_pre_u <- sqrt(mean(pre_gaps_u^2, na.rm = TRUE))
    RMSPE_post_u <- sqrt(mean(post_gaps_u^2, na.rm = TRUE))
    ratio_u <- if(RMSPE_pre_u > 0) RMSPE_post_u / RMSPE_pre_u else NA

    results_list[[length(results_list) + 1]] <- list(
      unit = u,
      time = res_u$time,
      gap = gap_u,
      RMSPE_pre = RMSPE_pre_u,
      RMSPE_post = RMSPE_post_u,
      ratio = ratio_u,
      is_real_treated = FALSE,
      converged = res_u$converged
    )

    successful_placebos <- successful_placebos + 1
  }

  message(paste("Completed:", successful_placebos, "successful placebo tests"))

  # Combine results into data frames
  gap_df_list <- list()
  summary_df_list <- list()

  for(res in results_list) {
    # Long format for gaps
    gap_df_list[[length(gap_df_list) + 1]] <- data.frame(
      unit = res$unit,
      time = res$time,
      gap = res$gap,
      is_real_treated = res$is_real_treated,
      stringsAsFactors = FALSE
    )

    # Summary format
    summary_df_list[[length(summary_df_list) + 1]] <- data.frame(
      unit = res$unit,
      RMSPE_pre = res$RMSPE_pre,
      RMSPE_post = res$RMSPE_post,
      ratio = res$ratio,
      is_real_treated = res$is_real_treated,
      converged = res$converged,
      stringsAsFactors = FALSE
    )
  }

  gap_df <- do.call(rbind, gap_df_list)
  summary_df <- do.call(rbind, summary_df_list)
  rownames(gap_df) <- NULL
  rownames(summary_df) <- NULL

  # Calculate p-value (rank of treated unit)
  valid_ratios <- summary_df$ratio[!is.na(summary_df$ratio)]
  if(length(valid_ratios) > 1 && !is.na(ratio_treated)) {
    p_value <- mean(valid_ratios >= ratio_treated, na.rm = TRUE)
  } else {
    p_value <- NA
  }

  return(list(
    gap_df = gap_df,
    summary_df = summary_df,
    p_value = p_value,
    treat_time = treat_time,
    treated_unit = treated_unit,
    successful_placebos = successful_placebos,
    total_attempted = length(donor_units)
  ))
}


#' In-time placebo test (fake intervention dates)
#'
#' Tests whether we observe similar "effects" when we pretend the intervention
#' happened at earlier time points (when we know no intervention occurred)
#'
#' @param data Panel dataset in long format
#' @param outcome_var Name of outcome variable
#' @param unit_var Name of unit identifier column
#' @param time_var Name of time variable column
#' @param treated_unit ID of the treated unit
#' @param true_treat_time The real treatment time
#' @param fake_treat_times Vector of fake treatment times to test
#' @param donor_units Vector of donor unit IDs
#' @param predictor_vars Vector of predictor variable names
#' @param special_predictors_config List of special predictor configs
#' @param min_pre_periods Minimum number of pre-periods required for each fake test
#' @param min_post_periods Minimum number of post-periods required for each fake test
#' @return List with gap_df (long format) and summary_df (fake-time level summaries)
in_time_placebo <- function(data, outcome_var, unit_var, time_var,
                           treated_unit, true_treat_time, fake_treat_times,
                           donor_units = NULL,
                           predictor_vars = NULL, special_predictors_config = NULL,
                           min_pre_periods = 3, min_post_periods = 2) {

  # Data validation
  if(!all(c(unit_var, time_var, outcome_var) %in% names(data))) {
    stop("Required variables not found in data")
  }

  # Auto-detect donor units if not provided
  if(is.null(donor_units)) {
    all_units <- unique(data[[unit_var]])
    donor_units <- setdiff(all_units, treated_unit)
  }

  # Validate fake treatment times
  all_times <- sort(unique(data[[time_var]]))
  fake_treat_times <- fake_treat_times[fake_treat_times < true_treat_time]
  fake_treat_times <- fake_treat_times[fake_treat_times %in% all_times]

  if(length(fake_treat_times) == 0) {
    stop("No valid fake treatment times provided")
  }

  # Filter fake times to ensure sufficient pre and post periods
  pre_treat_times <- all_times[all_times < true_treat_time]
  valid_fake_times <- c()

  for(fake_time in fake_treat_times) {
    pre_periods_fake <- pre_treat_times[pre_treat_times < fake_time]
    post_periods_fake <- pre_treat_times[pre_treat_times >= fake_time]

    if(length(pre_periods_fake) >= min_pre_periods && length(post_periods_fake) >= min_post_periods) {
      valid_fake_times <- c(valid_fake_times, fake_time)
    }
  }

  if(length(valid_fake_times) == 0) {
    stop(paste("No fake treatment times have sufficient periods. Need at least",
               min_pre_periods, "pre-periods and", min_post_periods, "post-periods."))
  }

  fake_treat_times <- valid_fake_times

  message(paste("Running in-time placebo test for treated unit:", treated_unit))
  message(paste("Testing", length(fake_treat_times), "fake treatment times"))

  results_list <- list()

  for(i in seq_along(fake_treat_times)) {
    fake_time <- fake_treat_times[i]
    message(paste0(i, ". Testing fake treatment time: ", fake_time))

    # Define periods for this fake treatment
    # Extend through the real treatment time to show comparison
    pre_period_fake <- all_times[all_times < fake_time]
    post_period_fake <- all_times[all_times >= fake_time & all_times <= true_treat_time]

    # Validate sufficient periods (but allow extension to true treatment)
    if(length(pre_period_fake) < min_pre_periods) {
      message(paste("  Skipping - insufficient pre-periods:", length(pre_period_fake)))
      next
    }
    # We need at least min_post_periods before the true treatment time
    post_before_true <- all_times[all_times >= fake_time & all_times < true_treat_time]
    if(length(post_before_true) < min_post_periods) {
      message(paste("  Skipping - insufficient post-periods before real treatment:", length(post_before_true)))
      next
    }

    message(paste("  Pre-periods:", length(pre_period_fake), "Post-periods:", length(post_period_fake)))

    # Run synthetic control with fake treatment time
    res_fake <- synth_runner(
      data = data,
      treated_unit_id = treated_unit,
      treat_time_point = fake_time,
      pre_periods = pre_period_fake,
      post_periods = post_period_fake,
      donor_pool = donor_units,
      unit_var = unit_var,
      time_var = time_var,
      outcome_var = outcome_var,
      predictor_vars = predictor_vars,
      special_predictors_config = special_predictors_config
    )

    if(length(res_fake$time) == 0 || !is.null(res_fake$error)) {
      message(paste("  Failed:", res_fake$error))
      next
    }

    # Calculate RMSPE ratios (only for periods before true treatment)
    gap_fake <- res_fake$gap
    pre_gaps_fake <- gap_fake[res_fake$time %in% pre_period_fake]
    # For RMSPE, only use post-fake periods before true treatment
    post_gaps_fake <- gap_fake[res_fake$time %in% post_before_true]

    RMSPE_pre_fake <- sqrt(mean(pre_gaps_fake^2, na.rm = TRUE))
    RMSPE_post_fake <- sqrt(mean(post_gaps_fake^2, na.rm = TRUE))
    ratio_fake <- if(RMSPE_pre_fake > 0) RMSPE_post_fake / RMSPE_pre_fake else NA

    results_list[[length(results_list) + 1]] <- list(
      fake_treat_time = fake_time,
      time = res_fake$time,
      gap = gap_fake,
      Y_treated = res_fake$Y_treated,
      Y_synth = res_fake$Y_synth,
      RMSPE_pre = RMSPE_pre_fake,
      RMSPE_post = RMSPE_post_fake,
      ratio = ratio_fake,
      converged = res_fake$converged,
      pre_periods_count = length(pre_period_fake),
      post_periods_count = length(post_before_true)
    )

    message(paste("  Success - RMSPE ratio:", round(ratio_fake, 3)))
  }

  message(paste("Completed:", length(results_list), "successful in-time placebo tests"))

  if(length(results_list) == 0) {
    stop("No successful in-time placebo tests")
  }

  # Combine results into data frames
  gap_df_list <- list()
  path_df_list <- list()
  summary_df_list <- list()

  for(res in results_list) {
    # Long format for gaps
    gap_df_list[[length(gap_df_list) + 1]] <- data.frame(
      fake_treat_time = res$fake_treat_time,
      time = res$time,
      gap = res$gap,
      stringsAsFactors = FALSE
    )

    # Long format for paths (treated and synthetic)
    path_df_list[[length(path_df_list) + 1]] <- data.frame(
      fake_treat_time = res$fake_treat_time,
      time = res$time,
      Y_treated = res$Y_treated,
      Y_synth = res$Y_synth,
      stringsAsFactors = FALSE
    )

    # Summary format
    summary_df_list[[length(summary_df_list) + 1]] <- data.frame(
      fake_treat_time = res$fake_treat_time,
      RMSPE_pre = res$RMSPE_pre,
      RMSPE_post = res$RMSPE_post,
      ratio = res$ratio,
      converged = res$converged,
      pre_periods_count = res$pre_periods_count,
      post_periods_count = res$post_periods_count,
      stringsAsFactors = FALSE
    )
  }

  gap_df <- do.call(rbind, gap_df_list)
  path_df <- do.call(rbind, path_df_list)
  summary_df <- do.call(rbind, summary_df_list)
  rownames(gap_df) <- NULL
  rownames(path_df) <- NULL
  rownames(summary_df) <- NULL

  return(list(
    gap_df = gap_df,
    path_df = path_df,
    summary_df = summary_df,
    true_treat_time = true_treat_time,
    treated_unit = treated_unit,
    successful_tests = length(results_list),
    total_attempted = length(fake_treat_times)
  ))
}


#' Plot in-space placebo test results
#'
#' @param placebo_result Result from in_space_placebo()
#' @param title Plot title
#' @return ggplot object
plot_in_space_placebo <- function(placebo_result, title = "In-Space Placebo Test") {

  gap_df <- placebo_result$gap_df
  treat_time <- placebo_result$treat_time

  # Separate treated and placebo units
  placebo_gaps <- gap_df[!gap_df$is_real_treated, ]
  treated_gaps <- gap_df[gap_df$is_real_treated, ]

  p <- ggplot() +
    # Placebo units as grey lines
    geom_line(data = placebo_gaps,
              aes(x = time, y = gap, group = unit),
              color = "grey60", alpha = 0.7, linewidth = 0.5) +
    # Real treated unit as thick colored line
    geom_line(data = treated_gaps,
              aes(x = time, y = gap),
              color = "red", linewidth = 1.2) +
    # Treatment time indicator
    geom_vline(xintercept = treat_time, linetype = "dashed", color = "blue") +
    # Zero line
    geom_hline(yintercept = 0, color = "black", alpha = 0.3) +
    labs(
      title = title,
      x = "Time",
      y = "Gap (Treated - Synthetic)",
      caption = paste0("Red line: ", placebo_result$treated_unit,
                      " | Grey lines: Placebo units",
                      " | P-value: ", round(placebo_result$p_value, 3))
    ) +
    theme_minimal()

  return(p)
}


#' Plot in-time placebo test results
#'
#' @param placebo_result Result from in_time_placebo()
#' @param title Plot title
#' @return ggplot object
plot_in_time_placebo <- function(placebo_result, title = "In-Time Placebo Test") {

  gap_df <- placebo_result$gap_df
  true_treat_time <- placebo_result$true_treat_time

  # Get the fake treatment time (should be just one now)
  fake_treat_time <- unique(gap_df$fake_treat_time)[1]

  p <- ggplot(gap_df, aes(x = time, y = gap)) +
    geom_line(linewidth = 0.8, color = "steelblue") +
    # Fake treatment time indicator
    geom_vline(aes(xintercept = fake_treat_time, linetype = "Fake Treatment"),
               color = "orange", linewidth = 1) +
    # Real treatment time indicator
    geom_vline(aes(xintercept = true_treat_time, linetype = "Real Treatment"),
               color = "red", linewidth = 1) +
    # Zero line
    geom_hline(yintercept = 0, color = "black", alpha = 0.3) +
    # Manual legend for vertical lines
    scale_linetype_manual(
      name = "Treatment Times",
      values = c("Fake Treatment" = "dashed", "Real Treatment" = "solid"),
      guide = guide_legend(override.aes = list(color = c("orange", "red")))
    ) +
    labs(
      title = title,
      x = "Time",
      y = "Gap (Treated - Synthetic)",
      caption = paste0("Unit: ", placebo_result$treated_unit,
                      " | Fake treatment: ", fake_treat_time,
                      " | Real treatment: ", true_treat_time)
    ) +
    theme_minimal() +
    theme(legend.position = "bottom")

  return(p)
}


#' Plot in-time placebo test paths (treated vs synthetic)
#'
#' @param placebo_result Result from in_time_placebo()
#' @param title Plot title
#' @return ggplot object
plot_in_time_placebo_paths <- function(placebo_result, title = "In-Time Placebo Test: Outcome Paths") {

  path_df <- placebo_result$path_df
  true_treat_time <- placebo_result$true_treat_time

  # Get the fake treatment time
  fake_treat_time <- unique(path_df$fake_treat_time)[1]

  # Reshape for plotting
  path_long <- rbind(
    data.frame(
      time = path_df$time,
      outcome = path_df$Y_treated,
      type = "Treated",
      stringsAsFactors = FALSE
    ),
    data.frame(
      time = path_df$time,
      outcome = path_df$Y_synth,
      type = "Synthetic",
      stringsAsFactors = FALSE
    )
  )

  p <- ggplot(path_long, aes(x = time, y = outcome, color = type, linetype = type)) +
    geom_line(linewidth = 0.9) +
    # Fake treatment time indicator
    geom_vline(aes(xintercept = fake_treat_time),
               color = "orange", linetype = "dotted", linewidth = 1) +
    # Real treatment time indicator
    geom_vline(aes(xintercept = true_treat_time),
               color = "red", linetype = "dotted", linewidth = 1) +
    annotate("text", x = fake_treat_time, y = Inf,
             label = "Fake Treatment", vjust = 1.5, hjust = 0.5,
             color = "orange", size = 3.5, fontface = "bold") +
    annotate("text", x = true_treat_time, y = Inf,
             label = "Real Treatment", vjust = 3, hjust = 0.5,
             color = "red", size = 3.5, fontface = "bold") +
    scale_color_manual(
      name = "Outcome",
      values = c("Treated" = "black", "Synthetic" = "steelblue")
    ) +
    scale_linetype_manual(
      name = "Outcome",
      values = c("Treated" = "solid", "Synthetic" = "dashed")
    ) +
    labs(
      title = title,
      x = "Time",
      y = "Outcome",
      caption = paste0("Unit: ", placebo_result$treated_unit,
                      " | Fake treatment: ", fake_treat_time,
                      " | Real treatment: ", true_treat_time)
    ) +
    theme_minimal() +
    theme(legend.position = "bottom")

  return(p)
}


#' Plot in-space placebo RMSPE ratio histogram
#'
#' @param placebo_result Result from in_space_placebo()
#' @param title Plot title
#' @return ggplot object
plot_in_space_rmspe_histogram <- function(placebo_result, title = "In-Space Placebo: Post/Pre RMSPE Ratios") {

  summary_df <- placebo_result$summary_df
  treated_unit <- placebo_result$treated_unit

  # Filter out NA ratios
  rmspe_filtered <- summary_df[!is.na(summary_df$ratio), ]

  if(nrow(rmspe_filtered) == 0) {
    stop("No valid RMSPE ratios to plot")
  }

  # Get treated unit result
  treated_result <- rmspe_filtered[rmspe_filtered$is_real_treated == TRUE, ]

  if(nrow(treated_result) == 0) {
    stop("Treated unit not found in results")
  }

  treated_ratio <- treated_result$ratio[1]

  # Calculate rank and p-value
  rank <- sum(rmspe_filtered$ratio >= treated_ratio, na.rm = TRUE)
  p_value <- rank / nrow(rmspe_filtered)

  # Determine bin width dynamically
  max_ratio <- max(rmspe_filtered$ratio, na.rm = TRUE)
  bin_width <- max(1, ceiling(max_ratio / 20))

  p <- ggplot(rmspe_filtered, aes(x = ratio, fill = is_real_treated)) +
    geom_histogram(breaks = seq(0, max_ratio + bin_width, by = bin_width),
                   color = "black", alpha = 0.8) +
    scale_fill_manual(
      values = c("FALSE" = "gray70", "TRUE" = "red"),
      labels = c("FALSE" = "Donor Units", "TRUE" = treated_unit),
      name = ""
    ) +
    geom_vline(xintercept = treated_ratio,
               color = "red",
               linetype = "dashed",
               linewidth = 1) +
    annotate("text",
             x = treated_ratio,
             y = Inf,
             label = paste0(treated_unit, "\n", round(treated_ratio, 2)),
             vjust = 1.5,
             hjust = ifelse(treated_ratio > max_ratio * 0.7, 1.1, -0.1),
             color = "red",
             fontface = "bold",
             size = 3.5) +
    scale_x_continuous(breaks = seq(0, ceiling(max_ratio), by = max(2, ceiling(max_ratio / 10)))) +
    labs(
      x = "Post/Pre RMSPE Ratio",
      y = "Frequency",
      title = title,
      subtitle = paste0(treated_unit, " rank: ", rank, " out of ",
                        nrow(rmspe_filtered), " units (p = ",
                        round(p_value, 3), ")")
    ) +
    theme_minimal() +
    theme(
      panel.grid.minor = element_blank(),
      axis.text = element_text(size = 11),
      axis.title = element_text(size = 12),
      legend.position = "bottom"
    )

  return(p)
}
# Plotting Functions for Synthetic Control Analysis
library(ggplot2)
library(dplyr)
library(tidyr)
library(DT)

#' Create actual vs synthetic plot
#' @param outcome_data Data frame with time, treated_outcome, synthetic_outcome columns
#' @param treatment_year The treatment year
#' @param unit_name Name of the treated unit
#' @return ggplot object
plot_actual_vs_synthetic <- function(outcome_data, treatment_year, unit_name = "Treated Unit") {

  # Reshape data for plotting
  plot_data <- outcome_data %>%
    select(time, treated_outcome, synthetic_outcome) %>%
    pivot_longer(cols = c(treated_outcome, synthetic_outcome),
                names_to = "series", values_to = "outcome") %>%
    mutate(series = case_when(
      series == "treated_outcome" ~ unit_name,
      series == "synthetic_outcome" ~ paste("Synthetic", unit_name)
    ))

  # Create plot
  p <- ggplot(plot_data, aes(x = time, y = outcome, color = series, linetype = series)) +
    geom_line(size = 1.2) +
    geom_vline(xintercept = treatment_year, linetype = "dashed", color = "red", alpha = 0.7) +
    scale_color_manual(values = c("black", "blue")) +
    scale_linetype_manual(values = c("solid", "dashed")) +
    labs(
      title = "Actual vs Synthetic Outcome",
      subtitle = paste("Treatment begins in", treatment_year),
      x = "Time",
      y = "Outcome",
      color = "",
      linetype = ""
    ) +
    theme_minimal() +
    theme(
      plot.title = element_text(size = 14, face = "bold"),
      legend.position = "bottom",
      panel.grid.minor = element_blank()
    )

  return(p)
}

#' Create gap plot (treatment effect over time)
#' @param outcome_data Data frame with time, gap, post_treatment columns
#' @param treatment_year The treatment year
#' @return ggplot object
plot_treatment_gap <- function(outcome_data, treatment_year) {

  p <- ggplot(outcome_data, aes(x = time, y = gap)) +
    geom_hline(yintercept = 0, linetype = "solid", color = "gray50", alpha = 0.7) +
    geom_line(size = 1.2, color = "darkred") +
    geom_vline(xintercept = treatment_year, linetype = "dashed", color = "red", alpha = 0.7) +
    labs(
      title = "Treatment Effect Over Time",
      subtitle = paste("Gap = Actual - Synthetic. Treatment begins in", treatment_year),
      x = "Time",
      y = "Gap (Treatment Effect)"
    ) +
    theme_minimal() +
    theme(
      plot.title = element_text(size = 14, face = "bold"),
      panel.grid.minor = element_blank()
    )

  return(p)
}

#' Create donor weights bar chart
#' @param weights Named vector of donor weights
#' @param title_suffix Optional suffix for the title
#' @return ggplot object
plot_donor_weights <- function(weights, title_suffix = "") {

  # Convert to data frame
  weight_data <- data.frame(
    unit = names(weights),
    weight = as.numeric(weights)
  ) %>%
    arrange(desc(weight))

  # Create plot
  p <- ggplot(weight_data, aes(x = reorder(unit, weight), y = weight)) +
    geom_col(fill = "steelblue", alpha = 0.8) +
    coord_flip() +
    labs(
      title = paste("Donor Unit Weights", title_suffix),
      x = "Donor Units",
      y = "Weight"
    ) +
    theme_minimal() +
    theme(
      plot.title = element_text(size = 14, face = "bold"),
      panel.grid.minor = element_blank()
    )

  return(p)
}

#' Create predictor balance table
#' @param balance_data Data frame with predictor, treated, synthetic, difference columns
#' @return DT datatable
create_balance_table <- function(balance_data) {

  # Format the data
  formatted_data <- balance_data %>%
    mutate(
      Treated = round(treated, 3),
      Synthetic = round(synthetic, 3),
      Difference = round(difference, 3),
      `Abs Diff` = round(abs(difference), 3)
    ) %>%
    select(Predictor = predictor, Treated, Synthetic, Difference, `Abs Diff`)

  # Create table
  DT::datatable(formatted_data,
                options = list(
                  pageLength = 15,
                  searching = FALSE,
                  ordering = TRUE,
                  dom = "t"
                ),
                caption = "Pre-treatment Predictor Balance",
                class = "compact stripe") %>%
    DT::formatStyle("Abs Diff",
                   backgroundColor = DT::styleInterval(
                     cuts = c(0.1, 0.5),
                     values = c("white", "#fff3cd", "#f8d7da")
                   ))
}

#' Create placebo plot
#' @param placebo_data Data frame with time, gap, unit, is_treated columns
#' @param treatment_year The treatment year
#' @return ggplot object
plot_placebo_gaps <- function(placebo_data, treatment_year) {

  p <- ggplot(placebo_data, aes(x = time, y = gap, group = unit)) +
    geom_hline(yintercept = 0, linetype = "solid", color = "gray50", alpha = 0.7) +
    geom_line(data = filter(placebo_data, !is_treated),
             color = "gray70", alpha = 0.6, size = 0.8) +
    geom_line(data = filter(placebo_data, is_treated),
             color = "darkred", size = 1.5) +
    geom_vline(xintercept = treatment_year, linetype = "dashed", color = "red", alpha = 0.7) +
    labs(
      title = "Placebo Tests: Treatment Effect vs Donor Units",
      subtitle = "Gray lines: placebo effects for donor units. Red line: actual treated unit.",
      x = "Time",
      y = "Gap (Treatment Effect)"
    ) +
    theme_minimal() +
    theme(
      plot.title = element_text(size = 14, face = "bold"),
      panel.grid.minor = element_blank()
    )

  return(p)
}
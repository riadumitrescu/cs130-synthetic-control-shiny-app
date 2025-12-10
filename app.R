# Synthetic Control Shiny Application
# A simple, no-code tool for synthetic control analysis

# Load required packages
library(shiny)
library(DT)
library(ggplot2)
library(readxl)
library(dplyr)
library(tidyr)
library(shinydashboard)
library(quadprog)

# Source helper functions
source("functions/data_functions.R")
source("functions/synthetic_control_bulletproof.R")
source("functions/plotting_functions.R")

# Define UI
ui <- dashboardPage(
  dashboardHeader(title = "Synthetic Control Analysis"),

  dashboardSidebar(
    sidebarMenu(
      menuItem("1. Upload Data", tabName = "upload", icon = icon("upload")),
      menuItem("2. Configure Analysis", tabName = "config", icon = icon("cog")),
      menuItem("3. Results", tabName = "results", icon = icon("chart-line")),
      menuItem("4. Placebo Tests", tabName = "placebo", icon = icon("flask")),
      menuItem("5. Export", tabName = "export", icon = icon("download"))
    )
  ),

  dashboardBody(
    tags$head(
      tags$style(HTML("
        .content-wrapper, .right-side {
          background-color: #f8f9fa;
        }
        .box {
          border-radius: 8px;
          box-shadow: 0 1px 3px rgba(0,0,0,0.12);
        }
        .main-header .navbar {
          background-color: #2c3e50;
        }
        .skin-blue .main-sidebar {
          background-color: #34495e;
        }
      "))
    ),

    tabItems(
      # Data Upload Tab
      tabItem(tabName = "upload",
        fluidRow(
          box(
            title = "Data Upload", status = "primary", solidHeader = TRUE, width = 12,
            p("Upload your panel dataset (CSV or Excel format). Your data should have columns for:",
              tags$br(), "• Unit identifier (e.g., country, state, firm)",
              tags$br(), "• Time variable (e.g., year, quarter)",
              tags$br(), "• Outcome variable of interest",
              tags$br(), "• Predictor variables "),

            fileInput("file", "Choose Dataset File",
                     accept = c(".csv", ".xlsx", ".xls"),
                     placeholder = "No file selected"),

            conditionalPanel(
              condition = "output.fileUploaded",
              hr(),
              h4("Data Preview"),
              DT::dataTableOutput("dataPreview"),
              br(),
              div(id = "validationResults",
                  uiOutput("validationOutput")
              )
            )
          )
        )
      ),

      # Configuration Tab
      tabItem(tabName = "config",
        conditionalPanel(
          condition = "output.fileUploaded",
          fluidRow(
            # Variable Mapping
            box(
              title = "Variable Mapping", status = "primary", solidHeader = TRUE, width = 6,
              p("Select the key variables for your analysis:"),
              selectInput("unitVar", "Unit Identifier:",
                         choices = NULL, selected = NULL),
              selectInput("timeVar", "Time Variable:",
                         choices = NULL, selected = NULL),
              selectInput("outcomeVar", "Outcome Variable:",
                         choices = NULL, selected = NULL),
              checkboxGroupInput("predictorVars", "Predictor Variables (required):",
                               choices = NULL, selected = NULL),
              p("Note: At least one predictor variable must be selected for synthetic control analysis.",
                style = "color: #856404; font-size: 12px; font-style: italic;")
            ),

            # Treatment Configuration
            box(
              title = "Treatment Setup", status = "info", solidHeader = TRUE, width = 6,
              conditionalPanel(
                condition = "output.variablesMapped",
                p("Configure your treatment settings:"),
                selectInput("treatedUnit", "Treated Unit:",
                           choices = NULL, selected = NULL),
                numericInput("treatmentYear", "Treatment Year:",
                            value = 2005, min = 2000, max = 2020),
                conditionalPanel(
                  condition = "input.treatedUnit != '' && input.treatmentYear",
                  hr(),
                  h5("Treatment Summary:", style = "font-weight: bold; color: #2c3e50;"),
                  div(id = "treatmentSummary",
                      uiOutput("treatmentInfo"),
                      style = "background-color: #f8f9fa; padding: 10px; border-radius: 5px; margin-top: 10px;")
                )
              )
            )
          ),

          fluidRow(
            # Predictor Builder
            box(
              title = "Predictor Configuration", status = "warning", solidHeader = TRUE, width = 6,
              conditionalPanel(
                condition = "output.treatmentConfigured",
                p("Choose how to build predictors for synthetic control:"),
                radioButtons("predictorMode", "Predictor Mode:",
                           choices = list(
                             "Simple (Recommended)" = "simple",
                             "Advanced" = "advanced"
                           ),
                           selected = "simple"),

                conditionalPanel(
                  condition = "input.predictorMode == 'simple'",
                  p("Simple mode automatically uses pre-treatment means of selected variables.",
                    style = "color: #6c757d; font-size: 14px;"),
                  conditionalPanel(
                    condition = "input.predictorVars != null && input.predictorVars.length > 0",
                    h6("Selected Predictors:"),
                    verbatimTextOutput("selectedPredictors", placeholder = TRUE)
                  ),
                  conditionalPanel(
                    condition = "input.predictorVars == null || input.predictorVars.length == 0",
                    div(class = "alert alert-warning", style = "font-size: 14px;",
                        icon("exclamation-triangle"), " Please select at least one predictor variable for analysis.")
                  )
                ),

                conditionalPanel(
                  condition = "input.predictorMode == 'advanced'",
                  p("Advanced mode allows custom predictor transformations.",
                    style = "color: #6c757d; font-size: 14px;"),
                  p("(Feature coming soon)", style = "font-style: italic; color: #dc3545;")
                )
              )
            ),

            # Analysis Preview
            box(
              title = "Analysis Preview", status = "success", solidHeader = TRUE, width = 6,
              conditionalPanel(
                condition = "output.readyForAnalysis",
                verbatimTextOutput("analysisSummary"),
                hr(),
                actionButton("runAnalysis", "Run Synthetic Control Analysis",
                           class = "btn-success btn-lg", style = "width: 100%;", icon = icon("play")),
                br(), br(),
                conditionalPanel(
                  condition = "output.analysisCompleted",
                  div(class = "alert alert-success",
                      icon("check"), " Analysis completed! View results in the Results tab.")
                )
              ),
              conditionalPanel(
                condition = "!output.readyForAnalysis",
                div(class = "alert alert-warning", style = "text-align: center;",
                    icon("exclamation-triangle"), " Complete the configuration above to run analysis.")
              )
            )
          )
        ),

        conditionalPanel(
          condition = "!output.fileUploaded",
          fluidRow(
            box(
              title = "Configuration", status = "warning", solidHeader = TRUE, width = 12,
              p("Please upload a dataset first to configure your analysis.",
                style = "text-align: center; color: #856404; font-size: 16px;")
            )
          )
        )
      ),

      # Results Tab
      tabItem(tabName = "results",
        conditionalPanel(
          condition = "output.analysisCompleted",

          # Summary Statistics Row
          fluidRow(
            valueBoxOutput("rmspBox"),
            valueBoxOutput("donorCountBox"),
            valueBoxOutput("treatmentEffectBox")
          ),

          # Main Plots Row
          fluidRow(
            box(
              title = "Actual vs Synthetic", status = "primary", solidHeader = TRUE, width = 6,
              plotOutput("actualVsSyntheticPlot", height = "400px")
            ),
            box(
              title = "Treatment Effect (Gap)", status = "info", solidHeader = TRUE, width = 6,
              plotOutput("gapPlot", height = "400px")
            )
          ),

          # Secondary Analysis Row
          fluidRow(
            box(
              title = "Donor Unit Weights", status = "success", solidHeader = TRUE, width = 6,
              plotOutput("weightsPlot", height = "350px")
            ),
            box(
              title = "Pre-treatment Balance", status = "warning", solidHeader = TRUE, width = 6,
              DT::dataTableOutput("balanceTable")
            )
          ),

          # Outcome Path Data
          fluidRow(
            box(
              title = "Outcome Data", status = "primary", solidHeader = TRUE, width = 12,
              collapsible = TRUE, collapsed = TRUE,
              DT::dataTableOutput("outcomeDataTable")
            )
          )
        ),

        conditionalPanel(
          condition = "!output.analysisCompleted",
          fluidRow(
            box(
              title = "Analysis Results", status = "primary", solidHeader = TRUE, width = 12,
              div(class = "alert alert-info", style = "text-align: center; margin: 40px;",
                  icon("info-circle"), " Run the analysis in the Configuration tab to see results here.")
            )
          )
        )
      ),

      # Placebo Tab
      tabItem(tabName = "placebo",
        conditionalPanel(
          condition = "output.analysisCompleted",

          fluidRow(
            box(
              title = "Placebo Test Controls", status = "primary", solidHeader = TRUE, width = 12,
              p("Placebo tests run synthetic control on each donor unit as if it received treatment."),
              actionButton("runPlacebo", "Run Placebo Tests",
                          class = "btn-warning btn-lg", icon = icon("flask")),
              conditionalPanel(
                condition = "output.placeboCompleted",
                hr(),
                div(class = "alert alert-success",
                    icon("check"), " Placebo tests completed!")
              )
            )
          ),

          conditionalPanel(
            condition = "output.placeboCompleted",

            # Placebo Results
            fluidRow(
              box(
                title = "Statistical Significance", status = "info", solidHeader = TRUE, width = 4,
                valueBoxOutput("placeboRankBox", width = 12)
              ),
              box(
                title = "Placebo Gap Trajectories", status = "warning", solidHeader = TRUE, width = 8,
                plotOutput("placeboPlot", height = "400px")
              )
            ),

            fluidRow(
              box(
                title = "Placebo Results Summary", status = "primary", solidHeader = TRUE, width = 12,
                DT::dataTableOutput("placeboTable")
              )
            )
          )
        ),

        conditionalPanel(
          condition = "!output.analysisCompleted",
          fluidRow(
            box(
              title = "Placebo Tests", status = "primary", solidHeader = TRUE, width = 12,
              div(class = "alert alert-info", style = "text-align: center; margin: 40px;",
                  icon("info-circle"), " Complete the main analysis first to run placebo tests.")
            )
          )
        )
      ),

      # Export Tab
      tabItem(tabName = "export",
        conditionalPanel(
          condition = "output.analysisCompleted",

          fluidRow(
            box(
              title = "Export Data", status = "primary", solidHeader = TRUE, width = 6,
              h4("Download Data Tables"),
              downloadButton("downloadResults", "Download Results (CSV)",
                           class = "btn-success", style = "margin-bottom: 10px; width: 100%;"),
              downloadButton("downloadWeights", "Download Weights (CSV)",
                           class = "btn-info", style = "margin-bottom: 10px; width: 100%;"),
              downloadButton("downloadBalance", "Download Balance Table (CSV)",
                           class = "btn-warning", style = "width: 100%;")
            ),

            box(
              title = "Export Plots", status = "info", solidHeader = TRUE, width = 6,
              h4("Download Visualizations"),
              downloadButton("downloadMainPlot", "Download Main Plot (PNG)",
                           class = "btn-success", style = "margin-bottom: 10px; width: 100%;"),
              downloadButton("downloadGapPlot", "Download Gap Plot (PNG)",
                           class = "btn-info", style = "margin-bottom: 10px; width: 100%;"),
              downloadButton("downloadWeightsPlot", "Download Weights Chart (PNG)",
                           class = "btn-warning", style = "width: 100%;")
            )
          ),

          fluidRow(
            box(
              title = "PDF Report", status = "success", solidHeader = TRUE, width = 12,
              p("Generate a comprehensive PDF report with all results, plots, and analysis summary."),
              downloadButton("downloadReport", "Generate PDF Report",
                           class = "btn-success btn-lg", style = "width: 100%;", icon = icon("file-pdf")),
              br(), br(),
              div(id = "reportStatus", style = "margin-top: 15px;")
            )
          )
        ),

        conditionalPanel(
          condition = "!output.analysisCompleted",
          fluidRow(
            box(
              title = "Export Results", status = "primary", solidHeader = TRUE, width = 12,
              div(class = "alert alert-info", style = "text-align: center; margin: 40px;",
                  icon("info-circle"), " Complete the analysis first to export results.")
            )
          )
        )
      )
    )
  )
)

# Define Server
server <- function(input, output, session) {

  # Reactive values for storing data and state
  values <- reactiveValues(
    data = NULL,
    validation = NULL,
    analysis_ready = FALSE,
    analysis_results = NULL,
    analysis_completed = FALSE,
    placebo_results = NULL,
    placebo_completed = FALSE
  )

  # File upload handling
  observeEvent(input$file, {
    req(input$file)

    # Read the uploaded file
    ext <- tools::file_ext(input$file$datapath)

    if(ext == "csv") {
      values$data <- read.csv(input$file$datapath, stringsAsFactors = FALSE)
    } else if(ext %in% c("xlsx", "xls")) {
      values$data <- read_excel(input$file$datapath)
    }

    # Validate the data structure
    values$validation <- validate_panel_data(values$data)

    # Update variable choices
    updateSelectInput(session, "unitVar", choices = names(values$data))
    updateSelectInput(session, "timeVar", choices = names(values$data))
    updateSelectInput(session, "outcomeVar", choices = names(values$data))
    updateCheckboxGroupInput(session, "predictorVars", choices = names(values$data))
  })

  # Update treated unit choices when unit variable is selected
  observeEvent(input$unitVar, {
    if(!is.null(values$data) && !is.null(input$unitVar)) {
      units <- unique(values$data[[input$unitVar]])
      updateSelectInput(session, "treatedUnit", choices = units)
    }
  })

  # Update treatment year range when time variable is selected
  observeEvent(input$timeVar, {
    if(!is.null(values$data) && !is.null(input$timeVar)) {
      years <- sort(unique(values$data[[input$timeVar]]))
      updateNumericInput(session, "treatmentYear",
                        min = min(years), max = max(years),
                        value = median(years))
    }
  })

  # Check if file is uploaded
  output$fileUploaded <- reactive({
    return(!is.null(values$data))
  })
  outputOptions(output, "fileUploaded", suspendWhenHidden = FALSE)

  # Check if variables are mapped
  output$variablesMapped <- reactive({
    return(!is.null(input$unitVar) && !is.null(input$timeVar) && !is.null(input$outcomeVar))
  })
  outputOptions(output, "variablesMapped", suspendWhenHidden = FALSE)

  # Check if treatment is configured
  output$treatmentConfigured <- reactive({
    return(!is.null(input$treatedUnit) && !is.null(input$treatmentYear) &&
           input$treatedUnit != "" && !is.na(input$treatmentYear))
  })
  outputOptions(output, "treatmentConfigured", suspendWhenHidden = FALSE)

  # Check if ready for analysis
  output$readyForAnalysis <- reactive({
    mapped <- !is.null(input$unitVar) && !is.null(input$timeVar) && !is.null(input$outcomeVar)
    treatment <- !is.null(input$treatedUnit) && !is.null(input$treatmentYear) &&
                 input$treatedUnit != "" && !is.na(input$treatmentYear)
    predictors <- !is.null(input$predictorVars) && length(input$predictorVars) > 0
    return(mapped && treatment && predictors)
  })
  outputOptions(output, "readyForAnalysis", suspendWhenHidden = FALSE)

  # Check if analysis is completed
  output$analysisCompleted <- reactive({
    return(values$analysis_completed)
  })
  outputOptions(output, "analysisCompleted", suspendWhenHidden = FALSE)

  # Check if placebo tests are completed
  output$placeboCompleted <- reactive({
    return(values$placebo_completed)
  })
  outputOptions(output, "placeboCompleted", suspendWhenHidden = FALSE)

  # Data preview table
  output$dataPreview <- DT::renderDataTable({
    req(values$data)
    DT::datatable(values$data,
                  options = list(scrollX = TRUE, pageLength = 5),
                  class = "compact stripe")
  })

  # Validation output
  output$validationOutput <- renderUI({
    req(values$validation)

    if(values$validation$valid) {
      div(class = "alert alert-success",
          icon("check-circle"), " Data structure looks good!",
          p(paste("Found", nrow(values$data), "observations and", ncol(values$data), "variables.")))
    } else {
      div(class = "alert alert-warning",
          icon("exclamation-triangle"), " Data validation warnings:",
          tags$ul(
            lapply(values$validation$messages, function(msg) tags$li(msg))
          ))
    }
  })

  # Treatment info summary
  output$treatmentInfo <- renderUI({
    req(input$treatedUnit, input$treatmentYear, input$timeVar, input$unitVar)

    # Calculate donor pool
    all_units <- unique(values$data[[input$unitVar]])
    donor_pool <- setdiff(all_units, input$treatedUnit)

    # Get time range
    all_years <- sort(unique(values$data[[input$timeVar]]))
    pre_years <- all_years[all_years < input$treatmentYear]
    post_years <- all_years[all_years >= input$treatmentYear]

    div(
      p(strong("Treatment Configuration:")),
      p("• Treated Unit:", input$treatedUnit),
      p("• Treatment Year:", input$treatmentYear),
      p("• Donor Pool:", length(donor_pool), "units"),
      p("• Pre-treatment period:", length(pre_years), "periods"),
      p("• Post-treatment period:", length(post_years), "periods")
    )
  })

  # Selected predictors display
  output$selectedPredictors <- renderText({
    req(input$predictorVars)
    if(length(input$predictorVars) > 0) {
      paste(input$predictorVars, collapse = "\n")
    } else {
      "None selected"
    }
  })

  # Analysis summary
  output$analysisSummary <- renderText({
    req(input$unitVar, input$timeVar, input$outcomeVar, input$treatedUnit, input$treatmentYear)

    predictors <- if(is.null(input$predictorVars) || length(input$predictorVars) == 0) {
      "⚠️ No predictors selected!"
    } else {
      paste(input$predictorVars, collapse = ", ")
    }

    # Get donor pool info
    all_units <- unique(values$data[[input$unitVar]])
    donor_pool <- setdiff(all_units, input$treatedUnit)

    # Get time info
    all_years <- sort(unique(values$data[[input$timeVar]]))
    pre_years <- all_years[all_years < input$treatmentYear]
    post_years <- all_years[all_years >= input$treatmentYear]

    paste(
      "Analysis Configuration:",
      paste("• Dataset:", nrow(values$data), "observations,", length(all_units), "units"),
      paste("• Unit Variable:", input$unitVar),
      paste("• Time Variable:", input$timeVar),
      paste("• Outcome Variable:", input$outcomeVar),
      paste("• Predictors:", predictors),
      paste("• Treated Unit:", input$treatedUnit),
      paste("• Treatment Year:", input$treatmentYear),
      paste("• Donor Pool Size:", length(donor_pool), "units"),
      paste("• Pre-treatment Periods:", length(pre_years)),
      paste("• Post-treatment Periods:", length(post_years)),
      "",
      "✓ Ready to run synthetic control analysis!",
      sep = "\n"
    )
  })

  # Run analysis when button is clicked
  observeEvent(input$runAnalysis, {
    req(values$data, input$unitVar, input$timeVar, input$outcomeVar,
        input$treatedUnit, input$treatmentYear)

    tryCatch({
      # Show progress
      showNotification("Running synthetic control analysis...", type = "message", duration = NULL, id = "analysis_progress")

      # Validate predictors are selected
      if(is.null(input$predictorVars) || length(input$predictorVars) == 0) {
        stop("Please select at least one predictor variable")
      }

      # Run the bulletproof analysis
      values$analysis_results <- run_synthetic_control_bulletproof(
        data = values$data,
        unit_var = input$unitVar,
        time_var = input$timeVar,
        outcome_var = input$outcomeVar,
        treated_unit = input$treatedUnit,
        treatment_year = input$treatmentYear,
        predictor_vars = input$predictorVars
      )

      values$analysis_completed <- TRUE

      # Remove progress notification and show success
      removeNotification("analysis_progress")
      showNotification("Analysis completed successfully!", type = "message", duration = 3)

    }, error = function(e) {
      removeNotification("analysis_progress")
      showNotification(paste("Analysis failed:", e$message), type = "error", duration = 5)
      values$analysis_completed <- FALSE
    })
  })

  # Value boxes for summary statistics
  output$rmspBox <- renderValueBox({
    valueBox(
      value = if(!is.null(values$analysis_results)) {
        round(values$analysis_results$rmspe, 3)
      } else {
        "N/A"
      },
      subtitle = "Pre-treatment RMSPE",
      icon = icon("chart-line"),
      color = "blue"
    )
  })

  output$donorCountBox <- renderValueBox({
    valueBox(
      value = if(!is.null(values$analysis_results)) {
        length(values$analysis_results$donor_units)
      } else {
        "N/A"
      },
      subtitle = "Donor Units",
      icon = icon("users"),
      color = "green"
    )
  })

  output$treatmentEffectBox <- renderValueBox({
    valueBox(
      value = if(!is.null(values$analysis_results)) {
        post_data <- values$analysis_results$outcome_path[values$analysis_results$outcome_path$post_treatment, ]
        round(mean(post_data$gap, na.rm = TRUE), 2)
      } else {
        "N/A"
      },
      subtitle = "Average Treatment Effect",
      icon = icon("bullseye"),
      color = "yellow"
    )
  })

  # Main plots
  output$actualVsSyntheticPlot <- renderPlot({
    req(values$analysis_results)
    plot_actual_vs_synthetic(
      values$analysis_results$outcome_path,
      values$analysis_results$treatment_year,
      values$analysis_results$treated_unit
    )
  })

  output$gapPlot <- renderPlot({
    req(values$analysis_results)
    plot_treatment_gap(
      values$analysis_results$outcome_path,
      values$analysis_results$treatment_year
    )
  })

  output$weightsPlot <- renderPlot({
    req(values$analysis_results)
    plot_donor_weights(values$analysis_results$weights)
  })

  # Tables
  output$balanceTable <- DT::renderDataTable({
    req(values$analysis_results)
    create_balance_table(values$analysis_results$predictor_balance)
  })

  output$outcomeDataTable <- DT::renderDataTable({
    req(values$analysis_results)
    formatted_data <- values$analysis_results$outcome_path %>%
      mutate(
        time = as.character(time),
        treated_outcome = round(treated_outcome, 3),
        synthetic_outcome = round(synthetic_outcome, 3),
        gap = round(gap, 3),
        post_treatment = ifelse(post_treatment, "Post", "Pre")
      ) %>%
      rename(
        Time = time,
        `Actual Outcome` = treated_outcome,
        `Synthetic Outcome` = synthetic_outcome,
        `Gap (Effect)` = gap,
        Period = post_treatment
      )

    DT::datatable(formatted_data,
                  options = list(pageLength = 15, scrollX = TRUE),
                  class = "compact stripe")
  })

  # Placebo test analysis
  observeEvent(input$runPlacebo, {
    req(values$analysis_results, input$unitVar, input$timeVar, input$outcomeVar,
        input$treatedUnit, input$treatmentYear)

    tryCatch({
      showNotification("Running placebo tests...", type = "message", duration = NULL, id = "placebo_progress")

      # Run bulletproof placebo tests
      values$placebo_results <- run_placebo_tests_bulletproof(
        data = values$data,
        unit_var = input$unitVar,
        time_var = input$timeVar,
        outcome_var = input$outcomeVar,
        treated_unit = input$treatedUnit,
        treatment_year = input$treatmentYear,
        predictor_vars = input$predictorVars
      )

      values$placebo_completed <- TRUE

      removeNotification("placebo_progress")
      showNotification("Placebo tests completed successfully!", type = "message", duration = 3)

    }, error = function(e) {
      removeNotification("placebo_progress")
      showNotification(paste("Placebo tests failed:", e$message), type = "error", duration = 5)
      values$placebo_completed <- FALSE
    })
  })

  # Placebo results
  output$placeboRankBox <- renderValueBox({
    valueBox(
      value = if(!is.null(values$placebo_results) && !is.na(values$placebo_results$ranking)) {
        paste0(round(values$placebo_results$ranking * 100, 1), "%")
      } else {
        "N/A"
      },
      subtitle = "P-value (Rank)",
      icon = icon("percentage"),
      color = "orange"
    )
  })

  output$placeboPlot <- renderPlot({
    req(values$placebo_results)
    plot_placebo_gaps(values$placebo_results$placebo_gaps, input$treatmentYear)
  })

  output$placeboTable <- DT::renderDataTable({
    req(values$placebo_results)

    # Create summary table with safe filtering
    req(values$placebo_results$placebo_gaps)

    # Use bulletproof filtering
    all_gaps <- values$placebo_results$placebo_gaps
    post_idx <- which(all_gaps$time >= input$treatmentYear)

    if(length(post_idx) > 0) {
      post_gaps <- all_gaps[post_idx, ]

      # Manual aggregation to avoid issues
      units <- unique(post_gaps$unit)
      summary_list <- list()

      for(u in units) {
        unit_data <- post_gaps[post_gaps$unit == u, ]
        summary_list[[u]] <- data.frame(
          Unit = u,
          Is_Treated = ifelse(any(unit_data$is_treated), "Yes", "No"),
          Avg_Effect = round(mean(unit_data$gap, na.rm = TRUE), 3),
          Max_Effect = round(max(unit_data$gap, na.rm = TRUE), 3),
          Min_Effect = round(min(unit_data$gap, na.rm = TRUE), 3),
          stringsAsFactors = FALSE
        )
      }

      summary_df <- do.call(rbind, summary_list)
    } else {
      summary_df <- data.frame(
        Unit = character(0),
        Is_Treated = character(0),
        Avg_Effect = numeric(0),
        Max_Effect = numeric(0),
        Min_Effect = numeric(0),
        stringsAsFactors = FALSE
      )
    }

    DT::datatable(summary_df,
                  options = list(pageLength = 10),
                  class = "compact stripe")
  })

  # Download handlers
  output$downloadResults <- downloadHandler(
    filename = function() {
      paste("synthetic_control_results_", Sys.Date(), ".csv", sep = "")
    },
    content = function(file) {
      req(values$analysis_results)
      write.csv(values$analysis_results$outcome_path, file, row.names = FALSE)
    }
  )

  output$downloadWeights <- downloadHandler(
    filename = function() {
      paste("donor_weights_", Sys.Date(), ".csv", sep = "")
    },
    content = function(file) {
      req(values$analysis_results)
      weights_df <- data.frame(
        Unit = names(values$analysis_results$weights),
        Weight = as.numeric(values$analysis_results$weights)
      )
      write.csv(weights_df, file, row.names = FALSE)
    }
  )

  output$downloadBalance <- downloadHandler(
    filename = function() {
      paste("predictor_balance_", Sys.Date(), ".csv", sep = "")
    },
    content = function(file) {
      req(values$analysis_results)
      write.csv(values$analysis_results$predictor_balance, file, row.names = FALSE)
    }
  )

  output$downloadMainPlot <- downloadHandler(
    filename = function() {
      paste("actual_vs_synthetic_", Sys.Date(), ".png", sep = "")
    },
    content = function(file) {
      req(values$analysis_results)
      p <- plot_actual_vs_synthetic(
        values$analysis_results$outcome_path,
        values$analysis_results$treatment_year,
        values$analysis_results$treated_unit
      )
      ggsave(file, plot = p, width = 10, height = 6, dpi = 300)
    }
  )

  output$downloadGapPlot <- downloadHandler(
    filename = function() {
      paste("treatment_gap_", Sys.Date(), ".png", sep = "")
    },
    content = function(file) {
      req(values$analysis_results)
      p <- plot_treatment_gap(
        values$analysis_results$outcome_path,
        values$analysis_results$treatment_year
      )
      ggsave(file, plot = p, width = 10, height = 6, dpi = 300)
    }
  )

  output$downloadWeightsPlot <- downloadHandler(
    filename = function() {
      paste("donor_weights_", Sys.Date(), ".png", sep = "")
    },
    content = function(file) {
      req(values$analysis_results)
      p <- plot_donor_weights(values$analysis_results$weights)
      ggsave(file, plot = p, width = 8, height = 6, dpi = 300)
    }
  )

  output$downloadReport <- downloadHandler(
    filename = function() {
      paste("synthetic_control_report_", Sys.Date(), ".pdf", sep = "")
    },
    content = function(file) {
      req(values$analysis_results)

      # Create a temporary RMarkdown file for the report
      tempReport <- file.path(tempdir(), "report.Rmd")

      # Copy the report template (we'll create a simple one)
      report_content <- sprintf('
---
title: "Synthetic Control Analysis Report"
date: "%s"
output: pdf_document
---

# Analysis Summary

**Treated Unit:** %s
**Treatment Year:** %s
**RMSPE:** %.3f
**Converged:** %s

# Results

## Donor Unit Weights

```{r echo=FALSE, results="asis"}
library(knitr)
weights_df <- data.frame(
  Unit = c(%s),
  Weight = c(%s)
)
kable(weights_df)
```

## Treatment Effects

Average post-treatment effect: %.2f

*Report generated automatically by Synthetic Control Shiny App*
',
        Sys.Date(),
        values$analysis_results$treated_unit,
        values$analysis_results$treatment_year,
        values$analysis_results$rmspe,
        values$analysis_results$converged,
        paste('"', names(values$analysis_results$weights), '"', collapse = ", "),
        paste(round(values$analysis_results$weights, 3), collapse = ", "),
        mean(values$analysis_results$outcome_path[values$analysis_results$outcome_path$post_treatment, "gap"], na.rm = TRUE)
      )

      writeLines(report_content, tempReport)

      # Render the report
      rmarkdown::render(tempReport, output_file = file, envir = new.env())
    }
  )

}

# Run the application
shinyApp(ui = ui, server = server)
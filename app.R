# Synthetic Control Shiny Application
# A simple, no-code tool for synthetic control analysis
# Uses the official Synth R package by Abadie et al.

# Load required packages
library(shiny)
library(DT)
library(ggplot2)
library(readxl)
library(dplyr)
library(tidyr)
library(shinydashboard)
library(Synth)  # Official synthetic control package

# Source helper functions
source("functions/data_functions.R")
source("functions/synth_wrapper.R")
source("functions/plotting_functions.R")

# Define UI
ui <- dashboardPage(
  dashboardHeader(
    title = "Synthetic Control Analysis",
    tags$li(
      class = "dropdown",
      style = "padding: 8px 15px;",
      actionButton("saveParamsBtn", 
                   "💾 Save Parameters", 
                   class = "btn-success",
                   style = "color: white; font-weight: bold; padding: 8px 16px; font-size: 14px;",
                   icon = icon("save")),
      tags$script(HTML("
        $(document).ready(function() {
          $('#saveParamsBtn').css({
            'background-color': '#28a745',
            'border-color': '#28a745',
            'margin-top': '8px',
            'box-shadow': '0 2px 4px rgba(0,0,0,0.2)'
          }).hover(function() {
            $(this).css('background-color', '#218838');
          }, function() {
            $(this).css('background-color', '#28a745');
          }).click(function() {
            var btn = $(this);
            var originalText = btn.html();
            btn.html('<i class=\"fa fa-spinner fa-spin\"></i> Saving...').prop('disabled', true);
            setTimeout(function() {
              btn.html(originalText).prop('disabled', false);
            }, 1000);
          });
        });
      "))
    )
  ),

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
        .settings-restored {
          background-color: #d4edda;
          border: 1px solid #c3e6cb;
          color: #155724;
          padding: 10px;
          border-radius: 5px;
          margin: 10px 0;
        }
      ")),
      tags$script(HTML("
        // IndexedDB Storage Manager for Synthetic Control App Settings
        (function() {
          const DB_NAME = 'SyntheticControlDB';
          const DB_VERSION = 1;
          const STORE_NAME = 'settings';
          let db = null;
          
          // Initialize IndexedDB
          function initDB() {
            return new Promise((resolve, reject) => {
              if (db) {
                resolve(db);
                return;
              }
              
              const request = indexedDB.open(DB_NAME, DB_VERSION);
              
              request.onerror = function() {
                console.error('IndexedDB error:', request.error);
                reject(request.error);
              };
              
              request.onsuccess = function() {
                db = request.result;
                console.log('IndexedDB opened successfully');
                resolve(db);
              };
              
              request.onupgradeneeded = function(event) {
                const db = event.target.result;
                if (!db.objectStoreNames.contains(STORE_NAME)) {
                  const objectStore = db.createObjectStore(STORE_NAME, { keyPath: 'fileName' });
                  objectStore.createIndex('savedAt', 'savedAt', { unique: false });
                  console.log('IndexedDB store created');
                }
              };
            });
          }
          
          // Save settings to IndexedDB
          window.saveSettings = function(fileName, settings) {
            initDB().then(function(db) {
              const transaction = db.transaction([STORE_NAME], 'readwrite');
              const store = transaction.objectStore(STORE_NAME);
              
              const data = {
                fileName: fileName,
                settings: settings,
                savedAt: new Date().toISOString()
              };
              
              const request = store.put(data);
              
              request.onsuccess = function() {
                console.log('Settings saved successfully for:', fileName);
                console.log('Saved data:', data);
              };
              
              request.onerror = function() {
                console.error('Error saving settings:', request.error);
              };
            }).catch(function(error) {
              console.error('Failed to initialize DB:', error);
              // Fallback to localStorage
              try {
                const STORAGE_KEY = 'synthetic_control_settings';
                let allSettings = JSON.parse(localStorage.getItem(STORAGE_KEY) || '{}');
                allSettings[fileName] = {
                  ...settings,
                  savedAt: new Date().toISOString()
                };
                localStorage.setItem(STORAGE_KEY, JSON.stringify(allSettings));
                console.log('Settings saved to localStorage (fallback) for:', fileName);
              } catch(e) {
                console.error('Error saving to localStorage:', e);
              }
            });
          };
          
          // Load settings from IndexedDB
          window.loadSettings = function(fileName) {
            return new Promise((resolve) => {
              initDB().then(function(db) {
                const transaction = db.transaction([STORE_NAME], 'readonly');
                const store = transaction.objectStore(STORE_NAME);
                const request = store.get(fileName);
                
                request.onsuccess = function() {
                  if (request.result) {
                    console.log('Settings loaded from IndexedDB for:', fileName);
                    console.log('Loaded data:', request.result);
                    resolve(request.result.settings);
                  } else {
                    console.log('No settings found in IndexedDB for:', fileName);
                    // Try localStorage fallback
                    try {
                      const STORAGE_KEY = 'synthetic_control_settings';
                      const allSettings = JSON.parse(localStorage.getItem(STORAGE_KEY) || '{}');
                      if (allSettings[fileName]) {
                        console.log('Settings loaded from localStorage (fallback) for:', fileName);
                        resolve(allSettings[fileName]);
                      } else {
                        resolve(null);
                      }
                    } catch(e) {
                      console.error('Error loading from localStorage:', e);
                      resolve(null);
                    }
                  }
                };
                
                request.onerror = function() {
                  console.error('Error loading settings:', request.error);
                  resolve(null);
                };
              }).catch(function(error) {
                console.error('Failed to load from IndexedDB:', error);
                // Fallback to localStorage
                try {
                  const STORAGE_KEY = 'synthetic_control_settings';
                  const allSettings = JSON.parse(localStorage.getItem(STORAGE_KEY) || '{}');
                  if (allSettings[fileName]) {
                    console.log('Settings loaded from localStorage (fallback) for:', fileName);
                    resolve(allSettings[fileName]);
                  } else {
                    resolve(null);
                  }
                } catch(e) {
                  console.error('Error loading from localStorage:', e);
                  resolve(null);
                }
              });
            });
          };
          
          // Get all saved file names
          window.getSavedFileNames = function() {
            return new Promise((resolve) => {
              initDB().then(function(db) {
                const transaction = db.transaction([STORE_NAME], 'readonly');
                const store = transaction.objectStore(STORE_NAME);
                const request = store.getAll();
                
                request.onsuccess = function() {
                  const fileNames = request.result.map(item => item.fileName);
                  console.log('Saved file names:', fileNames);
                  resolve(fileNames);
                };
                
                request.onerror = function() {
                  console.error('Error getting file names:', request.error);
                  resolve([]);
                };
              }).catch(function(error) {
                console.error('Failed to get file names:', error);
                // Fallback to localStorage
                try {
                  const STORAGE_KEY = 'synthetic_control_settings';
                  const allSettings = JSON.parse(localStorage.getItem(STORAGE_KEY) || '{}');
                  resolve(Object.keys(allSettings));
                } catch(e) {
                  console.error('Error getting file names from localStorage:', e);
                  resolve([]);
                }
              });
            });
          };
          
          // Clear settings for a specific file
          window.clearSettings = function(fileName) {
            initDB().then(function(db) {
              const transaction = db.transaction([STORE_NAME], 'readwrite');
              const store = transaction.objectStore(STORE_NAME);
              const request = store.delete(fileName);
              
              request.onsuccess = function() {
                console.log('Settings cleared for:', fileName);
              };
              
              request.onerror = function() {
                console.error('Error clearing settings:', request.error);
              };
            }).catch(function(error) {
              console.error('Failed to clear settings:', error);
            });
          };
          
          // Listen for Shiny messages to save settings
          Shiny.addCustomMessageHandler('saveSettings', function(message) {
            if (message.fileName && message.settings) {
              console.log('Received saveSettings message for:', message.fileName);
              saveSettings(message.fileName, message.settings);
            }
          });
          
          // Listen for Shiny messages to load settings
          Shiny.addCustomMessageHandler('loadSettings', function(message) {
            if (message.fileName) {
              console.log('Received loadSettings message for:', message.fileName);
              loadSettings(message.fileName).then(function(settings) {
                if (settings) {
                  console.log('Sending restored settings to Shiny');
                  Shiny.setInputValue('restored_settings', settings, {priority: 'event'});
                } else {
                  console.log('No settings found to restore');
                }
              });
            }
          });
          
          // Auto-save settings when inputs change (debounced)
          let saveTimeout;
          $(document).on('change', '#unitVar, #timeVar, #outcomeVar, #treatedUnit, #treatmentYear, input[name=\"predictorVars\"]', function() {
            clearTimeout(saveTimeout);
            saveTimeout = setTimeout(function() {
              console.log('Auto-save triggered by input change');
              Shiny.setInputValue('auto_save_trigger', Math.random(), {priority: 'event'});
            }, 1000); // Wait 1 second after last change
          });
          
          // Listen for trigger to auto-save
          Shiny.addCustomMessageHandler('triggerAutoSave', function(message) {
            console.log('Auto-save triggered by message');
            Shiny.setInputValue('auto_save_trigger', Math.random(), {priority: 'event'});
          });
          
          // Initialize DB on page load
          initDB().then(function() {
            console.log('IndexedDB initialized and ready');
          }).catch(function(error) {
            console.error('Failed to initialize IndexedDB:', error);
          });
        })();
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
          conditionalPanel(
            condition = "output.settingsRestored",
            fluidRow(
              box(
                width = 12,
                div(class = "settings-restored",
                    icon("check-circle"), 
                    strong(" Settings Restored!"), 
                    " Your previous configuration for this file has been automatically loaded.")
              )
            )
          ),
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
                         choices = NULL, selected = NULL)
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
            # Regular Predictors (time-invariant or use pre-treatment mean)
            box(
              title = "Predictors (Pre-treatment Mean)", status = "primary", solidHeader = TRUE, width = 6,
              conditionalPanel(
                condition = "output.treatmentConfigured",
                p("Select variables to use as predictors. Uses pre-treatment mean for matching.",
                  style = "color: #6c757d; font-size: 14px;"),
                checkboxGroupInput("predictorVars", "Select Predictor Variables:",
                                 choices = NULL, selected = NULL),
                p("These variables will be averaged over the entire pre-treatment period.",
                  style = "color: #856404; font-size: 12px; font-style: italic;")
              )
            ),
            
            # Special Predictors Configuration
            box(
              title = "Special Predictors (Custom Time Windows)", status = "warning", solidHeader = TRUE, width = 6,
              conditionalPanel(
                condition = "output.treatmentConfigured",
                p("Optional: Configure predictors with specific time windows.",
                  style = "color: #6c757d; font-size: 14px;"),
                
                # Dynamic UI for special predictors
                uiOutput("specialPredictorsUI"),
                
                br(),
                fluidRow(
                  column(4,
                    actionButton("addPredictor", "Add", 
                               class = "btn-info btn-sm", icon = icon("plus"))
                  ),
                  column(4,
                    actionButton("removePredictor", "Remove", 
                               class = "btn-danger btn-sm", icon = icon("minus"))
                  ),
                  column(4,
                    actionButton("clearPredictors", "Clear", 
                               class = "btn-warning btn-sm", icon = icon("trash"))
                  )
                )
              )
            )
          ),
          
          fluidRow(
            box(
              title = "Predictor Summary", status = "info", solidHeader = TRUE, width = 12,
              collapsible = TRUE,
              verbatimTextOutput("predictorsSummary", placeholder = TRUE)
            )
          ),
          
          fluidRow(
            # Analysis Preview
            box(
              title = "Analysis Preview", status = "success", solidHeader = TRUE, width = 12,
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
    placebo_completed = FALSE,
    special_predictors = list(),  # List of special predictor configs
    predictor_count = 0,          # Counter for predictor IDs
    current_file_name = NULL,     # Current uploaded file name for localStorage
    settings_restored = FALSE,    # Flag to track if settings were restored
    pending_settings = NULL       # Settings waiting to be restored
  )

  # Helper function to save current settings
  saveCurrentSettings <- function() {
    if(is.null(values$current_file_name) || is.null(values$data)) {
      showNotification("Please upload a file first!", type = "warning", duration = 3)
      return(FALSE)
    }
    
    if(is.null(input$unitVar) || is.null(input$timeVar) || is.null(input$outcomeVar)) {
      showNotification("Please configure at least Unit, Time, and Outcome variables!", type = "warning", duration = 3)
      return(FALSE)
    }
    
    settings_to_save <- list(
      unitVar = input$unitVar,
      timeVar = input$timeVar,
      outcomeVar = input$outcomeVar,
      treatedUnit = input$treatedUnit,
      treatmentYear = input$treatmentYear,
      predictorVars = input$predictorVars,
      special_predictors = values$special_predictors
    )
    
    cat("Manual save triggered for file:", values$current_file_name, "\n")
    cat("Settings being saved:", paste(names(settings_to_save), collapse=", "), "\n")
    
    session$sendCustomMessage("saveSettings", list(
      fileName = values$current_file_name,
      settings = settings_to_save
    ))
    
    return(TRUE)
  }
  
  # Manual save button handler
  observeEvent(input$saveParamsBtn, {
    if(saveCurrentSettings()) {
      showNotification(
        HTML(paste0(
          icon("check-circle"), 
          " Parameters saved successfully for: <strong>", 
          values$current_file_name, 
          "</strong>"
        )), 
        type = "success", 
        duration = 4
      )
    }
  })

  # File upload handling
  observeEvent(input$file, {
    req(input$file)

    # Store file name for IndexedDB (use exact file name as key)
    values$current_file_name <- input$file$name
    values$settings_restored <- FALSE
    values$pending_settings <- NULL

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
    
    # Reset special predictors when new data is uploaded
    values$special_predictors <- list()
    values$predictor_count <- 0
    
    # Try to restore saved settings for this file (use exact file name)
    cat("Attempting to load settings for file:", input$file$name, "\n")
    session$sendCustomMessage("loadSettings", list(fileName = input$file$name))
  })
  
  # Handle restored settings from IndexedDB
  observeEvent(input$restored_settings, {
    req(input$restored_settings, values$data)
    settings <- input$restored_settings
    values$pending_settings <- settings
    
    # Restore variable mappings if they exist in current data
    if(!is.null(settings$unitVar) && settings$unitVar %in% names(values$data)) {
      updateSelectInput(session, "unitVar", selected = settings$unitVar)
    }
    if(!is.null(settings$timeVar) && settings$timeVar %in% names(values$data)) {
      updateSelectInput(session, "timeVar", selected = settings$timeVar)
    }
    if(!is.null(settings$outcomeVar) && settings$outcomeVar %in% names(values$data)) {
      updateSelectInput(session, "outcomeVar", selected = settings$outcomeVar)
    }
    
    # Restore predictor variables
    if(!is.null(settings$predictorVars) && length(settings$predictorVars) > 0) {
      valid_predictors <- intersect(settings$predictorVars, names(values$data))
      if(length(valid_predictors) > 0) {
        updateCheckboxGroupInput(session, "predictorVars", selected = valid_predictors)
      }
    }
  })
  
  # Restore treatment year after timeVar is set
  observeEvent(input$timeVar, {
    if(!is.null(values$pending_settings) && !is.null(values$pending_settings$treatmentYear)) {
      settings <- values$pending_settings
      if(!is.null(input$timeVar) && input$timeVar %in% names(values$data)) {
        years <- sort(unique(values$data[[input$timeVar]]))
        if(settings$treatmentYear >= min(years) && settings$treatmentYear <= max(years)) {
          updateNumericInput(session, "treatmentYear", value = settings$treatmentYear)
        }
        # If no special predictors to restore, mark as restored now
        if(is.null(settings$special_predictors) || length(settings$special_predictors) == 0) {
          values$pending_settings <- NULL
          values$settings_restored <- TRUE
          showNotification("Previous settings restored for this file!", type = "message", duration = 5)
        }
      }
    }
  }, priority = 1)
  
  # Restore treated unit after unitVar is set
  observeEvent(input$unitVar, {
    if(!is.null(values$pending_settings) && !is.null(values$pending_settings$treatedUnit)) {
      settings <- values$pending_settings
      if(!is.null(input$unitVar) && input$unitVar %in% names(values$data)) {
        units <- unique(values$data[[input$unitVar]])
        if(settings$treatedUnit %in% units) {
          updateSelectInput(session, "treatedUnit", selected = settings$treatedUnit)
        }
      }
    }
  }, priority = 1)
  
  # Restore special predictors after timeVar is set
  observeEvent(input$timeVar, {
    if(!is.null(values$pending_settings)) {
      settings <- values$pending_settings
      if(!is.null(input$timeVar) && input$timeVar %in% names(values$data)) {
        # Restore special predictors if they exist
        if(!is.null(settings$special_predictors) && length(settings$special_predictors) > 0) {
          valid_predictors <- list()
          years <- sort(unique(values$data[[input$timeVar]]))
          for(pred in settings$special_predictors) {
            if(!is.null(pred$var) && pred$var %in% names(values$data)) {
              if(!is.null(pred$start) && !is.null(pred$end) && 
                 pred$start >= min(years) && pred$end <= max(years)) {
                values$predictor_count <- values$predictor_count + 1
                valid_predictors <- c(valid_predictors, list(list(
                  id = values$predictor_count,
                  var = pred$var,
                  start = pred$start,
                  end = pred$end,
                  op = if(!is.null(pred$op)) pred$op else "mean"
                )))
              }
            }
          }
          if(length(valid_predictors) > 0) {
            values$special_predictors <- valid_predictors
          }
        }
        # Clear pending settings after restoration
        values$pending_settings <- NULL
        values$settings_restored <- TRUE
        showNotification("Previous settings restored for this file!", type = "message", duration = 5)
      }
    }
  }, priority = 0)
  
  # Auto-save settings when they change (triggered by JS debounced events)
  observeEvent(input$auto_save_trigger, {
    req(values$current_file_name, values$data)
    
    # Only save if we have the minimum required settings
    # Save even if not restored yet (user might be configuring for first time)
    if(!is.null(input$unitVar) && !is.null(input$timeVar) && !is.null(input$outcomeVar)) {
      settings_to_save <- list(
        unitVar = input$unitVar,
        timeVar = input$timeVar,
        outcomeVar = input$outcomeVar,
        treatedUnit = input$treatedUnit,
        treatmentYear = input$treatmentYear,
        predictorVars = input$predictorVars,
        special_predictors = values$special_predictors
      )
      
      cat("Auto-saving settings for file:", values$current_file_name, "\n")
      cat("Settings:", paste(names(settings_to_save), collapse=", "), "\n")
      
      session$sendCustomMessage("saveSettings", list(
        fileName = values$current_file_name,
        settings = settings_to_save
      ))
    } else {
      cat("Cannot save: missing required variables\n")
    }
  }, ignoreInit = TRUE)
  
  # Also save immediately when key settings are complete (not just on auto-save trigger)
  observeEvent(c(input$unitVar, input$timeVar, input$outcomeVar, input$treatedUnit, 
                 input$treatmentYear), {
    if(!is.null(values$current_file_name) && values$settings_restored && 
       !is.null(input$unitVar) && !is.null(input$timeVar) && !is.null(input$outcomeVar)) {
      # Small delay to batch multiple changes
      invalidateLater(500, session)
      observe({
        if(!is.null(input$unitVar) && !is.null(input$timeVar) && !is.null(input$outcomeVar)) {
          settings_to_save <- list(
            unitVar = input$unitVar,
            timeVar = input$timeVar,
            outcomeVar = input$outcomeVar,
            treatedUnit = input$treatedUnit,
            treatmentYear = input$treatmentYear,
            predictorVars = input$predictorVars,
            special_predictors = values$special_predictors
          )
          
          cat("Saving settings immediately for file:", values$current_file_name, "\n")
          session$sendCustomMessage("saveSettings", list(
            fileName = values$current_file_name,
            settings = settings_to_save
          ))
        }
      })
    }
  }, ignoreInit = TRUE)
  
  # Save settings when special predictors change
  observeEvent(values$special_predictors, {
    if(!is.null(values$current_file_name) && values$settings_restored && 
       !is.null(input$unitVar) && !is.null(input$timeVar) && !is.null(input$outcomeVar)) {
      invalidateLater(500, session)
      observe({
        settings_to_save <- list(
          unitVar = input$unitVar,
          timeVar = input$timeVar,
          outcomeVar = input$outcomeVar,
          treatedUnit = input$treatedUnit,
          treatmentYear = input$treatmentYear,
          predictorVars = input$predictorVars,
          special_predictors = values$special_predictors
        )
        
        cat("Saving settings (special predictors changed) for file:", values$current_file_name, "\n")
        session$sendCustomMessage("saveSettings", list(
          fileName = values$current_file_name,
          settings = settings_to_save
        ))
      })
    }
  }, ignoreInit = TRUE)
  
  # Save settings when predictor vars change
  observeEvent(input$predictorVars, {
    if(!is.null(values$current_file_name) && values$settings_restored && 
       !is.null(input$unitVar) && !is.null(input$timeVar) && !is.null(input$outcomeVar)) {
      invalidateLater(500, session)
      observe({
        settings_to_save <- list(
          unitVar = input$unitVar,
          timeVar = input$timeVar,
          outcomeVar = input$outcomeVar,
          treatedUnit = input$treatedUnit,
          treatmentYear = input$treatmentYear,
          predictorVars = input$predictorVars,
          special_predictors = values$special_predictors
        )
        
        cat("Saving settings (predictor vars changed) for file:", values$current_file_name, "\n")
        session$sendCustomMessage("saveSettings", list(
          fileName = values$current_file_name,
          settings = settings_to_save
        ))
      })
    }
  }, ignoreInit = TRUE)

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
  # Note: special predictors are optional - if none configured, uses outcome at each pre-treatment period
  output$readyForAnalysis <- reactive({
    mapped <- !is.null(input$unitVar) && !is.null(input$timeVar) && !is.null(input$outcomeVar)
    treatment <- !is.null(input$treatedUnit) && !is.null(input$treatmentYear) &&
                 input$treatedUnit != "" && !is.na(input$treatmentYear)
    return(mapped && treatment)
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
  
  # Check if settings were restored
  output$settingsRestored <- reactive({
    return(values$settings_restored)
  })
  outputOptions(output, "settingsRestored", suspendWhenHidden = FALSE)

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

  # ============================================
  # Special Predictors Dynamic UI and Handlers
  # ============================================
  
  # Dynamic UI for special predictors
  output$specialPredictorsUI <- renderUI({
    req(values$data, input$timeVar, input$treatmentYear)
    
    # Get available variables (exclude unit and time vars)
    all_vars <- names(values$data)
    predictor_vars <- setdiff(all_vars, c(input$unitVar, input$timeVar))
    
    # Get pre-treatment years for the sliders
    all_years <- sort(unique(values$data[[input$timeVar]]))
    pre_years <- all_years[all_years < input$treatmentYear]
    
    if(length(pre_years) == 0) {
      return(div(class = "alert alert-warning", "No pre-treatment periods available"))
    }
    
    min_year <- min(pre_years)
    max_year <- max(pre_years)
    
    # Generate UI for each predictor in the list
    if(length(values$special_predictors) == 0) {
      return(div(class = "alert alert-info", style = "font-size: 13px;",
                 icon("info-circle"), 
                 " No special predictors configured. Click 'Add Predictor' or leave empty to use outcome at each pre-treatment period."))
    }
    
    predictor_uis <- lapply(seq_along(values$special_predictors), function(i) {
      pred <- values$special_predictors[[i]]
      
      fluidRow(
        column(4,
          selectInput(
            inputId = paste0("pred_var_", i),
            label = paste("Variable", i),
            choices = predictor_vars,
            selected = pred$var
          )
        ),
        column(3,
          numericInput(
            inputId = paste0("pred_start_", i),
            label = "Start Year",
            value = pred$start,
            min = min_year,
            max = max_year
          )
        ),
        column(3,
          numericInput(
            inputId = paste0("pred_end_", i),
            label = "End Year",
            value = pred$end,
            min = min_year,
            max = max_year
          )
        ),
        column(2,
          div(style = "margin-top: 25px;",
            tags$span(class = "badge bg-primary", paste("mean"))
          )
        )
      )
    })
    
    do.call(tagList, predictor_uis)
  })
  
  # Add predictor button
  observeEvent(input$addPredictor, {
    req(values$data, input$timeVar, input$treatmentYear)
    
    # Get defaults
    all_vars <- names(values$data)
    predictor_vars <- setdiff(all_vars, c(input$unitVar, input$timeVar))
    
    all_years <- sort(unique(values$data[[input$timeVar]]))
    pre_years <- all_years[all_years < input$treatmentYear]
    
    if(length(pre_years) == 0 || length(predictor_vars) == 0) return()
    
    values$predictor_count <- values$predictor_count + 1
    
    # Add new predictor with defaults
    new_pred <- list(
      id = values$predictor_count,
      var = predictor_vars[1],
      start = min(pre_years),
      end = max(pre_years),
      op = "mean"
    )
    
    values$special_predictors <- c(values$special_predictors, list(new_pred))
  })
  
  # Remove last predictor button
  observeEvent(input$removePredictor, {
    if(length(values$special_predictors) > 0) {
      values$special_predictors <- values$special_predictors[-length(values$special_predictors)]
    }
  })
  
  # Clear all predictors button
  observeEvent(input$clearPredictors, {
    values$special_predictors <- list()
    values$predictor_count <- 0
  })
  
  # Update special predictors when inputs change
  observe({
    req(values$data)
    
    if(length(values$special_predictors) > 0) {
      for(i in seq_along(values$special_predictors)) {
        var_input <- input[[paste0("pred_var_", i)]]
        start_input <- input[[paste0("pred_start_", i)]]
        end_input <- input[[paste0("pred_end_", i)]]
        
        if(!is.null(var_input)) values$special_predictors[[i]]$var <- var_input
        if(!is.null(start_input)) values$special_predictors[[i]]$start <- start_input
        if(!is.null(end_input)) values$special_predictors[[i]]$end <- end_input
      }
    }
  })
  
  # Predictors summary display
  output$predictorsSummary <- renderText({
    lines <- c()
    
    # Regular predictors
    if(!is.null(input$predictorVars) && length(input$predictorVars) > 0) {
      lines <- c(lines, "Regular Predictors (pre-treatment mean):")
      for(v in input$predictorVars) {
        lines <- c(lines, paste0("  • ", v))
      }
    }
    
    # Special predictors
    if(length(values$special_predictors) > 0) {
      if(length(lines) > 0) lines <- c(lines, "")
      lines <- c(lines, "Special Predictors (custom time windows):")
      for(pred in values$special_predictors) {
        lines <- c(lines, paste0("  • ", pred$var, " [", pred$start, "-", pred$end, "] (mean)"))
      }
    }
    
    # Default message if nothing configured
    if(length(lines) == 0) {
      lines <- c("No predictors configured.",
                 "Will use outcome variable at each pre-treatment period as default.")
    }
    
    paste(lines, collapse = "\n")
  })
  
  # ============================================
  # Analysis Summary
  # ============================================
  
  # Analysis summary
  output$analysisSummary <- renderText({
    req(input$unitVar, input$timeVar, input$outcomeVar, input$treatedUnit, input$treatmentYear)

    # Describe predictors
    predictor_desc <- c()
    
    # Regular predictors
    if(!is.null(input$predictorVars) && length(input$predictorVars) > 0) {
      predictor_desc <- c(predictor_desc, paste(input$predictorVars, collapse = ", "))
    }
    
    # Special predictors
    if(length(values$special_predictors) > 0) {
      special_summaries <- sapply(values$special_predictors, function(pred) {
        paste0(pred$var, "[", pred$start, "-", pred$end, "]")
      })
      predictor_desc <- c(predictor_desc, paste(special_summaries, collapse = ", "))
    }
    
    predictors <- if(length(predictor_desc) == 0) {
      "Default: outcome at each pre-treatment period"
    } else {
      paste(predictor_desc, collapse = "; ")
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
      showNotification("Running synthetic control analysis using Synth package...", 
                       type = "message", duration = NULL, id = "analysis_progress")

      # Build predictor configs from UI
      # Regular predictors (pre-treatment mean)
      predictor_vars <- input$predictorVars
      if(is.null(predictor_vars) || length(predictor_vars) == 0) {
        predictor_vars <- NULL
      }
      
      # Special predictors (custom time windows)
      special_predictors_config <- NULL
      if(length(values$special_predictors) > 0) {
        special_predictors_config <- values$special_predictors
      }

      # Run analysis using Synth package (dataprep + synth)
      values$analysis_results <- run_synth_analysis(
        data = values$data,
        unit_var = input$unitVar,
        time_var = input$timeVar,
        outcome_var = input$outcomeVar,
        treated_unit = input$treatedUnit,
        treatment_year = input$treatmentYear,
        predictor_vars = predictor_vars,
        special_predictors_config = special_predictors_config
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

    # Reset state
    values$placebo_results <- NULL
    values$placebo_completed <- FALSE

    tryCatch({
      # Count total units for progress
      all_units <- unique(values$data[[input$unitVar]])
      total_units <- length(all_units)
      
      showNotification(
        HTML(paste0(
          icon("spinner", class = "fa-spin"), 
          " Running placebo tests (0/", total_units, " units)..."
        )), 
        type = "message", 
        duration = NULL, 
        id = "placebo_progress"
      )

      # Use same predictor config as main analysis
      predictor_vars <- input$predictorVars
      if(is.null(predictor_vars) || length(predictor_vars) == 0) {
        predictor_vars <- NULL
      }
      
      special_predictors_config <- NULL
      if(length(values$special_predictors) > 0) {
        special_predictors_config <- values$special_predictors
      }

      # Run placebo tests with progress updates
      cat("Starting placebo tests for", total_units, "units...\n")
      
      # Run placebo tests using Synth package
      values$placebo_results <- run_synth_placebo(
        data = values$data,
        unit_var = input$unitVar,
        time_var = input$timeVar,
        outcome_var = input$outcomeVar,
        treated_unit = input$treatedUnit,
        treatment_year = input$treatmentYear,
        predictor_vars = predictor_vars,
        special_predictors_config = special_predictors_config
      )

      # Check if we got results
      if(is.null(values$placebo_results)) {
        stop("Placebo tests returned NULL - no results generated")
      }
      
      if(is.null(values$placebo_results$placebo_gaps) || 
         nrow(values$placebo_results$placebo_gaps) == 0) {
        stop("Placebo tests completed but no gap data was generated. Check that units have sufficient data.")
      }

      values$placebo_completed <- TRUE
      
      successful_units <- if(!is.null(values$placebo_results$placebo_gaps)) {
        length(unique(values$placebo_results$placebo_gaps$unit))
      } else {
        0
      }

      removeNotification("placebo_progress")
      showNotification(
        HTML(paste0(
          icon("check-circle"), 
          " Placebo tests completed! (", successful_units, "/", total_units, " units successful)"
        )), 
        type = "success", 
        duration = 5
      )
      
      cat("Placebo tests completed successfully:", successful_units, "units\n")

    }, error = function(e) {
      removeNotification("placebo_progress")
      error_msg <- paste("Placebo tests failed:", e$message)
      cat("ERROR:", error_msg, "\n")
      showNotification(
        HTML(paste0(icon("exclamation-triangle"), " ", error_msg)), 
        type = "error", 
        duration = 8
      )
      values$placebo_completed <- FALSE
      values$placebo_results <- NULL
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
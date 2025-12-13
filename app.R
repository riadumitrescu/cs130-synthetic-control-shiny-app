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
library(rmarkdown)  # For PDF report generation

# Source helper functions
source("functions/data_functions.R")
source("functions/synth_wrapper.R")
source("functions/plotting_functions.R")
source("functions/placebo_tests.R")

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
      menuItem("5. Export", tabName = "export", icon = icon("download")),
      menuItem("6. Analysis History", tabName = "history", icon = icon("history"))
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
          const DB_VERSION = 2;
          const STORE_NAME = 'settings';
          const ANALYSIS_STORE_NAME = 'analyses';
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

                // Settings store
                if (!db.objectStoreNames.contains(STORE_NAME)) {
                  const objectStore = db.createObjectStore(STORE_NAME, { keyPath: 'fileName' });
                  objectStore.createIndex('savedAt', 'savedAt', { unique: false });
                  console.log('IndexedDB settings store created');
                }

                // Analysis results store
                if (!db.objectStoreNames.contains(ANALYSIS_STORE_NAME)) {
                  const analysisStore = db.createObjectStore(ANALYSIS_STORE_NAME, { keyPath: 'analysisId' });
                  analysisStore.createIndex('savedAt', 'savedAt', { unique: false });
                  analysisStore.createIndex('fileName', 'fileName', { unique: false });
                  analysisStore.createIndex('treatedUnit', 'treatedUnit', { unique: false });
                  console.log('IndexedDB analyses store created');
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

          // === ANALYSIS RESULTS FUNCTIONS ===

          // Save analysis results to IndexedDB
          window.saveAnalysisResults = function(analysisData) {
            initDB().then(function(db) {
              const transaction = db.transaction([ANALYSIS_STORE_NAME], 'readwrite');
              const store = transaction.objectStore(ANALYSIS_STORE_NAME);

              const data = {
                analysisId: analysisData.analysisId,
                fileName: analysisData.fileName,
                treatedUnit: analysisData.treatedUnit,
                treatmentYear: analysisData.treatmentYear,
                analysisResults: analysisData.analysisResults,
                analysisConfig: analysisData.analysisConfig,
                savedAt: new Date().toISOString(),
                rmspe: analysisData.rmspe,
                converged: analysisData.converged
              };

              const request = store.put(data);

              request.onsuccess = function() {
                console.log('Analysis results saved successfully:', data.analysisId);
              };

              request.onerror = function() {
                console.error('Error saving analysis results:', request.error);
              };
            }).catch(function(error) {
              console.error('Failed to save analysis results:', error);
            });
          };

          // Get all saved analyses
          window.getAllAnalyses = function() {
            return new Promise((resolve) => {
              initDB().then(function(db) {
                const transaction = db.transaction([ANALYSIS_STORE_NAME], 'readonly');
                const store = transaction.objectStore(ANALYSIS_STORE_NAME);
                const request = store.getAll();

                request.onsuccess = function() {
                  const analyses = request.result.map(item => ({
                    analysisId: item.analysisId,
                    fileName: item.fileName,
                    treatedUnit: item.treatedUnit,
                    treatmentYear: item.treatmentYear,
                    savedAt: item.savedAt,
                    rmspe: item.rmspe,
                    converged: item.converged
                  }));
                  console.log('Retrieved analyses:', analyses.length);
                  resolve(analyses);
                };

                request.onerror = function() {
                  console.error('Error getting analyses:', request.error);
                  resolve([]);
                };
              }).catch(function(error) {
                console.error('Failed to get analyses:', error);
                resolve([]);
              });
            });
          };

          // Load specific analysis results
          window.loadAnalysisResults = function(analysisId) {
            return new Promise((resolve) => {
              initDB().then(function(db) {
                const transaction = db.transaction([ANALYSIS_STORE_NAME], 'readonly');
                const store = transaction.objectStore(ANALYSIS_STORE_NAME);
                const request = store.get(analysisId);

                request.onsuccess = function() {
                  if (request.result) {
                    console.log('Analysis loaded:', analysisId);
                    resolve(request.result);
                  } else {
                    console.log('Analysis not found:', analysisId);
                    resolve(null);
                  }
                };

                request.onerror = function() {
                  console.error('Error loading analysis:', request.error);
                  resolve(null);
                };
              }).catch(function(error) {
                console.error('Failed to load analysis:', error);
                resolve(null);
              });
            });
          };

          // Delete analysis
          window.deleteAnalysis = function(analysisId) {
            initDB().then(function(db) {
              const transaction = db.transaction([ANALYSIS_STORE_NAME], 'readwrite');
              const store = transaction.objectStore(ANALYSIS_STORE_NAME);
              const request = store.delete(analysisId);

              request.onsuccess = function() {
                console.log('Analysis deleted:', analysisId);
              };

              request.onerror = function() {
                console.error('Error deleting analysis:', request.error);
              };
            }).catch(function(error) {
              console.error('Failed to delete analysis:', error);
            });
          };

          // Clear all analyses
          window.clearAllAnalyses = function() {
            initDB().then(function(db) {
              const transaction = db.transaction([ANALYSIS_STORE_NAME], 'readwrite');
              const store = transaction.objectStore(ANALYSIS_STORE_NAME);
              const request = store.clear();

              request.onsuccess = function() {
                console.log('All analyses cleared');
              };

              request.onerror = function() {
                console.error('Error clearing all analyses:', request.error);
              };
            }).catch(function(error) {
              console.error('Failed to clear all analyses:', error);
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

          // === ANALYSIS RESULTS MESSAGE HANDLERS ===

          // Listen for save analysis results
          Shiny.addCustomMessageHandler('saveAnalysisResults', function(message) {
            if (message.analysisData) {
              console.log('Received saveAnalysisResults message');
              saveAnalysisResults(message.analysisData);
            }
          });

          // Listen for get all analyses
          Shiny.addCustomMessageHandler('getAllAnalyses', function(message) {
            console.log('Received getAllAnalyses message');
            getAllAnalyses().then(function(analyses) {
              Shiny.setInputValue('analysis_history_data', analyses, {priority: 'event'});
            });
          });

          // Listen for load analysis
          Shiny.addCustomMessageHandler('loadAnalysisResults', function(message) {
            if (message.analysisId) {
              console.log('Received loadAnalysisResults message for:', message.analysisId);
              loadAnalysisResults(message.analysisId).then(function(analysis) {
                if (analysis) {
                  Shiny.setInputValue('loaded_analysis_data', analysis, {priority: 'event'});
                } else {
                  console.log('Analysis not found');
                }
              });
            }
          });

          // Listen for delete analysis
          Shiny.addCustomMessageHandler('deleteAnalysis', function(message) {
            if (message.analysisId) {
              console.log('Received deleteAnalysis message for:', message.analysisId);
              deleteAnalysis(message.analysisId);
              // Refresh the list
              setTimeout(function() {
                getAllAnalyses().then(function(analyses) {
                  Shiny.setInputValue('analysis_history_data', analyses, {priority: 'event'});
                });
              }, 100);
            }
          });

          // Listen for clear all analyses
          Shiny.addCustomMessageHandler('clearAllAnalyses', function(message) {
            console.log('Received clearAllAnalyses message');
            clearAllAnalyses();
            // Refresh the list
            setTimeout(function() {
              Shiny.setInputValue('analysis_history_data', [], {priority: 'event'});
            }, 100);
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
            # In-Space Placebo Tests
            box(
              title = "In-Space Placebo Tests", status = "primary", solidHeader = TRUE, width = 6,
              p("Tests whether the treatment effect is unusually large compared to control units."),
              p(strong("Method:"), "Run synthetic control on each donor unit as if it received treatment at the same time."),
              br(),
              actionButton("runInSpacePlacebo", "Run In-Space Placebo",
                          class = "btn-warning", icon = icon("users"),
                          style = "width: 100%; margin-bottom: 10px;"),
              conditionalPanel(
                condition = "output.inSpacePlaceboCompleted",
                div(class = "alert alert-success",
                    icon("check"), " In-space placebo tests completed!")
              )
            ),

            # In-Time Placebo Tests
            box(
              title = "In-Time Placebo Tests", status = "info", solidHeader = TRUE, width = 6,
              p("Tests whether similar effects occur at fake treatment dates."),
              p(strong("Method:"), "Pretend treatment happened earlier, when no intervention actually occurred."),

              numericInput("numFakeTimes", "Number of Fake Treatment Times:",
                          value = 3, min = 1, max = 10, step = 1),
              p("Fake times will be evenly spaced before the real treatment.",
                style = "font-size: 12px; color: #6c757d;"),
              br(),
              actionButton("runInTimePlacebo", "Run In-Time Placebo",
                          class = "btn-info", icon = icon("clock"),
                          style = "width: 100%; margin-bottom: 10px;"),
              conditionalPanel(
                condition = "output.inTimePlaceboCompleted",
                div(class = "alert alert-success",
                    icon("check"), " In-time placebo tests completed!")
              )
            )
          ),

          # In-Space Results
          conditionalPanel(
            condition = "output.inSpacePlaceboCompleted",
            fluidRow(
              box(
                title = "In-Space Results", status = "primary", solidHeader = TRUE, width = 12,
                fluidRow(
                  column(4,
                    valueBoxOutput("inSpacePValueBox", width = 12)
                  ),
                  column(8,
                    plotOutput("inSpacePlaceboPlot", height = "350px")
                  )
                ),
                br(),
                DT::dataTableOutput("inSpacePlaceboTable")
              )
            )
          ),

          # In-Time Results
          conditionalPanel(
            condition = "output.inTimePlaceboCompleted",
            fluidRow(
              box(
                title = "In-Time Results", status = "info", solidHeader = TRUE, width = 12,
                fluidRow(
                  column(6,
                    plotOutput("inTimePlaceboPlot", height = "350px")
                  ),
                  column(6,
                    DT::dataTableOutput("inTimePlaceboTable")
                  )
                )
              )
            )
          ),

          # Legacy Placebo (Original Implementation)
          fluidRow(
            box(
              title = "Legacy Placebo Test", status = "warning", solidHeader = TRUE, width = 12,
              collapsible = TRUE, collapsed = TRUE,
              p("Original placebo implementation (same as in-space but different interface)."),
              actionButton("runPlacebo", "Run Legacy Placebo Tests",
                          class = "btn-warning btn-sm", icon = icon("flask")),
              conditionalPanel(
                condition = "output.placeboCompleted",
                hr(),
                valueBoxOutput("placeboRankBox", width = 3),
                plotOutput("placeboPlot", height = "300px"),
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

              fluidRow(
                column(6,
                  actionButton("previewReport", "Preview Report",
                             class = "btn-info btn-lg", style = "width: 100%; margin-bottom: 10px;",
                             icon = icon("eye"))
                ),
                column(6,
                  downloadButton("downloadReport", "Generate PDF Report",
                               class = "btn-success btn-lg", style = "width: 100%; margin-bottom: 10px;",
                               icon = icon("file-pdf"))
                )
              ),

              conditionalPanel(
                condition = "output.reportPreviewAvailable",
                hr(),
                h4("Report Preview", style = "color: #2c3e50;"),
                div(style = "max-height: 400px; overflow-y: auto; border: 1px solid #ddd; padding: 15px; background-color: #f8f9fa;",
                    verbatimTextOutput("reportPreview")
                )
              ),

              br(),
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
      ),

      # Analysis History Tab
      tabItem(tabName = "history",
        fluidRow(
          box(
            title = "Analysis History", status = "primary", solidHeader = TRUE, width = 12,
            p("View and manage your saved analysis results. Each analysis is automatically saved when completed."),

            fluidRow(
              column(8,
                actionButton("refreshHistory", "Refresh History",
                           class = "btn-info", icon = icon("refresh"))
              ),
              column(4,
                actionButton("clearAllHistory", "Clear All History",
                           class = "btn-danger", icon = icon("trash"),
                           onclick = "return confirm('Are you sure you want to delete all saved analyses? This cannot be undone.');")
              )
            ),
            br(),

            conditionalPanel(
              condition = "output.hasHistoryData",
              DT::dataTableOutput("historyTable"),
              br(),

              conditionalPanel(
                condition = "input.historyTable_rows_selected.length > 0",
                fluidRow(
                  box(
                    title = "Selected Analysis Details", status = "info", solidHeader = TRUE, width = 12,
                    fluidRow(
                      column(4,
                        actionButton("loadSelectedAnalysis", "Load Analysis",
                                   class = "btn-success btn-lg", style = "width: 100%;",
                                   icon = icon("upload"))
                      ),
                      column(4,
                        actionButton("viewSelectedSummary", "View Summary",
                                   class = "btn-info btn-lg", style = "width: 100%;",
                                   icon = icon("eye"))
                      ),
                      column(4,
                        actionButton("deleteSelected", "Delete Selected",
                                   class = "btn-danger btn-lg", style = "width: 100%;",
                                   icon = icon("trash"))
                      )
                    ),

                    conditionalPanel(
                      condition = "output.showAnalysisSummary",
                      hr(),
                      h4("Analysis Summary"),
                      verbatimTextOutput("selectedAnalysisSummary")
                    )
                  )
                )
              )
            ),

            conditionalPanel(
              condition = "!output.hasHistoryData",
              div(class = "alert alert-info", style = "text-align: center; margin: 40px;",
                  icon("info-circle"),
                  " No saved analyses found. Complete an analysis to see it here automatically.")
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
    in_space_placebo_results = NULL,
    in_space_placebo_completed = FALSE,
    in_time_placebo_results = NULL,
    in_time_placebo_completed = FALSE,
    special_predictors = list(),  # List of special predictor configs
    predictor_count = 0,          # Counter for predictor IDs
    current_file_name = NULL,     # Current uploaded file name for localStorage
    settings_restored = FALSE,    # Flag to track if settings were restored
    pending_settings = NULL,      # Settings waiting to be restored
    report_preview = NULL,        # Preview content for the report
    report_preview_available = FALSE,  # Flag for preview availability
    analysis_history = NULL,      # List of saved analyses
    selected_history_analysis = NULL,  # Currently selected analysis from history
    show_analysis_summary = FALSE  # Flag for showing analysis summary
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
        type = "message", 
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

  # Check if in-space placebo tests are completed
  output$inSpacePlaceboCompleted <- reactive({
    return(values$in_space_placebo_completed)
  })
  outputOptions(output, "inSpacePlaceboCompleted", suspendWhenHidden = FALSE)

  # Check if in-time placebo tests are completed
  output$inTimePlaceboCompleted <- reactive({
    return(values$in_time_placebo_completed)
  })
  outputOptions(output, "inTimePlaceboCompleted", suspendWhenHidden = FALSE)
  
  # Check if settings were restored
  output$settingsRestored <- reactive({
    return(values$settings_restored)
  })
  outputOptions(output, "settingsRestored", suspendWhenHidden = FALSE)

  # Check if report preview is available
  output$reportPreviewAvailable <- reactive({
    return(values$report_preview_available)
  })
  outputOptions(output, "reportPreviewAvailable", suspendWhenHidden = FALSE)

  # Check if analysis history data is available
  output$hasHistoryData <- reactive({
    return(!is.null(values$analysis_history) && length(values$analysis_history) > 0)
  })
  outputOptions(output, "hasHistoryData", suspendWhenHidden = FALSE)

  # Check if analysis summary should be shown
  output$showAnalysisSummary <- reactive({
    return(values$show_analysis_summary)
  })
  outputOptions(output, "showAnalysisSummary", suspendWhenHidden = FALSE)

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

      # Save analysis results to IndexedDB
      tryCatch({
        analysis_id <- paste0(
          gsub("[^A-Za-z0-9]", "_", input$treatedUnit), "_",
          input$treatmentYear, "_",
          format(Sys.time(), "%Y%m%d_%H%M%S")
        )

        analysis_data <- list(
          analysisId = analysis_id,
          fileName = if(!is.null(values$current_file_name)) values$current_file_name else "unknown_file",
          treatedUnit = input$treatedUnit,
          treatmentYear = input$treatmentYear,
          analysisResults = values$analysis_results,
          analysisConfig = list(
            unitVar = input$unitVar,
            timeVar = input$timeVar,
            outcomeVar = input$outcomeVar,
            predictorVars = input$predictorVars,
            special_predictors = values$special_predictors
          ),
          rmspe = values$analysis_results$rmspe,
          converged = values$analysis_results$converged
        )

        session$sendCustomMessage("saveAnalysisResults", list(analysisData = analysis_data))
      }, error = function(e) {
        cat("Error saving analysis results:", e$message, "\n")
      })

      # Remove progress notification and show success
      removeNotification("analysis_progress")
      showNotification("Analysis completed and saved successfully!", type = "message", duration = 3)

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
        type = "message", 
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

  # Report preview handler
  observeEvent(input$previewReport, {
    req(values$analysis_results)

    tryCatch({
      showNotification("Generating report preview...", type = "message", duration = 2, id = "preview_progress")

      # Safely extract values
      treated_unit <- as.character(values$analysis_results$treated_unit)
      treatment_year <- as.character(values$analysis_results$treatment_year)
      num_donors <- length(values$analysis_results$donor_units)
      rmspe <- as.numeric(values$analysis_results$rmspe)
      converged <- ifelse(values$analysis_results$converged, "Yes", "No")

      # Calculate metrics
      post_treatment_data <- values$analysis_results$outcome_path[values$analysis_results$outcome_path$post_treatment, ]
      avg_treatment_effect <- mean(post_treatment_data$gap, na.rm = TRUE)

      pre_treatment_data <- values$analysis_results$outcome_path[!values$analysis_results$outcome_path$post_treatment, ]
      treated_mean <- mean(pre_treatment_data$treated_outcome, na.rm = TRUE)
      synthetic_mean <- mean(pre_treatment_data$synthetic_outcome, na.rm = TRUE)

      # Create weights summary
      weights_summary <- data.frame(
        Unit = names(values$analysis_results$weights),
        Weight = as.numeric(values$analysis_results$weights)
      ) %>%
        filter(Weight > 0) %>%
        arrange(desc(Weight)) %>%
        head(5)  # Top 5 weights

      weights_text <- paste(sprintf("%s: %.3f", weights_summary$Unit, weights_summary$Weight), collapse = ", ")

      # Statistical inference
      stat_inference <- if(values$placebo_completed) {
        sprintf("Placebo test p-value: %.3f", values$placebo_results$ranking)
      } else {
        "Run placebo tests for statistical inference"
      }

      # Create preview text
      preview_text <- sprintf(
"SYNTHETIC CONTROL ANALYSIS REPORT

EXECUTIVE SUMMARY
=================
Analysis of intervention on %s starting in %s using synthetic control method.

ANALYSIS CONFIGURATION
======================
• Treated Unit: %s
• Treatment Year: %s
• Number of Donor Units: %d
• Pre-treatment RMSPE: %.4f
• Analysis Converged: %s

KEY RESULTS
===========
• Average Post-treatment Effect: %.3f
• Pre-treatment Fit (Treated): %.3f
• Pre-treatment Fit (Synthetic): %.3f

TOP DONOR WEIGHTS
=================
%s

STATISTICAL INFERENCE
====================
%s

INTERPRETATION
==============
- Pre-treatment RMSPE of %.4f indicates the quality of fit
- The synthetic control method provides a data-driven counterfactual
- Treatment effect represents the difference between actual and synthetic outcomes

Note: This is a text preview. The full PDF report includes:
• High-resolution plots showing actual vs synthetic outcomes
• Treatment effect visualization over time
• Donor unit weights bar chart
• Placebo test results (if available)
• Detailed tables and technical notes

Generated on %s",
        treated_unit, treatment_year,
        treated_unit, treatment_year, num_donors, rmspe, converged,
        avg_treatment_effect, treated_mean, synthetic_mean,
        weights_text,
        stat_inference,
        rmspe,
        Sys.Date()
      )

      values$report_preview <- preview_text
      values$report_preview_available <- TRUE

      removeNotification("preview_progress")
      showNotification("Report preview ready!", type = "message", duration = 3)

    }, error = function(e) {
      removeNotification("preview_progress")
      values$report_preview <- paste("Error generating preview:", e$message)
      values$report_preview_available <- TRUE
      showNotification("Error generating preview", type = "error", duration = 3)
    })
  })

  # Report preview output
  output$reportPreview <- renderText({
    req(values$report_preview)
    values$report_preview
  })

  # ============================================
  # ANALYSIS HISTORY HANDLERS
  # ============================================

  # Handle analysis history data from IndexedDB
  observeEvent(input$analysis_history_data, {
    req(input$analysis_history_data)
    values$analysis_history <- input$analysis_history_data
  })

  # Refresh history button
  observeEvent(input$refreshHistory, {
    session$sendCustomMessage("getAllAnalyses", list())
    showNotification("Refreshing analysis history...", type = "message", duration = 2)
  })

  # Clear all history button
  observeEvent(input$clearAllHistory, {
    session$sendCustomMessage("clearAllAnalyses", list())
    values$analysis_history <- NULL
    showNotification("All analysis history cleared!", type = "warning", duration = 3)
  })

  # History table
  output$historyTable <- DT::renderDataTable({
    req(values$analysis_history)

    # Format the data for display
    history_df <- data.frame(
      `Analysis ID` = sapply(values$analysis_history, function(x) x$analysisId),
      `File Name` = sapply(values$analysis_history, function(x) x$fileName),
      `Treated Unit` = sapply(values$analysis_history, function(x) x$treatedUnit),
      `Treatment Year` = sapply(values$analysis_history, function(x) x$treatmentYear),
      `RMSPE` = round(sapply(values$analysis_history, function(x) as.numeric(x$rmspe)), 4),
      `Converged` = sapply(values$analysis_history, function(x) if(x$converged) "Yes" else "No"),
      `Saved At` = sapply(values$analysis_history, function(x) {
        format(as.POSIXct(x$savedAt), "%Y-%m-%d %H:%M")
      }),
      stringsAsFactors = FALSE
    )

    DT::datatable(
      history_df,
      selection = "single",
      options = list(
        pageLength = 10,
        scrollX = TRUE,
        ordering = TRUE,
        order = list(list(6, "desc"))  # Sort by date descending
      ),
      class = "compact stripe"
    )
  })

  # View selected summary button
  observeEvent(input$viewSelectedSummary, {
    req(input$historyTable_rows_selected)
    selected_row <- input$historyTable_rows_selected
    req(selected_row <= length(values$analysis_history))

    selected_analysis <- values$analysis_history[[selected_row]]
    values$selected_history_analysis <- selected_analysis
    values$show_analysis_summary <- TRUE

    showNotification("Analysis summary loaded!", type = "message", duration = 2)
  })

  # Selected analysis summary output
  output$selectedAnalysisSummary <- renderText({
    req(values$selected_history_analysis)

    analysis <- values$selected_history_analysis

    summary_text <- sprintf(
"Analysis ID: %s
File Name: %s
Treated Unit: %s
Treatment Year: %s
RMSPE: %.4f
Converged: %s
Saved At: %s

Configuration:
- Unit Variable: %s
- Time Variable: %s
- Outcome Variable: %s
- Predictor Variables: %s

This analysis can be loaded to view all results, plots, and details.",
      analysis$analysisId,
      analysis$fileName,
      analysis$treatedUnit,
      analysis$treatmentYear,
      as.numeric(analysis$rmspe),
      if(analysis$converged) "Yes" else "No",
      format(as.POSIXct(analysis$savedAt), "%Y-%m-%d %H:%M:%S"),
      if(!is.null(analysis$analysisConfig)) analysis$analysisConfig$unitVar else "N/A",
      if(!is.null(analysis$analysisConfig)) analysis$analysisConfig$timeVar else "N/A",
      if(!is.null(analysis$analysisConfig)) analysis$analysisConfig$outcomeVar else "N/A",
      if(!is.null(analysis$analysisConfig) && !is.null(analysis$analysisConfig$predictorVars)) {
        paste(analysis$analysisConfig$predictorVars, collapse = ", ")
      } else {
        "None"
      }
    )

    summary_text
  })

  # Load selected analysis button
  observeEvent(input$loadSelectedAnalysis, {
    req(input$historyTable_rows_selected)
    selected_row <- input$historyTable_rows_selected
    req(selected_row <= length(values$analysis_history))

    selected_analysis <- values$analysis_history[[selected_row]]
    analysis_id <- selected_analysis$analysisId

    showNotification("Loading analysis results...", type = "message", duration = NULL, id = "load_analysis")
    session$sendCustomMessage("loadAnalysisResults", list(analysisId = analysis_id))
  })

  # Handle loaded analysis data
  observeEvent(input$loaded_analysis_data, {
    req(input$loaded_analysis_data)
    loaded_analysis <- input$loaded_analysis_data

    tryCatch({
      # Restore the analysis results
      values$analysis_results <- loaded_analysis$analysisResults
      values$analysis_completed <- TRUE

      # Also restore configuration if available
      if(!is.null(loaded_analysis$analysisConfig)) {
        config <- loaded_analysis$analysisConfig

        # Update inputs to match the loaded analysis
        if(!is.null(config$unitVar)) {
          updateSelectInput(session, "unitVar", selected = config$unitVar)
        }
        if(!is.null(config$timeVar)) {
          updateSelectInput(session, "timeVar", selected = config$timeVar)
        }
        if(!is.null(config$outcomeVar)) {
          updateSelectInput(session, "outcomeVar", selected = config$outcomeVar)
        }
        if(!is.null(config$treatedUnit)) {
          updateSelectInput(session, "treatedUnit", selected = loaded_analysis$treatedUnit)
        }
        if(!is.null(config$treatmentYear)) {
          updateNumericInput(session, "treatmentYear", value = loaded_analysis$treatmentYear)
        }
        if(!is.null(config$predictorVars)) {
          updateCheckboxGroupInput(session, "predictorVars", selected = config$predictorVars)
        }
        if(!is.null(config$special_predictors)) {
          values$special_predictors <- config$special_predictors
        }
      }

      removeNotification("load_analysis")
      showNotification(
        HTML(paste0(
          icon("check-circle"),
          " Analysis loaded successfully! View results in the Results tab."
        )),
        type = "message",
        duration = 5
      )

    }, error = function(e) {
      removeNotification("load_analysis")
      showNotification(paste("Error loading analysis:", e$message), type = "error", duration = 5)
    })
  })

  # Delete selected analysis button
  observeEvent(input$deleteSelected, {
    req(input$historyTable_rows_selected)
    selected_row <- input$historyTable_rows_selected
    req(selected_row <= length(values$analysis_history))

    selected_analysis <- values$analysis_history[[selected_row]]
    analysis_id <- selected_analysis$analysisId

    session$sendCustomMessage("deleteAnalysis", list(analysisId = analysis_id))
    values$show_analysis_summary <- FALSE
    showNotification("Analysis deleted!", type = "warning", duration = 3)
  })

  # Auto-load history when the history tab is accessed
  observeEvent(input$sidebar, {
    if(!is.null(input$sidebar) && input$sidebar == "history") {
      session$sendCustomMessage("getAllAnalyses", list())
    }
  }, ignoreInit = TRUE)

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

      # Create temporary directory for plots
      tempDir <- tempdir()
      tempReport <- file.path(tempDir, "report.Rmd")

      # Generate plots and save them temporarily
      tryCatch({
        showNotification("Generating plots for PDF...", type = "message", duration = NULL, id = "pdf_plots")

        # Generate main plots with error handling
        p1 <- plot_actual_vs_synthetic(
          values$analysis_results$outcome_path,
          values$analysis_results$treatment_year,
          values$analysis_results$treated_unit
        )
        main_plot_path <- file.path(tempDir, "main_plot.png")
        ggsave(main_plot_path, plot = p1, width = 10, height = 6, dpi = 300, device = "png")

        if(!file.exists(main_plot_path)) {
          stop("Failed to save main plot")
        }

        p2 <- plot_treatment_gap(
          values$analysis_results$outcome_path,
          values$analysis_results$treatment_year
        )
        gap_plot_path <- file.path(tempDir, "gap_plot.png")
        ggsave(gap_plot_path, plot = p2, width = 10, height = 6, dpi = 300, device = "png")

        if(!file.exists(gap_plot_path)) {
          stop("Failed to save gap plot")
        }

        p3 <- plot_donor_weights(values$analysis_results$weights)
        weights_plot_path <- file.path(tempDir, "weights_plot.png")
        ggsave(weights_plot_path, plot = p3, width = 8, height = 6, dpi = 300, device = "png")

        if(!file.exists(weights_plot_path)) {
          stop("Failed to save weights plot")
        }

        removeNotification("pdf_plots")

        # Include placebo plots if available
        placebo_plot_section <- ""
        if(values$placebo_completed && !is.null(values$placebo_results)) {
          p_placebo <- plot_placebo_gaps(values$placebo_results$placebo_gaps, input$treatmentYear)
          placebo_plot_path <- file.path(tempDir, "placebo_plot.png")
          ggsave(placebo_plot_path, plot = p_placebo, width = 10, height = 6, dpi = 300)

          placebo_plot_section <- sprintf('
## Placebo Tests

```{r echo=FALSE, fig.height=4}
knitr::include_graphics("%s")
```

**P-value (Rank):** %.3f

The placebo test evaluates whether the observed treatment effect is unusually large compared to what we would expect by chance. The red line shows the actual treated unit, while gray lines show the effects when we artificially assign treatment to donor units.

', placebo_plot_path, values$placebo_results$ranking)
        }

        # Safely extract values and convert to appropriate types
        treated_unit <- as.character(values$analysis_results$treated_unit)
        treatment_year <- as.character(values$analysis_results$treatment_year)
        num_donors <- length(values$analysis_results$donor_units)
        rmspe <- as.numeric(values$analysis_results$rmspe)
        converged <- ifelse(values$analysis_results$converged, "Yes", "No")

        # Calculate average treatment effect safely
        post_treatment_data <- values$analysis_results$outcome_path[values$analysis_results$outcome_path$post_treatment, ]
        avg_treatment_effect <- mean(post_treatment_data$gap, na.rm = TRUE)

        # Calculate balance info safely
        pre_treatment_data <- values$analysis_results$outcome_path[!values$analysis_results$outcome_path$post_treatment, ]
        treated_mean <- mean(pre_treatment_data$treated_outcome, na.rm = TRUE)
        synthetic_mean <- mean(pre_treatment_data$synthetic_outcome, na.rm = TRUE)
        balance_diff <- mean(pre_treatment_data$gap, na.rm = TRUE)

        # Statistical inference text
        stat_inference <- if(values$placebo_completed) {
          sprintf("Placebo test p-value: %.3f", values$placebo_results$ranking)
        } else {
          "Run placebo tests for statistical inference"
        }

        # Create comprehensive report content
        report_content <- sprintf('
---
title: "Synthetic Control Analysis Report"
date: "%s"
output:
  pdf_document:
    fig_caption: yes
geometry: margin=1in
---

```{r setup, include=FALSE}
knitr::opts_chunk$set(echo = FALSE, warning = FALSE, message = FALSE)
library(knitr)
library(ggplot2)
```

# Executive Summary

This report presents results from a synthetic control analysis examining the impact of an intervention on **%s** starting in **%s**. The synthetic control method creates a weighted combination of similar control units to approximate what would have happened to the treated unit in the absence of the intervention.

# Analysis Configuration

- **Treated Unit:** %s
- **Treatment Year:** %s
- **Number of Donor Units:** %d
- **Pre-treatment RMSPE:** %.4f
- **Analysis Converged:** %s

# Results

## Outcome Trajectory

The following plot compares the actual outcome trajectory for the treated unit with its synthetic counterpart:

```{r echo=FALSE, fig.height=4, fig.cap="Actual vs Synthetic Outcome"}
knitr::include_graphics("%s")
```

## Treatment Effect

The treatment effect (gap between actual and synthetic) over time:

```{r echo=FALSE, fig.height=4, fig.cap="Treatment Effect Over Time"}
knitr::include_graphics("%s")
```

**Average Post-treatment Effect:** %.3f

## Donor Unit Composition

The synthetic control is constructed using the following donor unit weights:

```{r echo=FALSE, fig.height=4, fig.cap="Donor Unit Weights"}
knitr::include_graphics("%s")
```

### Donor Weights Table

```{r echo=FALSE}
weights_df <- data.frame(
  Unit = c(%s),
  Weight = round(c(%s), 4),
  stringsAsFactors = FALSE
)
weights_df <- weights_df[weights_df$Weight > 0, ]
weights_df <- weights_df[order(weights_df$Weight, decreasing = TRUE), ]
kable(weights_df, caption = "Donor Unit Weights (non-zero only)")
```

## Predictor Balance

The pre-treatment balance between treated and synthetic units:

```{r echo=FALSE}
balance_df <- data.frame(
  Predictor = "Outcome Variable",
  Treated = %.3f,
  Synthetic = %.3f,
  Difference = %.3f,
  stringsAsFactors = FALSE
)
kable(balance_df, caption = "Pre-treatment Predictor Balance")
```

%s

# Interpretation

The synthetic control method provides a transparent way to evaluate policy interventions by constructing a data-driven counterfactual. Key considerations:

- **Pre-treatment Fit:** Lower RMSPE indicates better pre-treatment fit (current: %.4f)
- **Treatment Effect:** The gap shows the estimated impact of the intervention
- **Statistical Inference:** %s

# Technical Notes

- Analysis conducted using the Synth R package
- Weights determined through optimization to minimize pre-treatment prediction error
- Results should be interpreted alongside domain knowledge and institutional context

*Report generated on %s using Synthetic Control Shiny App*

',
          # Header info
          Sys.Date(),
          treated_unit,
          treatment_year,

          # Configuration
          treated_unit,
          treatment_year,
          num_donors,
          rmspe,
          converged,

          # Plot paths
          main_plot_path,
          gap_plot_path,
          weights_plot_path,

          # Treatment effect
          avg_treatment_effect,

          # Weights table
          paste('"', names(values$analysis_results$weights), '"', collapse = ", "),
          paste(round(values$analysis_results$weights, 4), collapse = ", "),

          # Balance info
          treated_mean,
          synthetic_mean,
          balance_diff,

          # Placebo section
          placebo_plot_section,

          # Footer info
          rmspe,
          stat_inference,
          Sys.Date()
        )

        writeLines(report_content, tempReport)

        # Render the report
        rmarkdown::render(tempReport, output_file = file,
                         envir = new.env(), quiet = TRUE)

        # Clean up temporary plot files (optional - temp dir will be cleaned automatically)
        unlink(c(main_plot_path, gap_plot_path, weights_plot_path))
        if(values$placebo_completed) {
          placebo_plot_path <- file.path(tempDir, "placebo_plot.png")
          unlink(placebo_plot_path)
        }

      }, error = function(e) {
        # Fallback to simple report if plot generation fails
        simple_content <- sprintf('
---
title: "Synthetic Control Analysis Report"
date: "%s"
output: pdf_document
---

# Analysis Summary

**Error:** Could not generate plots for report: %s

**Treated Unit:** %s
**Treatment Year:** %s
**RMSPE:** %.3f

Please check the console for errors and try again.

*Report generated by Synthetic Control Shiny App*
',
          Sys.Date(),
          e$message,
          values$analysis_results$treated_unit,
          values$analysis_results$treatment_year,
          values$analysis_results$rmspe
        )

        writeLines(simple_content, tempReport)
        rmarkdown::render(tempReport, output_file = file, envir = new.env(), quiet = TRUE)
      })
    }
  )

  # ============================================
  # NEW PLACEBO TEST HANDLERS
  # ============================================

  # In-Space Placebo Test
  observeEvent(input$runInSpacePlacebo, {
    req(values$analysis_results, input$unitVar, input$timeVar, input$outcomeVar,
        input$treatedUnit, input$treatmentYear)

    # Reset state
    values$in_space_placebo_results <- NULL
    values$in_space_placebo_completed <- FALSE

    tryCatch({
      showNotification(
        HTML(paste0(icon("spinner", class = "fa-spin"), " Running in-space placebo tests...")),
        type = "message", duration = NULL, id = "in_space_placebo_progress"
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

      # Run in-space placebo test
      values$in_space_placebo_results <- in_space_placebo(
        data = values$data,
        outcome_var = input$outcomeVar,
        unit_var = input$unitVar,
        time_var = input$timeVar,
        treated_unit = input$treatedUnit,
        treat_time = input$treatmentYear,
        predictor_vars = predictor_vars,
        special_predictors_config = special_predictors_config
      )

      values$in_space_placebo_completed <- TRUE

      removeNotification("in_space_placebo_progress")
      showNotification(
        HTML(paste0(
          icon("check-circle"),
          " In-space placebo completed! (",
          values$in_space_placebo_results$successful_placebos, "/",
          values$in_space_placebo_results$total_attempted, " successful)"
        )),
        type = "message", duration = 5
      )

    }, error = function(e) {
      removeNotification("in_space_placebo_progress")
      showNotification(
        HTML(paste0(icon("exclamation-triangle"), " In-space placebo failed: ", e$message)),
        type = "error", duration = 8
      )
      values$in_space_placebo_completed <- FALSE
    })
  })

  # In-Time Placebo Test
  observeEvent(input$runInTimePlacebo, {
    req(values$analysis_results, input$unitVar, input$timeVar, input$outcomeVar,
        input$treatedUnit, input$treatmentYear, input$numFakeTimes)

    # Reset state
    values$in_time_placebo_results <- NULL
    values$in_time_placebo_completed <- FALSE

    tryCatch({
      showNotification(
        HTML(paste0(icon("spinner", class = "fa-spin"), " Running in-time placebo tests...")),
        type = "message", duration = NULL, id = "in_time_placebo_progress"
      )

      # Generate fake treatment times
      all_times <- sort(unique(values$data[[input$timeVar]]))
      pre_times <- all_times[all_times < input$treatmentYear]

      # Minimum periods required for each fake test (matching the function defaults)
      min_pre_periods <- 3
      min_post_periods <- 2
      min_total_buffer <- min_pre_periods + min_post_periods

      if(length(pre_times) < min_total_buffer + input$numFakeTimes) {
        stop(paste("Not enough pre-treatment periods. Need at least",
                  min_total_buffer + input$numFakeTimes,
                  "pre-treatment periods for", input$numFakeTimes, "fake times"))
      }

      # Create evenly spaced fake times, ensuring sufficient periods
      min_fake <- min(pre_times) + min_pre_periods  # Leave buffer for pre-periods
      max_fake <- input$treatmentYear - min_post_periods - 1  # Leave buffer for post-periods

      if(max_fake <= min_fake) {
        stop("Not enough time periods for fake treatment times with required buffers")
      }

      fake_treat_times <- seq(min_fake, max_fake, length.out = input$numFakeTimes)
      fake_treat_times <- round(fake_treat_times)
      fake_treat_times <- unique(fake_treat_times)

      # Ensure fake times exist in the data
      fake_treat_times <- fake_treat_times[fake_treat_times %in% pre_times]

      if(length(fake_treat_times) == 0) {
        stop("No valid fake treatment times could be generated")
      }

      # Use same predictor config as main analysis
      predictor_vars <- input$predictorVars
      if(is.null(predictor_vars) || length(predictor_vars) == 0) {
        predictor_vars <- NULL
      }

      special_predictors_config <- NULL
      if(length(values$special_predictors) > 0) {
        special_predictors_config <- values$special_predictors
      }

      # Run in-time placebo test
      values$in_time_placebo_results <- in_time_placebo(
        data = values$data,
        outcome_var = input$outcomeVar,
        unit_var = input$unitVar,
        time_var = input$timeVar,
        treated_unit = input$treatedUnit,
        true_treat_time = input$treatmentYear,
        fake_treat_times = fake_treat_times,
        predictor_vars = predictor_vars,
        special_predictors_config = special_predictors_config
      )

      values$in_time_placebo_completed <- TRUE

      removeNotification("in_time_placebo_progress")
      showNotification(
        HTML(paste0(
          icon("check-circle"),
          " In-time placebo completed! (",
          values$in_time_placebo_results$successful_tests, "/",
          values$in_time_placebo_results$total_attempted, " successful)"
        )),
        type = "message", duration = 5
      )

    }, error = function(e) {
      removeNotification("in_time_placebo_progress")
      showNotification(
        HTML(paste0(icon("exclamation-triangle"), " In-time placebo failed: ", e$message)),
        type = "error", duration = 8
      )
      values$in_time_placebo_completed <- FALSE
    })
  })

  # In-Space Placebo Outputs
  output$inSpacePValueBox <- renderValueBox({
    valueBox(
      value = if(!is.null(values$in_space_placebo_results) && !is.na(values$in_space_placebo_results$p_value)) {
        paste0(round(values$in_space_placebo_results$p_value * 100, 1), "%")
      } else {
        "N/A"
      },
      subtitle = "P-value (In-Space)",
      icon = icon("percentage"),
      color = "orange"
    )
  })

  output$inSpacePlaceboPlot <- renderPlot({
    req(values$in_space_placebo_results)
    plot_in_space_placebo(values$in_space_placebo_results)
  })

  output$inSpacePlaceboTable <- DT::renderDataTable({
    req(values$in_space_placebo_results)
    DT::datatable(values$in_space_placebo_results$summary_df,
                  options = list(pageLength = 10),
                  class = "compact stripe")
  })

  # In-Time Placebo Outputs
  output$inTimePlaceboPlot <- renderPlot({
    req(values$in_time_placebo_results)
    plot_in_time_placebo(values$in_time_placebo_results)
  })

  output$inTimePlaceboTable <- DT::renderDataTable({
    req(values$in_time_placebo_results)
    DT::datatable(values$in_time_placebo_results$summary_df,
                  options = list(pageLength = 10),
                  class = "compact stripe")
  })

}

# Run the application
shinyApp(ui = ui, server = server)
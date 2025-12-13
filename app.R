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
library(zip)  # For creating ZIP files

# Source helper functions
source("functions/data_functions.R")
source("functions/synth_wrapper.R")
source("functions/plotting_functions.R")
source("functions/placebo_tests.R")

# Define UI
ui <- dashboardPage(
  dashboardHeader(
    title = "Synthetic Control Analysis"
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
            console.log('saveAnalysisResults called with:', analysisData);

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
                inSpacePlacebo: analysisData.inSpacePlacebo || null,
                inTimePlacebo: analysisData.inTimePlacebo || null,
                savedAt: new Date().toISOString(),
                rmspe: analysisData.rmspe,
                converged: analysisData.converged
              };

              console.log('Saving to IndexedDB:', {
                id: data.analysisId,
                fileName: data.fileName,
                treatedUnit: data.treatedUnit,
                hasResults: !!data.analysisResults,
                hasConfig: !!data.analysisConfig
              });

              const request = store.put(data);

              request.onsuccess = function() {
                console.log('Analysis saved successfully:', data.analysisId);
                // Refresh history
                setTimeout(function() {
                  getAllAnalyses().then(function(analyses) {
                    console.log('Refreshing history after save, count:', analyses.length);
                    Shiny.setInputValue('analysis_history_data', analyses, {priority: 'event'});
                  });
                }, 100);
              };

              request.onerror = function() {
                console.error('Error saving analysis:', request.error);
              };
            }).catch(function(error) {
              console.error('Failed to save analysis:', error);
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
              p("Tests whether similar effects occur at a fake treatment date."),
              p(strong("Method:"), "Pretend treatment happened earlier, when no intervention actually occurred."),

              numericInput("fakeTreatmentYear", "Fake Treatment Year:",
                          value = NULL, min = 1900, max = 2100, step = 1),
              p("Select a year before the real treatment year.",
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
                title = "In-Space Results: Gap Plot", status = "primary", solidHeader = TRUE, width = 6,
                plotOutput("inSpacePlaceboPlot", height = "350px")
              ),
              box(
                title = "In-Space Results: RMSPE Ratio Distribution", status = "primary", solidHeader = TRUE, width = 6,
                plotOutput("inSpaceRMSPEHistogram", height = "350px")
              )
            ),
            fluidRow(
              box(
                title = "In-Space Summary Statistics", status = "primary", solidHeader = TRUE, width = 12,
                fluidRow(
                  column(4,
                    valueBoxOutput("inSpacePValueBox", width = 12)
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
                title = "In-Time Results: Outcome Paths", status = "info", solidHeader = TRUE, width = 6,
                plotOutput("inTimePlaceboPathPlot", height = "350px")
              ),
              box(
                title = "In-Time Results: Gap Plot", status = "info", solidHeader = TRUE, width = 6,
                plotOutput("inTimePlaceboPlot", height = "350px")
              )
            ),
            fluidRow(
              box(
                title = "In-Time Summary Statistics", status = "info", solidHeader = TRUE, width = 12,
                DT::dataTableOutput("inTimePlaceboTable")
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
              title = "Save Analysis", status = "success", solidHeader = TRUE, width = 12,
              h4("Save Current Analysis to History"),
              p("Save this analysis with a custom name for future reference."),
              fluidRow(
                column(8,
                  textInput("analysisName", "Analysis Name:",
                           placeholder = "e.g., California Prop 99 Analysis")
                ),
                column(4,
                  actionButton("saveAnalysis", "Save to History",
                             class = "btn-success btn-lg", style = "width: 100%; margin-top: 25px;",
                             icon = icon("save"))
                )
              )
            )
          ),

          fluidRow(
            box(
              title = "Export Data", status = "primary", solidHeader = TRUE, width = 6,
              h4("Download Data Tables"),
              downloadButton("downloadResults", "Download Results (CSV)",
                           class = "btn-success", style = "margin-bottom: 10px; width: 100%;"),
              downloadButton("downloadWeights", "Download Weights (CSV)",
                           class = "btn-info", style = "margin-bottom: 10px; width: 100%;"),
              downloadButton("downloadBalance", "Download Balance Table (CSV)",
                           class = "btn-warning", style = "margin-bottom: 10px; width: 100%;"),
              hr(),
              downloadButton("downloadAllData", "Download All Data (ZIP)",
                           class = "btn-primary btn-lg", style = "width: 100%;",
                           icon = icon("download"))
            ),

            box(
              title = "Export Plots", status = "info", solidHeader = TRUE, width = 6,
              h4("Main Analysis Plots"),
              downloadButton("downloadMainPlot", "Download Main Plot (PNG)",
                           class = "btn-success", style = "margin-bottom: 10px; width: 100%;"),
              downloadButton("downloadGapPlot", "Download Gap Plot (PNG)",
                           class = "btn-info", style = "margin-bottom: 10px; width: 100%;"),
              downloadButton("downloadWeightsPlot", "Download Weights Chart (PNG)",
                           class = "btn-warning", style = "margin-bottom: 10px; width: 100%;"),

              conditionalPanel(
                condition = "output.inSpacePlaceboCompleted || output.inTimePlaceboCompleted",
                hr(),
                h4("Placebo Test Plots"),
                conditionalPanel(
                  condition = "output.inSpacePlaceboCompleted",
                  downloadButton("downloadInSpaceGapPlot", "In-Space Gap Plot (PNG)",
                               class = "btn-success", style = "margin-bottom: 10px; width: 100%;"),
                  downloadButton("downloadInSpaceRMSPEPlot", "In-Space RMSPE Histogram (PNG)",
                               class = "btn-info", style = "margin-bottom: 10px; width: 100%;")
                ),
                conditionalPanel(
                  condition = "output.inTimePlaceboCompleted",
                  downloadButton("downloadInTimePathPlot", "In-Time Path Plot (PNG)",
                               class = "btn-success", style = "margin-bottom: 10px; width: 100%;"),
                  downloadButton("downloadInTimeGapPlot", "In-Time Gap Plot (PNG)",
                               class = "btn-info", style = "margin-bottom: 10px; width: 100%;")
                )
              ),

              hr(),
              downloadButton("downloadAllPlots", "Download All Plots (ZIP)",
                           class = "btn-primary btn-lg", style = "width: 100%;",
                           icon = icon("download"))
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
            p("View and manage your saved analysis results. Use the 'Save to History' button in the Export tab to save analyses."),

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

      # Remove progress notification and show success
      removeNotification("analysis_progress")
      showNotification("Analysis completed successfully! Use the Export tab to save to history.", type = "message", duration = 3)

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

  # Report preview handler

  # ============================================
  # ANALYSIS HISTORY HANDLERS
  # ============================================

  # Handle analysis history data from IndexedDB
  observeEvent(input$analysis_history_data, {
    req(input$analysis_history_data)

    cat("Received analysis_history_data\n")
    cat("Type:", class(input$analysis_history_data), "\n")
    cat("Length:", length(input$analysis_history_data), "\n")

    # Check if it's a valid list of analyses
    if(is.list(input$analysis_history_data) && length(input$analysis_history_data) > 0) {
      # Check if the first element has the expected structure
      first_elem <- input$analysis_history_data[[1]]
      if(is.list(first_elem) && "analysisId" %in% names(first_elem)) {
        values$analysis_history <- input$analysis_history_data
        cat("Valid analysis history data received:", length(input$analysis_history_data), "analyses\n")
      } else {
        cat("Invalid structure - first element doesn't have expected fields\n")
        cat("First element names:", names(first_elem), "\n")
        values$analysis_history <- NULL
      }
    } else {
      cat("Empty or invalid analysis history data\n")
      values$analysis_history <- NULL
    }
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

  # Save analysis with custom name
  observeEvent(input$saveAnalysis, {
    req(values$analysis_results)

    # Get custom name or use default
    custom_name <- input$analysisName
    if(is.null(custom_name) || custom_name == "") {
      custom_name <- paste0(input$treatedUnit, " Analysis")
    }

    tryCatch({
      analysis_id <- paste0(
        gsub("[^A-Za-z0-9]", "_", custom_name), "_",
        format(Sys.time(), "%Y%m%d_%H%M%S")
      )

      analysis_data <- list(
        analysisId = analysis_id,
        fileName = custom_name,
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
        converged = values$analysis_results$converged,
        inSpacePlacebo = values$in_space_placebo_results,
        inTimePlacebo = values$in_time_placebo_results
      )

      session$sendCustomMessage("saveAnalysisResults", list(analysisData = analysis_data))

      showNotification(
        paste0("Analysis saved as: ", custom_name),
        type = "message",
        duration = 3
      )

      # Clear the input field
      updateTextInput(session, "analysisName", value = "")

    }, error = function(e) {
      showNotification(
        paste("Error saving analysis:", e$message),
        type = "error",
        duration = 5
      )
    })
  })

  # History table
  output$historyTable <- DT::renderDataTable({
    req(values$analysis_history)

    # Validate it's a proper list
    if(!is.list(values$analysis_history) || length(values$analysis_history) == 0) {
      return(DT::datatable(
        data.frame(Message = "No analyses saved yet"),
        options = list(dom = 't'),
        rownames = FALSE
      ))
    }

    cat("Rendering history table with", length(values$analysis_history), "entries\n")

    # Format the data for display - use [[ for safer access
    history_df <- data.frame(
      `Analysis ID` = sapply(values$analysis_history, function(x) {
        if(is.list(x) && "analysisId" %in% names(x)) x[["analysisId"]] else "N/A"
      }),
      `File Name` = sapply(values$analysis_history, function(x) {
        if(is.list(x) && "fileName" %in% names(x)) x[["fileName"]] else "N/A"
      }),
      `Treated Unit` = sapply(values$analysis_history, function(x) {
        if(is.list(x) && "treatedUnit" %in% names(x)) x[["treatedUnit"]] else "N/A"
      }),
      `Treatment Year` = sapply(values$analysis_history, function(x) {
        if(is.list(x) && "treatmentYear" %in% names(x)) x[["treatmentYear"]] else NA
      }),
      `RMSPE` = round(sapply(values$analysis_history, function(x) {
        if(is.list(x) && "rmspe" %in% names(x)) as.numeric(x[["rmspe"]]) else NA
      }), 4),
      `Converged` = sapply(values$analysis_history, function(x) {
        if(is.list(x) && "converged" %in% names(x) && x[["converged"]]) "Yes" else "No"
      }),
      `Saved At` = sapply(values$analysis_history, function(x) {
        if(is.list(x) && "savedAt" %in% names(x)) {
          format(as.POSIXct(x[["savedAt"]]), "%Y-%m-%d %H:%M")
        } else {
          "N/A"
        }
      }),
      stringsAsFactors = FALSE,
      check.names = FALSE
    )

    cat("History table data frame has", nrow(history_df), "rows\n")

    DT::datatable(
      history_df,
      selection = "single",
      options = list(
        pageLength = 10,
        scrollX = TRUE,
        ordering = TRUE,
        order = list(list(6, "desc"))  # Sort by date descending
      ),
      class = "compact stripe",
      rownames = FALSE
    )
  })

  # View selected summary button
  observeEvent(input$viewSelectedSummary, {
    req(input$historyTable_rows_selected)
    selected_row <- input$historyTable_rows_selected
    req(selected_row <= length(values$analysis_history))

    selected_analysis <- values$analysis_history[[selected_row]]

    # Validate analysis data before storing
    if(is.list(selected_analysis)) {
      values$selected_history_analysis <- selected_analysis
      values$show_analysis_summary <- TRUE
      showNotification("Analysis summary loaded!", type = "message", duration = 2)
    } else {
      showNotification("Error: Invalid analysis data", type = "error", duration = 5)
    }
  })

  # Selected analysis summary output
  output$selectedAnalysisSummary <- renderText({
    req(values$selected_history_analysis)

    analysis <- values$selected_history_analysis

    # Safely extract values using [[ operator
    tryCatch({
      analysis_id <- if("analysisId" %in% names(analysis)) analysis[["analysisId"]] else "N/A"
      file_name <- if("fileName" %in% names(analysis)) analysis[["fileName"]] else "N/A"
      treated_unit <- if("treatedUnit" %in% names(analysis)) analysis[["treatedUnit"]] else "N/A"
      treatment_year <- if("treatmentYear" %in% names(analysis)) analysis[["treatmentYear"]] else "N/A"
      rmspe <- if("rmspe" %in% names(analysis)) as.numeric(analysis[["rmspe"]]) else NA
      converged <- if("converged" %in% names(analysis) && analysis[["converged"]]) "Yes" else "No"
      saved_at <- if("savedAt" %in% names(analysis)) {
        format(as.POSIXct(analysis[["savedAt"]]), "%Y-%m-%d %H:%M:%S")
      } else {
        "N/A"
      }

      # Extract config safely
      unit_var <- "N/A"
      time_var <- "N/A"
      outcome_var <- "N/A"
      predictor_vars <- "None"

      if("analysisConfig" %in% names(analysis) && is.list(analysis[["analysisConfig"]])) {
        config <- analysis[["analysisConfig"]]
        if("unitVar" %in% names(config)) unit_var <- config[["unitVar"]]
        if("timeVar" %in% names(config)) time_var <- config[["timeVar"]]
        if("outcomeVar" %in% names(config)) outcome_var <- config[["outcomeVar"]]
        if("predictorVars" %in% names(config) && !is.null(config[["predictorVars"]])) {
          predictor_vars <- paste(config[["predictorVars"]], collapse = ", ")
        }
      }

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
        analysis_id, file_name, treated_unit, treatment_year,
        rmspe, converged, saved_at,
        unit_var, time_var, outcome_var, predictor_vars
      )

      summary_text
    }, error = function(e) {
      paste("Error rendering summary:", e$message)
    })
  })

  # Load selected analysis button
  observeEvent(input$loadSelectedAnalysis, {
    req(input$historyTable_rows_selected)
    selected_row <- input$historyTable_rows_selected
    req(selected_row <= length(values$analysis_history))

    selected_analysis <- values$analysis_history[[selected_row]]

    # Safely extract analysisId using [[ operator
    if(is.list(selected_analysis) && "analysisId" %in% names(selected_analysis)) {
      analysis_id <- selected_analysis[["analysisId"]]
      showNotification("Loading analysis results...", type = "message", duration = NULL, id = "load_analysis")
      session$sendCustomMessage("loadAnalysisResults", list(analysisId = analysis_id))
    } else {
      showNotification("Error: Invalid analysis data", type = "error", duration = 5)
    }
  })

  # Handle loaded analysis data
  observeEvent(input$loaded_analysis_data, {
    req(input$loaded_analysis_data)
    loaded_analysis <- input$loaded_analysis_data

    tryCatch({
      # Validate that loaded_analysis is a list
      if(!is.list(loaded_analysis)) {
        stop(paste("Invalid data type:", class(loaded_analysis)[1], "- Expected list"))
      }

      # Safely extract analysisResults using [[ operator
      if(!"analysisResults" %in% names(loaded_analysis)) {
        stop("Missing 'analysisResults' in loaded data")
      }

      analysis_results <- loaded_analysis[["analysisResults"]]
      if(is.null(analysis_results)) {
        stop("Analysis results are NULL")
      }

      # Restore the analysis results
      values$analysis_results <- analysis_results
      values$analysis_completed <- TRUE

      # Restore placebo results if available
      if("inSpacePlacebo" %in% names(loaded_analysis) && !is.null(loaded_analysis[["inSpacePlacebo"]])) {
        values$in_space_placebo_results <- loaded_analysis[["inSpacePlacebo"]]
        values$in_space_placebo_completed <- TRUE
      }

      if("inTimePlacebo" %in% names(loaded_analysis) && !is.null(loaded_analysis[["inTimePlacebo"]])) {
        values$in_time_placebo_results <- loaded_analysis[["inTimePlacebo"]]
        values$in_time_placebo_completed <- TRUE
      }

      # Also restore configuration if available
      if("analysisConfig" %in% names(loaded_analysis) && !is.null(loaded_analysis[["analysisConfig"]])) {
        config <- loaded_analysis[["analysisConfig"]]

        # Update inputs to match the loaded analysis - use [[ for safer access
        if(is.list(config)) {
          if("unitVar" %in% names(config) && !is.null(config[["unitVar"]])) {
            updateSelectInput(session, "unitVar", selected = config[["unitVar"]])
          }
          if("timeVar" %in% names(config) && !is.null(config[["timeVar"]])) {
            updateSelectInput(session, "timeVar", selected = config[["timeVar"]])
          }
          if("outcomeVar" %in% names(config) && !is.null(config[["outcomeVar"]])) {
            updateSelectInput(session, "outcomeVar", selected = config[["outcomeVar"]])
          }
          if("treatedUnit" %in% names(loaded_analysis) && !is.null(loaded_analysis[["treatedUnit"]])) {
            updateSelectInput(session, "treatedUnit", selected = loaded_analysis[["treatedUnit"]])
          }
          if("treatmentYear" %in% names(loaded_analysis) && !is.null(loaded_analysis[["treatmentYear"]])) {
            updateNumericInput(session, "treatmentYear", value = loaded_analysis[["treatmentYear"]])
          }
          if("predictorVars" %in% names(config) && !is.null(config[["predictorVars"]])) {
            updateCheckboxGroupInput(session, "predictorVars", selected = config[["predictorVars"]])
          }
          if("special_predictors" %in% names(config) && !is.null(config[["special_predictors"]])) {
            values$special_predictors <- config[["special_predictors"]]
          }
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
      showNotification(
        paste("Error loading analysis:", e$message),
        type = "error",
        duration = 10
      )
      cat("ERROR loading analysis:", e$message, "\n")
      cat("loaded_analysis structure:\n")
      str(loaded_analysis)
    })
  })

  # Delete selected analysis button
  observeEvent(input$deleteSelected, {
    req(input$historyTable_rows_selected)
    selected_row <- input$historyTable_rows_selected
    req(selected_row <= length(values$analysis_history))

    selected_analysis <- values$analysis_history[[selected_row]]

    # Safely extract analysisId using [[ operator
    if(is.list(selected_analysis) && "analysisId" %in% names(selected_analysis)) {
      analysis_id <- selected_analysis[["analysisId"]]
      session$sendCustomMessage("deleteAnalysis", list(analysisId = analysis_id))
      values$show_analysis_summary <- FALSE
    } else {
      showNotification("Error: Invalid analysis data", type = "error", duration = 5)
    }
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

  # Placebo test plot downloads
  output$downloadInSpaceGapPlot <- downloadHandler(
    filename = function() {
      paste("in_space_gap_plot_", Sys.Date(), ".png", sep = "")
    },
    content = function(file) {
      req(values$in_space_placebo_results)
      p <- plot_in_space_placebo(values$in_space_placebo_results)
      ggsave(file, plot = p, width = 10, height = 6, dpi = 300)
    }
  )

  output$downloadInSpaceRMSPEPlot <- downloadHandler(
    filename = function() {
      paste("in_space_rmspe_histogram_", Sys.Date(), ".png", sep = "")
    },
    content = function(file) {
      req(values$in_space_placebo_results)
      p <- plot_in_space_rmspe_histogram(values$in_space_placebo_results)
      ggsave(file, plot = p, width = 10, height = 6, dpi = 300)
    }
  )

  output$downloadInTimePathPlot <- downloadHandler(
    filename = function() {
      paste("in_time_path_plot_", Sys.Date(), ".png", sep = "")
    },
    content = function(file) {
      req(values$in_time_placebo_results)
      p <- plot_in_time_placebo_paths(values$in_time_placebo_results)
      ggsave(file, plot = p, width = 10, height = 6, dpi = 300)
    }
  )

  output$downloadInTimeGapPlot <- downloadHandler(
    filename = function() {
      paste("in_time_gap_plot_", Sys.Date(), ".png", sep = "")
    },
    content = function(file) {
      req(values$in_time_placebo_results)
      p <- plot_in_time_placebo(values$in_time_placebo_results)
      ggsave(file, plot = p, width = 10, height = 6, dpi = 300)
    }
  )

  # Download all plots as ZIP
  output$downloadAllPlots <- downloadHandler(
    filename = function() {
      paste("all_plots_", Sys.Date(), ".zip", sep = "")
    },
    content = function(file) {
      req(values$analysis_results)

      # Create temporary directory
      temp_dir <- file.path(tempdir(), "plots")
      dir.create(temp_dir, showWarnings = FALSE, recursive = TRUE)

      # Generate and save all main plots
      p1 <- plot_actual_vs_synthetic(
        values$analysis_results$outcome_path,
        values$analysis_results$treatment_year,
        values$analysis_results$treated_unit
      )
      ggsave(file.path(temp_dir, "01_main_plot.png"), plot = p1,
             width = 10, height = 6, dpi = 300)

      p2 <- plot_treatment_gap(
        values$analysis_results$outcome_path,
        values$analysis_results$treatment_year
      )
      ggsave(file.path(temp_dir, "02_gap_plot.png"), plot = p2,
             width = 10, height = 6, dpi = 300)

      p3 <- plot_donor_weights(values$analysis_results$weights)
      ggsave(file.path(temp_dir, "03_donor_weights.png"), plot = p3,
             width = 8, height = 6, dpi = 300)

      # Add in-space placebo plots if available
      if(!is.null(values$in_space_placebo_results)) {
        p4 <- plot_in_space_placebo(values$in_space_placebo_results)
        ggsave(file.path(temp_dir, "04_in_space_gap.png"), plot = p4,
               width = 10, height = 6, dpi = 300)

        p5 <- plot_in_space_rmspe_histogram(values$in_space_placebo_results)
        ggsave(file.path(temp_dir, "05_in_space_rmspe.png"), plot = p5,
               width = 10, height = 6, dpi = 300)
      }

      # Add in-time placebo plots if available
      if(!is.null(values$in_time_placebo_results)) {
        p6 <- plot_in_time_placebo_paths(values$in_time_placebo_results)
        ggsave(file.path(temp_dir, "06_in_time_paths.png"), plot = p6,
               width = 10, height = 6, dpi = 300)

        p7 <- plot_in_time_placebo(values$in_time_placebo_results)
        ggsave(file.path(temp_dir, "07_in_time_gap.png"), plot = p7,
               width = 10, height = 6, dpi = 300)
      }

      # Create ZIP file
      zip::zip(file, files = list.files(temp_dir, full.names = TRUE),
               mode = "cherry-pick")

      # Clean up
      unlink(temp_dir, recursive = TRUE)
    }
  )

  # Download all data as ZIP
  output$downloadAllData <- downloadHandler(
    filename = function() {
      paste("all_data_", Sys.Date(), ".zip", sep = "")
    },
    content = function(file) {
      req(values$analysis_results)

      # Create temporary directory
      temp_dir <- file.path(tempdir(), "data")
      dir.create(temp_dir, showWarnings = FALSE, recursive = TRUE)

      # Save main analysis results
      write.csv(values$analysis_results$outcome_path,
                file.path(temp_dir, "01_outcome_path.csv"),
                row.names = FALSE)

      # Save weights
      weights_df <- data.frame(
        Unit = names(values$analysis_results$weights),
        Weight = as.numeric(values$analysis_results$weights)
      )
      weights_df <- weights_df[weights_df$Weight > 0, ]
      weights_df <- weights_df[order(weights_df$Weight, decreasing = TRUE), ]
      write.csv(weights_df,
                file.path(temp_dir, "02_donor_weights.csv"),
                row.names = FALSE)

      # Save balance table if available
      if(!is.null(values$analysis_results$balance_table)) {
        write.csv(values$analysis_results$balance_table,
                  file.path(temp_dir, "03_balance_table.csv"),
                  row.names = FALSE)
      }

      # Save in-space placebo results if available
      if(!is.null(values$in_space_placebo_results)) {
        write.csv(values$in_space_placebo_results$summary_df,
                  file.path(temp_dir, "04_in_space_placebo_summary.csv"),
                  row.names = FALSE)
        write.csv(values$in_space_placebo_results$gap_df,
                  file.path(temp_dir, "05_in_space_placebo_gaps.csv"),
                  row.names = FALSE)
      }

      # Save in-time placebo results if available
      if(!is.null(values$in_time_placebo_results)) {
        write.csv(values$in_time_placebo_results$summary_df,
                  file.path(temp_dir, "06_in_time_placebo_summary.csv"),
                  row.names = FALSE)
        write.csv(values$in_time_placebo_results$gap_df,
                  file.path(temp_dir, "07_in_time_placebo_gaps.csv"),
                  row.names = FALSE)
        write.csv(values$in_time_placebo_results$path_df,
                  file.path(temp_dir, "08_in_time_placebo_paths.csv"),
                  row.names = FALSE)
      }

      # Create ZIP file
      zip::zip(file, files = list.files(temp_dir, full.names = TRUE),
               mode = "cherry-pick")

      # Clean up
      unlink(temp_dir, recursive = TRUE)
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
        input$treatedUnit, input$treatmentYear, input$fakeTreatmentYear)

    # Reset state
    values$in_time_placebo_results <- NULL
    values$in_time_placebo_completed <- FALSE

    tryCatch({
      showNotification(
        HTML(paste0(icon("spinner", class = "fa-spin"), " Running in-time placebo test...")),
        type = "message", duration = NULL, id = "in_time_placebo_progress"
      )

      # Validate fake treatment year
      if(input$fakeTreatmentYear >= input$treatmentYear) {
        stop("Fake treatment year must be before the real treatment year")
      }

      all_times <- sort(unique(values$data[[input$timeVar]]))

      if(!(input$fakeTreatmentYear %in% all_times)) {
        stop("Fake treatment year does not exist in the data")
      }

      # Minimum periods required
      min_pre_periods <- 3
      min_post_periods <- 2

      pre_fake_times <- all_times[all_times < input$fakeTreatmentYear]
      post_fake_times <- all_times[all_times >= input$fakeTreatmentYear & all_times < input$treatmentYear]

      if(length(pre_fake_times) < min_pre_periods) {
        stop(paste("Not enough pre-periods before fake treatment year. Need at least", min_pre_periods))
      }

      if(length(post_fake_times) < min_post_periods) {
        stop(paste("Not enough post-periods after fake treatment year. Need at least", min_post_periods))
      }

      # Store fake treatment year for plotting
      values$fake_treatment_year <- input$fakeTreatmentYear

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
        fake_treat_times = input$fakeTreatmentYear,
        predictor_vars = predictor_vars,
        special_predictors_config = special_predictors_config
      )

      values$in_time_placebo_completed <- TRUE

      removeNotification("in_time_placebo_progress")
      showNotification(
        HTML(paste0(
          icon("check-circle"),
          " In-time placebo completed!"
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

  output$inSpaceRMSPEHistogram <- renderPlot({
    req(values$in_space_placebo_results)
    plot_in_space_rmspe_histogram(values$in_space_placebo_results)
  })

  output$inSpacePlaceboTable <- DT::renderDataTable({
    req(values$in_space_placebo_results)
    DT::datatable(values$in_space_placebo_results$summary_df,
                  options = list(pageLength = 10),
                  class = "compact stripe")
  })

  # In-Time Placebo Outputs
  output$inTimePlaceboPathPlot <- renderPlot({
    req(values$in_time_placebo_results)
    plot_in_time_placebo_paths(values$in_time_placebo_results)
  })

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
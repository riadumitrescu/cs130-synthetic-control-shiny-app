# Synthetic Control Shiny App - Project Overview

## 📋 Project Summary

This is a **no-code R Shiny application** for performing synthetic control analysis - a causal inference method used to estimate treatment effects. The app provides a user-friendly interface for uploading panel data, configuring analysis parameters, and generating publication-quality results.

**Status:** ✅ **COMPLETED** - All 8 core tasks implemented according to PRD

---

## 🎯 What This Project Does

The app allows users to:
1. **Upload panel datasets** (CSV or Excel)
2. **Configure analysis** by selecting variables and treatment settings
3. **Run synthetic control analysis** using the official Synth R package
4. **Visualize results** with publication-quality plots
5. **Perform placebo tests** for statistical inference
6. **Export results** as CSV, PNG, or PDF reports

**Target Users:** Students, policy analysts, and instructors who need to run synthetic control without coding.

---

## 📦 Required R Packages

### Currently Installed ✅
- `ggplot2` (3.5.2) - Plotting
- `dplyr` (1.1.4) - Data manipulation
- `tidyr` (1.3.1) - Data reshaping
- `Synth` (1.1-9) - Official synthetic control package
- `rmarkdown` (2.29) - PDF report generation
- `knitr` (1.50) - Report rendering

### Missing Packages ❌ (Need Installation)
- `shiny` - Core Shiny framework
- `DT` - Interactive data tables
- `readxl` - Excel file reading
- `shinydashboard` - Dashboard UI components

### Installation Command
```r
install.packages(c("shiny", "DT", "readxl", "shinydashboard"))
```

---

## 📁 Project Structure

```
cs130-synthetic-control-shiny-app/
├── app.R                          # Main Shiny application (1137 lines)
├── synthetic_control_prd.txt      # Product Requirements Document
├── TASK_LIST.md                   # Development progress tracking
│
├── functions/                     # Core functionality modules
│   ├── data_functions.R          # Data validation & preparation
│   ├── synth_wrapper.R           # Synth package integration
│   ├── plotting_functions.R      # Visualization functions
│   ├── synthetic_control_simple.R    # (Legacy - not used)
│   ├── synthetic_control_robust.R    # (Legacy - not used)
│   ├── synthetic_control_functions.R # (Legacy - not used)
│   └── synthetic_control_bulletproof.R # (Legacy - not used)
│
├── test_*.R                       # Various test scripts
├── debug_*.R                      # Debugging scripts
│
└── example_data.csv               # Sample dataset for testing
```

---

## 🔧 How to Run the App

### Prerequisites
1. Install missing R packages (see above)
2. Ensure R version 4.0+ is installed

### Running the App
```r
# In R or RStudio:
shiny::runApp("app.R")
```

Or from command line:
```bash
Rscript -e "shiny::runApp('app.R')"
```

The app will launch in your default web browser, typically at `http://127.0.0.1:XXXX`

---

## 🏗️ Architecture & Key Components

### Main Application (`app.R`)
- **UI:** Built with `shinydashboard` - 5-tab workflow:
  1. Upload Data
  2. Configure Analysis
  3. Results
  4. Placebo Tests
  5. Export

- **Server Logic:** 
  - Reactive data handling
  - Dynamic UI updates
  - Analysis execution
  - Result storage and display

### Core Functions

#### `functions/data_functions.R`
- `validate_panel_data()` - Validates uploaded data structure
- `check_panel_balance()` - Checks panel balance
- `prepare_sc_data()` - Prepares data for analysis
- `calculate_predictor_means()` - Computes pre-treatment means

#### `functions/synth_wrapper.R`
- `run_synth_analysis()` - Main analysis function using Synth package
- `run_synth_placebo()` - Placebo test execution
- Handles `dataprep()` and `synth()` from Synth package

#### `functions/plotting_functions.R`
- `plot_actual_vs_synthetic()` - Main comparison plot
- `plot_treatment_gap()` - Treatment effect visualization
- `plot_donor_weights()` - Weight distribution chart
- `plot_placebo_gaps()` - Placebo test visualization
- `create_balance_table()` - Predictor balance table

---

## ✅ Current Status

### Completed Features
- ✅ Data upload (CSV/Excel) with validation
- ✅ Variable mapping interface
- ✅ Treatment configuration
- ✅ Predictor builder (simple + advanced modes)
- ✅ Synthetic control computation using Synth package
- ✅ Comprehensive visualizations (4 plots + tables)
- ✅ Placebo inference with statistical ranking
- ✅ Complete export system (CSV, PNG, PDF)

### Technical Implementation
- Uses **official Synth package** by Abadie et al. (not custom implementation)
- Supports both regular predictors (pre-treatment mean) and special predictors (custom time windows)
- Robust error handling throughout
- Clean, minimal UI design per PRD requirements

---

## ⚠️ Known Issues & Notes

### Missing Dependencies
- **4 packages need installation** before app can run:
  - `shiny`, `DT`, `readxl`, `shinydashboard`

### Legacy Code
- Several test/debug files exist but are not part of the main app:
  - `test_*.R` files - Various test scripts
  - `debug_*.R` files - Debugging utilities
  - Legacy synthetic control implementations in `functions/` (not used by app.R)

### Potential Issues
1. **No package version locking** - No `renv` or `packrat` setup
2. **No error logging** - Errors shown via notifications only
3. **PDF report generation** - Requires LaTeX installation for rmarkdown PDF output
4. **Large datasets** - No explicit performance optimization for very large datasets

---

## 🧪 Testing

The project includes several test scripts:
- `test_complete_workflow.R` - End-to-end workflow test
- `test_simple.R`, `test_robust.R`, etc. - Various component tests
- `example_data.csv` - Sample dataset for testing

**Note:** Tests use legacy synthetic control functions, not the Synth package wrapper used in the app.

---

## 📊 Data Format Requirements

The app expects panel data with:
- **Unit identifier column** (e.g., state, country, firm)
- **Time variable column** (e.g., year, quarter)
- **Outcome variable** (the variable of interest)
- **Optional predictor variables** (for matching)

Example structure (from `example_data.csv`):
```
state, year, gdp, population, education_spending
California, 2000, 1000, 33000000, 50000
California, 2001, 1050, 33500000, 52000
...
```

---

## 🚀 Next Steps / Recommendations

1. **Install missing packages** to make app runnable
2. **Test the app** with example_data.csv
3. **Consider adding:**
   - Package version management (renv)
   - Error logging system
   - Performance optimization for large datasets
   - Unit tests for core functions
   - Docker containerization for deployment

---

## 📝 Key Files Reference

| File | Purpose | Lines |
|------|---------|-------|
| `app.R` | Main Shiny application | 1137 |
| `functions/synth_wrapper.R` | Synth package integration | 330 |
| `functions/plotting_functions.R` | Visualization functions | 162 |
| `functions/data_functions.R` | Data processing | 118 |
| `synthetic_control_prd.txt` | Product requirements | 156 |
| `TASK_LIST.md` | Development progress | 184 |

---

## 🎓 Academic Context

This appears to be a CS130 (likely a course) project implementing synthetic control methodology. The app follows the standard synthetic control workflow:
1. Pre-treatment matching using predictors
2. Weight optimization (non-negative, sum-to-one constraints)
3. Synthetic control construction
4. Treatment effect estimation
5. Placebo inference for validation

---

**Last Updated:** Based on current repository state
**Repository Status:** Up to date with GitHub (verified)


# How the Synthetic Control Shiny App Works

## 🎯 Overview

This is a **no-code web application** built with R Shiny that performs **Synthetic Control Analysis** - a statistical method for estimating causal treatment effects. The app provides a user-friendly interface to run this complex analysis without writing any code.

---

## 📚 What is Synthetic Control?

**Synthetic Control** is a method developed by Abadie & Gardeazabal (2003) and Abadie, Diamond, & Hainmueller (2010) to estimate the causal effect of an intervention (treatment) on a single treated unit.

### The Core Idea:
Instead of finding one "control" unit to compare against, synthetic control creates a **weighted combination of multiple control units** (called "donors") that best matches the treated unit's pre-treatment characteristics. This "synthetic control" serves as the counterfactual - what would have happened to the treated unit if it hadn't received treatment.

### Example Use Case:
- **Question:** Did California's smoking ban (treatment) reduce smoking rates?
- **Data:** Panel data with states (units) over years (time)
- **Treated Unit:** California (treatment in 2005)
- **Donor Pool:** All other US states
- **Outcome:** Smoking rate
- **Method:** Create a "synthetic California" from weighted combination of other states that matches California's pre-2005 smoking trends, then compare post-2005 outcomes

---

## 🏗️ App Architecture

### Technology Stack
- **Frontend:** Shiny Dashboard (R Shiny framework)
- **Backend:** R with Synth package (official implementation)
- **Visualization:** ggplot2, DT (interactive tables)
- **Data Processing:** dplyr, tidyr

### Key Components

```
app.R (Main Application)
├── UI Layer (User Interface)
│   ├── 5 Tabs: Upload → Configure → Results → Placebo → Export
│   └── Reactive UI elements (dropdowns, buttons, plots)
│
└── Server Layer (Business Logic)
    ├── Data Upload & Validation
    ├── Analysis Execution
    ├── Result Storage
    └── Visualization Generation

functions/ (Supporting Modules)
├── data_functions.R      → Data validation & preparation
├── synth_wrapper.R       → Synth package integration
└── plotting_functions.R  → Visualization functions
```

---

## 🔄 Complete Workflow

### Step 1: Upload Data (Tab 1)
**What happens:**
1. User uploads CSV or Excel file
2. App reads file using `read.csv()` or `read_excel()`
3. Data preview shown in interactive table
4. `validate_panel_data()` checks:
   - Minimum columns (≥3)
   - Empty columns
   - Reasonable number of observations

**User sees:**
- File upload button
- Data preview table
- Validation status (green checkmark or warnings)

---

### Step 2: Configure Analysis (Tab 2)

#### 2.1 Variable Mapping
**What happens:**
- User selects columns from dropdowns:
  - **Unit Identifier:** Which column identifies units (e.g., "state", "country")
  - **Time Variable:** Which column is time (e.g., "year", "quarter")
  - **Outcome Variable:** What we're measuring (e.g., "gdp", "smoking_rate")

**Code location:** `app.R` lines 92-97

#### 2.2 Treatment Setup
**What happens:**
- User selects:
  - **Treated Unit:** Which unit received treatment (e.g., "California")
  - **Treatment Year:** When treatment started (e.g., 2005)
- App automatically:
  - Identifies donor pool (all units except treated)
  - Calculates pre/post treatment periods
  - Shows summary statistics

**Code location:** `app.R` lines 106-118

#### 2.3 Predictor Configuration
**Two types of predictors:**

**A. Regular Predictors (Simple Mode)**
- User selects variables (e.g., "population", "education_spending")
- App automatically calculates **pre-treatment mean** for each variable
- Used to match treated unit's characteristics

**B. Special Predictors (Advanced Mode)**
- User can specify custom time windows
- Example: "gdp" from 2000-2004 (mean)
- More flexible matching

**Default Behavior:**
- If no predictors specified, app uses **outcome variable at each pre-treatment time point**
- This is the standard synthetic control approach

**Code location:** `app.R` lines 124-173, `functions/synth_wrapper.R` lines 58-95

#### 2.4 Run Analysis
**What happens when user clicks "Run Analysis":**

1. **Data Preparation** (`functions/synth_wrapper.R` lines 102-121):
   ```r
   dataprep()  # From Synth package
   ```
   - Converts data to format required by Synth package
   - Creates predictor matrices (treated vs donors)
   - Sets up optimization constraints

2. **Weight Optimization** (`functions/synth_wrapper.R` lines 124-129):
   ```r
   synth()  # From Synth package
   ```
   - Finds optimal weights for donor units
   - Minimizes pre-treatment prediction error
   - Constraints: weights ≥ 0, sum to 1

3. **Result Extraction** (`functions/synth_wrapper.R` lines 131-198):
   - Extracts weights for each donor unit
   - Computes synthetic outcome path
   - Calculates treatment effect (gap = actual - synthetic)
   - Computes RMSPE (Root Mean Squared Prediction Error)

**Code location:** `app.R` lines 748-794

---

### Step 3: View Results (Tab 3)

**What the user sees:**

#### 3.1 Summary Statistics (Value Boxes)
- **Pre-treatment RMSPE:** How well synthetic control matches pre-treatment
- **Donor Units:** Number of units in donor pool
- **Average Treatment Effect:** Mean gap in post-treatment period

#### 3.2 Main Visualizations

**A. Actual vs Synthetic Plot**
- Shows treated unit's actual outcome vs synthetic control
- Vertical line marks treatment year
- **Interpretation:** If lines diverge after treatment, suggests treatment effect

**B. Gap Plot (Treatment Effect)**
- Shows gap = actual - synthetic over time
- **Interpretation:** Positive gap = treatment increased outcome

**C. Donor Weights Chart**
- Bar chart showing which donor units contribute most
- **Interpretation:** Higher weights = more similar to treated unit

**D. Pre-treatment Balance Table**
- Compares treated unit vs synthetic control on predictors
- **Interpretation:** Small differences = good match

**Code location:** `app.R` lines 796-887, `functions/plotting_functions.R`

---

### Step 4: Placebo Tests (Tab 4)

**What is a Placebo Test?**
- Runs synthetic control analysis on **each donor unit** as if it were treated
- Tests: "Would we see similar effects if treatment happened to a control unit?"
- **Purpose:** Validates that the treatment effect is real, not spurious

**What happens:**
1. User clicks "Run Placebo Tests"
2. App runs `run_synth_placebo()` which:
   - Loops through each donor unit
   - Runs synthetic control with that unit as "treated"
   - Collects gap trajectories
3. Results shown:
   - **Placebo Plot:** Gray lines (placebos) + red line (actual treated)
   - **P-value (Rank):** Percentile ranking of actual effect
   - **Summary Table:** Statistics for each unit

**Interpretation:**
- If actual treated unit's effect is extreme (high rank), suggests real treatment effect
- P-value = proportion of placebo effects ≥ actual effect

**Code location:** `app.R` lines 889-996, `functions/synth_wrapper.R` lines 202-296

---

### Step 5: Export (Tab 5)

**Available Downloads:**
1. **Results CSV:** Outcome path data (time, actual, synthetic, gap)
2. **Weights CSV:** Donor unit weights
3. **Balance Table CSV:** Predictor balance comparison
4. **Plot PNGs:** High-resolution versions of visualizations
5. **PDF Report:** Comprehensive report with all results

**Code location:** `app.R` lines 998-1132

---

## 🔬 The Math Behind It

### Objective Function
The synthetic control method finds weights **w** that minimize:

```
RMSPE = √[Σ(Y_treated - Y_synthetic)² / T_pre]
```

Where:
- `Y_treated` = treated unit's pre-treatment outcome
- `Y_synthetic` = Σ(w_i × Y_donor_i) = weighted combination of donors
- `T_pre` = number of pre-treatment periods

### Constraints
1. **Non-negativity:** w_i ≥ 0 for all i
2. **Sum to one:** Σw_i = 1

### Optimization
- Uses **quadratic programming** (via Synth package)
- Algorithm: BFGS (Broyden-Fletcher-Goldfarb-Shanno)

---

## 💻 Code Flow Example

Let's trace what happens when a user runs an analysis:

```
1. User clicks "Run Analysis" button
   ↓
2. app.R line 749: observeEvent(input$runAnalysis, ...)
   ↓
3. app.R line 772: run_synth_analysis(...)
   ↓
4. synth_wrapper.R line 22: run_synth_analysis() function
   ↓
5. synth_wrapper.R line 105: dataprep() - prepare data
   ↓
6. synth_wrapper.R line 126: synth() - optimize weights
   ↓
7. synth_wrapper.R line 132: synth.tab() - extract results
   ↓
8. synth_wrapper.R line 176: Return results list
   ↓
9. app.R line 783: Store in values$analysis_results
   ↓
10. app.R line 838: Render plots using plotting functions
```

---

## 🎨 UI/UX Design Philosophy

### Design Principles (from PRD):
1. **Minimal cognitive load:** Simple, step-by-step workflow
2. **No jargon:** User-friendly language
3. **Visual feedback:** Progress notifications, validation messages
4. **Progressive disclosure:** Advanced features hidden by default
5. **Clean layout:** Sidebar navigation, organized tabs

### Reactive Programming
The app uses Shiny's reactive programming model:
- **Reactive values:** `values$data`, `values$analysis_results`
- **Reactive expressions:** Update automatically when inputs change
- **Observers:** Respond to user actions (button clicks, file uploads)

---

## 🧪 Testing the App

### Quick Test:
1. **Start the app:**
   ```r
   shiny::runApp("app.R")
   ```

2. **Use example data:**
   - Upload `example_data.csv`
   - Select: Unit = "state", Time = "year", Outcome = "gdp"
   - Treated Unit = "California", Treatment Year = 2005
   - Run analysis

3. **Expected results:**
   - Synthetic control matches California's pre-2005 GDP
   - Post-2005 gap shows treatment effect
   - Donor weights show which states contribute most

---

## 🔍 Key Functions Reference

### `run_synth_analysis()` (`functions/synth_wrapper.R`)
**Purpose:** Main analysis function
**Inputs:**
- `data`: Panel dataset
- `unit_var`, `time_var`, `outcome_var`: Column names
- `treated_unit`, `treatment_year`: Treatment specification
- `predictor_vars`: Optional predictor variables
- `special_predictors_config`: Optional custom predictors

**Outputs:**
- `weights`: Donor unit weights
- `outcome_path`: Time series of actual vs synthetic
- `rmspe`: Pre-treatment fit quality
- `predictor_balance`: Matching quality on predictors

### `run_synth_placebo()` (`functions/synth_wrapper.R`)
**Purpose:** Run placebo tests
**Process:**
- Loops through each unit
- Runs synthetic control with that unit as treated
- Collects gap trajectories
- Calculates p-value (rank)

### `plot_actual_vs_synthetic()` (`functions/plotting_functions.R`)
**Purpose:** Create main comparison plot
**Uses:** ggplot2 for publication-quality visualization

---

## ⚠️ Important Notes

### Data Requirements:
- **Panel structure:** Unit × Time combinations
- **Minimum:** 2 units, 2 time periods
- **Recommended:** More units and time periods for better matching

### Limitations:
- **Single treated unit:** App handles one treated unit at a time
- **No missing data handling:** App expects complete data
- **Performance:** Placebo tests can be slow with many units

### Best Practices:
1. **Pre-treatment periods:** More periods = better matching
2. **Donor pool:** Should include units similar to treated unit
3. **Predictors:** Include variables that affect outcome
4. **Placebo tests:** Always run to validate results

---

## 🚀 Running the App

### Prerequisites:
✅ All packages installed (verified)

### Start the app:
```r
# In R or RStudio:
shiny::runApp("app.R")

# Or from command line:
Rscript -e "shiny::runApp('app.R')"
```

The app will open in your default web browser at `http://127.0.0.1:XXXX`

---

## 📖 Further Reading

- **Original Paper:** Abadie, A., Diamond, A., & Hainmueller, J. (2010). Synthetic control methods for comparative case studies: Estimating the effect of California's tobacco control program. *Journal of the American Statistical Association*, 105(490), 493-505.

- **Synth Package Documentation:** `?Synth::dataprep` and `?Synth::synth` in R

- **PRD:** See `synthetic_control_prd.txt` for original requirements

---

**Last Updated:** Based on current codebase analysis
**Status:** ✅ Ready to use - all dependencies installed


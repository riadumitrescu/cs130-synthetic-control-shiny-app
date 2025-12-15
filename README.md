# Synthetic Control Analysis Web Application

**Created by Hakkei Sekine & Maria Dumitrescu, 2025**

A user-friendly Shiny web application for conducting synthetic control analysis using the Synth package by Abadie et al.

---

## Purpose

This application provides an interactive interface to perform synthetic control analysis—a method for estimating causal effects of interventions when randomized controlled trials are not feasible. It constructs a synthetic version of a treated unit as a weighted combination of control units to estimate counterfactual outcomes.

---

## Features

- **Interactive Data Upload**: Upload CSV or Excel files
- **Flexible Configuration**:
  - Map variables (unit, time, outcome, predictors)
  - Configure treatment settings (treated unit, treatment year)
  - Define special predictors with custom time windows
  - Advanced Synth parameters (optional)
- **Comprehensive Results**:
  - Path plot (treated vs. synthetic)
  - Gap plot (treatment effect over time)
  - Unit weights and predictor balance tables
  - Pre-treatment RMSPE
- **Robustness Testing**:
  - In-space placebo tests (permutation across units)
  - In-time placebo tests (backdating treatment)
- **Export Capabilities**: Download plots, tables, and full analysis reports
- **Parameter Management**: Save and load analysis configurations

---

## Data Requirements

### Input Format
Your dataset must be in **panel/longitudinal format** with the following structure:

| unit | year | outcome | predictor1 | predictor2 | ... |
|------|------|---------|------------|------------|-----|
| CA   | 1980 | 100     | 5.2        | 20         | ... |
| CA   | 1981 | 105     | 5.4        | 22         | ... |
| TX   | 1980 | 95      | 4.8        | 18         | ... |
| ...  | ...  | ...     | ...        | ...        | ... |

### Requirements
- **Multiple units**: At least 3 units (1 treated + 2+ controls)
- **Multiple time periods**: At least 3 pre-treatment periods
- **Balanced panel**: Each unit must have observations for all time periods
- **Numeric variables**: Outcome and predictors must be numeric
- **No missing values**: Complete data required for analysis periods

### Supported File Types
- CSV (`.csv`)
- Excel (`.xlsx`, `.xls`)

---

## How to Use

### 1. Upload Data
- Navigate to "1. Upload Data"
- Upload your CSV or Excel file
- Review the data preview and validation messages

### 2. Configure Analysis
- **Variable Mapping**: Select unit, time, outcome variables
- **Treatment Setup**: Choose treated unit and treatment year
- **Predictors**:
  - Select regular predictors (averaged over pre-treatment period)
  - Add special predictors with custom time windows (optional)
- **Advanced Settings** (optional):
  - Time period for predictor means
  - Time period for optimization
  - Time period for plotting
- **Save Parameters**: Save your configuration for later use

### 3. Run Analysis
- Click "Run Synthetic Control Analysis"
- Click "Show Results" to view outputs

### 4. Review Results
- **Summary Statistics**: RMSPE, donor count, treatment effect
- **Path Plot**: Observed vs. synthetic outcomes
- **Gap Plot**: Treatment effects over time
- **Tables**: Unit weights, predictor balance

### 5. Placebo Tests (Optional)
- **In-Space**: Tests each control unit as if treated
- **In-Time**: Backdates treatment to earlier periods

### 6. Export
- Download plots as PNG
- Download tables as CSV
- Generate comprehensive analysis report

---

## Outputs

### Plots
1. **Path Plot**: Shows treated unit vs. synthetic control over time
2. **Gap Plot**: Shows the difference (treatment effect) over time
3. **Placebo Plots**: Visual comparison of your treatment effect vs. placebo effects

### Tables
1. **Unit Weights**: Contribution of each donor unit to synthetic control
2. **Predictor Balance**: Comparison of predictor values (treated vs. synthetic)
3. **Placebo Test Results**: Statistical significance assessment

### Metrics
- **Pre-treatment RMSPE**: Measure of fit quality (lower is better)
- **Post-treatment Gap**: Estimated treatment effect
- **P-value** (from placebo tests): Statistical significance

---

## Advanced Settings

### Time Period Parameters

These optional settings correspond to the Synth package parameters:

1. **Time Period for Predictor Means** (`time.predictors.prior`)
   - Default: All pre-treatment years
   - Specifies when to calculate predictor averages
   - Example: 1985-1998 instead of all pre-treatment years

2. **Time Period for Optimization** (`time.optimize.ssr`)
   - Default: All pre-treatment years
   - Specifies when to minimize the loss function
   - Should match or be a subset of predictor period

3. **Time Period for Plotting** (`time.plot`)
   - Default: All available years
   - Controls which years appear in plots
   - Example: 1980-2007 instead of full dataset range

**When to use**: Match your R code exactly or focus analysis on specific periods.

---

## Limitations and Assumptions

### Method Assumptions
1. **Parallel trends**: Control units would follow similar trends as treated unit absent treatment
2. **No spillover**: Treatment doesn't affect control units
3. **No anticipation**: Units don't change behavior in anticipation of treatment
4. **Stable composition**: Control pool composition is stable

### Technical Limitations
1. **Optimization convergence**: May fail with poor data quality or insufficient controls
2. **Extrapolation**: Results unreliable if treated unit is outside control unit convex hull
3. **Pre-treatment fit**: Poor fit (high RMSPE) suggests synthetic control is unreliable
4. **Sample size**: Requires sufficient control units for valid inference

### Application Notes
- **RMSPE threshold**: No universal cutoff; compare to placebo tests
- **Placebo tests**: Essential for assessing statistical significance
- **Sensitivity**: Test different predictor specifications
- **Donor pool**: Ensure control units are comparable

---

## Interpretation Guide

### Good Fit Indicators
- ✅ Low pre-treatment RMSPE (< 10% of outcome scale)
- ✅ Close path plot match before treatment
- ✅ Balanced predictor values
- ✅ Treatment effect larger than most placebos

### Warning Signs
- ⚠️ High pre-treatment RMSPE
- ⚠️ Visible pre-treatment gap
- ⚠️ Few donor units with non-zero weights
- ⚠️ Treatment effect similar to placebo effects

---

## Example Use Case

**Research Question**: Did California's Proposition 99 (1988 tobacco tax) reduce cigarette consumption?

**Setup**:
- Unit: US states
- Time: 1970-2000
- Treatment: California in 1989
- Outcome: Per capita cigarette sales
- Predictors: GDP, beer consumption, smoking restrictions, etc.

**Expected Result**: Negative treatment effect (reduced consumption) that is larger than placebo effects.

---

## Technical Details

### R Packages Used
- **Synth**: Core synthetic control implementation
- **shiny/shinydashboard**: Web interface
- **ggplot2**: Plotting
- **dplyr**: Data manipulation
- **DT**: Interactive tables
- **openxlsx**: Excel export

### Optimization Method
Uses BFGS algorithm to minimize:
$$\sum_{t \in T_0} (Y_{1t} - \sum_{j=2}^{J+1} w_j Y_{jt})^2$$

Where:
- $Y_{1t}$: Outcome for treated unit at time $t$
- $Y_{jt}$: Outcome for control unit $j$ at time $t$
- $w_j$: Weight assigned to control unit $j$ (non-negative, sum to 1)
- $T_0$: Pre-treatment period

---

## Citation

If you use this application in your research, please cite:

**The Synth Package**:
```
Abadie, A., Diamond, A., & Hainmueller, J. (2010).
Synthetic Control Methods for Comparative Case Studies:
Estimating the Effect of California's Tobacco Control Program.
Journal of the American Statistical Association, 105(490), 493-505.
```

**This Application**:
```
Sekine, H., & Dumitrescu, M. (2025).
Synthetic Control Analysis Web Application.
https://YOUR_ACCOUNT.shinyapps.io/synthetic-control-analysis/
```

---

## Support and Issues

For questions or issues:
1. Review this README and the app's built-in help text
2. Consult the Synth package documentation: `?Synth::synth`
3. Check the original papers by Abadie et al.

---

## License

This application is provided as-is for educational and research purposes.

**Synth Package**: GPL-2 | GPL-3
**Application Code**: Created by Hakkei Sekine & Maria Dumitrescu, 2025

---

## Version

**Version 1.0** (December 2025)

Last updated: December 14, 2025

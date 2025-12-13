# Synthetic Control Shiny App – Detailed Architecture & Behavior

This document captures how the app is actually built in this repo (`app.R` plus `functions/` helpers), organized by the requested topics A–J.

## A. App Architecture
- **Framework:** R Shiny with `shinydashboard` (`app.R`), launched via `shinyApp(ui, server)`.
- **Structure:** Single Shiny file for UI/server. Logic is factored into helpers but not Shiny modules. Helpers: `functions/synth_wrapper.R` (Synth wrapper), `functions/placebo_tests.R` (placebo engines), `functions/plotting_functions.R` (plots/tables), `functions/data_functions.R` (validation utilities).
- **Deployment:** Only local run paths present; no `rsconnect` or deployment scripts. Assumed to run locally in R.
- **Packages:** Uses `Synth`, `shiny`, `shinydashboard`, `DT`, `ggplot2`, `readxl`, `dplyr`, `tidyr`, `rmarkdown`.
- **State persistence:** Browser-side IndexedDB + localStorage (JS in `app.R`) for saving settings per file and saving analysis results history.

## B. Data Requirements + Loading
- **Accepted structure:** Generic panel data expected to contain at least unit id, time, outcome; predictors optional. No hard-coded column names—user maps columns.
- **Formats:** CSV (`read.csv`) and Excel (`readxl::read_excel` for `.xlsx`/`.xls`) in `app.R`.
- **Validation:** `validate_panel_data()` only checks: data exists, ≥3 columns, warns on empty columns, warns if rows < 10. No enforcement of unique unit–time pairs, numeric time, or missing-value completeness.
- **Panel completeness:** Not enforced. Helper `check_panel_balance()` exists but is not called in UI; app proceeds even if panel is unbalanced or has missing cells.
- **Pre/post sufficiency:** Downstream checks in `run_synth_analysis()` require ≥2 pre-treatment periods and ≥2 donor units; otherwise errors are shown via notifications.

## C. Variable Mapping UI
- **User selections:** `selectInput` for Unit Identifier (`unitVar`), Time Variable (`timeVar`), Outcome Variable (`outcomeVar`). `checkboxGroupInput` for Predictors (`predictorVars`). Special predictors configured via dynamic UI (`specialPredictorsUI`).
- **Auto-detect:** Choice lists auto-populated from uploaded data column names. No auto-guessing of defaults.
- **Validation/warnings:** Readiness/reactive checks ensure required selections exist before analysis. No explicit type checks (e.g., numeric time) beyond Synth errors. Notifications appear if required fields missing when saving or running.

## D. Treatment Setup
- **Inputs:** Treated unit from unique unit values; treatment year via `numericInput` whose min/max is set from observed time range.
- **Donor pool:** Automatically all units except the treated unit (used in summaries and passed to Synth).
- **Restrictions:** Year bounded to observed min/max. No explicit guard on very short pre-periods; Synth wrapper enforces ≥2 pre-periods.
- **Summary:** UI shows donor count, pre/post period counts, and treatment year once configured.

## E. Synthetic Control Engine
- **Implementation:** Fully relies on official `Synth` package. `run_synth_analysis()` calls `dataprep()` then `synth(method = "BFGS")`, then `synth.tab()`.
- **Predictor handling:** `predictors.op = "mean"` for regular predictors; special predictors list constructed as `list(var, time_range, op)`.
- **Weights:** Inherit Synth constraints—non-negative, sum to one across donors. Display filters weights > 0.001 but retains all weights internally.
- **Outputs computed:** Outcome path (treated, synthetic, gap, post_treatment flag), donor weights, predictor balance table, RMSPE (pre-treatment), donor list, convergence flag. Average treatment effect for display is computed in server from post-period gap mean.
- **Defaults:** If no regular/special predictors chosen, defaults to using the outcome variable at every pre-treatment period as special predictors.

## F. Predictors
- **Regular predictors:** Any user-selected variables; aggregated as pre-treatment means.
- **Special predictors:** User-defined variable plus start/end years (pre-period windows). UI fixes op to mean; config supports custom op but not exposed. Multiple windows supported; stored as `special_predictors` list in reactive values and persisted.
- **Fallback behavior:** With zero predictors selected, app builds special predictors from outcome lags at each pre-treatment time automatically (Synth default behavior in this app).

## G. Visualizations & Tables
- **Plots:** Actual vs Synthetic line plot, Gap (treatment effect) plot, Donor weight barplot (`functions/plotting_functions.R`); placebo plots for legacy and new placebo tests.
- **Tables:** Pre-treatment balance table (DT), outcome path table, placebo summaries, history table.
- **Value boxes:** RMSPE, donor unit count, average post-treatment effect.
- **Exports:** CSV downloads for outcome results, weights, balance; PNG downloads for main/gap/weights plots; PDF report generation via `rmarkdown` including plots, weights table, placebo section if available.

## H. Placebo Tests
- **Legacy placebo (Results tab “Legacy Placebo Test”):** `run_synth_placebo()` reruns Synth treating each unit (including treated) as if treated at the same year; gap plot and ranking p-value from RMSPE ratio.
- **In-space placebo (Placebo tab):** `in_space_placebo()` applies Synth to each donor unit; computes pre/post RMSPE, ratio, p-value rank; outputs gap plot, summary table, and p-value value box. Requires ≥2 donors and ≥2 pre periods.
- **In-time placebo:** `in_time_placebo()` reruns Synth on the treated unit using multiple fake treatment dates before the true treatment. Ensures minimum pre/post buffers (defaults: ≥3 pre, ≥2 post). Produces gap plot and summary table. Fake times auto-generated evenly in pre-period, snapped to observed times.
- **Distributions:** No histogram/density plot of effects; inference reported via p-value (in-space) and tables/plots for fake times.

## I. Performance Characteristics
- **Single run:** Synth on moderate panels is fast; pre-checks prevent tiny panels (but only a warning for <10 rows). Expect near-instant on small datasets, seconds on medium.
- **Placebos:** Scale linearly with units (in-space) or fake dates (in-time); each reruns full Synth, so many units/dates can be slow. Progress notifications provided; no parallelization or caching.
- **Stability:** Extensive try/catch around Synth calls to surface errors as notifications; filters ensure pre-period minimums to reduce failures.

## J. User Experience Decisions
- **Flow:** Sidebar steps mirror workflow (Upload → Configure → Results → Placebo → Export → History) to reduce friction.
- **Persistence:** IndexedDB/localStorage auto-save and restore per file; explicit “Save Parameters” button with spinner feedback; “Settings Restored” banner when applied.
- **Guidance:** Readiness banners and treatment summaries; predictor summary text; warnings when config incomplete; success alerts when analysis/placebos finish.
- **Layout:** Color-coded `shinydashboard` boxes, collapsible sections (balance/outcome data/history details), full-width run button, and clear call-to-action buttons for placebo and exports.
- **History:** Automatic saving of completed analyses; history table with load/view/delete and summaries enables quick reuse without re-uploading.

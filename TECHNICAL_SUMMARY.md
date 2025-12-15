# Technical Summary

1. App structure: Multi-file. Main UI/server in `app.R` sourcing helpers; supporting scripts include `functions/data_functions.R`, `functions/synth_wrapper.R`, `functions/plotting_functions.R`, `functions/placebo_tests.R`, `functions/synthetic_control_bulletproof.R`, `functions/synthetic_control_functions.R`, `functions/synthetic_control_robust.R`, `functions/synthetic_control_simple.R`.
2. `/functions` folder: `data_functions.R`, `synth_wrapper.R`, `plotting_functions.R`, `placebo_tests.R`, `synthetic_control_bulletproof.R`, `synthetic_control_functions.R`, `synthetic_control_robust.R`, `synthetic_control_simple.R`.
3. Reactivity: `reactive()` yes; `observeEvent()` yes; `reactiveValues()` yes.
4. User settings storage: Browser-side IndexedDB with localStorage fallback for per-file settings and parameter configs; in-session state held in Shiny reactive values (no server file persistence).
5. Upload validation: `validate_panel_data()` checks data exists, at least 3 columns, flags empty columns, and notes very small row counts; no checks for required column names, minimum pre-treatment years, or numeric outcome types beyond that.
6. Lag predictors: Yes. If no predictors or special predictors are chosen, default special predictors are built from outcome lags at each pre-treatment period.
7. Placebo tests: Via dedicated functions (`in_space_placebo()`, `in_time_placebo()`, `synth_runner()` in `functions/placebo_tests.R`) that loop over units/donors.
8. RMSPE: Calculated manually from gap series (pre-period RMSPE and post/pre RMSPE ratios computed directly), not taken automatically from Synth outputs.
9. Plotting: Uses `ggplot2` (see `functions/plotting_functions.R` and `functions/placebo_tests.R`); no base R plots.
10. Exports: CSV downloads for main results, donor weights, predictor balance, and zipped data bundles; PNG downloads for individual and bundled plots; no PDF or report export.
11. Warnings/errors: Uses Shiny `req()` guards and `showNotification()` for warnings/errors; no `validate()` calls observed.
12. History: No persisted run history of analyses; only settings/parameters are cached in the browser, and results are exported manually.

# Count Regression Toolkit

A Shiny web application for fitting, diagnosing, and interpreting count regression models. Upload your data, select predictors, and receive model output, assumption checks, and plain-language interpretations — all in one interface.

---

## Features

- Six count regression models: Poisson, Quasi-Poisson, Negative Binomial, ZIP, ZINB, and Tweedie
- Comprehensive diagnostics: RQR plots, influence diagnostics, VIF, and goodness-of-fit tests
- Interaction analysis: simple slopes, estimated marginal means, emtrends, contrasts, and Johnson-Neyman plots
- Automated plain-language interpretation of model results
- Downloadable PNG exports for all plots

---

## Requirements

Install the following R packages before running the app:

install.packages(c(
  "shiny", "shinyWidgets", "tidyverse", "ggeffects",
  "MASS", "pscl", "statmod", "tweedie", "COMPoissonReg",
  "emmeans", "magrittr", "parameters", "broom", "car",
  "GGally", "patchwork", "DHARMa", "glue"
))

---

## Sample Datasets

The app includes four built-in datasets from class that can be loaded directly from the sidebar without uploading a file:

| Dataset | Description |
|---------|-------------|
| Armadillo (McMillan et al.) | Armadillo counts by hunter age and trek frequency (n = 38) |
| Monkey (McMillan et al.) | Monkey hunting counts from the same study |
| Greenberg 26 | Count data from Greenberg (n = 26) |
| CS Replication | Replication dataset used in class |

---

## Running the App

From the count-app/ directory:

shiny::runApp(".")

Or from the terminal:

Rscript -e "shiny::runApp('.')"

---

## Vignette

A worked example using the app is available in `vignette.qmd`. It walks through a full count regression analysis of the McMillan Ache Armadillo dataset, including model selection, diagnostics, and interaction analysis using the Negative Binomial model.

To render it locally, open `vignette.qmd` in RStudio and click **Render**, or run:

quarto render vignette.qmd

---

## Running the Tests

Unit tests are written with shinytest2 and live in tests/testthat/. To run them, open R from the count-app/ directory and run...

shinytest2::test_app(".")

The test suite covers: app load, five model types (Poisson, Quasi-Poisson, Negative Binomial, ZIP, ZINB), single predictor, interaction terms, and interaction without a main effect.

---

## Sidebar Controls

| Input | Description |
|-------|-------------|
| Upload CSV File | Load your dataset in .csv format |
| Select Response Variable | Choose the count outcome variable |
| Select Predictor Variable(s) | Choose predictors; interaction terms (e.g. Age × Treks) can be selected directly |
| Select Offset Variable | Optional — for rate models (e.g. log population exposure) |
| Scale Variable(s) | Optional — z-score standardize continuous predictors before fitting |
| Select Model to Fit | Choose from six count regression models |
| Fit Model | Fit the selected model |

---

## Tabs

### Data Preview
- Displays the first rows of the uploaded dataset

### Data Summary
- Summary statistics: mean, variance, min, max, proportion of zeros
- Count distribution histogram
- Pairwise scatterplot matrix with model-appropriate smooths
- Coefficient correlation matrix from the fitted model

### Diagnostics
- RQR Plot: Randomized quantile residual diagnostics with checks for normality, dispersion, excess zeros, and mean-variance relationship; includes model recommendation
- VIF Table: Generalized VIF for multicollinearity (supports interaction models)
- Assumption Checks: Eight-point Poisson assumption evaluation — overdispersion, linearity, zero inflation, events-per-predictor, goodness-of-fit, and more
- Influence Diagnostics: Leverage, Cook's distance, and DFFITS plots with reference thresholds
- Zero-Inflation Test: DHARMa-based simulation test for excess zeros (shown for non-zero-inflated models as a model selection check)

### Interpretation
- Incidence Rate Ratio (IRR) table with exponentiated coefficients, 95% CIs, and p-values
- Plain-language interpretation of:
  - Main effects
  - Interaction terms
  - Zero-inflation component (ZIP/ZINB only)

### Interaction
Requires at least one interaction term in the model.

- Simple Slopes Plot: Predicted outcome across the focal predictor at low (−1 SD) and high (+1 SD) moderator levels
- Estimated Marginal Means: EMM at specific predictor/moderator combinations with interpretation
- Contrasts of Marginal Means: Pairwise comparisons between EMM cells
- Marginal Effects (emtrends): Slope of the focal predictor at each moderator level
- Contrasts of Marginal Effects: Tests of differences in slopes across moderator levels
- Johnson-Neyman Plot: Region of significance along the moderator's range

---

## Supported Models

| Model | Use Case |
|-------|----------|
| Poisson | Standard count data with equidispersion |
| Quasi-Poisson | Overdispersed counts (adjusts standard errors) |
| Negative Binomial | Overdispersed counts (estimates dispersion parameter) |
| Zero-Inflated Poisson (ZIP) | Excess zeros + Poisson-distributed counts |
| Zero-Inflated Negative Binomial (ZINB) | Excess zeros + overdispersed counts |
| Tweedie | Compound Poisson-Gamma; power parameter estimated via profile likelihood |

---

## File Structure

- `app.R` — Main Shiny application
- `R/`
  - `poisson.R` — Poisson model fitting and IRR table
  - `quasi_poisson2.R` — Quasi-Poisson model fitting
  - `nb.R` — Negative Binomial model fitting
  - `zip.R` — ZIP model fitting
  - `zinb.R` — ZINB model fitting
  - `tweedie.R` — Tweedie model fitting with power estimation
  - `compois.R` — COM-Poisson model fitting
  - `plotRQR.R` — Randomized quantile residual plots
  - `plotInfluence.R` — Influence diagnostic plots
  - `plotResiduals.R` — Residual diagnostic plots
  - `pairwise_plots.R` — Pairwise scatterplot matrix
  - `assumptions.R` — Poisson assumption checks
  - `emmeans.R` — EMM, emtrends, and contrast tables
  - `interpretation.R` — Plain-language model interpretation
  - `johnson_neyman.R` — Johnson-Neyman floodlight analysis
  - `corr_matrix.R` — Coefficient correlation matrix
  - `summary.R` — Count response summary statistics

---

## AI Appendix

### UI / Shiny Structure

- **Prompt:** We used Claude to help with the structure and syntax of Shiny UI components, since we were unfamiliar with the shiny and shinyWidgets packages.  
  **Outcome:** AI provided the structure (sidebar, tabsetPanel, renderUI patterns). We designed the content of each tab and what inputs/outputs were needed. AI helped us express that in Shiny syntax we hadn't used before.

### Integrating External R Files

- **Prompt:** We used Claude to help understand how to source and integrate separate R files (e.g., poisson.R, emmeans.R, johnson_neyman.R, etc) into the main app.R.  
  **Outcome:** AI explained how `source()` works within a Shiny app and how to pass reactive data into external functions. We wrote the logic and statistical content of each module ourselves; AI helped with the connection between them.

### Debugging

- **Prompt:** We used Claude throughout development to debug error messages in R and Shiny, particularly around reactive expressions, NULL outputs, and model fitting edge cases.  
  **Outcome:** AI helped identify the source of specific errors. In each case we reviewed the fix and made sure we understood why it worked before applying it. Some suggestions were rejected or modified when they didn't fit the context of the app.

### Diagnostics / App Development

- **Prompt:** We asked Claude to go through all R files for edge cases where the code might crash or behave unexpectedly.  
  **Outcome:** Several issues were flagged (duplicate output definition, a non-existent input reference, interaction term handling in VIF). We reviewed each one and applied the fixes we agreed were valid. We rejected suggestions that added unnecessary complexity to the functionality.

### Unit Testing

- **Prompt:** We used Claude to help set up shinytest2 for our Shiny app and debug why tests kept failing.  
  **Outcome:** The root issue (Shiny suspending outputs in hidden tabs) was identified through back-and-forth debugging. We implemented the `outputOptions(..., suspendWhenHidden = FALSE)` fix after understanding why it was necessary. We wrote the structure and logic of all 9 tests ourselves.


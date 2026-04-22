# Implementation Plan: Ship-Readiness & Improvements

**Repo:** `KristofM854/Shiny-App-Competitive-Bioassays`
**Target:** v1.0.0 release, Zenodo DOI, SoftwareX/JOSS paper
**Status:** Draft for Claude Code implementation

This plan consolidates outcomes from a deep-dive review of the codebase. Items are grouped by priority: ship-blockers must be done before tagging v1.0.0; high-value improvements are features that meaningfully improve the product; medium-value items are quality-of-life refactors; future items are out of scope for v1.0 but tracked for visibility.

Each task has a status checkbox, a scope description, and acceptance criteria. Tasks that touch multiple files list them all. Tasks that are manual (not code) are marked **[MANUAL]** and are not for Claude Code to execute.

---

## SHIP-BLOCKERS (required for v1.0.0)

### B1. Delete `run_analysis_modular.R`

- [x] Remove the file `run_analysis_modular.R` from the repo root
- [x] Update the README `Quick Start` section so it shows only two entry points:
  - `shiny::runGitHub("Shiny-App-Competitive-Bioassays", "KristofM854")` for remote use
  - `shiny::runApp("app.R")` for local use after cloning
- [x] Remove any references to `run_analysis_modular.R` in other documentation

**Rationale:** The file is obsolete. `app.R` handles standalone mode (output directory creation, environment variables, folder picker) since the modular refactor. The rstudioapi-based path-discovery mechanism is fragile outside RStudio and produces a misleading "please run the script again" error. No downstream tooling depends on it.

**Acceptance:** The file is gone, the README shows only the two supported entry points, and `grep -r "run_analysis_modular" .` returns no hits outside of git history.

---

### B2. Add MIT License

- [ ] Create `LICENSE` at repo root with the MIT License text:

```
MIT License

Copyright (c) 2026 Kristof Moeller and Arnold Molina Porras

Permission is hereby granted, free of charge, to any person obtaining a copy
of this software and associated documentation files (the "Software"), to deal
in the Software without restriction, including without limitation the rights
to use, copy, modify, merge, publish, distribute, sublicense, and/or sell
copies of the Software, and to permit persons to whom the Software is
furnished to do so, subject to the following conditions:

The above copyright notice and this permission notice shall be included in all
copies or substantial portions of the Software.

THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE
AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE
SOFTWARE.
```

- [ ] Add a `## License` section at the bottom of `README.md`:

```markdown
## License

This project is licensed under the MIT License — see the [LICENSE](LICENSE) file for details.

Copyright (c) 2026 Kristof Moeller and Arnold Molina Porras.
```

- [ ] Add a short license badge near the top of the README, right after the title:

```markdown
[![License: MIT](https://img.shields.io/badge/License-MIT-yellow.svg)](https://opensource.org/licenses/MIT)
```

**Note to Kristof:** Confirm with Arnold Molina Porras that he's comfortable being listed as joint copyright holder before tagging v1.0.0. Confirm with IAEA that this is acceptable. These checks are manual and not for Claude Code.

**Acceptance:** `LICENSE` file exists with exact text above, README has License section and badge, both display correctly on GitHub.

---

### B3. Add screenshot placeholders to README **[MANUAL screenshots, code for placeholders]**

- [ ] Create directory `docs/screenshots/`
- [ ] Add a placeholder `.gitkeep` file so the empty directory is tracked
- [ ] Update `README.md` to include image references at the appropriate points. Insert a `## Screenshots` section immediately after the `## The 5-step workflow` section. The section should include six images, each with a caption:

```markdown
## Screenshots

### 1. Configuration — Quick Start and assay setup
![Configuration tab](docs/screenshots/01_configuration.png)

Pick an assay type and load a preset, or configure manually. Each Quick Start preset has two buttons: "Instant demo" loads example data so you can see a full report in seconds, and "Configure manually" sets up the assay type and layout so you can upload your own plate reader file.

### 2. Plate Layout — four synchronized matrices
![Plate Layout tab](docs/screenshots/02_plate_layout.png)

Define sample type, sample ID, dilution factor, and replicate groups in parallel. Presets, import from CSV/Excel, and undo/redo all supported.

### 3. Upload & Preview — auto-detected heatmap
![Upload tab](docs/screenshots/03_upload_heatmap.png)

The app auto-detects plate regions in `.xlsx`, `.csv`, and `.txt` files. The heatmap preview lets you confirm the correct region was detected before running the analysis.

### 4. Analysis Settings — advanced statistical options
![Analysis Settings tab](docs/screenshots/04_analysis_settings.png)

Choose regression weighting (including data-driven Auto option), confidence interval method, outlier detection strategy, and quantification range.

### 5. Report — executive summary and dose-response curve
![Report — summary and DRC](docs/screenshots/05_report_summary.png)

Every report opens with a colour-coded executive summary, a QC traffic-light table, and the fitted dose-response curve.

### 6. Report — quantified samples with confidence intervals
![Report — sample results](docs/screenshots/06_report_results.png)

Per-replicate-group mean concentrations with 95% confidence intervals, CV, range flags, and (for ELISA with tissue weights) pg/g tissue.
```

- [ ] For Kristof to do manually: take the six screenshots and place them at:
  - `docs/screenshots/01_configuration.png`
  - `docs/screenshots/02_plate_layout.png`
  - `docs/screenshots/03_upload_heatmap.png`
  - `docs/screenshots/04_analysis_settings.png`
  - `docs/screenshots/05_report_summary.png`
  - `docs/screenshots/06_report_results.png`
- [ ] Target resolution ~1400-1600px wide, PNG format, <500KB each if possible.

**Acceptance:** README renders with six image references. When Kristof adds the PNG files, they appear inline.

---

### B4. Set up GitHub Actions CI

- [ ] Create `.github/workflows/R-CMD-check.yml` with a minimal workflow that:
  - Runs on push to `main` and on pull requests
  - Uses `ubuntu-latest`
  - Installs R 4.3
  - Installs system dependencies needed by `drc`, `readxl`, `rmarkdown` (libxml2, libcurl, libssl, pandoc)
  - Caches the R library
  - Installs all packages listed in `global.R` plus `testthat`, `shinytest2`, `chromote`
  - Runs `testthat::test_dir("tests/testthat")` and fails the workflow if any test fails

Starter template:

```yaml
name: R-tests

on:
  push:
    branches: [main]
  pull_request:
    branches: [main]

jobs:
  R-CMD-check:
    runs-on: ubuntu-latest
    env:
      GITHUB_PAT: ${{ secrets.GITHUB_TOKEN }}
      R_KEEP_PKG_SOURCE: yes
    steps:
      - uses: actions/checkout@v4
      - uses: r-lib/actions/setup-r@v2
        with:
          r-version: '4.3'
          use-public-rspm: true
      - uses: r-lib/actions/setup-pandoc@v2
      - uses: r-lib/actions/setup-r-dependencies@v2
        with:
          packages: |
            any::testthat
            any::shinytest2
            any::chromote
          extra-packages: |
            any::shiny
            any::drc
            any::readxl
            any::rmarkdown
            any::knitr
            any::dplyr
            any::ggplot2
            any::plotly
            any::jsonlite
            any::rhandsontable
            any::kableExtra
      - name: Run tests
        run: Rscript -e 'testthat::test_dir("tests/testthat", stop_on_failure = TRUE)'
```

- [ ] Add a badge to the top of README:

```markdown
[![R-tests](https://github.com/KristofM854/Shiny-App-Competitive-Bioassays/actions/workflows/R-CMD-check.yml/badge.svg)](https://github.com/KristofM854/Shiny-App-Competitive-Bioassays/actions/workflows/R-CMD-check.yml)
```

**Note:** The workflow will run shinytest2 tests added in B5. If shinytest2 tests aren't committed yet, the workflow still passes with just the existing testthat tests.

**Acceptance:** A push to a branch triggers the workflow; it completes green; the badge renders in README.

---

### B5. Add `shinytest2` happy-path tests

- [ ] Create `tests/testthat/test-shinytest-rba.R`:

```r
# End-to-end happy path: RBA Saxitoxin with Instant Demo button
# Loads example data, advances through all 5 tabs, generates a report,
# asserts that an HTML report file is written.

test_that("RBA Instant Demo produces a complete report", {
  skip_on_cran()
  skip_if_not_installed("shinytest2")

  app <- shinytest2::AppDriver$new(
    app_dir = testthat::test_path("../.."),  # app.R is at repo root
    name = "rba-instant-demo",
    load_timeout = 30000,
    timeout = 30000
  )
  on.exit(app$stop())

  # Dismiss welcome modal if present
  tryCatch(app$click(selector = ".modal-footer button"), error = function(e) NULL)
  app$wait_for_idle(500)

  # Click the RBA Saxitoxin "Instant demo" button
  app$click("qs_rba_stx_demo")
  app$wait_for_idle(2000)

  # Should now be on upload tab with data loaded; heatmap visible
  app$wait_for_value(output = "plate_heatmap", timeout = 15000)

  # Advance to analysis tab
  app$click("next_to_analysis")
  app$wait_for_idle(500)

  # Advance to report tab
  app$click("next_to_report")
  app$wait_for_idle(500)

  # Generate report
  app$click("convert")

  # Report generation can be slow (DRC fits + knitr render)
  Sys.sleep(5)
  app$wait_for_idle(timeout = 180000)

  # Check that an HTML report was written to the output dir
  output_dir <- app$get_value(export = "output_dir")
  if (is.null(output_dir)) {
    # Fallback: session-scoped output dir isn't exported; use env var
    output_dir <- Sys.getenv("RBA_OUTPUT_DIR")
  }

  expect_true(dir.exists(output_dir))
  html_files <- list.files(output_dir, pattern = "\\.html$", full.names = TRUE)
  expect_gt(length(html_files), 0)

  # Basic sanity: file is non-trivially sized
  expect_gt(file.info(html_files[1])$size, 10000)
})
```

- [ ] Create `tests/testthat/test-shinytest-elisa.R` with analogous logic using `qs_elisa_cortisol_demo` and asserting that the Cortisol report renders. Include an assertion that `control_summary` appears in the report (grep the HTML output for "Blank average" text).

- [ ] Create `tests/testthat/test-shinytest-multiwavelength.R`:
  - Upload a synthetic multi-wavelength fixture (see below)
  - Assert that two per-wavelength CSVs are written
  - Assert that the Bland-Altman plot section appears in the report

- [ ] Create the multi-wavelength fixture at `tests/testthat/fixtures/multiwave_synthetic.csv`. The file should contain two "Raw Data (450)" and "Raw Data (630)" blocks with labeled A-H rows and 12 columns of plausible absorbance values. Generate using a script `tests/testthat/fixtures/generate_multiwave.R` so future updates are reproducible.

- [ ] **Performance note:** Add `app$set_window_size(width = 1400, height = 900)` before interactions to ensure consistent layouts.

- [ ] Update `.github/workflows/R-CMD-check.yml` to install Chrome for shinytest2:

```yaml
      - name: Install Chrome
        uses: browser-actions/setup-chrome@v1
```

Add this step before the `Run tests` step.

**Acceptance:** All three shinytest2 tests pass locally on Kristof's machine with Chrome installed, and on GitHub Actions after B4 is in place.

---

### B6. Golden-number regression test

- [ ] Create `tests/testthat/test-report-numbers.R` that renders the unified RBA template against the `rba_stx_example.csv` fixture and asserts specific numeric outputs. This catches silent statistical regressions where a report still generates but the numbers have shifted.

- [ ] The test should:
  - Build the necessary config JSON files in `tempdir()` to simulate what the app writes
  - Copy the example CSV as `long_data_output.csv` (after running `matrix_to_long()`)
  - Call `rmarkdown::render()` with the test fixture
  - Parse the generated `model_stats.json` and `unknown_results_summary.csv`
  - Assert R² is within ±0.001 of a stored expected value (say, 0.998)
  - Assert IC50 is within ±5% of expected
  - Assert the number of quantified samples matches

```r
test_that("RBA golden numbers match reference", {
  skip_on_cran()
  skip_if_not_installed("rmarkdown")

  # Setup fixture directory
  fixture_out <- file.path(tempdir(), "golden-rba")
  dir.create(fixture_out, showWarnings = FALSE, recursive = TRUE)

  # ... (build config JSONs, copy CSV, render template)

  stats <- jsonlite::fromJSON(file.path(fixture_out, "model_stats.json"))
  expect_lt(abs(stats$r_squared - 0.998), 0.002)
  expect_lt(abs(stats$ic50 - 5.2e-9) / 5.2e-9, 0.05)

  results <- read.csv(file.path(fixture_out, "unknown_results_summary.csv"))
  expect_equal(nrow(results), 24)  # 24 sample replicate groups in example
})
```

The exact expected values need to be captured from one known-good run; include a commented-out "capture reference values" block at the top of the test that Kristof can uncomment to regenerate expected values after intentional changes.

**Acceptance:** The test passes against the current code; intentional stat-changing commits require updating the expected values as part of the PR.

---

## HIGH-VALUE IMPROVEMENTS (v1.0 or v1.1)

### H1. Quick Start redesign — 2×3 grid with Instant Demo and Configure Manually

**Scope:** Restructure Tab 1's Quick Start section from the current three single-purpose buttons into a 2×3 grid. Rows = action type (Instant demo / Configure manually); Columns = preset (RBA Saxitoxin / ELISA Cortisol / ELISA Custom). Total six buttons, with the exception that ELISA Custom has no "Instant demo" variant (it's inherently a manual-config template with no example data).

**Files to modify:** `app.R`, `server_config.R`, `i18n.R`, `server_common.R`

**Sub-tasks:**

- [ ] In `app.R`, replace the current Quick Start `fluidRow` (lines ~90-110) with a 2×3 grid layout. Use a header for each row ("🚀 Instant demo" / "⚙️ Configure manually"). The ELISA Custom "Instant demo" slot should be an empty placeholder div or a short message ("No example data — use Configure manually"), not an active button.

- [ ] Button IDs:
  - Row 1 (Instant demo): `qs_rba_stx_demo`, `qs_elisa_cortisol_demo`, (empty slot)
  - Row 2 (Configure manually): `qs_rba_stx_manual`, `qs_elisa_cortisol_manual`, `qs_elisa_custom_manual`

- [ ] In `server_config.R`, create a helper function `load_preset(assay, analyte, load_example = FALSE)` that:
  - Updates the assay type and analyte selectors
  - Populates the matrix state (type, id, dilution, replicate) using either preset RDS or generator functions
  - If `load_example = TRUE`: calls `parse_plate_file()` on the matching example file, pushes results into `shared$matrix_measresults()` (and `shared$rv$wavelength_plates` if multi-wave), and navigates to `tab_upload`. Wraps the file read in `tryCatch()` — on failure, show a notification ("Example file not found — please upload your own data") and still navigate to `tab_upload`.
  - If `load_example = FALSE`: navigates to `tab_layout` for manual configuration.

- [ ] Replace the current `qs_rba_stx`, `qs_elisa_cortisol`, `qs_elisa_custom` observers with six observers (five for Custom) calling the appropriate `load_preset()` variant.

- [ ] Example file paths (hardcoded, relative to app root):
  - RBA Saxitoxin: `examples/rba_stx_example.csv`
  - ELISA Cortisol: `examples/elisa_cortisol_example.csv`
  - ELISA Custom: no example file

- [ ] Update notifications:
  - Demo buttons: "RBA Saxitoxin example loaded. Click Generate Report to see the full workflow." (and analogous for ELISA)
  - Manual buttons: "RBA Saxitoxin preset loaded. Configure your plate layout and upload your data." (and analogous)

- [ ] Add new i18n keys for the Quick Start section:
  - `quickstart_demo_row_label` = "🚀 Instant demo"
  - `quickstart_manual_row_label` = "⚙️ Configure manually"
  - `quickstart_no_demo_available` = "No example data — use Configure manually"
  - Six new button label keys (reusing existing preset labels where possible)
  - Six new notification message keys
  - Spanish translations for all

- [ ] In `server_common.R` language observer, add `updateActionButton()` calls for all six new button IDs.

**Acceptance:** The Tab 1 Quick Start section shows a 2×3 grid. Clicking any "Instant demo" button loads example data and navigates to Tab 3 with the heatmap visible. Clicking any "Configure manually" button sets up the assay type and navigates to Tab 2. Language toggle correctly updates all new labels.

---

### H2. Auto option for regression weighting

**Scope:** Add a fourth option to the regression weighting checkbox in Tab 4: "Auto (data-driven)". When selected, the app fits an initial unweighted LL.4 model, runs `assess_heteroscedasticity()`, and automatically picks the appropriate weighting (none / 1/Y / 1/Y²) based on the diagnostic result.

**Files to modify:** `app.R`, `reports/unified_analysis_template.Rmd`, `i18n.R`, `server_report.R` (or new analysis_pipeline.R after M2)

**Sub-tasks:**

- [ ] In `app.R`, add a fourth choice to the `regression_weight` `checkboxGroupInput`:
  - Value: `"auto"`
  - Label: "Auto (data-driven)"
  - Default: keep current default (`"none"`)

- [ ] In `server_report.R` or new `server_common.R` observer: when the user checks "Auto", uncheck all other options. When the user checks any other option, uncheck "Auto". Use a `observeEvent(input$regression_weight, ...)` with a helper flag to avoid infinite loops.

- [ ] In `reports/unified_analysis_template.Rmd` `model-fitting` chunk (or the extracted `fit_all_models()` function after M1), handle `"auto"`:
  - Fit unweighted LL.4 as the initial model
  - Call `assess_heteroscedasticity()` on it
  - Map to a weighting choice:
    - `variance_ratio < 3` OR `p_value >= 0.05` → unweighted
    - `variance_ratio 3-10` OR `p_value < 0.05` (moderate effect) → 1/Y
    - `variance_ratio > 10` OR `p_value < 0.01` with strong effect → 1/Y²
  - Refit with the chosen weighting
  - Set `selected_weights <- c(chosen_weight)` so downstream code treats it as single-weight
  - Store the auto-selection result for reporting

- [ ] Edge case: if the initial unweighted fit fails (falls back to LL.3 or interpolation), default to 1/Y² without a diagnostic and log a note.

- [ ] In the report, add a paragraph in the model-fitting section (or the weighting suitability section) that explicitly documents the auto-decision:

  > "Auto-weighting selected **1/Y²** based on the Brown-Forsythe test
  > (F = 7.3, p = 0.002). Residual variance increased strongly across
  > concentration levels, justifying down-weighting of high-response
  > points."

- [ ] Add i18n keys for the auto option:
  - `weight_auto` = "Auto (data-driven)"
  - `weight_auto_help` = "Picks unweighted, 1/Y, or 1/Y² based on a Brown-Forsythe test on an initial unweighted fit."
  - `weight_auto_decision` = "Auto-weighting selected **%s** based on %s (statistic = %.3f, p = %.4f)."
  - `weight_auto_fallback` = "Auto-weighting defaulted to 1/Y² because the initial unweighted fit could not be assessed."
  - Spanish translations for all

**Acceptance:** Selecting "Auto" and generating a report produces a DRC fit with an automatically chosen weighting, and the report contains a paragraph documenting which weighting was chosen and why. The auto option is mutually exclusive with the manual options.

---

### H3. One-line multi-wavelength summary at top of report

**Scope:** Add a prominent one-line conclusion at the top of the multi-wavelength report executive summary that tells the user whether the wavelengths agreed, without requiring them to scroll to the Lin's CCC section buried at the bottom.

**Files to modify:** `reports/multiwavelength_analysis_template.Rmd`

**Sub-tasks:**

- [ ] In `reports/multiwavelength_analysis_template.Rmd`, after the wavelength-summary chunk and before the detailed wavelength reports, compute the Lin's CCC for the primary vs secondary wavelength pair.

- [ ] Add a one-line result immediately after the overview:

  > "**Wavelength agreement:** 450nm and 630nm show **excellent agreement** (CCC = 0.994). Either wavelength can be used interchangeably for quantification."

  Or, if agreement is poor:

  > "**Wavelength agreement:** 450nm and 630nm show **poor agreement** (CCC = 0.82). Investigate wavelength-specific effects before interchangeable use."

- [ ] Use the same interpretation buckets as the detailed CCC section (> 0.99 excellent, 0.95-0.99 good, 0.90-0.95 moderate, < 0.90 poor).

- [ ] For 3+ wavelengths, report the minimum pairwise CCC across all pairs and the corresponding pair name: "Worst-pair agreement across 3 wavelengths: 450nm vs 630nm (CCC = 0.92, moderate)."

- [ ] Wrap in `tryCatch()` — if the CCC computation fails (too few paired samples), fall back to "Wavelength agreement could not be computed (insufficient paired data)." rather than crashing the report.

- [ ] Add i18n keys for all the summary messages and their Spanish translations.

**Acceptance:** A multi-wavelength report's executive summary contains a prominent agreement statement within the first three lines.

---

### H4. Compact + detailed report generation

**Scope:** Generate both a compact (3-4 page) and detailed (full) report in every analysis run, using the same statistical computations (computed once). The compact version is the default download; the detailed version is accessible via a secondary link.

**Prerequisite:** M1 (Rmd splitting) must be at least partially done. Until then, implement compact mode as a `compact` param in the existing template. After M1, implement compact as a separate thin orchestration template.

**Files to modify:** `app.R`, `server_report.R`, `report_pipeline.R`, `reports/unified_analysis_template.Rmd` (or new `reports/unified_analysis_template_compact.Rmd`), `i18n.R`

**Sub-tasks (pre-split, stopgap):**

- [x] Add `compact` to the `params` block of `unified_analysis_template.Rmd` (default `FALSE`).

- [x] Wrap optional sections with `if (!params$compact) { ... }`. Sections to suppress in compact mode:
  - Exclusion audit
  - Weighting comparison (when multiple weights selected)
  - Model stability assessment
  - Back-calculation table (LLOQ/ULOQ section keeps the determined LLOQ/ULOQ value but skips the per-standard table)
  - Heteroscedasticity diagnostic
  - Parallelism assessment
  - Plate positional QC
  - Tissue normalization traceability (keep the final pg/g numbers, skip the worked example and formula)

- [x] Keep in compact mode:
  - Executive summary
  - QC traffic light
  - Dose-response curve plot
  - Model parameters (4PL coefficients)
  - Sample results table
  - Sample variability boxplot
  - DRC-with-samples plot

**Sub-tasks (always, regardless of split status):**

- [x] In `render_reports()` in `report_pipeline.R`, change the loop to render twice per format: once with `compact = TRUE` (output filename `-compact` suffix), once with `compact = FALSE` (output filename `-full` suffix).

- [x] Add a "Generate compact report" checkbox to Tab 5, default checked. If unchecked, only the detailed report is generated.

- [x] In `server_upload.R`, update the `download_report` handler: default to the compact report, fall back to the detailed report if no compact version exists.

- [x] On Tab 5, after report generation, show two download links: "📄 Compact report (recommended)" and "📋 Detailed report (full audit)". Both should point to the correct files.

- [x] Add i18n keys for the new UI elements and their Spanish translations.

**Sub-tasks (post-split, eventual):**

- [ ] Create `reports/unified_analysis_template_compact.Rmd` as a thin orchestration file that calls the same functions in `analysis_pipeline.R` and `report_sections.R` but includes only the compact-mode sections. Remove the `compact` param from the detailed template.

**Acceptance:** Every report run produces both a compact and detailed HTML (and DOCX/PDF if those formats were selected). The compact version is 3-5 pages for a typical single-plate RBA run. The detailed version is unchanged from current output. Download button defaults to compact.

---

## MEDIUM-VALUE IMPROVEMENTS

### M1. Split unified_analysis_template.Rmd into functions

**Scope:** Extract compute-heavy chunks from the 2000-line Rmd into pure R functions, so the Rmd becomes an orchestration + presentation layer. This is a rolling refactor across multiple PRs. Each sub-task leaves the Rmd in a working state.

**Files to modify/create:** `reports/analysis_pipeline.R` (new), `reports/report_sections.R` (new), `reports/unified_analysis_template.Rmd` (shrinking)

**Sub-tasks, in recommended order:**

- [ ] **M1.1 Extract `sample-analysis` chunk.** Create `reports/analysis_pipeline.R`. Move the sample-analysis logic into a function:

  ```r
  quantify_samples <- function(data_long, model_fit, analysis_config,
                               is_elisa, response_var, classify_range,
                               flag_range) {
    # ... existing logic ...
    list(
      sample_results = sample_results,
      replicate_stats = replicate_stats,
      outlier_flags = outlier_flags,
      outlier_method_log = outlier_method_log,
      replicate_summary = replicate_summary
    )
  }
  ```

  The Rmd chunk becomes:
  ```r
  results <- quantify_samples(data_long, model_fit, analysis_config,
                              is_elisa, response_var, classify_range,
                              flag_range)
  list2env(results, envir = environment())
  ```

  Add a unit test `tests/testthat/test-quantify-samples.R` with a fixture input and expected outputs.

- [ ] **M1.2 Extract `model-fitting` chunk.** Function signature:

  ```r
  fit_all_models <- function(standards_for_model, response_var,
                             selected_weights, is_elisa,
                             analysis_config, data_long) {
    # ... existing logic including LL.4 -> LL.3 -> interpolation fallback ...
    list(
      all_models = all_models,
      primary_key = primary_key,
      model_fit = model_fit,
      weight_desc = weight_desc,
      R2 = R2, RMSE = RMSE,
      model_fits = model_fits,
      ec20 = ec20, ec80 = ec80,
      classify_range = classify_range,
      flag_range = flag_range,
      drc_failed_completely = drc_failed_completely,
      multi_weight_mode = multi_weight_mode
    )
  }
  ```

  Include auto-weighting logic here (H2).

  Add a unit test.

- [ ] **M1.3 Extract `lloq-uloq` chunk.** Function: `determine_lloq_uloq(standards_for_model, model_fit, response_var, is_elisa)` returning `list(formal_lloq, formal_uloq, backcalc_summary, backcalc_display_df)`.

- [ ] **M1.4 Extract `standard-backcalculation` chunk.** Function: `compute_standard_recovery(data_long, model_fit, response_var, is_elisa)` returning the recovery summary data frame.

- [ ] **M1.5 Extract `plate-positional-qc` chunk.** Function: `assess_plate_positional(data_long)` returning the row stats, column stats, and flag lists.

- [ ] **M1.6 Extract rendering helpers.** Create `reports/report_sections.R`. Move large cat-heavy rendering blocks (executive summary, QC traffic light, exclusion audit, tissue normalization traceability) into functions that take data frames and produce markdown via `cat()` and `render_table()`.

- [x] **M1.7 Update source chain.** Both new files (`analysis_pipeline.R` and `report_sections.R`) are sourced from the Rmd `setup` chunk alongside `report_constants.R`, `report_functions.R`, and `plot_functions.R`. Update the file-search loop to include them.

**Acceptance after all sub-tasks:** `unified_analysis_template.Rmd` is ~700-900 lines (down from ~2000). Each new function has a unit test. Rendering the template against the RBA example CSV produces a report identical to the pre-refactor version (compare via B6 golden-number test).

---

### M2. Fix autosave session leakage

**Scope:** Currently `server_common.R` writes autosaves to `tempdir()` keyed by `session$token`, but on startup the "check for previous autosave" block scans all autosaves in `tempdir()` system-wide and restores the most recent one. On a shared machine, user A could be shown user B's layout.

**Files to modify:** `server_common.R`

**Sub-tasks:**

- [x] Scope the autosave scan to a user-specific directory. Use `tools::R_user_dir("bioassay-analysis", "data")` as the base directory (this resolves to a per-user location on all platforms).

- [x] Within that directory, store autosaves as `autosave_<session_token>.rds`. On startup, list files matching that pattern in the user-specific directory only.

- [x] Add a cleanup step: on successful restore or on "Start Fresh" dismissal, delete the loaded autosave file so it isn't re-offered on the next launch.

- [x] Add a background cleanup of autosaves older than 7 days (run once at app startup).

**Acceptance:** Autosaves written by one OS user are invisible to another OS user on the same machine. The restore modal only offers autosaves from the current user.

---

### M3. Golden-number regression test (consolidated with B6)

*Already covered under B6. Listed here for cross-reference.*

---

## FUTURE / DEFERRED (not for v1.0)

### F1. Automatic CurveID detection for parallelism studies

**Scope:** Tab 1 toggle "this plate contains multiple standard curves" that maps the replicate column to `CurveID` and triggers the parallelism assessment. Currently `assess_parallelism()` only fires when a `CurveID` column exists in the long-format data, which it never does in the current UI workflow.

**Rationale:** The parallelism assessment in `reports/report_functions.R` is already implemented but invisible to users who don't manually add a `CurveID` column. A toggle in the UI would expose the functionality without requiring data-engineering skills.

**Status:** Deferred to v1.2+. Not required for publication or first release.

---

### F2. Paper submission to SoftwareX or JOSS

**Scope:** Write and submit a software paper describing the suite. The codebase is already at publication quality — the remaining effort is prose.

**Prerequisites:**

- [ ] v1.0.0 tagged on GitHub
- [ ] Zenodo DOI minted (both concept DOI and v1.0.0 version DOI)
- [ ] Screenshots in README (B3)
- [ ] MIT license in place (B2)
- [ ] CI passing (B4, B5)

**Target journals, in order of fit:**

1. **SoftwareX** (Elsevier, open access, ~4-6 pages). Good fit for a full-featured scientific application with statistical methodology. Typical review time 1-3 months.
2. **Journal of Open Source Software (JOSS)**. Very short (~500 words), reviewed on GitHub against the code itself. Free to submit. Good fit if the scope feels tighter.
3. **Harmful Algae** application note. Strong venue fit given the domain, but less common for pure software.

**Citation guidance:**

- In the paper body, cite the v1.0.0 version DOI.
- In the README, use the concept DOI (which always points to latest).
- Add a CITATION.cff file at repo root with both.

**Status:** Planned for after v1.0.0 release. Can proceed while v1.1 development continues in parallel.

---

## MANUAL CHECKLIST (not for Claude Code)

- [ ] Confirm with Arnold Molina Porras that he's comfortable as joint copyright holder (for B2)
- [ ] Confirm with IAEA that MIT licensing is permitted (for B2)
- [ ] Take the six screenshots described in B3 and commit to `docs/screenshots/`
- [ ] Enable GitHub-Zenodo integration: https://zenodo.org/account/settings/github/
- [ ] After v1.0.0 release, verify Zenodo webhook created both concept DOI and version DOI
- [ ] Update README with concept DOI badge
- [ ] Create CITATION.cff with both DOIs
- [ ] Draft paper for SoftwareX or JOSS

---

## Open questions for Kristof

None currently — all clarifications from the review discussion are incorporated above.

If any of the acceptance criteria are ambiguous once implementation starts, Claude Code should ask before guessing.

---

*Last updated: 2026-04-21*

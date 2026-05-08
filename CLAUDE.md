# CLAUDE.md

This file provides guidance to Claude Code (claude.ai/code) when working with code in this repository.

## Running the app

```r
shiny::runApp("app.R")           # from repo root in an R session
shiny::runGitHub("Shiny-App-Competitive-Bioassays", "KristofM854")  # directly from GitHub
Rscript scripts/run_local.R      # standalone launcher — creates dated output dirs and auto-renders reports
```

## Tests

```r
testthat::test_dir("tests/testthat", stop_on_failure = TRUE)   # all tests
testthat::test_file("tests/testthat/test-analysis-pipeline.R") # single file
```

Run tests from the **repo root**. `helper-setup.R` resolves paths relative to repo root; tests that need unavailable packages call `skip_if_not_installed()` themselves. CI runs via `.github/workflows/R-CMD-check.yml` (R 4.3, Chrome, Pandoc).

Key test files: `test-analysis-pipeline.R`, `test-report-numbers.R`, `test-utils_plate.R`, `test-utils_normalization.R`, `test-shinytest-rba.R`, `test-shinytest-elisa.R`.

## Critical package masking issue

`drc` loads `MASS`, which **masks `dplyr::select()`**. Every `select()` call across the entire codebase must be written as `dplyr::select()`. This is enforced by convention — there is no linter rule catching it.

## Architecture

The app is a 5-tab Shiny wizard: Configuration → Plate Layout → Upload & Preview → Analysis Settings → Generate Report.

**Shared reactive state** (`shared` list assembled in `app.R`):

| Reactive | Contents |
|---|---|
| `shared$matrix_type` | 8×12 sample-type matrix |
| `shared$matrix_id` | 8×12 sample-ID matrix |
| `shared$matrix_dilution` | 8×12 dilution-factor matrix |
| `shared$matrix_replicate` | 8×12 replicate-group matrix |
| `shared$matrix_measresults` | 8×12 measurement values from file |
| `shared$rv` | `reactiveValues` for language, multi-wavelength flag, etc. |

**Server modules** (`server/`):

| File | Owns |
|---|---|
| `server_config.R` | Tab 1 — assay type, standards, QC inputs, toxin config |
| `server_layout.R` | Tab 2 — rhandsontable matrix editors, presets, undo/redo |
| `server_upload.R` | Tab 3 — file import, heatmap preview, visual selector |
| `server_analysis.R` | Tab 4 — weighting, CI method, outlier settings |
| `server_report.R` | Tab 5 — pre-flight validation, report trigger |
| `server_common.R` | Auto-save (60 s), tab navigation, language switcher, guided tour |
| `report_pipeline.R` | Staged helpers called by `server_report.R`: flush state → build long data → normalize → save artifacts → render |
| `layout_history.R` | 20-state undo/redo for matrix edits |
| `i18n.R` | 480+ translation keys (EN/ES/FR/RU/ZH); `tr(key, lang)` for direct lookup, `tr(key, lang, arg1, ...)` for sprintf |

**Utilities** (`utils/`): `utils_plate.R` (matrix ↔ long format), `utils_import_v3.R` (auto-detect plate region in xlsx/csv/txt), `utils_import_multiwavelength.R` (per-sheet parsing), `utils_normalization.R` (ELISA %B/B0 blank correction).

## Report generation

Reports are triggered by `observeEvent(input$convert)` in `server_report.R`, which calls the staged pipeline in `server/report_pipeline.R`:

1. Flush debounced dilution edits
2. Convert 4 matrices → long format CSV (`long_data_output.csv`)
3. Validate/normalize (ELISA: check Blank/NSB/B0 hierarchy; RBA: pass-through)
4. Write JSON metadata (`model_stats.json`, `notes.json`, `selected_formats.json`, etc.)
5. Call `rmarkdown::render()` on the appropriate template

**Templates** (`reports/`):

| File | Role |
|---|---|
| `unified_analysis_template.Rmd` | Main template (~2 100 lines) for single-wavelength |
| `unified_analysis_template_compact.Rmd` | Thin wrapper setting `params$compact = TRUE` |
| `multiwavelength_analysis_template.Rmd` | Multi-wavelength wrapper |
| `analysis_pipeline.R` | Pure compute: DRC fitting, LLOQ/ULOQ, outlier detection, sample quantification |
| `report_functions.R` | Data loading, QC checks, heteroscedasticity test, stability grading |
| `report_sections.R` | HTML helper functions: `render_report_header()`, `render_kpi_strip()`, `render_overall_status_box()`, `render_qc_traffic_light_section()`, etc. |
| `plot_functions.R` | `render_table()`, `render_plot()`, `section_open()`, `section_start()`, `section_close()` |
| `report_constants.R` | Validation rules, molecular weights, output file names |
| `report_style.css` | Injected into HTML reports (lives inside the `reports/` directory) |

**Output** goes to a dated directory set by the `RBA_OUTPUT_DIR` environment variable. The Rmd reads the CSV and JSON files from that directory rather than receiving data from Shiny's memory.

## Key constants (global.R and report_constants.R)

- `PLATE_NROW` = 8, `PLATE_NCOL` = 12
- `DEFAULT_STX_CONC` — 8-point RBA saxitoxin standards (1e-6 → 3e-11 mol/L)
- `DEFAULT_CORTISOL_CONC` / `TESTOSTERONE_CONC` / `ESTRADIOL_CONC` — ELISA defaults
- `QC_THRESHOLDS` — `cv_limit` (30%), `hill_slope_tolerance` (±20%), `qc_deviation_limit` (30%), `ec50_se_limit` (0.3)
- `MW_LOOKUP` — molecular weights for named toxins/analytes (used for mol ↔ mass conversion)
- `STATS_CONFIG` — heteroscedasticity variance-ratio thresholds, CI truncation floor

## i18n usage rule

`tr("key", lang)` returns the string directly (never passes through `sprintf`). `tr("key", lang, arg1, arg2)` calls `sprintf(text, arg1, arg2)`. Keys that are called **without** extra args must use a single `%` in their string; keys called **with** extra args must use `%%` to produce a literal `%` in sprintf output. Mixing these breaks the rendered text.

## Report HTML structure conventions

- Collapsible sections: `section_open(title, default_open = FALSE)` (inline) / `section_start(title, default_open = FALSE)` (cat-based) + matching `section_close()` / `section_end()`.
- Format-aware rendering: `is_html_out()` / `is_docx_out()` in `plot_functions.R` — use these instead of `knitr::is_html_output()` directly.
- `render_table()` passes `format = "html", escape = TRUE` for HTML; sanitises `|` → `│` (U+2502) for DOCX pipe-table output.
- CSS class prefix convention: `.bs-*` (bioassay-specific) for all new report styles; add to `reports/report_style.css`, not as inline `style=""` attributes.

## Presets and examples

- Preset plate layouts live in `presets/`; `presets/generate_presets.R` recreates them if missing.
- Example datasets: `examples/rba_stx_example.csv` and `examples/elisa_cortisol_example.csv`; regenerate with `examples/generate_example_data.R`.

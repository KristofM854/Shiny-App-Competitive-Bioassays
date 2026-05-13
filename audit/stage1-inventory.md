# Stage 1 Inventory — Pre-1.0.0 Audit
## Competitive Binding Assay Analysis Suite (RBAElisaApp)
**Audit date:** 2026-05-13  
**Auditor:** Claude (static read-only pass, no code changes)

---

## 1. Repository Overview

### Framework
Raw Shiny (`shiny::fluidPage` + `shiny::tabsetPanel`). No golem, rhino, or bs4Dash scaffolding. Supplemental UI packages: `shinyjs`, `shinyFeedback`, `shinycssloaders`, `rintrojs`, `rhandsontable`.

### Total R Files and Approximate LoC

| File | LoC |
|---|---|
| `app.R` | 1167 |
| `global.R` | 335 |
| `server/i18n.R` | 3109 |
| `server/server_common.R` | 1069 |
| `server/server_layout.R` | 666 |
| `server/server_upload.R` | 844 |
| `server/server_report.R` | 581 |
| `server/report_pipeline.R` | 501 |
| `server/server_config.R` | 364 |
| `server/server_analysis.R` | 134 |
| `server/layout_history.R` | 109 |
| `reports/analysis_pipeline.R` | 1007 |
| `reports/report_functions.R` | 915 |
| `reports/report_sections.R` | 838 |
| `reports/plot_functions.R` | 610 |
| `reports/report_constants.R` | 148 |
| `reports/create_reference_doc.R` | 33 |
| `reports/unified_analysis_template.Rmd` | 2234 |
| `reports/multiwavelength_analysis_template.Rmd` | 752 |
| `reports/unified_analysis_template_compact.Rmd` | 53 |
| `utils/utils_import_v3.R` | 455 |
| `utils/utils_plate.R` | 493 |
| `utils/utils_normalization.R` | 249 |
| `utils/utils_import_multiwavelength.R` | 332 |
| `scripts/run_local.R` | 349 |
| `tests/testthat/test-*.R` (13 files) | ~1100 total |
| `examples/generate_example_data.R` | 89 |
| `presets/generate_presets.R` | 55 |
| **Grand total (R + Rmd)** | **≈18 600** |

### Directory Structure

```
.
├── app.R                          # Entry point: UI + server wiring
├── global.R                       # Packages, constants, helper fns
├── DESCRIPTION                    # Package metadata (no renv.lock)
├── CITATION.cff                   # CFF citation metadata
├── LICENSE                        # MIT
├── README.md                      # User-facing documentation
├── server/                        # Modular server logic
│   ├── i18n.R                     # 480+ translation keys (EN/ES/FR/RU/ZH)
│   ├── layout_history.R           # 20-state undo/redo
│   ├── report_pipeline.R          # Staged pipeline helpers
│   ├── server_analysis.R          # Tab 4: weighting preview
│   ├── server_common.R            # Auto-save, navigation, guided tour
│   ├── server_config.R            # Tab 1: assay type, standards
│   ├── server_layout.R            # Tab 2: matrix editors, presets
│   ├── server_report.R            # Tab 5: pre-flight, report trigger
│   └── server_upload.R            # Tab 3: file import, heatmap
├── utils/                         # Stateless utility modules
│   ├── utils_import_v3.R          # Plate reader file auto-detection
│   ├── utils_import_multiwavelength.R  # Multi-wavelength Excel parsing
│   ├── utils_normalization.R      # ELISA %B/B0 normalization (Strategy pattern)
│   └── utils_plate.R              # Matrix creation, conversion, dilution parsing
├── reports/                       # Report rendering layer
│   ├── unified_analysis_template.Rmd      # Single-wavelength (~2200 lines)
│   ├── unified_analysis_template_compact.Rmd  # Thin wrapper (compact=TRUE)
│   ├── multiwavelength_analysis_template.Rmd  # Multi-wavelength wrapper
│   ├── analysis_pipeline.R        # Pure compute: DRC fit, LLOQ, quantification
│   ├── report_functions.R         # Heteroscedasticity, stability, BB0, parallelism
│   ├── report_sections.R          # HTML helper fns (render_report_header, etc.)
│   ├── plot_functions.R           # render_table(), render_plot(), section helpers
│   ├── report_constants.R         # STATS_CONFIG, QC_THRESHOLDS, VALIDATION_RULES
│   ├── report_style.css           # Injected into HTML reports
│   └── create_reference_doc.R     # Generates reference.docx for DOCX output
├── presets/                       # Pre-built plate layout .rds files
│   └── generate_presets.R         # Recreates presets if missing
├── examples/                      # Example datasets
│   ├── rba_stx_example.csv
│   ├── elisa_cortisol_example.csv
│   ├── plate_template.csv
│   └── generate_example_data.R
├── scripts/
│   └── run_local.R                # Standalone launcher with dated output dirs
├── tests/testthat/                # Test suite
├── www/                           # Static web assets
│   ├── js/bs-handlers.js
│   └── style.css
├── style.css                      # Root-level CSS (likely for runApp())
├── docs/                          # Screenshots only
├── .github/workflows/R-CMD-check.yml  # CI
└── audit/                         # This audit
```

### Build/Run Entry Points

| Command | Entry Point | Notes |
|---|---|---|
| `shiny::runApp("app.R")` | `app.R:1167` | Standalone; sources `global.R` via `if (!exists("PLATE_NROW"))` guard |
| `shiny::runGitHub(...)` | Same | Identical path |
| `Rscript scripts/run_local.R` | `scripts/run_local.R` | Creates dated output dir, sets env vars, calls `shiny::runApp()` |

`app.R` sources all modules at parse time (lines 42–56) before calling `shinyApp()`.

---

## 2. Dependency and Reproducibility Infrastructure

### DESCRIPTION
Present at repo root (`DESCRIPTION`). Declares `Package: RBAElisaApp`, `Version: 2.0.0` (conflicts with `app.R` comment "Version: 1.0.0" and `CITATION.cff` "version: 1.0.0"). Lists 37 imports with **no version pins** (`Imports: car, digest, dplyr, drc, ...`). `RoxygenNote: 7.0.0` present. No `Suggests` field for test packages.

### renv / packrat
**Neither is present.** No `renv.lock`, no `packrat/` directory. Reproducibility of the package environment is not pinned.

### NAMESPACE
**Absent.** The repo is structured as a Shiny app (not built as a proper R package), so no `NAMESPACE` is generated.

### .Rprofile
**Absent.**

### Pinned vs Floating Versions
All dependency versions are **floating** (`any::` in CI YAML; no version constraints in `DESCRIPTION`). The self-healing install logic in `global.R:57–115` installs missing packages from RSPM at whatever version is current.

### R Version Constraint
`global.R:20–24` warns at runtime if `getRversion() < "4.2.0"`. DESCRIPTION does not declare a `Depends: R (>= 4.2)` field. CI pins R to `4.3` (`R-CMD-check.yml:19`).

### Dockerfile / Container
**None.** No Dockerfile, `docker-compose.yml`, or Binder configuration is present.

### CI Workflows
Single workflow: `.github/workflows/R-CMD-check.yml`.
- Trigger: push/PR to `main`
- R version: 4.3 (single version, no matrix)
- Steps: checkout → setup-r → setup-pandoc → setup-r-dependencies → install Chrome → `testthat::test_dir()`
- Uses `any::` prefix for all packages (floating, resolved at CI time)
- No `R CMD check`, no `covr`, no linting step

---

## 3. Data Flow Trace

A single user session from file upload to final report:

### Step 1 — Configuration (Tab 1, `server_config.R`)
- User selects assay type (`input$assay_type` = "rba" or "elisa"), standard concentrations (`input$std1`…`input$stdN`), toxin class, analyte, molecular weight.
- `config_reactives$std_conc_raw()` returns a numeric vector of standard concentrations parsed by `server_config.R`.
- Shared state updated: `shared$matrix_type()` and `shared$matrix_replicate()` initialised via `create_type_matrix()` / `create_replicate_matrix()`.
- **Validation:** `is_valid_scientific()` (`global.R:285`) checks each standard input. `shinyFeedback::feedbackDanger()` shown for invalid entries. No hard stop; invalid values simply remain NA.

### Step 2 — Plate Layout (Tab 2, `server_layout.R`)
- User edits four `rHandsontableOutput` matrices (type, id, dilution, replicates).
- Dilution cells parsed by `parse_dilution_matrix()` (`utils_plate.R:324`); ratio notation `1:2` → 0.5.
- `shared$dilution_validity` (`reactiveVal`, 8×12 logical matrix) tracks parse failures; `shared$dilution_error()` flags any failure.
- Data structure leaving this step: four 8×12 `data.frame` objects (`shared$matrix_type()`, etc.).

### Step 3 — Upload & Preview (Tab 3, `server_upload.R`)
- User uploads file via `fileInput("plate_file", ...)`.
- `parse_plate_file()` (`utils_import_v3.R:338`) dispatches to:
  - Multi-wavelength Excel: `detect_and_import_multiwavelength()` (`utils_import_multiwavelength.R`)
  - Single plate: `import_plate_data()` → `detect_plate_location()` (two strategies: row-label A–H scan, then pure numeric block scan)
- Result is an 8×12 numeric `data.frame` stored in `shared$matrix_measresults()`.
- **Validations:** overflow markers (`#SAT`, `OVER`, `ERR`, etc.) replaced with `NA` (`utils_import_v3.R:246–258`). Comma-decimal auto-detected and converted (`utils_import_v3.R:262–271`). Server upload handler also coerces the matrix to numeric column-by-column (`server_upload.R:47`).
- **Error path:** `detect_plate_location()` returns `NULL` if no valid block found; `import_plate_data()` calls `stop()` with a user-readable message. Caught by `tryCatch` in the upload observer; displayed as `showNotification(..., type = "error")`.
- Heatmap rendered via `plotly::plot_ly()` from the raw matrix values.

### Step 4 — Analysis Settings (Tab 4, `server_analysis.R`)
- User selects regression weighting(s), CI method, outlier detection, quantification range, CV threshold.
- No new data structures created; user choices are read from `input$*` at report time.
- Weighting preview rendered via a synthetic ggplot2 plot (`server_analysis.R:98`).

### Step 5 — Report Generation (Tab 5, `server_report.R` + `server/report_pipeline.R`)
Triggered by `observeEvent(input$convert, ...)` (`server_report.R:347`):

| Stage | Function | Input | Output |
|---|---|---|---|
| Flush dilution | `flush_latest_layout_state()` | `input$matrix_dilution` | Updates `shared$matrix_dilution()` |
| Build long data | `build_long_data()` → `matrix_to_long()` | 4 matrices + std concs | `data.frame` with columns: Well, Row, Column, SampleType, SampleID, StandardConc, DilutionFactor, Replicate, MeasurementValue |
| Normalise | `normalize_assay_data()` | Long data + assay type | Same data.frame + NormalizedValue + ResponseUnit |
| Save artifacts | `save_analysis_artifacts()` | Normalised data + config | `long_data_output.csv`, JSON sidecars |
| Render | `render_reports()` → `rmarkdown::render()` | `output_dir` + lang + formats | HTML/DOCX/PDF files |

Inside `rmarkdown::render()`, the Rmd sources helper R files, reads `long_data_output.csv` from `output_dir`, re-runs the statistical pipeline (`fit_all_models()`, `quantify_samples()`), and writes `model_stats.json` and `unknown_results*.csv`.

**Error propagation:** The outer `tryCatch` in `observeEvent(input$convert)` (`server_report.R:565`) catches any error from the pipeline and shows `showNotification(..., type = "error")`. Per-format render errors inside `render_reports()` are individually caught (`report_pipeline.R:431`) and shown as notifications, but the outer orchestration continues to try remaining formats. A fully empty `output_paths` list after all renders sets `shared$rv$analysis_state <- "failed"` (`server_report.R:558`).

---

## 4. Statistical Pipeline (Critical)

### Curve-Fitting Code Paths

All fitting occurs in `fit_all_models()` (`reports/analysis_pipeline.R:252`). The template chunk `model-fitting` in `unified_analysis_template.Rmd:554` calls this function and injects results with `list2env()`.

**Model hierarchy (per weighting key):**

```r
# LL.4 attempt (analysis_pipeline.R:362–384)
fit <- tryCatch({
  drc::drm(fml, data = standards_for_model, fct = drc::LL.4(), ...)
}, error = function(e) {
  warning("LL.4() failed ... Trying LL.3()...")
  NULL
})

# LL.3 fallback (analysis_pipeline.R:386–399)
if (is.null(fit)) {
  fit_method <- "LL.3"
  fit <- tryCatch({
    drc::drm(fml, data = standards_for_model, fct = drc::LL.3(), ...)
  }, error = function(e) {
    warning("LL.3() also failed ... Falling back to log-linear interpolation.")
    NULL
  })
}

# Log-linear interpolation fallback (analysis_pipeline.R:448–479)
if (is.null(fit)) {
  tryCatch({
    std_means <- ...
    interp_fun <- approxfun(log10(std_means$concentration), std_means$mean_resp, rule = 2)
    all_models[[wt_key]] <- list(model = NULL, interp_fun = interp_fun, ...,
                                 fit_method = "interpolation")
  }, ...)
}
```

Trigger mechanism for each fallback: **`tryCatch` catching any `error`** from `drc::drm()`. There is no AIC comparison, no SE check, no convergence flag — any error from `drm()` triggers the next fallback immediately.

**ELISA-specific LL.4 constraints:** When `is_elisa == TRUE`, `lowerl = c(NA, 0, NA, NA)` and `upperl = c(NA, NA, 100, NA)` are passed to constrain bottom ≥ 0 and top ≤ 100 (`analysis_pipeline.R:364–378`).

**Auto-weighting (H2):** If `"auto"` is in `selected_weights`, an unweighted LL.4 is fitted first; `assess_heteroscedasticity()` (`report_functions.R:235`) is called on its residuals. The chosen weighting key is:
- `"inv_y2"` if p < 0.01 AND variance_ratio > 10, or if variance_ratio > 10 alone
- `"inv_y"` if p < 0.05 or variance_ratio ≥ 3
- `"none"` otherwise
(`analysis_pipeline.R:323–348`)

### EC Value Computation

EC values use `drc::ED()` with `type = "absolute"`:

```r
# analysis_pipeline.R:525–537
ec20 <- tryCatch({
  ED(model_fit, respLev = STATS_CONFIG$ec20_resp_level,   # = 80
     type = "absolute", display = FALSE)
  as.numeric(ed_result[1, 1])
}, error = function(e) NA_real_)
ec80 <- tryCatch({
  ED(model_fit, respLev = STATS_CONFIG$ec80_resp_level,   # = 20
     type = "absolute", display = FALSE)
  as.numeric(ed_result[1, 1])
}, error = function(e) NA_real_)
```

`STATS_CONFIG$ec20_resp_level = 80` and `$ec80_resp_level = 20` (`report_constants.R:52–54`). This is the `drc` convention where `respLev` is the percent *reduction* from the top asymptote, so `respLev=80` returns the concentration where response has dropped to 20% (EC20), and `respLev=20` returns EC80. After computation, if `ec20 > ec80`, they are swapped (`analysis_pipeline.R:535–537`).

**EC values for interpolation fallback:** `quantify_samples()` calls `drc::ED()` on `model_fit` for per-well predictions. If `model_fit` is `NULL` (interpolation fallback was used), `quantify_samples()` uses `model_fit` directly in `ED()` — **this call will error silently** (caught by the per-well `tryCatch`) and the result will be `NA_real_` for all sample concentrations. The interpolation's `interp_fun` is never used inside `quantify_samples()` for sample back-calculation; only the `model = NULL` branch of the result is checked in the Rmd via `drc_failed_completely`.

**EC values from linear fallback:** EC20/EC80 computation (`analysis_pipeline.R:524–537`) calls `ED(model_fit, ...)` — if `model_fit` is `NULL` this catches the error and returns `NA_real_`. There is **no separate flag** indicating that EC values were computed from a sigmoid vs linear fallback. The `weight_desc` string for an interpolation model reads e.g. `"Unweighted (log-linear interpolation)"`, and this string is stored in `model_stats.json:weight_method`. The report renders the fit method in the coefficients table header (`unified_analysis_template.Rmd:1113`), but there is no distinct flag or warning on the sample result table indicating that sample concentrations were obtained via interpolation rather than parametric inversion.

### Whether the Model Used Is Logged

- **In the report:** `weight_desc` is printed in the model coefficients section heading (`unified_analysis_template.Rmd:1113`). The stability assessment table (`analysis_pipeline.R:439`) includes `fit_method` per weighting. The executive summary includes `weight_method` from `model_stats`.
- **In JSON sidecar:** `model_stats.json` contains `weight_method` (the `weight_desc` string including fit method in parentheses). Written at `save-model-stats` chunk (`unified_analysis_template.Rmd:1198`).
- **In UI:** The KPI strip on Tab 4 reads `shared$rv$last_model_stats` which includes `weight_method`.

### Weight Vector Handling

Weight vectors are constructed before fitting (`analysis_pipeline.R:288–293`):
```r
weight_options$inv_y$weights  <- ifelse(resp_vals > 0, 1 / resp_vals,     1)
weight_options$inv_y2$weights <- ifelse(resp_vals > 0, 1 / (resp_vals^2), 1)
```
Zero or negative response values receive weight = 1 (not excluded). Length assertions (`stopifnot`) verify alignment with `standards_for_model` (`analysis_pipeline.R:292–293`). Weighted R² uses `weighted.mean` and weighted sum-of-squares (`analysis_pipeline.R:406–416`).

There are also two alternative bootstrap paths:
- `analysis_pipeline.R:599–603` uses `set.seed(42)` then `set.seed(NULL)`.
- `reports/report_sections.R:587` has `boot_n <- STATS_CONFIG$bootstrap_iterations %||% 2000` (overrides the constant's value of 1000 with 2000 for some report section rendering).

---

## 5. Input Handling

### File Readers

| Format | Reader | Sheet Handling |
|---|---|---|
| `.xlsx`, `.xls` | `readxl::read_excel(..., col_names = FALSE)` (`utils_import_v3.R:29`) | Scans all sheets if plate not found on sheet 1 (`import_plate_data:208`) |
| `.csv` | `read.csv(..., header = FALSE, stringsAsFactors = FALSE)` (`utils_import_v3.R:34`) | — |
| `.txt` | `read.table(..., header = FALSE, sep = "\t", stringsAsFactors = FALSE)` (`utils_import_v3.R:37`) | — |

**TSV/tab-separated text is handled; semicolon-separated CSV is not.** Files with other delimiters would silently produce a single-column parse.

### Column Name Matching
None. The import detects the plate data block by **positional scanning** (`detect_plate_location()`), not by column name. Strategy 1 looks for row labels A–H in the first column; Strategy 2 finds the largest valid numeric 8×N block. No fuzzy or exact header matching.

### Decimal Separator Handling
Auto-detected: if the cell block contains `[0-9],[0-9]` patterns but no `[0-9]\.[0-9]` patterns, all commas are replaced with periods (`utils_import_v3.R:262–271`). A `warning()` is emitted. Mixed files (some comma, some dot) are not handled and would silently produce NAs.

### Encoding Handling
No explicit encoding handling. `read.csv` uses the system locale; `readxl::read_excel` handles UTF-8/UTF-16 internally. No BOM stripping, no `fileEncoding=` argument, no `iconv()` call anywhere in the codebase. Files with non-UTF-8 encodings (e.g. Latin-1 from European plate readers) may produce garbled sample IDs.

### Predefined Format Specification
The upload accepts **any** file where `detect_plate_location()` can find an 8×N numeric block (N ≥ 4, ≥ 70% valid cells, ≥ 32 valid values). The exact spec as understood by the importer:
- Row labels A–H in column 1 (optional), with sequential numeric column headers (1, 2, 3, …) in the row above the first data row (also optional)
- OR: a contiguous 8-row block of numeric values, all 8 rows having ≥ 4 valid numbers, ≥ 6/8 rows valid per good column
- Overflow markers: `#SAT`, `OVER`, `ERR`, `****`, `Overfl`, `Sat` (case-insensitive) → NA
- Standard IDs are matched to concentrations by the regex `^S[0-9]+$` pattern in `matrix_to_long()` (`utils_plate.R:416`)

### In-App Format Guidance
Shown in the Troubleshooting section of `README.md:236`. In the app: Tab 3 shows a "Show default plate layout" button and a downloadable template (`plate_template.csv` in `examples/`). No formal spec document exists beyond the README troubleshooting section.

### Example/Template Files
Three shipped example files:
- `examples/rba_stx_example.csv` — RBA saxitoxin triplicate layout
- `examples/elisa_cortisol_example.csv` — ELISA cortisol with controls
- `examples/plate_template.csv` — Blank plate template

---

## 6. Report Generation

### Output Formats and Rendering Toolchain

| Format | Renderer | Notes |
|---|---|---|
| HTML | `rmarkdown::render(..., output_format = "html_document")` | `self_contained: true`; inline CSS injected from `report_style.css` |
| DOCX | `rmarkdown::render(..., output_format = rmarkdown::word_document(...))` | `reference.docx` from `reports/create_reference_doc.R` if present; static PNG figures at 300 DPI |
| PDF | `rmarkdown::render(..., output_format = "pdf_document")` | Requires TinyTeX / pdflatex / xelatex / lualatex; falls back to HTML with notification if unavailable (`report_pipeline.R:311–321`) |

Pandoc (via `rmarkdown`) is the rendering toolchain for all three formats. `kableExtra` provides table styling. `plotly` provides interactive HTML figures; DOCX/PDF use static `ggplot2` PNGs.

### Figure Embedding
HTML: Plotly figures rendered as interactive widgets embedded inline (self-contained HTML). ggplot2 static figures also embedded base64 (`self_contained: true`). DOCX/PDF: `dev = "png"`, `dpi = 300`, `fig.retina = 1` set in `knitr::opts_chunk$set()` when `!knitr::is_html_output()` (`unified_analysis_template.Rmd:86–96`).

### Report Metadata

The report header chunk (`report-meta`) writes (`unified_analysis_template.Rmd:431–475`):
- Authors (hardcoded)
- Generated date (`format(Sys.Date(), "%Y-%m-%d")`) and OS user (`Sys.info()[["user"]]`)
- Assay label
- App version (from `REPORT_INFO$version` = "2.0"), R version string, drc version

**Missing from the report header:** session info (other package versions), input file name or hash, analysis settings summary (weighting, CI method, outlier settings), run UUID.

### JSON Sidecar Files

Written by `save_analysis_artifacts()` (`server/report_pipeline.R:207–234`) and updated by `save-model-stats` / `save-sample-stats` chunks inside the Rmd:

| File | Contents | When Written |
|---|---|---|
| `long_data_output.csv` | 96-row long-format plate data (Well, SampleType, SampleID, StandardConc, DilutionFactor, Replicate, MeasurementValue, NormalizedValue, ResponseUnit) | Stage 4 (before render) |
| `selected_formats.json` | Array of format strings ("html", "docx", "pdf") | Stage 4 |
| `notes.json` | `{notes: "..."}` | Stage 4 |
| `qc_params.json` | QC concentration, expected Hill slope (RBA) or analyte/units (ELISA) | Stage 4 |
| `assay_config.json` | assay_type, toxin_class, molecular_weight, detection_method, units | Stage 4 |
| `analysis_config.json` | regression_weight, quant_range_min/max, ci_method, enable_outlier_detection, outlier_min_n, normality_assumption, cv_limit | Stage 4 |
| `report_language.json` | `{lang: "en", langs: ["en"]}` | Stage 4 |
| `tissue_weights.json` | Per-replicate tissue weight + extraction volume (ELISA only) | Stage 4 |
| `sample_processing_config.json` | `{extraction_volume_ul: 500, ...}` (ELISA only) | Stage 4 |
| `model_stats.json` | r_squared, rmse, ic50, hill_slope, n_standards, n_unique_concs, weight_method, mean_sample_cv | During render (two chunks) |
| `unknown_results.csv` | Per-well predicted concentrations | During render, `sample-results-table` chunk |
| `unknown_results_summary.csv` | Per-replicate-group summary | During render |
| `wavelength_manifest.json` | List of wavelength labels (multi-wave only) | Stage 4 |

**Sidecar sufficiency for deterministic replay:** The combination of `long_data_output.csv` + `assay_config.json` + `analysis_config.json` + `qc_params.json` contains sufficient state to re-run `analysis_pipeline.R` and reproduce numerical results, **provided the same package versions are used**. There is no input file hash, no package version snapshot in the sidecars.

---

## 7. Session and State Management

### `Sys.setenv` / Process-Global State

**Four process-global environment variables** are set at application startup in `app.R:77–81` (standalone mode):
```r
Sys.setenv(RBA_OUTPUT_DIR = output_dir)
Sys.setenv(RBA_CSV_PATH   = ...)
Sys.setenv(RBA_FMT_JSON   = ...)
Sys.setenv(RBA_NOTES_FILE = ...)
```
These are **process-wide** — all concurrent Shiny sessions in a multi-user deployment share them until session-scoped paths overwrite them in `session$userData`. The session-scoped copy is captured at session start (`app.R:1139–1146`) and then re-pointed to a per-run subdirectory at each report generation (`server_report.R:379–393`). The `Sys.getenv` fallback in the Rmd templates reads the process-global values if a session is not running (e.g. standalone render). On a multi-user Shiny Server this creates a race condition: a new session started during another session's report generation will inherit the stale env var from the previous run.

`scripts/run_local.R:123–126` also sets the same four env vars.

### `<<-` (Super-Assignment) Usage

| Location | Variable | Scope |
|---|---|---|
| `server/i18n.R:23` | `.TRANSLATIONS_CACHE <<-` | Module-level variable in i18n.R; assigned once and cached. Shared across all sessions in the same process. |
| `reports/report_functions.R:396, 546, 547, 795` | `result$*` inside `tryCatch` error handlers | Assigns into the enclosing function's `result` list; local scope, correct pattern. |
| `reports/plot_functions.R:189` | `tr_idx <<-` | Counter inside a closure; process-wide. |
| `reports/analysis_pipeline.R:678` | `sample_results$quantification_status <<-` | Inside a `tryCatch` error handler; assigns to enclosing `quantify_samples()` scope. |

### Hardcoded Paths

- `app.R:73`: `file.path(getwd(), "reports", "runs", ...)` — depends on working directory at startup.
- `server/report_pipeline.R:296–298`: `file.path(if (file.exists("reports")) "reports" else ..., "reference.docx")` — relative path resolution.
- `server/report_pipeline.R:326–328`: template directory resolved as `file.path(app_root, "reports")` with a fallback; relies on `file.exists("reports")` check.

### `tempfile` / `tempdir` Usage

- `reports/create_reference_doc.R:7`: `tempfile(fileext = ".Rmd")` for a throwaway DOCX stub — does not persist.
- `reports/multiwavelength_analysis_template.Rmd:127`: `preprocess_template_chunks(..., temp_dir = tempdir())` for Rmd child-include preprocessing — session-scoped by Rmd render process.
- `server/server_common.R:30`: `tempdir()` fallback for autosave directory if `tools::R_user_dir()` fails.
- Test files use `tempfile()` for fixture generation — not in production code.

### ReactiveValues and Process-Global State

- `shared$rv` (`reactiveValues`) — defined per session in `app.R:1119–1129`. Correctly session-scoped.
- `shared$matrix_*` (`reactiveVal`) — all session-scoped.
- `.TRANSLATIONS_CACHE` in `server/i18n.R:17,23` — process-global (module-level `<<-`). If translations were mutable this would be a cross-session bug; in practice translations are read-only after first load, so this is safe but fragile.
- `theme_set(theme_rba())` in `global.R:308` — sets the ggplot2 default theme **process-globally** for all concurrent sessions.

### Auto-Save Path

Autosave files written to `tools::R_user_dir("bioassay-analysis", which = "data")` with per-session token in filename (`server_common.R:28–36`). Old autosaves (> 7 days) cleaned up at session start. Correct per-user isolation since M2 refactor.

---

## 8. Error Handling and User Feedback

### `showNotification` Patterns
Used extensively throughout the pipeline for progress (`type = "message"`) and errors (`type = "error"`, `type = "warning"`). A persistent notification with `id = "report_progress"` is used during report generation and removed on completion or error (`server_report.R:360, 514`).

### `validate` / `need`
**Not used anywhere in the codebase.** Shiny's `validate(need(...))` pattern is absent. Pre-flight checks in `server_report.R:117–297` compute conditions imperatively and drive `shinyjs::enable/disable("convert")` rather than using reactive validation.

### `modalDialog`
Used for: autosave restore prompt (`server_common.R:62–73`), welcome modal (`server_common.R:147–167`), ELISA controls missing warning on tab navigation (`server_common.R:182–190`), upload instruction modal (inferred from `server_upload.R`).

### Silently Swallowed Errors

| Location | Pattern | Risk |
|---|---|---|
| `server_common.R:134–137` | `tryCatch({ saveRDS(...) }, error = function(e) {})` — empty error handler | Auto-save failure is completely silent; user never informed |
| `server/i18n.R:23` via `<<-` in `get_translations()` | Full function body in `tryCatch`; on error returns whatever was partially built | Translation errors produce wrong or missing text without notification |
| `analysis_pipeline.R:448–479` | Log-linear interpolation fallback emits `warning()` not `showNotification()` | User in Shiny session sees no warning that the fallback was triggered (warnings are suppressed by `knitr::opts_chunk$set(warning = FALSE)` in the Rmd) |
| `analysis_pipeline.R:622–664` (per-well ED) | `tryCatch(..., error = function(e) data.frame(... NA_real_ ...))` | Per-well quantification failures silently produce NA concentrations |
| `report_pipeline.R:431` | `tryCatch(render(...), error = function(e) { showNotification(...); FALSE })` | Per-format render failure shown as notification but orchestrator continues; all-fail condition detected only after all formats tried |
| `utils_import_v3.R:246–258` | Overflow markers → NA with `warning()` only; no Shiny notification at this level | Server upload handler catches this via `tryCatch` and shows it, but only if the warning propagates correctly |
| `reports/report_functions.R:396` | `result$interpretation <<- paste("Heteroscedasticity assessment failed:", ...)` | Failure produces NA test statistics but the function returns a partial result; the report renders the partial result as if valid |

### Where Wrong Results Could Be Returned Silently

1. **Interpolation fallback with no model**: `quantify_samples()` passes `model_fit = NULL` to `drc::ED()`, which silently returns `NA` for all samples. Report shows "Not estimable" in the concentration column but no prominent warning that all values failed. The fit method string in `weight_desc` does indicate "log-linear interpolation" but this appears only in the model coefficients section, not adjacent to the sample table.

2. **Heteroscedasticity test with degenerate F**: `assess_heteroscedasticity()` caps `result$statistic <- 1e6` when F > 1×10⁶ and returns a partial interpretation string (`report_functions.R:336–347`). The auto-weighting logic in `fit_all_models()` uses the `p_value` from this, which may be NA in the degenerate case, causing it to fall through to the variance-ratio branch silently.

3. **ELISA normalization formula discrepancy**: Two different normalization formulas exist:
   - `utils_normalization.R:92`: `100 * (MeasurementValue - nsb_mean) / (b0_mean - nsb_mean)` — NSB-subtraction only, no Blank correction
   - `report_functions.R::calculate_elisa_bb0():192–207`: Cayman protocol with Blank correction: `(Value - blank_avg - nsb_avg) / corrected_b0 * 100`
   The pipeline calls `normalize_data()` (`utils_normalization.R`) for normalization, which uses the first (simpler) formula. The Rmd template calls `calculate_elisa_bb0()` from `report_functions.R` for the report display. These two code paths produce **different %B/B0 values** for the same input. The CSV artifact (`long_data_output.csv`) reflects the `utils_normalization.R` formula; the report displays results from `calculate_elisa_bb0()`.

---

## 9. Testing Infrastructure

### Frameworks
- `testthat` (version ≥ 3 inferred from `stop_on_failure` usage)
- `shinytest2` (skipped if not installed; requires Chrome via `chromote`)

### Test Files and Coverage

| File | What it tests |
|---|---|
| `test-analysis-pipeline.R` (73 lines) | `fit_all_models()`, `quantify_samples()`, `determine_lloq_uloq()` with synthetic data |
| `test-report-numbers.R` (156 lines) | Golden-value regression: RBA fit produces expected R², RMSE, IC50, sample CV |
| `test-utils_plate.R` (129 lines) | `matrix_to_long()`, `parse_dilution_cell/matrix()`, `enforce_plate_shape()` |
| `test-utils_normalization.R` (87 lines) | `calculate_elisa_bb0()`, `validate_controls()`, `normalize_data()` |
| `test-utils_import.R` (76 lines) | `detect_plate_location()`, `import_plate_data()` with `tempfile` fixtures |
| `test-dilution_parsing.R` (32 lines) | Ratio parsing: "1:2", "0.5", "3e-9", invalid strings |
| `test-report_functions.R` (62 lines) | `assess_heteroscedasticity()`, `assess_model_stability()` |
| `test-stats-config.R` (32 lines) | Existence and values of `STATS_CONFIG` keys |
| `test-smoke-import.R` (78 lines) | Smoke tests for `parse_plate_file()` with partial-plate CSV |
| `test-smoke-format-helpers.R` (61 lines) | `is_html_out()`, `render_table()`, `section_open()` format helpers |
| `test-integration.R` (93 lines) | End-to-end: matrix→long→fit→quantify for RBA and ELISA |
| `test-shinytest-rba.R` (62 lines) | `shinytest2::AppDriver` end-to-end: RBA demo preset → report HTML |
| `test-shinytest-elisa.R` (58 lines) | Same for ELISA cortisol preset |
| `test-shinytest-multiwavelength.R` (74 lines) | Multi-wavelength demo preset |

**Coverage measurement:** No `covr` setup. Coverage is not measured in CI.

**Snapshot tests:** No `_snaps/` directory; no `expect_snapshot_*` calls.

**Example datasets used by tests:** `tests/testthat/fixtures/rba_nominal.csv`, `elisa_nominal.csv`, `flat_response.csv`, `partial_plate_6col.csv`, `multiwave_synthetic.csv` (plus `tempfile` fixtures in `test-utils_import.R`).

**Golden artifact comparison:** `diff_golden_artifacts.R` exists in `tests/testthat/` but is not automatically called from any test file — appears to be a manual diffing utility.

---

## 10. Documentation

### README Completeness (JOSS Criteria)

| Criterion | Present? | Notes |
|---|---|---|
| Statement of need | Partial | First paragraph describes what it does; no explicit comparison to alternatives or description of target user community |
| Installation instructions | Yes | `shiny::runGitHub()` and `shiny::runApp()` (`README.md:88–96`) |
| Example usage | Yes | Screenshots + Quick Start + Try it with example data sections |
| Citation | Yes | DOI badge + `How to cite` section with Zenodo DOI |
| Contribution guide | **No** | No `CONTRIBUTING.md`, no contribution section in README |
| License | Yes | MIT badge + LICENSE file |
| Community guidelines | **No** | Only a feedback form URL (`README.md:292`) |

The `README.md` project structure table (`README.md:166–182`) lists files at the old (pre-refactor) location (e.g. `i18n.R` shown as root-level, not `server/i18n.R`).

### Vignettes
**None.** No `vignettes/` directory.

### In-App Help / Documentation
- Guided tour via `rintrojs` (`server_common.R`) with `data.intro` attributes on wizard sections.
- Contextual help text rendered via `renderUI` + `tr()` calls throughout server modules.
- `show_sample_layout` button in Tab 3 shows a default plate layout modal.
- Tooltip `title` attribute on the Generate Report button (`app.R:1066`).

### Function-Level Roxygen Documentation
Partial. `report_pipeline.R` functions have `@param` / `@return` roxygen-style comments. `utils_plate.R`, `utils_import_v3.R`, and `utils_normalization.R` have `#'` documentation. `server_common.R`, `server_layout.R`, `server_upload.R`, `server_config.R` server module functions have **no roxygen docs**. `DESCRIPTION` sets `RoxygenNote: 7.0.0` but no `NAMESPACE` is generated.

### CITATION.cff
Present. Contains: title, authors with ORCID, version 1.0.0, license MIT, Zenodo DOI (concept + version-specific). Note: the Zenodo version DOI is for "v0.9.0" (`CITATION.cff:39`) but the version field says "1.0.0".

### Zenodo
DOI `10.5281/zenodo.19691224` referenced in both README and CITATION.cff. Badges present.

---

## 11. Code Hygiene Markers

### TODO / FIXME / HACK / XXX

| Location | Text |
|---|---|
| `server/i18n.R:793` | `# TODO(i18n): "Readiness check" — verify with native ES speaker` |
| `server/i18n.R:1227` | `# TODO i18n-es` (ES translations for PR-A new strings incomplete) |
| `server/i18n.R:2011` | `# TODO(i18n): verify with native RU speaker` |
| `server/i18n.R:2619` | `# TODO(i18n): verify with native ZH speaker` |
| `tests/testthat/fixtures/generate_multiwave.R:60` | `# ---- Assemble CSV in the "Raw Data (XXX)" format ...` — XXX is format placeholder, not a marker |

### Commented-Out Code
- `tests/testthat/test-report-numbers.R:17`: `#   setwd(rprojroot::find_root(...))` — old approach commented out
- Various `#--- Section ---` dividers throughout; no substantial commented-out logic blocks found.

### Deprecated Function Usage
- `ggplot2::geom_line(..., size = 1.2)` in `unified_analysis_template.Rmd:1069` — `size` deprecated in ggplot2 ≥ 3.4.0 in favour of `linewidth`.

### Version Inconsistency
| Source | Version |
|---|---|
| `DESCRIPTION` | 2.0.0 |
| `app.R` comment (line 7) | 1.0.0 |
| `CITATION.cff` | 1.0.0 |
| `www` topbar badge (`app.R:121`) | v1.0.0 |
| `REPORT_INFO$version` in `report_constants.R` | 2.0 |
| Zenodo version DOI description in `CITATION.cff` | v0.9.0 |

### Potentially Unused Items
- `reports/create_reference_doc.R` — creates `reference.docx` for DOCX styling; used only if the output file already exists at `reports/reference.docx`. No evidence this is called in the normal workflow; no .rds or reference.docx found in `reports/`.
- `SAMPLE_TYPE_COLORS` in `global.R:313` — defined but not confirmed used anywhere outside potential theming contexts.
- `QC_STATUS_COLORS` in `global.R:325` — similar.
- `DEFAULT_STANDARDS` list in `report_constants.R:107` — defines RBA defaults different from `global.R:140` (different concentration series); only one can be authoritative.
- `diff_golden_artifacts.R` in `tests/testthat/` — not sourced by any test file; a standalone diffing utility.
- `alignment-root-cause.md`, `gui-fix-pass.md`, `report-design-review.md`, `v1.0-polish-plan.md`, `v1.0-usability-plan.md` — development planning documents checked into the repository root.

---

## 12. Open Questions for Stage 2

1. **ELISA normalization formula discrepancy (Section 8):** Are `utils_normalization.R:92` and `report_functions.R::calculate_elisa_bb0()` intentionally computing different %B/B0 formulas? The CSV artifact uses the simpler formula; the report displays the Cayman-protocol formula. This needs a definitive decision and a single code path.

2. **Interpolation fallback sample quantification:** When `model_fit = NULL` (interpolation path), `quantify_samples()` calls `drc::ED(NULL, ...)` which will error per-well and return all NA concentrations. The `interp_fun` stored in `all_models[[wt_key]]$interp_fun` is never called during sample quantification. Is interpolation-based sample quantification (using `approxfun` inversion) intentionally not implemented, or is this a silent regression?

3. **`bootstrap_iterations` discrepancy:** `STATS_CONFIG$bootstrap_iterations = 1000` (`report_constants.R:39`) but `report_sections.R:587` uses `STATS_CONFIG$bootstrap_iterations %||% 2000` — the fallback value is 2000, meaning if `STATS_CONFIG` is not loaded in scope the bootstrap uses 2000 iterations. Which is authoritative?

4. **`set.seed(42)` then `set.seed(NULL)` in bootstrap:** The seed reset to `NULL` after bootstrap (`report_functions.R:603`) restores randomness but also means results are reproducible within a session only if bootstrap is the first random operation. Is this the intended reproducibility contract?

5. **Process-global `Sys.setenv` in standalone mode (Section 7):** On a multi-user Shiny Server (not ShinyApps.io), the four `RBA_*` environment variables set in `app.R:77–81` are shared across all R sessions. If session-scoped copies in `session$userData` are sufficient for all downstream usage, the `Sys.setenv` calls in standalone mode may be removable. Confirm all paths that read `Sys.getenv("RBA_OUTPUT_DIR")` directly rather than via `session$userData`.

6. **`.TRANSLATIONS_CACHE <<-` in i18n.R:** The translation cache is a process-global singleton. In concurrent multi-user deployment this is read-only after first load (safe). But if any future call modifies a translation key (e.g. for personalisation), this would leak across sessions. Confirm that `get_translations()` is never called with mutable state.

7. **RU/ZH translation completeness:** `TODO i18n-es` markers at `i18n.R:1227` and TODO comments for RU/ZH (`i18n.R:2011, 2619`) indicate incomplete translations. The beta banner in the UI only shows for RU/ZH (`app.R:293`), not ES. What is the actual translation coverage percentage for each language?

8. **`ggplot2::size` deprecation:** `geom_line(..., size = 1.2)` at `unified_analysis_template.Rmd:1069` triggers a deprecation warning in ggplot2 ≥ 3.4.0. The Rmd has `warning = FALSE` globally so this is suppressed in reports. Needs updating to `linewidth`.

9. **`DEFAULT_STANDARDS` duplication:** `report_constants.R:108` defines `rba_saxitoxin` as `c(1e-6, 3e-7, 1e-7, ...)` (8-point series starting with `3e-7`), while `global.R:140` defines `DEFAULT_STX_CONC` as `c(1e-6, 1e-7, 3e-8, ...)` (different concentrations). Which is the authoritative standard series?

10. **`DESCRIPTION` version vs app version:** Five different version strings exist (Section 11). Which is canonical and what is the release process that keeps them synchronised?

11. **No `CONTRIBUTING.md`:** JOSS requires community guidelines. What contribution model is intended?

12. **`CITATION.cff` version DOI for v0.9.0:** The concept DOI resolves to the latest version, but the explicit version DOI (`10.5281/zenodo.19691223`) is labelled as v0.9.0 while the version field says 1.0.0. Is the Zenodo release for v1.0.0 still pending?

13. **`remotes` in `global.R` required_pkgs:** `remotes` is listed in the required packages (`global.R:33`) but not in `DESCRIPTION` Imports. This means `remotes` may be unavailable in environments that install from `DESCRIPTION` rather than from `global.R`.

14. **Shinytest2 tests without snapshot files:** The three `test-shinytest-*.R` files assert that an HTML file is produced and is non-trivially large, but do not assert any content. If the report renders but produces wrong results, these tests pass. Are golden output files (HTML diff) planned?

---

## Summary of Highest-Risk Areas

The audit reveals five clusters of elevated risk. First, the ELISA normalization code path is bifurcated: `utils_normalization.R` and `report_functions.R::calculate_elisa_bb0()` implement materially different %B/B0 formulas (the former omits Blank correction), the CSV artifact records one formula and the report renders the other, and no test currently checks whether these two outputs are numerically consistent. Second, the log-linear interpolation fallback is incomplete: when LL.4 and LL.3 both fail the `interp_fun` is stored but never used for sample back-calculation — all sample concentrations silently become NA without a prominent user warning, because the Rmd globally suppresses warnings and the only indicator is the model coefficients section deep in the report. Third, process-global environment variables (`RBA_OUTPUT_DIR` etc.) set in standalone mode create a race condition on multi-user Shiny Server deployments; although session-scoped `session$userData` copies mitigate this for the report pipeline, any code path that calls `Sys.getenv("RBA_OUTPUT_DIR")` directly (e.g. standalone Rmd renders) would read a stale value from a concurrent session. Fourth, there is no environment pinning (no renv, no package version constraints in DESCRIPTION, floating `any::` in CI), meaning a change in any upstream package — particularly `drc`, `ggplot2`, or `kableExtra` — could silently alter numerical results or break rendering without triggering a version-mismatch error. Fifth, version metadata is inconsistent across five locations (DESCRIPTION says 2.0.0, the UI badge and CITATION.cff say 1.0.0, report_constants.R says 2.0, and the Zenodo version DOI still points to v0.9.0), which poses a direct problem for the JOSS submission requirement of a citable, versioned release.

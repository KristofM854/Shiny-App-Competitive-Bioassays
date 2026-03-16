# Implementation Guide: Competitive Binding Assay Analysis Suite
## Deep Code Review & Roadmap for Claude Code

**Prepared for:** Kristof Moeller  
**Date:** 2026-03-16  
**Scope:** Dead code, redundancies, UX simplification, mobile feasibility, robustness, and edge cases

---

## 1. DEAD CODE & REDUNDANCIES

### 1.1 Duplicate Normalization Logic

**Problem:** `%B/B0` normalization is implemented **three separate times** with slightly different logic:

| Location | Function | Notes |
|----------|----------|-------|
| `utils_normalization.R` | `normalize_data()` → strategy `calculate()` | Uses `(Value - NSB) / (B0 - NSB) * 100` |
| `reports/report_functions.R` | `calculate_elisa_bb0()` | Uses `(Value - Blank) / ((B0 - Blank) - (NSB - Blank)) * 100` — blank-corrects first |
| `unified_analysis_template.Rmd` | Lines in `load-data` chunk | Calls `calculate_elisa_bb0()` but has fallback that uses `NormalizedValue` from the app |

The app's `normalize_data()` runs during report generation (`app.R` line ~700), but then the Rmd template **re-calculates** B/B0 from scratch using `calculate_elisa_bb0()`. This means the app normalization is effectively dead code for ELISA — the report always overwrites it.

**Fix:** Remove normalization from `app.R` report generation for ELISA. Let the Rmd template be the single source of truth. Alternatively, normalize once in the app and pass the result through without re-calculating.

### 1.2 Dead/Unreachable Code in `report_functions.R`

- `predict_sample_concentrations()` (lines 120-250): This entire function is **never called**. The Rmd template does its own `ED()` prediction loop in the `sample-analysis` chunk. The function has elaborate ELISA tissue-weight logic that is duplicated (and superseded) by the Rmd template's own implementation.

- `calculate_replicate_stats()` (end of file): Never called anywhere. The Rmd template computes replicate stats inline.

- `prepare_standards_for_modeling()`: Never called. The Rmd template does its own filtering.

- `inv_ll4_elisa()`: Defined in `report_functions.R` but never called from the Rmd template. The template uses `ED()` directly for all inverse predictions.

- `determine_dilution_status()`: Never called. The template uses `classify_range()` defined inline.

### 1.3 Redundant Helper Definitions

- `%||%` is defined in **four** places: `global.R`, `report_constants.R`, `unified_analysis_template.Rmd`, `multiwavelength_analysis_template.Rmd`. Each has a guard (`if (!exists(...))`), but this is fragile.

- `write_json_safe()` is defined in `global.R`, then redefined with guards in both Rmd templates.

- `get_script_dir()` is defined in both `global.R` and `run_analysis_modular.R` with different implementations.

### 1.4 Unused Variables and Reactives in `app.R`

- `rv$plate_data` and `rv$plate_layout`: Declared in `reactiveValues()` but never read or written.
- `rv_file_preview$selected_plates`: Declared but never used (the visual import reads `detected_plates` and checkbox inputs instead).
- The `excluded_wells_input` text input is referenced in the report generation block (`input$excluded_wells_input`) but **never created in the UI**. This means the excluded wells JSON is never written via that path — only the visual plate selector's click-to-exclude mechanism works.

### 1.5 Unused Package Loads

In `run_analysis_modular.R`, these packages are loaded but never used in that script: `blastula`, `fs`, `htmlwidgets`, `shinyBS`, `tinytex`, `zip`. They may be transitive dependencies but should be documented or removed from the explicit list.

### 1.6 Inconsistent ELISA Analyte Lists

- `global.R` defines `ELISA_ANALYTES` with cortisol, testosterone, estradiol, custom
- `app.R` UI only offers cortisol, testosterone, custom (missing estradiol)
- Default concentrations only exist for cortisol (`DEFAULT_CORTISOL_CONC`)
- `report_functions.R` has `MOLECULAR_WEIGHTS` for estradiol but no default concentrations

---

## 2. SIMPLIFYING DATA INPUT & REPORT GENERATION

### 2.1 Current Pain Points

1. **Too many matrices to fill:** Users must manually configure 4 separate 8×12 grids (Type, ID, Dilution, Replicate) plus optionally tissue weights. This is 384+ cells of manual input.

2. **No templates/presets:** Despite supporting multiple assay types, there are no one-click preset layouts like "Standard Cortisol ELISA" or "STX RBA with 8 standards."

3. **Three-step workflow feels linear but isn't:** The UI presents Steps 0-3 on a single scrolling page. Users can edit Step 1 after uploading data in Step 2, causing silent inconsistencies.

4. **Report generation kills the app:** `stopApp()` is called after report generation, forcing the user to restart if they want to re-run with different settings.

### 2.2 Recommended Simplifications

#### A. Preset Plate Layouts (Priority: HIGH)
Add a dropdown: "Load Preset Layout" with options:
- "RBA: 8 STX standards (triplicate, cols 1-3) + 16 samples (triplicate)"
- "ELISA: Cortisol (Cayman kit, 8-point, duplicate)"
- "ELISA: Custom (blank template)"
- "Custom from saved layout"

Implementation: Create a `presets/` folder with `.rds` files. The existing save/load mechanism already supports this — just pre-populate.

#### B. Auto-Fill ID and Replicate from Type Matrix
When a user sets wells to "Standard" in the Type matrix, auto-generate S1, S2... in the ID matrix and auto-assign replicate groups. Currently each matrix is independent, creating tedious redundancy.

#### C. Wizard-Style Tabs Instead of Single Page
Replace the single scrolling page with `navbarPage()` or `tabsetPanel()`:
- Tab 1: Assay Config + Standards
- Tab 2: Plate Layout (all matrices)
- Tab 3: Upload & Preview
- Tab 4: Analysis Settings + Generate

Add validation gates: Tab 3 is locked until Tab 2 is valid, etc.

#### D. Remove `stopApp()` After Report
Let users iterate. Add a "Download Report" button that saves to the output folder without killing the session. Use `downloadHandler()` for direct browser downloads.

#### E. Simplify Dilution Matrix
Most users use uniform dilutions. Add a "Set all dilutions to:" input with a single value, plus an "Advanced: per-well" toggle that reveals the full matrix.

#### F. Smart Standard Auto-Detection
If the uploaded file has more than 96 cells or contains wavelength labels, auto-detect multi-wavelength mode without requiring user action. Currently works for "Raw Data (XXXnm)" markers but not for other common plate reader formats (e.g., BioTek Gen5, Molecular Devices SoftMax).


## 4. ROBUSTNESS FOR PUBLIC RELEASE

### 4.1 Critical Issues

#### A. No Input Validation on File Upload
`import_plate_data()` can silently return garbage if the file has:
- Non-numeric values that coerce to NA
- More than one plate-like region (it takes the first match)
- Unicode or locale-specific decimal separators (comma vs period)

**Fix:** Add strict validation after import: check that ≥50% of wells are finite numeric values, warn on high NA count, reject if <4 standards have valid data.

#### B. DRC Model Fitting Has No Fallback
If `drc::drm()` fails (convergence failure, singular gradient), the entire report generation crashes. The `tryCatch` in the Rmd template catches the error but provides no useful output — the user gets a broken report.

**Fix:** Implement a 3-tier fallback:
1. Try LL.4() with user-selected weighting
2. Fall back to LL.3() (fixed bottom)
3. Fall back to simple log-linear interpolation
4. If all fail, show standards data table with a clear error message

#### C. ELISA Control Well Validation Is Incomplete
`calculate_elisa_bb0()` checks `B0 > NSB > Blank` but:
- Doesn't check that B0 and NSB are blank-corrected before comparing
- Doesn't validate minimum number of control replicates (single B0 well would pass)
- Doesn't flag if TotalActivity is lower than B0 (indicates assay failure)

#### D. `stopApp()` Prevents Error Recovery
If report rendering fails, the app has already called `stopApp()` in the `observeEvent(input$convert, ...)` block. The user loses all entered data.

**Fix:** Move `stopApp()` to a separate "Close App" button, or remove it entirely.

#### E. No Session State Persistence
If the browser tab closes or refreshes, all matrix edits are lost. The save/load layout feature mitigates this, but it requires manual action.

**Fix:** Auto-save session state to a temp file every 30 seconds using `reactiveTimer()`.

#### F. Concurrency Issues with Environment Variables
The app uses `Sys.setenv()` / `Sys.getenv()` for output paths. If two instances run simultaneously (e.g., on ShinyProxy), they'll overwrite each other's environment variables.

**Fix:** Use `session$userData` or pass paths through reactive values instead of environment variables.

#### G. Report Template Path Resolution Is Fragile
The Rmd templates search for `i18n.R` and helper files across 8+ candidate paths. This works locally but fails in Docker, package installations, or when `knit_root_dir` changes.

**Fix:** Bundle all report dependencies in a single directory and use a single reliable path resolution strategy.

### 4.2 Missing Features for Public Release

1. **No authentication or multi-user support** — fine for desktop, problematic for hosted deployment
2. **No data export besides CSV** — add JSON, RDS, and clipboard copy
3. **No undo/redo for matrix edits** — one wrong click in a 96-well grid loses data
4. **No input data preview heatmap** — users can't verify their upload visually before analysis
5. **No batch processing** — can only analyze one plate at a time
6. **No dark mode** — minor but expected for modern apps
7. **No accessibility** — no ARIA labels, keyboard navigation, or screen reader support
8. **No loading spinners** — `withProgress()` is used but the UI doesn't indicate what's happening during long operations
9. **No version checking** — no way to tell users their installed packages are outdated

---

## 5. EDGE CASES FOR CLAUDE CODE

### 5.1 File Import Edge Cases

```
EDGE_CASE_001: Excel file with merged cells in plate region
  Input: .xlsx where row labels A-H are merged across 2 rows each
  Expected: Detect merged cells, unmerge, and extract correctly
  Current: Silently produces wrong mapping (16 rows instead of 8)

EDGE_CASE_002: CSV with comma decimal separator (European locale)
  Input: "0,034\t0,128\t0,256" (tab-separated, comma decimals)
  Expected: Parse correctly or warn about locale
  Current: All values become NA

EDGE_CASE_003: Excel file with multiple sheets, plate on sheet 2
  Input: .xlsx with metadata on sheet 1, plate data on sheet 2
  Expected: Detect plate on correct sheet
  Current: Only reads sheet 1 (hardcoded default)

EDGE_CASE_004: Plate reader output with >12 columns (e.g., temperature column)
  Input: 8×13 grid where column 13 is "Temperature"
  Expected: Ignore non-numeric trailing columns
  Current: May include temperature as column 12 data

EDGE_CASE_005: File with plate data starting at row 50+ (long header)
  Input: Plate reader file with 40 lines of metadata before A-H block
  Expected: Detect plate regardless of header length
  Current: Works (scans all rows) but very slow for large files

EDGE_CASE_006: Partial plate with only columns 1-6 filled
  Input: Half-plate experiment (48 wells)
  Expected: Import correctly, treat columns 7-12 as NA
  Current: Works but no visual indication to user that plate is partial

EDGE_CASE_007: Plate data with "#SAT" or "OVER" overflow markers
  Input: Wells that exceeded detector range show "OVER" instead of number
  Expected: Convert to NA with warning about saturated wells
  Current: Entire column may fail numeric conversion

EDGE_CASE_008: Multi-wavelength file with different plate layouts per wavelength
  Input: 450nm has full plate, 540nm has only standards (reference wavelength)
  Expected: Handle asymmetric plates gracefully
  Current: Assumes all wavelengths have identical layout

EDGE_CASE_009: .txt file with mixed tab and space delimiters
  Input: Some plate readers use inconsistent whitespace
  Expected: Auto-detect delimiter
  Current: Hardcoded sep="\t" in read.table()

EDGE_CASE_010: File upload with 0-byte file
  Input: Empty file
  Expected: Clear error message
  Current: Cryptic R error
```

### 5.2 Plate Layout Edge Cases

```
EDGE_CASE_011: User sets all wells to "Standard" (no samples)
  Expected: Generate report with DRC only, skip sample quantification section
  Current: Report crashes looking for Sample wells

EDGE_CASE_012: User sets 0 standards
  Expected: Disable report generation with clear message
  Current: num_standards=0 is allowed in UI but causes crash downstream

EDGE_CASE_013: User assigns same SampleID to wells with different DilutionFactors
  Expected: Treat as serial dilution, group correctly in replicate stats
  Current: May double-count or misgroup

EDGE_CASE_014: Dilution factor "1:0" (division by zero)
  Expected: Reject with validation error
  Current: parse_dilution_cell() returns valid=FALSE but error message is generic

EDGE_CASE_015: Replicate group with only 1 well
  Expected: Skip CV calculation, show "n=1" in results
  Current: CV returns NA, some functions may error on sd() of length-1 vector

EDGE_CASE_016: ELISA plate with no Blank wells assigned
  Expected: Clear error before report generation
  Current: Crashes during normalization with unhelpful "Missing required ELISA controls"

EDGE_CASE_017: ELISA plate with NSB > B0 (assay failure)
  Expected: Stop with "Assay failure detected: NSB absorbance exceeds B0"
  Current: Warning only, continues with meaningless negative %B/B0 values

EDGE_CASE_018: User changes assay type from RBA to ELISA after filling matrices
  Expected: Confirm before resetting, or adapt existing layout
  Current: Resets all matrices silently (has prev_assay_type guard but still resets)

EDGE_CASE_019: Standard concentrations not in descending order
  Expected: Auto-sort or warn
  Current: DRC fitting may produce inverted curves

EDGE_CASE_020: Duplicate standard concentrations (e.g., S3 and S4 both set to 1e-8)
  Expected: Warn about duplicates
  Current: Silently accepted, may cause DRC issues
```

### 5.3 DRC Fitting Edge Cases

```
EDGE_CASE_021: All standard replicates have identical values (zero variance)
  Expected: Warn about suspicious data, proceed with single points
  Current: CV=0 passes all checks, but DRC may still fail

EDGE_CASE_022: Standards show non-monotonic response (hook effect)
  Expected: Detect and warn about hook effect, suggest excluding highest concentration
  Current: DRC fitting may converge to wrong curve or fail

EDGE_CASE_023: Fewer than 4 unique standard concentrations after high-CV exclusion
  Expected: Skip high-CV exclusion, fit with all standards, warn
  Current: Crashes with "Insufficient standards"

EDGE_CASE_024: Hill slope > 3 or < 0.1 (biologically implausible)
  Expected: Flag in QC card, suggest data review
  Current: Reported but not flagged as unusual

EDGE_CASE_025: IC50 outside standard concentration range
  Expected: Warn that extrapolation is unreliable
  Current: Silently extrapolates

EDGE_CASE_026: Model R² < 0.8 (poor fit)
  Expected: Strong warning, suggest alternative model or data review
  Current: QC card shows red but report continues normally

EDGE_CASE_027: Bootstrap CI with n=2 replicates
  Expected: Fall back to t-distribution (bootstrap unreliable for n<3)
  Current: Attempts bootstrap with 2 values, produces nonsensical CIs

EDGE_CASE_028: All samples fall outside quantification range
  Expected: Clear message "No samples within LLOQ-ULOQ range"
  Current: Empty results table with no explanation

EDGE_CASE_029: 1/Y² weighting with response values near zero
  Expected: Cap weights to prevent numerical instability
  Current: Weights can become Inf, causing DRC failure

EDGE_CASE_030: Multiple weighting comparison where one model fails
  Expected: Show results for successful models, note which failed
  Current: If first (primary) model fails, entire report fails
```

### 5.4 Report Generation Edge Cases

```
EDGE_CASE_031: Report language set to Spanish but i18n.R not found
  Expected: Fall back to English with warning
  Current: All text shows as "[key_name]" placeholders

EDGE_CASE_032: Word output with plotly plots
  Expected: Gracefully degrade to static ggplot
  Current: Works (has is_html_output() check) but plot quality is lower

EDGE_CASE_033: Output directory path contains spaces or Unicode
  Expected: Handle correctly
  Current: normalizePath() may fail on some systems

EDGE_CASE_034: Very long sample IDs (>50 characters)
  Expected: Truncate or wrap in tables/plots
  Current: Tables overflow, plot labels overlap

EDGE_CASE_035: >50 replicate groups (large experiment)
  Expected: Paginate results table, split plot into panels
  Current: Single giant table and overlapping boxplot

EDGE_CASE_036: Multi-wavelength with >5 wavelengths
  Expected: Concordance analysis handles n*(n-1)/2 pairs efficiently
  Current: O(n²) Bland-Altman plots may make report very long

EDGE_CASE_037: Tissue weight = 0 for some samples
  Expected: Skip tissue calculation for those samples, show pg/mL only
  Current: Division by zero in concentration_pg_per_g calculation

EDGE_CASE_038: DOCX report generation without pandoc installed
  Expected: Clear error message suggesting pandoc installation
  Current: Cryptic rmarkdown error

EDGE_CASE_039: HTML report > 10MB (many plotly plots)
  Expected: Warn about file size, offer to use static plots
  Current: Silently generates huge file that may not open in browsers

EDGE_CASE_040: Concurrent report generation (two users, hosted deployment)
  Expected: Independent output directories, no cross-contamination
  Current: Environment variables create race condition
```

### 5.5 UI/UX Edge Cases

```
EDGE_CASE_041: User pastes tab-separated data into rhandsontable cell
  Expected: Distribute across cells (spreadsheet paste behavior)
  Current: rhandsontable may handle this but not tested

EDGE_CASE_042: Browser zoom > 150% or < 75%
  Expected: Responsive layout adapts
  Current: Fixed-width elements may overflow or underflow

EDGE_CASE_043: Screen width < 768px (tablet/phone)
  Expected: Stack columns vertically
  Current: Two-column layout clips on narrow screens

EDGE_CASE_044: User clicks "Generate Report" twice rapidly
  Expected: Debounce, show spinner, disable button
  Current: Button is disabled via observe() but timing depends on Shiny reactivity cycle

EDGE_CASE_045: Internet disconnection during runGitHub() execution
  Expected: Cached packages used, clear error if source files can't download
  Current: Partial download may leave corrupted state

EDGE_CASE_046: R version < 4.2 (requirement not enforced)
  Expected: Check R version at startup, show clear message
  Current: May fail with cryptic pipe operator or other syntax errors

EDGE_CASE_047: User uploads .xlsm (macro-enabled Excel)
  Expected: Accept and read data (macros are harmless for data extraction)
  Current: File extension not in accepted list, rejected

EDGE_CASE_048: Layout save when ~/.bioassay_layouts directory is not writable
  Expected: Clear error with alternative location suggestion
  Current: Silent failure

EDGE_CASE_049: Guided tour (rintrojs) on elements hidden by conditionalPanel
  Expected: Skip hidden steps or show relevant steps only
  Current: Tour highlights invisible elements (e.g., tissue weight section when RBA selected)

EDGE_CASE_050: User enters standard concentration as "1e-6 mol/L" (with units in text field)
  Expected: Strip units, parse number
  Current: as.numeric() returns NA
```

### 5.6 Specific Code-Level Bugs

```
BUG_001: app.R line ~730 - normalize_data() is called inside observeEvent(input$convert)
  but the ELISA normalization requires control wells (Blank, NSB, B0) which are filtered
  out by matrix_to_long() if user accidentally set them as "Other" instead of their
  correct ELISA type. No pre-check validates that control wells exist before attempting
  normalization.

BUG_002: report_functions.R calculate_elisa_bb0() uses "B0" and "MaximumBinding" as
  equivalent SampleTypes, but the app UI only offers "B0". The "MaximumBinding" alias
  is dead code that creates confusion.

BUG_003: unified_analysis_template.Rmd, model-fitting chunk:
  weight_options$inv_y$weights and $inv_y2$weights are computed from resp_vals BEFORE
  high-variability exclusion is applied. The weights are then used with
  standards_for_model which HAS been filtered. Length mismatch will crash drc::drm().
  The weights vector has length = nrow(original standards), but data has 
  length = nrow(filtered standards).

BUG_004: utils_plate.R create_replicate_matrix() for ELISA: Column 1 uses hardcoded
  control names ("Blank", "NSB", "B0", "TotalActivity") as replicate labels. This means
  control wells are treated as replicate groups, and their "mean concentration" appears
  in the results summary table. They should be excluded from replicate stats.

BUG_005: app.R visual plate selector: rv_file_preview$excluded_wells is a reactiveValues
  list that uses string keys like "plate_1_A3". But when the user re-uploads a file,
  the excluded wells from the previous file persist because rv_file_preview$excluded_wells
  is never cleared on new upload.

BUG_006: multiwavelength_analysis_template.Rmd: The preprocess_template_chunks() function
  renames chunk labels to avoid duplicates when knit_child() is called multiple times.
  But it uses a simple regex that can match partial names (e.g., chunk "setup" in one
  wavelength would conflict with "setup-extended" in another if the prefix were identical).

BUG_007: app.R tissue_weight_table: The isolate() call on tissue_weights_rv() prevents
  re-render on edit (good), but if replicate_groups() changes (user edits replicate matrix),
  the tissue weight table won't update to show new groups until a full re-render is triggered.

BUG_008: unified_analysis_template.Rmd save-model-stats chunk: model_stats$mean_sample_cv
  references replicate_stats which may not exist yet at that point in the Rmd execution
  flow (it's defined later in the sample-analysis chunk). The chunk order matters and
  this dependency is fragile.
```

---

## 6. PRIORITY IMPLEMENTATION ROADMAP

### Phase 1: Bug Fixes & Stability (1-2 weeks)

1. Fix BUG_003 (weight vector length mismatch) — **critical crash bug**
2. Fix BUG_005 (stale excluded wells on re-upload)
3. Fix BUG_008 (chunk execution order in Rmd)
4. Remove `stopApp()` from report generation
5. Add input validation gate before report generation (check control wells exist for ELISA)
6. Add R version check at startup

### Phase 2: Code Cleanup (1 week)

1. Delete dead functions from `report_functions.R` (predict_sample_concentrations, calculate_replicate_stats, prepare_standards_for_modeling, inv_ll4_elisa, determine_dilution_status)
2. Consolidate normalization to single location
3. Remove redundant `%||%` and `write_json_safe()` definitions
4. Remove unused `rv$plate_data`, `rv$plate_layout`
5. Remove `excluded_wells_input` reference from report generation
6. Add estradiol to app UI to match `ELISA_ANALYTES` constant

### Phase 3: UX Improvements (2-3 weeks)

1. Add preset plate layouts (3-5 common configurations)
2. Auto-fill ID matrix from Type matrix changes
3. Replace scrolling page with tabbed wizard
4. Add input data heatmap preview after upload
5. Add "Download Report" button instead of filesystem save
6. Simplify dilution matrix with "uniform dilution" shortcut

### Phase 4: Robustness (2 weeks)

1. DRC fitting fallback chain (LL.4 → LL.3 → interpolation)
2. Locale-aware decimal parsing in file import
3. Handle #SAT/OVER/ERR markers in plate data
4. Auto-detect plate on non-first Excel sheet
5. Session state auto-save
6. Replace environment variables with session-scoped paths

---

## 7. ARCHITECTURE RECOMMENDATIONS

### Current Architecture (Monolithic Shiny)
```
app.R (UI + Server, ~900 lines)
├── global.R (packages, constants)
├── utils_*.R (plate, import, normalization)
├── i18n.R (translations)
└── reports/
    ├── unified_analysis_template.Rmd (700+ lines, does its own analysis)
    ├── report_functions.R (mostly dead code)
    ├── report_constants.R
    └── plot_functions.R (partially used)
```

### Recommended Architecture (Modular)
```
R/
├── analysis/
│   ├── drc_fitting.R        # Single DRC fitting + fallback logic
│   ├── normalization.R      # Single normalization pipeline
│   ├── quantification.R     # Sample prediction + CI
│   ├── qc_assessment.R      # Traffic light QC
│   └── outlier_detection.R  # Dixon/Grubbs
├── import/
│   ├── file_import.R        # Unified import (single + multi-wavelength)
│   └── plate_layout.R       # Matrix creation + presets
├── export/
│   ├── report_renderer.R    # Rmd rendering wrapper
│   └── data_export.R        # CSV, JSON, RDS export
├── i18n/
│   └── translations.R       # All translations
└── app/
    ├── ui.R                 # UI only
    ├── server.R             # Server only
    └── modules/             # Shiny modules
        ├── mod_config.R     # Step 0: Assay config
        ├── mod_layout.R     # Step 1: Plate layout
        ├── mod_upload.R     # Step 2: File upload
        └── mod_report.R     # Step 3: Report generation

reports/
├── unified_template.Rmd     # Thin template (calls R/ functions)
└── multiwavelength_template.Rmd
```

Key principle: **Rmd templates should only render, not analyze.** All computation should happen in `R/analysis/` functions that are tested independently.

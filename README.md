# Competitive Binding Assay Analysis Suite

A Shiny web application for analyzing competitive binding assay data from 96-well plate readers. Supports both **Receptor Binding Assays (RBA)** and **Enzyme-Linked Immunosorbent Assays (ELISA)** with bilingual reporting (English/Spanish).

Developed by **Arnold Molina Porras** (University of Costa Rica) and **Kristof Moeller** (IAEA).

## Quick Start

Run directly from GitHub (requires R >= 4.2):

```r
shiny::runGitHub("Shiny-App-Competitive-Bioassays", "KristofM854")
```

Or clone and run locally:

```r
source("run_analysis_modular.R")
```

## Features

### Assay Support
- **RBA**: Saxitoxin, Brevetoxin (PbTx-2/PbTx-3), Ciguatoxin variants
- **ELISA**: Cortisol, Testosterone, Estradiol, Custom analytes
- **Multi-wavelength**: Automatic detection and concordance analysis across wavelengths

### Plate Configuration
- Interactive 96-well plate editor for Sample Type, Sample ID, Dilution Factors, and Replicate Groups
- Pre-filled layouts for RBA (paired standards/samples) and ELISA (control column + paired samples)
- Save and load plate layouts for reuse across experiments

### Data Import
- Smart file import: auto-detects plate data in `.xlsx`, `.csv`, and `.txt` files
- Dual detection strategy: row-label matching (A-H) and numeric block detection
- Visual plate preview with well-level exclusion
- Multi-wavelength Excel files parsed automatically

### Statistical Analysis
- **4-Parameter Logistic (4PL)** dose-response curve fitting via the `drc` package
- **Multiple regression weightings**: Unweighted, 1/Y, 1/Y^2 -- select multiple for side-by-side comparison
- **Quantification range**: Configurable %B/B0 bounds for LLOQ/ULOQ flagging
- **Confidence intervals**: t-distribution (default) or bootstrap (1000 resamples)
- **Outlier detection**: Dixon's Q-test (n=3-5) or Grubbs' test (n>=6)
- **QC traffic-light card**: R^2, RMSE, Hill slope, EC50 SE, sample CV assessment

### ELISA-Specific
- %B/B0 normalization following Cayman protocol (Blank, NSB, B0 control wells)
- Tissue weight entry per replicate group for pg/g tissue calculations
- Configurable extraction volume

### Reporting
- HTML and/or Word (DOCX) output formats
- Bilingual: English and Spanish (480+ translated keys)
- Interactive plots (Plotly hover tooltips in HTML)
- Sections: Standard curve, DRC fit, model diagnostics, sample quantification, QC card, plate heatmap
- Multi-wavelength: Per-wavelength analysis + Lin's Concordance and Bland-Altman comparison

## Project Structure

```
.
├── app.R                          # Main Shiny app (UI + server)
├── global.R                       # Packages, constants, theme, helpers
├── i18n.R                         # Bilingual translations (EN/ES)
├── run_analysis_modular.R         # Local entry point (env setup + report render)
├── utils_plate.R                  # Plate matrix creation and conversion
├── utils_import_v3.R              # Smart plate reader file import
├── utils_import_multiwavelength.R # Multi-wavelength Excel parsing
├── utils_normalization.R          # ELISA %B/B0 normalization
└── reports/
    ├── unified_analysis_template.Rmd         # Single-wavelength report
    ├── multiwavelength_analysis_template.Rmd # Multi-wavelength wrapper
    ├── report_functions.R                    # Analysis functions (DRC, QC)
    ├── report_constants.R                    # Report-specific constants
    └── plot_functions.R                      # Standardized plot/table rendering
```

## Dependencies

### Required R Packages

| Category | Packages |
|----------|----------|
| **Shiny** | shiny, shinyjs, shinyFeedback, rintrojs, rhandsontable |
| **Data** | dplyr, tidyr, tibble, stringr, purrr, readr |
| **Plots** | ggplot2, ggrepel, ggthemes, ggtext, plotly, scales, patchwork |
| **Analysis** | drc, knitr, rmarkdown, kableExtra |
| **I/O** | readxl, jsonlite, digest |

### Known Issue: MASS::select Masking

The `drc` package loads `MASS`, which masks `dplyr::select()`. All `select()` calls in this codebase use the explicit `dplyr::select()` form. If you add new code, always use `dplyr::select()`.

## Workflow

1. **Configure Assay** (Step 0): Select RBA or ELISA, choose analyte, enter standard concentrations
2. **Edit Plate Layout** (Step 1): Define Sample Types, IDs, Dilution Factors, and Replicate Groups using the interactive plate editors
3. **Upload Data** (Step 2): Upload plate reader file; app auto-detects format and displays preview
4. **Set Analysis Options**: Choose regression weighting(s), quantification range, CI method, and outlier detection
5. **Generate Report** (Step 3): Select output format(s) and language, click Generate Report

## Output

Reports and data files are saved to a date-stamped folder (e.g., `2026-02-22/`). If the folder already exists, a suffix is appended (`_01`, `_02`, etc.).

Output files include:
- `long_data_output.csv` -- Plate data in long format
- `unknown_results.csv` -- Individual sample predictions
- `unknown_results_summary.csv` -- Replicate-level summary statistics
- `model_stats.json` -- DRC model parameters and QC metrics
- `analysis_report.html` / `.docx` -- Final report

## Contact

For questions or bug reports: kr.moeller@iaea.org

[Give Feedback](https://forms.office.com/e/q8eqJfp4QM)

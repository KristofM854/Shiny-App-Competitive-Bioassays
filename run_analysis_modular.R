# ==============================================================================
# Run Analysis Script
# Purpose: Entry point for local execution of the analysis workflow.
#          Sets up environment variables, launches the Shiny app, then renders
#          the Rmd report(s) after the app closes.
#
# Workflow:
#   1. Install/load required packages
#   2. Set up output directory with date-based naming
#   3. Export paths as environment variables (RBA_OUTPUT_DIR, etc.)
#   4. Launch Shiny app (app.R) for data entry
#   5. After app closes, detect single vs. multi-wavelength data
#   6. Render report(s) in selected format(s) (HTML, DOCX)
#
# NOTE: For runGitHub() usage, app.R handles standalone mode directly.
#       This script is only needed for local/scripted execution.
# ==============================================================================

# ------------------------------------------------------------------------------
# 0. Package Installation & Loading
# ------------------------------------------------------------------------------

required_pkgs <- c(
  "digest", "drc", "dplyr", "glue",
  "ggplot2", "ggrepel", "ggthemes", "ggtext", "htmltools",
  "jsonlite", "kableExtra", "knitr", "patchwork", "plotly", "purrr",
  "readr", "readxl", "rintrojs", "rmarkdown", "rhandsontable",
  "scales", "shiny", "shinyFeedback", "shinyjs",
  "stringr", "tibble", "tidyr", "xfun"
)
# Removed unused packages: blastula, fs, htmlwidgets, shinyBS, tinytex, zip

# Install missing packages
missing_pkgs <- required_pkgs[!sapply(required_pkgs, requireNamespace, quietly = TRUE)]
if(length(missing_pkgs) > 0) {
  message("Installing missing packages: ", paste(missing_pkgs, collapse = ", "))
  install.packages(missing_pkgs)
}

# Ensure xfun version >= 0.54
if (!requireNamespace("xfun", quietly = TRUE) || utils::packageVersion("xfun") < "0.54") {
  install.packages("xfun")
}

# Load all packages
invisible(lapply(required_pkgs, library, character.only = TRUE))

# ------------------------------------------------------------------------------
# 1. Define Paths
# ------------------------------------------------------------------------------

get_script_dir <- function() {
  ctx <- try(rstudioapi::getActiveDocumentContext(), silent = TRUE)
  if (inherits(ctx, "try-error") || is.null(ctx$path) || ctx$path == "") {
    stop(
      "❌ Could not determine script location.\n",
      "➡️ Please run the script again (Ctrl+A → Run, or click 'Source').\n",
      "   RStudio sometimes needs a second run to detect the active document."
    )
  }
  dirname(ctx$path)
}

script_dir <- get_script_dir()
app_dir <- script_dir  # app.R should be in same folder

report_template <- file.path(script_dir, "reports", "unified_analysis_template.Rmd")

# Debug: Check if template file exists
if (!file.exists(report_template)) {
  stop("❌ Template file not found at: ", report_template, 
       "\nMake sure you have created the 'reports/' folder and copied analysis_template.Rmd there.")
}

message("📄 Using template: ", basename(report_template))
report_template <- normalizePath(report_template, winslash = "/", mustWork = TRUE)

# ------------------------------------------------------------------------------
# 2. Create Output Directory
# ------------------------------------------------------------------------------

report_date <- Sys.Date()
base_output_dir <- file.path(script_dir, format(report_date, "%Y-%m-%d"))
output_dir <- base_output_dir

# Handle existing directories (append _1, _2, etc.)
if (dir.exists(output_dir)) {
  existing_dirs <- list.dirs(path = script_dir, full.names = FALSE, recursive = FALSE)
  pattern <- paste0("^", format(report_date, "%Y-%m-%d"), "(?:_\\d+)?$")
  suffix_nums <- as.integer(sub(
    paste0("^", format(report_date, "%Y-%m-%d"), "_?(\\d*)$"),
    "\\1",
    existing_dirs[grepl(pattern, existing_dirs)]
  ))
  suffix_nums[is.na(suffix_nums)] <- 1
  next_suffix <- max(suffix_nums) + 1
  output_dir <- paste0(base_output_dir, "_", next_suffix)
}

dir.create(output_dir, showWarnings = FALSE, recursive = TRUE)
message("✅ Output directory created: ", output_dir)

# ------------------------------------------------------------------------------
# 3. Export Paths as Environment Variables (for Shiny app)
# ------------------------------------------------------------------------------

output_csv     <- file.path(output_dir, "long_data_output.csv")
formats_json   <- file.path(output_dir, "selected_formats.json")
notes_file     <- file.path(output_dir, "notes.json")
qc_params_file <- file.path(output_dir, "qc_params.json")

# Ensure directory exists before normalizePath
dir.create(output_dir, recursive = TRUE, showWarnings = FALSE)

# Make absolute paths
output_csv     <- normalizePath(output_csv, winslash = "/", mustWork = FALSE)
formats_json   <- normalizePath(formats_json, winslash = "/", mustWork = FALSE)
notes_file     <- normalizePath(notes_file, winslash = "/", mustWork = FALSE)
qc_params_file <- normalizePath(qc_params_file, winslash = "/", mustWork = FALSE)

# Set environment variables
Sys.setenv(RBA_OUTPUT_DIR = output_dir)
Sys.setenv(RBA_CSV_PATH   = output_csv)
Sys.setenv(RBA_FMT_JSON   = formats_json)
Sys.setenv(RBA_NOTES_FILE = notes_file)

# ------------------------------------------------------------------------------
# 4. Run Shiny App
# ------------------------------------------------------------------------------

message("🚀 Launching Shiny app...")
message("   App directory: ", app_dir)

# Run the modular app
shiny::runApp(file.path(app_dir, "app.R"))

message("✅ Shiny app closed. Continuing with report generation...")

# ------------------------------------------------------------------------------
# 5. Validate Output Files
# ------------------------------------------------------------------------------

# CSV (required)
if (!file.exists(output_csv)) {
  stop("❌ CSV not found. App may not have saved it.")
}

# Formats JSON (required)
if (!file.exists(formats_json)) {
  stop("❌ Formats JSON not found. App may not have saved it.")
} else {
  txt <- readLines(formats_json, warn = FALSE)
  if (!jsonlite::validate(paste(txt, collapse = "\n"))) {
    stop("❌ Formats JSON exists but is not valid JSON.")
  }
}

# Notes (optional)
if (!file.exists(notes_file)) {
  warning("⚠️ Notes JSON missing — report will show 'No notes added'.")
} else {
  txt <- readLines(notes_file, warn = FALSE)
  if (!jsonlite::validate(paste(txt, collapse = "\n"))) {
    warning("⚠️ Notes JSON is invalid — report will show 'No notes added'.")
  }
}

# QC params (optional but recommended)
if (!file.exists(qc_params_file)) {
  warning("⚠️ QC params JSON missing — report may use defaults.")
} else {
  txt <- readLines(qc_params_file, warn = FALSE)
  if (!jsonlite::validate(paste(txt, collapse = "\n"))) {
    stop("❌ QC params JSON is invalid.")
  }
}

message("✅ All required files validated")

# ------------------------------------------------------------------------------
# 5.1 Check for Multi-Wavelength Data
# ------------------------------------------------------------------------------

wavelength_manifest_file <- file.path(output_dir, "wavelength_manifest.json")

if (file.exists(wavelength_manifest_file)) {
  # Multi-wavelength mode
  manifest <- jsonlite::fromJSON(wavelength_manifest_file)
  wavelengths <- manifest$wavelengths
  
  message(sprintf("📊 Multi-wavelength data detected: %s", 
                  paste(wavelengths, collapse = ", ")))
  
  is_multiwavelength <- TRUE
  
} else {
  # Standard single-wavelength mode
  is_multiwavelength <- FALSE
  wavelengths <- NULL
}

# ------------------------------------------------------------------------------
# 6. Load JSON Outputs
# ------------------------------------------------------------------------------

selected_formats <- jsonlite::fromJSON(formats_json)

# Notes: safe read (optional)
notes <- if (file.exists(notes_file) &&
             jsonlite::validate(paste(readLines(notes_file, warn = FALSE), collapse = "\n"))) {
  jsonlite::fromJSON(notes_file)
} else {
  list(notes = "", saved_at = NA)
}

# Load report language preference
report_lang_file <- file.path(output_dir, "report_language.json")
report_lang <- "en"
if (file.exists(report_lang_file)) {
  lang_data <- tryCatch(jsonlite::fromJSON(report_lang_file), error = function(e) list(lang = "en"))
  report_lang <- lang_data$lang %||% "en"
}
message("📝 Report language: ", report_lang)

# ------------------------------------------------------------------------------
# 7. Render Reports
# ------------------------------------------------------------------------------
# ------------------------------------------------------------------------------
# 7. Render Reports
# ------------------------------------------------------------------------------

formats_map <- list(
  html = "html_document",
  pdf  = "pdf_document",
  docx = "word_document"
)

# Track successful renders
successful_renders <- c()
failed_renders <- c()

# Select appropriate template
if (is_multiwavelength) {
  # Use multi-wavelength template
  report_template <- file.path(script_dir, "reports", "multiwavelength_analysis_template.Rmd")
  
  if (!file.exists(report_template)) {
    stop("❌ Multi-wavelength template not found at: ", report_template,
         "\nMake sure multiwavelength_analysis_template.Rmd is in the reports/ folder.")
  }
  
  message("📊 Using multi-wavelength analysis template")
  
  for (fmt in selected_formats) {
    message(sprintf("📄 Rendering multi-wavelength %s report...", toupper(fmt)))
    
    render_result <- tryCatch({
      rmarkdown::render(
        input = report_template,
        output_format = formats_map[[fmt]],
        output_file = "Multi-Wavelength-Analysis-Report",
        output_dir = output_dir,
        params = list(
          output_dir = normalizePath(output_dir, winslash = "/", mustWork = TRUE),
          wavelengths = wavelengths,
          lang = report_lang
        ),
        knit_root_dir = dirname(report_template),
        envir = new.env(parent = globalenv())
      )
      
      successful_renders <- c(successful_renders, fmt)
      TRUE
      
    }, error = function(e) {
      failed_renders <- c(failed_renders, fmt)
      warning(sprintf("⚠️ Failed to render %s: %s", toupper(fmt), e$message))
      FALSE
    })
  }
  
} else {
  # Standard single-wavelength mode
  report_template <- file.path(script_dir, "reports", "unified_analysis_template.Rmd")
  
  if (!file.exists(report_template)) {
    stop("❌ Template file not found at: ", report_template)
  }
  
  message("📄 Using standard analysis template")
  
  for (fmt in selected_formats) {
    message("📄 Rendering: ", toupper(fmt))
    
    render_result <- tryCatch({
      rmarkdown::render(
        input = report_template,
        output_format = formats_map[[fmt]],
        output_file = "RBA-results-report",
        output_dir = output_dir,
        params = list(
          output_dir = normalizePath(output_dir, winslash = "/", mustWork = TRUE),
          lang = report_lang
        ),
        knit_root_dir = dirname(report_template),
        envir = new.env(parent = globalenv())
      )
      
      successful_renders <- c(successful_renders, fmt)
      TRUE
      
    }, error = function(e) {
      failed_renders <- c(failed_renders, fmt)
      warning(sprintf("⚠️ Failed to render %s: %s", toupper(fmt), e$message))
      FALSE
    })
  }
}

# Report final status
if (length(successful_renders) > 0) {
  message("✅ Successfully rendered: ", paste(toupper(successful_renders), collapse = ", "))
}

if (length(failed_renders) > 0) {
  message("❌ Failed to render: ", paste(toupper(failed_renders), collapse = ", "))
}

if (length(successful_renders) == length(selected_formats)) {
  message("✅ Report generation complete!")
} else if (length(successful_renders) > 0) {
  message("⚠️ Report generation partially complete (", length(successful_renders), "/", length(selected_formats), " formats)")
} else {
  message("❌ Report generation failed for all formats!")
}

message("📁 Results saved to: ", output_dir)

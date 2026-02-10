# ==============================================================================
# Global Configuration
# Purpose: Shared libraries, constants, and app-wide settings
# ==============================================================================

# --- Package Loading ----------------------------------------------------------
required_pkgs <- c(
  "shiny", "shinyjs", "shinyFeedback", "rintrojs", "rhandsontable",
  "dplyr", "tidyr", "tibble", "stringr", "purrr", "readr",
  "ggplot2", "ggrepel", "ggthemes", "ggtext", "plotly", "scales", "patchwork",
  "drc", "readxl", "jsonlite", "knitr", "rmarkdown", "digest"
)

# Suppress startup messages
suppressPackageStartupMessages({
  invisible(lapply(required_pkgs, library, character.only = TRUE, warn.conflicts = FALSE))
})

# --- App Metadata -------------------------------------------------------------
APP_VERSION <- tryCatch(
  utils::packageVersion("RBAElisaApp") |> as.character(),
  error = function(e) "2.0.0-dev"
)

APP_NAME <- "RBA & ELISA Analysis Suite"
APP_AUTHOR <- "Arnold Molina Porras (UCR) & Kristof Moeller (IAEA)"
APP_CONTACT <- "kr.moeller@iaea.org"

# --- Plate Constants ----------------------------------------------------------
PLATE_NROW <- 8
PLATE_NCOL <- 12
ROW_NAMES <- LETTERS[1:PLATE_NROW]
COL_NAMES <- as.character(1:PLATE_NCOL)

# --- Default Standard Concentrations (RBA - Saxitoxin) -----------------------
DEFAULT_STX_CONC <- c(
  1e-6,   # S1
  1e-7,   # S2
  3e-8,   # S3
  1e-8,   # S4
  3e-9,   # S5
  1e-9,   # S6
  1e-10,  # S7
  3e-11   # S8
)

# --- Default ELISA Standard Concentrations (Cortisol pg/mL) ------------------
DEFAULT_CORTISOL_CONC <- c(
  4000,   # S1
  1600,   # S2
  640,    # S3
  256,    # S4
  102.4,  # S5
  41.0,   # S6
  16.4,   # S7
  6.6     # S8
)

# --- Toxin Variants -----------------------------------------------------------
CTX_VARIANTS <- c(
  "CTX1B (Caribbean ciguatoxin-1)" = "CTX1B",
  "CTX3C (Pacific ciguatoxin)"     = "CTX3C",
  "P-CTX-1"                        = "P-CTX-1",
  "P-CTX-2"                        = "P-CTX-2",
  "P-CTX-3"                        = "P-CTX-3"
)

BTX_VARIANTS <- c(
  "PbTx-2" = "PbTx-2",
  "PbTx-3" = "PbTx-3"
)

# --- Molecular Weights (g/mol) ------------------------------------------------
MW_LOOKUP <- c(
  "Saxitoxin" = 299.29,
  "PbTx-2"    = 895.08,
  "PbTx-3"    = 897.10,
  "CTX1B"     = 1110.6,
  "CTX3C"     = 1023.2
)

# --- ELISA Analytes -----------------------------------------------------------
ELISA_ANALYTES <- c(
  "Cortisol"      = "cortisol",
  "Testosterone"  = "testosterone",
  "Estradiol"     = "estradiol",
  "Custom"        = "custom"
)

# --- QC Thresholds ------------------------------------------------------------
QC_THRESHOLDS <- list(
  hill_slope_tolerance = 0.20,    # ±20%
  ec50_cv_max = 0.30,             # 30% max CV
  qc_deviation_max = 0.30,        # ±30% for QC samples
  replicate_cv_max = 0.30,        # 30% max CV for replicates
  
  # ELISA-specific
  elisa_control_hierarchy = c("B0", "NSB", "Blank"),  # B0 > NSB > Blank
  min_control_wells = 2           # Minimum replicates per control
)

# --- File Upload Settings -----------------------------------------------------
MAX_UPLOAD_SIZE_MB <- 10
ACCEPTED_FILE_TYPES <- c(
  ".txt", ".csv", ".xlsx", ".xls"
)

# --- Helper Functions ---------------------------------------------------------

#' Safe null coalescing operator
`%||%` <- function(a, b) if (!is.null(a) && length(a) > 0) a else b

#' Get session info hash (for reproducibility tracking)
get_sessioninfo_hash <- function() {
  if (!requireNamespace("digest", quietly = TRUE)) {
    return(NA_character_)
  }
  info_text <- paste(capture.output(sessionInfo()), collapse = "\n")
  digest::digest(info_text, algo = "sha256")
}

#' Get script directory (works in RStudio and command line)
get_script_dir <- function() {
  tryCatch({
    ctx <- rstudioapi::getActiveDocumentContext()
    if (!is.null(ctx$path) && nzchar(ctx$path)) {
      return(normalizePath(dirname(ctx$path), winslash = "/"))
    }
  }, error = function(e) NULL)
  
  # Fallback to working directory
  normalizePath(getwd(), winslash = "/")
}

#' Safe JSON write with directory creation
write_json_safe <- function(x, file) {
  dir.create(dirname(file), recursive = TRUE, showWarnings = FALSE)
  jsonlite::write_json(x, path = file, pretty = TRUE, auto_unbox = TRUE, null = "null")
}

#' Validate scientific notation input
is_valid_scientific <- function(x) {
  if (is.null(x) || trimws(x) == "") return(FALSE)
  !is.na(suppressWarnings(as.numeric(x))) && grepl("^[0-9.]+[eE][-+]?[0-9]+$", trimws(x))
}

# --- Theming ------------------------------------------------------------------

# Custom ggplot2 theme for all plots
theme_rba <- function(base_size = 12) {
  theme_classic(base_size = base_size) +
    theme(
      plot.title = element_text(face = "bold", size = base_size * 1.2),
      axis.title = element_text(size = base_size * 1.1),
      axis.text = element_text(size = base_size),
      legend.title = element_blank(),
      legend.text = element_text(size = base_size * 0.9),
      legend.position = "top",
      panel.grid.major = element_line(color = "gray95"),
      panel.grid.minor = element_blank()
    )
}

# Set as default theme
theme_set(theme_rba())

# --- Color Palettes -----------------------------------------------------------

# Sample type colors (for plate visualization)
SAMPLE_TYPE_COLORS <- c(
  "Standard"      = "#90EE90",  # light green
  "QC"            = "#ADD8E6",  # light blue
  "Blank"         = "#FFE4E1",  # misty rose
  "NSB"           = "#FFDAB9",  # peach puff
  "B0"            = "#F0E68C",  # khaki
  "TotalActivity" = "#DDA0DD",  # plum
  "Sample"        = "#E6E6FA",  # lavender
  "Other"         = "#FFD700"   # gold
)

# QC status colors
QC_STATUS_COLORS <- c(
  "pass" = "#28a745",   # green
  "warn" = "#ffc107",   # amber
  "fail" = "#dc3545"    # red
)

# --- Startup Message ----------------------------------------------------------
message(sprintf(
  "\n%s (v%s)\nLoaded at %s\n",
  APP_NAME, APP_VERSION, Sys.time()
))

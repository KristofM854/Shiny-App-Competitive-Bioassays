# ==============================================================================
# Global Configuration
# Purpose: Shared libraries, constants, and app-wide settings.
#          Loaded once at app startup before app.R.
#
# Defines:
#   - Package loading (all required CRAN packages)
#   - Plate constants (PLATE_NROW, PLATE_NCOL, row/col names)
#   - Default standard concentrations (RBA saxitoxin, ELISA cortisol)
#   - Toxin variants, molecular weights, ELISA analyte list
#   - QC thresholds (CV limits, Hill slope tolerance)
#   - Helper functions (%||%, write_json_safe, theme_rba)
#   - Color palettes for sample types and QC status
#
# NOTE: The drc package loads MASS, which masks dplyr::select().
#       All select() calls throughout the codebase MUST use dplyr::select().
# ==============================================================================

# --- R Version Check ----------------------------------------------------------
if (getRversion() < "4.2.0") {
  warning("This application requires R >= 4.2.0 (for native pipe operator support). ",
          "You are running R ", as.character(getRversion()), ". ",
          "Some features may not work correctly.", call. = FALSE)
}

# --- Package Loading ----------------------------------------------------------
required_pkgs <- c(
  "car", "digest", "dplyr", "drc",
  "ggplot2", "ggrepel", "ggtext", "ggthemes", "glue",
  "htmltools", "htmlwidgets",
  "jsonlite", "kableExtra", "knitr",
  "patchwork", "plotly", "purrr",
  "readr", "readxl", "remotes", "rhandsontable", "rintrojs", "rmarkdown",
  "scales", "shiny", "shinyFeedback", "shinycssloaders", "shinyjs",
  "stringr", "tibble", "tidyr"
)

# Repositories: Posit Package Manager is listed first because it provides
# pre-built Windows/Linux binaries for new R versions (e.g. 4.6.x) faster
# than CRAN mirrors.  cloud.r-project.org is kept as a fallback.
.pkg_repos <- c(
  RSPM = "https://packagemanager.posit.co/cran/latest",
  CRAN = "https://cloud.r-project.org"
)

# Remove stale lock directories that block reinstallation after interrupted installs.
for (.lp in .libPaths()) {
  .locks <- list.dirs(.lp, recursive = FALSE, full.names = TRUE)
  .locks <- .locks[grepl("00LOCK-", basename(.locks))]
  if (length(.locks) > 0L) {
    message("Removing stale lock dirs: ", paste(basename(.locks), collapse = ", "))
    unlink(.locks, recursive = TRUE)
  }
}
rm(.lp, .locks)

# Install any packages that are missing from the local library.
# This covers shiny::runApp() and shiny::runGitHub() launches where no
# external installer (run_local.R, renv, DESCRIPTION) has run first.
missing_pkgs <- required_pkgs[
  !vapply(required_pkgs, requireNamespace, logical(1L), quietly = TRUE)
]
if (length(missing_pkgs) > 0L) {
  message("Installing missing packages: ", paste(missing_pkgs, collapse = ", "))

  # Use the first writable library path. In non-interactive sessions (Shiny
  # Server, Docker) the system library is often read-only; without an explicit
  # lib= R silently skips the install rather than creating a user library.
  writable_libs <- Filter(function(p) file.access(p, 2L) == 0L, .libPaths())
  install_call  <- list(
    pkgs         = missing_pkgs,
    dependencies = TRUE,
    repos        = .pkg_repos
  )
  if (length(writable_libs) > 0L) install_call$lib <- writable_libs[1L]
  do.call(install.packages, install_call)
  rm(writable_libs, install_call)

  # GitHub fallback: some packages have no CRAN/RSPM binary for a brand-new R
  # version immediately after release.  Try the GitHub source for known cases.
  .gh_sources <- c(kableExtra = "haozhu233/kableExtra")
  still_missing <- missing_pkgs[
    !vapply(missing_pkgs, requireNamespace, logical(1L), quietly = TRUE)
  ]
  .needs_gh <- still_missing[still_missing %in% names(.gh_sources)]
  if (length(.needs_gh) > 0L) {
    message("Binary install incomplete for: ", paste(.needs_gh, collapse = ", "),
            " — trying GitHub source...")
    if (!requireNamespace("remotes", quietly = TRUE))
      install.packages("remotes", repos = .pkg_repos)
    for (.p in .needs_gh) {
      tryCatch(
        remotes::install_github(.gh_sources[[.p]]),
        error = function(e) message("GitHub install failed for ", .p, ": ", e$message)
      )
    }
    rm(.p)
  }
  rm(.gh_sources, .needs_gh)

  # Final verification — abort loudly if still missing after all attempts so
  # the problem surfaces at startup rather than silently later during render.
  still_missing <- missing_pkgs[
    !vapply(missing_pkgs, requireNamespace, logical(1L), quietly = TRUE)
  ]
  if (length(still_missing) > 0L) {
    stop(
      "Required packages failed to install: ",
      paste(still_missing, collapse = ", "),
      "\nPlease install them manually:\n  install.packages(c(",
      paste0('"', still_missing, '"', collapse = ", "), "))",
      call. = FALSE
    )
  }
}
rm(.pkg_repos)

# Suppress startup messages
suppressPackageStartupMessages({
  invisible(lapply(required_pkgs, library, character.only = TRUE, warn.conflicts = FALSE))
})

# --- App Metadata -------------------------------------------------------------
APP_VERSION <- tryCatch(
  utils::packageVersion("RBAElisaApp") |> as.character(),
  error = function(e) "1.0.0-dev"
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

# --- Default ELISA Standard Concentrations (Testosterone pg/mL) ---------------
DEFAULT_TESTOSTERONE_CONC <- c(
  10000,  # S1
  4000,   # S2
  1600,   # S3
  640,    # S4
  256,    # S5
  102.4,  # S6
  41.0,   # S7
  16.4    # S8
)

# --- Default ELISA Standard Concentrations (Estradiol pg/mL) -----------------
DEFAULT_ESTRADIOL_CONC <- c(
  2000,   # S1
  800,    # S2
  320,    # S3
  128,    # S4
  51.2,   # S5
  20.5,   # S6
  8.2,    # S7
  3.3     # S8
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
options(shiny.maxRequestSize = MAX_UPLOAD_SIZE_MB * 1024^2)
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

#' Open a folder in the OS file manager (local installs only)
open_folder_in_os <- function(path) {
  if (is.null(path) || !nzchar(path)) return(FALSE)
  norm <- tryCatch(normalizePath(path, mustWork = TRUE), error = function(e) NULL)
  if (is.null(norm)) return(FALSE)
  tryCatch({
    switch(Sys.info()[["sysname"]],
      Windows = shell.exec(norm),
      Darwin  = system2("open",     shQuote(norm), wait = FALSE),
              system2("xdg-open", shQuote(norm), wait = FALSE)
    )
    TRUE
  }, error = function(e) FALSE)
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

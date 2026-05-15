# ==============================================================================
# scripts/_bootstrap_analysis.R
#
# Minimal bootstrap for invoking the analysis pipeline outside a Shiny session.
# Sourced by scripts/capture_baseline.R and scripts/replay_report.R.
# Does NOT load Shiny UI packages (rhandsontable, rintrojs, shinyFeedback,
# shinycssloaders) since these are not used by the analysis pipeline.
#
# Replaces global.R for non-Shiny analysis scripts. Sourcing global.R dragged
# in the full Shiny UI dependency tree, causing install failures in environments
# that do not have those packages available.
#
# Package list determined empirically: grep for ::, library(), require() across
# reports/analysis_pipeline.R, reports/report_functions.R,
# reports/report_sections.R, reports/plot_functions.R, reports/report_constants.R,
# utils/utils_plate.R, utils/utils_import_v3.R, utils/utils_import_multiwavelength.R,
# utils/utils_normalization.R, and the three Rmd templates.
# ==============================================================================

# --- Analysis packages --------------------------------------------------------
suppressPackageStartupMessages({
  library(drc)
  library(car)
  library(dplyr)
  library(tidyr)
  library(tibble)
  library(purrr)
  library(stringr)
  library(glue)
  library(readxl)
  library(ggplot2)
  library(ggtext)
  library(scales)
  library(patchwork)
  library(plotly)
  library(htmltools)
  library(jsonlite)
  library(knitr)
  library(kableExtra)
  library(rmarkdown)
  library(withr)
})

# --- Helper functions from global.R -------------------------------------------
# Copied from global.R; keep in sync.
`%||%` <- function(a, b) if (!is.null(a) && length(a) > 0) a else b

write_json_safe <- function(x, file) {
  dir.create(dirname(file), recursive = TRUE, showWarnings = FALSE)
  jsonlite::write_json(x, path = file, pretty = TRUE, auto_unbox = TRUE, null = "null")
}

# --- Plate constants ----------------------------------------------------------
# Copied from global.R; keep in sync.
PLATE_NROW <- 8L
PLATE_NCOL <- 12L
ROW_NAMES  <- LETTERS[1:PLATE_NROW]
COL_NAMES  <- as.character(1:PLATE_NCOL)

# --- Default standard concentrations -----------------------------------------
# Copied from global.R; keep in sync.
DEFAULT_STX_CONC <- c(
  1e-6, 1e-7, 3e-8, 1e-8, 3e-9, 1e-9, 1e-10, 3e-11
)

DEFAULT_CORTISOL_CONC <- c(
  4000, 1600, 640, 256, 102.4, 41.0, 16.4, 6.6
)

# --- Molecular weight lookup --------------------------------------------------
# Copied from global.R; keep in sync.
MW_LOOKUP <- c(
  "Saxitoxin" = 299.29,
  "PbTx-2"    = 895.08,
  "PbTx-3"    = 897.10,
  "CTX1B"     = 1110.6,
  "CTX3C"     = 1023.2
)

# --- Analysis-side source files -----------------------------------------------
source("reports/report_constants.R")
source("server/i18n.R")
source("utils/utils_plate.R")
source("utils/utils_import_v3.R")
source("utils/utils_import_multiwavelength.R")
source("utils/utils_normalization.R")
source("reports/report_functions.R")
source("reports/plot_functions.R")
source("reports/analysis_pipeline.R")
source("reports/report_sections.R")

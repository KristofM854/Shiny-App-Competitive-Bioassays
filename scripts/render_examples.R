#!/usr/bin/env Rscript
# render_examples.R — Render both example datasets (RBA + ELISA) in compact
# and full modes for CI verification.
#
# Usage:
#   Rscript scripts/render_examples.R
#   Rscript scripts/render_examples.R --out /tmp/renders
#
# Outputs (written to out_dir):
#   rba_full.html      rba_compact.html
#   elisa_full.html    elisa_compact.html
#
# Requires: rmarkdown, drc, dplyr, jsonlite, kableExtra, plotly, ...
# All packages are listed in global.R / the CI R-CMD-check.yml extra-packages.

args     <- commandArgs(trailingOnly = TRUE)
out_flag <- which(args == "--out")
out_dir  <- if (length(out_flag) > 0L && length(args) >= out_flag + 1L) {
  args[[out_flag + 1L]]
} else {
  file.path(tempdir(), "render_examples", format(Sys.time(), "%Y%m%d-%H%M%S"))
}

dir.create(out_dir, recursive = TRUE, showWarnings = FALSE)
cat("Output directory:", out_dir, "\n")

# Derive repo root from this script's path
all_args    <- commandArgs(trailingOnly = FALSE)
script_flag <- grep("--file=", all_args, value = TRUE)
if (length(script_flag) > 0L) {
  repo_root <- normalizePath(
    file.path(dirname(sub("^--file=", "", script_flag[[1L]])), ".."),
    mustWork = FALSE)
} else {
  repo_root <- normalizePath(".", mustWork = FALSE)
}
cat("Repo root:", repo_root, "\n")

# Source global.R to get helpers (create_type_matrix, DEFAULT_STX_CONC, etc.)
source(file.path(repo_root, "global.R"))
source(file.path(repo_root, "utils", "utils_plate.R"))
source(file.path(repo_root, "server", "i18n.R"))

template <- file.path(repo_root, "reports", "unified_analysis_template.Rmd")
compact_template <- file.path(repo_root, "reports", "unified_analysis_template_compact.Rmd")

# ---------------------------------------------------------------------------
# Helper: build and render one fixture
# ---------------------------------------------------------------------------
render_fixture <- function(fixture_dir, out_file, compact) {
  stopifnot(dir.exists(fixture_dir))
  # Clear model-fit and sample-quantify caches so the full pipeline runs
  unlink(file.path(fixture_dir, ".fit_all_models_cache.rds"))
  unlink(file.path(fixture_dir, ".quantify_samples_cache.rds"))
  # Clear knitr cache
  for (pat in c("_cache", "_files")) {
    dirs <- list.dirs(fixture_dir, recursive = FALSE, full.names = TRUE)
    dirs <- dirs[grepl(pat, basename(dirs))]
    if (length(dirs) > 0L) unlink(dirs, recursive = TRUE)
  }

  tpl <- if (isTRUE(compact)) compact_template else template
  if (!file.exists(tpl)) tpl <- template   # fallback: pass compact param

  cat(sprintf("  Rendering %s (compact=%s) ...\n", basename(out_file), compact))
  out <- tryCatch(
    rmarkdown::render(
      input      = tpl,
      output_file = out_file,
      output_dir  = dirname(out_file),
      params      = list(output_dir = fixture_dir, compact = compact, lang = "en"),
      quiet       = TRUE,
      envir       = new.env(parent = globalenv()),
      knit_root_dir = file.path(repo_root, "reports")
    ),
    error = function(e) {
      message("  ERROR: ", e$message)
      NULL
    }
  )
  if (!is.null(out)) {
    sz <- file.info(out_file)$size
    cat(sprintf("  OK  %s  %d bytes\n", basename(out_file), sz))
  }
  invisible(out)
}

# ---------------------------------------------------------------------------
# Setup RBA fixture
# ---------------------------------------------------------------------------
setup_rba <- function(dir) {
  dir.create(dir, recursive = TRUE, showWarnings = FALSE)
  raw <- read.table(file.path(repo_root, "examples", "rba_stx_example.csv"),
                    header = FALSE, sep = "\t", stringsAsFactors = FALSE)
  meas_mat <- as.data.frame(apply(raw[, 2:13], 2, as.numeric))
  n_std <- 8L
  df_long <- matrix_to_long(
    create_type_matrix("rba", n_std),
    create_id_matrix("rba", n_std),
    create_dilution_matrix(),
    create_replicate_matrix("rba"),
    meas_mat,
    std_conc = DEFAULT_STX_CONC[1:n_std]
  )
  write.csv(df_long, file.path(dir, "long_data_output.csv"), row.names = FALSE)
  jsonlite::write_json(list("html"),
                       file.path(dir, "selected_formats.json"), auto_unbox = TRUE)
  jsonlite::write_json(list(notes = ""),
                       file.path(dir, "notes.json"), auto_unbox = TRUE)
  jsonlite::write_json(list(
    qc_concentration  = "3e-9",
    expected_hill     = "1",
    assay_type        = "rba",
    detection_method  = "radioligand",
    analyte           = "Saxitoxin"
  ), file.path(dir, "qc_params.json"), auto_unbox = TRUE)
  jsonlite::write_json(list(
    assay_type             = "rba",
    toxin_class            = "Saxitoxin",
    toxin_standard_label   = "Saxitoxin",
    molecular_weight_g_mol = MW_LOOKUP[["Saxitoxin"]],
    detection_method       = "radioligand",
    units                  = "mol/L"
  ), file.path(dir, "assay_config.json"), auto_unbox = TRUE)
  jsonlite::write_json(list(
    regression_weight        = "none",
    quant_range_min          = 20,
    quant_range_max          = 80,
    ci_method                = "t_dist",
    enable_outlier_detection = FALSE,
    outlier_min_n            = 3,
    normality_assumption     = "assume",
    cv_limit                 = 30
  ), file.path(dir, "analysis_config.json"), auto_unbox = TRUE)
  invisible(dir)
}

# ---------------------------------------------------------------------------
# Setup ELISA fixture
# ---------------------------------------------------------------------------
setup_elisa <- function(dir) {
  dir.create(dir, recursive = TRUE, showWarnings = FALSE)
  # Read ELISA example plate (8 rows x 12 cols, tab-separated, row letter in col 1)
  raw <- read.table(file.path(repo_root, "examples", "elisa_cortisol_example.csv"),
                    header = FALSE, sep = "\t", stringsAsFactors = FALSE)
  meas_mat <- as.data.frame(apply(raw[, 2:13], 2, as.numeric))
  n_std <- 8L
  df_long <- matrix_to_long(
    create_type_matrix("elisa", n_std),
    create_id_matrix("elisa", n_std),
    create_dilution_matrix(),
    create_replicate_matrix("elisa"),
    meas_mat,
    std_conc = DEFAULT_CORTISOL_CONC[1:n_std]
  )
  # Add NormalizedValue column so the template detects ELISA mode
  df_long$NormalizedValue <- df_long$MeasurementValue

  write.csv(df_long, file.path(dir, "long_data_output.csv"), row.names = FALSE)
  jsonlite::write_json(list("html"),
                       file.path(dir, "selected_formats.json"), auto_unbox = TRUE)
  jsonlite::write_json(list(notes = ""),
                       file.path(dir, "notes.json"), auto_unbox = TRUE)
  jsonlite::write_json(list(
    qc_concentration = "200",
    expected_hill    = "1",
    assay_type       = "elisa",
    detection_method = "absorbance",
    analyte          = "Cortisol"
  ), file.path(dir, "qc_params.json"), auto_unbox = TRUE)
  jsonlite::write_json(list(
    assay_type       = "elisa",
    analyte          = "Cortisol",
    detection_method = "absorbance",
    units            = "pg/mL"
  ), file.path(dir, "assay_config.json"), auto_unbox = TRUE)
  jsonlite::write_json(list(
    regression_weight        = "none",
    quant_range_min          = 20,
    quant_range_max          = 80,
    ci_method                = "t_dist",
    enable_outlier_detection = FALSE,
    outlier_min_n            = 3,
    normality_assumption     = "assume",
    cv_limit                 = 30
  ), file.path(dir, "analysis_config.json"), auto_unbox = TRUE)
  invisible(dir)
}

# ---------------------------------------------------------------------------
# Build fixtures and render all four HTMLs
# ---------------------------------------------------------------------------
cat("\n=== Setting up fixtures ===\n")
rba_dir   <- setup_rba(file.path(out_dir, "rba_fixture"))
elisa_dir <- setup_elisa(file.path(out_dir, "elisa_fixture"))

cat("\n=== Rendering ===\n")
render_fixture(rba_dir,   file.path(out_dir, "rba_full.html"),     compact = FALSE)
render_fixture(rba_dir,   file.path(out_dir, "rba_compact.html"),  compact = TRUE)
render_fixture(elisa_dir, file.path(out_dir, "elisa_full.html"),   compact = FALSE)
render_fixture(elisa_dir, file.path(out_dir, "elisa_compact.html"),compact = TRUE)

cat("\n=== File sizes ===\n")
for (f in c("rba_full.html", "rba_compact.html", "elisa_full.html", "elisa_compact.html")) {
  p <- file.path(out_dir, f)
  if (file.exists(p)) {
    cat(sprintf("  wc -c %-28s = %d\n", f, file.info(p)$size))
  } else {
    cat(sprintf("  MISSING: %s\n", f))
  }
}

cat("\nDone. HTMLs are in:", out_dir, "\n")

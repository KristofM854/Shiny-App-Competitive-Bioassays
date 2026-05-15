# ==============================================================================
# scripts/capture_baseline.R
#
# Captures the pre-fix numerical baseline for the three shipped example
# datasets. This is the freezing artefact for branch
# `claude/audit-baseline-and-decisions` (Phase 0 of the v1.0.0 audit) and
# the diff reference against which every subsequent statistical-fix branch
# is measured.
#
# Usage:
#   From the repository root, in a fresh R session:
#       Rscript scripts/capture_baseline.R
#   or interactively:
#       source("scripts/capture_baseline.R")
#
# Output:
#   audit/pre-fix-snapshot/
#     rba_stx/
#       long_data_output.csv
#       unknown_results.csv
#       unknown_results_summary.csv
#       model_stats.json
#       analysis_report.html
#       baseline_meta.json
#     elisa_cortisol/
#       (same set)
#     multiwavelength/
#       long_data_output.csv
#       long_data_output_<wl>.csv (one per wavelength)
#       wavelength_manifest.json
#       analysis_report.html
#       baseline_meta.json
#
# Design notes:
#   - Modelled on tests/testthat/test-report-numbers.R: build the four plate
#     matrices via create_*_matrix(), read the example file via
#     read_file_raw() or parse_plate_file(), build long-format data via
#     matrix_to_long(), write the sidecar JSONs that report_pipeline.R
#     normally writes, then call rmarkdown::render() on the appropriate
#     template. No Shiny session, no shinytest2, no AppDriver.
#   - Multi-wavelength uses tests/testthat/fixtures/multiwave_synthetic.csv
#     as input because the repository does not ship a primary
#     multi-wavelength example dataset. The synthetic fixture is the same
#     one exercised by tests/testthat/test-shinytest-multiwavelength.R.
#   - All output goes under audit/pre-fix-snapshot/<example>/ so a single
#     `git add audit/pre-fix-snapshot` captures the baseline.
#   - This script is the minimal precursor to the full
#     scripts/replay_report.R (AUDIT-021); when that lands it will
#     subsume this one.
# ==============================================================================

# --- Paths --------------------------------------------------------------------

repo_root <- tryCatch({
  # Try git-based root if rprojroot is available
  if (requireNamespace("rprojroot", quietly = TRUE)) {
    rprojroot::find_root(rprojroot::is_git_root)
  } else {
    # Fallback: assume current working directory is repo root
    normalizePath(getwd(), winslash = "/")
  }
}, error = function(e) normalizePath(getwd(), winslash = "/"))

setwd(repo_root)

# Source the minimal analysis bootstrap (replaces global.R + individual sources).
# See scripts/_bootstrap_analysis.R for the full package and constant inventory.
source(file.path(repo_root, "scripts", "_bootstrap_analysis.R"))

snapshot_root <- file.path(repo_root, "audit", "pre-fix-snapshot")
dir.create(snapshot_root, showWarnings = FALSE, recursive = TRUE)

git_sha <- tryCatch(
  trimws(system("git rev-parse HEAD", intern = TRUE)),
  error = function(e) "unknown"
)
capture_time <- Sys.time()
session_info_str <- paste(capture.output(sessionInfo()), collapse = "\n")

# --- Helper: build the standard sidecar JSON set ------------------------------
write_sidecars <- function(out_dir, assay_type, std_conc_vec,
                           toxin_label = NA_character_,
                           elisa_analyte = NA_character_,
                           elisa_units = "pg/mL") {
  jsonlite::write_json(list("html"),
                       file.path(out_dir, "selected_formats.json"),
                       auto_unbox = TRUE)
  jsonlite::write_json(list(notes = ""),
                       file.path(out_dir, "notes.json"),
                       auto_unbox = TRUE)
  if (assay_type == "rba") {
    jsonlite::write_json(list(
      qc_concentration = "3e-9",
      expected_hill    = "1",
      assay_type       = "rba",
      detection_method = "radioligand",
      analyte          = toxin_label %||% "Saxitoxin"
    ), file.path(out_dir, "qc_params.json"), auto_unbox = TRUE)
    jsonlite::write_json(list(
      assay_type             = "rba",
      toxin_class            = toxin_label %||% "Saxitoxin",
      toxin_standard_label   = toxin_label %||% "Saxitoxin",
      molecular_weight_g_mol = unname(MW_LOOKUP[[toxin_label %||% "Saxitoxin"]]),
      detection_method       = "radioligand",
      units                  = "mol/L"
    ), file.path(out_dir, "assay_config.json"), auto_unbox = TRUE)
  } else {
    jsonlite::write_json(list(
      assay_type       = "elisa",
      detection_method = "absorbance",
      analyte          = elisa_analyte %||% "cortisol",
      units            = elisa_units,
      normalization    = "percent_b_b0"
    ), file.path(out_dir, "qc_params.json"), auto_unbox = TRUE)
    jsonlite::write_json(list(
      assay_type       = "elisa",
      analyte          = elisa_analyte %||% "cortisol",
      units            = elisa_units,
      detection_method = "absorbance"
    ), file.path(out_dir, "assay_config.json"), auto_unbox = TRUE)
  }
  jsonlite::write_json(list(
    regression_weight        = "none",
    quant_range_min          = 20,
    quant_range_max          = 80,
    ci_method                = "t_dist",
    enable_outlier_detection = FALSE,
    outlier_min_n            = 3,
    normality_assumption     = "assume",
    cv_limit                 = 30
  ), file.path(out_dir, "analysis_config.json"), auto_unbox = TRUE)
  jsonlite::write_json(list(lang = "en", langs = list("en")),
                       file.path(out_dir, "report_language.json"),
                       auto_unbox = TRUE)
}

# --- Helper: write baseline_meta.json -----------------------------------------
write_baseline_meta <- function(out_dir, example_name, notes = "") {
  meta <- list(
    example         = example_name,
    git_sha         = git_sha,
    capture_time    = format(capture_time, "%Y-%m-%d %H:%M:%S %z"),
    r_version       = R.version.string,
    session_info    = session_info_str,
    capture_script  = "scripts/capture_baseline.R",
    notes           = notes
  )
  jsonlite::write_json(meta, file.path(out_dir, "baseline_meta.json"),
                       pretty = TRUE, auto_unbox = TRUE)
}

# --- Helper: render single-plate Rmd in an isolated env -----------------------
render_single_plate <- function(out_dir) {
  template <- file.path(repo_root, "reports", "unified_analysis_template.Rmd")
  withr::with_envvar(
    list(
      RBA_OUTPUT_DIR = out_dir,
      RBA_CSV_PATH   = file.path(out_dir, "long_data_output.csv"),
      RBA_FMT_JSON   = file.path(out_dir, "selected_formats.json"),
      RBA_NOTES_FILE = file.path(out_dir, "notes.json")
    ),
    rmarkdown::render(
      input        = template,
      output_file  = "analysis_report.html",
      output_dir   = out_dir,
      params       = list(output_dir = out_dir, lang = "en"),
      knit_root_dir = dirname(template),
      envir        = new.env(parent = globalenv()),
      quiet        = TRUE
    )
  )
}

# =============================================================================
# RBA Saxitoxin baseline
# =============================================================================
message("\n[1/3] Capturing RBA STX baseline...")
rba_out <- file.path(snapshot_root, "rba_stx")
dir.create(rba_out, showWarnings = FALSE, recursive = TRUE)

n_std <- 8L
type_mat      <- create_type_matrix("rba", n_std)
id_mat        <- create_id_matrix("rba", n_std)
dilution_mat  <- create_dilution_matrix()
replicate_mat <- create_replicate_matrix("rba")

# The RBA example is a tab-separated file with row labels A-H in column 1
# and 12 numeric data columns. read_file_raw handles .csv with sep="," but
# this file is actually tab-separated; use read.table directly per
# test-report-numbers.R:59-62.
rba_raw <- read.table(
  file.path(repo_root, "examples", "rba_stx_example.csv"),
  header = FALSE, sep = "\t", stringsAsFactors = FALSE
)
rba_meas_mat <- as.data.frame(apply(rba_raw[, 2:13], 2, as.numeric))

rba_long <- matrix_to_long(
  type_mat, id_mat, dilution_mat, replicate_mat, rba_meas_mat,
  std_conc = DEFAULT_STX_CONC[1:n_std]
)
# Apply RBA normalization (raw pass-through with CPM unit)
rba_long_norm <- normalize_data(rba_long, "rba", "radioligand")
write.csv(rba_long_norm,
          file.path(rba_out, "long_data_output.csv"),
          row.names = FALSE)
write_sidecars(rba_out, "rba", DEFAULT_STX_CONC[1:n_std],
               toxin_label = "Saxitoxin")

render_single_plate(rba_out)
write_baseline_meta(rba_out, "rba_stx_example.csv",
                    notes = "RBA Saxitoxin, 8 standards triplicate, default settings.")
message("    Done: ", rba_out)

# =============================================================================
# ELISA Cortisol baseline
# =============================================================================
message("\n[2/3] Capturing ELISA Cortisol baseline...")
elisa_out <- file.path(snapshot_root, "elisa_cortisol")
dir.create(elisa_out, showWarnings = FALSE, recursive = TRUE)

# The ELISA example has a different layout:
#   Col 1: Blank/NSB/B0/TotalActivity (rows A-H)
#   Cols 2-3: 8 standards S1-S8 duplicated
#   Cols 4-12: samples
type_mat_e      <- create_type_matrix("elisa", n_std)
id_mat_e        <- create_id_matrix("elisa", n_std)
dilution_mat_e  <- create_dilution_matrix()
replicate_mat_e <- create_replicate_matrix("elisa")

elisa_raw <- read.table(
  file.path(repo_root, "examples", "elisa_cortisol_example.csv"),
  header = FALSE, sep = "\t", stringsAsFactors = FALSE
)
elisa_meas_mat <- as.data.frame(apply(elisa_raw[, 2:13], 2, as.numeric))

elisa_long <- matrix_to_long(
  type_mat_e, id_mat_e, dilution_mat_e, replicate_mat_e, elisa_meas_mat,
  std_conc = DEFAULT_CORTISOL_CONC[1:n_std]
)
# ELISA pipeline pass-through (Rmd template calls calculate_elisa_bb0
# internally; see server/report_pipeline.R:104–117 for parity with the
# live pipeline).
elisa_long_norm <- elisa_long
elisa_long_norm$NormalizedValue <- elisa_long_norm$MeasurementValue
elisa_long_norm$ResponseUnit    <- "Raw Absorbance"
write.csv(elisa_long_norm,
          file.path(elisa_out, "long_data_output.csv"),
          row.names = FALSE)
write_sidecars(elisa_out, "elisa", DEFAULT_CORTISOL_CONC[1:n_std],
               elisa_analyte = "cortisol", elisa_units = "pg/mL")

render_single_plate(elisa_out)
write_baseline_meta(elisa_out, "elisa_cortisol_example.csv",
                    notes = "ELISA Cortisol, 8 standards duplicate, default settings.")
message("    Done: ", elisa_out)

# =============================================================================
# Multi-wavelength baseline
# =============================================================================
message("\n[3/3] Capturing multi-wavelength baseline...")
mw_out <- file.path(snapshot_root, "multiwavelength")
dir.create(mw_out, showWarnings = FALSE, recursive = TRUE)

# The multi-wavelength shinytest fixture is the only multi-wave input
# shipped in the repo. Located at tests/testthat/fixtures/multiwave_synthetic.csv.
mw_fixture <- file.path(repo_root, "tests", "testthat", "fixtures",
                         "multiwave_synthetic.csv")
if (!file.exists(mw_fixture)) {
  message("    SKIP: multi-wavelength fixture not found at ", mw_fixture)
  message("    Regenerate via: source('tests/testthat/fixtures/generate_multiwave.R')")
  writeLines(
    paste0("# multi-wavelength baseline NOT captured\n",
           "# Reason: fixture file missing at ", mw_fixture, "\n",
           "# Run `Rscript tests/testthat/fixtures/generate_multiwave.R` to regenerate."),
    file.path(mw_out, "SKIPPED.md")
  )
} else {
  # Use the same ELISA layout as the multi-wavelength shinytest expects.
  type_mat_mw      <- create_type_matrix("elisa", n_std)
  id_mat_mw        <- create_id_matrix("elisa", n_std)
  dilution_mat_mw  <- create_dilution_matrix()
  replicate_mat_mw <- create_replicate_matrix("elisa")

  mw_parsed <- tryCatch(
    detect_and_import_multiwavelength(mw_fixture, sheet = 1),
    error = function(e) {
      message("    detect_and_import_multiwavelength errored: ", e$message)
      list(wavelengths = NULL, plates = NULL)
    }
  )

  # parse_plate_file() in utils_import_v3.R gates multi-wave detection behind
  # ext %in% c("xlsx", "xls"), so a CSV fixture cannot reach the multi-wave
  # path through that wrapper. We call the lower-level detector directly
  # because .read_raw_matrix() in utils_import_multiwavelength.R does support
  # CSV inputs. If the detector still returns no wavelengths (file lacks
  # "Raw Data (XXX)" markers, or markers fail the LETTERS[1:8] row-label
  # check), we abort capture for this example with a clear SKIPPED.md
  # rather than dereferencing a NULL list.
  if (is.null(mw_parsed$wavelengths) || length(mw_parsed$wavelengths) < 2 ||
      is.null(mw_parsed$plates) || length(mw_parsed$plates) < 2) {
    message("    SKIP: multi-wavelength detection found ",
            length(mw_parsed$wavelengths %||% list()),
            " wavelength(s); need >= 2.")
    writeLines(
      c("# multi-wavelength baseline NOT captured",
        "#",
        sprintf("# Fixture: %s", mw_fixture),
        sprintf("# Detected wavelengths: %d (need >= 2)",
                length(mw_parsed$wavelengths %||% list())),
        "#",
        "# Possible causes:",
        "#   - Fixture lacks 'Raw Data (XXX)' marker rows.",
        "#   - Marker rows are present but the 8 rows below the column-header",
        "#     row do not start with row labels A-H (the detection requirement",
        "#     in .scan_wavelength_locations() at utils_import_multiwavelength.R).",
        "#   - The fixture file is corrupt or has been regenerated against a",
        "#     different format.",
        "#",
        "# Regenerate the fixture via:",
        "#   Rscript tests/testthat/fixtures/generate_multiwave.R"),
      file.path(mw_out, "SKIPPED.md")
    )
    write_baseline_meta(mw_out, "multiwave_synthetic.csv",
                        notes = sprintf("SKIPPED: multi-wave detection found %d wavelengths (need >= 2). See SKIPPED.md.",
                                        length(mw_parsed$wavelengths %||% list())))
  } else {

    # Per-wavelength CSVs (matches the file naming convention in
    # server/report_pipeline.R:189–192).
    for (wl in mw_parsed$wavelengths) {
      plate_wl <- mw_parsed$plates[[wl]]
      df_wl <- matrix_to_long(
        type_mat_mw, id_mat_mw, dilution_mat_mw, replicate_mat_mw, plate_wl,
        std_conc = DEFAULT_CORTISOL_CONC[1:n_std]
      )
      df_wl$NormalizedValue <- df_wl$MeasurementValue
      df_wl$ResponseUnit    <- "Raw Absorbance"
      wl_safe <- gsub("[/\\\\:*?\"<>|]", "_", wl)
      write.csv(df_wl,
                file.path(mw_out, paste0("long_data_output_", wl_safe, ".csv")),
                row.names = FALSE)
    }
    # Primary CSV: the first wavelength's plate, named long_data_output.csv
    primary_wl <- mw_parsed$wavelengths[[1]]
    primary_plate <- mw_parsed$plates[[primary_wl]]
    df_primary <- matrix_to_long(
      type_mat_mw, id_mat_mw, dilution_mat_mw, replicate_mat_mw, primary_plate,
      std_conc = DEFAULT_CORTISOL_CONC[1:n_std]
    )
    df_primary$NormalizedValue <- df_primary$MeasurementValue
    df_primary$ResponseUnit    <- "Raw Absorbance"
    write.csv(df_primary,
              file.path(mw_out, "long_data_output.csv"),
              row.names = FALSE)

    # Wavelength manifest
    jsonlite::write_json(
      list(wavelengths = as.list(mw_parsed$wavelengths)),
      file.path(mw_out, "wavelength_manifest.json"),
      auto_unbox = TRUE
    )

    write_sidecars(mw_out, "elisa", DEFAULT_CORTISOL_CONC[1:n_std],
                   elisa_analyte = "cortisol", elisa_units = "pg/mL")

    # Render the multi-wavelength template
    mw_template <- file.path(repo_root, "reports",
                              "multiwavelength_analysis_template.Rmd")
    withr::with_envvar(
      list(
        RBA_OUTPUT_DIR = mw_out,
        RBA_CSV_PATH   = file.path(mw_out, "long_data_output.csv"),
        RBA_FMT_JSON   = file.path(mw_out, "selected_formats.json"),
        RBA_NOTES_FILE = file.path(mw_out, "notes.json")
      ),
      rmarkdown::render(
        input        = mw_template,
        output_file  = "analysis_report.html",
        output_dir   = mw_out,
        params       = list(
          output_dir  = mw_out,
          wavelengths = mw_parsed$wavelengths,
          lang        = "en"
        ),
        knit_root_dir = dirname(mw_template),
        envir        = new.env(parent = globalenv()),
        quiet        = TRUE
      )
    )

    write_baseline_meta(mw_out, "multiwave_synthetic.csv",
                        notes = sprintf("Multi-wavelength ELISA, %d wavelengths: %s.",
                                        length(mw_parsed$wavelengths),
                                        paste(mw_parsed$wavelengths, collapse = ", ")))
    message("    Done: ", mw_out)
  }
}

# --- Final summary ------------------------------------------------------------
message("\n==============================================================")
message("Baseline capture complete.")
message("Git SHA:       ", git_sha)
message("Captured at:   ", format(capture_time, "%Y-%m-%d %H:%M:%S %z"))
message("Output root:   ", snapshot_root)
message("==============================================================")
message("Next step: review captured model_stats.json in each subdir,")
message("then commit with `git add audit/pre-fix-snapshot/`.")

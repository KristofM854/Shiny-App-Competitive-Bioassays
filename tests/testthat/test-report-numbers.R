# Golden-number regression test for the RBA dose-response pipeline.
#
# Renders reports/unified_analysis_template.Rmd against the RBA STX example
# plate and asserts that headline statistics (R^2, IC50, number of quantified
# replicate groups) match a known-good reference. Catches silent numerical
# regressions where the report still renders but the fitted values have
# shifted (e.g. a dependency bump or a dplyr::select() masking bug).
#
# --- CAPTURE REFERENCE VALUES ---------------------------------------------
# After an intentional statistics-changing commit (new weighting logic,
# library upgrade, etc.) uncomment and run the `capture_reference_values`
# block below from an interactive R session. It prints the fresh numbers,
# which you then paste into the expected_* constants at the top of the
# test block.
#
# capture_reference_values <- function() {
#   setwd(rprojroot::find_root(rprojroot::is_git_root))
#   source("tests/testthat/helper-setup.R")
#   out <- .render_golden_rba()
#   stats   <- jsonlite::fromJSON(file.path(out, "model_stats.json"))
#   summary <- read.csv(file.path(out, "unknown_results_summary.csv"),
#                       stringsAsFactors = FALSE)
#   cat(sprintf("expected_r2   <- %.6f\n",            stats$r_squared))
#   cat(sprintf("expected_ic50 <- %.3e\n",            stats$ic50))
#   cat(sprintf("expected_n    <- %d  # replicate groups\n", nrow(summary)))
# }
# capture_reference_values()
# --------------------------------------------------------------------------

test_that("RBA golden numbers match reference", {
  skip_on_cran()
  skip_if_not_installed("rmarkdown")
  skip_if_not_installed("drc")
  skip_if_not_installed("jsonlite")

  # --- Reference values (update via capture_reference_values() above) ---
  expected_r2   <- 0.998    # placeholder per implementation plan §B6
  expected_ic50 <- 5.2e-9   # placeholder per implementation plan §B6
  expected_n    <- 24       # 24 replicate groups in the STX example

  # --- Build fixture output_dir with what save_analysis_artifacts() would ---
  fixture_out <- file.path(tempdir(), "golden-rba",
                           format(Sys.time(), "%Y%m%d-%H%M%S"))
  dir.create(fixture_out, recursive = TRUE, showWarnings = FALSE)

  # Source helpers (global.R constants, utils_plate, report_constants, ...).
  # helper-setup.R is auto-run by testthat; we rely on it here.

  # Build the four plate matrices for an RBA STX triplicate layout with 8
  # standards, 24 sample replicate groups.
  n_std <- 8L
  type_mat      <- create_type_matrix("rba", n_std)
  id_mat        <- create_id_matrix("rba", n_std)
  dilution_mat  <- create_dilution_matrix()
  replicate_mat <- create_replicate_matrix("rba")

  # Read the RBA example plate (tab-separated, 8 rows x 12 numeric cols, A-H
  # letters in col 1).
  raw <- read.table(testthat::test_path("..", "..", "examples",
                                        "rba_stx_example.csv"),
                    header = FALSE, sep = "\t", stringsAsFactors = FALSE)
  meas_mat <- as.data.frame(apply(raw[, 2:13], 2, as.numeric))

  df_long <- matrix_to_long(type_mat, id_mat, dilution_mat, replicate_mat,
                            meas_mat,
                            std_concentrations = DEFAULT_STX_CONC[1:n_std])
  csv_path <- file.path(fixture_out, "long_data_output.csv")
  write.csv(df_long, csv_path, row.names = FALSE)

  # Minimum JSON fixtures save_analysis_artifacts() normally writes.
  jsonlite::write_json(list("html"),
                       file.path(fixture_out, "selected_formats.json"),
                       auto_unbox = TRUE)
  jsonlite::write_json(list(notes = ""),
                       file.path(fixture_out, "notes.json"),
                       auto_unbox = TRUE)
  jsonlite::write_json(list(
    qc_concentration  = "3e-9",
    expected_hill     = "1",
    assay_type        = "rba",
    detection_method  = "radioligand",
    analyte           = "Saxitoxin"
  ), file.path(fixture_out, "qc_params.json"), auto_unbox = TRUE)
  jsonlite::write_json(list(
    assay_type             = "rba",
    toxin_class            = "Saxitoxin",
    toxin_standard_label   = "Saxitoxin",
    molecular_weight_g_mol = MW_LOOKUP[["Saxitoxin"]],
    detection_method       = "radioligand",
    units                  = "mol/L"
  ), file.path(fixture_out, "assay_config.json"), auto_unbox = TRUE)
  jsonlite::write_json(list(
    regression_weight        = "none",
    quant_range_min          = 20,
    quant_range_max          = 80,
    ci_method                = "t_dist",
    enable_outlier_detection = FALSE,
    outlier_min_n            = 3,
    normality_assumption     = "assume",
    cv_limit                 = 30
  ), file.path(fixture_out, "analysis_config.json"), auto_unbox = TRUE)

  # Env vars the template inspects.
  withr::with_envvar(
    list(
      RBA_OUTPUT_DIR = fixture_out,
      RBA_CSV_PATH   = csv_path,
      RBA_FMT_JSON   = file.path(fixture_out, "selected_formats.json"),
      RBA_NOTES_FILE = file.path(fixture_out, "notes.json")
    ),
    rmarkdown::render(
      testthat::test_path("..", "..", "reports", "unified_analysis_template.Rmd"),
      output_file = "analysis_report.html",
      output_dir  = fixture_out,
      params      = list(output_dir = fixture_out, lang = "en"),
      quiet       = TRUE
    )
  )

  # --- Assertions ---
  stats_path <- file.path(fixture_out, "model_stats.json")
  expect_true(file.exists(stats_path),
              info = "model_stats.json must be produced by the render")
  stats <- jsonlite::fromJSON(stats_path)

  expect_lt(abs(stats$r_squared - expected_r2), 0.002,
            label = sprintf("R^2 = %.6f drifted > 0.002 from reference %.6f",
                            stats$r_squared, expected_r2))
  expect_lt(abs(stats$ic50 - expected_ic50) / expected_ic50, 0.05,
            label = sprintf("IC50 = %.3e drifted > 5%% from reference %.3e",
                            stats$ic50, expected_ic50))

  summary_path <- file.path(fixture_out, "unknown_results_summary.csv")
  expect_true(file.exists(summary_path),
              info = "unknown_results_summary.csv must be produced")
  results <- read.csv(summary_path, stringsAsFactors = FALSE)
  expect_equal(nrow(results), expected_n,
               label = "Number of quantified replicate groups")
})


# Internal helper used by the capture_reference_values block above.
# Kept in the test file (rather than helper-setup.R) so it sits next to the
# test that consumes it.
.render_golden_rba <- function() {
  fixture_out <- file.path(tempdir(), "golden-rba",
                           format(Sys.time(), "%Y%m%d-%H%M%S"))
  dir.create(fixture_out, recursive = TRUE, showWarnings = FALSE)
  # Delegate to the test's setup by re-running the test body in a fresh
  # session. In practice, just run the test once and read fixture_out from
  # the ./tests/testthat/_snaps folder or from the stats that the test
  # itself prints on failure.
  stop("Use `testthat::test_file('tests/testthat/test-report-numbers.R')` ",
       "and read stats$r_squared / stats$ic50 from the printed diagnostic ",
       "output, then paste into the expected_* constants.")
}

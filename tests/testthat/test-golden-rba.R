# Golden regression test for RBA pipeline artifacts.
#
# Compares model_stats.json, unknown_results.csv, and
# unknown_results_summary.csv produced by the current pipeline against
# reference files stored in tests/testthat/golden/rba/.
#
# SETUP: Run `Rscript scripts/capture_golden.R` once from the repo root,
# then commit the files it writes to tests/testthat/golden/rba/.
# Until those files exist this test silently skips.

local({
  diff_script <- tryCatch(testthat::test_path("diff_golden_artifacts.R"),
                          error = function(e) "")
  if (nzchar(diff_script) && file.exists(diff_script)) source(diff_script)
})

test_that("RBA golden artifacts match snapshot", {
  skip_on_cran()
  skip_if_not_installed("rmarkdown")
  skip_if_not_installed("drc")
  skip_if_not_installed("jsonlite")
  skip_if_not_installed("withr")
  skip_if_not(exists("diff_json"),
              "diff_json() unavailable — diff_golden_artifacts.R not loaded")

  golden_dir <- testthat::test_path("golden", "rba")
  skip_if(
    !dir.exists(golden_dir) ||
      !file.exists(file.path(golden_dir, "model_stats.json")),
    "Golden RBA artifacts missing — run: Rscript scripts/capture_golden.R"
  )

  fixture_out <- file.path(tempdir(),
                           paste0("golden-rba-", format(Sys.time(), "%Y%m%d%H%M%S")))
  dir.create(fixture_out, recursive = TRUE, showWarnings = FALSE)

  n_std    <- 8L
  raw      <- read.table(
    testthat::test_path("..", "..", "examples", "rba_stx_example.csv"),
    header = FALSE, sep = "\t", stringsAsFactors = FALSE
  )
  meas_mat <- as.data.frame(apply(raw[, 2:13], 2, as.numeric))
  df_long  <- matrix_to_long(
    create_type_matrix("rba", n_std),
    create_id_matrix("rba", n_std),
    create_dilution_matrix(),
    create_replicate_matrix("rba"),
    meas_mat,
    std_conc = DEFAULT_STX_CONC[1:n_std]
  )
  csv_path <- file.path(fixture_out, "long_data_output.csv")
  write.csv(df_long, csv_path, row.names = FALSE)

  jsonlite::write_json(list("html"),
    file.path(fixture_out, "selected_formats.json"), auto_unbox = TRUE)
  jsonlite::write_json(list(notes = ""),
    file.path(fixture_out, "notes.json"), auto_unbox = TRUE)
  jsonlite::write_json(list(
    qc_concentration = "3e-9", expected_hill = "1",
    assay_type = "rba", detection_method = "radioligand", analyte = "Saxitoxin"
  ), file.path(fixture_out, "qc_params.json"), auto_unbox = TRUE)
  jsonlite::write_json(list(
    assay_type = "rba", toxin_class = "Saxitoxin",
    toxin_standard_label = "Saxitoxin",
    molecular_weight_g_mol = MW_LOOKUP[["Saxitoxin"]],
    detection_method = "radioligand", units = "mol/L"
  ), file.path(fixture_out, "assay_config.json"), auto_unbox = TRUE)
  jsonlite::write_json(list(
    regression_weight = "none", quant_range_min = 20, quant_range_max = 80,
    ci_method = "t_dist", enable_outlier_detection = FALSE,
    outlier_min_n = 3, normality_assumption = "assume", cv_limit = 30
  ), file.path(fixture_out, "analysis_config.json"), auto_unbox = TRUE)

  withr::with_envvar(
    list(
      RBA_OUTPUT_DIR = fixture_out,
      RBA_CSV_PATH   = csv_path,
      RBA_FMT_JSON   = file.path(fixture_out, "selected_formats.json"),
      RBA_NOTES_FILE = file.path(fixture_out, "notes.json")
    ),
    rmarkdown::render(
      testthat::test_path("..", "..", "reports", "unified_analysis_template.Rmd"),
      output_file   = "analysis_report.html",
      output_dir    = fixture_out,
      params        = list(output_dir = fixture_out, lang = "en"),
      quiet = TRUE,
      envir = new.env(parent = globalenv())
    )
  )

  tol <- 1e-4

  stats_path <- file.path(fixture_out, "model_stats.json")
  expect_true(file.exists(stats_path), info = "model_stats.json must be produced")

  drifts <- diff_json(file.path(golden_dir, "model_stats.json"), stats_path, tol)
  expect_equal(length(drifts), 0L,
               label = paste0("RBA model_stats.json drifted from golden:\n",
                              paste(drifts, collapse = "\n")))

  for (csv_name in c("unknown_results.csv", "unknown_results_summary.csv")) {
    g <- file.path(golden_dir, csv_name)
    a <- file.path(fixture_out, csv_name)
    if (file.exists(g) && file.exists(a)) {
      drifts <- diff_csv(g, a, tol)
      expect_equal(length(drifts), 0L,
                   label = paste0("RBA ", csv_name, " drifted from golden:\n",
                                  paste(drifts, collapse = "\n")))
    }
  }
})

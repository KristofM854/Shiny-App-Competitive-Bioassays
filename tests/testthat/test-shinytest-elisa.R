# End-to-end happy path: ELISA Cortisol with the Instant Demo button.
# Analogue of test-shinytest-rba.R for the ELISA path. Additionally asserts
# that the generated report contains the control summary ("Blank average"),
# which is a Cayman-protocol %B/B0-normalisation marker that must appear
# in a correctly rendered ELISA report.
#
# NOTE: clicks `qs_elisa_cortisol_demo`, the Instant-demo button added by
# task H1. The test skips with a helpful message until H1 is merged.
#
# Requires: shinytest2, chromote, and a local Chrome/Chromium install.

test_that("ELISA Instant Demo produces a complete report with control summary", {
  skip_on_cran()
  skip_if_not_installed("shinytest2")
  skip_if_not_installed("chromote")

  app_dir <- testthat::test_path("../..")

  app <- shinytest2::AppDriver$new(
    app_dir = app_dir,
    name = "elisa-instant-demo",
    load_timeout = 30000,
    timeout = 30000
  )
  on.exit(app$stop())
  app$set_window_size(width = 1400, height = 900)

  tryCatch(app$click(selector = ".modal-footer button"), error = function(e) NULL)
  app$wait_for_idle(500)

  if (!"qs_elisa_cortisol_demo" %in% names(app$get_values()$input)) {
    skip("qs_elisa_cortisol_demo input not present -- requires task H1 to be merged")
  }
  app$click("qs_elisa_cortisol_demo")
  app$wait_for_idle(2000)

  app$wait_for_value(output = "plate_heatmap", timeout = 15000)

  app$click("next_to_analysis")
  app$wait_for_idle(500)
  app$click("next_to_report")
  app$wait_for_idle(500)

  app$click("convert")
  Sys.sleep(5)
  app$wait_for_idle(timeout = 180000)

  # Locate the output directory written by the app. In standalone mode (no
  # RBA_OUTPUT_DIR env var set), the app creates:
  #   <app_dir>/reports/runs/<YYYYMMDD_HHMMSS>/run_<HHMMSS>/
  # Strategy: find the most recently modified leaf run_* directory under
  # <app_dir>/reports/runs/. This is robust regardless of exact timestamps.
  runs_base <- file.path(app_dir, "reports", "runs")

  output_dir <- NULL
  if (dir.exists(runs_base)) {
    # Session-level dirs: <YYYYMMDD_HHMMSS>/
    session_dirs <- list.dirs(runs_base, recursive = FALSE)
    if (length(session_dirs) > 0) {
      # Most recent session dir
      newest_session <- session_dirs[which.max(file.mtime(session_dirs))]
      # Per-run dirs: run_<HHMMSS>/ inside the session dir
      run_dirs <- list.dirs(newest_session, recursive = FALSE)
      run_dirs <- run_dirs[grepl("^run_", basename(run_dirs))]
      if (length(run_dirs) > 0) {
        output_dir <- run_dirs[which.max(file.mtime(run_dirs))]
      } else {
        # Fallback: session dir itself is the output dir (older layout)
        output_dir <- newest_session
      }
    }
  }

  # If standalone discovery failed, fall back to the env var (set by
  # run_local.R or external CI wrappers).
  if (is.null(output_dir) || !dir.exists(output_dir)) {
    output_dir <- Sys.getenv("RBA_OUTPUT_DIR")
  }

  expect_true(nzchar(output_dir), info = "output_dir must be non-empty")
  expect_true(dir.exists(output_dir), info = "output_dir must exist on disk")

  html_files <- list.files(output_dir, pattern = "\\.html$", full.names = TRUE,
                           recursive = TRUE)
  expect_gt(length(html_files), 0)
  expect_gt(file.info(html_files[1])$size, 10000)

  # Cayman-protocol control summary must appear in the rendered report.
  html <- paste(readLines(html_files[1], warn = FALSE), collapse = "\n")
  expect_match(html, "Blank average", fixed = TRUE,
               info = "ELISA report is expected to include the Blank average line from the control summary.")

  # ---- AUDIT-015: content assertions on model_stats.json ----
  # The report pipeline writes model_stats.json into the output directory
  # after DRC fitting. A passing file-size check does not guarantee the model
  # converged or that values are non-NA -- these assertions catch that case.
  model_stats_path <- file.path(output_dir, "model_stats.json")
  expect_true(file.exists(model_stats_path),
              info = "model_stats.json must exist in output_dir")

  if (file.exists(model_stats_path)) {
    stats <- jsonlite::read_json(model_stats_path, simplifyVector = TRUE)

    # R-squared must be present and reflect a well-fitted standard curve (>= 0.95).
    expect_true(!is.null(stats$r_squared),
                info = "r_squared must be present in model_stats.json")
    expect_gte(as.numeric(stats$r_squared), 0.95,
               label = "ELISA R2 >= 0.95")

    # IC50 must be a finite numeric value (ELISA cortisol standard curve is a
    # 4PL sigmoidal fit; IC50 represents the inflection point and must be
    # quantifiable for valid sample back-calculation).
    expect_true(!is.null(stats$ic50),
                info = "ic50 must be present in model_stats.json")
    ic50_val <- suppressWarnings(as.numeric(stats$ic50))
    expect_false(is.na(ic50_val),
                 info = "ic50 must be a finite numeric value, not NA")
    expect_true(is.finite(ic50_val),
                info = "ic50 must be finite (not Inf/-Inf)")

    # Hill slope must be present and positive for a direct (non-competitive)
    # ELISA assay (%B/B0 increases with analyte concentration -> rising curve).
    expect_true(!is.null(stats$hill_slope),
                info = "hill_slope must be present in model_stats.json")
    expect_gt(as.numeric(stats$hill_slope), 0,
              label = "ELISA hill_slope must be positive (direct assay)")

    # n_standards must be a positive integer.
    expect_true(!is.null(stats$n_standards),
                info = "n_standards must be present in model_stats.json")
    expect_gt(as.numeric(stats$n_standards), 0,
              label = "n_standards > 0")
  }
})

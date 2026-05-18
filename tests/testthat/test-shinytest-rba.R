# End-to-end happy path: RBA Saxitoxin with the Instant Demo button.
# Loads example data, advances through all 5 tabs, generates a report,
# and asserts that an HTML report file is written to the output dir.
#
# NOTE: clicks `qs_rba_stx_demo`, which is the Instant-demo button IDs
# created by task H1. This test is expected to skip with a clear message
# until H1 is merged and that widget id exists.
#
# Requires: shinytest2, chromote, and a local Chrome/Chromium install.

test_that("RBA Instant Demo produces a complete report", {
  skip_on_cran()
  skip_if_not_installed("shinytest2")
  skip_if_not_installed("chromote")

  app_dir <- testthat::test_path("../..")

  app <- shinytest2::AppDriver$new(
    app_dir = app_dir,
    name = "rba-instant-demo",
    load_timeout = 30000,
    timeout = 30000
  )
  on.exit(app$stop())
  app$set_window_size(width = 1400, height = 900)

  # Dismiss welcome modal if present.
  tryCatch(app$click(selector = ".modal-footer button"), error = function(e) NULL)
  app$wait_for_idle(500)

  # Click the RBA Saxitoxin "Instant demo" button (added in H1).
  # If H1 hasn't landed yet, skip with a helpful message so this doesn't
  # fail CI for unrelated branches.
  if (!"qs_rba_stx_demo" %in% names(app$get_values()$input)) {
    skip("qs_rba_stx_demo input not present -- requires task H1 to be merged")
  }
  app$click("qs_rba_stx_demo")
  app$wait_for_idle(2000)

  # Should now be on upload tab with data loaded; heatmap visible.
  app$wait_for_value(output = "plate_heatmap", timeout = 15000)

  # Advance to analysis tab
  app$click("next_to_analysis")
  app$wait_for_idle(500)

  # Advance to report tab
  app$click("next_to_report")
  app$wait_for_idle(500)

  # Generate report. DRC fit + knitr render can be slow; allow up to 3min.
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

  html_files <- list.files(output_dir, pattern = "\\.html$", full.names = TRUE)
  expect_gt(length(html_files), 0)
  expect_gt(file.info(html_files[1])$size, 10000)  # non-trivially sized

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
               label = "RBA R2 >= 0.95")

    # Hill slope must be present and negative for a competitive (inhibition)
    # assay (RBA saxitoxin displaces radiolabelled ligand -> decreasing curve).
    expect_true(!is.null(stats$hill_slope),
                info = "hill_slope must be present in model_stats.json")
    expect_lt(as.numeric(stats$hill_slope), 0,
              label = "RBA hill_slope must be negative (competitive assay)")

    # n_standards must be a positive integer.
    expect_true(!is.null(stats$n_standards),
                info = "n_standards must be present in model_stats.json")
    expect_gt(as.numeric(stats$n_standards), 0,
              label = "n_standards > 0")
  }
})

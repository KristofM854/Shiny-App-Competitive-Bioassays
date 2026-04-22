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

  app <- shinytest2::AppDriver$new(
    app_dir = testthat::test_path("../.."),
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
    skip("qs_rba_stx_demo input not present — requires task H1 to be merged")
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

  # Assert that an HTML report was written to the standalone output dir.
  output_dir <- Sys.getenv("RBA_OUTPUT_DIR")
  expect_true(nzchar(output_dir))
  expect_true(dir.exists(output_dir))

  html_files <- list.files(output_dir, pattern = "\\.html$", full.names = TRUE)
  expect_gt(length(html_files), 0)
  expect_gt(file.info(html_files[1])$size, 10000)  # non-trivially sized
})

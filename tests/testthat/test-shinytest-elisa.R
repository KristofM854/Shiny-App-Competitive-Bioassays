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

  app <- shinytest2::AppDriver$new(
    app_dir = testthat::test_path("../.."),
    name = "elisa-instant-demo",
    load_timeout = 30000,
    timeout = 30000
  )
  on.exit(app$stop())
  app$set_window_size(width = 1400, height = 900)

  tryCatch(app$click(selector = ".modal-footer button"), error = function(e) NULL)
  app$wait_for_idle(500)

  if (!"qs_elisa_cortisol_demo" %in% names(app$get_values()$input)) {
    skip("qs_elisa_cortisol_demo input not present — requires task H1 to be merged")
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

  output_dir <- Sys.getenv("RBA_OUTPUT_DIR")
  expect_true(nzchar(output_dir))
  expect_true(dir.exists(output_dir))

  html_files <- list.files(output_dir, pattern = "\\.html$", full.names = TRUE)
  expect_gt(length(html_files), 0)
  expect_gt(file.info(html_files[1])$size, 10000)

  # Cayman-protocol control summary must appear in the rendered report.
  html <- paste(readLines(html_files[1], warn = FALSE), collapse = "\n")
  expect_match(html, "Blank average", fixed = TRUE,
               info = "ELISA report is expected to include the Blank average line from the control summary.")
})

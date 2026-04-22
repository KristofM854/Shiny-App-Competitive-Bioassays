# End-to-end happy path: multi-wavelength ELISA via Classic Import.
# Uploads the synthetic fixture (two plates at 450 nm and 630 nm), navigates
# through the wizard, generates a report, and asserts:
#   1) per-wavelength CSVs are written (one per detected wavelength)
#   2) the rendered HTML contains the Bland-Altman concordance section
#
# Fixture: tests/testthat/fixtures/multiwave_synthetic.csv (regenerate via
# tests/testthat/fixtures/generate_multiwave.R).
#
# Requires: shinytest2, chromote, and a local Chrome/Chromium install.

test_that("Multi-wavelength upload produces per-wavelength CSVs and a concordance report", {
  skip_on_cran()
  skip_if_not_installed("shinytest2")
  skip_if_not_installed("chromote")

  fixture <- testthat::test_path("fixtures", "multiwave_synthetic.csv")
  skip_if_not(file.exists(fixture),
              "multi-wavelength fixture not found; regenerate via generate_multiwave.R")

  app <- shinytest2::AppDriver$new(
    app_dir = testthat::test_path("../.."),
    name = "multiwave-upload",
    load_timeout = 30000,
    timeout = 30000
  )
  on.exit(app$stop())
  app$set_window_size(width = 1400, height = 900)

  tryCatch(app$click(selector = ".modal-footer button"), error = function(e) NULL)
  app$wait_for_idle(500)

  # Configure as ELISA with Cortisol defaults (matches the fixture's shape).
  app$set_inputs(assay_type = "elisa")
  app$wait_for_idle(500)

  # Jump straight to Upload & Preview tab (no preset click; we're uploading).
  app$set_inputs(wizard_tabs = "tab_upload")
  app$wait_for_idle(500)

  # Keep Classic Import (default). Upload the fixture.
  app$upload_file(upload_counts = fixture)
  app$wait_for_idle(3000)

  # Multi-wavelength detection should have populated the wavelength-plates
  # reactive; the heatmap previews the primary (450 nm) plate.
  app$wait_for_value(output = "plate_heatmap", timeout = 15000)

  app$click("next_to_analysis")
  app$wait_for_idle(500)
  app$click("next_to_report")
  app$wait_for_idle(500)

  app$click("convert")
  Sys.sleep(5)
  app$wait_for_idle(timeout = 240000)

  output_dir <- Sys.getenv("RBA_OUTPUT_DIR")
  expect_true(nzchar(output_dir))
  expect_true(dir.exists(output_dir))

  # Per-wavelength result CSVs: one unknown_results(_summary).csv per wavelength
  # is written into output_dir (named with a 450 / 630 suffix by the pipeline).
  csvs <- list.files(output_dir, pattern = "unknown_results.*\\.csv$")
  expect_gt(length(csvs), 1,
            label = "At least one CSV per wavelength must be present")

  # Multi-wavelength template renders the Bland-Altman concordance block.
  html_files <- list.files(output_dir, pattern = "\\.html$", full.names = TRUE)
  expect_gt(length(html_files), 0)
  html <- paste(readLines(html_files[1], warn = FALSE), collapse = "\n")
  expect_match(html, "Bland-Altman", fixed = TRUE,
               info = "Multi-wavelength report should contain the Bland-Altman concordance section.")
})

test_that("AUDIT-024: shiny.maxRequestSize matches MAX_UPLOAD_SIZE_MB", {
  env <- new.env(parent = emptyenv())
  # MAX_UPLOAD_SIZE_MB is defined in global.R which is sourced by helper-setup.R
  # so it should already be available
  skip_if(!exists("MAX_UPLOAD_SIZE_MB"))
  expected_bytes <- MAX_UPLOAD_SIZE_MB * 1024^2
  actual <- getOption("shiny.maxRequestSize")
  expect_equal(actual, expected_bytes)
})

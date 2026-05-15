test_that("AUDIT-010: RBA_OUTPUT_DIR not set at package load time", {
  # The env var should not be set unconditionally by sourcing global.R
  # (we can't easily test session$userData in unit tests, but we can verify
  # that global.R does not call Sys.setenv unconditionally)
  lines <- readLines("global.R")
  setenv_lines <- grep("Sys\\.setenv\\(RBA_OUTPUT_DIR", lines)
  expect_length(setenv_lines, 0L)
})

test_that("AUDIT-011: theme_set not called in global.R", {
  lines <- readLines("global.R")
  theme_set_lines <- grep("theme_set\\(", lines)
  expect_length(theme_set_lines, 0L)
})

# AUDIT-025: Output filenames embed the report language code
# (report_pipeline.R: out_name <- paste0(base_out, "-", variant, "-",
# lang_code)). The UI restricts the choice to a fixed selectInput, but a
# malicious client could Shiny.setInputValue("report_languages",
# "../../../etc/passwd"). validate_lang_code() is the server-side
# defence-in-depth that keeps a crafted value from reaching a file path.
# validate_lang_code() is a top-level function in server/report_pipeline.R,
# sourced by helper-setup.R.

test_that("AUDIT-025: validate_lang_code rejects path-traversal language codes", {
  skip_if_not(exists("validate_lang_code"),
              "validate_lang_code() not available (report_pipeline.R not loaded)")

  # Legitimate codes pass through unchanged (order-preserving, de-duplicated)
  expect_equal(validate_lang_code(c("en", "es")), c("en", "es"))
  expect_equal(validate_lang_code("fr"), "fr")
  expect_equal(validate_lang_code(c("en", "en", "es")), c("en", "es"))

  # Path-traversal / injection attempts are stripped, valid codes retained
  expect_equal(
    suppressWarnings(validate_lang_code(c("en", "../../../etc/passwd", "es"))),
    c("en", "es"))

  # If nothing valid remains, fall back to "en" (never empty, never traversal)
  expect_equal(suppressWarnings(validate_lang_code("../../etc/passwd")), "en")
  expect_equal(suppressWarnings(validate_lang_code("en/../../secret")), "en")
  expect_equal(suppressWarnings(validate_lang_code(character(0))), "en")
  expect_equal(suppressWarnings(validate_lang_code(NULL)), "en")

  # A dropped code must emit a warning (audit trail; visible since AUDIT-014)
  expect_warning(validate_lang_code(c("en", "../etc")),
                 "unsupported report language code")

  # The sanitized code, embedded in the output-filename pattern, is always a
  # plain filename component: no separators, no parent-dir references.
  crafted <- c("../../x", "..\\..\\y", "en/../z", "es")
  for (lc in suppressWarnings(validate_lang_code(crafted))) {
    out_name <- paste0("RBA-results-report-full-", lc)
    expect_false(grepl("[/\\\\]", out_name),
                 info = "sanitized output name must not contain path separators")
    expect_false(grepl("\\.\\.", out_name),
                 info = "sanitized output name must not contain parent-dir refs")
  }
})

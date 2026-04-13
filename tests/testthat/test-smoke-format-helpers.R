# =============================================================================
# Smoke tests: format-aware rendering helpers
# Validates that helpers produce correct output types without requiring
# a full knitr rendering environment.
# =============================================================================

test_that("is_html_out returns logical", {
  result <- is_html_out()
  expect_type(result, "logical")
  expect_length(result, 1)
})

test_that("is_docx_out returns logical", {
  result <- is_docx_out()
  expect_type(result, "logical")
  expect_length(result, 1)
})

test_that("section_open returns a string for any format", {
  result <- section_open("Test Title")
  expect_type(result, "character")
  expect_length(result, 1)
  expect_true(nchar(result) > 0)
})

test_that("section_close returns a string", {
  result <- section_close()
  expect_type(result, "character")
  expect_length(result, 1)
})

test_that("emit_heading produces output via cat", {
  out <- capture.output(emit_heading("Test Heading", 3), type = "output")
  combined <- paste(out, collapse = "\n")
  # In non-HTML context (test runner), should produce markdown heading
  expect_true(grepl("Test Heading", combined))
})

test_that("emit_styled_block handles plain markdown fallback", {
  lines <- c("**Title**", "Body text here")
  out <- capture.output(emit_styled_block(lines, html_style = "color:red;"), type = "output")
  combined <- paste(out, collapse = "\n")
  # In non-HTML context, should strip HTML and produce plain text
  expect_true(grepl("Title", combined))
  expect_true(grepl("Body text here", combined))
})

test_that("render_plot returns invisible NULL for non-HTML", {
  skip_if(!requireNamespace("ggplot2", quietly = TRUE), "ggplot2 not available")
  p <- ggplot2::ggplot(data.frame(x = 1:3, y = 1:3), ggplot2::aes(x, y)) +
    ggplot2::geom_point()
  # In non-HTML context (test runner), should print static plot
  result <- render_plot(p)
  expect_null(result)
})

test_that("pdf_render_available returns logical", {
  result <- pdf_render_available()
  expect_type(result, "logical")
  expect_length(result, 1)
})

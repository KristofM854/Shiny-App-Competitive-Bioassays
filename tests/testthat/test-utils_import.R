# Helper: create a test plate file
create_test_plate <- function(filepath, with_labels = TRUE,
                               decimal_sep = ".", overflow_cell = FALSE) {
  set.seed(42)
  values <- matrix(round(runif(96, 100, 10000), 1), nrow = 8, ncol = 12)

  lines <- c()
  for (r in 1:8) {
    row_vals <- as.character(values[r, ])
    if (overflow_cell && r == 1) row_vals[1] <- "OVER"
    if (decimal_sep == ",") row_vals <- gsub("\\.", ",", row_vals)
    prefix <- if (with_labels) paste0(LETTERS[r], "\t") else ""
    lines <- c(lines, paste0(prefix, paste(row_vals, collapse = "\t")))
  }
  writeLines(lines, filepath)
  return(values)
}

test_that("import_plate_data reads labeled plate", {
  tmpfile <- tempfile(fileext = ".txt")
  on.exit(unlink(tmpfile))
  expected <- create_test_plate(tmpfile, with_labels = TRUE)

  result <- import_plate_data(tmpfile)
  expect_equal(nrow(result), 8)
  expect_equal(ncol(result), 12)
  expect_equal(sum(!is.na(result)), 96)
  expect_equal(as.numeric(result[1, 1]), expected[1, 1])
})

test_that("import_plate_data reads unlabeled plate", {
  tmpfile <- tempfile(fileext = ".txt")
  on.exit(unlink(tmpfile))
  create_test_plate(tmpfile, with_labels = FALSE)

  result <- import_plate_data(tmpfile)
  expect_equal(nrow(result), 8)
  expect_equal(ncol(result), 12)
})

test_that("import_plate_data handles overflow markers", {
  tmpfile <- tempfile(fileext = ".txt")
  on.exit(unlink(tmpfile))
  create_test_plate(tmpfile, with_labels = TRUE, overflow_cell = TRUE)

  expect_warning(result <- import_plate_data(tmpfile), "overflow")
  expect_true(is.na(result[1, 1]))
  expect_false(is.na(result[1, 2]))
})

test_that("import_plate_data handles European decimal format", {
  tmpfile <- tempfile(fileext = ".txt")
  on.exit(unlink(tmpfile))
  create_test_plate(tmpfile, with_labels = TRUE, decimal_sep = ",")

  expect_warning(result <- import_plate_data(tmpfile), "European decimal")
  expect_equal(nrow(result), 8)
  expect_equal(ncol(result), 12)
  expect_true(sum(!is.na(result)) >= 90)
})

test_that("import_plate_data rejects non-plate data", {
  tmpfile <- tempfile(fileext = ".csv")
  on.exit(unlink(tmpfile))
  writeLines(c("Name,Age,City", "Alice,30,NYC", "Bob,25,LA"), tmpfile)

  expect_error(import_plate_data(tmpfile))
})

test_that("detect_plate_location returns NULL for non-plate", {
  tmpfile <- tempfile(fileext = ".csv")
  on.exit(unlink(tmpfile))
  writeLines(c("Name,Age,City", "Alice,30,NYC", "Bob,25,LA"), tmpfile)

  expect_null(detect_plate_location(tmpfile))
})

# =============================================================================
# Smoke tests: import pipeline
# Validates that the unified import API works for representative fixtures.
# =============================================================================

fixture_dir <- file.path("tests", "testthat", "fixtures")
if (!dir.exists(fixture_dir)) fixture_dir <- "fixtures"

# ---------------------------------------------------------------------------
# parse_plate_file — the unified entry point
# ---------------------------------------------------------------------------

test_that("parse_plate_file reads RBA fixture as single-wave", {
  f <- file.path(fixture_dir, "rba_nominal.csv")
  skip_if(!file.exists(f), "RBA fixture not found")

 result <- parse_plate_file(f)
  expect_false(result$is_multiwavelength)
  expect_equal(result$format, "CSV")
  expect_true(result$detected_wells > 0)
  expect_equal(nrow(result$plates$primary), 8)
  expect_equal(ncol(result$plates$primary), 12)
})

test_that("parse_plate_file reads ELISA fixture as single-wave", {
  f <- file.path(fixture_dir, "elisa_nominal.csv")
  skip_if(!file.exists(f), "ELISA fixture not found")

  result <- parse_plate_file(f)
  expect_false(result$is_multiwavelength)
  expect_equal(nrow(result$plates$primary), 8)
  expect_equal(ncol(result$plates$primary), 12)
  # ELISA values are small (absorbance 0-2)
  vals <- as.numeric(unlist(result$plates$primary))
  expect_true(max(vals, na.rm = TRUE) < 5)
})

test_that("parse_plate_file handles partial plate (6 columns)", {
  f <- file.path(fixture_dir, "partial_plate_6col.csv")
  skip_if(!file.exists(f), "Partial plate fixture not found")

  result <- parse_plate_file(f)
  expect_false(result$is_multiwavelength)
  expect_true(result$partial_plate)
  # Should be padded to 12 columns
  expect_equal(ncol(result$plates$primary), 12)
  # First 6 columns have data, last 6 are NA
  expect_true(all(!is.na(result$plates$primary[1, 1:6])))
  expect_true(all(is.na(result$plates$primary[1, 7:12])))
})

# ---------------------------------------------------------------------------
# read_file_raw — shared low-level reader
# ---------------------------------------------------------------------------

test_that("read_file_raw returns a data.frame", {
  f <- file.path(fixture_dir, "rba_nominal.csv")
  skip_if(!file.exists(f), "RBA fixture not found")

  raw <- read_file_raw(f)
  expect_s3_class(raw, "data.frame")
  expect_true(nrow(raw) >= 8)
})

# ---------------------------------------------------------------------------
# detect_plate_location — reuses pre-read data
# ---------------------------------------------------------------------------

test_that("detect_plate_location works with pre-read raw_data", {
  f <- file.path(fixture_dir, "rba_nominal.csv")
  skip_if(!file.exists(f), "RBA fixture not found")

  raw <- read_file_raw(f)
  loc <- detect_plate_location(f, raw_data = raw)
  expect_false(is.null(loc))
  expect_equal(loc$nrows, 8)
  expect_true(loc$ncols >= 4)
})

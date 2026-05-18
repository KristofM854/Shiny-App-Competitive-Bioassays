# Tests for AUDIT-012 (semicolon CSV separator), AUDIT-013 (comma decimal),
# and AUDIT-022 (UTF-8 BOM stripping / Latin-1 fallback) in utils_import_v3.R.

# ---------------------------------------------------------------------------
# AUDIT-012: semicolon-separated CSV files
# ---------------------------------------------------------------------------

test_that("AUDIT-012: parse_plate_file handles semicolon separator", {
  path <- file.path(.repo_root, "tests", "testthat", "fixtures", "semicolon_plate.csv")
  result <- parse_plate_file(path)
  plate  <- result$plates$primary

  # Data must be split into 12 columns (not everything in one column)
  expect_equal(ncol(plate), 12L)

  # All 96 wells should contain non-NA numeric values
  expect_equal(result$detected_wells, 96L)

  # Spot-check: first well of row A should be 8468 (not "8468;8611;..." as one string)
  expect_equal(plate["A", "1"], 8468)
})

test_that("AUDIT-012: detect_csv_sep identifies semicolon files correctly", {
  lines_semi  <- c("A;8468;8611;8742", "B;7668;7691;7751")
  lines_comma <- c("A,8468,8611,8742", "B,7668,7691,7751")
  lines_tab   <- c("A\t8468\t8611\t8742", "B\t7668\t7691\t7751")

  expect_equal(detect_csv_sep(lines_semi),  ";")
  expect_equal(detect_csv_sep(lines_comma), ",")
  expect_equal(detect_csv_sep(lines_tab),   "\t")
})

test_that("AUDIT-012: detect_csv_sep prefers semicolon when tied with comma (European locale)", {
  # European format: ';' as field separator, ',' as decimal mark — counts tie
  line <- "A;84,68;86,11;87,42"
  lines <- c(line, "B;76,68;76,91;77,51")
  expect_equal(detect_csv_sep(lines), ";")
})

# ---------------------------------------------------------------------------
# AUDIT-013: comma decimal separators
# ---------------------------------------------------------------------------

test_that("AUDIT-013: parse_plate_file converts comma-decimal values", {
  path <- file.path(.repo_root, "tests", "testthat", "fixtures", "comma_decimal_plate.csv")
  result <- parse_plate_file(path)
  plate  <- result$plates$primary

  # 12 data columns and all 96 wells filled
  expect_equal(ncol(plate), 12L)
  expect_equal(result$detected_wells, 96L)

  # Values should be numeric; "84,68" should become 84.68
  expect_equal(plate["A", "1"], 84.68)
  expect_equal(plate["A", "2"], 86.11)

  # No NAs should remain after conversion
  vals         <- unlist(plate)
  numeric_vals <- suppressWarnings(as.numeric(vals[!is.na(vals)]))
  expect_false(any(is.na(numeric_vals)),
               info = "comma-decimal values should be converted to dot-decimal without NA")
})

test_that("AUDIT-013: fix_comma_decimals converts matching columns only", {
  df <- data.frame(
    label   = c("A", "B", "C"),
    val1    = c("84,68", "76,68", "73,84"),
    val2    = c("86.11", "76.91", "72.60"),   # dot-decimal: should NOT change
    mixed   = c("1,23", "4.56", "7,89"),      # mixed: should NOT change
    stringsAsFactors = FALSE
  )
  result <- fix_comma_decimals(df)

  # label column: unchanged (no comma-decimal pattern)
  expect_equal(result$label, c("A", "B", "C"))

  # val1: all comma-decimal -> converted to numeric
  expect_equal(result$val1, c(84.68, 76.68, 73.84))

  # val2: dot-decimal -> unchanged character
  expect_equal(result$val2, c("86.11", "76.91", "72.60"))

  # mixed: not all comma-decimal -> unchanged character
  expect_equal(result$mixed, c("1,23", "4.56", "7,89"))
})

# ---------------------------------------------------------------------------
# AUDIT-022: UTF-8 BOM stripping and Latin-1 fallback
# ---------------------------------------------------------------------------

test_that("AUDIT-022: parse_plate_file strips UTF-8 BOM without corrupting first cell", {
  path <- file.path(.repo_root, "tests", "testthat", "fixtures", "utf8bom_plate.csv")
  result <- parse_plate_file(path)
  plate  <- result$plates$primary

  # Should detect all 96 wells (not misparse first cell due to BOM)
  expect_equal(ncol(plate), 12L)
  expect_equal(result$detected_wells, 96L)

  # First cell must be a clean numeric value, not prefixed with BOM character
  first_val <- as.character(plate["A", "1"])
  expect_false(
    nchar(first_val) > 0L && utf8ToInt(substr(first_val, 1L, 1L)) == 65279L,
    info = "BOM character (U+FEFF) must not appear in parsed cell values"
  )
  expect_equal(plate["A", "1"], 8468)
})

test_that("AUDIT-022: read_lines_safe strips BOM from first line", {
  path  <- file.path(.repo_root, "tests", "testthat", "fixtures", "utf8bom_plate.csv")
  lines <- read_lines_safe(path)

  expect_gt(length(lines), 0L)

  # After stripping, first character of first line must not be U+FEFF (65279)
  first_cp <- utf8ToInt(substr(lines[[1L]], 1L, 1L))
  expect_false(first_cp == 65279L,
               info = "BOM (U+FEFF) should be stripped from the first line")

  # And the first line should start with a real data character
  expect_equal(substr(lines[[1L]], 1L, 1L), "A")
})

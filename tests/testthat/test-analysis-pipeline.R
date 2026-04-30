# Unit tests for reports/analysis_pipeline.R helpers
# (IMPLEMENTATION_PLAN.md §M1)

# --- M1.5 assess_plate_positional() ----------------------------------------

test_that("assess_plate_positional returns NULL for empty input", {
  expect_null(assess_plate_positional(NULL))
  expect_null(assess_plate_positional(data.frame()))
  expect_null(assess_plate_positional(data.frame(Row = "A", Column = "1")))
})

test_that("assess_plate_positional returns NULL when no finite values exist", {
  df <- data.frame(
    Row = rep("A", 3), Column = c("1", "2", "3"),
    MeasurementValue = c(NA_real_, NaN, NA_real_)
  )
  expect_null(assess_plate_positional(df))
})

test_that("assess_plate_positional flags zero rows/cols on uniform data", {
  rows <- rep(LETTERS[1:8], times = 12)
  cols <- as.character(rep(1:12, each = 8))
  df <- data.frame(Row = rows, Column = cols,
                   MeasurementValue = rep(1.0, 96))
  res <- assess_plate_positional(df)
  expect_type(res, "list")
  expect_equal(res$plate_mean, 1.0)
  expect_equal(res$plate_sd, 0)
  expect_equal(res$flagged_rows, 0L)
  expect_equal(res$flagged_cols, 0L)
  expect_equal(nrow(res$row_stats), 8L)
  expect_equal(nrow(res$col_stats), 12L)
  expect_setequal(res$row_stats$Flag, "")
  expect_setequal(res$col_stats$Flag, "")
})

test_that("assess_plate_positional flags an outlier row past 2 SD", {
  set.seed(42)
  rows <- rep(LETTERS[1:8], times = 12)
  cols <- as.character(rep(1:12, each = 8))
  vals <- rnorm(96, mean = 100, sd = 1)
  # Force row "H" far above plate mean
  vals[rows == "H"] <- 1000
  df <- data.frame(Row = rows, Column = cols, MeasurementValue = vals)
  res <- assess_plate_positional(df)
  expect_gte(res$flagged_rows, 1L)
  expect_true("H" %in% res$row_stats$Row[res$row_stats$Flag != ""])
})

test_that("assess_plate_positional ignores non-finite values in stats", {
  rows <- rep(LETTERS[1:8], times = 12)
  cols <- as.character(rep(1:12, each = 8))
  vals <- rep(2.0, 96)
  vals[1:5] <- NA_real_
  df <- data.frame(Row = rows, Column = cols, MeasurementValue = vals)
  res <- assess_plate_positional(df)
  expect_equal(res$plate_mean, 2.0)
  # row_stats / col_stats reflect the surviving values; no row should flag
  expect_equal(res$flagged_rows, 0L)
  expect_equal(res$flagged_cols, 0L)
})

test_that("assess_plate_positional preserves the column contract", {
  rows <- rep(LETTERS[1:8], times = 12)
  cols <- as.character(rep(1:12, each = 8))
  df <- data.frame(Row = rows, Column = cols,
                   MeasurementValue = seq_len(96) * 1.0)
  res <- assess_plate_positional(df)
  expect_named(res, c("plate_mean", "plate_sd", "row_stats", "col_stats",
                      "flagged_rows", "flagged_cols"))
  expect_named(res$row_stats, c("Row", "Mean", "SD", "Flag"))
  expect_named(res$col_stats, c("Column", "Mean", "SD", "Flag"))
})

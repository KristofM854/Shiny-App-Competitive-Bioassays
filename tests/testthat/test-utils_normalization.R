test_that("calculate_elisa_bb0 produces correct values", {
  # Known: Blank=0.05, NSB=0.15, B0=1.05
  # Corrected NSB = 0.15 - 0.05 = 0.10
  # Corrected B0  = 1.05 - 0.05 = 1.00
  # Corrected B0 net = 1.00 - 0.10 = 0.90
  #
  # For Standard at 0.55:
  #   blank_corrected = 0.55 - 0.05 = 0.50
  #   nsb_corrected = 0.50 - 0.10 = 0.40
  #   %B/B0 = 0.40 / 0.90 * 100 = 44.44%

  data_long <- data.frame(
    Well = c("A1", "B1", "C1", "D1"),
    SampleType = c("Blank", "NSB", "B0", "Standard"),
    MeasurementValue = c(0.05, 0.15, 1.05, 0.55),
    stringsAsFactors = FALSE
  )

  result <- calculate_elisa_bb0(data_long, use_percent = TRUE)

  std_row <- result[result$SampleType == "Standard", ]
  expect_equal(round(std_row$calculated_bb0, 2), 44.44)

  b0_row <- result[result$SampleType == "B0", ]
  expect_equal(round(b0_row$calculated_bb0, 2), 100.00)

  nsb_row <- result[result$SampleType == "NSB", ]
  expect_equal(round(nsb_row$calculated_bb0, 2), 0.00)
})

test_that("calculate_elisa_bb0 fails when B0 <= NSB", {
  data_long <- data.frame(
    Well = c("A1", "B1", "C1"),
    SampleType = c("Blank", "NSB", "B0"),
    MeasurementValue = c(0.05, 1.5, 0.5),
    stringsAsFactors = FALSE
  )
  expect_error(calculate_elisa_bb0(data_long), "Invalid control well hierarchy")
})

test_that("calculate_elisa_bb0 fails with missing controls", {
  data_long <- data.frame(
    Well = c("A1", "B1"),
    SampleType = c("Blank", "Standard"),
    MeasurementValue = c(0.05, 0.55),
    stringsAsFactors = FALSE
  )
  expect_error(calculate_elisa_bb0(data_long), "Missing control wells")
})

test_that("normalize_data for RBA passes through raw values", {
  data_long <- data.frame(
    Well = "A1", SampleType = "Standard",
    MeasurementValue = 12345, stringsAsFactors = FALSE
  )
  result <- normalize_data(data_long, "rba", "radioligand")
  expect_equal(result$NormalizedValue, 12345)
  expect_equal(result$ResponseUnit, "CPM")
})

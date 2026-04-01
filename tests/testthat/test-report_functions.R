test_that("validate_input_data passes with valid data", {
  valid_data <- data.frame(
    SampleID = c(paste0("S", 1:4), "Smp1"),
    MeasurementValue = c(1000, 800, 600, 400, 550),
    DilutionFactor = c(1, 1, 1, 1, 1),
    StandardConc = c(1e-6, 1e-7, 1e-8, 1e-9, NA),
    SampleType = c(rep("Standard", 4), "Sample"),
    Replicate = c("S1", "S2", "S3", "S4", "R1"),
    stringsAsFactors = FALSE
  )
  expect_true(validate_input_data(valid_data))
})

test_that("validate_input_data fails with missing columns", {
  bad_data <- data.frame(SampleID = "S1", MeasurementValue = 1000)
  expect_error(validate_input_data(bad_data), "Missing required columns")
})

test_that("validate_input_data fails with negative dilution", {
  bad_data <- data.frame(
    SampleID = c("S1", "S2", "S3", "S4"), MeasurementValue = c(1000, 800, 600, 400),
    DilutionFactor = c(-1, 1, 1, 1), StandardConc = c(1e-6, 1e-7, 1e-8, 1e-9),
    SampleType = rep("Standard", 4), Replicate = paste0("S", 1:4),
    stringsAsFactors = FALSE
  )
  expect_error(validate_input_data(bad_data), "Invalid dilution factors")
})

test_that("validate_input_data fails with <4 standards", {
  bad_data <- data.frame(
    SampleID = c("S1", "S2", "Smp1"), MeasurementValue = c(1000, 800, 550),
    DilutionFactor = c(1, 1, 1), StandardConc = c(1e-6, 1e-7, NA),
    SampleType = c("Standard", "Standard", "Sample"),
    Replicate = c("S1", "S2", "R1"), stringsAsFactors = FALSE
  )
  expect_error(validate_input_data(bad_data), "Insufficient standards")
})

test_that("coefficient_of_variation calculates correctly", {
  x <- c(8, 10, 12)
  cv <- coefficient_of_variation(x)
  expect_equal(round(cv, 1), 20.0)
})

test_that("coefficient_of_variation edge cases", {
  expect_equal(coefficient_of_variation(c(5, 5, 5)), 0)
  expect_true(is.na(coefficient_of_variation(c(-1, 0, 1))))
})

test_that("get_axis_labels correct for RBA", {
  config <- list(assay_type = "rba", detection_method = "radioligand")
  labels <- get_axis_labels(config)
  expect_true(grepl("mol/L", labels$x_label))
  expect_true(grepl("CPM", labels$y_label))
})

test_that("get_axis_labels correct for ELISA", {
  config <- list(assay_type = "elisa", detection_method = "absorbance", units = "pg/mL")
  labels <- get_axis_labels(config)
  expect_true(grepl("pg/mL", labels$x_label))
  expect_true(grepl("B/B0", labels$y_label))
})

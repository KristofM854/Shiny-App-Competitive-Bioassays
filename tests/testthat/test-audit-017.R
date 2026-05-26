# AUDIT-017: Dead ELISA branch removed from normalize_data().
# ELISA normalization is handled exclusively by calculate_elisa_bb0()
# in report_functions.R. Calling normalize_data() with assay_type = "elisa"
# must now raise an error instead of silently computing stale values.

test_that("AUDIT-017: normalize_data('elisa') raises an error", {
  skip_if_not(exists("normalize_data"),
              "normalize_data() not available (utils_normalization.R not loaded)")

  data_long <- data.frame(
    Well = "A1", SampleType = "Standard",
    MeasurementValue = 0.5, stringsAsFactors = FALSE
  )
  expect_error(normalize_data(data_long, "elisa"),
               regexp = "calculate_elisa_bb0")
})

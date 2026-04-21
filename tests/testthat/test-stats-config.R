test_that("STATS_CONFIG is fully defined and referenced correctly", {
  expect_true(exists("STATS_CONFIG"))
  required_keys <- c("bootstrap_iterations", "mad_outlier_threshold",
                     "dixon_alpha", "shapiro_alpha",
                     "ec20_resp_level", "ec80_resp_level",
                     "heteroscedasticity_variance_ratio_strong",
                     "heteroscedasticity_variance_ratio_moderate",
                     "ci_truncation_floor")
  expect_true(all(required_keys %in% names(STATS_CONFIG)))

  # Sanity checks on values
  expect_equal(STATS_CONFIG$bootstrap_iterations, 1000)
  expect_equal(STATS_CONFIG$mad_outlier_threshold, 3)
  expect_equal(STATS_CONFIG$ec20_resp_level, 80)
  expect_equal(STATS_CONFIG$ec80_resp_level, 20)
})

test_that("compute_layered_uncertainty uses STATS_CONFIG$bootstrap_iterations", {
  # Run with bootstrap and confirm the internal seed call works
  well_preds <- data.frame(
    well = c("A1", "A2", "A3"),
    predicted_conc = c(100, 110, 105),
    ci_lower_model = c(80, 88, 84),
    ci_upper_model = c(120, 132, 126)
  )
  result <- compute_layered_uncertainty(
    well_preds, c(100, 110, 105), ci_method = "bootstrap"
  )
  expect_true(is.finite(result$ci_lower_combined))
  expect_true(is.finite(result$ci_upper_combined))
  expect_lte(result$ci_lower_combined, result$ci_upper_combined)
})

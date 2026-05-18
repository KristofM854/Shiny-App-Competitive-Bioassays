# Test for AUDIT-007: heteroscedasticity degeneracy flag

test_that("AUDIT-007: assess_heteroscedasticity flags degenerate F as degenerate = TRUE", {
  skip_if_not_installed("drc")
  # Standards with zero within-group variance at one level (all replicates identical)
  conc <- c(rep(1e-9, 3), rep(1e-8, 3), rep(1e-7, 3), rep(1e-6, 3))
  resp <- c(15000, 15000, 15000,   # identical — zero variance
            11000, 11200, 10800,
             6000,  5800,  6200,
             2000,  1900,  2100)
  df <- data.frame(
    concentration    = conc,
    MeasurementValue = resp,
    SampleType       = "Standard",
    ReplicateGroup   = paste0("Std", rep(1:4, each = 3))
  )
  fit <- tryCatch(
    drc::drm(MeasurementValue ~ concentration, data = df, fct = drc::LL.4()),
    error = function(e) NULL
  )
  skip_if(is.null(fit), "DRC fit failed on test data — skipping heteroscedasticity test")
  result <- assess_heteroscedasticity(fit, df, "MeasurementValue")
  expect_true(isTRUE(result$degenerate),
              info = "Degenerate F should be flagged when one group has zero within-group variance")
})

test_that("AUDIT-007: assess_heteroscedasticity degenerate = FALSE for normal data", {
  skip_if_not_installed("drc")
  conc <- c(rep(1e-9, 3), rep(1e-8, 3), rep(1e-7, 3), rep(1e-6, 3))
  resp <- c(15000, 14800, 15200,
            11000, 11200, 10800,
             6000,  5800,  6200,
             2000,  1900,  2100)
  df <- data.frame(
    concentration    = conc,
    MeasurementValue = resp,
    SampleType       = "Standard",
    ReplicateGroup   = paste0("Std", rep(1:4, each = 3))
  )
  fit <- tryCatch(
    drc::drm(MeasurementValue ~ concentration, data = df, fct = drc::LL.4()),
    error = function(e) NULL
  )
  skip_if(is.null(fit), "DRC fit failed on test data — skipping heteroscedasticity test")
  result <- assess_heteroscedasticity(fit, df, "MeasurementValue")
  expect_false(isTRUE(result$degenerate),
               info = "Non-degenerate data should have degenerate = FALSE")
})

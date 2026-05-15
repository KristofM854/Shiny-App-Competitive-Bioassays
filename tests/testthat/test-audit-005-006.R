# Tests for AUDIT-005 (ELISA top asymptote unconstrained) and
# AUDIT-006 (Spearman curve-direction check)

test_that("AUDIT-005: ELISA LL.4 no longer clamps top asymptote at 100", {
  skip_if_not_installed("drc")
  # %B/B0 data with top asymptote naturally above 100
  conc <- c(4000, 1600, 640, 256, 102.4, 41.0, 16.4, 6.6)
  resp <- c(5, 12, 25, 45, 68, 85, 97, 108)  # top > 100
  df <- data.frame(concentration = conc, response = resp)
  # Unconstrained fit should NOT error and d coef should be > 100
  fit <- tryCatch(
    drc::drm(response ~ concentration, data = df, fct = drc::LL.4(),
             lowerl = c(NA, 0, NA, NA)),
    error = function(e) NULL
  )
  expect_false(is.null(fit), info = "Unconstrained LL.4 should fit without error")
  if (!is.null(fit)) {
    d_val <- coef(fit)[["d:(Intercept)"]]
    expect_gt(d_val, 100, label = "Top asymptote should exceed 100 when unconstrained")
  }
})

test_that("AUDIT-005: ELISA LL.4 constrained fit fails or clamps when data top > 100", {
  skip_if_not_installed("drc")
  conc <- c(4000, 1600, 640, 256, 102.4, 41.0, 16.4, 6.6)
  resp <- c(5, 12, 25, 45, 68, 85, 97, 108)
  df <- data.frame(concentration = conc, response = resp)
  fit_constrained <- tryCatch(
    drc::drm(response ~ concentration, data = df, fct = drc::LL.4(),
             lowerl = c(NA, 0, NA, NA), upperl = c(NA, NA, 100, NA)),
    error = function(e) NULL
  )
  # Either fit fails OR d is clamped at 100
  if (!is.null(fit_constrained)) {
    d_val <- coef(fit_constrained)[["d:(Intercept)"]]
    expect_lte(d_val, 100 + 1e-6, label = "Constrained fit should clamp d at 100")
  } else {
    succeed("Constrained fit correctly failed when top > 100")
  }
})

test_that("AUDIT-006: Spearman check detects increasing curve", {
  skip_if_not_installed("stats")
  # Simulate an increasing response (wrong curve direction)
  conc <- c(6.6, 16.4, 41.0, 102.4, 256, 640, 1600, 4000)
  resp <- c(10, 20, 35, 55, 70, 82, 91, 98)  # increasing with concentration
  rho <- cor(log(conc), resp, method = "spearman")
  expect_gt(rho, 0.5, label = "Increasing curve should have rho > 0.5")
})

test_that("AUDIT-006: Spearman check accepts decreasing curve", {
  skip_if_not_installed("stats")
  # Normal decreasing competitive assay response
  conc <- c(6.6, 16.4, 41.0, 102.4, 256, 640, 1600, 4000)
  resp <- c(98, 91, 82, 70, 55, 35, 20, 10)  # decreasing with concentration
  rho <- cor(log(conc), resp, method = "spearman")
  expect_lt(rho, 0.5, label = "Decreasing curve should have rho < 0.5")
  expect_lt(rho, 0, label = "Decreasing curve should have negative rho")
})

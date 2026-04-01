test_that("Full RBA pipeline: matrices -> long format -> DRC fit", {
  skip_if_not_installed("drc")

  # 1. Create plate layout
  type_mat <- create_type_matrix("rba", 8)
  id_mat <- create_id_matrix("rba", 8)
  dil_mat <- create_dilution_matrix()
  rep_mat <- create_replicate_matrix("rba")

  # 2. Generate synthetic 4PL data
  set.seed(123)
  concs <- c(1e-6, 1e-7, 3e-8, 1e-8, 3e-9, 1e-9, 1e-10, 3e-11)
  true_bottom <- 100; true_top <- 5000; true_ec50 <- 5e-9; true_hill <- -1.0

  meas_mat <- create_plate_matrix()
  for (s in 1:8) {
    true_resp <- true_bottom + (true_top - true_bottom) / (1 + (concs[s] / true_ec50)^true_hill)
    meas_mat[s, 1:3] <- round(rnorm(3, true_resp, true_resp * 0.05), 0)
  }
  for (r in 1:8) for (c in 4:12) meas_mat[r, c] <- round(rnorm(1, 2500, 500), 0)

  # 3. Convert to long format
  df_long <- matrix_to_long(type_mat, id_mat, dil_mat, rep_mat, meas_mat, concs)
  expect_equal(nrow(df_long), 96)

  # 4. Validate
  expect_true(validate_input_data(df_long))

  # 5. Fit DRC
  standards <- df_long %>%
    dplyr::filter(SampleType == "Standard", !is.na(StandardConc), !is.na(MeasurementValue))

  model <- drc::drm(MeasurementValue ~ StandardConc, data = standards, fct = drc::LL.4())

  coefs <- coef(model)
  resid <- residuals(model)
  r2 <- 1 - sum(resid^2) / sum((standards$MeasurementValue - mean(standards$MeasurementValue))^2)

  expect_gt(r2, 0.95)
  expect_lt(abs(abs(coefs[1]) - 1.0), 0.5)  # Hill slope near 1

  # 6. Predict a sample
  pred <- drc::ED(model, respLev = 2500, type = "absolute", display = FALSE)
  expect_true(is.finite(as.numeric(pred[1, 1])))
})

test_that("Full ELISA pipeline: matrices -> long -> normalize -> validate", {
  # 1. Create ELISA layout
  type_mat <- create_type_matrix("elisa", 8)
  id_mat <- create_id_matrix("elisa", 8)
  dil_mat <- create_dilution_matrix()
  rep_mat <- create_replicate_matrix("elisa")

  # 2. Generate synthetic ELISA data
  set.seed(456)
  meas_mat <- create_plate_matrix()

  # Controls in column 1
  meas_mat[1:2, 1] <- rnorm(2, 0.045, 0.005)  # Blank
  meas_mat[3:4, 1] <- rnorm(2, 0.085, 0.008)  # NSB
  meas_mat[5:7, 1] <- rnorm(3, 1.35, 0.04)    # B0
  meas_mat[8, 1]   <- rnorm(1, 2.10, 0.05)    # TA

  # Standards in columns 2-3 (decreasing absorbance with increasing concentration)
  elisa_concs <- c(4000, 1600, 640, 256, 102.4, 41.0, 16.4, 6.6)
  for (s in 1:8) {
    bb0 <- 100 / (1 + (elisa_concs[s] / 200)^1.1)
    abs_val <- 0.045 + (0.085 - 0.045) + bb0/100 * (1.35 - 0.045 - (0.085 - 0.045))
    meas_mat[s, 2:3] <- rnorm(2, abs_val, abs_val * 0.04)
  }

  # Samples in columns 4-11
  for (r in 1:8) for (c in 4:11) meas_mat[r, c] <- rnorm(1, 0.6, 0.1)

  # 3. Convert to long
  df_long <- matrix_to_long(type_mat, id_mat, dil_mat, rep_mat, meas_mat, elisa_concs)
  expect_equal(nrow(df_long), 96)

  # 4. Validate
  expect_true(validate_input_data(df_long))

  # 5. ELISA normalization
  result <- calculate_elisa_bb0(df_long, use_percent = TRUE)
  expect_true("calculated_bb0" %in% names(result))

  # B0 wells should be ~100%
  b0_vals <- result$calculated_bb0[result$SampleType == "B0"]
  expect_true(all(abs(b0_vals - 100) < 5))  # Within 5% of 100

  # NSB wells should be ~0%
  nsb_vals <- result$calculated_bb0[result$SampleType == "NSB"]
  expect_true(all(abs(nsb_vals) < 5))  # Within 5% of 0
})

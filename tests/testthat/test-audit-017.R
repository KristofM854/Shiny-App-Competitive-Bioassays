# AUDIT-017: Stage 1 flagged two different %B/B0 formulas as a possible
# numerical inconsistency:
#   utils_normalization.R (normalize_data ELISA branch):
#     100 * (M - mean(NSB_raw)) / (mean(B0_raw) - mean(NSB_raw))
#   report_functions.R (calculate_elisa_bb0):
#     blank-corrected form that reduces to the SAME expression (the blank
#     cancels): 100 * (M - mean(NSB)) / (mean(B0) - mean(NSB))
#
# They are algebraically identical, so the live pipeline is NOT numerically
# inconsistent (normalize_data's ELISA branch is in fact dead code — the
# Rmd uses calculate_elisa_bb0()). This is the equivalence test that should
# have caught the Stage 1 mischaracterization; it also guards any future
# consolidation of the two code paths. Both functions are sourced by
# helper-setup.R.

test_that("AUDIT-017: calculate_elisa_bb0 and normalize_data ELISA agree", {
  skip_if_not(exists("calculate_elisa_bb0"),
              "calculate_elisa_bb0() not available (report_functions.R not loaded)")
  skip_if_not(exists("normalize_data"),
              "normalize_data() not available (utils_normalization.R not loaded)")

  # Valid control hierarchy (B0 > NSB > Blank) so both paths run cleanly
  # without validation warnings.
  data_long <- data.frame(
    Well = c("A1", "A2", "B1", "B2", "C1", "C2", "D1", "D2", "E1"),
    SampleType = c("Blank", "Blank", "NSB", "NSB", "B0", "B0",
                   "Standard", "Standard", "Sample"),
    MeasurementValue = c(0.05, 0.06, 0.15, 0.16, 1.20, 1.30,
                         0.80, 0.55, 0.30),
    stringsAsFactors = FALSE
  )

  bb0  <- calculate_elisa_bb0(data_long, use_percent = TRUE)$calculated_bb0
  norm <- normalize_data(data_long, "elisa")$NormalizedValue

  # The two formulas must produce identical %B/B0 for every well (the blank
  # term cancels). Row order is preserved by both (mutate over data_long).
  expect_equal(bb0, norm, tolerance = 1e-9)

  # Sanity: averaged over their replicates, B0 -> 100% and NSB -> 0% under
  # the shared formula.
  is_b0  <- data_long$SampleType == "B0"
  is_nsb <- data_long$SampleType == "NSB"
  expect_equal(mean(norm[is_b0]), 100, tolerance = 1e-6)
  expect_equal(mean(norm[is_nsb]),   0, tolerance = 1e-6)
})

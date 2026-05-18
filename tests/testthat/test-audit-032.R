# Test for AUDIT-032: Auto weighting mutual exclusivity

test_that("AUDIT-032: analysis_pipeline does not contain interpolation weight method when auto selects", {
  skip_if_not_installed("drc")
  # Verify that fit_all_models() treats "auto" as a single weight selection
  # (multi_weight_mode = FALSE) and does not include more than one model
  # when auto is passed as the only selected weight.
  conc <- c(1e-6, 1e-7, 3e-8, 1e-8, 3e-9, 1e-9, 1e-10, 3e-11)
  resp <- c(800, 1500, 3200, 5500, 8200, 11000, 14000, 16000)
  set.seed(42)
  df <- data.frame(
    StandardConc     = rep(conc, each = 3),
    MeasurementValue = rep(resp, each = 3) * rnorm(24, 1, 0.05),
    SampleType       = "Standard",
    ReplicateGroup   = paste0("Std", rep(seq_along(conc), each = 3))
  )
  analysis_config <- list(
    regression_weight        = "auto",
    quant_range_min          = 20,
    quant_range_max          = 80,
    ci_method                = "t_dist",
    enable_outlier_detection = FALSE,
    outlier_min_n            = 3,
    normality_assumption     = "assume",
    cv_limit                 = 30
  )
  result <- fit_all_models(
    data_long       = df,
    response_var    = "MeasurementValue",
    analysis_config = analysis_config,
    is_elisa        = FALSE,
    lang            = "en",
    cv_limit        = 30,
    STATS_CONFIG    = STATS_CONFIG
  )
  expect_false(result$multi_weight_mode,
               info = "Auto mode should set multi_weight_mode = FALSE")
  expect_length(result$all_models, 1,
                info = "Auto mode should produce exactly one fitted model")
  expect_false(is.null(result$auto_weighting_result),
               info = "auto_weighting_result should be populated when auto is selected")
})

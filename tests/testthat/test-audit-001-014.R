# Tests for AUDIT-001 (interpolation fallback removed) and
# AUDIT-014 (global warning suppression lifted)

test_that("AUDIT-001: fit_all_models() stops when all fits fail", {
  skip_if_not_installed("drc")
  # Constant response — zero variance, LL.4 and LL.3 will both fail
  conc <- rep(c(1e-9, 1e-8, 1e-7, 1e-6), each = 3)
  resp <- rep(100, 12)
  df <- data.frame(
    concentration    = conc,
    MeasurementValue = resp,
    SampleType       = "Standard",
    ReplicateGroup   = paste0("Std", rep(1:4, each = 3))
  )
  analysis_config <- list(
    regression_weight        = "none",
    quant_range_min          = 20,
    quant_range_max          = 80,
    ci_method                = "t_dist",
    enable_outlier_detection = FALSE,
    outlier_min_n            = 3,
    normality_assumption     = "assume",
    cv_limit                 = 30
  )
  expect_error(
    fit_all_models(
      data_long       = df,
      response_var    = "MeasurementValue",
      analysis_config = analysis_config,
      is_elisa        = FALSE,
      lang            = "en",
      cv_limit        = 30,
      STATS_CONFIG    = STATS_CONFIG
    ),
    regexp = "Dose-response curve fitting failed"
  )
})

test_that("AUDIT-001: fit_all_models() result never contains interpolation fit_method", {
  skip_if_not_installed("drc")
  # Normal 8-point decreasing RBA data — all models succeed
  conc <- c(1e-6, 1e-7, 3e-8, 1e-8, 3e-9, 1e-9, 1e-10, 3e-11)
  resp <- c(800, 1500, 3200, 5500, 8200, 11000, 14000, 16000)
  df <- data.frame(
    concentration    = rep(conc, each = 3),
    MeasurementValue = rep(resp, each = 3) * rnorm(24, 1, 0.05),
    SampleType       = "Standard",
    ReplicateGroup   = paste0("Std", rep(seq_along(conc), each = 3))
  )
  analysis_config <- list(
    regression_weight        = "none",
    quant_range_min          = 20,
    quant_range_max          = 80,
    ci_method                = "t_dist",
    enable_outlier_detection = FALSE,
    outlier_min_n            = 3,
    normality_assumption     = "assume",
    cv_limit                 = 30
  )
  set.seed(42)
  result <- fit_all_models(
    data_long       = df,
    response_var    = "MeasurementValue",
    analysis_config = analysis_config,
    is_elisa        = FALSE,
    lang            = "en",
    cv_limit        = 30,
    STATS_CONFIG    = STATS_CONFIG
  )
  fit_methods <- vapply(result$all_models, `[[`, character(1), "fit_method")
  expect_false("interpolation" %in% fit_methods,
               info = "No model should use interpolation fallback")
})

test_that("AUDIT-014: unified_analysis_template.Rmd global chunk opts have warning = TRUE", {
  rmd_path <- file.path(.repo_root, "reports", "unified_analysis_template.Rmd")
  skip_if(!file.exists(rmd_path))
  rmd_lines <- readLines(rmd_path)
  setup_line <- grep('knitr::opts_chunk\\$set.*warning', rmd_lines, value = TRUE)[1]
  expect_false(grepl("warning\\s*=\\s*FALSE", setup_line),
               info = paste("Global warning should not be FALSE in setup chunk; found:", setup_line))
  expect_true(grepl("warning\\s*=\\s*TRUE", setup_line),
              info = paste("Global warning should be TRUE in setup chunk; found:", setup_line))
})

test_that("AUDIT-014: multiwavelength template global chunk opts have warning = TRUE", {
  rmd_path <- file.path(.repo_root, "reports", "multiwavelength_analysis_template.Rmd")
  skip_if(!file.exists(rmd_path))
  rmd_lines <- readLines(rmd_path)
  setup_line <- grep('knitr::opts_chunk\\$set.*warning', rmd_lines, value = TRUE)[1]
  expect_false(grepl("warning\\s*=\\s*FALSE", setup_line),
               info = paste("Global warning should not be FALSE; found:", setup_line))
  expect_true(grepl("warning\\s*=\\s*TRUE", setup_line),
              info = paste("Global warning should be TRUE; found:", setup_line))
})

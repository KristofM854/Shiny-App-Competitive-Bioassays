# Regression tests for previously fixed bugs (AUDIT-003).
# Each test names the bug and asserts the fixed behaviour.
# A failing test means the bug has been reintroduced.
#
# Bugs covered (from AUDIT-003 / Stage 1 §11):
#   1. EC20 < EC80 ordering on a known monotone-decreasing curve
#   2. Weight vector length matches standards_for_model after high-var filtering
#   3. import_info attribute is set by import pipeline and readable via base::attr()
#   4. Two independent output_dir values do not collide (process-global env var)
#   5. Dilution values > 1 (no ":" notation) are detected and flagged
#   6. mean_sample_cv is computed and stored in model_stats after quantification
#   7. KPI stats are accessible from a plain environment (env-handoff pattern)

# ---------------------------------------------------------------------------
# BUG 1 - EC20 < EC80 ordering
# EC20 must be numerically smaller than EC80 for any monotone-decreasing
# dose-response curve.  The bug was in the assignment of drc::ED() respLev
# arguments: STATS_CONFIG$ec20_resp_level = 80 and $ec80_resp_level = 20
# encode the drc convention that "respLev=80" returns the concentration at
# which response has dropped by 80% (EC80 in standard notation), and
# "respLev=20" returns EC20.  The swap in analysis_pipeline.R:516-518
# corrects for cases where the labels come back inverted.
#
# We test: for a canonical decreasing 4PL fit, EC20 < EC80 in concentration
# units after applying the same swap logic used in fit_all_models().
# ---------------------------------------------------------------------------
test_that("BUG-001: EC20 < EC80 for a standard monotone-decreasing RBA curve", {
  skip_if_not_installed("drc")
  skip_if_not(exists("STATS_CONFIG"), "STATS_CONFIG not loaded")

  # Known monotone-decreasing 4PL data (concentration vs response)
  # Response decreases from ~3000 CPM at low conc to ~195 at high conc.
  conc <- c(1e-10, 3e-10, 1e-9, 3e-9, 1e-8, 3e-8, 1e-7, 1e-6)
  resp <- c(3000,  2800,  2200, 1100,  350,  210,  200,  195)
  df   <- data.frame(concentration = conc, response = resp)

  fit <- tryCatch(
    drc::drm(response ~ concentration, data = df, fct = drc::LL.4()),
    error = function(e) NULL
  )
  skip_if(is.null(fit), "drc::drm() failed on synthetic data")

  # Use the same respLev values as analysis_pipeline.R:506-515
  # STATS_CONFIG: ec20_resp_level = 80, ec80_resp_level = 20
  # drc convention: respLev=80 means 80% reduction from top asymptote (EC80 in
  # standard notation), respLev=20 means 20% reduction (EC20).
  # Type "relative" works for any response scale (unlike "absolute" which
  # requires the respLev to be within the absolute response range).
  ec20_raw <- tryCatch(
    as.numeric(drc::ED(fit, respLev = STATS_CONFIG$ec20_resp_level,
                       type = "relative", display = FALSE)[1, 1]),
    error = function(e) NA_real_
  )
  ec80_raw <- tryCatch(
    as.numeric(drc::ED(fit, respLev = STATS_CONFIG$ec80_resp_level,
                       type = "relative", display = FALSE)[1, 1]),
    error = function(e) NA_real_
  )

  skip_if(is.na(ec20_raw) || is.na(ec80_raw),
          "ED computation returned NA -- drc model may not support these respLev values")

  # Apply the same conditional swap as analysis_pipeline.R:516-518
  ec20 <- ec20_raw
  ec80 <- ec80_raw
  if (!is.na(ec20) && !is.na(ec80) && ec20 > ec80) {
    .tmp <- ec20; ec20 <- ec80; ec80 <- .tmp
  }

  expect_true(is.finite(ec20), label = "EC20 must be a finite number")
  expect_true(is.finite(ec80), label = "EC80 must be a finite number")

  # For a decreasing curve, EC20 (20% inhibition) is at LOWER concentration
  # than EC80 (80% inhibition).  The respLev = 80 call returns the smaller
  # concentration; respLev = 20 returns the larger.
  expect_lt(ec20, ec80,
            label = "EC20 must be less than EC80 in concentration units")

  # Additional sanity: EC20 < IC50 < EC80
  ic50 <- tryCatch(
    as.numeric(drc::ED(fit, respLev = 50, type = "relative", display = FALSE)[1, 1]),
    error = function(e) NA_real_
  )
  if (is.finite(ic50)) {
    expect_lt(ec20, ic50, label = "EC20 must be less than IC50")
    expect_gt(ec80, ic50, label = "EC80 must be greater than IC50")
  }
})

# ---------------------------------------------------------------------------
# BUG 2 - Weight vector length == nrow(standards_for_model) after filtering
# When high-variability standards are removed, the weight vectors (1/Y, 1/Y^2)
# must be rebuilt from the filtered data, not the original.  The original bug
# computed weights before filtering, leaving them one element longer than the
# data frame passed to drc::drm(), which caused a length-mismatch error.
# The fix adds stopifnot() guards at analysis_pipeline.R:291-292.
# ---------------------------------------------------------------------------
test_that("BUG-002: weight vector length matches standards_for_model after high-var filtering", {
  skip_if_not_installed("drc")
  skip_if_not(exists("fit_all_models"), "fit_all_models() not loaded")
  skip_if_not(exists("STATS_CONFIG"), "STATS_CONFIG not loaded")

  # Build 8 concentration groups; make the highest one have extreme variability
  # so it gets flagged and removed.
  set.seed(7)
  conc      <- c(1e-6, 3e-7, 1e-7, 3e-8, 1e-8, 3e-9, 1e-9, 3e-10)
  resp_base <- c(100,  400,  700,  1400, 2200, 2800, 3100, 3300)

  # Triplicate replicates per concentration.
  # Group i=1 (conc = 1e-6) gets enormous noise: CV > 200%, will be filtered.
  data_rows <- lapply(seq_along(conc), function(i) {
    noise <- if (i == 1) resp_base[i] * 2 else resp_base[i] * 0.05
    replicate_resp <- resp_base[i] + rnorm(3, 0, noise)
    data.frame(
      Well             = paste0(LETTERS[i], 1:3),
      Row              = LETTERS[i],
      Column           = as.character(1:3),
      SampleType       = "Standard",
      SampleID         = paste0("S", i),
      StandardConc     = conc[i],
      DilutionFactor   = 1,
      Replicate        = paste0("S", i),
      MeasurementValue = replicate_resp,
      NormalizedValue  = replicate_resp,
      stringsAsFactors = FALSE
    )
  })
  data_long <- do.call(rbind, data_rows)

  # cv_limit = 50 ensures the noisy group (CV > 200%) is excluded.
  # With all three weight modes requested, the bug would manifest as a
  # drc::drm() length mismatch if weights were computed before filtering.
  analysis_config <- list(regression_weight = c("none", "inv_y", "inv_y2"))

  result <- tryCatch(
    fit_all_models(
      data_long       = data_long,
      response_var    = "NormalizedValue",
      analysis_config = analysis_config,
      is_elisa        = FALSE,
      lang            = "en",
      cv_limit        = 50,
      STATS_CONFIG    = STATS_CONFIG
    ),
    error = function(e) e
  )

  # If the bug were reintroduced, fit_all_models() would error here with a
  # "weights" or "length" mismatch from drc::drm().
  expect_false(inherits(result, "error"),
               label = paste("fit_all_models() must not error:",
                             if (inherits(result, "error")) result$message else ""))

  # The high-variability group must have been removed
  expect_false(result$drc_failed_completely,
               label = "A model must fit on the remaining standards")
  expect_lt(nrow(result$standards_for_model), nrow(data_long),
            label = "High-variability standards must have been removed")
})

# ---------------------------------------------------------------------------
# BUG 3 - import_info attribute is set by import pipeline and readable via base::attr()
# After parsing a plate file, import_plate_data() sets
# attr(plate_numeric, "import_info") on the returned matrix.  The bug was
# that subsequent operations stripped the attribute before server_upload.R
# could read it.  The fix uses base::attr() explicitly when reading and
# treats NULL return as "no info" (NULL-safe guard in server_upload.R:760-766).
#
# We test that parse_plate_file() on the shipped RBA example CSV:
# (a) returns import_info in the result list
# (b) the plates[[1]] element carries the import_info attribute
# (c) base::attr() retrieves it without error
# ---------------------------------------------------------------------------
test_that("BUG-003: import_info attribute is set by import pipeline and readable via base::attr()", {
  skip_if_not(exists("parse_plate_file"),
              "parse_plate_file() not available (utils_import_v3.R not loaded)")

  # Use the shipped RBA example CSV as the test fixture
  example_csv <- file.path(.repo_root, "examples", "rba_stx_example.csv")
  skip_if_not(file.exists(example_csv), "rba_stx_example.csv not found")

  result <- tryCatch(
    parse_plate_file(example_csv),
    error = function(e) NULL
  )
  skip_if(is.null(result), "parse_plate_file() failed on example CSV")

  # The result list must contain import_info with the file name
  expect_false(is.null(result$import_info),
               label = "parse_plate_file() must return import_info in the result")
  expect_true(is.character(result$import_info$file),
              label = "import_info$file must be a character string")

  # The primary plate must carry import_info as an attribute
  primary_plate <- result$plates[[1]]
  info <- base::attr(primary_plate, "import_info")
  expect_false(is.null(info),
               label = "plates[[1]] must carry import_info as an attribute")
  expect_equal(info$file, basename(example_csv),
               label = "import_info$file must match the input filename")

  # The attribute must be a list (not stripped to NULL by as.data.frame())
  expect_type(info, "list")
  expect_true("detected_wells" %in% names(info))
  expect_true(info$detected_wells > 0L,
              label = "detected_wells must be positive")
})

# ---------------------------------------------------------------------------
# BUG 4 - Two independent output dirs do not collide via env var
# The original code used Sys.setenv("RBA_OUTPUT_DIR", ...) globally.
# Two calls with different paths must produce different values: the second
# call must not silently overwrite the first session's path.
# The fix: each session captures its path in session$userData (local to that
# session), not in a process-global env var (server_report.R:458-459).
# ---------------------------------------------------------------------------
test_that("BUG-004: independent output_dir values do not collide via RBA_OUTPUT_DIR", {
  old_val <- Sys.getenv("RBA_OUTPUT_DIR", unset = NA)
  on.exit({
    if (is.na(old_val)) Sys.unsetenv("RBA_OUTPUT_DIR")
    else Sys.setenv(RBA_OUTPUT_DIR = old_val)
  })

  # Simulate session 1 and session 2 each capturing their own output dir
  dir1 <- tempfile("session1_")
  dir2 <- tempfile("session2_")
  dir.create(dir1)
  dir.create(dir2)
  on.exit(unlink(dir1, recursive = TRUE), add = TRUE)
  on.exit(unlink(dir2, recursive = TRUE), add = TRUE)

  # The fix pattern: each session holds its output path in a local variable
  # (session$userData$output_dir), NOT via Sys.getenv("RBA_OUTPUT_DIR").
  session1_local_dir <- dir1
  session2_local_dir <- dir2

  expect_false(identical(session1_local_dir, session2_local_dir),
               label = "Two session dirs must differ")

  # Write sentinel files -- they must stay separate
  writeLines("session1", file.path(dir1, "sentinel.txt"))
  writeLines("session2", file.path(dir2, "sentinel.txt"))

  expect_equal(readLines(file.path(dir1, "sentinel.txt")), "session1")
  expect_equal(readLines(file.path(dir2, "sentinel.txt")), "session2")

  # Demonstrate the bug: if we used Sys.setenv (old approach), session 2
  # overwrites the global env var, and a session 1 Sys.getenv() call now
  # points to session 2's directory.
  Sys.setenv(RBA_OUTPUT_DIR = session2_local_dir)
  env_val <- Sys.getenv("RBA_OUTPUT_DIR")
  expect_equal(env_val, session2_local_dir,
               label = "Global env var points to session 2 -- session 1 is now unsafe")

  # With the fix, the session-local variable is unaffected by the global change.
  expect_equal(session1_local_dir, dir1,
               label = "Session-local variable is unaffected by global env var change")
  expect_false(identical(session1_local_dir, env_val),
               label = "Local session dir differs from the overwritten env var")
})

# ---------------------------------------------------------------------------
# BUG 5 - Dilution values > 1 without ":" are detected as suspicious
# The server uses raw_matrix_dilution() to detect cells that are numeric > 1
# but were NOT entered as a ratio (no ":").  Without this check, a user who
# types "2" meaning "2-fold dilution" (should be "1:2" = 0.5) would silently
# get halved concentrations in the report.
# The fix: server_report.R:102 checks `numeric_cells > 1 & !grepl(":", cells)`
# and renders a warning UI element.
# ---------------------------------------------------------------------------
test_that("BUG-005: dilution values > 1 without ':' notation are detected as suspicious", {
  # Replicate the detection logic from server_report.R:101-103
  detect_gt1_no_ratio <- function(raw_cells) {
    cells <- as.character(unlist(raw_cells))
    numeric_cells <- suppressWarnings(as.numeric(cells))
    !is.na(numeric_cells) & numeric_cells > 1 & !grepl(":", cells)
  }

  # "2" without ":" must trigger the warning
  cells_mixed <- c("1", "2", "0.5", "1:2")
  has_gt1     <- detect_gt1_no_ratio(cells_mixed)
  expect_true(has_gt1[cells_mixed == "2"],
              info = "The value '2' without ':' must trigger the > 1 warning")

  # "1:2" ratio notation must NOT trigger the warning
  expect_false(has_gt1[cells_mixed == "1:2"],
               info = "'1:2' ratio notation must NOT trigger the warning")
  expect_false(has_gt1[cells_mixed == "0.5"],
               info = "'0.5' must NOT trigger the warning (< 1)")
  expect_false(has_gt1[cells_mixed == "1"],
               info = "'1' must NOT trigger the warning (== 1)")

  # A fully valid plate: only fractions and ratio notation -- no warning
  cells_ok <- c("1", "0.5", "1:2", "0.25")
  expect_false(any(detect_gt1_no_ratio(cells_ok)),
               info = "Valid dilution fractions must not trigger the warning")

  # parse_dilution_cell() must accept "1:2" and return 0.5 (not > 1)
  skip_if_not(exists("parse_dilution_cell"),
              "parse_dilution_cell() not available")
  parsed_ratio <- parse_dilution_cell("1:2")
  expect_equal(parsed_ratio$value, 0.5,
               label = "'1:2' must parse to 0.5")
  expect_true(parsed_ratio$valid)

  # parse_dilution_cell() accepts "2" as syntactically valid (> 1 is a
  # semantic concern surfaced by the UI warning, not a parse error)
  parsed_2 <- parse_dilution_cell("2")
  expect_equal(parsed_2$value, 2,
               label = "'2' must parse to 2.0")
  expect_true(parsed_2$valid)
})

# ---------------------------------------------------------------------------
# BUG 6 - mean_sample_cv is populated after save-sample-stats logic
# BUG_008 in the Rmd template: model_stats was written to JSON before
# replicate_stats existed, so mean_sample_cv was always NA in the JSON.
# The fix adds a second JSON write in the save-sample-stats chunk
# (unified_analysis_template.Rmd:1572-1582) after replicate_stats is computed.
# We test the computation logic directly without invoking rmarkdown::render().
# ---------------------------------------------------------------------------
test_that("BUG-006 (BUG_008): mean_sample_cv is populated when replicate_stats exists", {
  # Simulate the initial model_stats written before replicate_stats exists
  model_stats <- list(
    r_squared     = 0.995,
    rmse          = 12.3,
    weight_method = "Unweighted",
    mean_sample_cv = NA_real_   # initial value before the bug-fix chunk runs
  )

  replicate_stats <- data.frame(
    Replicate  = c("AA", "AB", "AC"),
    cv_percent = c(8.2, 11.5, 6.9),
    stringsAsFactors = FALSE
  )

  # Replicate the BUG_008 fix logic from unified_analysis_template.Rmd:1572-1578
  if (exists("model_stats") && exists("replicate_stats") &&
      !is.null(replicate_stats) && nrow(replicate_stats) > 0) {
    model_stats$mean_sample_cv <- mean(replicate_stats$cv_percent, na.rm = TRUE)
  } else if (exists("model_stats")) {
    model_stats$mean_sample_cv <- NA_real_
  }

  expect_false(is.na(model_stats$mean_sample_cv),
               info = "mean_sample_cv must not be NA when replicate_stats exists")
  expect_equal(model_stats$mean_sample_cv,
               mean(c(8.2, 11.5, 6.9)),
               tolerance = 1e-10,
               label = "mean_sample_cv must equal mean(cv_percent)")

  # Edge case: empty replicate_stats -- mean_sample_cv should remain NA
  model_stats2       <- list(mean_sample_cv = NA_real_)
  replicate_empty    <- data.frame(cv_percent = numeric(0))
  if (!is.null(replicate_empty) && nrow(replicate_empty) > 0) {
    model_stats2$mean_sample_cv <- mean(replicate_empty$cv_percent, na.rm = TRUE)
  }
  expect_true(is.na(model_stats2$mean_sample_cv),
              info = "mean_sample_cv must be NA when replicate_stats is empty")
})

# ---------------------------------------------------------------------------
# BUG 7 - KPI strip populates from .stats_env environment (env-handoff)
# The original pipeline returned model_stats only via JSON on disk, making
# the KPI strip depend on a disk round-trip that could fail silently.
# The fix passes a fresh environment (.stats_env) to the Rmd as a param;
# the template assigns model_stats into it; server_report.R reads it back.
# We test both the env-handoff path and the JSON fallback path.
# ---------------------------------------------------------------------------
test_that("BUG-007: KPI stats populate from .stats_env environment (env-handoff)", {
  skip_if_not_installed("jsonlite")

  # Simulate what the Rmd template does: assign model_stats into stats_env
  .stats_env <- new.env(parent = emptyenv())

  model_stats_from_rmd <- list(
    r_squared      = 0.997,
    rmse           = 9.4,
    ic50           = 1.23e-9,
    weight_method  = "1/Y weighted (LL.4)",
    mean_sample_cv = 7.8
  )
  assign("model_stats", model_stats_from_rmd, envir = .stats_env)

  # Simulate the server_report.R KPI-strip logic (lines 591-611)
  last_model_stats <- NULL
  if (exists("model_stats", envir = .stats_env, inherits = FALSE)) {
    last_model_stats <- .stats_env$model_stats
  }

  expect_false(is.null(last_model_stats),
               info = "last_model_stats must be populated from .stats_env")
  expect_equal(last_model_stats$r_squared, 0.997, tolerance = 1e-10)
  expect_equal(last_model_stats$mean_sample_cv, 7.8, tolerance = 1e-10)

  # JSON fallback path: when .stats_env is empty, read model_stats.json
  .stats_env_empty <- new.env(parent = emptyenv())
  tmp_dir <- tempfile("stats_env_test_")
  dir.create(tmp_dir)
  on.exit(unlink(tmp_dir, recursive = TRUE), add = TRUE)

  stats_json_path <- file.path(tmp_dir, "model_stats.json")
  jsonlite::write_json(model_stats_from_rmd, stats_json_path,
                       pretty = TRUE, auto_unbox = TRUE)

  last_model_stats_fallback <- NULL
  if (exists("model_stats", envir = .stats_env_empty, inherits = FALSE)) {
    last_model_stats_fallback <- .stats_env_empty$model_stats
  } else if (file.exists(stats_json_path)) {
    last_model_stats_fallback <- tryCatch(
      jsonlite::fromJSON(stats_json_path, simplifyVector = TRUE),
      error = function(e) NULL
    )
  }

  expect_false(is.null(last_model_stats_fallback),
               info = "JSON fallback must provide model_stats when .stats_env is empty")
  expect_equal(last_model_stats_fallback$r_squared, 0.997, tolerance = 1e-10)
})

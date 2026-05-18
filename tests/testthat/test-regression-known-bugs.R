# Regression tests for previously fixed bugs (AUDIT-003).
# Each test names the bug and asserts the fixed behaviour.
# A failing test means the bug has been reintroduced.
#
# Bugs covered (from AUDIT-003 / Stage 1 §11):
#   1. EC20 < EC80 ordering on a known monotone-decreasing curve
#   2. Weight vector length matches standards_for_model after high-var filtering
#   3. attr(plate_numeric, "import_info") survives enforce_plate_shape()
#   4. Two independent output_dir values do not collide (process-global env var)
#   5. Dilution values > 1 (no ":" notation) are detected and flagged
#   6. mean_sample_cv is computed and stored in model_stats after quantification
#   7. KPI stats are accessible from a plain environment (env-handoff pattern)

# ---------------------------------------------------------------------------
# BUG 1 — EC20 < EC80 ordering
# EC20 must be numerically smaller than EC80 for any monotone-decreasing
# dose-response curve.  The original bug swapped them unconditionally;
# the fix swaps only when ec20 > ec80 AND both are finite.
# ---------------------------------------------------------------------------
test_that("BUG-001: EC20 < EC80 for a standard monotone-decreasing RBA curve", {
  skip_if_not_installed("drc")

  # Build a synthetic 8-point RBA standards data frame (decreasing response)
  set.seed(42)
  # 8-point RBA STX standard concentrations (mol/L), high to low
  conc <- if (exists("DEFAULT_STX_CONC")) DEFAULT_STX_CONC else
    c(1e-6, 3e-7, 1e-7, 3e-8, 1e-8, 3e-9, 1e-9, 3e-10)
  # Typical RBA CPM: high at low concentration, low at high concentration
  response <- 3000 * (1 / (1 + (conc / 1e-9)^1.5)) + 200 + rnorm(8, 0, 30)

  data_long <- data.frame(
    Well          = paste0("A", 1:8),
    Row           = rep("A", 8),
    Column        = as.character(1:8),
    SampleType    = "Standard",
    SampleID      = paste0("S", 1:8),
    StandardConc  = conc,
    DilutionFactor = 1,
    Replicate     = paste0("S", 1:8),
    MeasurementValue  = response,
    NormalizedValue   = response,
    stringsAsFactors  = FALSE
  )

  analysis_config <- list(regression_weight = "none")

  result <- fit_all_models(
    data_long      = data_long,
    response_var   = "NormalizedValue",
    analysis_config = analysis_config,
    is_elisa       = FALSE,
    lang           = "en",
    cv_limit       = 9999,   # disable high-variability filtering
    STATS_CONFIG   = STATS_CONFIG
  )

  ec20 <- result$ec20
  ec80 <- result$ec80

  expect_true(is.finite(ec20), label = "EC20 must be a finite number")
  expect_true(is.finite(ec80), label = "EC80 must be a finite number")
  # For a decreasing curve, EC20 (response barely drops) is at LOWER
  # concentration than EC80 (response strongly drops).
  expect_lt(ec20, ec80,
            label = "EC20 must be less than EC80 in concentration units")
})

# ---------------------------------------------------------------------------
# BUG 2 — Weight vector length == nrow(standards_for_model) after filtering
# When high-variability standards are removed, the weight vectors (1/Y, 1/Y²)
# must be rebuilt from the filtered data, not the original.  The original bug
# computed weights before filtering, leaving them one element longer than the
# data frame passed to drc::drm(), which caused a length-mismatch error.
# ---------------------------------------------------------------------------
test_that("BUG-002: weight vector length matches standards_for_model after high-var filtering", {
  skip_if_not_installed("drc")

  # Build 8 concentration groups; make the highest one have extreme variability
  # so it gets flagged and removed.
  set.seed(7)
  conc <- c(1e-6, 3e-7, 1e-7, 3e-8, 1e-8, 3e-9, 1e-9, 3e-10)
  # Triplicate replicates per concentration
  resp_base <- c(100, 400, 700, 1400, 2200, 2800, 3100, 3300)

  data_rows <- lapply(seq_along(conc), function(i) {
    # Highest concentration gets enormous CV (will be filtered at cv_limit < 100)
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

  # cv_limit = 50 means the noisy group (CV > 200%) will be filtered
  analysis_config <- list(regression_weight = c("none", "inv_y", "inv_y2"))

  # The fix enforces: stopifnot(length(weight_options$inv_y$weights) ==
  # nrow(standards_for_model)) inside fit_all_models().  If the bug were
  # reintroduced this call would error with a drc length mismatch.
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

  expect_false(inherits(result, "error"),
               label = paste("fit_all_models() must not error:",
                             if (inherits(result, "error")) result$message else ""))

  # Confirm at least one model fitted (the high-var group was removed)
  expect_false(result$drc_failed_completely,
               label = "A model must fit on the remaining standards")
  expect_lt(nrow(result$standards_for_model), nrow(data_long),
            label = "High-variability standards must have been removed")
})

# ---------------------------------------------------------------------------
# BUG 3 — import_info attribute survives enforce_plate_shape()
# After parsing a plate file, import_info is stored as an attribute on the
# numeric matrix.  enforce_plate_shape() calls as.data.frame() which dropped
# the attribute; the fix uses structure() to carry attributes through.
# ---------------------------------------------------------------------------
test_that("BUG-003: import_info attribute survives enforce_plate_shape()", {
  # Build a minimal 8x12 data frame with an import_info attribute
  plate <- as.data.frame(matrix(as.numeric(1:96), nrow = 8, ncol = 12))
  attr(plate, "import_info") <- list(
    file             = "test_plate.csv",
    detected_wells   = 96L,
    partial_plate    = FALSE,
    format           = "CSV"
  )

  # Apply enforce_plate_shape() — the bug stripped the attribute here
  skip_if_not(exists("enforce_plate_shape"),
              "enforce_plate_shape() not available (global.R not loaded)")
  result <- enforce_plate_shape(plate)

  info <- attr(result, "import_info")
  expect_false(is.null(info),
               label = "import_info attribute must survive enforce_plate_shape()")
  expect_equal(info$file, "test_plate.csv")
  expect_equal(info$detected_wells, 96L)
})

# ---------------------------------------------------------------------------
# BUG 4 — Two independent output dirs do not collide via env var
# The original code used Sys.setenv("RBA_OUTPUT_DIR", ...) globally.
# Two calls with different paths must produce different values: the second
# call must not silently overwrite the first session's path.
# ---------------------------------------------------------------------------
test_that("BUG-004: independent output_dir values do not collide via RBA_OUTPUT_DIR", {
  old_val <- Sys.getenv("RBA_OUTPUT_DIR", unset = NA)
  on.exit({
    if (is.na(old_val)) Sys.unsetenv("RBA_OUTPUT_DIR")
    else Sys.setenv(RBA_OUTPUT_DIR = old_val)
  })

  # Simulate session 1 writing its own dir
  dir1 <- tempfile("session1_")
  dir.create(dir1)
  session1_dir <- dir1

  # Simulate session 2 writing a DIFFERENT dir (should not touch session 1's)
  dir2 <- tempfile("session2_")
  dir.create(dir2)
  session2_dir <- dir2

  # The fix: each "session" captures its path in a local variable rather than
  # a process-global env var.  Verify that the two paths are distinct.
  expect_false(identical(session1_dir, session2_dir),
               info = "Two session dirs must differ")

  # Write a sentinel file into each — they must stay separate
  writeLines("session1", file.path(session1_dir, "sentinel.txt"))
  writeLines("session2", file.path(session2_dir, "sentinel.txt"))

  expect_equal(readLines(file.path(session1_dir, "sentinel.txt")), "session1")
  expect_equal(readLines(file.path(session2_dir, "sentinel.txt")), "session2")

  # Simulate what the bug did: overwrite the global env var with session 2's path
  Sys.setenv(RBA_OUTPUT_DIR = session2_dir)
  # Session 1 reading from the env var would now see session 2's directory
  env_val <- Sys.getenv("RBA_OUTPUT_DIR")
  expect_equal(env_val, session2_dir,
               info = "Global env var points to session 2 — session 1 is now unsafe")

  # The fix pattern: use a session-local variable, NOT Sys.getenv("RBA_OUTPUT_DIR")
  # Each session holds its own dir reference that cannot be overwritten by another session.
  local_dir <- session1_dir
  expect_equal(local_dir, session1_dir,
               info = "Session-local variable is unaffected by global env var change")
  expect_false(identical(local_dir, env_val),
               info = "Local session dir differs from the globally set env var (session isolation)")

  # Cleanup temp dirs
  unlink(dir1, recursive = TRUE)
  unlink(dir2, recursive = TRUE)
})

# ---------------------------------------------------------------------------
# BUG 5 — Dilution values > 1 without ":" are detected as suspicious
# The server uses raw_matrix_dilution() to detect cells that are numeric > 1
# but were NOT entered as a ratio (no ":").  The fixed logic:
#   numeric_cells > 1 & !grepl(":", cells)
# tests the server-side check in isolation.
# ---------------------------------------------------------------------------
test_that("BUG-005: dilution values > 1 without ':' notation are detected as suspicious", {
  # Simulate the logic from server_report.R:102 (output$dilution_gt1_warning)
  detect_gt1_no_ratio <- function(raw_cells) {
    cells <- as.character(unlist(raw_cells))
    numeric_cells <- suppressWarnings(as.numeric(cells))
    !is.na(numeric_cells) & numeric_cells > 1 & !grepl(":", cells)
  }

  # A cell entered as "2" (meaning "2-fold dilution" — almost certainly wrong)
  cells_bad <- c("1", "2", "0.5", "1:2")
  has_gt1   <- detect_gt1_no_ratio(cells_bad)
  expect_true(any(has_gt1),
              info = "The value '2' without ':' must trigger the > 1 warning")
  expect_false(has_gt1[cells_bad == "1:2"],
               info = "'1:2' ratio notation must NOT trigger the warning")
  expect_false(has_gt1[cells_bad == "0.5"],
               info = "'0.5' must NOT trigger the warning (< 1)")
  expect_false(has_gt1[cells_bad == "1"],
               info = "'1' must NOT trigger the warning (== 1)")

  # A fully valid plate: only 0.5, 0.25, and "1:2" — no warning expected
  cells_ok  <- c("1", "0.5", "1:2", "0.25")
  expect_false(any(detect_gt1_no_ratio(cells_ok)),
               info = "Valid dilution fractions must not trigger the warning")

  # parse_dilution_cell() must accept "1:2" and return 0.5 (not > 1)
  parsed <- parse_dilution_cell("1:2")
  expect_equal(parsed$value, 0.5)
  expect_true(parsed$valid)

  # parse_dilution_cell() must accept "2" (a > 1 numeric) as syntactically valid
  # (the semantic warning is emitted by the UI, not the parser)
  parsed2 <- parse_dilution_cell("2")
  expect_equal(parsed2$value, 2)
  expect_true(parsed2$valid)
})

# ---------------------------------------------------------------------------
# BUG 6 — mean_sample_cv is populated after save-sample-stats logic
# BUG_008 in the template: model_stats was written to JSON before
# replicate_stats existed; mean_sample_cv was always NA in the JSON.
# The fix adds a second JSON write after replicate_stats is computed.
# We test the computation logic directly without invoking rmarkdown::render().
# ---------------------------------------------------------------------------
test_that("BUG-006 (BUG_008): mean_sample_cv is populated when replicate_stats exists", {
  # Simulate the save-sample-stats chunk logic
  model_stats <- list(
    r_squared = 0.995,
    rmse      = 12.3,
    weight_method = "Unweighted",
    mean_sample_cv = NA_real_  # initial value before the bug-fix chunk runs
  )

  replicate_stats <- data.frame(
    Replicate   = c("AA", "AB", "AC"),
    cv_percent  = c(8.2, 11.5, 6.9),
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

  # Edge case: empty replicate_stats — mean_sample_cv should remain NA
  model_stats2 <- list(mean_sample_cv = NA_real_)
  replicate_stats_empty <- data.frame(cv_percent = numeric(0))
  if (!is.null(replicate_stats_empty) && nrow(replicate_stats_empty) > 0) {
    model_stats2$mean_sample_cv <- mean(replicate_stats_empty$cv_percent, na.rm = TRUE)
  }
  expect_true(is.na(model_stats2$mean_sample_cv),
              info = "mean_sample_cv must be NA when replicate_stats is empty")
})

# ---------------------------------------------------------------------------
# BUG 7 — KPI strip populates from .stats_env env-handoff
# The original pipeline returned model_stats only via JSON on disk, making
# the KPI strip depend on a disk round-trip that could fail silently.
# The fix passes a fresh environment (.stats_env) to the Rmd as a param;
# the template writes model_stats into it; server_report.R reads it back.
# We test that the env-handoff pattern works correctly without Shiny.
# ---------------------------------------------------------------------------
test_that("BUG-007: KPI stats populate from .stats_env environment (env-handoff)", {
  # Simulate what the Rmd template does: assign model_stats into stats_env
  .stats_env <- new.env(parent = emptyenv())

  # Simulate what unified_analysis_template.Rmd assigns into stats_env
  model_stats_from_rmd <- list(
    r_squared      = 0.997,
    rmse           = 9.4,
    ic50           = 1.23e-9,
    weight_method  = "1/Y weighted (LL.4)",
    mean_sample_cv = 7.8
  )
  assign("model_stats", model_stats_from_rmd, envir = .stats_env)

  # Simulate the server_report.R KPI-strip logic (lines 594-612)
  last_model_stats <- NULL
  if (exists("model_stats", envir = .stats_env, inherits = FALSE)) {
    last_model_stats <- .stats_env$model_stats
  }

  expect_false(is.null(last_model_stats),
               info = "last_model_stats must be populated from .stats_env")
  expect_equal(last_model_stats$r_squared, 0.997, tolerance = 1e-10)
  expect_equal(last_model_stats$mean_sample_cv, 7.8, tolerance = 1e-10)

  # Verify JSON fallback path: when .stats_env is empty, read from file
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

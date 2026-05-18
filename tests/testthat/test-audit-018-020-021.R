test_that("AUDIT-020: STATS_CONFIG has report_seed", {
  env <- new.env(parent = globalenv())
  source(file.path(.repo_root, "reports", "report_constants.R"), local = env)
  expect_true("report_seed" %in% names(env$STATS_CONFIG))
  expect_type(env$STATS_CONFIG$report_seed, "integer")
})

test_that("AUDIT-020: set.seed(42) removed from report_functions.R", {
  lines <- readLines(file.path(.repo_root, "reports", "report_functions.R"))
  hardcoded <- grep("set\\.seed\\(42\\)", lines)
  expect_length(hardcoded, 0L)
})

test_that("AUDIT-020: set.seed(NULL) removed from report_functions.R", {
  lines <- readLines(file.path(.repo_root, "reports", "report_functions.R"))
  hardcoded <- grep("set\\.seed\\(NULL\\)", lines)
  expect_length(hardcoded, 0L)
})

test_that("AUDIT-020: setup chunk calls set.seed(STATS_CONFIG$report_seed)", {
  lines <- readLines(file.path(.repo_root, "reports", "unified_analysis_template.Rmd"))
  expect_true(any(grepl("set\\.seed\\(STATS_CONFIG\\$report_seed\\)", lines)))
})

test_that("AUDIT-018: report-metadata-sidecar chunk exists in template", {
  lines <- readLines(file.path(.repo_root, "reports", "unified_analysis_template.Rmd"))
  expect_true(any(grepl("report-metadata-sidecar", lines)))
})

test_that("AUDIT-018: metadata chunk writes report_metadata.json", {
  lines <- readLines(file.path(.repo_root, "reports", "unified_analysis_template.Rmd"))
  expect_true(any(grepl("report_metadata\\.json", lines)))
})

test_that("AUDIT-018: metadata chunk uses digest::digest for input hash", {
  lines <- readLines(file.path(.repo_root, "reports", "unified_analysis_template.Rmd"))
  expect_true(any(grepl("digest::digest", lines)))
})

test_that("AUDIT-018: metadata chunk captures sessionInfo", {
  lines <- readLines(file.path(.repo_root, "reports", "unified_analysis_template.Rmd"))
  expect_true(any(grepl("sessionInfo", lines)))
})

test_that("AUDIT-021: replay_report.R exists and is executable R code", {
  replay_path <- file.path(.repo_root, "scripts", "replay_report.R")
  expect_true(file.exists(replay_path))
  lines <- readLines(replay_path)
  expect_true(any(grepl("rmarkdown::render", lines)))
})

test_that("AUDIT-021: replay_report.R accepts output_dir as argument or env var", {
  lines <- readLines(file.path(.repo_root, "scripts", "replay_report.R"))
  expect_true(any(grepl("commandArgs", lines)))
  expect_true(any(grepl("RBA_OUTPUT_DIR", lines)))
})

test_that("AUDIT-021: replay_report.R verifies required files exist", {
  lines <- readLines(file.path(.repo_root, "scripts", "replay_report.R"))
  expect_true(any(grepl("long_data_output\\.csv", lines)))
  expect_true(any(grepl("model_stats\\.json", lines)))
})

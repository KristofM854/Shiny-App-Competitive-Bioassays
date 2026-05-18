# Load app dependencies for testing.
# Run tests from the repo root: testthat::test_dir("tests/testthat")
#
# testthat sources helpers with chdir=TRUE so the working directory while
# this file is sourced is tests/testthat/. Resolve all paths relative to the
# repo root via "../..".
#
# global.R loads heavy packages (drc, shinyFeedback, rintrojs, ...) that may
# be unavailable in minimal CI sandboxes. Tests that need a specific package
# should `skip_if_not_installed()` themselves; non-app utility tests should
# still be able to source utils_plate.R and friends even when global.R fails.

suppressPackageStartupMessages({
  for (pkg in c("dplyr", "tidyr", "tibble", "stringr", "jsonlite")) {
    if (requireNamespace(pkg, quietly = TRUE)) {
      library(pkg, character.only = TRUE, warn.conflicts = FALSE)
    }
  }
})

.repo_root <- normalizePath(file.path("..", ".."), mustWork = TRUE)

.try_source <- function(path) {
  tryCatch(
    source(path),
    error = function(e) {
      message(sprintf("helper-setup.R: skipping %s (%s)",
                      basename(path), conditionMessage(e)))
      NULL
    }
  )
}

.try_source(file.path(.repo_root, "global.R"))
.try_source(file.path(.repo_root, "utils", "utils_plate.R"))
.try_source(file.path(.repo_root, "utils", "utils_import_v3.R"))
.try_source(file.path(.repo_root, "utils", "utils_normalization.R"))
.try_source(file.path(.repo_root, "reports", "report_constants.R"))
.try_source(file.path(.repo_root, "reports", "report_functions.R"))
.try_source(file.path(.repo_root, "reports", "plot_functions.R"))
.try_source(file.path(.repo_root, "reports", "analysis_pipeline.R"))
.try_source(file.path(.repo_root, "reports", "report_sections.R"))
.try_source(file.path(.repo_root, "server", "report_pipeline.R"))

#!/usr/bin/env Rscript
# tests/testthat/diff_golden_artifacts.R
#
# Compare two report-output directories field-by-field and print any
# numeric drift exceeding `tol`. Useful as a regression guardrail when
# refactoring the compute layer of the unified Rmd template, or when
# upgrading drc / dplyr / rmarkdown.
#
# Usage:
#
#   Rscript tests/testthat/diff_golden_artifacts.R \
#       <before_dir> <after_dir> [tol]
#
# Example:
#
#   Rscript tests/testthat/diff_golden_artifacts.R \
#       /path/to/2026-04-30_04 \
#       /path/to/2026-04-30_05
#
# Default tol = 1e-9 (relative for non-zero values, absolute for values
# below 1). The script walks both directories recursively and diffs:
#
#   * model_stats.json
#   * unknown_results.csv
#   * unknown_results_summary.csv
#   * replicate_stats.csv  (if present)
#
# at the top level AND inside any matching per-plate subdirectory (for
# multi-plate / multi-wavelength runs). Files that exist in only one side
# are reported as added / removed. The script exits with status 1 if any
# drift is detected and 0 otherwise, so it can be wired into CI.
#
# IMPLEMENTATION_PLAN.md §F0.

suppressPackageStartupMessages({
  if (!requireNamespace("jsonlite", quietly = TRUE)) {
    stop("Package 'jsonlite' is required. Install with install.packages('jsonlite').")
  }
})

# ----- numeric comparison ---------------------------------------------------
# Flag a drift when |a - b| > tol * max(1, |a|). The max(1, ...) makes the
# tolerance behave as absolute for small a and relative for large a, so we
# don't false-positive on tiny round-off near zero.
num_drifts <- function(a, b, tol) {
  na_a <- is.na(a)
  na_b <- is.na(b)
  both_na <- na_a & na_b
  one_na  <- xor(na_a, na_b)
  abs_diff  <- abs(a - b)
  rel_scale <- pmax(1, abs(a))
  drifted <- abs_diff > tol * rel_scale
  drifted[is.na(drifted)] <- FALSE  # NA from arithmetic on NA -> not drift
  one_na | drifted & !both_na
}

# ----- discover target files ------------------------------------------------
TARGET_PATTERNS <- c(
  "(^|/)model_stats\\.json$",
  "(^|/)unknown_results\\.csv$",
  "(^|/)unknown_results_summary\\.csv$",
  "(^|/)replicate_stats\\.csv$"
)

list_targets <- function(root) {
  if (!dir.exists(root)) {
    stop(sprintf("Directory not found: %s", root))
  }
  rel_paths <- list.files(root, recursive = TRUE, full.names = FALSE)
  combined  <- paste(TARGET_PATTERNS, collapse = "|")
  hits <- rel_paths[grepl(combined, rel_paths)]
  sort(hits)
}

# ----- per-file diff helpers ------------------------------------------------

diff_json <- function(before_path, after_path, tol) {
  b <- jsonlite::fromJSON(before_path, simplifyVector = FALSE)
  a <- jsonlite::fromJSON(after_path,  simplifyVector = FALSE)
  drifts <- character(0)
  keys <- union(names(b), names(a))
  for (k in keys) {
    bv <- b[[k]]
    av <- a[[k]]
    if (is.null(bv) && is.null(av)) next
    if (is.null(bv) || is.null(av)) {
      drifts <- c(drifts, sprintf("  %-22s : %s -> %s",
                                   k,
                                   if (is.null(bv)) "<absent>" else as.character(bv),
                                   if (is.null(av)) "<absent>" else as.character(av)))
      next
    }
    # Treat scalar numerics with relative tolerance; everything else by
    # exact identity. JSON's "NA" string vs real NA is also caught.
    bn <- suppressWarnings(as.numeric(bv))
    an <- suppressWarnings(as.numeric(av))
    if (length(bv) == 1 && length(av) == 1 &&
        !is.na(bn) && !is.na(an)) {
      if (num_drifts(bn, an, tol)) {
        drifts <- c(drifts, sprintf("  %-22s : %.10g -> %.10g (delta %.3g)",
                                     k, bn, an, abs(bn - an)))
      }
    } else if (!identical(bv, av)) {
      drifts <- c(drifts,
                  sprintf("  %-22s : %s -> %s", k,
                          paste(as.character(bv), collapse = ","),
                          paste(as.character(av), collapse = ",")))
    }
  }
  drifts
}

diff_csv <- function(before_path, after_path, tol, max_rows_per_col = 3) {
  b <- read.csv(before_path, stringsAsFactors = FALSE,
                na.strings = c("NA", ""), check.names = FALSE)
  a <- read.csv(after_path,  stringsAsFactors = FALSE,
                na.strings = c("NA", ""), check.names = FALSE)
  drifts <- character(0)

  if (nrow(b) != nrow(a)) {
    drifts <- c(drifts, sprintf("  row count: %d -> %d", nrow(b), nrow(a)))
    return(drifts)
  }
  only_b <- setdiff(names(b), names(a))
  only_a <- setdiff(names(a), names(b))
  if (length(only_b))
    drifts <- c(drifts, sprintf("  columns removed: %s",
                                 paste(only_b, collapse = ", ")))
  if (length(only_a))
    drifts <- c(drifts, sprintf("  columns added  : %s",
                                 paste(only_a, collapse = ", ")))

  for (col in intersect(names(b), names(a))) {
    bv <- b[[col]]
    av <- a[[col]]
    if (is.numeric(bv) && is.numeric(av)) {
      mismatch <- num_drifts(bv, av, tol)
      idx <- which(mismatch)
      if (length(idx) > 0) {
        head_idx <- head(idx, max_rows_per_col)
        for (i in head_idx) {
          drifts <- c(drifts,
                      sprintf("  row %d / col %s : %.10g -> %.10g",
                              i, col, bv[i], av[i]))
        }
        if (length(idx) > max_rows_per_col) {
          drifts <- c(drifts,
                      sprintf("  ... and %d more drifted rows in col %s",
                              length(idx) - max_rows_per_col, col))
        }
      }
    } else {
      mismatch <- !mapply(identical, bv, av)
      idx <- which(mismatch)
      if (length(idx) > 0) {
        head_idx <- head(idx, max_rows_per_col)
        for (i in head_idx) {
          drifts <- c(drifts,
                      sprintf("  row %d / col %s : '%s' -> '%s'",
                              i, col,
                              as.character(bv[i]),
                              as.character(av[i])))
        }
        if (length(idx) > max_rows_per_col) {
          drifts <- c(drifts,
                      sprintf("  ... and %d more differing rows in col %s",
                              length(idx) - max_rows_per_col, col))
        }
      }
    }
  }
  drifts
}

# ----- main -----------------------------------------------------------------
main <- function(args) {
  if (length(args) < 2) {
    stop("Usage: Rscript tests/testthat/diff_golden_artifacts.R ",
         "<before_dir> <after_dir> [tol]")
  }
  before <- normalizePath(args[[1]], mustWork = TRUE)
  after  <- normalizePath(args[[2]], mustWork = TRUE)
  tol <- if (length(args) >= 3) as.numeric(args[[3]]) else 1e-9
  if (is.na(tol) || tol <= 0) {
    stop(sprintf("Invalid tolerance: %s", args[[3]]))
  }

  cat(sprintf("diff_golden_artifacts: tol = %.1e\n", tol))
  cat(sprintf("  before: %s\n", before))
  cat(sprintf("  after : %s\n\n", after))

  before_files <- list_targets(before)
  after_files  <- list_targets(after)

  only_before <- setdiff(before_files, after_files)
  only_after  <- setdiff(after_files,  before_files)
  common      <- intersect(before_files, after_files)

  if (length(only_before)) {
    cat("Files only in BEFORE:\n")
    for (f in only_before) cat(sprintf("  - %s\n", f))
    cat("\n")
  }
  if (length(only_after)) {
    cat("Files only in AFTER:\n")
    for (f in only_after) cat(sprintf("  + %s\n", f))
    cat("\n")
  }

  total_drifted_files <- 0L
  total_drift_lines   <- 0L

  for (f in common) {
    bp <- file.path(before, f)
    ap <- file.path(after,  f)
    is_json <- grepl("\\.json$", f)
    drifts <- tryCatch(
      if (is_json) diff_json(bp, ap, tol) else diff_csv(bp, ap, tol),
      error = function(e) sprintf("  ERROR while comparing: %s", e$message)
    )
    if (length(drifts) > 0) {
      cat(sprintf("DRIFT  %s\n", f))
      for (d in drifts) cat(d, "\n", sep = "")
      cat("\n")
      total_drifted_files <- total_drifted_files + 1L
      total_drift_lines   <- total_drift_lines + length(drifts)
    } else {
      cat(sprintf("OK     %s\n", f))
    }
  }

  cat("\n--- summary ---\n")
  cat(sprintf("  files compared      : %d\n", length(common)))
  cat(sprintf("  files only in before: %d\n", length(only_before)))
  cat(sprintf("  files only in after : %d\n", length(only_after)))
  cat(sprintf("  files with drift    : %d\n", total_drifted_files))
  cat(sprintf("  drift lines emitted : %d\n", total_drift_lines))

  bad <- total_drifted_files > 0 ||
         length(only_before) > 0 ||
         length(only_after)  > 0
  quit(save = "no", status = if (bad) 1L else 0L)
}

# Allow sourcing without invoking the CLI entry point (useful for tests).
if (!interactive() && sys.nframe() == 0L) {
  main(commandArgs(trailingOnly = TRUE))
}

#!/usr/bin/env Rscript
# verify_report.R — Assert checklist items against rendered HTML reports.
# Usage:  Rscript scripts/verify_report.R <html_file> [<html_file2> ...]
# Exit:   0 = all assertions pass, 1 = one or more failures.
# CI:     called from .github/workflows/verify-report.yml after rendering.

args <- commandArgs(trailingOnly = TRUE)
if (length(args) == 0L) {
  cat("Usage: Rscript scripts/verify_report.R <html_file> [<html_file2> ...]\n")
  quit(status = 1L)
}

PASS <- 0L
FAIL <- 0L

assert_present <- function(label, pattern, lines) {
  if (any(grepl(pattern, lines, perl = TRUE))) {
    cat(sprintf("  PASS  %s\n", label))
    PASS <<- PASS + 1L
  } else {
    cat(sprintf("  FAIL  %s  [pattern: %s]\n", label, pattern))
    FAIL <<- FAIL + 1L
  }
}

assert_absent <- function(label, pattern, lines) {
  if (!any(grepl(pattern, lines, perl = TRUE))) {
    cat(sprintf("  PASS  %s (absent)\n", label))
    PASS <<- PASS + 1L
  } else {
    cat(sprintf("  FAIL  %s should be ABSENT  [pattern: %s]\n", label, pattern))
    FAIL <<- FAIL + 1L
  }
}

# Counts lines containing the pattern (mirrors bash grep -cP).
assert_count <- function(label, pattern, expected, lines) {
  count <- sum(grepl(pattern, lines, perl = TRUE))
  if (count == expected) {
    cat(sprintf("  PASS  %s (count=%d)\n", label, count))
    PASS <<- PASS + 1L
  } else {
    cat(sprintf("  FAIL  %s  expected=%d  actual=%d\n", label, expected, count))
    FAIL <<- FAIL + 1L
  }
}

assert_lte <- function(label, actual, limit) {
  if (actual <= limit) {
    cat(sprintf("  PASS  %s (%d bytes ≤ %d bytes)\n", label, actual, limit))
    PASS <<- PASS + 1L
  } else {
    cat(sprintf("  FAIL  %s  %d bytes > %d bytes\n", label, actual, limit))
    FAIL <<- FAIL + 1L
  }
}

# Counts total occurrences across all lines (mirrors bash grep -oP | wc -l).
count_occurrences <- function(pattern, lines) {
  sum(vapply(gregexpr(pattern, lines, perl = TRUE),
             function(m) if (m[[1L]] == -1L) 0L else length(m),
             integer(1L)))
}

for (html_path in args) {
  cat("\n")
  cat("========================================================\n")
  cat("Checking:", html_path, "\n")
  cat("========================================================\n")

  if (!file.exists(html_path)) {
    cat("  FAIL  File not found:", html_path, "\n")
    FAIL <- FAIL + 1L
    next
  }

  bytes      <- file.info(html_path)$size
  is_compact <- grepl("compact", basename(html_path), fixed = TRUE)
  lines      <- readLines(html_path, warn = FALSE)

  cat("\n--- A. Structure ---\n")

  assert_absent( "A.2  No <h2>Summary</h2>",            '<h2[^>]*>Summary<',         lines)
  assert_absent( "A.3  No empty Parallelism H2",         '<h2[^>]*>Parallelism',      lines)
  assert_absent( "A.4  No duplicate detailed_summary H2", '<h2[^>]*>Detailed Sample', lines)
  assert_count(  "A.5  Exactly one <h1>",                '<h1[ >]',           1L,     lines)
  assert_present("A.6  exec-summary-card class present", 'exec-summary-card|bs-kpi-strip', lines)
  assert_present("A.7  Merged QC card (.exec-summary-card)", 'class="exec-summary-card', lines)
  assert_absent( "A.8  No <pre><code>## Warning blocks", '<pre><code>[^<]*## Warning', lines)

  cat("\n--- B. Tables ---\n")

  if (!is_compact) {
    assert_present("B.1  DT datatable widget",
                   'HTMLWidgets\\.widget|class="datatables', lines)

    n_pills   <- count_occurrences('<span class="bs-status-pill', lines)
    n_rows    <- sum(grepl('class="bs-replicate-meta"', lines, perl = TRUE))
    expected2 <- n_rows * 2L
    if (n_pills >= expected2) {
      cat(sprintf("  PASS  B.2  Two pills per row (pills=%d rows=%d)\n", n_pills, n_rows))
      PASS <- PASS + 1L
    } else {
      cat(sprintf("  FAIL  B.2  Pill count %d < expected >= %d (2 * %d rows)\n",
                  n_pills, expected2, n_rows))
      FAIL <- FAIL + 1L
    }

    if (n_rows > 0L) {
      assert_present("B.2b Quant-range pill present",
                     'Within quantifiable|bs-status-pill is-info|estimable', lines)
    }
  }

  assert_present("B.5  CV 1-decimal format",        '\\d+\\.\\d%',          lines)
  assert_present("B.6  .ci-uninformative CSS defined", '\\.ci-uninformative', lines)
  assert_present("B.7  4PL coefficient table",
                 'four_pl_coefficients|Hill slope|Hill&nbsp;slope|bs-kpi',    lines)

  if (!is_compact) {
    assert_present("B.10 Exclusion Audit section rendered",
                   'exclusion.audit|Exclusion.Audit', lines)
  }

  assert_present("C6   bsSortTable JS present", 'bsSortTable', lines)

  cat("\n--- C. Visuals ---\n")

  assert_present("N1   sticky thead bg #f1f3f5",       'f1f3f5',             lines)
  assert_present("C.2  Okabe-Ito palette in CSS",      '#009E73|009E73',     lines)
  assert_present("C.3  KPI tiles individually colored",
                 'bs-kpi-tile is-pass|bs-kpi-tile is-warn|bs-kpi-tile is-fail', lines)

  if (!is_compact) {
    assert_present("C.4  <details open> for variability plot", '<details open>', lines)
    assert_present("C.5  Plate heatmap is plotly widget",
                   'plotly|heatmap.*htmlwidget', lines)
  }

  assert_present("C.6  Table width 100% CSS",
                 'width:\\s*100%|width: 100%', lines)

  cat("\n--- D. File size ---\n")

  assert_present("D.2  System font stack", '-apple-system|Segoe UI', lines)

  if (is_compact) {
    assert_absent("D.1  No base64 @font-face",
                  'data:font/ttf[;,]base64|data:font/otf[;,]base64', lines)
    assert_lte("D.6  Compact ≤ 4.5 MB", bytes, 4608000L)

    assert_present("D.7  DRC section present",
                   'Dose.{1,10}Response|Dosis-Respuesta|section_drc', lines)
    assert_absent("D.8  Compact: no plate heatmap",
                  'heatmap_title|Plate.*Heatmap', lines)
  } else {
    assert_lte("D.6  Full ≤ 5500 KB", bytes, 5632000L)
    assert_present("D.7  Full: plate heatmap plotly present",
                   'plotly|heatmap.*htmlwidget|type.*heatmap', lines)
  }

  if (is_compact) {
    assert_absent( "A.9  Compact: no Methods section",    '<strong>Methods</strong>',          lines)
    assert_absent( "A.10 Compact: no Curve Diagnostics",  '<strong>Curve Diagnostics</strong>', lines)
    assert_present("A.11 Compact: Interpretation present", 'Interpretation|<details open>',     lines)
  }

  if (grepl("_es\\.html$", basename(html_path))) {
    cat("\n--- B.4 Spanish smoke-test (no literal i18n keys) ---\n")
    es_keys <- c("cal_in_range", "cal_below", "cal_above",
                 "quant_in_range", "quant_below_lloq", "quant_above_uloq",
                 "quant_extrapolated", "drc_legend_hint")
    for (k in es_keys) {
      assert_absent(paste0("B.4  Key token absent: ", k),
                    paste0("\\b", k, "\\b"), lines)
    }
  }

  cat("\n--- E. Print ---\n")
  assert_present("E.1  @media print block",        '@media print',                          lines)
  assert_present("E.1  Print hides TOC",           '#TOC',                                  lines)
  assert_present("E.1  Print forces details open", 'details.*display.*block|page-break-inside.*avoid', lines)

  cat("\n--- F. Responsive ---\n")
  assert_present("F.1  Responsive TOC min-width 992px",
                 'min-width:\\s*992px|min-width: 992px', lines)
}

cat("\n")
cat("========================================================\n")
cat(sprintf("Results: %d passed, %d failed\n", PASS, FAIL))
cat("========================================================\n")

quit(status = if (FAIL == 0L) 0L else 1L)

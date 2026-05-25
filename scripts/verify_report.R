#!/usr/bin/env Rscript
# verify_report.R — Pure-R port of scripts/verify_report.sh.
#
# Why this exists:
#   Calling the bash script from Windows with the project sitting in
#   OneDrive ("OneDrive - IAEA/My Documents/...") fails because of MSYS
#   path conversion and embedded spaces. R doesn't care about any of that.
#
# Usage:
#   From R:   source("scripts/verify_report.R"); verify_reports(c(html1, html2))
#   From CLI: Rscript scripts/verify_report.R path1.html [path2.html ...]
#
# Behaviour matches verify_report.sh: prints PASS/FAIL per assertion,
# exits non-zero if any fail. Sized for the same A–F checklist.

`%||%` <- function(a, b) if (is.null(a) || length(a) == 0L) b else a

verify_reports <- function(html_files, log_file = NULL) {
  if (length(html_files) == 0L) {
    cat("Usage: verify_reports(c('path1.html', 'path2.html', ...))\n")
    return(invisible(1L))
  }

  pass <- 0L
  fail <- 0L
  lines <- character(0)

  emit <- function(s) {
    cat(s, "\n", sep = "")
    lines[[length(lines) + 1L]] <<- s
  }

  assert_present <- function(label, pattern, txt) {
    ok <- length(grep(pattern, txt, perl = TRUE)) > 0L
    if (ok) { pass <<- pass + 1L; emit(sprintf("  PASS  %s", label)) }
    else    { fail <<- fail + 1L; emit(sprintf("  FAIL  %s  [pattern: %s]", label, pattern)) }
  }
  assert_absent <- function(label, pattern, txt) {
    ok <- length(grep(pattern, txt, perl = TRUE)) == 0L
    if (ok) { pass <<- pass + 1L; emit(sprintf("  PASS  %s (absent)", label)) }
    else    { fail <<- fail + 1L; emit(sprintf("  FAIL  %s should be ABSENT  [pattern: %s]", label, pattern)) }
  }
  assert_count <- function(label, pattern, expected, txt) {
    n <- length(grep(pattern, txt, perl = TRUE))
    if (n == expected) { pass <<- pass + 1L; emit(sprintf("  PASS  %s (count=%d)", label, n)) }
    else               { fail <<- fail + 1L; emit(sprintf("  FAIL  %s  expected=%d  actual=%d", label, expected, n)) }
  }
  assert_lte <- function(label, actual, limit) {
    if (actual <= limit) { pass <<- pass + 1L; emit(sprintf("  PASS  %s (%d bytes <= %d bytes)", label, actual, limit)) }
    else                 { fail <<- fail + 1L; emit(sprintf("  FAIL  %s  %d bytes > %d bytes",  label, actual, limit)) }
  }

  for (html in html_files) {
    emit(""); emit(strrep("=", 56))
    emit(sprintf("Checking: %s", html))
    emit(strrep("=", 56))
    if (!file.exists(html)) {
      fail <- fail + 1L
      emit(sprintf("  FAIL  File not found: %s", html))
      next
    }

    # grep -c works line-by-line; we want the same so read by line, paste later
    raw <- paste(readLines(html, warn = FALSE), collapse = "\n")
    bytes <- file.info(html)$size
    is_compact <- grepl("compact", basename(html), ignore.case = TRUE)

    emit(""); emit("--- A. Structure ---")
    assert_absent("A.2  No <h2>Summary</h2>",                 "<h2[^>]*>Summary<",       raw)
    assert_absent("A.3  No empty Parallelism H2",             "<h2[^>]*>Parallelism",    raw)
    assert_absent("A.4  No duplicate detailed_summary H2",    "<h2[^>]*>Detailed Sample", raw)
    assert_count ("A.5  Exactly one <h1>",                    "<h1[ >]",                 1L, raw)
    assert_present("A.6  exec-summary-card present",          "exec-summary-card|bs-kpi-strip", raw)
    assert_present("A.7  Merged QC card",                     'class="exec-summary-card', raw)
    assert_absent ("A.8  No '## Warning' blocks",             "<pre><code>[^<]*## Warning", raw)

    emit(""); emit("--- B. Tables ---")
    n_pills <- length(grep('<span class="bs-status-pill', raw, perl = TRUE))
    n_rows  <- length(grep('class="bs-replicate-meta"', raw, perl = TRUE))
    if (!is_compact) {
      expected <- n_rows * 2L
      if (n_pills >= expected) {
        pass <- pass + 1L
        emit(sprintf("  PASS  B.2  Two pills per row (pills=%d rows=%d)", n_pills, n_rows))
      } else {
        fail <- fail + 1L
        emit(sprintf("  FAIL  B.2  Pill count %d < expected >= %d (2 * %d rows)",
                     n_pills, expected, n_rows))
      }
      if (n_rows > 0L)
        assert_present("B.2b Quant-range pill present",
                       'Within quantifiable|bs-status-pill is-info|estimable', raw)
      assert_present("B.3  row-out-of-range CSS class wired", "row-out-of-range", raw)
      assert_present("B.3  row-cv-high CSS class wired",      "row-cv-high",       raw)
      if (n_rows > 0L)
        assert_present("B.3a row-out-of-range on <tr>",
                       '<tr[^>]*class="[^"]*row-out-of-range', raw)
    }
    assert_present("B.5  CV 1-decimal",                       "\\d+\\.\\d%", raw)
    assert_present("B.6  .ci-uninformative CSS defined",      "\\.ci-uninformative", raw)
    assert_present("B.7  4PL table present",
                   "four_pl_coefficients|Hill slope|Hill&nbsp;slope|bs-kpi", raw)
    if (!is_compact)
      assert_present("B.10 Exclusion Audit rendered",
                     "exclusion.audit|Exclusion.Audit", raw)
    assert_present("C6   bsSortTable JS",                     "bsSortTable", raw)

    emit(""); emit("--- C. Visuals ---")
    assert_present("N1   sticky thead bg #f1f3f5",            "f1f3f5", raw)
    assert_present("C.2  Okabe-Ito palette",                  "#009E73|009E73", raw)
    assert_present("C.3  KPI tiles individually colored",
                   "bs-kpi-tile is-pass|bs-kpi-tile is-warn|bs-kpi-tile is-fail", raw)
    if (!is_compact) {
      assert_present("C.4  <details open> for variability",   "<details open>", raw)
      assert_present("C.5  Plate heatmap is plotly",
                     "plotly|heatmap.*htmlwidget", raw)
    }
    assert_present("C.6  Table width 100% CSS",
                   "width:\\s*100%|width: 100%", raw)

    emit(""); emit("--- D. File size ---")
    assert_present("D.2  System font stack",                  "-apple-system|Segoe UI", raw)
    if (is_compact) {
      assert_absent("D.1  No base64 @font-face",
                    "data:font/ttf[;,]base64|data:font/otf[;,]base64", raw)
      assert_lte("D.6  Compact <= 4 MB", bytes, 4194304L)
      n_plotly <- length(grep('class="plotly html-widget', raw, perl = TRUE))
      if (n_plotly == 1L) {
        pass <- pass + 1L
        emit("  PASS  D.7  Compact has exactly 1 Plotly widget (DRC)")
      } else {
        fail <- fail + 1L
        emit(sprintf("  FAIL  D.7  Compact Plotly count: expected 1, got %d", n_plotly))
      }
      assert_absent("D.8  Compact: no plate heatmap",         "heatmap_title|Plate.*Heatmap", raw)
      assert_absent("A.9  Compact: no Methods section",       "<strong>Methods</strong>", raw)
      assert_absent("A.10 Compact: no Curve Diagnostics",     "<strong>Curve Diagnostics</strong>", raw)
      assert_present("A.11 Compact: Interpretation present",  "Interpretation|<details open>", raw)
    } else {
      assert_lte("D.6  Full <= 5500 KB", bytes, 5632000L)
      assert_present("D.7  Full: plate heatmap plotly",
                     "plotly|heatmap.*htmlwidget|type.*heatmap", raw)
    }

    emit(""); emit("--- E. Print ---")
    assert_present("E.1  @media print block",                 "@media print", raw)
    assert_present("E.1  Print hides TOC",                    "#TOC", raw)
    assert_present("E.1  Print forces details open",
                   "details.*display.*block|page-break-inside.*avoid", raw)

    emit(""); emit("--- F. Responsive ---")
    assert_present("F.1  Responsive TOC min-width 992px",
                   "min-width:\\s*992px|min-width: 992px", raw)
  }

  emit(""); emit(strrep("=", 56))
  emit(sprintf("Results: %d passed, %d failed", pass, fail))
  emit(strrep("=", 56))

  if (!is.null(log_file)) {
    dir.create(dirname(log_file), recursive = TRUE, showWarnings = FALSE)
    writeLines(lines, log_file)
    cat(sprintf("Log written: %s\n", log_file))
  }
  invisible(as.integer(fail > 0L))
}

# CLI entry point
if (sys.nframe() == 0L) {
  args <- commandArgs(trailingOnly = TRUE)
  if (length(args) == 0L) {
    cat("Usage: Rscript scripts/verify_report.R <html_file> [<html_file2> ...]\n")
    quit(status = 1L)
  }
  status <- verify_reports(args)
  quit(status = status)
}

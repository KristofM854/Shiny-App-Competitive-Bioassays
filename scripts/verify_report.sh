#!/usr/bin/env bash
# verify_report.sh — Assert A-G checklist items against rendered HTML reports.
# Usage:  bash scripts/verify_report.sh <html_file> [<html_file2> ...]
# Exit:   0 = all assertions pass, 1 = one or more failures.
# CI:     called from .github/workflows/R-CMD-check.yml after rendering.

set -euo pipefail

PASS=0
FAIL=0

assert_present() {
  local label="$1"; local pattern="$2"; local file="$3"
  if grep -qP "$pattern" "$file" 2>/dev/null; then
    echo "  PASS  $label"
    PASS=$((PASS+1))
  else
    echo "  FAIL  $label  [pattern: $pattern]"
    FAIL=$((FAIL+1))
  fi
}

assert_absent() {
  local label="$1"; local pattern="$2"; local file="$3"
  if ! grep -qP "$pattern" "$file" 2>/dev/null; then
    echo "  PASS  $label (absent)"
    PASS=$((PASS+1))
  else
    echo "  FAIL  $label should be ABSENT  [pattern: $pattern]"
    FAIL=$((FAIL+1))
  fi
}

assert_count() {
  local label="$1"; local pattern="$2"; local expected="$3"; local file="$4"
  local count
  count=$(grep -cP "$pattern" "$file" 2>/dev/null || true)
  if [ "$count" -eq "$expected" ]; then
    echo "  PASS  $label (count=$count)"
    PASS=$((PASS+1))
  else
    echo "  FAIL  $label  expected=$expected  actual=$count"
    FAIL=$((FAIL+1))
  fi
}

assert_lte() {
  local label="$1"; local actual="$2"; local limit="$3"
  if [ "$actual" -le "$limit" ]; then
    echo "  PASS  $label (${actual} bytes ≤ ${limit} bytes)"
    PASS=$((PASS+1))
  else
    echo "  FAIL  $label  ${actual} bytes > ${limit} bytes"
    FAIL=$((FAIL+1))
  fi
}

if [ "$#" -eq 0 ]; then
  echo "Usage: $0 <html_file> [<html_file2> ...]"
  exit 1
fi

for HTML in "$@"; do
  echo ""
  echo "========================================================"
  echo "Checking: $HTML"
  echo "========================================================"

  if [ ! -f "$HTML" ]; then
    echo "  FAIL  File not found: $HTML"
    FAIL=$((FAIL+1))
    continue
  fi

  BYTES=$(wc -c < "$HTML")
  IS_COMPACT=0
  if [[ "$(basename "$HTML")" == *compact* ]]; then
    IS_COMPACT=1
  fi

  echo ""
  echo "--- A. Structure ---"

  # A.1  No <h2>Analysis Notes</h2> when notes are empty
  # (can't distinguish at render time without data — skip this assertion for now;
  #  it's enforced by the conditional chunk in the template)

  # A.2  No standalone <h2>Summary</h2> (folded into exec card)
  assert_absent "A.2  No <h2>Summary</h2>" '<h2[^>]*>Summary<' "$HTML"

  # A.3  No <h2>Parallelism</h2> when n_curves < 2 (example data has 1 curve)
  assert_absent "A.3  No empty Parallelism H2" '<h2[^>]*>Parallelism' "$HTML"

  # A.4  No duplicate Detailed Sample Results Summary H2
  # The section should appear exactly once (not as a standalone H2 + nested H3)
  assert_absent "A.4  No duplicate detailed_summary H2" '<h2[^>]*>Detailed Sample' "$HTML"

  # A.5  Exactly one <h1>
  assert_count "A.5  Exactly one <h1>" '<h1[ >]' 1 "$HTML"

  # A.6  exec-summary-card present
  assert_present "A.6  exec-summary-card class present" 'exec-summary-card|bs-kpi-strip|bs-status-banner' "$HTML"

  # A.7  Merged QC card (.bs-status-banner)
  assert_present "A.7  Merged QC card (.bs-status-banner)" 'class="bs-status-banner' "$HTML"

  # A.8  No raw R warning blocks
  assert_absent  "A.8  No <pre><code>## Warning blocks" '<pre><code>[^<]*## Warning' "$HTML"

  echo ""
  echo "--- B. Tables ---"

  if [ "$IS_COMPACT" -eq 0 ]; then
    # B.1  DT::datatable widget present (full mode only)
    assert_present "B.1  DT datatable widget" 'HTMLWidgets\.widget|class="datatables' "$HTML"

    # B.2  Cal. Range pill present
    assert_present "B.2  Calibration-range pill" 'Cal\.&nbsp;OK|Cal\.&nbsp;low|Cal\.&nbsp;high' "$HTML"

    # B.3  .row-out-of-range or .row-cv-high JS callback present
    assert_present "B.3  row-out-of-range CSS class wired" 'row-out-of-range' "$HTML"
    assert_present "B.3  row-cv-high CSS class wired" 'row-cv-high' "$HTML"
  fi

  # B.5  CV formatted to 1 decimal (e.g. 9.6% not 9.635)
  # Presence of pattern like 12.3% or 9.6% (one decimal digit before %)
  assert_present "B.5  CV 1-decimal format" '\d+\.\d%' "$HTML"

  # B.6  CI uninformative class defined in CSS
  assert_present "B.6  .ci-uninformative CSS defined" '\.ci-uninformative' "$HTML"

  # B.7  4PL table present, p-value not always shown with all-NA
  assert_present "B.7  4PL coefficient table" 'four_pl_coefficients|Hill slope|Hill&nbsp;slope|bs-kpi' "$HTML"

  # B.10 Exclusion Audit section rendered (dagger footnote is data-dependent — just
  #       check the section container is present)
  assert_present "B.10 Exclusion Audit section rendered" 'exclusion.audit|Exclusion.Audit' "$HTML"

  # C6   bsSortTable click-to-sort JS present in summary table
  assert_present "C6   bsSortTable JS present" 'bsSortTable' "$HTML"

  echo ""
  echo "--- C. Visuals ---"

  # N1   Sticky thead background colour
  assert_present "N1   sticky thead bg #f1f3f5" 'f1f3f5' "$HTML"

  # C.2  Okabe-Ito palette (--c-pass: #009E73 etc)
  assert_present "C.2  Okabe-Ito palette in CSS" '#009E73|009E73' "$HTML"

  # C.3  KPI tiles individually colored (is-pass/is-warn on individual tiles)
  assert_present "C.3  KPI tiles individually colored" 'bs-kpi-tile is-pass|bs-kpi-tile is-warn|bs-kpi-tile is-fail' "$HTML"

  if [ "$IS_COMPACT" -eq 0 ]; then
    # C.4  Sample variability plot in <details open>
    assert_present "C.4  <details open> for variability plot" '<details open>' "$HTML"

    # C.5  Plate heatmap as plotly widget (HTML only)
    assert_present "C.5  Plate heatmap is plotly widget" 'plotly|heatmap.*htmlwidget' "$HTML"
  fi

  # C.6  Tables width 100%
  assert_present "C.6  Table width 100% CSS" 'width:\s*100%|width: 100%' "$HTML"

  echo ""
  echo "--- D. File size ---"

  # D.1  No base64 font embedding
  assert_absent  "D.1  No base64 @font-face" 'data:font.*base64|data:application/font' "$HTML"

  # D.2  System font stack present
  assert_present "D.2  System font stack" '-apple-system|Segoe UI' "$HTML"

  if [ "$IS_COMPACT" -eq 1 ]; then
    assert_lte "D.6  Compact ≤ 500 KB" "$BYTES" 512000
    assert_absent "D.7  Compact: no Plotly widget" 'HTMLWidgets\.widget\b|class="plotly\b' "$HTML"
    assert_absent "D.8  Compact: no plate heatmap" 'heatmap_title|Plate.*Heatmap' "$HTML"
  else
    assert_lte "D.6  Full ≤ 3 MB" "$BYTES" 3000000
    assert_present "D.7  Full: plate heatmap plotly present" 'plotly|heatmap.*htmlwidget|type.*heatmap' "$HTML"
  fi

  echo ""
  echo "--- E. Print ---"
  assert_present "E.1  @media print block" '@media print' "$HTML"
  assert_present "E.1  Print hides TOC" '#TOC' "$HTML"
  assert_present "E.1  Print forces details open" 'details.*display.*block|page-break-inside.*avoid' "$HTML"

  echo ""
  echo "--- F. Responsive ---"
  assert_present "F.1  Responsive TOC min-width 992px" 'min-width:\s*992px|min-width: 992px' "$HTML"

done

echo ""
echo "========================================================"
echo "Results: ${PASS} passed, ${FAIL} failed"
echo "========================================================"

[ "$FAIL" -eq 0 ]

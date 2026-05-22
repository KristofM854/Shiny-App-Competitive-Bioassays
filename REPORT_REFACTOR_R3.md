# Report refactor — Round 3 implementation guide

Status as of the last render (compact `19113d4a`, full `d17b2899`):

- Compact and full are nearly identical in structure (4.7 MB vs 5.05 MB; same H1–H3 tree).
- Duplicate "Detailed Sample Results Summary" sections (one empty, one with the table) still present.
- Section order: Standards → Methods → DRC → ... — exec card not at top.
- Status pills: single pill per row; two stacked pills (Cal. Range + Quant. Range) never implemented.
- `.inject_row_classes()` is wired in `plot_functions.R` but the rendered HTML carries `row-out-of-range` / `is-extrapolated` exactly **once each** (the CSS rule line). No `<tr>` actually receives the class.
- Plate heatmap inversion: plotly in compact, missing in full.
- Empty `<h2>Parallelism and Relative Potency</h2>` still renders.

Recent wins worth preserving: Lato fonts removed, box plot 480 px with no facet strips, sticky thead with bg, `.ci-uninformative` applied, sort+filter JS on the sample table, Brown-Forsythe degenerate case suppressed, no R warning leaks.

Targets after this round:
- **compact ≤ 500 KB**, no Plotly, no plate heatmap, single page
- **full ≤ 3 MB**
- exec card visible above the fold (inside the header block)
- no duplicate sections; no empty H2s; section order matches §1 below

---

## Section order (final)

```
HEADER BLOCK  (no H2 — sits above the first heading)
  • Title + meta tiles (analyst / date / assay / wells / controls)
  • Exec summary card  (Overall pill + R² + RMSE + LLOQ/ULOQ + n samples + n flagged)
  • KPI strip          (5 tiles, color-coded by threshold)

H2  Dose–Response Curve & Samples       ← the COMBINED plot (was "DRC with Unknown Samples")
                                          ← Standalone DRC plot is REMOVED. Plotly legend toggling
                                            lets the user hide/show standards vs. samples on demand.

H2  Sample Concentration Results
    • Filter + sortable sample table (existing bs-table-filter + bs-table-scroll widget)
    • Flagged rows sorted to the top
    • Footnotes (CI methods, lower-bound truncation note, output-file list)

H2  Methods                              <details>  closed
    H3  Standard curve configuration
    H3  LLOQ / ULOQ derivation
    H3  Confidence intervals
    H3  Outlier detection
    H3  Software

H2  Curve diagnostics                    <details>  closed
    H3  4PL model parameters
    H3  Weighting suitability assessment
    H3  Standard back-calculation and recovery
    H3  Quality control summary

H2  Plate diagnostics                    <details>  closed   (full only — see §3)
    H3  Sample variability
    H3  Plate heatmap

H2  Interpretation                       <details>  open by default
    • narrative paragraph + recommendations

H2  Exclusion audit                      <details>  closed
H2  Abbreviations                        <details>  closed
```

Notes:
- The H2s "Standard Curve Configuration", "Model Parameters", "Sample Variability Visualization", "Plate Heatmap", "Dose-Response Curve with Unknown Samples", "Summary", "Interpretation & Recommendations", and the duplicate "Detailed Sample Results Summary" all disappear as separate top-level sections.
- "Summary" is folded into the exec card; if any unique content remained, move it into Interpretation.
- Sample Variability is the **only** `<details>` open by default in full mode.

---

## PRs (suggested order, each independently mergeable)

### PR 1 — Structural cleanup

**Files:** `reports/unified_analysis_template.Rmd`, `reports/report_sections.R`, `server/i18n.R`.

1. **Merge duplicate sample-results sections.** Delete the `Sample Concentration Results` H2 entirely; move its prose (CI methods, truncation note, output-file list) into footnotes directly beneath the sample table inside the now-renamed `Sample Concentration Results` section (the one that currently renders as `detailed-sample-results-summary-1`).
2. **Remove the standalone DRC plot section.** Keep only the combined "DRC with Unknown Samples" plot, retitled `Dose–Response Curve & Samples`. Tooltip-driven hide/show in the Plotly legend covers the standalone view.
3. **Hide empty Parallelism section.** Gate the chunk on `n_curves >= 2` (no H2 if no body).
4. **Hide "Summary" H2.** Move any unique bullets to Interpretation or the exec card; delete the chunk.
5. **Move exec card into the header.** Render the `bs-status-banner` block (with overall pill + R² + RMSE + LLOQ/ULOQ + n samples + n flagged) *inside* the `.bs-header-flex` container, beside or below the meta tiles, before the first H2. Drop the `bs-status-banner` class in favor of `exec-summary-card` for naming consistency.
6. **Reorder chunks** in the Rmd to match §1 above. Group existing diagnostic chunks under one `Curve diagnostics` `<details>` wrapper and the plate chunks under `Plate diagnostics`.
7. **Default open/closed:** Only `Sample variability` and `Interpretation` are `<details open>`; everything else closed.
8. **i18n keys** for new section titles: `section_drc_samples`, `section_curve_diagnostics`, `section_plate_diagnostics`. Add to `server/i18n.R` for `en` / `de` / `es`.

### PR 2 — Status pills + row highlighting

**Files:** `reports/analysis_pipeline.R`, `reports/plot_functions.R` (`.inject_row_classes()`), `reports/report_style.css`.

1. **Two stacked pills per row.** Replace the single `Status` column with two columns or two stacked pills in one cell:
   - **Cal. Range:** `In range` (pass) / `<Cal. low` (warn) / `>Cal. high` (warn)
   - **Quant. Range:** `Within quantifiable` (pass) / `<LLOQ` (warn) / `>ULOQ` (warn) / `Extrapolated` (info)
   - Render order: Cal. on top, Quant. on bottom, separated by `<br>`.
2. **Forensics on `.inject_row_classes()`.** Currently only 1 `is-extrapolated` and 0 `row-out-of-range` actually emitted. Likely causes to check in order:
   - `render_table()` is called without the `row_highlight = list(amber_rows = …, red_left_rows = …)` argument for the sample table (chunk path: `unified_analysis_template.Rmd` sample-results chunk → does it pass `row_highlight`?).
   - The `amber_rows` / `red_left_rows` vectors are computed from a different data frame than the one passed to `render_table()` (off-by-one row indices after sorting / filtering).
   - The kable post-processor regex `(?=<tr\\b)` fails on whichever HTML kable now produces (kableExtra may have changed the markup since the helper was written).
   - **Action:** add `message(sprintf("amber=%d red=%d total=%d", length(amber_rows), length(red_rows), nrow(data)))` inside `.inject_row_classes()`. Re-render. If counts are 0, the chunk isn't passing them. If counts are correct but rows still aren't highlighted, the regex is broken.
3. **Drop the calibration vs. quantifiable interpretation blockquote** at the bottom of the report if both ranges are now visible on every row — the redundancy was noted in earlier rounds.
4. **CSS:** add `.bs-status-pill.is-info { color: var(--c-info); background: var(--c-info-soft); }` for the `Extrapolated` info pill.

### PR 3 — Compact mode = real single page

**Files:** `reports/unified_analysis_template.Rmd`, `reports/compact_*.Rmd` (if a wrapper exists), `reports/report_pipeline.R`, `reports/report_style.css`.

Goal: compact ≤ 500 KB, no Plotly, no plate heatmap, no diagnostics, no detailed-replicate-stats blocks. What the bench scientist needs on one screen.

1. **Drop Plotly entirely in compact.** Render the DRC+samples plot as a static `ggplot` exported to inline SVG (or PNG fallback). No `plotly::ggplotly()` call. No `htmlwidgets` script tags. This alone saves ~3 MB.
2. **Drop the remaining font `@font-face data:font/ttf;base64` blob.** Use the system stack already in `body { font-family }`. ~250 KB savings.
3. **Compact = exec header + DRC plot + sample table + interpretation.** Everything else (`Methods`, `Curve diagnostics`, `Plate diagnostics`, `Exclusion audit`, `Abbreviations`) gated `eval = !is_compact`.
4. **Sample table in compact:** all groups, sorted with flagged at top. Definition of flagged: out-of-calibration OR CV > 30 % OR `<LLOQ` / `>ULOQ`. Keep the filter input + sticky thead.
5. **Print `wc -c` of the output to stderr** at end of render so each render's size is visible in the Shiny log.
6. **CSS:** add a `body.compact { … }` modifier that the compact wrapper sets on `<body>` for any compact-only style tweaks (tighter spacing, no fixed TOC sidebar).

### PR 4 — Polish & verification

**Files:** `reports/unified_analysis_template.Rmd`, `scripts/verify_report.sh`, `reports/report_style.css`.

1. **Plate heatmap inversion fix:** chunk gate is `eval = !is_compact`; the chunk renders Plotly when `is_html_out()`. Verify both: compact omits the chunk; full renders Plotly. (Current state is reversed.)
2. **Consolidate naming:** rename `bs-status-banner` → `exec-summary-card` (CSS + R helpers) to match the spec. Keep the existing class name as an alias if back-compat matters.
3. **Extend `scripts/verify_report.sh`** with assertions for every item in PRs 1–3, against the rendered HTML for both example datasets in both modes:
   - `! grep -q "<h2>Sample Concentration Results</h2>" $f && grep -q "Detailed Sample Results" $f` → merged
   - `! grep -q "<h2>Parallelism" $f` → empty section hidden
   - `! grep -q "<h2>Summary</h2>" $f` → summary folded into exec card
   - `grep -q "exec-summary-card" $f` → exec card at top, in header
   - `grep -cE "bs-status-pill" $f` ≥ 2 × nrow(samples) → two pills per row
   - `grep -cE "row-out-of-range|is-extrapolated" $f` ≥ expected number of flagged rows (parameterize per dataset, or assert ≥ 10)
   - compact: `! grep -q "plotly" $f` and `! grep -q "data:font/ttf;base64" $f`
   - compact: `wc -c $f` < 512000
   - full: `wc -c $f` < 3_000_000
4. **Print verify_report.sh full output and `wc -c` for all 4 HTMLs in the PR description.**

---

## Definition of done

A change is "done" only if it satisfies *all three*:

1. Source code edited and committed.
2. Re-rendered HTML (after `unlink("*_cache", recursive=TRUE); unlink(".fit_all_models_cache.rds"); unlink(".quantify_samples_cache.rds")`) shows the change.
3. `scripts/verify_report.sh` has an assertion that fails when the change is reverted.

Items reported "done" in commit `331237a` that turned out to be invisible in the rendered HTML are the reason we now require all three.

---

## Out of scope for this round

- Switching to `DT::datatable` (the custom `bs-table-filter` + `bsSortTable` widget works fine and avoids the 200 KB DT bundle).
- LaTeX-based PDF export (Save-as-PDF from the browser is acceptable; `@media print` block is already in place).
- Translation review of new strings (translator pass happens after structural work lands).

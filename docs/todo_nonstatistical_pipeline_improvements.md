# To-Do List: Non-Statistical Pipeline, Reporting, and UX Improvements

Repo: `KristofM854/Shiny-App-Competitive-Bioassays`

## Scope
This file intentionally excludes:
- statistical-method improvements,
- assay-modeling enhancements,
- and developer-mode/debug-summary ideas.

It focuses only on later-stage improvements in:
- code efficiency/modularity/maintainability,
- report preparation and output architecture,
- Shiny usability,
- workflow robustness,
- and testing infrastructure.

---

# 1. Report architecture and format safety

## 1.1 Centralize output-format handling ✅ COMPLETE
7 helpers in `reports/plot_functions.R`: `is_html_out()`, `render_plot()`, `section_start/end()`, `section_open/close()`, `emit_heading()`, `emit_styled_block()`. All scattered `knitr::is_html_output()` calls in the unified template replaced with `is_html_out()` (only 2 remain in the exec-summary box, both with proper else branches). Multiwavelength template now sources `plot_functions.R` and uses `render_table()` instead of inline format checks.

## 1.2 Reduce raw HTML in report templates ✅ COMPLETE
All 48 raw HTML constructs in `unified_analysis_template.Rmd` replaced: 19 raw `<details>/<summary>` pairs → `section_open()`/`section_close()`; 8 cat-based `<details>` pairs → `section_start()`/`section_end()`; 7 `<h4>` tags → `emit_heading()`; 4 `plotly::ggplotly()` sites → `render_plot()`; 2 `<div style>` blocks → `emit_styled_block()`. Zero `<details>`, `<h4>`, `ggplotly`, `htmltools::tagList` remain in either template.

## 1.3 Move more business logic out of the unified Rmd — DEFERRED
The template still carries heavy computation in `model-fitting` and `sample-analysis` chunks. Full extraction to helpers is a large refactor that risks breaking the rendering pipeline. Deferred in favor of higher-impact tasks.

**Remaining limitation:** 2681-line template still mixes computation and presentation.

## 1.4 Make child-template rendering safer ✅ COMPLETE
Multiwavelength template now sources `plot_functions.R` and `report_constants.R`, gaining access to all format-aware helpers. Inline format checks replaced with `render_table()`. Unified template (used as child) has zero raw HTML constructs. Both templates have `pdf_document` in YAML headers.

---

# 2. Import pipeline and file handling

## 2.1 Complete the strict single-read / single-detection import goal ✅ COMPLETE
Extracted `read_file_raw()` as shared reader in `utils_import_v3.R`. Added optional `raw_data`/`location`/`raw_matrix` params to `detect_plate_location()`, `import_plate_data()`, and `detect_and_import_multiwavelength()`. `parse_plate_file()` calls `read_file_raw()` exactly once and passes data through. File-read count: 1 for Excel multi-wave, 1 for Excel single-plate, 1 for CSV/TXT.

## 2.2 Further simplify upload-module structure — DEFERRED
`server_upload.R` (645 lines) has clear section dividers and a coherent structure. Splitting visual selector into its own file would improve modularity but is lower priority than UI and testing tasks. Deferred.

---

# 3. Shiny usability improvements

## 3.1 Improve prominence of Advanced Options ✅ COMPLETE
Replaced collapsed `<details>` with an amber card (background #FFF8E1, orange left-border accent, sliders icon, explicit heading, explanatory subtitle). Open by default.

## 3.2 Clarify report output behavior in the UI ✅ COMPLETE
Export format selector now shows three options (HTML, Word (DOCX), PDF) with two-line help text: "HTML reports have interactive plots. Word and PDF use static figures." / "PDF requires a LaTeX engine (e.g. TinyTeX). If unavailable, the app will fall back to HTML."

## 3.3 Improve severity signaling in pre-flight checks ✅ COMPLETE
Added a summary badge at the top of the pre-flight panel:
- Red badge: "Blocking issues found — resolve before generating report"
- Orange badge: "Warnings found — report can be generated but review recommended"
- Green badge: "All checks passed — ready to generate report"
The badge dynamically reflects whether any red (blocking) or orange (warning) items are present.

## 3.4 Improve visual selector feedback ✅ COMPLETE
Three improvements to the visual plate selector:
- Per-plate **exclusion count badge** (red pill: "3 wells excluded")
- **Primary plate badge** (green pill: "Primary plate") on the first selected plate
- **Multi-plate context help**: when multiple plates detected, explains that selecting multiple enables multi-wavelength comparison and first plate is primary

---

# 4. Workflow robustness and output behavior

## 4.1 Add graceful capability checks for report output
The app should explicitly detect report-environment capabilities.

Examples:
- DOCX path available
- PDF path available
- HTML always available

Goal:
- fail gracefully
- provide clear fallback behavior
- avoid hard render failures when optional capabilities are missing

## 4.2 Strengthen separation between artifacts and report rendering
The app already saves CSV/JSON artifacts.

Goal:
- make reports more clearly consume saved artifacts rather than recomputing too much inline
- improve reproducibility and debugging

---

# 5. Testing infrastructure (high value, later-stage)

## 5.1 Add lightweight regression/smoke tests
The repo is now rich enough that even a small test layer would prevent regressions.

High-value initial targets:
- import smoke tests
- report render smoke tests
- HTML render smoke tests
- DOCX render smoke tests once format-safe rendering is complete
- PDF fallback behavior tests
- ELISA tissue-normalization smoke test
- interpolation-fallback render test

Goal:
- catch regressions early
- make future refactors safer
- distinguish structural correctness from actually working output

## 5.2 Add representative fixture datasets
Create a small curated set of example inputs for testing and regression checking.

Suggested fixtures:
- RBA nominal dataset
- ELISA nominal dataset
- ELISA tissue-normalized dataset
- multi-wavelength dataset
- interpolation-fallback dataset
- outlier-detection dataset

Goal:
- allow consistent test coverage
- support both automated and manual validation
- make bug reproduction easier

---

# Recommended later-stage implementation order

1. Centralize report output-format handling
2. Complete strict single-read import refactor
3. Improve Advanced Options visibility and output-format messaging in the UI
4. Improve pre-flight severity signaling and visual selector feedback
5. Strengthen artifact/report separation
6. Add lightweight smoke/regression tests
7. Add fixture datasets for repeatable validation

---

# One-line summary

The next non-statistical improvements should focus on cleaner report architecture, a fully efficient import path, clearer Shiny UX, graceful output capability handling, and a lightweight testing layer supported by representative fixture datasets.

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

## 2.1 Complete the strict single-read / single-detection import goal
A unified import entry point exists, but the internal path still duplicates work in some fallback cases.

Goal:
- one file read per classic import
- one detection workflow per import attempt
- no restart of detection from scratch in fallback branches

## 2.2 Further simplify upload-module structure
`server_upload.R` still contains several distinct responsibilities:
- file preview
- visual selector state
- exclusions
- classic import
- visual import confirmation
- heatmap preview

Goal:
- separate visual-selector logic more cleanly
- make the upload module easier to reason about and maintain

---

# 3. Shiny usability improvements

## 3.1 Improve prominence of Advanced Options
Advanced Options in Tab 4 remain too easy to miss.

Goal:
- stronger visual treatment
- clearer label
- short explanation of what the section controls
- consider default-open behavior

## 3.2 Clarify report output behavior in the UI
Users should understand the output trade-offs clearly.

Goal:
- explain that HTML is interactive
- explain that DOCX/PDF use static figures
- explain PDF fallback behavior if no TeX environment is available

## 3.3 Improve severity signaling in pre-flight checks
Pre-flight checks have improved, but the app should make it even clearer which findings are:
- blocking,
- warning-level,
- informational only.

Goal:
- improve decision-making before report generation

## 3.4 Improve visual selector feedback
The visual selector is much stronger than before, but it could still communicate more clearly:
- how many wells are excluded per plate
- which selected plate becomes the primary one
- what multi-plate selection implies for downstream analysis

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

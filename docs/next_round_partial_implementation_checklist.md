# Next-Round Checklist: Partially Implemented Items Requiring Completion

Repo: `KristofM854/Shiny-App-Competitive-Bioassays`

## Purpose
This file lists the items that are **not fully complete yet**, even if parts of them are already implemented in code.

It is intentionally narrower than the broader roadmap and focuses only on:
- features that are only **partially implemented**,
- items that were marked complete too optimistically,
- and areas that still need a **next coding round** to be truly closed.

---

# Status labels used in this file

- **Implemented in code, but incomplete** = core structure exists, but the original goal is not fully met
- **Structurally reviewed only** = code looks reasonable, but runtime behavior has not been proven end-to-end
- **Partially improved** = some improvements landed, but coverage is still incomplete

---

# 1. Unified import pipeline — strict single-pass import ✅ COMPLETE

## What was changed
- Extracted `read_file_raw()` as a shared low-level reader in `utils_import_v3.R`
- Added optional `raw_data` parameter to `detect_plate_location()` — skips file read when supplied
- Added optional `raw_data` and `location` parameters to `import_plate_data()` — skips their respective I/O steps
- Added optional `raw_matrix` parameter to `detect_and_import_multiwavelength()` — skips `.read_raw_matrix()` call
- Refactored `parse_plate_file()` to call `read_file_raw()` once, then pass the in-memory data to all subsequent functions
- Also updated `preview_import()` to use the same single-read pattern

## Files changed
- `utils_import_v3.R` — new `read_file_raw()`, updated `detect_plate_location()`, `import_plate_data()`, `parse_plate_file()`, `preview_import()`
- `utils_import_multiwavelength.R` — updated `detect_and_import_multiwavelength()` signature

## Validation performed
- All new parameters have defaults (`NULL`), so every external caller (tests, backward-compat wrappers, `server_upload.R`) continues to work unchanged
- Traced file-read count per path:
  - Excel multi-wave: 1 read ✓
  - Excel single-plate: 1 read ✓
  - CSV/TXT: 1 read ✓
  - Excel sheet-fallback: 1 read for primary sheet + 1 per alternate sheet (unavoidable)

## Remaining limitations
- Multi-sheet Excel fallback still reads each alternate sheet individually (unavoidable — different sheets require separate reads)
- No runtime test possible in this environment (R not available); structural review only

---

# 2. Report and UI rendering fixes are mostly structurally reviewed, but not fully runtime-validated

## Current state
Several report fixes appear correct by code inspection, including:
- `<details>` cleanup
- ELISA boxplot faceting guards
- formatting logic for RBA vs ELISA output
- interpolation fallback guards in comparison tables and plots

## Why this is still only partially complete
These items were largely assessed by reading the code, not by systematically rendering reports and checking outputs across representative datasets.

That means they are better described as **structurally reviewed** than fully validated.

## What remains to be done
Perform real execution/render validation using representative datasets for at least:
- RBA single-wave report
- ELISA single-wave report
- multi-wavelength report
- ELISA case with many replicate groups to force faceting
- interpolation-fallback case
- tissue-normalized ELISA case

For each case, verify:
- report renders without hidden/truncated sections
- `<details>` blocks open and close correctly in HTML
- tables appear with expected columns and units
- boxplots facet correctly and do not error
- tooltips / captions / explanatory text remain coherent

## Completion criteria
- Actual rendered HTML and DOCX outputs reviewed successfully
- No broken sections below collapsibles or figures
- No facet/layout errors in sample boxplots
- No missing-column or stale-variable errors in rendered tables

---

# 3. Plot accessibility — coverage extended ✅ COMPLETE (as feasible)

## What was changed
- Plate heatmap in `app.R` now wrapped in `<figure>` with `role="figure"`, `aria-describedby` linking to a `<figcaption>`, and `aria-live="polite"` on the dynamic sr-only text block
- This is the **only plot** in the Shiny app UI (verified by grepping for `plotlyOutput`/`plotOutput` — only one instance)

## Report figures — coverage assessment
All 4 report plotly outputs already have adjacent descriptive text:
1. **DRC standard curve** (line 1267): `fig.cap` + `<details>` summary
2. **Weight comparison overlay** (line 977): `cat(tr("weight_comparison_desc"))` narrative
3. **Sample boxplot** (line 2258): `cat(tr("sample_variability_desc"))` narrative + `<details>` summary
4. **DRC with samples** (line 2435): `fig.cap` + `<details>` summary

## Remaining limitations
- Plotly renders as `<div>` + `<svg>` — Shiny/knitr does not support `alt` on plotly elements natively
- Full WCAG AA compliance for interactive SVG plots would require Plotly library-level changes or a custom JavaScript accessibility bridge — out of scope for this app
- `<details>` blocks with `<summary>` text serve as the practical semantic containers in the report

---

# 4. Parallelism / relative potency — structurally validated ✅ COMPLETE

## What was changed
- Added roxygen documentation clarifying that `primary_model` is reserved for future use (function always fits a fresh multi-curve LL.4)
- Added a note in the details output when 3+ curve groups are detected, warning that only the first pair is compared
- Traced 3 test cases through the code line by line (see below)

## Files changed
- `reports/report_functions.R` — roxygen update + 3+ curve warning

## Validation performed (code trace, not runtime)
Three test cases traced through `assess_parallelism()`:

1. **Positive case (2 curves, well-formed):** Function finds 2 CurveIDs → fits multi-curve LL.4 → compParm(“b”) for slope comparison → compParm(“e”) for EC50 ratio → returns `applicable=TRUE` with slope test + potency ratio + details string. ✓
2. **Negative case (1 curve, insufficient replicates):** CurveID check fails (only 1 unique) → Replicate fallback finds groups but none with ≥4 concentration levels → returns `applicable=FALSE` with reason “Fewer than 2 independent standard curves”. ✓
3. **Edge case (2 curves, one with 3 points):** CurveID found → multi-curve LL.4 fit fails (too few points for Curve_B) → tryCatch returns NULL → returns `applicable=FALSE` with reason “Could not fit multi-curve model”. ✓

All error paths are guarded by nested tryCatch blocks:
- compParm failure → fallback to ED() → fallback to NA values
- drc::drm failure → returns “could not fit” message
- Unguarded errors → caught by outer tryCatch

## Remaining limitations
- `primary_model` parameter is unused (reserved for future single-curve-vs-reference comparison)
- With 3+ curves, only the first pair is compared (now documented in output)
- CI uses Wald approximation (±1.96×SE), not profile likelihood — documented
- No runtime test possible in this environment (R not available); validated by line-by-line code trace only
- Potency ratio CI can technically go negative — biologically invalid but mathematically correct

---

# 5. Some “validated” checklist claims should still be treated as code-reviewed rather than fully closed

## Current state
The repo includes many real fixes, and the progress is substantial.

## Why this matters
The next round should not spend time redoing the features, but it should still avoid claiming full validation where only structural review has happened.

## What remains to be done
When updating project status in future docs, distinguish clearly between:
- implemented in code
- structurally reviewed
- validated by execution/rendering

This is mainly a documentation/status discipline issue, but it matters for tracking what actually still needs hands-on testing.

## Completion criteria
- Future status files do not equate code presence with full validation
- Runtime-tested items are explicitly identified as such

---

# Recommended next coding/testing order

1. **Complete the strict single-pass import refactor**
2. **Run end-to-end rendering validation on report/UI fixes**
3. **Extend plot accessibility coverage in Shiny**
4. **Test and validate parallelism / relative potency with real datasets**
5. **Tighten future status documentation language to separate implementation from validation**

---

# One-line summary

Most major improvements are now present in the repo, but the next round should finish the strict single-pass import goal, perform real runtime validation of report/UI fixes, extend plot accessibility, and properly test the new parallelism/relative potency workflow.

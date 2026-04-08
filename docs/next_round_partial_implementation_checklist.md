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

# 1. Unified import pipeline is implemented, but strict single-pass import is still incomplete

## Current state
A unified import entry point now exists via `parse_plate_file()`, and `server_upload.R` uses it for classic import mode.

This is a real improvement and should be kept.

## Why it is still only partially complete
The original target was a **strict single-read / single-detection pipeline**.

Current behavior still does:
1. try `detect_and_import_multiwavelength(...)` for Excel files,
2. and if that does not produce multi-wavelength output,
3. fall back to `import_plate_data(...)`, which re-reads and re-detects the file.

So the API is unified, but the physical import path is not yet truly single-pass in all cases.

## What remains to be done
- Refactor the Excel import path so a file is read once and routed through one shared detection result
- Avoid duplicate read/detect work between multi-wavelength and single-plate fallback paths
- Reuse the same parsed raw structure for both multi-wave and single-wave decisions
- Preserve the current normalized return object from `parse_plate_file()`

## Completion criteria
- One physical file read per classic import
- One detection workflow per import attempt
- No fallback path that restarts detection from scratch
- Same user-visible behavior as current implementation

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

# 3. Plot accessibility has improved, but is still only partially complete

## Current state
Accessibility support is better than before. The repo now includes:
- ARIA labels on matrix sections
- `aria-live` in important validation areas
- descriptive text for some figures
- a screen-reader-only description for the Shiny plate heatmap
- more narrative text in report sections

## Why this is still only partially complete
Plotly outputs in the Shiny app still do not have full accessibility coverage.
The current improvements help, but they do not amount to comprehensive accessible figure support.

## What remains to be done
- Add explicit descriptive companions for all major Shiny figures, not only some of them
- Wrap Plotly outputs in semantic containers where practical
- Link figures to descriptions via `aria-describedby` where feasible
- Ensure every major report/app figure has a nearby explanation of what the user should read from it
- Review whether key plots need hidden screen-reader text blocks

## Completion criteria
- All major Shiny plots have descriptive text associated with them
- Plotly outputs are wrapped/accessibly described as far as technically feasible
- No critical figure is left without explanation or alternative descriptive support

---

# 4. Parallelism / relative potency exists in code, but should still be considered only partly closed until tested

## Current state
A parallelism / relative potency section now exists in the report and appears sensibly guarded for non-applicable cases.

## Why this is still only partially complete
The presence of code is not enough here. This is a statistically sensitive feature and should be treated as incomplete until it is exercised with realistic compatible and incompatible datasets.

## What remains to be done
- Test the feature on a dataset with two genuinely comparable curves
- Test the feature on a dataset where it should correctly report “not applicable”
- Verify that slope comparison, EC50 ratio, and CI output are numerically sensible
- Confirm wording in the report remains clear when assumptions are not met

## Completion criteria
- Works correctly on at least one positive test case and one negative/not-applicable case
- No misleading output when curves are unsuitable for comparison
- Report wording remains scientifically defensible

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

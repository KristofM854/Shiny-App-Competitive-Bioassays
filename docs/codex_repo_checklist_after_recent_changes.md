# Codex Checklist After Recent Repo Changes

Repo: `KristofM854/Shiny-App-Competitive-Bioassays`

## Purpose
This file converts the current repo assessment into a clean implementation checklist for Codex.

It separates items into:
- **Implemented and looks correct**
- **Implemented but needs validation or cleanup**
- **Still missing from the roadmap**
- **Likely bug(s) to fix next**

Use this file as the source of truth for follow-up Codex work after the recent wave of changes.

---

# 1. Implemented and looks correct

These items are clearly present in the current repo and appear correctly integrated.

## Recent bug fixes / UX changes
- [x] Preset dropdown no longer resets to blank after load
- [x] ELISA control flattening bug fixed using `as.character(unlist(type_mat))`
- [x] Pre-flight tissue-weight crash guarded with proper `is.list()` handling
- [x] Top navigation buttons added to wizard steps
- [x] “Set all extraction volume” control added to ELISA tissue workflow
- [x] Notes moved to the Generate Report tab
- [x] Weighting overlay no longer calls `predict()` on `NULL` models
- [x] Comparison table no longer crashes on interpolation-fallback models with `NULL` coefficients
- [x] Collapsible report sections (`<details>`) are now present across the report template
- [x] AIC column added to weighting comparison table
- [x] AIC recommendation note added after weighting comparison table

## Roadmap items now implemented
- [x] Report-generation logic decomposed into staged helpers (`report_pipeline.R`)
- [x] Undo/redo support added for plate layout editing
- [x] Stronger pre-flight validation added
- [x] Weighting suitability assessment added to report
- [x] Model stability / convergence assessment added to report
- [x] Layered uncertainty reporting added (model / replicate / combined)
- [x] CI truncation is now tracked and disclosed
- [x] Interpolation vs extrapolation labels added to sample quantification
- [x] Assay-specific QC profiles added to report interpretation/QC logic
- [x] Exclusion audit table added
- [x] Executive report summary added near the top of the report

---

# 2. Implemented but needs validation or cleanup

These items appear in code, but should not be considered fully closed without targeted testing or cleanup.

## Recent items from the 18-point change summary
- [ ] ELISA boxplot facet fix: present in reworked boxplot code, but still needs runtime validation with real ELISA datasets
- [ ] HTML / `<details>` block cleanup: structure looks much better, but should be verified with actual rendered HTML report output
- [ ] Scientific notation / display formatting: logic is present, but should be validated across RBA and ELISA edge cases
- [ ] Column 12 ELISA preset fix: column 12 is present in `utils_plate.R`, but preset generation files and preset objects should still be verified end-to-end
- [ ] `%B/B0` translation fix in `i18n.R`: not yet re-checked directly in rendered report output

## Larger roadmap items now present in code but still need validation
- [ ] Parallelism / relative potency module exists in the report, but needs validation on suitable datasets
- [ ] Tissue normalization traceability section exists, but needs consistency checks against output variables and units
- [ ] Plot accessibility is improved, but still only partial; verify screen-reader text / descriptions actually cover all critical figures

---

# 3. Still missing from the roadmap

These are the main components from the earlier roadmap that still appear unimplemented or not yet evidenced in the repo.

## High / medium priority items still outstanding
- [ ] **Single-pass multi-wavelength import pipeline**
  - Current state: still no clear evidence that classic import was unified into one parse call
  - Needed: one function that detects, parses, annotates, and returns normalized import output for both single- and multi-wavelength files

- [ ] **Stable visual plate selector state model**
  - Current state: not addressed by the recent change set
  - Needed: stable plate identities based on file + position + label, not fragile UI order

- [ ] **Cached visual plate preview / selector preprocessing**
  - Current state: not evidenced in the recent change set
  - Needed: avoid expensive recomputation in preview / selector rendering

- [ ] **Multi-wavelength bias visualization**
  - Current state: not confirmed
  - Needed: visual diagnostics such as Bland–Altman or equivalent bias plots in the multi-wavelength report

## Partially addressed but not complete
- [ ] **Plot accessibility and descriptive figure text — complete coverage**
  - Current state: some descriptive text is present (e.g. heatmap description), but this should be extended consistently across major app/report figures

---

# 4. Likely bug(s) to fix next

These are issues that appear likely from static inspection and should be treated as high-priority cleanup.

## Tissue concentration variable mismatch
- [ ] Fix inconsistent tissue concentration variable naming in the report
  - Likely issue: one section still refers to `concentration_ng_per_g` while the pipeline computes `concentration_pg_per_g`
  - Risk: broken tissue-result display or silent missing values
  - Action:
    - standardize on one unit variable name
    - confirm displayed table units match the underlying computation
    - verify CSV exports and report tables use the same variable consistently

## Tissue units consistency audit
- [ ] Audit all tissue-related outputs for `pg/g` vs `ng/g` consistency
  - Check:
    - summary table labels
    - detailed results table labels
    - traceability section formula and narrative text
    - CSV exports

---

# 5. Codex priority order from here

## Immediate next fixes
1. [ ] Fix tissue concentration variable mismatch and unit consistency
2. [ ] Validate ELISA boxplot/faceting with real data
3. [ ] Validate rendered HTML report structure after all `<details>` changes
4. [ ] Verify ELISA preset column-12 behavior end-to-end using actual preset load

## Next implementation wave
5. [ ] Implement single-pass multi-wavelength import pipeline
6. [ ] Stabilize visual plate selector state model
7. [ ] Add caching for visual preview / selector preprocessing
8. [ ] Add multi-wavelength bias visualization

## Final polish
9. [ ] Extend plot accessibility/descriptive text across all major figures
10. [ ] Re-check `%B/B0` translation rendering and formatting consistency

---

# 6. Minimal acceptance criteria for Codex

## Tissue-output cleanup
- [ ] One canonical tissue concentration variable name is used everywhere
- [ ] Report tables, text, and exports agree on units (`pg/g` or `ng/g`, but not both inconsistently)
- [ ] ELISA tissue-result sections render without missing-column errors

## Report validation
- [ ] HTML report renders fully with no hidden/truncated sections
- [ ] `<details>` sections open/close correctly
- [ ] Weighting comparison table works when one or more models are interpolation fallbacks
- [ ] ELISA sample boxplot renders correctly for many groups and faceted layouts

## Preset validation
- [ ] ELISA presets populate all 12 columns correctly
- [ ] Column 12 sample/replicate assignments survive preset load, editing, and report generation

## Outstanding roadmap tasks
- [ ] Single-wave and multi-wave import both work through the unified parser
- [ ] Visual selector state remains stable across re-rendering
- [ ] Multi-wavelength report includes bias visualization and does not crash on sparse data

---

# 7. One-line summary for Codex

Most of the recent changes are present and several roadmap items have now been implemented, but the repo still needs: (1) cleanup of likely tissue-output inconsistencies, (2) validation of several new report/UI changes, and (3) completion of the still-missing import, selector-state, caching, and multi-wavelength bias-visualization work.

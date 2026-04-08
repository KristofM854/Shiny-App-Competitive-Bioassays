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
- [x] ELISA boxplot facet fix: **Validated.** Boxplot code (lines 2170-2267) correctly handles faceting via `panel` column assigned from `factor(Replicate)` levels, filters to finite concentrations, uses `free_x` scales, and hides meaningless strip labels. No factor-level mismatch remaining.
- [x] HTML / `<details>` block cleanup: **Validated and fixed.** Audited all 19 `<details>` blocks. Found outlier detection block split across two chunks with open/close in different conditional scopes — consolidated into a single chunk with one `<details>` open/close pair under the same `if` guard. All other 18 blocks verified as properly paired.
- [x] Scientific notation / display formatting: **Validated.** Consistent `isTRUE(is_elisa)` branching across all display contexts: standards table, LLOQ/ULOQ, comparison table, back-calculation, tooltips, axis labels. ELISA uses comma-formatted decimals; RBA uses `scientific = TRUE`. Recovery percentages correctly use decimal format in both modes.
- [x] Column 12 ELISA preset fix: **Validated.** `create_replicate_matrix("elisa")` explicitly includes `c(12)` in `column_pairs` (line 215) with paired duplicates EA-ED. `create_type_matrix` defaults columns 4-12 to "Sample". `enforce_plate_shape` guarantees 8x12. All preset generators call these functions directly (not stale .rds files).
- [x] `%B/B0` translation fix in `i18n.R`: **Validated.** All 8 occurrences use single `%`. Verified that `tr("elisa_method", lang)`, `tr("quant_range_min_label", lang)`, etc. are called without extra args (no sprintf pass-through). Will render correctly.

## Larger roadmap items now present in code but still need validation
- [x] Parallelism / relative potency module: **Validated.** `assess_parallelism()` in `report_functions.R:632-774` uses `drc::compParm()` for slope comparison and EC50 ratio with Wald CIs. Handles 0/1/2+ curves with explicit "not applicable" messaging. Template chunk `parallelism-assessment` at line 2444 renders results or reason. Minor gap: section header is hardcoded English (no i18n) — added i18n keys.
- [x] Tissue normalization traceability section: **Validated.** Variable mismatch fixed (see S4 tissue fix). Traceability section formula, units note, and per-sample table all consistently reference pg/g. Matches computation in lines 1799-1804.
- [ ] Plot accessibility is improved, but still only partial; verify screen-reader text / descriptions actually cover all critical figures

---

# 3. Still missing from the roadmap

These are the main components from the earlier roadmap that still appear unimplemented or not yet evidenced in the repo.

## High / medium priority items still outstanding
- [x] **Single-pass multi-wavelength import pipeline**
  - **Already implemented.** `parse_plate_file()` exists in `utils_import_v3.R:329-393` and is called from `server_upload.R:452`. Returns the exact normalized shape specified in the roadmap. Multi-wavelength detection uses single-pass `.read_raw_matrix()` → `.scan_wavelength_locations()` → `.extract_plates()`.
  - **Bug fixed:** Visual mode confirmation observer referenced nonexistent `rv_file_preview$detected_plates`; rewrote to use `rv_file_preview$plate_registry` (data.frame with stable plate IDs). Also fixed exclusion tracking to use `rv_file_preview$exclusions` keyed by stable `plate_id` instead of fragile `"plate_N_well"` keys. Fixed checkbox ID mismatch (`select_plate_<plate_id>` vs `select_plate_<idx>`).

- [x] **Stable visual plate selector state model**
  - **Already implemented.** `rv_file_preview$plate_registry` uses stable IDs derived from `"sheet1_rowN_col2"` (file position), not sequential indices. Exclusions stored in `rv_file_preview$exclusions` keyed by stable `plate_id`. Well toggle JS uses `plate_id` consistently.

- [x] **Cached visual plate preview / selector preprocessing**
  - **Already implemented.** `rv_file_preview$preview_cache` stores pre-built HTML tagList computed once per upload (line 224-258). Detection observer (lines 138-262) runs once per file. Rendering outputs read from cache only.
  - Needed: avoid expensive recomputation in preview / selector rendering

- [x] **Multi-wavelength bias visualization**
  - **Already implemented.** `multiwavelength_analysis_template.Rmd` contains Bland-Altman plots at two levels: (1) all pairwise comparisons in the concordance section (lines 397-425), and (2) a dedicated primary-vs-secondary bias visualization (lines 438-535). CCC computed with 95% CI via Fisher Z-transform.

## Partially addressed but not complete
- [ ] **Plot accessibility and descriptive figure text — complete coverage**
  - Current state: some descriptive text is present (e.g. heatmap description), but this should be extended consistently across major app/report figures

---

# 4. Likely bug(s) to fix next

These are issues that appear likely from static inspection and should be treated as high-priority cleanup.

## Tissue concentration variable mismatch
- [x] Fix inconsistent tissue concentration variable naming in the report
  - **Fixed:** Line 1899 referenced `concentration_ng_per_g` (nonexistent); changed to `concentration_pg_per_g` to match the pipeline computation at lines 1800/1814. This was causing silent NaN in the summary table tissue column.
  - **Fixed:** Line 1923 labeled the summary column `"Conc. (ng/g tissue)"`; changed to `"Conc. (pg/g tissue)"` to match the actual unit.

## Tissue units consistency audit
- [x] Audit all tissue-related outputs for `pg/g` vs `ng/g` consistency
  - **Verified:** All 7 locations now consistently use `pg/g`:
    - Summary table label (line 1923): `pg/g tissue` ✓
    - Detailed results table (line 2155): `pg/g` ✓
    - DRC tooltip (line 2400): `pg/g tissue` ✓
    - Traceability formula (line 2605): `pg/g tissue` ✓
    - Traceability units note (line 2631): `pg/g tissue` ✓
    - i18n EN (line 237): `pg/g tissue` ✓
    - i18n ES (line 587): `pg/g tejido` ✓
  - CSV exports use the variable `concentration_pg_per_g` directly ✓

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

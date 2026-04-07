# Codex Implementation Roadmap

Repo: `KristofM854/Shiny-App-Competitive-Bioassays`

## Objective
Improve:
1. computing performance,
2. Shiny app usability,
3. robustness of statistical analysis,
4. informativeness and decision value of the report.

## Ground rules for Codex
- Do **not** re-implement items already solved:
  - vectorized dilution parsing
  - debounce on dilution matrix edits
  - cached layout reuse for multi-wavelength conversion
  - debounced standards reactive
  - ARIA labels already present on matrix wrappers
  - `aria-live` already present for key validation regions
  - spinners already present on main matrix tables
  - CSV template download already present
  - modular split from monolithic `app.R` already done
  - weighted R² fix already done
  - lack-of-fit test already present
  - Shapiro/MAD fallback already present
  - configurable CV threshold already present
  - report version info already present
  - interpretation section already present
  - excluded-well/outlier summary already present
  - plate positional QC already present
  - visible back-calculation section already present

- Prefer **small, testable PRs** over one giant rewrite.
- Preserve backward compatibility for saved outputs wherever feasible.
- Avoid introducing asynchronous complexity unless it clearly improves user experience and failure handling.
- Keep current report outputs reproducible and traceable.

---

# Priority roadmap

## HIGH PRIORITY 1 — Single-pass multi-wavelength import pipeline
### Why
Current classic upload mode likely scans the same file multiple times:
- detect if multi-wavelength
- import all wavelength plates
- inspect imported metadata afterward

This is redundant and will scale poorly with larger Excel files.

### Current likely touchpoints
- `server_upload.R`
- `utils_import_multiwavelength.R`
- possibly `utils_import_v3.R`

### Required change
Refactor the multi-wavelength import API so one function does all of this in one pass:
- detect whether file is multi-wavelength
- parse wavelength labels
- extract plate matrices
- attach import metadata
- return a normalized object consumed directly by `server_upload.R`

### Target API
Create or refactor toward something like:

```r
parse_plate_file(file_path)
```

Return shape:

```r
list(
  is_multiwavelength = TRUE/FALSE,
  format = "Excel" / "CSV" / "TXT",
  detected_wells = ...,
  partial_plate = TRUE/FALSE,
  wavelengths = c(...),          # NULL for single-wave
  plates = list(...),            # one plate for single-wave, named list for multi-wave
  import_info = list(...)
)
```

### Required server changes
In `server_upload.R`, replace the current branch logic with one call:
- call `parse_plate_file(file_path)`
- set `shared$rv$is_multiwavelength`
- set `shared$rv$wavelength_plates`
- set `shared$rv$wavelengths`
- set `shared$matrix_measresults`
- show one unified notification

### Acceptance criteria
- No separate “detect then import” pass in classic upload mode.
- Single-wavelength and multi-wavelength files both work through one API.
- Existing user-visible behavior remains the same or better.
- Import notifications still report wells detected, format, and partial/full plate status.
- Multi-wavelength output remains compatible with current report generation.

### Suggested PR title
`refactor: unify plate import into single-pass parser`

---

## HIGH PRIORITY 2 — Decompose or background report generation
### Why
Report generation still runs as one large blocking `observeEvent(input$convert, ...)`. This is the biggest remaining performance and UX bottleneck.

### Current likely touchpoints
- `server_report.R`
- possibly helper files under `reports/`
- maybe add a new helper like `report_pipeline.R`

### Required change
Split the current report-generation observer into distinct stages with clean helpers:
1. flush pending UI state
2. construct long-format data
3. normalize / assay-specific transform
4. persist CSV + config JSONs
5. render one or more reports
6. collect results and show final status

At minimum, refactor into functions.  
Preferred next step: use `eventReactive()` or an explicit pipeline object so the observer becomes orchestration only.

### Stretch goal
If stable in your environment, evaluate `future`/`promises` for report rendering only.  
But only do this if:
- error handling remains clean,
- path/session handling remains safe,
- download behavior remains predictable.

If async adds fragility, stop at structural decomposition.

### Suggested helper functions
- `flush_latest_layout_state(...)`
- `build_long_data(...)`
- `normalize_assay_data(...)`
- `save_analysis_artifacts(...)`
- `render_reports(...)`

### Acceptance criteria
- `server_report.R` no longer contains one massive all-in-one export block.
- Failures in one stage are clearly attributed.
- Rendering multiple formats uses shared precomputed inputs rather than rebuilding work.
- Existing outputs remain unchanged unless intentionally improved.
- UI still gives progress feedback.

### Suggested PR title
`refactor: split report generation into staged pipeline`

---

## HIGH PRIORITY 3 — Formal homoscedasticity and weighting guidance
### Why
Weighted regression is supported, but weighting choice is still mostly user-driven and only lightly guided by a variance-ratio heuristic. Statistical defensibility would improve with more formal diagnostics.

### Current likely touchpoints
- `reports/unified_analysis_template.Rmd`
- `reports/report_functions.R`
- perhaps new helper functions in `reports/`

### Required change
Add a formal variance diagnostic to guide weighting recommendation.  
Suggested order of preference:
1. Brown–Forsythe / Levene-type test on grouped residual magnitudes by standard concentration level
2. fallback to variance-ratio heuristic if formal test is not reliable due to sparse levels

### Desired behavior
- Compute heteroscedasticity diagnostics for the unweighted model.
- Report:
  - test/statistic
  - p-value if available
  - practical interpretation
  - recommendation: unweighted vs weighted preferred
- Do **not** auto-switch the selected primary model silently.
- Do present the recommendation clearly.

### Acceptance criteria
- Report contains a dedicated weighting suitability subsection.
- User can understand why 1/Y or 1/Y² might be preferable.
- Sparse-data cases degrade gracefully without crashing.

### Suggested PR title
`feat: add formal heteroscedasticity diagnostics for weighting guidance`

---

## HIGH PRIORITY 4 — Deeper convergence diagnostics for DRC fitting
### Why
Current convergence reporting is still fairly shallow. It checks standard errors and surfaces a warning, but does not give enough information about fit stability.

### Current likely touchpoints
- `reports/unified_analysis_template.Rmd`
- `reports/report_functions.R`

### Required change
Expand model diagnostics captured per fitted weighting:
- convergence status
- parameter standard error availability
- suspicious parameter magnitudes
- boundary-hitting behavior for bounded ELISA fits
- warnings/errors captured during fitting
- optionally condition-like diagnostics if available from the fit object or vcov

### Desired output
For each fitted model, record something like:

```r
list(
  converged = TRUE/FALSE/NA,
  fit_method = "LL.4" / "LL.3" / "interpolation",
  se_available = TRUE/FALSE,
  boundary_flag = TRUE/FALSE,
  suspicious_params = c(...),
  warnings = c(...)
)
```

### Acceptance criteria
- QC/report explicitly distinguishes:
  - good fit
  - fit succeeded but unstable
  - fallback fit used
  - interpolation fallback used
- Executive summary and methods/QC tables reflect this clearly.

### Suggested PR title
`feat: strengthen convergence and model stability diagnostics`

---

## HIGH PRIORITY 5 — Parallelism / relative potency module
### Why
This is a meaningful scientific gap for assay interpretation and method comparison, especially where users compare standards or assay runs.

### Current likely touchpoints
- `reports/unified_analysis_template.Rmd`
- `reports/report_functions.R`
- possibly app settings if you want this user-configurable later

### Required change
Add optional relative-potency / parallelism analysis for suitable cases.

### Scope for first implementation
- Only activate when there are two valid standard-like curves or explicitly comparable grouped curves.
- Start with a report-side capability, not necessarily a UI workflow.
- Implement:
  - common-slope / parallelism test where feasible
  - EC50 ratio (or potency ratio)
  - CI if feasible
  - clear statement when conditions are not met

### Acceptance criteria
- Feature is off or hidden unless data structure supports it.
- No crashes when unsuitable data are present.
- Report contains a short “not applicable” note when analysis cannot be performed.

### Suggested PR title
`feat: add optional parallelism and relative potency analysis`

---

## HIGH PRIORITY 6 — Replace heuristic replicate/model CI fusion with clearer uncertainty logic
### Why
Current CI aggregation is better than before but still heuristic: replicate variability and per-well model uncertainty are combined by taking wider bounds. That is conservative, but methodologically rough.

### Current likely touchpoints
- `reports/unified_analysis_template.Rmd`
- maybe `reports/report_functions.R`

### Required change
Refactor uncertainty reporting into explicit layers:
1. per-well inverse-prediction uncertainty from model
2. replicate dispersion across wells
3. final replicate-group interval with documented rule

### Recommended first implementation
Do not jump straight to a heavy hierarchical model.  
Instead:
- compute and report **two distinct uncertainties**:
  - model-based inverse prediction interval
  - replicate dispersion interval
- optionally provide a combined conservative interval, but label it clearly as conservative combined uncertainty

### Reporting language
Avoid implying a mathematically exact propagated CI if it is still heuristic.

### Acceptance criteria
- Report explicitly distinguishes model uncertainty vs replicate variability.
- Final reported interval wording matches actual method used.
- No silent clipping or opaque combination rule.

### Suggested PR title
`refactor: separate model and replicate uncertainty in sample CI reporting`

---

## HIGH PRIORITY 7 — Fix CI floor handling and make truncation explicit
### Why
`pmax(ci_lower, 0)` is still present. For concentration-like outcomes that may be practically reasonable, but it should be explicit and better handled.

### Current likely touchpoints
- `reports/unified_analysis_template.Rmd`

### Required change
Refactor CI handling so:
- raw interval is retained internally
- displayed interval may be truncated at 0 if required for domain realism
- report explicitly labels the displayed interval as truncated when truncation happened

### Acceptance criteria
- No silent truncation.
- Report footnote or inline marker makes truncation explicit.
- Exported summary file should ideally contain both raw and displayed CI bounds.

### Suggested PR title
`fix: make lower-bound CI truncation explicit and auditable`

---

## HIGH PRIORITY 8 — Undo/redo stack for plate layout editing
### Why
This is the most valuable remaining usability upgrade for a matrix-heavy Shiny workflow.

### Current likely touchpoints
- `server_layout.R`
- maybe a new helper file like `layout_history.R`
- possibly UI additions in `app.R`

### Required change
Implement lightweight state history for:
- `matrix_type`
- `matrix_id`
- `raw_matrix_dilution`
- `matrix_replicate`

### Suggested design
Use a reactive history stack with:
- `undo`
- `redo`
- max stack size, e.g. 20 or 30 states

Snapshot state only when matrix edits change, not on every unrelated reactive invalidation.

### UI
Add buttons near layout controls:
- Undo
- Redo

Disable appropriately when unavailable.

### Acceptance criteria
- User can undo and redo plate-layout edits reliably.
- Loading a preset or imported layout pushes a new history state.
- Uniform dilution apply/reset actions also participate coherently.
- No runaway memory growth.

### Suggested PR title
`feat: add undo and redo support for plate layout editing`

---

## HIGH PRIORITY 9 — Plot accessibility and descriptive figure text
### Why
ARIA on matrices is already better, but plots are still weak for accessibility.

### Current likely touchpoints
- `app.R`
- `server_upload.R`
- `reports/unified_analysis_template.Rmd`
- maybe helper text functions

### Required change
Add accessible descriptions for:
- plate heatmap in app
- major report figures
- dose-response plots
- sample variability plot

### Practical implementation
In Shiny:
- add nearby descriptive text outputs tied to the figure content
- optionally add hidden screen-reader text blocks

In report:
- add short narrative captions that explain what the figure shows and what to look for

Do not chase perfect ARIA for plotly if that becomes brittle; useful textual alternatives are acceptable.

### Acceptance criteria
- Each major plot has a concise human-readable description.
- Description updates with assay context where relevant.
- No purely decorative chart appears without nearby explanatory text.

### Suggested PR title
`feat: add accessible descriptive text for plots and heatmaps`

---

# MEDIUM PRIORITY — New suggestions from the deep dive

## MEDIUM 1 — Stronger pre-flight validation beyond presence checks
### Why
Current pre-flight checks presence, but not deeper consistency or plausibility.

### Add checks for
- standards count and duplication consistency
- replicate-group consistency
- empty sample IDs where sample wells exist
- ELISA control count plausibility
- suspicious control hierarchy before report generation
- missing tissue-weight metadata when tissue normalization is expected

### Acceptance criteria
- Pre-flight panel surfaces actionable warnings, not just red/green presence states.
- Convert remains allowed only when truly critical failures exist; noncritical issues remain warnings.

### Suggested PR title
`feat: expand preflight validation with structural and plausibility checks`

---

## MEDIUM 2 — Stable state model for visual plate selector
### Why
The visual selector works, but the exclusion state looks fragile and tied to detection order.

### Required change
Refactor selector state so each detected plate has a stable identity:
- derived from file + position + label
- not just sequential index

Persist exclusion state under stable plate IDs.

### Acceptance criteria
- Re-rendering does not accidentally scramble exclusions.
- Plate state remains stable if UI invalidates.
- Code is easier to reason about.

### Suggested PR title
`refactor: stabilize visual plate selector state and exclusion tracking`

---

## MEDIUM 3 — Cache preprocessed visual preview data
### Why
The visual selector likely rebuilds expensive HTML/UI structures too often.

### Required change
Precompute:
- raw matrix
- detected plate ranges
- rendered preview metadata

Store lightweight derived structures in reactive state rather than recomputing them inside `renderUI()`.

### Acceptance criteria
- Visual mode feels snappier on large files.
- Detection runs once per upload unless file changes.
- UI invalidations do not reread raw file content unnecessarily.

### Suggested PR title
`perf: cache derived data for visual plate preview`

---

## MEDIUM 4 — Front-page executive report summary
### Why
The report is informative, but busy readers still need a short one-page answer.

### Required change
Add a front summary block near the top with:
- assay and analyte
- model used
- fit quality
- standards used / excluded
- LLOQ/ULOQ
- number of quantified samples
- overall pass / qualified pass / review required
- top 2–4 key warnings

### Acceptance criteria
- A reader can understand assay status in under 30 seconds.
- This summary does not replace the deeper interpretation section; it complements it.

### Suggested PR title
`feat: add concise front-page executive summary to report`

---

## MEDIUM 5 — Formal exclusion audit table
### Why
Exclusions are mentioned, but not yet fully centralized.

### Required change
Add a formal table listing:
- excluded well
- reason
- stage of exclusion
- user-excluded vs import-missing vs non-finite vs outlier-flagged

### Acceptance criteria
- Exclusions are auditable in one place.
- Table is included in report and optionally exported as CSV.

### Suggested PR title
`feat: add formal exclusion audit table and export`

---

## MEDIUM 6 — Explicit interpolation vs extrapolation labels in sample results
### Why
Range flags exist, but assay users need this distinction to jump off the page.

### Required change
For each quantified result, classify:
- interpolated within validated range
- extrapolated below range
- extrapolated above range
- uncertain / not estimable

### Acceptance criteria
- Summary tables and detailed results both expose this clearly.
- Interpretation section counts and comments on extrapolated results.

### Suggested PR title
`feat: label interpolation and extrapolation status explicitly in sample results`

---

## MEDIUM 7 — Assay-specific QC rule profiles
### Why
Current interpretation is generic. RBA and ELISA often deserve different thresholds and logic.

### Required change
Introduce QC rule profiles by assay type, possibly configurable:
- R² threshold
- CV thresholds
- control hierarchy expectations
- acceptable recovery ranges
- Hill slope expectations

### Acceptance criteria
- Interpretation uses assay-aware rules.
- Defaults remain sensible and transparent.
- Existing behavior stays close to current defaults unless intentionally tuned.

### Suggested PR title
`feat: introduce assay-specific QC rule profiles`

---

## MEDIUM 8 — Tissue-normalization method traceability in report
### Why
ELISA tissue normalization is scientifically important and should be easier to audit.

### Required change
Add a short subsection describing:
- tissue mass used
- extraction volume used
- conversion chain from concentration in extract to mass-normalized value
- units used

### Acceptance criteria
- A reviewer can reconstruct the tissue normalization logic from the report alone.
- No ambiguity between pg/g and ng/g conversions.

### Suggested PR title
`feat: document tissue normalization workflow explicitly in report`

---

## MEDIUM 9 — Multi-wavelength bias visualization
### Why
Concordance metrics are good, but a visual bias summary would be much more informative.

### Current likely touchpoints
- `reports/multiwavelength_analysis_template.Rmd`
- supporting plot helpers

### Required change
Add one or more:
- Bland–Altman plot
- wavelength-vs-wavelength difference plot
- per-sample bias summary

### Acceptance criteria
- Visual comparison complements CCC/text summary.
- Missing or sparse wavelength data do not crash plotting.
- Plot captions explain what bias patterns would matter.

### Suggested PR title
`feat: add visual bias diagnostics to multi-wavelength report`

---

# Suggested implementation order for Codex

## Phase 1 — highest leverage
1. Single-pass multi-wavelength import
2. Report-generation pipeline decomposition
3. Stronger weighting diagnostics
4. Deeper convergence diagnostics

## Phase 2 — statistical trustworthiness
5. Uncertainty reporting refactor
6. Explicit CI truncation handling
7. Parallelism / relative potency module

## Phase 3 — usability and accessibility
8. Undo/redo stack for plate layout
9. Plot accessibility text
10. Expanded pre-flight validation
11. Visual selector state stabilization
12. Visual preview caching

## Phase 4 — reporting polish and auditability
13. Front-page executive summary
14. Formal exclusion audit table
15. Explicit interpolation/extrapolation labels
16. Assay-specific QC rule profiles
17. Tissue-normalization traceability
18. Multi-wavelength bias visualization

---

# Codex work format requested

For each PR, Codex should provide:
1. a short problem statement,
2. exact files changed,
3. implementation summary,
4. backward-compatibility note,
5. acceptance checks performed,
6. any remaining limitations.

---

# Minimal acceptance test checklist for Codex across the roadmap

## Import / app behavior
- Single-wave CSV/TXT/XLSX still imports correctly
- Multi-wavelength XLSX still imports correctly
- Partial plate imports still work
- Visual selector still works with exclusions
- Undo/redo behaves predictably across reset, preset load, import, and manual edit

## Report generation
- HTML and DOCX reports both still render
- Single-wave and multi-wave report paths still work
- Errors in one report format do not silently corrupt others
- Existing JSON/CSV outputs remain readable

## Statistical outputs
- Weighted/unweighted comparisons still render
- Lack-of-fit still works
- Shapiro/MAD fallback still works
- Sample results do not crash when all values are identical, sparse, or out of range
- Extrapolated results are labeled correctly

## Report clarity
- Executive summary appears at top
- Exclusion audit table present when relevant
- Tissue-normalization explanation appears only when applicable
- Multi-wavelength bias plots render only when applicable

---

# One-line summary for Codex
Prioritize the remaining real gaps: single-pass import, decomposed/non-blocking report generation, stronger weighting and convergence diagnostics, clearer uncertainty handling, undo/redo, and better report auditability; do not spend time re-solving items already fixed.

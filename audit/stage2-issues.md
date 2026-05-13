# Stage 2 — Audit Findings
## Competitive Binding Assay Analysis Suite (RBAElisaApp), pre-1.0.0 JOSS readiness
**Audit date:** 2026-05-13
**Auditor:** Claude (static read-only pass, no code changes)
**References Stage 1 inventory:** `audit/stage1-inventory.md`

Severity definitions
- **Critical** — produces silently incorrect scientific output, data loss, or crash on common input.
- **High** — silently wrong behavior on plausible input, reproducibility failure, or JOSS blocker.
- **Medium** — brittle but recoverable, poor UX that affects trust.
- **Low** — code smell, minor doc gap, future maintainability.

Findings are sorted Critical → High → Medium → Low, then by category.

---

## CRITICAL

---
**Issue ID**: AUDIT-001
**Category**: A. Statistical correctness
**Severity**: Critical
**Location**: `reports/analysis_pipeline.R:448–479` (storage of `interp_fun`), `reports/analysis_pipeline.R:589–664` (`quantify_samples` per-well prediction)
**Title**: Log-linear interpolation fallback never quantifies samples — silent NA output for all unknowns
**Evidence**: When both LL.4 and LL.3 `drc::drm` calls fail, `fit_all_models()` stores an `approxfun()`-based interpolator under `all_models[[wt_key]]$interp_fun` and sets `model = NULL`. The primary model becomes `NULL` (`analysis_pipeline.R:494`). `quantify_samples()` is then called with `model_fit = NULL` from the chunk env (`unified_analysis_template.Rmd:1300`). For every well it executes `ED(model_fit, respLev = response_value, ...)` (`analysis_pipeline.R:628`); `drc::ED(NULL, ...)` throws an error, which is caught by the per-well `tryCatch` and silently returns a row of `NA_real_` (`analysis_pipeline.R:648–662`). The stored `interp_fun` is never invoked. The Rmd globally suppresses warnings (`knitr::opts_chunk$set(warning = FALSE)`, `unified_analysis_template.Rmd:84`), so the `warning("Using log-linear interpolation fallback ...")` (`analysis_pipeline.R:460`) is not visible in the rendered report.

Reproducer: any standards subset where 4PL convergence fails twice (e.g. 3 unique concentrations with nominal but identical replicates, or all-flat response with `cv_limit` filtering all standards). All sample concentrations become NA, replicate summaries show "Not estimable", but the report renders normally and the only loud breadcrumb is "log-linear interpolation" inside the `weight_method` field of the model-stats section.

**Impact**: A user uploads a degraded plate, the app renders a polished report, and every sample concentration is silently NA. Worse, what Stage 1 called a "linear" fallback is actually log-linear interpolation via `approxfun(log10(concentration), response)` — this should be labeled accurately for the user and in any publication.

**Recommended fix**: Either (a) wire `quantify_samples()` to call `all_models[[primary_key]]$interp_fun` and its inverse for sample back-calculation when `model_fit` is NULL, returning concentrations with a "Interpolated (no parametric fit)" status flag and an inflated "interpolation-uncertainty" CI; OR (b) remove the interpolation fallback entirely and fail loudly with an `showNotification(..., type = "error")` when both LL.4 and LL.3 fail, so the user is forced to fix the upstream data or analysis settings. Option (b) is recommended for v1.0.0 because deriving meaningful CIs from `approxfun()` interpolation is non-trivial and would itself need validation. Relabel any user-facing reference from "log-linear" to "log-linear interpolation" so the distinction from a parametric linear fit is explicit.

**Regression test needed**: yes — `test-analysis-pipeline.R` should add a case where both LL.4 and LL.3 are forced to fail (e.g. by passing a degenerate `standards_for_model` with all-equal responses) and assert the chosen failure mode: either (a) sample concentrations are computed from `interp_fun` and `quantification_status` is "Interpolated", or (b) `quantify_samples()` returns an explicit failure flag and the orchestrator triggers `showNotification(..., type = "error")`.

---
**Issue ID**: AUDIT-002
**Category**: A. Statistical correctness
**Severity**: Critical
**Location**: `tests/testthat/test-report-numbers.R:37–39`
**Title**: Golden-number regression test ships with placeholder expected values
**Evidence**: `expected_r2 <- 0.998   # placeholder per implementation plan §B6` and `expected_ic50 <- 5.2e-9   # placeholder per implementation plan §B6`. The comment block at the top of the file (lines 9–28) acknowledges these are placeholders to be filled in via `capture_reference_values()`, but the values have not been captured. There is also a probable parameter-name bug at line 66: the test passes `std_concentrations = DEFAULT_STX_CONC[1:n_std]` but `matrix_to_long()` declares the parameter as `std_conc` (`utils_plate.R:382`), and R partial matching does not match a longer supplied name to a shorter declared name — the call should error with "unused argument: std_concentrations". The test must either be silently passing because CI skips it, or because the placeholders happen to land within the tolerance window by coincidence.

**Impact**: The single most important regression test in the suite is non-functional. Any future change that silently breaks the RBA fitting numerics — exactly what the test exists to catch — would not be flagged.

**Recommended fix**: Run the test once interactively (or via the `capture_reference_values()` helper) on a known-good build, paste the actual `r_squared`, `ic50`, and `n` into the constants, and rename the `std_concentrations` argument to `std_conc`. Add an `expect_no_error()` around the render call so future signature-mismatch bugs surface immediately.

**Regression test needed**: yes — the fix is the test itself; it must be made functional and the captured values must be persisted with a comment recording the git SHA at which they were captured.

---
**Issue ID**: AUDIT-003
**Category**: A. Statistical correctness / G. Test coverage
**Severity**: Critical
**Location**: `tests/testthat/` (absence)
**Title**: No regression tests for the seven previously fixed bugs called out in the Stage 1 brief
**Evidence**: Stage 1 §11 noted that the suite has 13 test files for ~18,600 LoC. A search of `tests/testthat/` for tests named for or asserting on the previously fixed bug areas (EC20/EC80 swap, DRC weight vector mismatch, attribute stripping, process-global `RBA_OUTPUT_DIR` assignment, dilution >1 warning, BUG_008 `mean_sample_cv` computation, env-handoff for KPI stats) returns no matches beyond `test-stats-config.R:14–15`, which only asserts the *constant* values `STATS_CONFIG$ec20_resp_level == 80` and `$ec80_resp_level == 20` — not that the EC values produced from a model fit are correctly assigned. The bug fix commits exist (`git log --grep="fix" | head -40` returns dozens of entries with messages like `fix: env-handoff for KPI stats`, `fix(report): correct stale column names ...`, `fix(deps): harden package loading ...`) but none have a paired test file.

**Impact**: The whole point of regression tests is to prevent re-introducing bugs. Without them, the same seven bugs (and likely several more) will return as soon as anyone refactors the affected code paths.

**Recommended fix**: For each named previously fixed bug, write a test that constructs an input which would have triggered the bug and asserts the corrected behavior. At minimum: (1) EC20 < EC80 ordering with a known monotonic decreasing curve; (2) weight vector length matches `standards_for_model` after high-variability filtering; (3) `attr(plate_numeric, "import_info")` survives the dilution flush; (4) two concurrent sessions on the same R process do not see each other's `output_dir`; (5) dilution values > 1 without ":" notation emit a warning; (6) `mean_sample_cv` is populated in `model_stats.json` after the save-sample-stats chunk runs; (7) KPI strip populates from `.stats_env` or JSON fallback after a successful render.

**Regression test needed**: yes — one per bug. This is a single-shot test-writing exercise; recommend grouping into `tests/testthat/test-regression-known-bugs.R`.

---
**Issue ID**: AUDIT-004
**Category**: D. Reproducibility
**Severity**: Critical (JOSS blocker)
**Location**: `DESCRIPTION` (no version pins), `.github/workflows/R-CMD-check.yml` (uses `any::`)
**Title**: No dependency pinning — package upgrades can silently change scientific output
**Evidence**: Stage 1 §2 documented that `DESCRIPTION` has 37 unpinned imports, no `renv.lock`, no `.Rprofile`, and CI uses `any::` for every package. The `drc` package in particular has changed `LL.4()` initialization heuristics in past minor releases; `ggplot2 ≥ 3.4.0` deprecated `size=` in `geom_line` (already affecting `unified_analysis_template.Rmd:1069`); `dplyr` changes default behavior across versions. There is no record of which package versions were used to produce the screenshots in `docs/screenshots/`, the Zenodo deposit, or any test reference values.

Reproducer: bump `drc` to a hypothetical 4.0 and the LL.4 starting values change; numerical results drift; the golden-number test (once AUDIT-002 is fixed) catches it on the developer's machine but not at user install time.

**Impact**: A bench scientist who installs the app a year after publication gets different EC values than the paper reports. JOSS reviewers will ask for a pinned environment.

**Recommended fix**: Add a `renv.lock` capturing the exact versions used to generate the v1.0.0 reference outputs. Add minimum versions to `DESCRIPTION` Imports for the four packages where output is sensitive: `drc`, `ggplot2`, `dplyr`, `rmarkdown`. Declare `Depends: R (>= 4.2)` in `DESCRIPTION` to match the runtime warning in `global.R:20`. In CI, replace `any::` with `min::` pins for the same four sensitive packages and add a `release/devel` matrix axis so dependency upgrades are caught before user-visible bumps.

**Regression test needed**: not directly; the renv.lock + golden-number test combination is the test.

---

## HIGH

---
**Issue ID**: AUDIT-005
**Category**: A. Statistical correctness
**Severity**: High
**Location**: `reports/analysis_pipeline.R:364–378`
**Title**: ELISA LL.4 `upperl = c(NA, NA, 100, NA)` silently biases the top asymptote in matrix-effect plates
**Evidence**: When `is_elisa == TRUE`, `drc::drm(..., upperl = c(NA, NA, 100, NA))` constrains parameter d (top asymptote) ≤ 100. Real-world competitive ELISAs with low NSB or matrix effects can produce sample %B/B0 values above 100 (binding stronger than B0 control). drc's L-BFGS-B optimizer will clamp d at 100 and the fitted curve will not pass through the data — the residuals at low-concentration standards will be biased upward and R² will be artificially depressed. The fit still converges and reports a normal `R²`, `RMSE`, and IC50 — no flag tells the user the constraint was active.

Reproducer: ELISA dataset where one or more standards have computed %B/B0 ≥ 105% (entirely plausible with weak NSB or fresh receptor preparations). The fit caps top asymptote at 100; the IC50 estimate is biased toward higher values.

**Impact**: A common analytical scenario silently produces a wrong IC50.

**Recommended fix**: Either (a) remove the upper bound on d and let drc fit it freely (recommended — competitive-binding theory does not require %B/B0 ≤ 100 in finite samples), or (b) keep the bound but check `summary(fit)$coefficients["d:(Intercept)", ...]` against the bound after fitting and emit a prominent in-report warning when the parameter is at or near the boundary (within 1e-6, already detected by `assess_model_stability():502–512`).

**Regression test needed**: yes — fit a synthetic ELISA dataset with engineered %B/B0 > 100 at the top standard, assert that either (a) the fitted top asymptote exceeds 100, or (b) `assess_model_stability()$boundary_flag` is TRUE and the warning is rendered.

---
**Issue ID**: AUDIT-006
**Category**: A. Statistical correctness
**Severity**: High
**Location**: `reports/analysis_pipeline.R:535–537`
**Title**: EC20/EC80 swap logic silently produces inverted values for any non-decreasing curve
**Evidence**: After computing EC20 (`ED(model_fit, respLev = 80)`) and EC80 (`respLev = 20`), the code unconditionally swaps if `ec20 > ec80`. The drc `respLev` convention measures the percent reduction from the top asymptote; for a decreasing competitive-binding curve, EC20 < EC80 in concentration units. For an *increasing* curve (agonist binding, or an ELISA where signal increases with analyte because of an antibody quirk), the convention inverts and the swap silently masks it. The UI restricts the assay type to "rba" and "elisa" (`app.R:436–441`), so an increasing curve is not nominally supported — but the curve direction is a property of the data, not the UI selection. A user who uploads agonist data to the RBA path gets a fit with an inverted Hill slope and EC values that are swapped and labeled as if they were canonical EC20/EC80.

Reproducer: synthetic dataset where MeasurementValue *increases* with concentration. drc fits LL.4 with negative Hill slope (b < 0 in drc's parameterization for monotone-increasing). EC20 and EC80 from `ED(...respLev=80/20)` are reversed; the swap reorders them but does not flag the direction inversion.

**Impact**: Wrong scientific result on a class of data the app does not detect.

**Recommended fix**: Check the sign of `coef(fit)["b:(Intercept)"]` (Hill slope) before EC computation. For decreasing curves (the only supported scope), b > 0 in drc's LL.4 parameterization; if b < 0, refuse to compute EC values and emit `showNotification("Increasing dose-response curves are not supported. Check your standards and sample types.", type = "error")`. Document the supported scope in the README and in the eventual JOSS paper.

**Regression test needed**: yes — synthetic increasing-curve dataset, assert the pipeline either errors with the documented message or sets `direction_supported = FALSE` in `model_stats.json`.

---
**Issue ID**: AUDIT-007
**Category**: A. Statistical correctness / F. Error handling
**Severity**: High
**Location**: `reports/report_functions.R:336–347`
**Title**: Heteroscedasticity F-statistic cap at 1×10⁶ swallows degenerate-variance cases without halting auto-weighting
**Evidence**: When at least one replicate group has zero within-group variance, the Brown-Forsythe F-stat → ∞. The function caps it at 1e6 and emits a partial interpretation string but sets `p_value` to whatever the original test returned (NA in many degenerate cases). The auto-weighting selector at `analysis_pipeline.R:323–348` reads `hetero_auto$p_value` and `hetero_auto$variance_ratio`. NA-safe guards (`!is.na(...)`) protect the comparison, so an NA p_value falls through to the variance-ratio branch — but the variance_ratio may itself be `Inf` if min(valid_vars) is zero. `Inf > 10` is TRUE, so `inv_y2` is selected. That is probably the right choice for genuinely heteroscedastic data, but the user is never informed that the heteroscedasticity diagnostic was numerically degenerate.

Reproducer: standards where one concentration level has all-identical responses across replicates (e.g. saturating top standard at the instrument's upper limit). The auto-weighting branch silently picks `inv_y2` even when the underlying data may not justify it.

**Impact**: Auto-weighting decisions on real plates with bench-data quirks happen without telling the user.

**Recommended fix**: When `assess_heteroscedasticity()` caps F at 1e6, set `result$degenerate = TRUE` and propagate that flag into `auto_weighting_result$degenerate`. In the report's weighting-suitability section, render a prominent warning when `degenerate = TRUE` so the user knows to inspect the standards manually rather than trusting the auto-pick.

**Regression test needed**: yes — synthetic standards with zero within-group variance at one level, assert `assess_heteroscedasticity()$degenerate == TRUE` and that the auto-weighting result carries the flag through.

---
**Issue ID**: AUDIT-008
**Category**: D. Reproducibility / H. JOSS documentation
**Severity**: High (JOSS blocker)
**Location**: `DESCRIPTION:3`, `app.R:7,121`, `CITATION.cff:19,38–39`, `reports/report_constants.R:144`
**Title**: Five inconsistent version strings across the repository
**Evidence**: Stage 1 §11 already enumerated: DESCRIPTION says `Version: 2.0.0`, `app.R:7` comment and `app.R:121` UI badge say `1.0.0`, `CITATION.cff:19` field says `1.0.0`, `report_constants.R:144` says `version = "2.0"`, and the explicit Zenodo version DOI at `CITATION.cff:38–39` is labelled `v0.9.0`.

**Impact**: JOSS reviewers will reject a submission where citation, package metadata, and UI badge disagree about which version is being reviewed. Downstream users cannot reliably cite a specific release.

**Recommended fix**: Establish `DESCRIPTION` as the single source of truth. Replace every hardcoded version string with a runtime lookup from `utils::packageVersion("RBAElisaApp")` (already partially done at `global.R:124–127` for `APP_VERSION`; extend the same pattern to `report_constants.R:REPORT_INFO`, the UI top-bar badge in `app.R:121`, and the report-meta chunk). Add a `tools/release.R` script that (a) bumps the DESCRIPTION version, (b) updates `CITATION.cff` programmatically, (c) creates a git tag, (d) instructs the maintainer to upload to Zenodo. Document the release process in `CONTRIBUTING.md`.

**Regression test needed**: yes — a single test that reads DESCRIPTION, parses CITATION.cff, and asserts the version strings match.

---
**Issue ID**: AUDIT-009
**Category**: H. JOSS documentation
**Severity**: High (JOSS blocker)
**Location**: `CITATION.cff:38–39`
**Title**: Zenodo version DOI in CITATION.cff is labelled v0.9.0 while the version field claims 1.0.0
**Evidence**: `CITATION.cff:38: value: "10.5281/zenodo.19691223"` with `description: "Version DOI for v0.9.0"`. The version field at line 19 says `1.0.0`.

**Impact**: A user citing this version DOI cites v0.9.0, not v1.0.0. JOSS requires a specific release deposit; the mismatch will block acceptance.

**Recommended fix**: On v1.0.0 tag, create a fresh Zenodo release, capture the new version DOI, and update `CITATION.cff` to reference it. Remove the old `v0.9.0` version DOI entry (the concept DOI at line 35 always resolves to the latest version, so historical pinning is preserved).

**Regression test needed**: no — caught by AUDIT-008 regression test once both strings have to match a single source.

---
**Issue ID**: AUDIT-010
**Category**: E. Session safety
**Severity**: High
**Location**: `app.R:67–84`, `app.R:1139–1146`, `scripts/run_local.R:123–126`, `tests/testthat/test-shinytest-*.R:46–58`
**Title**: Process-global `Sys.setenv("RBA_OUTPUT_DIR", ...)` creates a race condition on multi-user Shiny Server
**Evidence**: Stage 1 §7 documented the four env vars. `Sys.getenv("RBA_OUTPUT_DIR")` call sites (verified): `app.R:67, 68, 84, 1139–1142` and the three shinytest2 files. The session-scoped copy in `session$userData` (`app.R:1139–1146`) is captured at session-start time, so two sessions starting in close succession on the same R process will both read the same global env var. The first session then triggers `Sys.setenv(RBA_OUTPUT_DIR = ...)` in standalone mode, and a second session arriving immediately after inherits the wrong directory. The standalone-mode guard `Sys.getenv("RBA_STANDALONE") == "1"` is also a process-global flag, so once any session sets it the whole process behaves as if it's in standalone mode.

For the Rmd templates themselves: `unified_analysis_template.Rmd` uses `params$output_dir` passed by the orchestrator (`server_report.R:500`), not `Sys.getenv`, so the in-render path is safe. The race condition only manifests at session-startup boundary and in any standalone-render path that does not pass `params$output_dir` explicitly.

**Impact**: On a multi-user Shiny Server (a likely deployment for an IAEA-hosted analysis service), concurrent users can have their outputs written into each other's directories.

**Recommended fix**: Move the standalone-mode initialization out of `app.R` global scope and into a `session$onSessionEnded` handler scoped function called once per session. Stop using `Sys.setenv` for paths; instead pass paths through `session$userData` populated at session start from per-session-token defaults under `tools::R_user_dir()`. Keep the `Sys.getenv` fallback only for the `scripts/run_local.R` non-Shiny entry point, where there is no race.

**Regression test needed**: yes — a `shinytest2` test that starts two `AppDriver` instances against the same `app.R`, runs each through report generation, and asserts the two output directories differ and contain only their own session's data.

---
**Issue ID**: AUDIT-011
**Category**: E. Session safety
**Severity**: High
**Location**: `global.R:308`
**Title**: `theme_set(theme_rba())` mutates the calling R session's global ggplot2 theme
**Evidence**: `theme_set(theme_rba())` at the bottom of `global.R` is executed at package load time. In a hosted R process, this changes the ggplot2 default theme for every concurrent Shiny session AND for any other R code running in the same process (e.g. another Shiny app on the same Shiny Server). Within the app itself this is desirable; outside it, it's an action-at-a-distance bug.

**Impact**: Side effect on shared infrastructure; JOSS reviewers running multiple Shiny apps in the same process will see unexpected behavior in unrelated apps.

**Recommended fix**: Remove the global `theme_set` call. Apply `theme_rba()` explicitly at each `ggplot()` call site via `+ theme_rba()`, or wrap plot construction in a helper that applies the theme. Where the app already constructs plots through a helper (`render_plot()` in `reports/plot_functions.R`), apply the theme there. If a global default truly is required, restore it in an `onStop()` cleanup at session end.

**Regression test needed**: no — change is mechanical; can be verified by `expect_equal(ggplot2::theme_get(), ggplot2::theme_gray())` after sourcing global.R in a fresh session (once the theme_set is removed).

---
**Issue ID**: AUDIT-012
**Category**: B. Input handling
**Severity**: High
**Location**: `utils/utils_import_v3.R:33–37`
**Title**: CSV import is hardcoded to comma separator — German/French locale exports parse as a single column
**Evidence**: `read.csv(file_path, header = FALSE, stringsAsFactors = FALSE)` defaults to `sep = ","`. European plate-reader software (Tecan i-control, BMG, BioTek) exports CSV with `;` separator and `,` decimal in German/French locales. Such a file parses as a single character column; `detect_plate_location()` then fails because it cannot find an 8×N numeric block. The error returned to the user (`utils_import_v3.R:228–236`) is generic ("Could not detect plate data in file"). There is no automatic detection of the field separator and no user-facing field-separator option.

Reproducer: a CSV exported from any plate reader in `de_DE.UTF-8` or `fr_FR.UTF-8` locale.

**Impact**: A predictable class of real-world inputs fails with an unhelpful error. The app advertises "Smart auto-detection of plate data in .xlsx, .csv, and .txt" (README line 127); for a European user this claim does not hold.

**Recommended fix**: In `read_file_raw()`, detect the separator by reading the first non-blank line and counting candidates: if the line has more `;` than `,`, use `;`; if more `\t` than either, use `\t`; else default to `,`. Pair with the existing decimal-comma autodetection (`utils_import_v3.R:262–271`) so a `;`-separated file with `,` decimals parses correctly. Emit a `showNotification()` informing the user which separator was detected.

**Regression test needed**: yes — `test-utils_import.R` should add fixtures for `;`-separated, `\t`-separated, and `,`-separated with `,` decimal, asserting that `parse_plate_file()` returns the expected 8×12 numeric matrix in each case.

---
**Issue ID**: AUDIT-013
**Category**: B. Input handling
**Severity**: High
**Location**: `utils/utils_import_v3.R:262–271`
**Title**: Mixed comma/dot decimal separators in one file silently produce NAs without warning
**Evidence**: The auto-detection logic only swaps comma → period if `has_comma_decimal && !has_dot_decimal`. If a file contains BOTH (e.g. a CSV where someone edited a few cells in a US-locale Excel after the original European export), the swap is skipped. Cells with comma decimals then convert to NA in `as.numeric()` (the `suppressWarnings` at line 275 silences the warnings). The plate "imports" with some cells silently zeroed.

Reproducer: take `examples/elisa_cortisol_example.csv`, hand-edit two cells to use `,` as the decimal, save, re-import. Those two wells become NA.

**Impact**: Silent data corruption.

**Recommended fix**: After conversion, count NAs introduced and compare to the count of non-empty input cells. If the conversion loss exceeds a threshold (say 5%) AND both decimal styles were present in input, halt with `showNotification("Mixed decimal separators detected in file — please re-export with a single locale.", type = "error")`. Alternatively, convert per-cell: if a cell parses cleanly only after comma→period swap, swap that cell.

**Regression test needed**: yes — fixture with mixed decimal separators; assert that `parse_plate_file()` either errors or correctly handles all cells.

---
**Issue ID**: AUDIT-014
**Category**: F. Error handling
**Severity**: High
**Location**: `reports/unified_analysis_template.Rmd:83–84`
**Title**: Global `warning = FALSE` in knitr opts means pipeline warnings never reach the user
**Evidence**: `knitr::opts_chunk$set(echo = FALSE, warning = FALSE, message = FALSE)`. The most important warnings emitted by the statistical pipeline — `warning("LL.4() failed ...")`, `warning("Using log-linear interpolation fallback ...")`, `warning("Detected European decimal format ...")`, `warning("Control hierarchy violated ...")`, etc. — never appear in the rendered report and do not propagate to a Shiny `showNotification`. The only mechanism that surfaces these warnings is via the `tryCatch` in the orchestrator at `server_report.R:565`, but `tryCatch` catches errors, not warnings; so warnings die silently.

**Impact**: This compounds AUDIT-001 (interpolation fallback is silent), AUDIT-005 (LL.4 upper bound is silent), AUDIT-007 (heteroscedasticity F-cap is silent), and AUDIT-013 (decimal-separator mismatch is silent). The Rmd's blanket suppression of warnings is the upstream cause for several of the silent-result issues.

**Recommended fix**: Replace the global `warning = FALSE` with chunk-local suppression only where warnings are known to be benign (e.g. ggplot2 deprecation warnings during transition). Add a `withCallingHandlers(..., warning = function(w) { captured_warnings <<- c(captured_warnings, conditionMessage(w)) })` wrapper around the statistical-pipeline calls and render a "Diagnostics" section at the end of the report listing all captured warnings. For Shiny use, also call `showNotification(conditionMessage(w), type = "warning", duration = 12)` for each warning in the wrapper, so the user sees them in real time.

**Regression test needed**: yes — a render test that triggers a known warning (e.g. by passing a `wt_key` that forces LL.4 failure) and asserts the warning appears in the captured-diagnostics section.

---
**Issue ID**: AUDIT-015
**Category**: G. Test coverage
**Severity**: High
**Location**: `tests/testthat/test-shinytest-*.R`
**Title**: Shinytest2 tests only assert "HTML file > 10 KB" — they pass on completely wrong numerical content
**Evidence**: All three `test-shinytest-*.R` files end with assertions of the form `expect_gt(file.info(html_files[1])$size, 10000)`. They do not parse the HTML or read `model_stats.json`. A regression that produces an HTML report containing all-NA sample concentrations would pass these tests as long as the HTML scaffolding renders.

**Impact**: The integration test layer provides false confidence.

**Recommended fix**: After the report renders, parse `model_stats.json` and assert (a) `r_squared` is within a known range, (b) `weight_method` matches what the test selected, (c) `mean_sample_cv` is populated. Also parse `unknown_results_summary.csv` and assert the expected number of replicate groups is present and all `mean_concentration` values are finite. Pair with a golden snapshot of `model_stats.json` per assay path.

**Regression test needed**: this is the fix; recommend updating each shinytest2 test in place.

---
**Issue ID**: AUDIT-016
**Category**: F. Error handling
**Severity**: High
**Location**: `server/server_common.R:113–138`
**Title**: Auto-save errors are caught and silently discarded — user has no way to know their session is not being saved
**Evidence**: `observe({ autoSaveTimer(); tryCatch({ saveRDS(...) }, error = function(e) {} ) })`. The error handler is empty. Common failure modes — disk full, no write permission to `R_user_dir`, file lock — produce no notification.

**Impact**: A user works for an hour expecting auto-save to protect them; the disk fills up; on browser refresh they lose everything. Trust failure for a non-critical-path UX.

**Recommended fix**: Log the error to `message()` (visible in Shiny Server logs) and emit a one-time `showNotification("Auto-save failed: <reason>. Your changes are not being saved.", type = "warning", duration = 15)` per session (track with a session-scoped flag to avoid spamming).

**Regression test needed**: no — change is mechanical and would be tested as part of a UX test suite.

---

## MEDIUM

---
**Issue ID**: AUDIT-017
**Category**: A. Statistical correctness / F. Error handling
**Severity**: Medium (downgraded from Stage 1 "Critical" after algebraic verification)
**Location**: `utils/utils_normalization.R:62–108` (ELISA branch of `normalize_data`)
**Title**: ELISA branch of `normalize_data()` is dead code with an alternate-form formula; misleads code reviewers
**Evidence**: Stage 1 §8 flagged two different %B/B0 formulas. On algebraic check, the two formulas are equivalent:
- `utils_normalization.R:92`: `100 * (M - NSB_raw) / (B0_raw - NSB_raw)`
- `calculate_elisa_bb0()` (`report_functions.R:170–221`): `(M - blank - (NSB - blank)) / ((B0 - blank) - (NSB - blank)) * 100` = `(M - NSB) / (B0 - NSB) * 100`

Both reduce to `100 * (M - NSB) / (B0 - NSB)`. So the *numerical* finding from Stage 1 is wrong — they would produce identical values if both ran.

What is real: `normalize_assay_data()` in `report_pipeline.R:104–117` short-circuits the ELISA path to `NormalizedValue = MeasurementValue` (pass-through, not normalized). The actual normalization is performed inside the Rmd at `unified_analysis_template.Rmd:373` via `calculate_elisa_bb0()`. So `normalize_data()`'s ELISA branch in `utils_normalization.R` is **never invoked from the live pipeline** — it's dead code. The test at `test-utils_normalization.R:79–87` only exercises the RBA path, confirming the ELISA path is unused.

**Impact**: A reviewer reading the code (as Stage 1 did) reasonably assumes both formulas run and that the pipeline could have a numerical inconsistency. The dead code is a JOSS-reviewer trap.

**Recommended fix**: Delete the ELISA branch of `normalize_data()` and the supporting helpers (`get_normalization_strategy` ELISA branch, `validate_controls()`, `extract_controls()`) — OR consolidate by making `normalize_assay_data()` in `report_pipeline.R` delegate to `calculate_elisa_bb0()` for ELISA so the same function performs the normalization in both contexts. The second option is preferable because it keeps the normalization formula in one place (`calculate_elisa_bb0()`) and removes the bifurcation entirely.

**Regression test needed**: yes — a comparison test asserting `calculate_elisa_bb0(data, use_percent = TRUE)$calculated_bb0` and `normalize_data(data, "elisa")$NormalizedValue` produce the same numbers on a fixture (this is the test that should have caught the Stage 1 mischaracterization). After consolidation, the test should be adapted to assert the single remaining function still produces the expected values.

---
**Issue ID**: AUDIT-018
**Category**: C. Report generation / D. Reproducibility
**Severity**: Medium
**Location**: `reports/unified_analysis_template.Rmd:431–475`, `reports/analysis_pipeline.R` (no input hash), `server/report_pipeline.R:207–234` (no session info)
**Title**: Report metadata block lacks input-file hash, full package versions, run UUID, analysis settings summary
**Evidence**: The `report-meta` chunk records authors, date, OS user, assay label, app version, R version, and drc version. Stage 1 §6 catalogued what's missing. There is no input file hash, no per-package version snapshot, no run UUID. Two consecutive runs on the same data produce reports indistinguishable in metadata.

**Impact**: A user cannot prove which input file produced a given report. A reviewer cannot reproduce a published report from sidecar JSON alone.

**Recommended fix**: Add to the report-meta block:
- Input file SHA-256 (computed during upload, stored in `assay_config.json`)
- Run UUID (e.g. `uuid::UUIDgenerate()` at report-start time)
- A compact `sessionInfo()` summary: just the named-package list and versions, not the full base/locale/system dump
- Analysis settings summary: regression_weight, ci_method, outlier settings, CV limit
- Save the same metadata as `report_metadata.json` sidecar for programmatic access

**Regression test needed**: yes — assert that `report_metadata.json` exists post-render and includes the listed fields with non-empty values.

---
**Issue ID**: AUDIT-019
**Category**: A. Statistical correctness
**Severity**: Medium
**Location**: `reports/report_functions.R:567–630`, `reports/report_constants.R:39`, `reports/report_sections.R:587`
**Title**: `bootstrap_iterations` constant disagrees with literal fallback in one call site
**Evidence**: `STATS_CONFIG$bootstrap_iterations = 1000` (`report_constants.R:39`). `compute_layered_uncertainty()` uses it directly (`report_functions.R:601`). But `report_sections.R:587` reads it as `boot_n <- STATS_CONFIG$bootstrap_iterations %||% 2000` — the 2000 fallback is unreachable when `report_constants.R` is loaded (always in practice), so the *effective* value is 1000. The literal 2000 is misleading.

**Impact**: Low practical impact (1000 is used), but a future refactor that misses `report_constants.R` loading could silently double the bootstrap iterations.

**Recommended fix**: Replace the literal `%||% 2000` with `%||% STATS_CONFIG$bootstrap_iterations` or simply remove the fallback. Standardize all bootstrap call sites to read directly from `STATS_CONFIG$bootstrap_iterations` and fail loudly with `stopifnot(!is.null(STATS_CONFIG$bootstrap_iterations))` at top of script.

**Regression test needed**: yes — `test-stats-config.R` already asserts the value; add an assertion that the value used by `compute_layered_uncertainty()` and any other consumer matches.

---
**Issue ID**: AUDIT-020
**Category**: A. Statistical correctness / D. Reproducibility
**Severity**: Medium
**Location**: `reports/report_functions.R:599–603`
**Title**: Bootstrap `set.seed(42)` + `set.seed(NULL)` produces reports reproducible only if bootstrap is the first random operation
**Evidence**: `set.seed(42); boot_vals <- replicate(N, mean(sample(...))); set.seed(NULL)`. The `set.seed(NULL)` re-randomizes the RNG state from system entropy after the bootstrap, so any subsequent random operation in the same render uses a non-reproducible seed. There is no other `set.seed` call in the production pipeline (`set.seed(42)` and `set.seed(123/456)` appear in test fixtures, not in the live render), so in practice the bootstrap *is* reproducible — but the contract is fragile. A future addition of any `rnorm`/`sample` call between report start and the bootstrap chunk would silently break reproducibility.

**Impact**: Currently reproducible by luck. Any refactor that adds randomness elsewhere will break it.

**Recommended fix**: Set the seed once at top of the Rmd `setup` chunk (e.g. `set.seed(STATS_CONFIG$report_seed %||% 42)`) and remove the local `set.seed(42)`/`set.seed(NULL)` pair from `compute_layered_uncertainty()`. Document the contract: "Every render of this template is deterministic given the same inputs and seed." Add `set.seed(NULL)` only at session-end if needed; not after bootstrap.

**Regression test needed**: yes — run the golden RBA render twice in a fresh R session and assert `model_stats.json` and `unknown_results_summary.csv` are byte-identical.

---
**Issue ID**: AUDIT-021
**Category**: C. Report generation / D. Reproducibility
**Severity**: Medium
**Location**: `server/report_pipeline.R` (no replay script), no documented invocation
**Title**: No replay script — sidecar JSONs are sufficient in principle but no documented entry point regenerates a report from them
**Evidence**: Stage 1 §6 notes that `long_data_output.csv` + `assay_config.json` + `analysis_config.json` + `qc_params.json` together contain enough state to re-run the pipeline. But there is no `scripts/replay_report.R` or equivalent, and the Rmd is normally invoked from the Shiny session, not from the command line. Reviewers wanting to replay a deposit need to reverse-engineer the invocation from `server_report.R:498–512`.

**Impact**: Reproducibility claim is theoretical; nobody can exercise it.

**Recommended fix**: Add `scripts/replay_report.R` taking an output_dir argument, reading the sidecars, calling `rmarkdown::render(reports/unified_analysis_template.Rmd, params = list(output_dir = ..., lang = "en"))`. Document in README. Make a release-time CI job that runs the replay against the shipped example to confirm reports remain reproducible.

**Regression test needed**: yes — `test-replay.R` that runs the shipped example through `scripts/replay_report.R` and asserts the generated HTML/JSON match a snapshot.

---
**Issue ID**: AUDIT-022
**Category**: B. Input handling
**Severity**: Medium
**Location**: `utils/utils_import_v3.R:26–40` (no encoding handling), `server/server_upload.R` (no BOM stripping)
**Title**: No encoding handling — Latin-1 / UTF-8-BOM CSV inputs corrupt sample IDs
**Evidence**: Stage 1 §5 confirmed no `fileEncoding=`, no `iconv`, no BOM stripping. `read.csv` uses the system locale; on a Linux container that's typically UTF-8. A Windows-exported CSV in `Windows-1252` containing accented sample IDs (common on French/German bench data) produces mojibake in the rendered report. UTF-8-with-BOM (Excel "CSV UTF-8") leaves a `﻿` prefix on the first column header, which currently gets silently ignored because column names are not matched.

**Impact**: Sample IDs and notes rendered in the report may be garbled. Low magnitude per occurrence but predictable on common inputs.

**Recommended fix**: In `read_file_raw()`, detect BOM via `readBin(file_path, "raw", n = 3)` and strip if present; detect non-ASCII bytes outside UTF-8 sequences and offer a re-import-as-Latin-1 path with a user prompt. Add `encoding = "UTF-8"` to `read.csv` and surface a `showNotification` when the file appears not to be valid UTF-8.

**Regression test needed**: yes — fixtures for UTF-8-BOM, Latin-1, UTF-8 with accented sample IDs; assert sample IDs round-trip correctly through the pipeline.

---
**Issue ID**: AUDIT-023
**Category**: C. Report generation
**Severity**: Medium
**Location**: `server/report_pipeline.R:295–298`, `reports/create_reference_doc.R`
**Title**: DOCX reference.docx is referenced but appears never to be generated automatically
**Evidence**: `server/report_pipeline.R:296–298` looks for `reports/reference.docx`. `reports/create_reference_doc.R` generates one, but is never sourced by any production code path or test (Stage 1 §11 flagged as "Potentially Unused"). If the file doesn't exist, `rmarkdown::word_document(reference_docx = NULL)` falls back to pandoc defaults — fine, but means the DOCX output lacks the intended branded styling. There is no shipped `reference.docx` in `reports/`.

**Impact**: DOCX reports look generic, not branded; a JOSS reviewer comparing DOCX/HTML outputs will see formatting inconsistencies that suggest the DOCX path is less polished.

**Recommended fix**: Either (a) ship a checked-in `reports/reference.docx` (generated once and committed), or (b) source `create_reference_doc.R` once at first report render and cache the output. Option (a) is preferred because the reference doc rarely needs regeneration.

**Regression test needed**: no — verify in a manual sanity check that DOCX output uses the intended styling.

---
**Issue ID**: AUDIT-024
**Category**: I. Security and file handling
**Severity**: Medium
**Location**: `global.R:231` (defined), `server/server_upload.R` (no consumer)
**Title**: `MAX_UPLOAD_SIZE_MB` is defined but never wired to `options(shiny.maxRequestSize=...)` — effective limit is the Shiny default of 5 MB
**Evidence**: `global.R:231: MAX_UPLOAD_SIZE_MB <- 10`. Grep across the codebase shows no consumer that calls `options(shiny.maxRequestSize = MAX_UPLOAD_SIZE_MB * 1024^2)`. The Shiny default is `5 * 1024^2`, so uploads > 5 MB are rejected with a generic error.

**Impact**: A multi-wavelength Excel file (which can be 6–8 MB) is silently rejected. User has no way to know the limit is 5 MB, not the documented 10 MB.

**Recommended fix**: Add `options(shiny.maxRequestSize = MAX_UPLOAD_SIZE_MB * 1024^2)` at the top of `app.R` (after sourcing `global.R`).

**Regression test needed**: yes — assert `getOption("shiny.maxRequestSize")` equals `MAX_UPLOAD_SIZE_MB * 1024^2` after `global.R` is loaded.

---
**Issue ID**: AUDIT-025
**Category**: I. Security and file handling
**Severity**: Medium
**Location**: `server/report_pipeline.R:189` (wavelength label sanitization), `server/report_pipeline.R:418` (output file name)
**Title**: Output file names derived from user/upload-controlled strings need explicit path-traversal sanitization
**Evidence**: `wl_safe <- gsub("[/\\\\:*?\"<>|]", "_", wl)` (`report_pipeline.R:189`) sanitizes wavelength labels — good. But `out_name <- paste0(base_out, "-", variant, "-", lang_code)` (`report_pipeline.R:418`) uses `lang_code` which originates from `input$report_languages`. The choices are restricted to a fixed `selectInput` (`app.R:1055–1059`), so server-side this is currently safe. But a malicious client could `Shiny.setInputValue("report_languages", "../../../etc/passwd")` and break out of `output_dir`. Defense in depth would validate `lang_code %in% c("en","es","fr","ru","zh")` server-side.

The `notes.json` content (`input$notes`) is written to a file path that does not derive from user input, so safe — but the notes content is rendered into HTML without any explicit sanitization. Since `rmarkdown::render` itself escapes markdown by default this is low risk, but explicit escaping would be safer.

**Impact**: Low in current deployment (single-user); higher in a public Shiny Server deployment.

**Recommended fix**: Add a `validate_lang_code()` helper used everywhere `lang_code` reaches a file path. Sanitize `notes` content rendered to HTML via `htmltools::htmlEscape()` before passing to the Rmd.

**Regression test needed**: yes — security regression test that injects a path-traversal `lang_code` and asserts the output file remains within `output_dir`.

---
**Issue ID**: AUDIT-026
**Category**: I. Security and file handling
**Severity**: Medium
**Location**: `global.R:33`, `DESCRIPTION`
**Title**: `remotes` is required at runtime but missing from DESCRIPTION Imports
**Evidence**: Stage 1 Open Q #13. `global.R:33` lists `"remotes"` in `required_pkgs`. `DESCRIPTION` does not list `remotes` in `Imports`. A user who installs via `devtools::install("path/to/repo")` or `remotes::install_github(...)` gets the DESCRIPTION-declared deps; `remotes` is then a fresh install, which the startup install loop (`global.R:60–115`) handles only if `remotes` itself is installable from RSPM at startup — possible chicken-and-egg failure on minimal sandboxes.

**Impact**: Edge-case install failure on environments without prior `remotes` installation.

**Recommended fix**: Either (a) add `remotes` to DESCRIPTION Imports, or (b) remove the runtime dependency on `remotes` by accepting that GitHub fallback installs only work in interactive environments and instructing users to install missing packages manually. Option (a) is simpler and matches current runtime expectation.

**Regression test needed**: no — caught by adding `Imports: remotes` to DESCRIPTION; CI then verifies it installs.

---
**Issue ID**: AUDIT-027
**Category**: F. Error handling
**Severity**: Medium
**Location**: `reports/analysis_pipeline.R:670–679`, `reports/report_functions.R:396`
**Title**: Several `tryCatch` error handlers use `<<-` to write into the enclosing function's local scope, which is correct but easy to misread as a global side effect
**Evidence**: `sample_results$quantification_status <<- "Unknown"` (`analysis_pipeline.R:678`) writes into `quantify_samples()`'s local environment, not the global environment. Same pattern at `report_functions.R:396, 546, 547, 795`. The `<<-` works because the enclosing function defines `result`/`sample_results` in its own scope. But the pattern reads like a global write to anyone not deeply familiar with R scoping rules.

**Impact**: Maintainability hazard; a future refactor that moves the body around could accidentally promote the variable to global. Not a current bug.

**Recommended fix**: Replace `<<-` inside `tryCatch` error handlers with explicit `assign("varname", value, envir = parent.frame())` calls (more explicit) or restructure to capture the failure in a return value rather than mutating closure state.

**Regression test needed**: no — refactor-only; existing tests cover the behavior.

---
**Issue ID**: AUDIT-028
**Category**: H. JOSS documentation
**Severity**: Medium (JOSS expectation)
**Location**: repository root (absence)
**Title**: No CONTRIBUTING.md, no CODE_OF_CONDUCT.md
**Evidence**: Stage 1 §10. JOSS requires community guidelines and a contribution process.

**Impact**: Direct JOSS submission blocker per the JOSS review checklist.

**Recommended fix**: Add `CONTRIBUTING.md` documenting: branch model, PR process, how to run tests locally, how to capture golden-number reference values, release process (paired with AUDIT-008 fix). Add `CODE_OF_CONDUCT.md` (Contributor Covenant 2.1).

**Regression test needed**: no — documentation deliverable.

---
**Issue ID**: AUDIT-029
**Category**: H. JOSS documentation
**Severity**: Medium
**Location**: `README.md:166–182`, `.github/workflows/R-CMD-check.yml`
**Title**: README project-structure table is stale (pre-refactor paths); CI matrix is a single R version with no `R CMD check`, `covr`, or `lintr`
**Evidence**: Stage 1 §2, §10. README lists `i18n.R`, `utils_plate.R`, etc. as root-level; they were moved to `server/` and `utils/` subdirectories. CI runs only R 4.3 on Ubuntu.

**Impact**: Documentation lies to new contributors. Single-version CI does not catch R-version-specific regressions.

**Recommended fix**: Update README project-structure table to match the actual layout. Expand CI to a 3×2 matrix (Linux + macOS + Windows × R 4.2/4.3/release) and add `R CMD check`, `covr` with a coverage threshold (start at 50%, raise over time), and `lintr` with the recommended preset.

**Regression test needed**: no — documentation/CI deliverable.

---
**Issue ID**: AUDIT-030
**Category**: H. JOSS documentation
**Severity**: Medium
**Location**: `README.md`
**Title**: Statement of need is partial — no comparison to alternatives, no target user community
**Evidence**: Stage 1 §10. JOSS requires an explicit statement of need that situates the software relative to alternatives. The README's opening paragraph describes what the app does but does not compare to GraphPad Prism, `drc` directly, PROAST, or web tools like ED50plus.

**Impact**: JOSS reviewers will ask. Without it, the paper risks rejection on "fit" grounds.

**Recommended fix**: Add a 2–3 paragraph "Statement of need" section near the top of the README and in the eventual JOSS paper, naming alternatives, what they lack (no full assay-config workflow, no QC profiling, no bilingual reports, no plate-layout editor), and the target user community (bench scientists in IAEA/UCR-style marine biotoxin labs who want reproducible reports without writing R).

**Regression test needed**: no — documentation deliverable.

---
**Issue ID**: AUDIT-031
**Category**: H. JOSS documentation
**Severity**: Medium
**Location**: `README.md` (Quick Start section)
**Title**: Installation instructions omit system dependencies (pandoc, tinytex, Chrome for shinytest2)
**Evidence**: The Quick Start jumps from `shiny::runGitHub(...)` straight to launching the app. Pandoc is required by `rmarkdown::render`; tinytex is required for PDF output; Chrome is required for the shinytest2 suite. On a clean R install (no RStudio bundled pandoc), the app fails at report generation. Stage 1 §10.

**Impact**: A new user without RStudio fails at report generation with an opaque error.

**Recommended fix**: Add a "System dependencies" subsection under Installation listing pandoc, tinytex (for PDF), and Chrome (for shinytest2). Provide a one-line OS-specific install command for each.

**Regression test needed**: no — documentation deliverable.

---
**Issue ID**: AUDIT-032
**Category**: A. Statistical correctness
**Severity**: Medium
**Location**: `reports/analysis_pipeline.R:301–305`
**Title**: "Auto" weighting collapses any user-selected multi-weight comparison to a single weight; user is not informed
**Evidence**: If `selected_weights` contains `"auto"` AND any other key (e.g. user ticked all four checkboxes), the auto branch (`analysis_pipeline.R:307–349`) overwrites `selected_weights` with the single chosen weight and sets `multi_weight_mode <- FALSE`. The weight-comparison plot in the report is then suppressed (`eval=!is_compact` AND `multi_weight_mode == FALSE`), so the user does not see the side-by-side comparison they implicitly requested.

**Impact**: User expectation mismatch. The user ticked "Unweighted" and "Auto" expecting to see both, only one rendered.

**Recommended fix**: Either (a) when "Auto" is combined with other selections, treat it as a hint and still fit all selected weights; render the auto-pick as a recommendation badge alongside. Or (b) make Auto a radio button rather than a checkbox so it is mutually exclusive with explicit weights. Option (b) is simpler.

**Regression test needed**: yes — assert the multi-weight comparison plot is rendered when user selects multiple explicit weights, and that Auto cannot be combined with explicit selections.

---
**Issue ID**: AUDIT-033
**Category**: B. Input handling
**Severity**: Medium
**Location**: `utils/utils_import_v3.R:246–258` (overflow markers); README (no spec doc)
**Title**: The "predefined format" is undocumented — what the importer accepts is described only by its source code
**Evidence**: Stage 1 §5. The README troubleshooting section gives partial guidance. There is no formal `docs/INPUT_FORMAT.md` describing the exact accepted shapes, overflow markers, decimal-separator handling, max file size, encoding requirements, or partial-plate semantics.

**Impact**: Reproducibility claim is weakened — a user cannot pre-validate their file without uploading.

**Recommended fix**: Add `docs/INPUT_FORMAT.md` with: (a) accepted file extensions, (b) two recognized layouts (labeled / unlabeled), (c) overflow marker list, (d) decimal-separator rules, (e) encoding requirements, (f) max file size, (g) partial-plate semantics, (h) example for each layout. Link from README.

**Regression test needed**: no — documentation deliverable, but the spec doc is the contract that future format-related tests can target.

---
**Issue ID**: AUDIT-034
**Category**: D. Reproducibility / G. Test coverage
**Severity**: Medium
**Location**: `tests/testthat/diff_golden_artifacts.R` (orphan)
**Title**: `diff_golden_artifacts.R` exists but is not wired to any test
**Evidence**: Stage 1 §9. The file appears intended as a golden-artifact diff utility but is not called from any test. No `_snaps/` directory exists.

**Impact**: A useful utility goes unused; golden-output tests are not in place.

**Recommended fix**: Convert `diff_golden_artifacts.R` into a `helper-golden.R` providing a `diff_against_golden(path, golden_path)` function; wire it into a new `test-golden-rba.R` and `test-golden-elisa.R` that compare full sidecar+CSV output against `tests/testthat/golden/<assay>/` snapshots.

**Regression test needed**: yes — these golden tests are the regression tests; the helper is just the plumbing.

---

## LOW

---
**Issue ID**: AUDIT-035
**Category**: A. Code hygiene
**Severity**: Low
**Location**: `reports/unified_analysis_template.Rmd:1069`
**Title**: `ggplot2::geom_line(..., size = 1.2)` — `size` is deprecated in ggplot2 ≥ 3.4.0
**Evidence**: Stage 1 §11. ggplot2 ≥ 3.4.0 deprecated `size=` in line geoms in favour of `linewidth=`. Currently hidden by the global `warning = FALSE` (AUDIT-014).

**Impact**: When warnings are restored (AUDIT-014 fix) this will surface; eventually ggplot2 will turn it into an error.

**Recommended fix**: Rename to `linewidth = 1.2`. Pin `ggplot2 (>= 3.4.0)` if not already, since the rename is one-directional.

**Regression test needed**: no — caught by AUDIT-014's diagnostic capture.

---
**Issue ID**: AUDIT-036
**Category**: A. Code hygiene
**Severity**: Low
**Location**: `reports/report_constants.R:107–112` vs `global.R:140–149`
**Title**: Two definitions of default RBA STX standard concentrations with different values
**Evidence**: `global.R:140`: `DEFAULT_STX_CONC <- c(1e-6, 1e-7, 3e-8, ...)`. `report_constants.R:108`: `DEFAULT_STANDARDS$rba_saxitoxin <- c(1e-6, 3e-7, 1e-7, ...)`. S2 differs: `1e-7` vs `3e-7`.

**Impact**: Which series is authoritative? The Rmd uses `global.R`'s when sourced from the live pipeline; `report_constants.R`'s standalone defaults differ. A user running the standalone Rmd path would get different standards.

**Recommended fix**: Delete `DEFAULT_STANDARDS` from `report_constants.R`; require sourcing `global.R` to obtain canonical defaults. Or, if `report_constants.R` is the standalone-mode source of truth, fix the values to match `global.R`.

**Regression test needed**: yes — assert `global.R::DEFAULT_STX_CONC` and `report_constants.R::DEFAULT_STANDARDS$rba_saxitoxin` are identical (or that one of them no longer exists).

---
**Issue ID**: AUDIT-037
**Category**: A. Code hygiene
**Severity**: Low
**Location**: `server/i18n.R:793, 1227, 2011, 2619`
**Title**: Four `TODO(i18n)` markers for unverified ES/RU/ZH translations
**Evidence**: Stage 1 §11. Translations for "Readiness check" in ES, RU, ZH labeled as needing native-speaker verification. ES has a generic `TODO i18n-es` for "PR-A new strings".

**Impact**: Beta-quality translations claimed as final. Beta banner shown for RU/ZH but not ES (`app.R:293`), inconsistent.

**Recommended fix**: Either (a) ship v1.0.0 with EN+ES only and label RU/ZH/FR as beta in the language picker (already done for RU/ZH; extend the banner to ES if any TODOs remain), or (b) resolve the TODOs via native-speaker review before v1.0.0.

**Regression test needed**: no — documentation/translation deliverable.

---
**Issue ID**: AUDIT-038
**Category**: E. Session safety
**Severity**: Low
**Location**: `server/i18n.R:23`
**Title**: `.TRANSLATIONS_CACHE <<-` is read-only after first load — safe today, but the pattern is fragile
**Evidence**: Stage 1 §7. The cache is populated once via super-assignment and never mutated after. Safe but undocumented.

**Impact**: A future contributor adding "per-user translation override" logic could mutate the cache and leak across sessions.

**Recommended fix**: Wrap the cache in a closure that exposes a `get()` method only, not a setter, so future mutation is structurally prevented. Add a comment at the `<<-` site stating "Initialise once; do not mutate after first call."

**Regression test needed**: no — code-hygiene change.

---
**Issue ID**: AUDIT-039
**Category**: A. Code hygiene
**Severity**: Low
**Location**: repository root
**Title**: Five development-process markdown files checked into root: `alignment-root-cause.md`, `gui-fix-pass.md`, `report-design-review.md`, `v1.0-polish-plan.md`, `v1.0-usability-plan.md`
**Evidence**: Stage 1 §11. These are working notes; checking them into the public repo clutters the JOSS submission and may confuse new contributors.

**Impact**: Noise in JOSS review. Mild.

**Recommended fix**: Move under `docs/development/` or delete if no longer relevant. Keep the audit dir (`audit/`) for now, but plan to relocate it to `docs/audits/` post-v1.0.0.

**Regression test needed**: no.

---
**Issue ID**: AUDIT-040
**Category**: H. JOSS documentation
**Severity**: Low
**Location**: `server/server_common.R`, `server/server_layout.R`, `server/server_config.R`, `server/server_upload.R`
**Title**: Four server modules have no roxygen function-level docs
**Evidence**: Stage 1 §10. `report_pipeline.R`, `utils_plate.R`, `utils_import_v3.R`, `utils_normalization.R` have roxygen; the listed server modules do not.

**Impact**: Acceptable for a non-package Shiny app, but JOSS reviewers prefer documented internals. Low-severity gap.

**Recommended fix**: Add brief roxygen `#'` block (Title + Description + @param + @return) to each top-level function in the four named server modules. Do not generate a NAMESPACE — these functions are not exported.

**Regression test needed**: no — documentation deliverable.

---
**Issue ID**: AUDIT-041
**Category**: C. Report generation
**Severity**: Low
**Location**: `reports/multiwavelength_analysis_template.Rmd:127`
**Title**: `tempdir()` used for Rmd chunk preprocessing — process-scoped, not session-scoped
**Evidence**: `preprocess_template_chunks(template_path, label_prefix, temp_dir = tempdir())`. R's `tempdir()` is session-scoped on a normal R session but is the same directory across concurrent Shiny sessions in the same R process (because Shiny runs all sessions in one R process). If two multi-wavelength renders run concurrently, they could collide on the same temp files.

**Impact**: Low — multi-wavelength renders are rare and the preprocessing produces uniquely named files via `label_prefix`. But the safer pattern is `tempdir(check = TRUE)` or per-session subdirs.

**Recommended fix**: Use `tempfile(pattern = paste0("mw_", label_prefix, "_"))` to obtain unique paths per render, instead of relying on label uniqueness.

**Regression test needed**: no — low risk, mechanical change.

---
**Issue ID**: AUDIT-042
**Category**: H. JOSS documentation / D. Reproducibility
**Severity**: Low
**Location**: `examples/README.md` (per Stage 1 inventory listing), `tests/testthat/_snaps/` (absent)
**Title**: Shipped example datasets have no documented expected output for diff-checking
**Evidence**: Stage 1 §9. The example datasets are usable for manual demo but produce no published expected R², IC50, sample count, or replicate summary. A user trying to verify their install produces "the right numbers" has nothing to compare against.

**Impact**: Install-verification is by visual inspection only.

**Recommended fix**: Generate a reference report from each shipped example, capture key statistics (R², IC50, n_replicate_groups, mean_sample_cv) into `examples/reference_outputs.json`, document in `examples/README.md` as "After installing, run the RBA STX example and verify the report shows R² = ..., IC50 = ...".

**Regression test needed**: yes — the same reference values are the input to AUDIT-002's golden-number test; this finding is partially redundant with AUDIT-002 but pertains to user-facing documentation rather than the test suite.

---
**Issue ID**: AUDIT-043
**Category**: F. Error handling
**Severity**: Low
**Location**: `reports/plot_functions.R:189`
**Title**: `tr_idx <<-` counter in a closure mutates closure state; safe but unusual pattern
**Evidence**: Stage 1 §7. A counter used to label translation lookups. The `<<-` writes into the closure's enclosing environment.

**Impact**: None — pattern is correct, just unusual for R contributors.

**Recommended fix**: Add a one-line comment at the `<<-` explaining the counter's purpose. No behavior change.

**Regression test needed**: no.

---

## SUMMARY

| Severity | Count |
|---|---|
| Critical | 4 |
| High | 12 |
| Medium | 18 |
| Low | 9 |
| **Total** | **43** |

The implementation plan (`audit/stage2-plan.md`) groups these into work units and prioritizes them against v1.0.0.

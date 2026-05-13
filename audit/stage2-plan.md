# Stage 2 — Prioritized Implementation Plan
## Competitive Binding Assay Analysis Suite (RBAElisaApp), pre-1.0.0 JOSS readiness
**Audit date:** 2026-05-13
**Source:** `audit/stage2-issues.md` (43 issues)

This plan groups the 43 issues from `stage2-issues.md` into work units sized for separate `claude/` branches. Each branch carries the regression tests that exercise its fixes. The grouping is by *coupling* (changes that touch the same files / require coordinated test updates) rather than by category, so each branch can be reviewed and merged independently.

Effort sizes: S = ≤ 1 day, M = 2–4 days, L = ≥ 5 days.

---

## Branch sequence

### Phase 0 — Decision capture and pre-fix baseline (this branch)

#### 0.1 `claude/audit-baseline-and-decisions`
**Issues**: documentation updates to AUDIT-001, AUDIT-005, AUDIT-006, AUDIT-008, AUDIT-021, AUDIT-029, AUDIT-040 + capture of the pre-fix numerical baseline
**Effort**: S
**Blocker for v1.0.0**: **yes — must precede Phase 1**
**Rationale (sequencing)**: Records the maintainer's decisions on the option choices in the four highest-impact statistical fixes (AUDIT-001 fail-loud, AUDIT-005 unbounded asymptote, AUDIT-006 pre-flight reject, AUDIT-008 v1.0.0 version) and captures the "what the app produces today" snapshot. Subsequent fix branches use the snapshot as the diff baseline to characterize numerical impact. Reasoning for sequencing: the statistical fix branches will change numerical output, so the baseline must be frozen against an unmodified `main` (or this branch's HEAD) before any fix lands.
**Required tests to ship**: none — documentation + script + frozen snapshot only. The captured `audit/pre-fix-snapshot/` directory IS the artifact this branch ships.

---

### Phase 1 — Numerical correctness (must precede everything that depends on results)

#### 1.1 `claude/fix-statistical-fallback-and-warnings`
**Issues**: AUDIT-001 (interpolation fallback silent NA), AUDIT-014 (global `warning=FALSE` swallows diagnostics)
**Effort**: M
**Blocker for v1.0.0**: yes
**Rationale (sequencing)**: This is the largest unknown-quantity correctness problem and it changes the user-visible behavior of degenerate-fit cases. Once warnings are surfaced (AUDIT-014) and the interpolation path is decided (AUDIT-001), several downstream issues (AUDIT-005, AUDIT-007) automatically gain user visibility. Do this first because it changes the contract of "what the report shows when the fit struggles."
**Required tests to ship**:
- `test-regression-known-bugs.R::test_that("interpolation fallback either quantifies or fails loudly")`
- `test-render-diagnostics.R::test_that("captured warnings render in the Diagnostics section")`
- A renderable fixture that forces LL.4 and LL.3 to fail (e.g. all-flat standards, 3 unique concentrations)

#### 1.2 `claude/fix-elisa-asymptote-and-curve-direction`
**Issues**: AUDIT-005 (ELISA top asymptote upper bound), AUDIT-006 (EC swap silently inverts increasing curves)
**Effort**: S
**Blocker for v1.0.0**: yes
**Rationale**: Both are short, targeted statistical-correctness fixes in `analysis_pipeline.R`. Both produce silently wrong scientific output on plausible data and JOSS reviewers will reproduce them. Bundling them in one branch is efficient because they touch the same file and share regression-test scaffolding (synthetic curve datasets).
**Required tests**:
- `test_that("ELISA fit handles %B/B0 > 100 without silently clamping")` — synthetic dataset with top standard at 105%, assert boundary flag or unconstrained fit
- `test_that("Increasing dose-response is rejected with a clear error")` — synthetic increasing curve, assert pipeline errors with the documented message

#### 1.3 `claude/fix-auto-weighting-degeneracy-flag`
**Issues**: AUDIT-007 (heteroscedasticity F-cap degeneracy)
**Effort**: S
**Blocker for v1.0.0**: yes (low-cost, high-trust gain)
**Rationale**: Small fix in `report_functions.R` plus propagation through `analysis_pipeline.R`. Independent of 1.1 and 1.2.
**Required tests**:
- `test_that("assess_heteroscedasticity flags degenerate F as $degenerate = TRUE")` — standards with zero within-group variance at one level

#### 1.4 `claude/fix-auto-weighting-multi-select`
**Issues**: AUDIT-032 (Auto checkbox silently collapses multi-weight comparison)
**Effort**: S
**Blocker for v1.0.0**: yes (small UX correctness fix; user expectation mismatch)
**Rationale**: UI change in `app.R` (replace Auto checkbox with a radio mode toggle) plus orchestration tweak in `server_analysis.R`. Independent of the above.
**Required tests**:
- `test_that("multi-weight comparison plot renders when multiple weights are selected")`

---

### Phase 2 — Reproducibility infrastructure (needed before any golden test can lock numbers)

#### 2.1 `claude/pin-dependencies-and-r-version`
**Issues**: AUDIT-004 (no dependency pinning), AUDIT-026 (remotes missing from DESCRIPTION)
**Effort**: M
**Blocker for v1.0.0**: yes
**Rationale**: Must precede any golden-number test, because the golden values are only meaningful relative to a pinned environment. Generates `renv.lock`, adds R version constraint to DESCRIPTION, adds minimum pins for drc/ggplot2/dplyr/rmarkdown, expands CI matrix.
**Required tests**:
- `test_that("DESCRIPTION declares R >= 4.2 and pinned versions for sensitive packages")` — parses DESCRIPTION and asserts presence

#### 2.2 `claude/version-source-of-truth`
**Issues**: AUDIT-008 (5 version strings), AUDIT-009 (CITATION.cff v0.9.0 mismatch)
**Effort**: S
**Blocker for v1.0.0**: yes
**Rationale**: Establish DESCRIPTION as the single source. Update all five locations to read from it. Create a release script. AUDIT-009 is resolved at the same time as the v1.0.0 Zenodo deposit.
**Required tests**:
- `test_that("version strings agree across DESCRIPTION, CITATION.cff, UI badge, REPORT_INFO")` — parses each location and asserts they match

#### 2.3 `claude/session-safety-pass`
**Issues**: AUDIT-010 (process-global Sys.setenv race), AUDIT-011 (theme_set global mutation)
**Effort**: M
**Blocker for v1.0.0**: yes
**Rationale**: Both mutate process-global state. Fixing them together is efficient because both involve refactoring `global.R` and `app.R` startup wiring.
**Required tests**:
- `test_that("two AppDriver sessions produce isolated output directories")` — shinytest2-based; requires Chrome in CI
- `test_that("loading global.R does not mutate ggplot2::theme_get()")` — sources global.R in a fresh session, asserts default theme unchanged

#### 2.4 `claude/report-metadata-and-replay`
**Issues**: AUDIT-018 (metadata block), AUDIT-021 (no replay script), AUDIT-020 (bootstrap seed contract)
**Effort**: M
**Blocker for v1.0.0**: yes
**Rationale**: These three define the reproducibility contract: every report has enough metadata to be replayed; bootstrap is deterministic given the seed; the replay script demonstrates the contract. Ships together so reviewers can exercise the full reproducibility claim.
**Required tests**:
- `test_that("report_metadata.json contains input hash, run UUID, package versions")`
- `test-replay.R::test_that("scripts/replay_report.R reproduces shipped example outputs")`
- `test_that("two renders of the golden RBA produce byte-identical model_stats.json")`

---

### Phase 3 — Test coverage (the regression net)

#### 3.1 `claude/repair-golden-number-test`
**Issues**: AUDIT-002 (placeholder expected values + std_concentrations name bug)
**Effort**: S
**Blocker for v1.0.0**: yes
**Rationale**: Must follow Phase 1 (numbers may shift) and Phase 2 (environment pinned) before golden values can be locked. Quick fix in `test-report-numbers.R`.
**Required tests**: the test itself becomes the regression test

#### 3.2 `claude/regression-tests-for-previously-fixed-bugs`
**Issues**: AUDIT-003 (no regression tests for seven previously fixed bugs)
**Effort**: M
**Blocker for v1.0.0**: yes
**Rationale**: Test-only branch. Builds on the now-working golden fixture from 3.1. One test per previously fixed bug grouped in `test-regression-known-bugs.R`.
**Required tests**: 7 named test_that blocks, one per bug listed in AUDIT-003

#### 3.3 `claude/upgrade-shinytest2-content-assertions`
**Issues**: AUDIT-015 (shinytest2 only checks file size)
**Effort**: S
**Blocker for v1.0.0**: yes
**Rationale**: Existing shinytest2 tests pass with all-NA reports. Upgrade them to read `model_stats.json` and `unknown_results_summary.csv` and assert real values. Coordinate with 3.1 so the expected values come from the same golden fixture.
**Required tests**: the upgraded shinytest2 files

#### 3.4 `claude/golden-snapshot-harness`
**Issues**: AUDIT-034 (orphan diff_golden_artifacts.R), AUDIT-042 (no documented expected output for shipped examples)
**Effort**: M
**Blocker for v1.0.0**: no (nice-to-have; AUDIT-002 + AUDIT-015 give the core regression coverage)
**Rationale**: Promotes `diff_golden_artifacts.R` to a real testing helper; adds `tests/testthat/golden/<assay>/` snapshots; documents expected output in `examples/README.md`. Defer to v1.0.1 because Phase 3.1 + 3.3 cover the regression-test minimum for v1.0.0.

---

### Phase 4 — Input handling robustness

#### 4.1 `claude/csv-separator-and-encoding`
**Issues**: AUDIT-012 (semicolon CSV unhandled), AUDIT-013 (mixed decimal separators), AUDIT-022 (no encoding handling)
**Effort**: M
**Blocker for v1.0.0**: yes
**Rationale**: Real-world European data is currently silently mis-imported. JOSS reviewers may upload `;`-separated CSV expecting success. Bundled because all three changes live in `read_file_raw()` and `detect_plate_location()`.
**Required tests**:
- `test_that("parse_plate_file handles ; separator")`
- `test_that("parse_plate_file errors loudly on mixed decimal separators")`
- `test_that("parse_plate_file handles UTF-8 BOM and Latin-1 sample IDs")`

#### 4.2 `claude/upload-size-and-format-doc`
**Issues**: AUDIT-024 (upload size never wired), AUDIT-033 (no input-format spec doc)
**Effort**: S
**Blocker for v1.0.0**: yes (AUDIT-024 is a one-liner; AUDIT-033 doc is needed for JOSS)
**Rationale**: Single-line code fix + new documentation file. Independent of 4.1.
**Required tests**:
- `test_that("getOption('shiny.maxRequestSize') equals MAX_UPLOAD_SIZE_MB * 1024^2")`

---

### Phase 5 — JOSS submission documentation

#### 5.1 `claude/joss-docs-pass`
**Issues**: AUDIT-028 (no CONTRIBUTING), AUDIT-029 (stale README + reduced CI matrix per decision), AUDIT-030 (statement of need), AUDIT-031 (system deps in install), **AUDIT-040 (roxygen on server modules — promoted from v1.0.1)**
**Effort**: **M+** (was M; promotion of AUDIT-040 adds ≈ 0.5 day of roxygen writing across `server/server_common.R`, `server/server_layout.R`, `server/server_config.R`, `server/server_upload.R`)
**Blocker for v1.0.0**: yes
**Rationale**: All documentation deliverables for JOSS, including function-level roxygen on server modules for contributor onboarding (AUDIT-040 decision). Bundled so the JOSS submission can be staged as a single PR. **CI matrix**: 3 jobs only — Ubuntu R 4.2, Ubuntu R release, Windows R release (per AUDIT-029 decision); add `R CMD check --as-cran` and `covr` (Codecov upload); defer `lintr` to v1.0.1. **roxygen**: documentation-only — do NOT generate a NAMESPACE.
**Required tests**:
- `test_that("CONTRIBUTING.md and CODE_OF_CONDUCT.md exist")` (file-existence assertions)
- CI matrix expanded as part of the same branch
- No new tests for AUDIT-040 — documentation-only

#### 5.2 `claude/cleanup-elisa-normalization-dead-code`
**Issues**: AUDIT-017 (dead ELISA branch in normalize_data)
**Effort**: S
**Blocker for v1.0.0**: no (defer to v1.0.1)
**Rationale**: Dead code is a JOSS-reviewer trap but not a correctness problem (formulas are algebraically equivalent and the branch is unreachable). Defer to keep v1.0.0 scope tight. Worth doing before publication of the JOSS paper since reviewers will read the code.
**Required tests**:
- `test_that("calculate_elisa_bb0 and normalize_data ELISA produce equal %B/B0 on the same fixture")` — even if `normalize_data` ELISA branch is removed, the test documents that the formulas were equivalent

---

### Phase 6 — Hardening and hygiene (post-v1.0.0)

#### 6.1 `claude/security-hardening`
**Issues**: AUDIT-025 (lang_code path-traversal defense)
**Effort**: S
**Blocker for v1.0.0**: no (low real-world risk in current deployment; defer to v1.0.1)
**Required tests**: security regression test for path traversal

#### 6.2 `claude/code-hygiene-pass`
**Issues**: AUDIT-027 (`<<-` in tryCatch handlers), AUDIT-035 (ggplot2 size→linewidth), AUDIT-036 (DEFAULT_STANDARDS duplication), AUDIT-037 (i18n TODOs), AUDIT-038 (TRANSLATIONS_CACHE closure), AUDIT-039 (dev-process markdown clutter), AUDIT-041 (multiwavelength tempdir), AUDIT-043 (tr_idx comment)
**Effort**: M
**Blocker for v1.0.0**: no
**Rationale**: Pure cleanup; defer to v1.0.1. Note: AUDIT-040 (roxygen on server modules) was **promoted to v1.0.0** in the Phase 5.1 bundle per the maintainer decision; it is no longer in this v1.0.1 list.
**Required tests**: tests already exist for behaviors touched by the changes; no new tests needed except AUDIT-036's standards-equality test.

#### 6.3 `claude/bootstrap-iterations-cleanup`
**Issues**: AUDIT-019 (literal 2000 fallback)
**Effort**: S
**Blocker for v1.0.0**: no
**Rationale**: Mechanical fix; defer.

#### 6.4 `claude/docx-reference-doc`
**Issues**: AUDIT-023 (reference.docx never shipped)
**Effort**: S
**Blocker for v1.0.0**: no
**Rationale**: DOCX output works without it; defer to v1.0.1 for branded polish.

---

## v1.0.0 release blocker checklist

The minimum set of branches that must merge before tagging v1.0.0:

| Order | Branch | Reason |
|---|---|---|
| 0 | `claude/audit-baseline-and-decisions` | Pre-fix baseline + maintainer decisions on AUDIT-001/005/006/008/021/029/040; must precede every numerical fix |
| 1 | `claude/fix-statistical-fallback-and-warnings` | AUDIT-001 (Critical, silent NA — fail-loud per maintainer decision) + AUDIT-014 (High, swallowed warnings) |
| 2 | `claude/fix-elisa-asymptote-and-curve-direction` | AUDIT-005 (unbounded top + 80–120 sanity warning per decision) + AUDIT-006 (pre-flight hard reject per decision) |
| 3 | `claude/fix-auto-weighting-degeneracy-flag` | AUDIT-007 (High) |
| 4 | `claude/fix-auto-weighting-multi-select` | AUDIT-032 (Medium UX correctness, cheap fix) |
| 5 | `claude/pin-dependencies-and-r-version` | AUDIT-004 (Critical JOSS blocker) + AUDIT-026 |
| 6 | `claude/version-source-of-truth` | AUDIT-008 (set DESCRIPTION to 1.0.0 per decision) + AUDIT-009 (High JOSS blockers) |
| 7 | `claude/session-safety-pass` | AUDIT-010 + AUDIT-011 (High, multi-user safety) |
| 8 | `claude/report-metadata-and-replay` | AUDIT-018 + AUDIT-020 + AUDIT-021 (Medium reproducibility, JOSS-relevant; replay-script value elevated by decision) |
| 9 | `claude/repair-golden-number-test` | AUDIT-002 (Critical regression-test gap) |
| 10 | `claude/regression-tests-for-previously-fixed-bugs` | AUDIT-003 (Critical) |
| 11 | `claude/upgrade-shinytest2-content-assertions` | AUDIT-015 (High, false confidence) |
| 12 | `claude/csv-separator-and-encoding` | AUDIT-012 + AUDIT-013 + AUDIT-022 (High, predictable failure on EU data) |
| 13 | `claude/upload-size-and-format-doc` | AUDIT-024 (Medium one-liner) + AUDIT-033 (Medium spec doc) |
| 14 | `claude/joss-docs-pass` | AUDIT-028 + AUDIT-029 (3-job CI matrix per decision) + AUDIT-030 + AUDIT-031 + AUDIT-040 (roxygen, promoted) |

**Total v1.0.0 blocker effort estimate:** 5× S + 9× M + 1× M+ = roughly 4–6 weeks of focused work if branches are sequenced; 2–3 weeks if multiple are landed in parallel by different contributors.

**Critical dependency chain (must be in this order):**
1. Phase 1 fixes statistics → 2. Phase 2 pins environment → 3. Phase 3 locks regression tests → 4–5. Phase 4 + Phase 5 polish for submission.

Phases 1, 2, and 4 within themselves can be parallelized across branches because the branches touch different files.

---

## v1.0.1 nice-to-have list

| Branch | Issues |
|---|---|
| `claude/golden-snapshot-harness` | AUDIT-034, AUDIT-042 |
| `claude/cleanup-elisa-normalization-dead-code` | AUDIT-017 |
| `claude/bootstrap-iterations-cleanup` | AUDIT-019 |
| `claude/docx-reference-doc` | AUDIT-023 |
| `claude/security-hardening` | AUDIT-025 |
| `claude/code-hygiene-pass` | AUDIT-027, AUDIT-035, AUDIT-036, AUDIT-037, AUDIT-038, AUDIT-039, AUDIT-040, AUDIT-041, AUDIT-043 |

---

## Post-publication list

These are improvements that are not appropriate for a 1.x patch but should be tracked for a future 2.0:

- A proper R package layout (NAMESPACE, exported functions, `R/` directory, unit-testable as a library). Currently the app is a Shiny scaffold that happens to live in a repo with a DESCRIPTION.
- Decouple the statistical pipeline (`reports/analysis_pipeline.R`, `reports/report_functions.R`) into an installable companion package that the Shiny app depends on; this would allow independent testing, citation, and reuse from non-Shiny contexts (RStudio scripts, Galaxy workflows).
- Per-user persistent state via a real database (currently auto-save uses per-session `.rds` files under `R_user_dir`).
- Interactive curve-fitting diagnostics with residual plots, leverage analysis, and Q-Q plots — currently the report shows the Brown-Forsythe result but no residual visualization.
- Localisation completeness: bring FR, RU, ZH out of beta (AUDIT-037) with native-speaker review.

---

## Publication risk statement

Once the v1.0.0 blocker checklist (15 branches, Phase 0 through Phase 5) is addressed, three classes of residual risk should be acknowledged as **limitations in the JOSS software paper** rather than fixed in code. These correspond directly to maintainer decisions captured in this branch.

**1. Interpolation fallback deliberately removed (AUDIT-001).** When both LL.4 and LL.3 fail to converge, the app now refuses to generate a report rather than falling back to log-linear interpolation. Pathological standards data — flat response, all-zero, fewer than 4 unique non-degenerate concentrations — is no longer "papered over" but surfaces as a clear error. This is the correct behaviour for a scientific tool, but constitutes a UX regression for the rare cases where interpolation would have produced anything usable. The paper should state this explicitly so users with degraded plates understand why the report fails.

**2. Scope of supported assay formats (AUDIT-006).** The app supports **decreasing (competitive) dose-response curves only** — RBA radioligand displacement, competitive ELISA. Agonist binding, sandwich ELISA, and other formats producing increasing response with concentration are rejected at the Tab 5 pre-flight stage with a modal dialog. The paper's "limitations" section should state this scope restriction and direct users with increasing-curve data to other tools (`drc` directly, GraphPad Prism, or PROAST).

**3. Unbounded ELISA top asymptote with sanity envelope (AUDIT-005).** The fitted top asymptote `d` is no longer constrained to ≤ 100% B/B0 — individual standards can plausibly exceed 100 due to measurement noise and matrix effects, and constraining `d` biased IC50 estimates. A sanity warning is emitted for fits where `d > 120` or `d < 80`, signalling real data problems (poor B0 controls, severe matrix effects, wrong assay orientation). The paper should document the 80–120 envelope as the expected range and note that fits outside it produce a warning rather than silent acceptance.

**4. Reproducibility caveat for environments beyond the lockfile (AUDIT-004 + AUDIT-021).** Even with `renv.lock` capturing the v1.0.0 environment, users who upgrade dependencies beyond the lockfile assume responsibility for re-validating against the shipped reference outputs. The paper should direct users to `scripts/replay_report.R` (AUDIT-021) as the recommended sanity check after any environment change and should state that JOSS-published numbers were produced with the locked environment captured at the release SHA.

# RBAElisaApp News

## 1.0.0 — 2026-05-25

### New features
- Spanish smoke-test render (`elisa_full_es.html`) added to CI verification pipeline; `verify_report.R` asserts no literal i18n key tokens appear in `*_es.html` outputs (B.4 assertion).
- DRC plot caption: a translatable one-line hint is now emitted below the combined dose-response plot in HTML output (`.bs-plot-caption` CSS class).

### UX improvements
- "How to read this table" legend is now expanded by default (`<details open>`) instead of collapsed.
- Redundant "Sample Type" column dropped from the sample-results summary table; all entries are always "Sample", so the column carried no information.

### Internal
- `render_fixture()` in `scripts/render_examples.R` accepts a `lang` parameter (default `"en"`), enabling non-English smoke-test renders.
- `global.R` version fallback updated from `"1.0.0-dev"` to `"1.0.0"`.

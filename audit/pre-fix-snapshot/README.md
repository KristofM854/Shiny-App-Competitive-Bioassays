# Pre-Fix Numerical Baseline

This directory captures **what the app produces today** — the frozen
reference against which every subsequent statistical-fix branch is
measured. Branch: `claude/audit-baseline-and-decisions` (Phase 0 of the
v1.0.0 audit).

## Purpose

After this branch lands, every later fix branch
(`claude/fix-statistical-fallback-and-warnings`,
`claude/fix-elisa-asymptote-and-curve-direction`, ...) will re-run the
same capture script and diff the new outputs against the snapshots
under this directory. The numerical delta produced by each fix becomes a
quantitative line item in that branch's PR description, rather than a
qualitative claim.

## What is captured per example

Each subdirectory (`rba_stx/`, `elisa_cortisol/`, `multiwavelength/`)
contains:

| File | Description |
|---|---|
| `long_data_output.csv` | 96-row long-format input data (per-well) |
| `unknown_results.csv` | Per-well predicted sample concentrations |
| `unknown_results_summary.csv` | Per-replicate-group summary |
| `model_stats.json` | R², RMSE, IC50, Hill slope, weight method |
| `analysis_report.html` | The rendered HTML report |
| `baseline_meta.json` | Git SHA, R version, sessionInfo, capture timestamp |

The multi-wavelength directory additionally contains
`long_data_output_<wl>.csv` per wavelength plus `wavelength_manifest.json`.

## How to capture (or refresh)

From a fresh R session at the repository root:

```bash
Rscript scripts/capture_baseline.R
```

Or interactively in R:

```r
source("scripts/capture_baseline.R")
```

The script:
1. Builds the four plate-layout matrices (type / id / dilution / replicate)
   via the `create_*_matrix()` helpers in `utils/utils_plate.R`.
2. Reads each shipped example dataset (`examples/rba_stx_example.csv`,
   `examples/elisa_cortisol_example.csv`,
   `tests/testthat/fixtures/multiwave_synthetic.csv`).
3. Writes the same sidecar JSONs the live Shiny pipeline writes
   (`assay_config.json`, `analysis_config.json`, `qc_params.json`,
   `selected_formats.json`, `notes.json`, `report_language.json`).
4. Calls `rmarkdown::render()` directly on
   `reports/unified_analysis_template.Rmd` (single-plate) or
   `reports/multiwavelength_analysis_template.Rmd` (multi-wavelength).
5. Writes `baseline_meta.json` per example with the git SHA, R version,
   and full `sessionInfo()` string.

This matches the proven non-Shiny render path used by
`tests/testthat/test-report-numbers.R`.

## Required R packages

The script will fail to source if any of these are missing:
`drc`, `rmarkdown`, `knitr`, `kableExtra`, `dplyr`, `ggplot2`,
`ggrepel`, `ggtext`, `ggthemes`, `htmltools`, `htmlwidgets`,
`jsonlite`, `purrr`, `readr`, `readxl`, `tibble`, `tidyr`, `scales`,
`patchwork`, `plotly`, `car`, `digest`, `glue`, `rhandsontable`,
`rintrojs`, `shiny`, `shinyFeedback`, `shinycssloaders`, `shinyjs`,
`stringr`, `withr`. The same set declared in `DESCRIPTION` and
loaded by `global.R`.

Pandoc must be on PATH for `rmarkdown::render()` to produce HTML.

## Capture status

**This capture has not yet been executed.** The branch
`claude/audit-baseline-and-decisions` shipped the script and the empty
snapshot directories but the R environment used to author this branch
does not have R installed, so the script could not run here.

**Action required from the maintainer:**
1. Check out this branch on a machine with R + pandoc.
2. Run `Rscript scripts/capture_baseline.R` from the repository root.
3. Spot-check each subdirectory's `model_stats.json` for plausibility
   (R² between 0.9 and 1.0 for the RBA STX example; IC50 around 1e-9 to
   1e-8 mol/L; weight_method populated).
4. Commit the captured files with: `git add audit/pre-fix-snapshot/`
   then `git commit -m "docs(audit): freeze pre-fix-snapshot baseline at <SHA>"`.
5. Push and update the PR description with the captured headline numbers.

Once captured, this README will be amended to record the actual git SHA,
capture date, R version, and per-example headline values
(R², IC50, n_standards, n_replicate_groups) so reviewers can sanity-check
the baseline at a glance.

## Why the snapshot must precede the fix branches

The fixes in Phase 1 (AUDIT-001, AUDIT-005, AUDIT-006, AUDIT-007) will
change numerical output. AUDIT-005 in particular (unbounded ELISA top
asymptote) will produce different IC50 values for any ELISA fit. To
quantify "how much" each fix changes results — and to spot any
fix that changes results unintentionally beyond its stated scope —
we need a frozen "before" snapshot at a known git SHA.

The snapshot is **not** a regression-test gold (that comes later in
AUDIT-002 / AUDIT-034). It is a diff baseline. Both are useful and they
serve different purposes:

- **This snapshot**: shows the maintainer and reviewers how much each
  fix shifts the numbers, branch by branch, in human-readable form.
- **AUDIT-002 golden test**: a CI-enforced numerical assertion that
  guards against silent regressions after v1.0.0 ships.

The snapshot is captured once on this branch and frozen.
The golden test is captured once Phase 1 + Phase 2 settle and the new
numerical baseline is locked.

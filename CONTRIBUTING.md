# Contributing to the RBA & ELISA Analysis Suite

Thank you for your interest in contributing. This document covers the branch
model, PR process, local test recipe, golden-value capture procedure, and
release process. For the full implementation plan, see
[`audit/stage2-plan.md`](audit/stage2-plan.md).

---

## Branch model

Active development uses descriptive branch names with a prefix that indicates
origin:

- `claude/` — branches created during the v1.0.0 audit implementation (see
  [`audit/stage2-issues.md`](audit/stage2-issues.md)). Each branch addresses
  one or more AUDIT-IDs.
- `fix/`, `feat/`, `docs/` — conventional branches for maintainer-initiated
  work.

Branches are merged manually by the maintainer via pull request; force-push
to `main` is not permitted.

---

## PR process

One branch per coherent unit of work as defined in
[`audit/stage2-plan.md`](audit/stage2-plan.md). The PR description should
include:

- The AUDIT-IDs closed by the PR.
- A brief summary of any numerical output changes detected against
  `audit/pre-fix-snapshot/` (run `scripts/capture_baseline.R` on a clean
  checkout, then diff against the snapshot).
- Confirmation of which source files were modified (to keep documentation-only
  PRs clearly separated from code changes).

---

## Local test recipe

### System dependencies

| Dependency | Required for | Install |
|---|---|---|
| **pandoc** | All report formats | Bundled with RStudio; `apt install pandoc` (Linux); `brew install pandoc` (macOS); [pandoc.org](https://pandoc.org/installing.html) (Windows) |
| **TinyTeX** | PDF report output only | `tinytex::install_tinytex()` in R |
| **Chrome or Chromium** | `shinytest2` test suite only (not needed by end users) | [chromium.org](https://www.chromium.org/getting-involved/download-chromium/) or system package manager |

### R packages

```r
install.packages(c("testthat", "shinytest2", "chromote"))
```

### Run all tests

```r
testthat::test_dir("tests/testthat", stop_on_failure = TRUE)
```

Run from the **repository root**. The helper `tests/testthat/helper-setup.R`
resolves all paths relative to repo root.

- `shinytest2` tests are automatically skipped when Chrome is not available
  (they call `skip_if_not_installed("shinytest2")` and
  `chromote::find_chrome()`).
- Tests that need unavailable packages call `skip_if_not_installed()` so the
  suite degrades gracefully.

---

## Golden-value capture procedure

After a deliberate numerical change, the reference values in
`tests/testthat/test-report-numbers.R` need to be recaptured. The full
procedure will be documented in v1.0.0 alongside the test-repair branch
(`claude/test-repair`). For now:

1. Run `Rscript scripts/capture_baseline.R` from the repo root to update
   `audit/pre-fix-snapshot/` with the new numerical output.
2. Diff the new snapshot against the committed one to characterise the delta.
3. Update the hard-coded expected values in `test-report-numbers.R` to match.
4. Commit the updated test file and snapshot together with the code change.

---

## Release process

1. Bump `Version:` in `DESCRIPTION` to the new version (e.g. `1.0.0`).
2. Update `CITATION.cff`: set `version` and `date-released`.
3. Run `Rscript scripts/capture_baseline.R` against the release candidate to
   produce the new reference snapshot under `audit/pre-fix-snapshot/`.
4. Tag the release: `git tag -a v1.0.0 -m "v1.0.0"` and push the tag.
5. Create the Zenodo release from the tag.
6. Update `CITATION.cff` with the new version DOI from Zenodo.
7. Open a PR to merge `CITATION.cff` with the DOI update back to `main`.

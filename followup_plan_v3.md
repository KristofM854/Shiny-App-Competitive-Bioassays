# Follow-up Plan: Minor Post-Review Fixes + Guided Tour Repair

**Repo:** `KristofM854/Shiny-App-Competitive-Bioassays`
**Scope:** Two leftover items from the v2 plan review, plus a proper diagnosis and fix for the guided-tour regressions.
**Ground rule (unchanged):** No changes to the analytical math.

---

## Part A — Leftover items from v2 review

### A.1 Wire `extraction_volume_report_note` into the Rmd (Spanish localization fix)

**File:** `reports/unified_analysis_template.Rmd`, chunk `tissue-normalization-traceability`

The i18n key `extraction_volume_report_note` is already defined in both `en` and `es` blocks of `i18n.R` (Phase 1.3 of the v2 plan), but the Rmd chunk never calls it. Instead, the blockquote note about extraction volume is hard-coded in English. Result: Spanish reports show English text for that one paragraph.

Current code (around the tissue traceability blockquote):

```r
cat("> **Note on extraction volume:** $V_{extraction}$ is the *total* volume ",
    "the tissue was extracted into, before any plate-loading dilutions. ",
    "For example, 50 mg of tissue homogenized in 500 \u00b5L of buffer, ",
    "then diluted 1:10 before plate loading, is recorded as ",
    "$V_{extraction}$ = 500 \u00b5L here and the 1:10 dilution is ",
    "recorded in the DilutionFactor matrix (as 0.1 or `1:10`).\n\n", sep = "")
```

Replace with:

```r
cat("> ", tr("extraction_volume_report_note", lang), "\n\n", sep = "")
```

Then update the `extraction_volume_report_note` values in `i18n.R` so both EN and ES versions of the string include the worked example ("50 mg ... 500 µL ... 1:10 ..."). The current Spanish string is already ~correct in content but shorter than the English blockquote; align both so that the English version matches what's currently hard-coded.

**Acceptance criteria:**
- Grep `reports/unified_analysis_template.Rmd` for `"V_{extraction} = 500"` returns zero matches (the hard-coded blockquote is gone).
- Rendering a report with `lang = "es"` shows a Spanish version of the extraction-volume note.
- Rendering with `lang = "en"` shows the same English note as before.

---

### A.2 Run the full test suite and record results

**Commands (from repo root):**

```r
testthat::test_dir("tests/testthat")
```

Prior to committing A.1, the STATS_CONFIG refactor in Phase 2.6 touched:
- `compute_layered_uncertainty()` — bootstrap iteration count
- Outlier detection chunk — Shapiro alpha, MAD threshold
- `classify_range()` / `flag_range()` — ED respLev choices
- `assess_heteroscedasticity()` — variance-ratio thresholds
- CI truncation in sample-results / replicate summary

The existing `tests/testthat/` directory covers:
- `test-utils_plate.R` (matrix construction + long-format conversion)
- `test-utils_import.R` + `test-smoke-import.R` (file import)
- `test-utils_normalization.R` (ELISA %B/B0)
- `test-dilution_parsing.R` (dilution cell parsing)
- `test-report_functions.R` (validate_input_data, CV, axis labels)
- `test-smoke-format-helpers.R` (HTML/DOCX helpers)
- `test-integration.R` (full RBA + ELISA pipeline)

None of these directly exercise `STATS_CONFIG` references. That's tolerable for now, but:

**New lightweight test to add: `tests/testthat/test-stats-config.R`**

```r
test_that("STATS_CONFIG is fully defined and referenced correctly", {
  expect_true(exists("STATS_CONFIG"))
  required_keys <- c("bootstrap_iterations", "mad_outlier_threshold",
                     "dixon_alpha", "shapiro_alpha",
                     "ec20_resp_level", "ec80_resp_level",
                     "heteroscedasticity_variance_ratio_strong",
                     "heteroscedasticity_variance_ratio_moderate",
                     "ci_truncation_floor")
  expect_true(all(required_keys %in% names(STATS_CONFIG)))

  # Sanity checks on values
  expect_equal(STATS_CONFIG$bootstrap_iterations, 1000)
  expect_equal(STATS_CONFIG$mad_outlier_threshold, 3)
  expect_equal(STATS_CONFIG$ec20_resp_level, 80)
  expect_equal(STATS_CONFIG$ec80_resp_level, 20)
})

test_that("compute_layered_uncertainty uses STATS_CONFIG$bootstrap_iterations", {
  # Run with bootstrap and confirm the internal seed call works
  well_preds <- data.frame(
    well = c("A1", "A2", "A3"),
    predicted_conc = c(100, 110, 105),
    ci_lower_model = c(80, 88, 84),
    ci_upper_model = c(120, 132, 126)
  )
  result <- compute_layered_uncertainty(
    well_preds, c(100, 110, 105), ci_method = "bootstrap"
  )
  expect_true(is.finite(result$ci_lower_combined))
  expect_true(is.finite(result$ci_upper_combined))
  expect_lte(result$ci_lower_combined, result$ci_upper_combined)
})
```

Place the new test file at `tests/testthat/test-stats-config.R`.

**Acceptance criteria:**
- `testthat::test_dir("tests/testthat")` passes with zero failures.
- The new test file runs in under 5 seconds.

---

## Part B — Guided tour repair

### B.0 Diagnosis (do this first, before writing code)

The current tour was rewritten to use 17 element selectors across all 5 wizard tabs, with a custom `onbeforechange` JS callback that messages R to switch tabs via `updateTabsetPanel()`. It doesn't work correctly. The plausible root causes, in rough order of likelihood:

**Cause 1 (primary): Race condition in the custom `onbeforechange` callback.**
The current callback in `server_common.R`:

```js
function(targetElement) {
  var $pane = $(targetElement).closest('.tab-pane');
  if ($pane.length && !$pane.hasClass('active')) {
    var targetTab = $pane.data('value');
    if (targetTab) {
      Shiny.setInputValue('tour_set_tab', targetTab,
                          {priority: 'event'});
    }
  }
}
```

`Shiny.setInputValue()` is asynchronous — it queues a message over the websocket, R's observer fires, `updateTabsetPanel()` sends back a message, the Shiny client applies it, and only *then* the pane becomes `.active` (gets `display: block`). Intro.js does *not* wait for any of this. It proceeds immediately to its own `nextStep()` logic, reads the DOM geometry of a still-hidden element (width 0, height 0, or off-screen), and places the highlight/tooltip at nonsensical coordinates. This is exactly the symptom Kristof reports: "help window is not highlighting the correct part of the interface."

**Cause 2: `conditionalPanel`-wrapped elements may not exist in the DOM at tour start.**
- `#qc_section` lives inside `conditionalPanel(condition = "input.assay_type == 'rba'", ...)`.
- `#tissue_weight_section` lives inside `conditionalPanel(condition = "input.assay_type == 'elisa'", ...)`.

When the assay condition is false, Shiny does not render the inner div at all (or renders it with `display: none` depending on Shiny version). Even the assay-branched tour code only adds the right step for the current assay — but Shiny might still strip the element from the DOM. Per the rintrojs maintainer's own comment on issue #49: *"all elements of the intro need to be in the DOM when the intro starts."*

**Cause 3: Tour starts on whichever tab the user is currently on.**
The code calls `updateTabsetPanel(session, "wizard_tabs", selected = "tab_config")` immediately followed by `introjs(session, ...)`. The `updateTabsetPanel` message is queued. If the user happens to already be on `tab_config`, there's no timing issue for the first step, but any other starting tab creates a lag before the DOM reflects the switch — and `introjs()` fires before the first pane is active. The first step's target (`#language_toggle_section`) happens to live outside the tabsetPanel, which is why the tour *starts* showing correctly, masking the bug.

**Cause 4: Matrix grid cells are in an unintuitive order.**
Not a bug per se, but a UX issue that makes the tour feel wrong. The layout is:
```
  Row 1: [Type]    [Dilution]
  Row 2: [ID]      [Replicate]
```
The current tour order is Type → ID → Dilution → Replicate, which zigzags diagonally instead of following reading order. The steps are correct semantically, they just jump around visually. Fix either the tour order or the grid layout, not both.

**Cause 5: `conditionalPanel` condition uses JS expression `input.assay_type == 'elisa'`.** These panels are *rendered to the DOM* with `display: none` when the condition is false, not stripped — so the conditional-panel branching in the tour actually *is* safe in most Shiny versions. This contradicts Cause 2 slightly, but the safe choice is still to only add the relevant step for the current assay (which the code already does).

**Action:** Before writing any fix, Claude Code should verify Causes 1–3 by adding a temporary `console.log` inside the `onbeforechange` callback that prints `targetElement.id` and `$pane.hasClass('active')`, then opening DevTools, starting the tour, and stepping through. The logs will show which specific step(s) have the race.

---

### B.1 Replace the custom `onbeforechange` callback with rintrojs's built-in `switchTabs`

**File:** `server_common.R`, inside the `observeEvent(input$start_tour, ...)` block.

The `rintrojs` package ships a `readCallback("switchTabs")` helper that is *synchronous* — it calls `jQuery.fn.tab('show')` directly on the target nav link, so by the time intro.js reads DOM geometry, the pane is already `.active`. It walks up from the target element via `.closest("[data-value]")` (a pane), finds the sibling nav link with matching `data-value` in the tabset, and clicks it. This works for both `type="tabs"` and `type="pills"` tabsetPanels.

Remove the custom JS callback and the companion `observeEvent(input$tour_set_tab, ...)` observer. Replace with:

```r
introjs(session,
        options = list(
          steps = tour_steps[, c("element", "intro")],
          nextLabel = tr("tour_next", lang),
          prevLabel = tr("tour_prev", lang),
          skipLabel = tr("tour_skip", lang),
          doneLabel = tr("tour_done", lang),
          showProgress = TRUE,
          scrollToElement = TRUE
        ),
        events = list(
          onbeforechange = readCallback("switchTabs")
        ))
```

Delete the `observeEvent(input$tour_set_tab, ...)` block entirely — it's no longer needed.

**Why this works where the custom version didn't:**
- Uses `jQuery.fn.tab('show')` directly, which triggers the `shown.bs.tab` event synchronously on the same JS tick.
- `switchTabs` in rintrojs 0.3.x uses `[data-value]` selectors, which match the Shiny tabsetPanel structure (each pane has `data-value="tab_config"`, etc.).
- No websocket round-trip, no race.

**Caveat (the "tabs with pill nav" test):** Shiny's `tabsetPanel(type = "pills")` produces nav links with `data-toggle="pill"` rather than `data-toggle="tab"`. Bootstrap 3's `$().tab('show')` works for both. Verify this once by stepping through the tour after the change — if the pill switch fails, fall back to the hybrid solution in B.2.

---

### B.2 Hybrid fallback: synchronous JS tab switch + `setTimeout` nudge (only if B.1 fails on pills)

If `readCallback("switchTabs")` genuinely does not work with `type = "pills"` (unlikely, but possible), use this drop-in replacement for the custom callback:

```js
function(targetElement) {
  var $pane = $(targetElement).closest('.tab-pane');
  if ($pane.length && !$pane.hasClass('active')) {
    var targetTab = $pane.attr('data-value');
    if (targetTab) {
      // Click the corresponding pill directly - synchronous
      var $link = $('a.nav-link[data-value="' + targetTab + '"], '
                  + 'a[data-toggle="pill"][data-value="' + targetTab + '"], '
                  + 'a[data-toggle="tab"][data-value="' + targetTab + '"]');
      if ($link.length) {
        $link.first().tab('show');
      }
    }
  }
}
```

This is synchronous and doesn't require any Shiny round-trip. Wrap in `I()` just like the current custom callback.

Do **not** implement B.1 and B.2 at the same time — they're alternatives. Try B.1 first; only fall back to B.2 if pills actually break.

---

### B.3 Fix the tour step order for the 2×2 matrix grid

**File:** `server_common.R`

Current step order (inside the `tab_layout` block):

```
matrix_type_section → matrix_id_section → matrix_dilution_section → matrix_replicate_section
```

This zigzags diagonally through the 2×2 grid. Change to reading order (left-to-right, top-to-bottom):

```
matrix_type_section → matrix_dilution_section → matrix_id_section → matrix_replicate_section
```

At the same time, update the corresponding i18n keys:
- `tour_matrix_type` — stays
- `tour_matrix_dilution` — moves to 2nd position
- `tour_matrix_id` — moves to 3rd position
- `tour_matrix_replicate` — stays

The step *text* (describing what each matrix does) stays identical — only the order in `layout_steps` changes.

**Acceptance criteria:**
- Stepping through the tour on the Plate Layout tab moves the highlight smoothly across the grid without diagonal jumps.

---

### B.4 Ensure `conditionalPanel` sections are reliably in the DOM

**File:** `app.R`

The `#qc_section` (RBA) and `#tissue_weight_section` (ELISA) are inside `conditionalPanel()`. In Shiny 1.7+, conditional panels render their contents to the DOM with `display: none` when the condition is false, so they *should* still be findable by intro.js. However, intro.js refuses to highlight elements with `display: none` — it reports "Element cannot be found" and jumps to the next step, which is the second class of broken-looking tour behavior.

Two options, in order of preference:

**Option A (recommended): Skip those steps if the panel is hidden.**
In `server_common.R`, before building the tour steps, check the assay type:

```r
assay <- input$assay_type %||% "rba"

# ... existing step construction up through matrix_replicate ...

# Assay-specific layout extras - only include if the condition is met
if (assay == "rba") {
  layout_steps <- rbind(layout_steps, data.frame(
    element = "#qc_section",
    intro = tr("tour_qc_rba", lang),
    tab = "tab_layout",
    stringsAsFactors = FALSE
  ))
} else if (assay == "elisa") {
  layout_steps <- rbind(layout_steps, data.frame(
    element = "#tissue_weight_section",
    intro = tr("tour_tissue_weights", lang),
    tab = "tab_layout",
    stringsAsFactors = FALSE
  ))
}
```

This is already what the code does. The bug would only occur if the conditional panel were collapsed before the user clicks "Start Tour" — which shouldn't happen because `input$assay_type` drives both the conditional panel *and* the tour branching. Verify this works as expected, and if the assay-specific step still jumps silently on some assay types, move to Option B.

**Option B (fallback): Force-render both conditional panels via a hidden wrapper that opens briefly during the tour.**
If intro.js truly can't find the element, use `shinyjs::show("qc_section")` / `shinyjs::show("tissue_weight_section")` at tour start, run the tour, then `shinyjs::hide` them on tour completion via the `oncomplete` event. This is uglier and only needed if Option A doesn't work.

---

### B.5 Verify every tour step element exists before passing to intro.js

**File:** `server_common.R`

Add a defensive check after `tour_steps` is assembled:

```r
# Sanity: drop any step whose element ID doesn't start with '#' or is NA
valid_mask <- !is.na(tour_steps$element) & nzchar(tour_steps$element) &
              grepl("^#[A-Za-z][-_A-Za-z0-9]*$", tour_steps$element)
if (!all(valid_mask)) {
  dropped <- tour_steps$element[!valid_mask]
  warning("Dropping invalid tour selectors: ", paste(dropped, collapse = ", "))
  tour_steps <- tour_steps[valid_mask, , drop = FALSE]
}
```

This prevents a single bad selector from breaking the whole tour, and surfaces the problem as a server-console warning so it's catchable during development.

---

### B.6 Smoke-test the tour end-to-end

No automated test for this — it's interactive. Document the manual test in the README or a `TESTING.md`:

**Test procedure:**
1. Launch the app with `shiny::runApp(".")` from the repo root.
2. Close the welcome modal.
3. With RBA assay selected (default), click "🚀 Start Guided Tour".
4. Walk through every step (Next → Next → ...). For each step, verify:
   - The correct wizard tab is active.
   - The highlighted element is visible and properly boxed.
   - The tooltip text matches the element being highlighted.
   - No console errors in the browser DevTools.
5. Hit "Finish" on the last step. Tour should end cleanly.
6. Switch to ELISA assay. Click "Start Guided Tour" again.
7. Repeat step 4 — the only difference should be that the RBA QC step is replaced by the tissue-weights step.
8. Switch app language to Spanish. Start the tour. Every tooltip should appear in Spanish.

Log any regressions in `tour_regressions.md` for the next session.

---

## Execution order

1. **A.1** — 5-minute fix, unlocks Spanish localization
2. **A.2** — add the `test-stats-config.R` file and run the suite
3. **B.0** — diagnose first (add console.log, step through once)
4. **B.1** — swap to `readCallback("switchTabs")`, remove the custom callback and its observer
5. **B.3** — reorder matrix steps
6. **B.4** — verify conditional-panel elements render
7. **B.5** — add the defensive selector filter
8. **B.6** — manual smoke test; document results

If B.1 fails on pill navigation, fall back to **B.2** and proceed to B.3.

---

## Out of scope (do not do in this session)

- Any change to the underlying tour content/wording (i18n keys remain the same).
- Any change to the wizard layout beyond the 2×2 grid reordering (B.3 reorders *steps*, not the grid).
- Tour persistence / "don't show again" checkboxes.
- Keyboard shortcuts for tour navigation.
- Integrating `introBox()` inline in UI (current `steps`-based approach is fine and already i18n-reactive).

---

## Acceptance checklist

- [ ] A.1: `extraction_volume_report_note` is called via `tr()` in the Rmd; EN and ES both render the full note.
- [ ] A.1: Grep for `V_{extraction} = 500` in `reports/unified_analysis_template.Rmd` returns zero matches.
- [ ] A.2: `tests/testthat/test-stats-config.R` added and passing.
- [ ] A.2: Full `testthat::test_dir("tests/testthat")` run recorded with pass/fail counts.
- [ ] B.0: Console-log diagnosis captured in a short note before writing fixes.
- [ ] B.1: Custom `onbeforechange` JS callback replaced with `readCallback("switchTabs")`.
- [ ] B.1: `observeEvent(input$tour_set_tab, ...)` observer removed from `server_common.R`.
- [ ] B.3: Matrix step order changed from Type → ID → Dilution → Replicate to Type → Dilution → ID → Replicate.
- [ ] B.4: Verified both assay-specific conditional-panel steps highlight correctly.
- [ ] B.5: Defensive selector filter added; a deliberately malformed step ID produces a server warning but doesn't crash the tour.
- [ ] B.6: Manual test procedure run end-to-end in RBA and ELISA assays, in English and Spanish.

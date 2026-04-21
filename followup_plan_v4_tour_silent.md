# Follow-up Plan v4: Guided Tour Shows No Tooltip At All

**Repo:** `KristofM854/Shiny-App-Competitive-Bioassays`
**Scope:** Fix the guided tour so that clicking "Start Guided Tour" actually
displays tooltips. Current symptom: cursor changes (intro.js is initializing),
but no tooltip, no highlight, no overlay appears. Not even step 1.
**Ground rule:** No changes to the analytical math. No changes to i18n text.

---

## 0. What you will NOT do before diagnosing

Do not blindly apply the rintrojs `readCallback("switchTabs")` fix from
`followup_plan_v3.md` (section B.1) as the first action. That fix addresses a
different previously reported symptom ("tour highlights wrong section on tab
switches"). The current symptom is stronger — no tooltip appears at all, not
even on step 1 whose target (`#language_toggle_section`) lives outside the
tabsetPanel and does not need a tab switch. Applying B.1 first may or may not
fix this; you must verify empirically.

This plan is written in execution order. Step 1 is diagnosis. Do not skip it.

---

## 1. Empirical diagnosis (required first step)

### 1.1 Add temporary diagnostic logging to the tour trigger

**File:** `server_common.R`, inside `observeEvent(input$start_tour, { ... })`

At the very top of the observer body, before any tour_steps construction,
inject a browser-side `console.group` that logs:
- that the observer fired
- the current `input$assay_type` value
- the current `input$app_language` value

Use `shinyjs::runjs()` (shinyjs is already loaded) to push the log:

```r
shinyjs::runjs(sprintf(
  "console.group('[TOUR DIAGNOSTIC]');
   console.log('tour trigger fired at', new Date().toISOString());
   console.log('assay_type:', %s);
   console.log('app_language:', %s);
   console.log('introJs available:', typeof introJs);
   console.log('rintrojs global:', typeof rintrojs);
   console.log('jQuery available:', typeof jQuery);
   console.groupEnd();",
  jsonlite::toJSON(input$assay_type %||% "rba", auto_unbox = TRUE),
  jsonlite::toJSON(input$app_language %||% "en", auto_unbox = TRUE)
))
```

### 1.2 Log each tour step's element, and whether it's in the DOM and visible

Immediately before the `introjs(session, options = ..., events = ...)` call,
add another runjs block that:
- prints the full resolved `tour_steps` array
- for each step, queries `document.querySelector(step.element)` and logs:
  - whether the element exists
  - whether it has non-zero width/height
  - whether any ancestor has `display: none` or `visibility: hidden`
  - the computed z-index

Concretely:

```r
steps_json <- jsonlite::toJSON(tour_steps[, c("element", "intro")],
                               auto_unbox = FALSE)
shinyjs::runjs(sprintf(
  "(function() {
    var steps = %s;
    console.group('[TOUR DIAGNOSTIC] resolved steps');
    steps.forEach(function(s, i) {
      var el = document.querySelector(s.element);
      if (!el) {
        console.warn(i, s.element, 'NOT FOUND IN DOM');
        return;
      }
      var rect = el.getBoundingClientRect();
      var style = window.getComputedStyle(el);
      var hidden = false;
      var walker = el;
      while (walker) {
        var ws = window.getComputedStyle(walker);
        if (ws.display === 'none' || ws.visibility === 'hidden') {
          hidden = walker.id || walker.tagName;
          break;
        }
        walker = walker.parentElement;
      }
      console.log(i, s.element,
        'w=' + rect.width.toFixed(0),
        'h=' + rect.height.toFixed(0),
        'z=' + style.zIndex,
        hidden ? ('HIDDEN by ' + hidden) : 'visible');
    });
    console.groupEnd();
  })();", steps_json))
```

### 1.3 Log errors from inside the intro.js lifecycle

intro.js swallows some errors silently. Inject a global error listener before
starting the tour:

```r
shinyjs::runjs(
  "window.addEventListener('error', function(e) {
     console.error('[TOUR DIAGNOSTIC] window.error:',
                   e.message, 'at', e.filename + ':' + e.lineno);
   }, {once: false});"
)
```

### 1.4 What to do with the diagnostic output

Run the app, open browser DevTools Console, click "Start Guided Tour", and
capture the console output. Based on what you see, branch:

| Symptom in console | Root cause | Go to |
|--------------------|------------|-------|
| `introJs available: undefined` | rintrojs JS failed to load | § 2.1 |
| Step 1 reports `NOT FOUND IN DOM` or `w=0 h=0` | target element is hidden/absent | § 2.2 |
| "onbeforechange is not a function" or similar TypeError | callback contract bug (v3 Cause 1) | § 2.3 |
| `window.error` about modal-backdrop | leftover Bootstrap modal blocking tour | § 2.4 |
| Everything looks normal in console but no tooltip | CSS hiding tooltip | § 2.5 |
| Observer never fires | `input$start_tour` observer dead | § 2.6 |

Do NOT proceed past this point until you have captured console output and
identified which branch applies. Save the console output to
`tour_diagnosis_YYYYMMDD.txt` in the repo root for the record (then delete it
after the fix is confirmed).

---

## 2. Branch fixes

### 2.1 rintrojs JS assets not loaded

**Check:** Is `introjsUI()` actually called in the UI? Grep `app.R`:
```
introjsUI()
```
It should appear in the top-level `fluidPage(...)` list, before the tabsetPanel.
If missing, add it at the top of `fluidPage()` alongside `useShinyjs()`.

If it is present, verify that no other Shiny widget is reinitialising the head:
check for any `tags$head()` later in the UI that might replace (not append to)
the head.

### 2.2 Target element hidden or missing

**Most likely culprits given the current UI:**

- `#language_toggle_section` lives in a top-level `div(id = ...)` before the
  tabsetPanel. This should always be visible. If § 1.2 reports w=0 or hidden,
  check that the div opening in `app.R` is not accidentally closed early.

- For steps on tabs 2–5, the pane containing the target is
  `display:none` until activated. If the tour starts and step 1 is on
  tab_config, it should be visible — but if intro.js's lifecycle runs
  onbeforechange *before* step 1 and the callback does not switch to
  tab_config, nothing happens.

**Fix:** Force tab_config to be active *synchronously* before calling
`introjs()`. The current code uses `updateTabsetPanel()` which is async.
Use a direct JS call instead, which executes immediately on the same tick:

```r
# Activate tab_config synchronously before introjs starts
shinyjs::runjs(
  "$('a[data-value=\"tab_config\"]').tab('show');"
)
# Small safety delay to let the DOM settle before intro.js reads geometry
Sys.sleep(0.05)
introjs(session, ...)
```

(Yes, `Sys.sleep(0.05)` blocks the Shiny server briefly. That is acceptable
for a user-triggered, one-shot UI action.)

### 2.3 Event callback contract bug (rintrojs I() + body-only)

**This is the fix described in `followup_plan_v3.md` section B.1.** Apply it
exactly as written there. Summary:

1. Delete the inline `"function(targetElement) { ... }"` string in the
   `events = list(onbeforechange = ...)` argument.

2. Replace with `readCallback("switchTabs")`. This returns
   `I("rintrojs.callback.switchTabs(targetElement)")` — the **body** of the
   callback (not a full `function(...) { ... }` declaration), wrapped in
   `I()` so `jsonlite::toJSON(auto_unbox = TRUE)` emits it as a raw JS
   literal rather than a quoted string.

3. Delete the companion `observeEvent(input$tour_set_tab, ...)` block
   entirely — no longer needed because `switchTabs` calls
   `$().tab('show')` directly on the client.

The `type = "pills"` concern raised in v3 section B.2 is a non-issue:
`rintrojs.callback.switchTabs` uses `[data-value]` lookup and calls
`.tab('show')`, which Bootstrap 3 honors for both `data-toggle="tab"` and
`data-toggle="pill"` nav links.

**Also update** the `wizard_tabs` tabsetPanel structure if needed: `switchTabs`
walks up from the target element via `$(targetElement).closest('[data-value]')`
to find the owning pane. All five `tabPanel(...)` calls already have `value =
"tab_xxx"` set explicitly, so this should work.

### 2.4 Modal backdrop blocking tour

If the welcome modal was dismissed but its `.modal-backdrop` div was
orphaned (can happen if a tour is launched from inside the modal footer
before its dismiss animation completes), the backdrop sits at `z-index:
1040` and swallows clicks on intro.js overlay elements.

**Fix:** at the top of the `observeEvent(input$start_tour, ...)` body, inject
a defensive cleanup:

```r
shinyjs::runjs(
  "$('.modal-backdrop').remove();
   $('body').removeClass('modal-open');"
)
```

This is harmless when no backdrop is present.

### 2.5 CSS hiding the tooltip

**Check `www/style.css`** for any of:
- `.introjs-tooltip { display: none }` or `visibility: hidden`
- `.introjs-helperLayer { z-index: <low number> }` or `display: none`
- `.introjs-overlay { ... }` overrides
- Body-level `overflow: hidden` that might clip fixed-position tooltips

If any of these exist, either remove the override or scope the `.introjs-*`
rules back to intro.js defaults (z-index 9999999 for the helperLayer and
tooltip).

### 2.6 Observer never fires

Verify that `input$start_tour` is actually wired by checking the button ID in
`app.R`:

```r
actionButton("start_tour", "\U0001F680 Start Guided Tour", ...)
```

And in `server_common.R`:

```r
observeEvent(input$start_tour, { ... })
```

IDs must match exactly. Also verify that no earlier `observeEvent(input$start_tour, ...)`
exists in another sourced file that would take precedence.

---

## 3. After the fix: confirm tour steps highlight the correct sections

### 3.1 Walk the tour end-to-end (manual)

With assay_type = RBA, language = EN:
1. Start the app, dismiss welcome modal.
2. Click Start Guided Tour.
3. Step through every step. For each, verify:
   - Correct wizard tab is active.
   - The highlighted element is visible, properly outlined by intro.js's
     helper layer, and the tooltip contains the text for that step.
   - No JS errors in the console.
4. Finish the tour cleanly.
5. Switch to ELISA, restart tour, repeat. Only difference should be that
   the RBA QC step is replaced by the tissue-weights step.
6. Switch to Spanish, restart tour. Every tooltip in Spanish.

### 3.2 Fix the matrix step order (reading order)

This is `followup_plan_v3.md` section B.3. Apply it after the tour works:
change the order in `layout_steps` from
```
matrix_type → matrix_id → matrix_dilution → matrix_replicate
```
to
```
matrix_type → matrix_dilution → matrix_id → matrix_replicate
```
to match the visual 2×2 grid reading order (the grid places Type next to
Dilution in row 1, ID next to Replicate in row 2).

### 3.3 Remove all diagnostic logging

Once the tour works end-to-end in both assay types and both languages,
remove every `shinyjs::runjs("console...")` call added in § 1. Delete the
`tour_diagnosis_YYYYMMDD.txt` file. The final `server_common.R` should have
no `console.log`, no `window.addEventListener('error', ...)`, no diagnostic
output.

---

## 4. Defensive cleanup (apply regardless of which branch fixed it)

Once the tour works, also apply these low-risk hardenings from
`followup_plan_v3.md`:

- **B.5: Defensive selector filter.** Before passing `tour_steps` to
  `introjs()`, drop rows whose `element` column is `NA`, empty, or does not
  match the pattern `^#[A-Za-z][-_A-Za-z0-9]*$`. Emit a `warning()` listing
  any dropped selectors so future regressions are visible on the R console.

- **Remove the custom `onbeforechange` JS string** and the companion
  `observeEvent(input$tour_set_tab, ...)` observer, if § 2.3 was applied.

Do not apply B.4 unless § 1.2 showed that the RBA QC or ELISA tissue-weight
conditional-panel section was reported as HIDDEN. The current conditional-panel
branching in the tour step construction is already correct: RBA tours include
`#qc_section` and ELISA tours include `#tissue_weight_section`; never both.

---

## 5. Out of scope (do not do in this session)

- Any change to the underlying tour content/wording (i18n keys stay put).
- Any change to the wizard layout beyond the 2×2 grid reordering in § 3.2.
- Tour persistence / "don't show again" preference.
- Keyboard shortcuts for tour navigation.
- Migrating tour construction to inline `introBox()` calls in the UI.

---

## 6. Acceptance checklist

Tour functionality:
- [ ] § 1.1–1.3 diagnostic logging added; console output captured.
- [ ] Console output reviewed; § 2.x branch identified and applied.
- [ ] Step 1 tooltip (`#language_toggle_section`) visible when tour starts.
- [ ] Every subsequent step shows its tooltip on the correct tab.
- [ ] No `console.error` lines during tour walk-through.
- [ ] Tour works in RBA, language = EN.
- [ ] Tour works in RBA, language = ES.
- [ ] Tour works in ELISA, language = EN.
- [ ] Tour works in ELISA, language = ES.

Cleanup:
- [ ] § 1 diagnostic logging removed from `server_common.R`.
- [ ] `tour_diagnosis_*.txt` file deleted.
- [ ] Matrix step order reflects reading order (§ 3.2).
- [ ] Defensive selector filter added (§ 4, B.5).
- [ ] Custom inline `onbeforechange` JS string removed if § 2.3 applied.

Regression:
- [ ] Existing tests still pass: `testthat::test_dir("tests/testthat")`.
- [ ] No new warnings from `shiny::runApp(".")` that were not present before.

---

## Notes on what previously failed

The previous attempt (tracked in `followup_plan_v3.md`) jumped straight to the
`readCallback("switchTabs")` fix without empirical verification and without
cleaning up the custom inline `onbeforechange` string. It is plausible but
not certain that that fix alone resolves the current "no tooltip at all"
symptom. The explicit diagnostic step in § 1 eliminates the guesswork —
find out what the browser is actually doing before you pick a branch in § 2.

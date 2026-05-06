# GUI Fix Pass — May 2026

Implementation notes for the CSS-only GUI fix pass applied to `www/style.css`
(and kept in sync with root `style.css`).

The round-by-round sections below record what was broken, why it was broken,
and what was done. Keep this file updated when further fixes are added.

---

## Round 1 — Initial fixes (commit: fix(gui): dropdown ghost rect…)

Five defects were identified and addressed in a single CSS block appended after
line 1010 of `www/style.css`, plus two inline-style removals in `app.R`.

### Defect 1 — Tiny white/black rectangle inside every dropdown

selectize.js keeps the original `<select>` in the DOM for form submission but
the previous hide rule (`.shiny-bound-input`) did not match current Shiny
builds. Fixed by hiding by tag:

```css
.selectize-control > select { display: none !important; }
```

### Defect 2 — fileInput "Browse…" button covered by the filename text field

Bootstrap 3 lays `.input-group` out with `display: table`. When the parent was
switched to `display: flex`, Bootstrap's `width: 100%` on `.form-control`
became the flex-basis, expanding the text input over the button. Two sub-fixes:

- `.input-group-btn`: `flex: 0 0 auto; width: auto !important` — natural width,
  no table-cell shrink hack.
- `.form-control`: `flex: 1 1 0% !important` — `0%` flex-basis bypasses
  Bootstrap's `width: 100%` entirely; item grows into remaining space only.

### Defect 3 — Tab 2 preset row vertical baseline

The empty `.progress` sibling on every `fileInput()` added invisible height.
Separately, `app.R` had `div(style="margin-top: 25px;", ...)` wrappers on the
Save Layout button and the Load `uiOutput` — magic-number hacks that fought the
new flex alignment.

CSS fix: hide `.shiny-file-input-progress` (re-show on `.active`); make the
preset row `display: flex; align-items: flex-end`; zero `margin-bottom` on
**all** `.form-group` elements in the row (not just the fileInput one — the
selectInput column kept Bootstrap's default 15 px margin and sat 15 px above
the button baseline).

`app.R` fix: removed the two `div(style="margin-top: 25px;", ...)` wrappers
around `actionButton("layout_save", …)` and `uiOutput("layout_load_ui")`.

### Defect 4 — Tab 3 upload row baseline

Same root cause as Defect 3. Fixed with `margin-bottom: 0` on `.form-group`
inside `.bs-card > div[style*="align-items: flex-end"]`.

### Defect 5 — "Give Feedback" text not vertically centered

The flex-centering rule in the file was scoped to `.bs-card`; the feedback
section uses `#notes_feedback_section`. Applied centering directly to
`#notes_feedback_section .btn-block` and `#notes_feedback_section a.btn-info`.

---

## Round 2 — Specificity / cascade conflicts (commit: fix(gui): resolve specificity conflicts…)

Round 1's baseline fixes were undermined by three CSS cascade conflicts that
only became visible in a running browser. All fixes are CSS-only; no further
`app.R` changes were needed.

### Bug R2-1 — `justify-content: center` fighting `align-items: flex-end`

**Location:** `www/style.css` ~line 987 (rule predating the GUI FIX PASS block)

```css
/* BEFORE */
.bs-layout-header .form-group,
.bs-card .form-group:has(> .input-group),
#preset_layout_section .form-group:has(> .input-group) {
  justify-content: center;   /* ← was centering the widget inside its column */
}
```

The `flex-direction: column` on each `.form-group` made `justify-content`
control *vertical* positioning of the widget inside the column box.
`center` placed the widget in the middle of the box; the row's
`align-items: flex-end` could only align the box's bottom edge — the widget's
visual bottom was still 50 % up the column.

**Fix:** Changed to `justify-content: flex-end` so the widget is pushed to the
bottom of its column box, matching the row baseline.

### Bug R2-2 — `.bs-card .form-group { margin-bottom: 14px }` re-opening the gap

**Location:** `www/style.css` v3.1 §3 block, ~line 1203 — **after** the GUI FIX
PASS block.

The v3.1 §3 rule has the same specificity as the GUI FIX PASS rules for
`.form-group` but appears later, so it wins. More importantly, `uiOutput()` and
`inputPanel()` inject `.shiny-html-output` / `.shiny-input-container` wrapper
divs that are *not* `.form-group` elements — they were not covered by the
`margin-bottom: 0` rules at all.

**Fix:** Added explicit margin-zeroing in the GUI FIX PASS block for the Shiny
wrapper classes:

```css
/* Tab 2 */
#preset_layout_section .shiny-html-output,
#preset_layout_section .shiny-input-container { margin-bottom: 0 !important; }

/* Tab 3 */
.bs-card > div[style*="align-items: flex-end"] > .shiny-html-output,
.bs-card > div[style*="align-items: flex-end"] .shiny-input-container,
… { margin-bottom: 0 !important; }
```

### Bug R2-3 — `uiOutput` wrapper not reaching the row baseline

**Location:** Tab 3 upload row (`app.R` ~line 782); `uiOutput` renders as
`<div class="shiny-html-output">` wrapping the button.

The row's `align-items: flex-end` positioned the wrapper's *box* bottom at the
baseline, but the button inside sat at the *top* of the wrapper (default block
stacking). The visible button bottom was therefore above the baseline.

**Fix:** Made `.shiny-html-output` wrappers in the Tab 3 upload row flex
columns with `justify-content: flex-end`:

```css
.bs-card > div[style*="align-items: flex-end"] > .shiny-html-output,
.bs-card > div[style*="align-items:flex-end"] > .shiny-html-output {
  display: flex !important;
  flex-direction: column;
  justify-content: flex-end;
}
```

This pushes the child button to the bottom of the wrapper box, which in turn
sits at the row baseline.

---

## Verification checklist

- [ ] **Dropdowns.** No small rectangle next to any chevron. Dropdown still
      opens and selects values.
- [ ] **Tab 2 file input.** "Browse…" fully visible and clickable first try.
      Filename area to its right, no overlap.
- [ ] **Tab 2 baseline.** Load Preset Layout, Import Layout, Save Layout, and
      "No saved layouts found." share one bottom line.
- [ ] **Tab 3 file input.** Same as Tab 2.
- [ ] **Tab 3 baseline.** "No file selected", trash icon, and Download Example
      File share one bottom line.
- [ ] **Upload progress.** Progress bar appears while upload runs, disappears on
      completion.
- [ ] **Give Feedback.** Icon and label vertically centered inside the blue button.
- [ ] **No regressions.** Language switcher, `.bs-layout-header` file inputs,
      plate layout header all render correctly.

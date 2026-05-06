# GUI Fix Pass — May 2026

Consolidated implementation guide for four interrelated UI defects in the
Shiny RBA / ELISA app. All fixes are CSS-only except for two small inline-style
removals in `app.R`.

Apply in order. Each section names the symptom, the root cause, and the
exact change.

---

## Defect 1 — Tiny white/black rectangle inside every dropdown

**Symptom.** Every `selectInput()` (Type of assay, Toxin standard, Number of
standards, Load Preset Layout, Analyte, Standard concentration units, the
top-bar language switch, etc.) shows a small rectangle next to the chevron.
It paints white in light mode, dark in dark mode.

**Root cause.** Shiny dropdowns are rendered by `selectize.js`, which wraps
the original `<select>` element with its own pretty box. The original
`<select>` stays in the DOM so the form still submits a value, but it must
never paint. `www/style.css` already had a hide rule:

```css
.selectize-control > select.shiny-bound-input,
.selectize-hidden-accessible { position: absolute !important; left: -9999px ... }
```

…but `.shiny-bound-input` is not stamped on the native `<select>` after
selectize wraps it in current Shiny builds, and `.selectize-hidden-accessible`
only targets the a11y clone. The original `<select>` therefore remains
visible next to the chevron.

**Fix (already applied in `www/style.css`, marker `(1)`).** Hide the native
control by tag:

```css
.selectize-control > select { display: none !important; }
```

selectize syncs the value back to the underlying `<select>` programmatically;
no rendering is needed for form submission.

**Affects.** Every dropdown in the app, including the top-bar language
switcher.

---

## Defect 2 — fileInput "Browse…" button covered by the filename text field

**Symptom.** On Tab 2 (Import Layout) and Tab 3 (Upload Bioassay Results),
the `fileInput()` looks like a single white box with placeholder text "No
file selected" — clicking the left edge sometimes works (because the Browse
button is underneath) and sometimes doesn't.

**Root cause.** Shiny's `fileInput()` renders:

```html
<div class="input-group">
  <label class="input-group-btn">
    <span class="btn btn-default btn-file">Browse…<input type="file"></span>
  </label>
  <input type="text" class="form-control" placeholder="No file selected" readonly>
</div>
```

Bootstrap 3 originally laid this out with `display: table-cell`. `style.css`
swapped the parent to `display: flex; align-items: center` (lines ~990–1007)
without re-stating the children's flex behaviour. Bootstrap 3's
`.input-group .form-control { width: 100% }` then takes effect inside the
flex parent, and the text input expands to the full row width and **paints
on top of** the Browse button.

**Fix (already applied, marker `(2)`).**

```css
.input-group-btn { flex: 0 0 auto; z-index: 2; position: relative; }
.input-group .form-control { flex: 1 1 auto; min-width: 0; width: auto !important; z-index: 1; }
```

Browse button keeps its natural width and sits on top; the text field
takes only the remaining space. Border-radius adjustments restore the
joined "Browse | filename" look.

**Affects.** Tab 2 `#preset_layout_section` (Import Layout CSV/Excel) and
Tab 3 `.bs-card` (Upload Bioassay Results).

---

## Defect 3 — Vertical alignment of the Tab 2 preset row

**Symptom.** Load Preset Layout dropdown, Import Layout file input, Save
Layout button, and "No saved layouts found." caption do not share a
baseline.

**Root cause.**

1. Each `fileInput()` carries an empty `<div class="progress">` sibling for
   upload progress. It paints nothing visible but **adds height**, so the
   file input column is taller than its visible content.
2. `app.R` lines 540 and 547 push the Save Layout / Load buttons down with
   inline `style = "margin-top: 25px;"` — a magic number that compensated
   for the labeled columns being taller, but no longer matches now that the
   file input column is taller still.

**Fix (already applied, markers `(3)` and `(4)`).**

```css
.shiny-file-input-progress, .progress:empty { display: none; }
#preset_layout_section .row { display: flex; align-items: flex-end; }
#preset_layout_section [class^="col-"] { display: flex; flex-direction: column; justify-content: flex-end; }
```

**Required follow-up in `app.R` — remove the inline margin hacks.** The
`align-items: flex-end` on the row now does this automatically, and the
inline styles fight the new rule.

In `app.R` around line 540:

```r
# BEFORE
column(2,
  div(style = "margin-top: 25px;",
    actionButton("layout_save", label = "Save Layout",
                 icon  = icon("save"),
                 class = "btn btn-success btn-sm", style = "width: 100%;")
  )
),
column(2,
  div(style = "margin-top: 25px;",
    uiOutput("layout_load_ui")
  )
)

# AFTER
column(2,
  actionButton("layout_save", label = "Save Layout",
               icon  = icon("save"),
               class = "btn btn-success btn-sm", style = "width: 100%;")
),
column(2,
  uiOutput("layout_load_ui")
)
```

**Affects.** Tab 2 only.

---

## Defect 4 — Tab 3 upload row baseline (file input + trash + Download Example)

**Symptom.** "No file selected" sits visibly higher than the trash and
Download Example File buttons next to it.

**Root cause.** Same as Defect 3 — the empty `.progress` div hangs off the
bottom of the file input. Once `(3)` hides it, the wrapper at `app.R:782`

```r
div(style = "display: flex; gap: 12px; flex-wrap: wrap; align-items: flex-end;",
    uiOutput("upload_counts_ui"),
    uiOutput("clear_upload_ui"),
    uiOutput("download_plate_template_ui"))
```

needs the children's `.form-group` bottom margin zeroed so the visible
widget really is at the bottom of its box.

**Fix (already applied, marker `(5)`).**

```css
.bs-card > div[style*="align-items: flex-end"] .form-group { margin-bottom: 0 !important; }
```

**Affects.** Tab 3 only.

---

## Defect 5 — "Give Feedback" text not vertically centered

**Symptom.** Icon and "Give Feedback" label paint along the top of the blue
box, with empty space below.

**Root cause.** `give_feedback_ui` is rendered by `server_common.R:440` as

```r
tags$a(href="...", class="btn btn-info btn-block", icon("comment"), " ", tr("give_feedback", lang))
```

`.btn-block` only sets `display:block; width:100%;` — it does not vertically
center text. There is a flex-centering rule lower in `style.css` that would
fix it, but it is scoped to `.bs-card` and the feedback section is wrapped
in a plain `#notes_feedback_section` `<div>`, not a `.bs-card`.

**Fix (already applied, marker `(6)`).**

```css
#notes_feedback_section .btn-block,
#notes_feedback_section a.btn-info {
  display: flex !important;
  justify-content: center;
  align-items: center;
  gap: 8px;
  min-height: 44px;
  line-height: 1;
}
```

`min-height: 44px` also brings the button up to a WCAG-compliant tap target.

**Affects.** Notes & Feedback section, all tabs.

---

## Verification checklist

After applying the CSS block + the two `app.R` edits in Defect 3:

- [ ] **Dropdowns.** Open every `selectInput` (Type of assay, Toxin standard,
      Number of standards, Load Preset Layout, language switch). The small
      rectangle next to the chevron is gone in all of them, and the dropdown
      still opens and selects values correctly. Submit a form and confirm
      the value is captured server-side.
- [ ] **Tab 2 file input.** "Browse…" is fully visible and clickable on
      first attempt. The filename area sits to its right with one shared
      border, no overlap.
- [ ] **Tab 2 baseline.** Load Preset Layout, Import Layout, Save Layout,
      and the "No saved layouts found." caption all share a single bottom
      line. No `margin-top: 25px` left in `app.R`.
- [ ] **Tab 3 file input.** Same as Tab 2 — Browse visible and clickable.
- [ ] **Tab 3 baseline.** "No file selected", trash icon, and Download
      Example File share one bottom line.
- [ ] **Upload progress.** Upload a real file. The progress bar appears
      while the upload runs and disappears when it finishes. (If progress
      stays hidden during upload, swap selector `(3)`'s
      `.shiny-file-input-progress.active` for `:not(:empty)` instead.)
- [ ] **Give Feedback.** Icon and label are vertically centered inside the
      blue button.
- [ ] **No regressions.** Top-bar language switcher still renders, opens,
      and switches language. Plate Layout header file input on tabs that
      use `.bs-layout-header` still aligns.

---

## Why is vertical alignment "so complicated"?

It's not the CSS — it's that **Shiny renders most inputs as block-level
`.form-group` wrappers with built-in label-on-top, hidden helpers,
progress bars, and validation slots**. So when you put a file input next to
a button next to a select, you're not actually putting three controls next
to each other — you're putting three different *vertical stacks* next to
each other, each with its own padding/margin and trailing siblings.
`align-items: flex-end` on a flex parent only aligns the children's
*boxes*, not the visible widgets inside them.

The reliable pattern, used throughout this fix:

1. Make the parent `display: flex; align-items: flex-end;`.
2. Strip the children's own `margin-bottom` so the visible widget really
   is at the bottom of its box.
3. Hide trailing helper elements (`.progress`, empty `.help-block`, etc.)
   until they are actually needed.
4. Never use `margin-top: 25px` to push down a labelless control to match
   a labeled one — that always drifts the moment something else changes.

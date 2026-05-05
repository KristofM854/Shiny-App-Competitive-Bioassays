# Bioassay Suite — GUI Refresh · Implementation Guide **v3.1**

> **Read this first.** v3 landed and the structural fixes (topbar, stepper, Quick Start tiles, plate-cell colours) all paint correctly. v3.1 is a **polish patch** addressing layout, sizing, and alignment issues that surfaced once the v3 markup settled. Apply on top of `main` after v3.
>
> **Ground rules (unchanged):**
> 1. **No business logic changes.** Do not touch DRC fitting, outlier detection, tissue normalization, multi-wavelength concordance, auto-save, file-import logic, output-folder naming, or `runApp()` entry points.
> 2. **All four plate matrices stay** with their observers and reactives intact.
> 3. **Bilingual i18n engine stays.** Only **add** keys; never rename.
> 4. If anything in this guide conflicts with existing logic, **stop and ask**.

---

## What's still off after v3 (six symptoms across the five wizard tabs)

| # | Tab | Symptom | Root cause | Section |
|---|---|---|---|---|
| **A** | 1 | "Type of assay" / "Analyte" / "Standard concentration units" selects wrap onto two lines because the dropdown control is too narrow | `selectize-input` width is set by Shiny's default + Bootstrap's `.form-control` rules, not by content. Long option text doesn't get a wider control. | § 1 |
| **B** | 1 | "Number of standards" select renders full-width; S1–S8 standard-value labels (e.g. "S1 (ng/ml)") wrap onto two lines | Two issues: (1) the numeric select inherits full-row width with no constraint, (2) the standards grid columns are too narrow for the unit-bearing label. | § 1 |
| **C** | 2 | Plate Layout header bar — "Load Preset Layout" select wraps; "Browse..." file input not vertically centered with neighbours | Same `selectize-input` issue plus Shiny's `fileInput` renders as `display: block` with stacked label, breaking the row's vertical alignment. | § 2 |
| **D** | 2, 3, 5 | A green action button shows `[object Object]` instead of its label | Three calls to `actionButton(label = tr(KEY, lang), icon = icon(...))` where `tr()` returns a `tagList(icon, text)` instead of a plain string. JS coerces the object to `[object Object]` for the label attribute. | § 4 |
| **E** | 3 | "Classic Import / Visual Plate Selector" radio buttons not vertically centered with their labels | Bootstrap 3 absolutely-positions `<input type="radio">` inside `<label>`, which breaks once the label text wraps or sits beside other flex content. | § 3 |
| **F** | 5 | Report buttons + "Give Feedback" — text/icon not centered within the button | The `.btn` rules have `display: flex` for icon-then-label spacing, but no `justify-content: center`. Content left-aligns inside a full-width button. | § 3 |

§ 4 (the `[object Object]` bug) is the only fix that touches R logic. Everything else is CSS appended to `www/style.css`.

---

## § 1 · Configuration form widths (Tab 1)

### Problem in detail

Shiny's `selectInput` ships as a `selectize-control` with a `selectize-input` inner. By default, `selectize-input` has `width: 100%` of its `form-group` parent, but the parent's width depends on Bootstrap column siblings. When the chosen option's text exceeds the rendered width, the text **wraps inside the control** rather than the control widening — producing the two-line "ELISA (Enzyme-Linked Immunosorbent Assay)" you see in Tab 1.

The standards-value grid (S1..S8 numeric inputs in a 4-column grid) has the inverse problem: each column is narrow enough that the label "S1 (ng/ml)" wraps after "S1 (ng/" onto a second line.

### 1.1 — Append to `www/style.css`

```css
/* ============================================
   v3.1 §1 — Configuration form widths (Tab 1)
   ============================================ */

/* Long-option selects (assay type, analyte, units, preset layout):
   give them a sensible minimum so the chosen option doesn't wrap
   onto two lines. Scoped to .bs-card so it doesn't leak to the
   topbar language select. */
.bs-card .shiny-input-container .selectize-control {
  min-width: 320px;
}
.bs-card .shiny-input-container .selectize-input {
  white-space: nowrap;
  overflow: hidden;
  text-overflow: ellipsis;
}

/* Number-of-standards: the numeric/select control should be narrow,
   not full-width. Target by id-bearing form-group via :has(). */
.bs-card .form-group:has(> label[for="num_standards"]) .selectize-control,
.bs-card .form-group:has(> #num_standards) {
  min-width: 96px;
  max-width: 120px;
  width: 120px !important;
}

/* Standards-value grid (S1..S8): widen each column so the label
   "S1 (ng/ml)" stays on a single line. If the markup uses a wrapper
   class .bs-standards-grid, this targets it directly; the second
   block is a fallback for any form-group whose label starts
   with std_value_. */
.bs-card .bs-standards-grid {
  display: grid;
  grid-template-columns: repeat(4, minmax(150px, 1fr));
  gap: 14px 18px;
}
.bs-card .bs-standards-grid .form-group label,
.bs-card .bs-standards-grid .control-label,
.bs-card .form-group:has(> label[for^="std_value_"]) .control-label {
  white-space: nowrap;
  font-size: 12px;
}
```

### 1.2 — Acceptance for § 1

- "Type of assay" shows `ELISA (Enzyme-Linked Immunosorbent Assay)` on a single line, ellipsised if it overflows the 320 px min-width.
- "Number of standards" select is ≤ 120 px wide.
- All eight S1..S8 labels render on a single line.
- No regression on selects elsewhere in the app — the rules are scoped to `.bs-card`.

---

## § 2 · Plate Layout header alignment (Tab 2)

### Problem in detail

The green header bar with "Load Preset Layout" and "Browse..." renders the `fileInput` as a stacked label-on-top form group, while the neighbouring `selectInput` renders inline-ish. Vertical alignment is broken because each child sets its own margin.

Additionally, the "LAYERS" rail title sits flush against the first item ("Sample Type") with too little breathing room.

### 2.1 — Append to `www/style.css`

```css
/* ============================================
   v3.1 §2 — Plate Layout header bar (Tab 2)
   ============================================ */

/* The green header bar with "Load Preset Layout / Import Layout".
   Add breathing room and vertical-center its inner controls.
   Targets either an explicit class or any well/panel sibling that
   the v3 markup placed there. */
.bs-card .bs-layout-header,
.bs-layout-header {
  display: flex;
  align-items: center;
  gap: 24px;
  padding: 14px 18px !important;
}
.bs-layout-header > * {
  margin: 0 !important;
}

/* File input ("Browse...") — Shiny's .input-group renders the
   button + filename text on one row, but the wrapping form-group
   stacks label-on-top. Force vertical center with the rest of the
   header. */
.bs-layout-header .form-group,
.bs-card .form-group:has(> .input-group) {
  display: flex;
  flex-direction: column;
  justify-content: center;
  margin-bottom: 0 !important;
}
.bs-card .form-group .input-group {
  display: flex;
  align-items: stretch;
  height: 36px;
}
.bs-card .form-group .input-group .btn-file,
.bs-card .form-group .input-group .input-group-btn {
  display: inline-flex;
  align-items: center;
  height: 36px;
}
.bs-card .form-group .input-group .form-control {
  height: 36px;
  display: flex;
  align-items: center;
}

/* LAYERS rail: more space between the title and the first item. */
.bs-card .bs-layers-rail h4,
.bs-card .bs-layers-rail .rail-title,
.bs-layers-rail h4 {
  margin: 0 0 14px 0;
  font-size: 11px;
  letter-spacing: 0.06em;
  text-transform: uppercase;
  color: var(--c-ink-3);
}
```

### 2.2 — Acceptance for § 2

- The green header bar renders as a single horizontal row at all viewport widths ≥ 1100 px.
- "Load Preset Layout" select has the 320 px min-width from § 1 and doesn't wrap.
- "Browse..." button and the filename input are both 36 px tall and vertically centered with the select.
- "LAYERS" title has 14 px of breathing room before "Sample Type".

---

## § 3 · Radio buttons + button content centering (Tabs 3 & 5)

### Problem in detail

Bootstrap 3 emits radio-group markup as:
```html
<div class="radio">
  <label>
    <input type="radio" name="..." value="..." />
    Classic Import
  </label>
</div>
```

Bootstrap's CSS sets `.radio input[type="radio"] { position: absolute; margin-left: -20px; }` to "hang" the dot in a left gutter. This works only when the label has enough left padding; in flex / inline-flex contexts (which the v3 design system uses heavily) the absolute positioning misaligns the dot against the text vertical center.

For buttons (§ 3 of this section, despite naming): many `.btn` rules in v2/v3 use `display: inline-flex` with `align-items: center` for the icon-then-label pattern, but never set `justify-content: center`. On a full-width button (`.btn-block` or `width: 100%`) the content left-aligns inside a wide button.

### 3.1 — Append to `www/style.css`

```css
/* ============================================
   v3.1 §3 — Radio alignment + button content centering
   ============================================ */

/* Radio rows: replace Bootstrap's absolute-positioned dot with
   a flex-aligned dot-then-label pair. */
.bs-card .radio,
.bs-card .radio-inline,
.bs-card .shiny-options-group .radio,
.bs-card .shiny-options-group .radio-inline {
  display: inline-flex;
  align-items: center;
  gap: 6px;
  padding: 0;
  margin: 0 16px 0 0;
}
.bs-card .radio > label,
.bs-card .radio-inline > label,
.bs-card .shiny-options-group label {
  display: inline-flex;
  align-items: center;
  gap: 6px;
  font-weight: 400;
  margin: 0;
  padding: 0;
  cursor: pointer;
}
.bs-card .radio input[type="radio"],
.bs-card .radio-inline input[type="radio"],
.bs-card .shiny-options-group input[type="radio"] {
  position: static !important;
  margin: 0 !important;
  vertical-align: middle;
  flex-shrink: 0;
}

/* Same for checkbox groups */
.bs-card .checkbox,
.bs-card .checkbox-inline {
  display: inline-flex;
  align-items: center;
  gap: 6px;
}
.bs-card .checkbox input[type="checkbox"],
.bs-card .checkbox-inline input[type="checkbox"] {
  position: static !important;
  margin: 0 !important;
  flex-shrink: 0;
}

/* Buttons: center icon + label content. Exclude .preset-tile
   (Quick Start cards — they intentionally left-align body copy)
   and .bs-quiet-btn (topbar text buttons — centered already). */
.bs-card .btn,
.bs-card .action-button:not(.preset-tile):not(.bs-quiet-btn) {
  display: inline-flex;
  align-items: center;
  justify-content: center;
  gap: 8px;
}

/* Full-width buttons (report buttons in Tab 5, Give Feedback) */
.bs-card .btn-block,
.bs-card .btn[style*="width: 100%"],
.bs-card .btn[style*="width:100%"],
.bs-card #give_feedback,
.bs-card .bs-feedback-btn {
  display: flex !important;
  justify-content: center;
  align-items: center;
  gap: 8px;
  text-align: center;
}

/* Generic: tighten label-to-control spacing inside .bs-card */
.bs-card .form-group {
  margin-bottom: 14px;
}
.bs-card .form-group > .control-label {
  margin-bottom: 6px;
  font-size: 12px;
  font-weight: 600;
  color: var(--c-ink);
}
```

### 3.2 — Acceptance for § 3

- "Classic Import" and "Visual Plate Selector" radios on Tab 3: dots are vertically centered with their labels.
- All `.btn` content (icon + label) is horizontally centered within the button.
- "Compact report", "Detailed report", and "Generate Report" buttons on Tab 5 have centered text+icon.
- "Give Feedback" full-width button has its icon+label centered.
- No regression on Quick Start preset tiles (they keep their left-aligned body copy).

---

## § 4 · The `[object Object]` button label bug (Tabs 2, 3, 5)

### Problem in detail

Three buttons render their label as the literal string `[object Object]`. This is the JavaScript stringification of any non-string object passed where a string is expected. In Shiny, the most common cause:

```r
# WRONG — tr() returns a tagList, which becomes "[object Object]"
# when serialized as the button's label attribute.
actionButton("import_layout_btn",
             label = tr("import_layout", lang),
             icon = icon("file-import"))
```

If `tr("import_layout", lang)` returns `tagList(icon("file-import"), "Import Layout")` rather than the string `"Import Layout"`, the label argument receives a Shiny tag object. When Shiny's `actionButton` builds the HTML, the tag object is rendered into the button's content. But for **subsequent** `updateActionButton(session, id, label = tr(..., lang))` calls (which fire on language change), the JS-side handler calls `el.textContent = label` — and a tag object stringifies to `[object Object]`.

The fix: `tr()` must return **plain strings only**. Icons go via `actionButton(... icon = icon(...))`, never bundled into the translation.

### 4.1 — Locate the three offending buttons

Search in `app.R` and the `server_*.R` files for any of these patterns:

```r
actionButton(..., label = tr(KEY, lang), icon = icon(...))
updateActionButton(session, ID, label = tr(KEY, lang), icon = icon(...))
```

The three known offenders (Tab 2 green action button, Tab 5 "Generate Report", and another action — likely "Compact report" or the import action) should all match.

### 4.2 — Inspect `tr()`

Open `R/i18n.R` (or wherever `tr()` is defined) and look at how the offending keys (`import_layout`, `generate_report`, etc.) are stored. If the translations dictionary contains entries like:

```r
i18n$en$import_layout <- tagList(icon("file-import"), "Import Layout")
```

…that's the bug. Replace with:

```r
i18n$en$import_layout <- "Import Layout"
i18n$es$import_layout <- "Importar diseño"
```

`tr()` itself should `return(as.character(value))` defensively — if a non-string sneaks in, return its string representation rather than the object:

```r
tr <- function(key, lang = "en") {
  v <- i18n[[lang]][[key]] %||% i18n$en[[key]] %||% key
  if (!is.character(v)) v <- as.character(v)
  v
}
```

### 4.3 — Where icons go instead

The icon belongs on the `actionButton` / `updateActionButton` call:

```r
actionButton("import_layout_btn",
             label = tr("import_layout", lang),   # plain string
             icon  = icon("file-import"),         # icon argument
             class = "btn-primary")

# Later, on language change:
updateActionButton(session, "import_layout_btn",
                   label = tr("import_layout", lang),
                   icon  = icon("file-import"))   # re-pass on update
```

### 4.4 — Acceptance for § 4

- Tab 2: the green action button reads "Import Layout" (or its Spanish equivalent), with a file-import icon.
- Tab 5: the "Generate Report" button reads its actual label, with the relevant icon. Same for any other previously-broken label.
- Toggle language English → Spanish → English. Every action button label updates correctly; none ever shows `[object Object]`.
- `grep -n "tr(" R/i18n.R | grep tagList` returns nothing — there are no `tagList()` values in the translation dictionary.

---

## § 5 · Apply order

1. **§ 4 first** (the R-side i18n fix). It's the only logic change and it's self-contained. Verify all three buttons label correctly before touching CSS.
2. **§ 1** (Tab 1 form widths). Verify on the Configuration tab.
3. **§ 2** (Tab 2 layout header). Verify on the Plate Layout tab.
4. **§ 3** (Tabs 3 & 5 radios + button centering). Verify last — it's the most cross-cutting CSS.

After each step: hard-reload the browser (**Cmd+Shift+R** / **Ctrl+Shift+F5**). If you haven't yet added a cache-buster to the `<link rel="stylesheet">` tag in `app.R`, do it now — it removes a category of confusion permanently:

```r
tags$link(rel = "stylesheet",
          type = "text/css",
          href = paste0("style.css?v=", as.numeric(Sys.time())))
```

Run a full happy path after the four sections land (RBA Saxitoxin → Quick Start → Plate Layout → Upload → Analysis → Generate Report). Toggle language EN ↔ ES at every tab.

---

## § 6 · Acceptance checklist (full v3.1)

### Tab 1 — Configuration
- [ ] "Type of assay" select renders on a single line; min-width 320 px.
- [ ] "Analyte" select renders on a single line.
- [ ] "Standard concentration units" select renders on a single line.
- [ ] "Number of standards" select is ≤ 120 px wide.
- [ ] All eight S1..S8 labels render on a single line each.

### Tab 2 — Plate Layout
- [ ] Green header bar is one horizontal row; controls vertically centered.
- [ ] "Load Preset Layout" select renders on a single line (320 px min-width).
- [ ] "Browse..." button + filename input are 36 px tall, vertically centered with the select.
- [ ] "LAYERS" title has 14 px breathing room before "Sample Type".
- [ ] Green action button reads "Import Layout" (or Spanish equivalent), not `[object Object]`.

### Tab 3 — Upload
- [ ] "Classic Import" / "Visual Plate Selector" radio dots are vertically centered with their labels.
- [ ] All buttons (Browse, Download Example File, Show default plate layout) have centered content.

### Tab 5 — Report
- [ ] All report-action buttons render their actual labels (not `[object Object]`).
- [ ] Compact report / Detailed report / Generate Report buttons have centered icon + text.
- [ ] "Give Feedback" full-width button is centered.

### Regression
- [ ] RBA Saxitoxin end-to-end produces the same numbers.
- [ ] ELISA Cortisol end-to-end produces the same control summary.
- [ ] Auto-save still restores layout on relaunch.
- [ ] Language toggle EN ↔ ES still flips all UI strings.
- [ ] No new console errors in DevTools.

### Caching
- [ ] `app.R` references `style.css` with a cache-busting query string.
- [ ] Hard-reload after each iteration; soft reload doesn't lie about CSS state.

---

## § 7 · Out of scope for v3.1

- No new tabs, sub-tabs, or step reordering.
- No business logic changes (DRC fitting, outlier detection, etc.).
- No changes to the Quick Start tile design (it's correct after v3).
- No changes to the plate-cell colour rendering (correct after v3).
- No changes to the wizard stepper (correct after v3).
- No new translation keys — only fix existing ones that incorrectly contain tags.

If any visual still looks wrong after v3.1 lands, capture screenshots of the broken element with DevTools Computed Styles open and we'll do a targeted v3.2.

---

*v3.1 supersedes v3 only for the polish issues above; v3 (and v2) remain the spec for everything else.*

# Vertical Alignment — Root-Cause Analysis & Fix

**Date:** May 2026 — third pass on the same defect.
**Status of prior fixes:** the CSS in `www/style.css` markers `(3)`, `(4)`, `(5)` is in the file. The inline `margin-top: 25px` hacks in `app.R` are gone. The bug is still visible.

This document explains exactly **why** the previous fixes do not work, with measurements from a faithful DOM reproduction, and gives a concrete, minimal patch.

---

## TL;DR

There are **two unrelated bugs** in the two screenshots, and the prior pass treated them as one.

| | Tab 2 (preset row) | Tab 3 (upload row) |
|---|---|---|
| Symptom | "Save Layout" sits ~15 px below the rest; "No saved layouts found." floats in mid-air | "No file selected" field sits ~40 px above the trash + Download buttons |
| Real cause | Each `column()` holds **a different number of stacked controls** (1 vs 2) and the existing CSS only matches *single-control* columns | The `.shiny-file-input-progress` element has class `progress active` from the moment it renders — `:empty` and `.shiny-file-input-progress` (without `.active`) selectors **never match it**, so the 20 px-tall progress bar is never hidden |
| Why prior pass missed it | Treated `layout_load_ui` (which renders TWO widgets via `tagList`) as if it were one widget; treated the unconditional `.active` class as if it were only set during upload | Same — selector `:empty` is wrong because the progress div has a child `.progress-bar` element from server-render time |

Fix both → both rows align.

---

## How I verified

I lifted the live DOM that Shiny generates for these two rows into a static HTML harness (`repro/repro.html`) loading the project's actual `style.css`. I then read `getBoundingClientRect()` for every interesting box. Numbers below are pixels in that harness.

### Tab 2 column boxes (after current CSS)

```
col                     top       bottom    height
Select Preset           197.6     285.3     87.7      label + selectize box
fileInput               190.7     285.3     94.6      label + input-group + .progress.active
Save Layout             251.3     285.3     34.0      bare <button>
layout_load_ui          230.0     285.3     55.3      <span class="help-block">…</span>
```

`align-items: flex-end` is doing its job — every column ends at `bottom = 285.3`. **The columns are aligned.** But each column's *visible content* is different, and `justify-content: flex-end` on the columns can't compensate when:

- The fileInput's `progress.active` bar lives inside the `form-group` (so it sits at the bottom of the form-group box; pushing the visible Browse/text-field row *up*).
- The `layout_load_ui` column is just a `<span class="help-block">` floating in the middle of an empty column. There is no widget anchored to the bottom edge.

### Tab 3 visible-control bottoms

```
upload form-group           bottom 611.8     ✓ (matches buttons)
upload input-group          bottom 571.8     ✗ visible widget sits 40 px high
trash button                bottom 611.8
Download Example            bottom 611.8
```

The form-group's *box* hits the baseline; the *visible* upload control hits 40 px above. The 40 px is the height of the still-visible `.shiny-file-input-progress` bar plus its margin.

### Why marker `(3)` does not hide the progress bar

Current selectors (style.css ≈1063):

```css
.bs-card .shiny-file-input-progress,
#preset_layout_section .shiny-file-input-progress,
.bs-card .progress:empty,
#preset_layout_section .progress:empty { display: none; }

.bs-card .shiny-file-input-progress.active,
#preset_layout_section .shiny-file-input-progress.active { display: block; }
```

This is **self-cancelling**. Shiny renders the progress bar markup as:

```html
<div id="…_progress" class="progress active shiny-file-input-progress">
  <div class="progress-bar"></div>
</div>
```

— `class="progress active"` is set **at server-render time, not when an upload starts**. So:

1. `.shiny-file-input-progress` matches → `display: none` ✓
2. `.shiny-file-input-progress.active` matches the SAME element with higher specificity (one extra class) → `display: block` ✗

Net effect: the progress bar is **always shown**, exactly as before the fix.

The `:empty` half of the rule never fires either because the element has a `.progress-bar` child.

The implementation guide hints at this in its checklist (*"If progress stays hidden during upload, swap … `.active` for `:not(:empty)` instead"*), but the actual problem is the opposite — the `.active` rule is what's leaving it visible.

---

## Why the Tab 2 row also looks wrong even with the progress hidden

Even after hiding the progress bar, the Tab 2 row has a layout that fundamentally cannot align via `align-items: flex-end` alone. The four columns hold:

1. labelled selectInput — visible widget anchored to bottom of column (label above)
2. labelled fileInput — visible widget anchored to bottom of column (label above)
3. unlabelled actionButton — visible widget IS the column
4. `renderUI` that emits **either**:
   - one `helpText()` (just a `<span>`), or
   - a `tagList(selectInput, actionButton)` — TWO stacked widgets, total height ≈ 96 px

Case (4) is the one in the screenshot ("No saved layouts found." branch). A `<span>` floating in a column with `justify-content: flex-end` does sit at the bottom — but it's tiny text, not a 32 px tall control, so its baseline is *above* the buttons' baseline by ~half a button.

When the user has saved layouts, case (4) is taller than every other column, breaking alignment in the other direction.

This needs a different mental model: **the row is not "four columns each with one bottom-aligned control"; it's "four heterogeneous content blocks of unpredictable height."** No amount of `align-items: flex-end` on the parent will normalise that.

---

## The fix

Three CSS changes plus one tiny `app.R` change.

### 1. Actually hide the empty progress bar (replaces marker (3))

```css
/* Hide the file-input progress bar until an upload is in flight.
   Shiny sets class="progress active" at render time, so .active
   alone is not a "currently uploading" signal — we must override it
   and only re-show during the .shiny-file-input-active state which
   Shiny *does* toggle live. */
.shiny-file-input-progress {
  display: none !important;
}
.shiny-file-input-progress .progress-bar[style*="width"]:not([style*="width: 0%"]):not([style*="width:0%"]) {
  /* keep — see next rule */
}
/* Re-show the bar only when its inner .progress-bar has a non-zero width
   (Shiny updates this attribute during upload). */
.shiny-file-input-progress:has(.progress-bar[aria-valuenow]:not([aria-valuenow="0"])),
.shiny-file-input-progress:has(.progress-bar[style*="width:"]:not([style*="width: 0%"])) {
  display: block !important;
}
```

If `:has()` support is a concern (it is in old browsers Shiny still supports), use the simpler approach instead:

```css
.shiny-file-input-progress { display: none !important; }
/* Shiny adds .shiny-file-input-active to the form-group while uploading. */
.shiny-input-container.shiny-file-input-active .shiny-file-input-progress {
  display: block !important;
}
```

Either fixes Tab 3 entirely and removes the height inflation in the Tab 2 fileInput column.

### 2. Stop trying to baseline-align unequal stacks. Align the visible widgets instead.

Replace the current `#preset_layout_section .row` block with a rule that targets the *visible widgets* and forces them all to live on the same horizontal line.

The cleanest way: wrap the contents of every column in a fixed-height "control row" and let labels float above with `position: absolute`. But that's invasive. A less invasive fix: explicitly normalise the heights of the non-labelled cells.

```css
/* All four columns: same total height = label-height + control-height.
   Empty columns (no label) get top padding equal to a label so their
   single control aligns with labelled columns' controls. */
#preset_layout_section .row {
  display: flex;
  align-items: stretch;          /* not flex-end */
}
#preset_layout_section [class^="col-"],
#preset_layout_section [class*=" col-"] {
  display: flex;
  flex-direction: column;
  justify-content: flex-end;
  float: none !important;
  /* reserve label space so unlabelled columns push their control down
     to match the labelled ones */
  padding-top: 22px;             /* matches the .control-label height + margin */
}
/* Labels get pulled back up out of the reserved space */
#preset_layout_section .control-label {
  margin-top: -22px;
  margin-bottom: 6px !important;
  height: 16px;
  line-height: 16px;
}
/* Kill all bottom margins so flex-end actually anchors to the visible widget */
#preset_layout_section .form-group,
#preset_layout_section .shiny-input-container { margin-bottom: 0 !important; }
```

This makes column 3 (Save Layout) and column 4 (load_ui) sit on the same baseline as the labelled selectInput and fileInput, regardless of whether `layout_load_ui` is the empty-state span or the two-control tagList.

### 3. For Tab 3, the inline-flex container is fine once (1) is in place.

Once the progress bar is hidden, the existing `align-items: flex-end` on the parent + the `.form-group { margin-bottom: 0 }` rule in marker (5) will line everything up. **No further change needed for Tab 3.**

But there's one wart: the `.bs-card > div[style*="align-items: flex-end"]` selector is fragile (matches by an inline-style substring). Replace with a stable class:

```r
# app.R line 777
div(
  class = "bs-upload-row",                    # NEW
  style = "display: flex; gap: 12px; flex-wrap: wrap; align-items: flex-end;",
  ...
)
```

```css
.bs-upload-row .form-group { margin-bottom: 0 !important; }
```

---

## Why the previous two passes failed

1. **Pass 1** assumed `align-items: flex-end` on the row was sufficient. It isn't, because the columns hold heterogeneous content stacks (label+control vs bare control vs help-text vs control-stack).
2. **Pass 2** added `margin-bottom: 0` and a `.shiny-file-input-progress` hide rule, but the hide rule is immediately reverted by an `.active` rule below it — and `.active` is set on the element from server render time, not just during upload. The progress bar stays at 20 px tall, the file-input column stays 20 px taller than its neighbours, the visible control stays 20 px above the baseline.

Both passes treated this as a CSS-tweak problem when it is actually two distinct problems: (a) a bad selector (the `.active` toggle), and (b) a structural mismatch (`layout_load_ui` returns *either* a 16-px-tall span *or* a 96-px-tall two-widget tagList, and you cannot bottom-align that against fixed-height neighbours without giving the empty state a placeholder of equal height — or pulling it out of the row entirely).

---

## Recommended order of work

1. Apply CSS change (1) — fixes Tab 3 immediately, also removes the height inflation in the Tab 2 fileInput column.
2. Verify in browser. Tab 3 should be correct.
3. Apply CSS change (2) — fixes the unlabelled-column-vs-labelled-column baseline drift in Tab 2.
4. Apply small app.R + CSS swap from change (3) for the upload row — purely cosmetic robustness, do last.
5. Test the empty-state and saved-layouts-state of `layout_load_ui` separately. Both should align.

If step 3 is more invasive than desired, an alternative is to **not put `layout_load_ui` in the same row at all** — render the empty-state as a small caption underneath the Save Layout button, and only show the saved-layouts dropdown when there's something to load. That sidesteps the heterogeneous-column-content problem entirely and is arguably better UX.

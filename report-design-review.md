# Report design review — `RBA-results-report-{compact,full}-en.html`

Scope: the two attached HTML reports. Inspected the rendered DOM, not the
underlying Rmd. Compact and full share the same templates so most points
apply to both.

Three categories below — pick what matters for v1.0:

- 🟥 **Content/correctness bugs** — these hurt the report more than any CSS choice. Fix first.
- 🟧 **Information design** — what's shown, in what order, what's hidden.
- 🟦 **Visual / typographic** — surface polish.

---

## 🟥 Correctness bugs (fix before v1.0)

### 1. Two contradictory LLOQ/ULOQ values in the same report
The KPI tile strip near the top reports **LLOQ = 102.4 pg/mL, ULOQ = 4,000 pg/mL**.
The green "Overall status" box directly below it reports **LLOQ = 62.8 pg/mL,
ULOQ = 1,387 pg/mL**. The summary section at the bottom uses 102.4 / 4,000 again.

These are different things (one is "lowest/highest standard concentration", the
other is "validated quantifiable range" derived from back-calculation accuracy)
but they share the same label. A reader sees two LLOQs and has no way to tell
which one was used to flag samples as `>ULOQ`. **This is the single biggest
clarity bug in the report.**

**Fix:** rename them. Suggested:
- KPI strip: "Standards range — low / high" (or "Calibration range")
- Validated range: "Quantifiable range (LLOQ / ULOQ)"
- Use the *validated* range for all `>ULOQ` / `<LLOQ` flags in the sample table — and say so explicitly in a footnote.

### 2. Status column is mangled — `✅ |` in one cell, `nterpolated |` in the next
In the sample results table, several rows show:

```
| Range  | Status         |
| ✅ |   | nterpolated |  |   ← rendering bug
| >ULOQ  | Interpolated   |   ← OK
```

The `I` of "Interpolated" has been eaten and the cell got split on a literal `|`
character that was meant to be a Markdown table delimiter. Rmd is rendering a
pipe-delimited string into two cells.

Affected rows in the data I inspected: AE, AG, BE, BG, CE, CF, CG, DA, DC, DD,
DE, DG, DH, EA, EC, ED. **Roughly half the in-range samples display broken.**

**Fix:** in the chunk that builds the Status cell, return clean text (not
`paste("✅ |", "Interpolated |")`); render the status icon + label in two
separate columns, or use a kable cell formatter, not literal `|`s.

### 3. `%%` literal leaking through to the rendered HTML
Multiple places have `%%` where one `%` was intended:
- "All standards show acceptable variability (<30%% CV)."
- "back-calculated standard accuracy (recovery 80-120%%, CV <20%%)."

This is `sprintf("…%%…")` being passed through `cat()` or rendered as Markdown
without a second pass. **Fix:** drop the second `%` (or use `%%` only in
`sprintf` format strings, never in raw glue/paste output destined for Markdown).

### 4. Brown-Forsythe statistic = 1.376 × 10³¹
"Statistic: 1.376e+31, p-value: 1.1171e-123, variance ratio = 14.1"

A variance-ratio of 14 is plausible. An F-statistic of 10³¹ is a numerical
artifact — almost certainly division by a near-zero within-group variance
(replicate of size 2 with identical readings → group variance = 0 → F → ∞).
Even if statistically meaningful, displaying it like that destroys the user's
trust in the rest of the numbers. **Fix:** clip to a sensible cap and report
"F > 1×10⁶ (effectively infinite — at least one group has zero within-group
variance)" or fall back to a reformulation.

### 5. Two separate "Detailed Sample Results Summary" sections
There's an `<h3>` "Detailed Sample Results Summary" containing only the footnote
and the quality-alert bullets, then immediately an `<h2>` "Detailed Sample
Results Summary" containing the actual table. Both have the same heading text.

**Fix:** merge — one section, with the table first and footnotes after, or
rename the first one to "Sample Quality Alerts".

---

## 🟧 Information design

### 6. The most important content is collapsed by default
Every `<details>` is closed:
- Standard Concentrations
- Quality Control Summary
- Limits of Quantification
- **Dose–Response Curve Analysis** ← the chart
- Four-Parameter Logistic Model Coefficients
- Detailed Sample Results table
- Sample variability visualization
- **Plate Heatmap**
- **Dose-Response Curve with Unknown Samples**
- Exclusion Audit Trail

A scientist opens this report to see the curve and the sample numbers. They
should not have to click 6 disclosure triangles before the page is useful.

**Fix:** open by default — the curve plot, the sample results table, the model
parameters, the plate heatmap. Keep "exclusion audit", "back-calculation
recovery", "Brown-Forsythe details" collapsed. Rule of thumb: collapse anything
the reader will skim past 80% of the time, expand the rest.

### 7. Three competing status banners in the first 1.5 screens
1. Blue-bordered "Executive Summary" box (top)
2. Green-gradient "Overall status: 🟢 Pass" box
3. KPI tile strip (R², RMSE, LLOQ, ULOQ, Overall ●Pass)

All three say "Pass". The KPI strip has the most useful info; the green box
duplicates 4/5 of those numbers in a different format; the blue box adds only
"date / standards count / sample count".

**Fix:** keep the KPI strip. Delete the green-gradient duplicate. Reduce the
blue exec-summary box to the three things not in the KPI strip (analyst, date,
sample count) and put it *next to* (not above) the KPIs as a 2-column header.

### 8. The "Range" column in the sample table contradicts itself
Sample AC: **Mean = 1,866 pg/mL** (well below KPI ULOQ of 4,000), but
**Range = ">ULOQ"**, **Status = Interpolated**.

The Range column is using the *validated* ULOQ (1,387) — the one in the green
box that I called out in #1. So the column is correct, but the same report tells
the reader ULOQ is 4,000. This is the same bug as #1 surfacing in a second
place — it has to be fixed in both spots together.

### 9. Extrapolated samples get the same row treatment as good ones
Sample BB: 6,150 ± 4,637 pg/mL, CV = 75%, CI = [0 – 47,815]. That row sits
adjacent to clean samples like AE (1,386 ± 45, CV 3.2%) with identical styling.

**Fix:** colour-shade rows by status. Yellow background for >ULOQ /
extrapolated, red for CV > 30% or CI hitting zero. Visually the user should
spot the unreliable rows in 2 seconds, not read a Status column on row 23.

### 10. No methods detail
The Methods section is one sentence. For an assay report destined for QA / a
publication appendix, this is too thin. Suggested minimal additions:
- Weighting selection rule (currently buried two sections later)
- LLOQ/ULOQ derivation criteria (recovery 80–120%, CV <20% — currently in a
  collapsed details block)
- CI method (delta method, t-distribution) — currently a footnote
- Outlier rule, if used
- Software version + key package versions (already in the report, just hoist
  them up here)

### 11. No glossary / abbreviations panel
RBA, ELISA, LL.4, IC50, EC20, EC80, %B/B0, B0, NSB, LLOQ, ULOQ, CV, SE — none
defined. Add a collapsed `<details>` "Abbreviations" block at the foot, or a
`<dl>` in the Methods.

### 12. Author block uses `<h4>`
`<h4 class="author">Kristof Moeller (IAEA, Monaco) and Arnold Molina Porras…</h4>`
renders identically to every "Analysis Notes" / "Methods" subhead. Visually it
reads like another section heading floating between the title and the body.

**Fix:** style as a metadata block — small, muted, paired with date/version/
contact. Use a `<header>` element, not h4.

---

## 🟦 Visual / typography

### 13. Two type systems collide
Body text is Lato. KPI tiles use IBM Plex Mono inline (`font-family: 'IBM Plex Mono', monospace`). The mono tiles look like they belong to a different report. Either commit to a tabular-numerals serif/sans for the values *or* pull the mono treatment into the rest of the numeric tables (sample results, standards) for consistency.

**Recommendation:** use mono only for numeric *measurements* (concentrations, R², RMSE), keep Lato for labels. Apply this rule globally — the sample-results table currently uses Lato for the numbers, which fights the KPI tiles.

### 14. Inconsistent status iconography
- KPI strip uses a green dot bullet `●`
- Status box uses 🟢 emoji
- Sample table uses ✅ emoji
- "Quality Alert" uses ⚠️
- Range column uses bare text "`>ULOQ`"

Pick one system. Suggested: small CSS dot (●) in three colours (green / amber / red) plus a one-word label. Drop the emoji.

### 15. Massive inline styles
Every KPI tile, status box, and quality alert is built with 200+ characters of
inline `style="…"`. This makes the Rmd hard to maintain and prevents the print
stylesheet from overriding anything.

**Fix:** move to classes in a single `<style>` block at the top of the report:
- `.bs-kpi-strip`, `.bs-kpi-tile`, `.bs-kpi-tile.is-pass / is-warn / is-fail`
- `.bs-status-banner.is-pass / .is-warn / .is-fail`
- `.bs-quality-alert`
The HTML body becomes 2× shorter and re-themable in one place.

### 16. No print stylesheet
This report is going to be saved as PDF and emailed. Right now print rendering
will be: enormous Lato fonts, 3-column TOC fighting for space, plotly canvases
that may or may not render, full-bleed gradient banners. Add a small
`@media print` block:
- Hide the TOC (`.tocify { display: none }`)
- Force `<details>` open on print (`details { display: block } summary { display: none }` or set `[open]` programmatically)
- Reduce KPI tiles, banners to print-friendly grayscale
- Add page-break hints (`h1, h2 { page-break-after: avoid }`, `.bs-kpi-strip, table { page-break-inside: avoid }`)

### 17. Standards table is over-narrow
`width:auto !important; margin-left:auto; margin-right:auto;` with
`min-width:75px` per cell renders an 8-row table that's ~250px wide and
floats lonely in the middle of a 900px column. **Fix:** drop the `width:auto`
override, let it be 50% width, or display side-by-side with the methods.

### 18. Sample results table is too dense — 12 columns
`Group | IDs | Type | n | Mean | SD | SE | CI | CV | %B/B0 | Range | Status`.
12 columns × 32 rows = a wall of numbers. Suggested:
- Collapse SD + SE into one column ("SD ±") — they encode the same thing for n=2
- Move "Sample IDs" and "n" into a tooltip / second line under group
- Right-align all numerics, left-align labels
- Use tabular-nums (`font-feature-settings: 'tnum'`) so digits column-align

### 19. Three contradictory shades of green
- KPI tile: `#009e73` (colour-blind safe green)
- Status banner: `#4CAF50` border with `#E8F5E9`/`#F1F8E9` gradient
- Quality alert blocks: `#1976D2` blue
Pick one accent palette and use it everywhere. The Okabe–Ito green (`#009e73`)
the KPI strip already uses is the right choice for accessibility.

### 20. TOC sub-items at 12px
Hard to read. Bump to 13–14px. Also: the TOC takes 25% of viewport width; on
laptop screens this leaves a single narrow column for content. Either narrow
the TOC to 18% or put it in a collapsible drawer.

---

## Suggested order of attack for v1.0

1. **Fix the LLOQ/ULOQ duplicate-label bug (#1, #8).** Single biggest
   correctness issue.
2. **Fix the mangled Status column (#2).** Half the rows currently render broken.
3. **Patch the `%%` and 10³¹ artifacts (#3, #4).** Cheap, makes the report
   trustworthy.
4. **Open the right `<details>` by default (#6).** One-line config change,
   biggest UX win.
5. **Move inline styles to classes + add print stylesheet (#15, #16).** Sets
   you up to iterate visuals quickly.
6. **Collapse the three status banners into one (#7).**
7. Everything else (#9–#20) is polish; do as time allows.

Items 1–4 are ~half a day of work and would dramatically raise the perceived
quality of the v1.0 release. Items 5–6 are another half day. The visual polish
beyond that is genuinely optional for a v1.0.

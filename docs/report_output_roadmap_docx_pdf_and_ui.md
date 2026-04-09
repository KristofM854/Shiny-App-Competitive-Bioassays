# Roadmap: Reliable DOCX/PDF Report Support and Analysis Settings UI Improvements

Repo: `KristofM854/Shiny-App-Competitive-Bioassays`

## Purpose
This roadmap covers two new tasks:

1. **Major:** Make report generation robust across **HTML, DOCX, and PDF**.
   - Fix the current DOCX failure caused by HTML-only output inside report templates.
   - Prepare a full PDF scaffold even when the current development machine does not have a LaTeX environment.
   - Add a **graceful fallback** so that if a user selects PDF but PDF rendering is not available in their environment, the app falls back to HTML with a clear notification.

2. **Minor:** Make **Advanced Options** under **4. Analysis Settings** much more visible in the Shiny app.

---

# 1. Clarifying the PDF approach

## Can PDF support be prepared without LaTeX installed locally?
Yes.

The repo can be fully prepared for PDF support **without** a LaTeX environment on the current machine, as long as we separate:
- **code scaffolding**, from
- **local render capability**.

That means we can already implement:
- a PDF option in the UI,
- format-aware report rendering logic,
- static-figure fallback for DOCX/PDF,
- graceful detection of missing PDF dependencies,
- and clean fallback to HTML.

What cannot be fully proven locally without LaTeX is the final end-to-end PDF render itself.

## Could PDF be rendered online somewhere instead?
Not in a way the current app can safely rely on by default.

In principle, PDF rendering could happen in:
- GitHub Actions,
- Posit Connect / Shiny Server with TeX installed,
- a remote Linux machine with TinyTeX or TeX Live,
- or a containerized environment.

But the current Shiny app is a **local interactive app**. It does not have a built-in remote rendering service. So the practical design should be:

- If the user has a working PDF environment locally, render PDF locally.
- If they do not, the app should **gracefully fall back to HTML** and tell them why.

This is the safest and most maintainable behavior.

## Recommended PDF strategy
Implement PDF support in three layers:

### Layer A — format-safe report code
Make sure the report templates can render correctly in non-HTML formats by:
- never relying on Plotly or other HTML widgets for DOCX/PDF,
- avoiding HTML-only constructs where possible,
- switching automatically to static ggplot and plain tables.

### Layer B — runtime capability detection
Before attempting PDF rendering, check whether a LaTeX engine is available.
Suggested checks:
- `rmarkdown::pandoc_available()`
- `tinytex::is_tinytex()` if `tinytex` is installed
- `Sys.which("pdflatex")`, `Sys.which("xelatex")`, or `Sys.which("lualatex")`

### Layer C — graceful fallback
If the user selected PDF but no PDF toolchain is available:
- show a clear notification,
- remove PDF from the actual render set,
- add HTML automatically if needed,
- and continue generating the report instead of failing hard.

Recommended user-facing behavior:
- message like:
  - "PDF rendering is not available in this environment because no LaTeX engine was detected. An HTML report was generated instead."

This gives users a working output and a clear explanation.

---

# 2. Root cause of the DOCX error

## Problem
Current report templates emit HTML-only output in code paths that are still reached during DOCX rendering.

Typical risky constructs include:
- `plotly::ggplotly(...)`
- `htmltools::tagList(...)`
- HTML-styled table output
- literal HTML collapsible blocks such as `<details>` / `<summary>`

DOCX cannot render HTML widgets directly. That is why the current render fails with:
- "Functions that produce HTML output found in document targeting docx output"

## Correct design principle
Use:
- **interactive Plotly only for HTML**
- **plain static ggplot for DOCX and PDF**

Do the same for any HTML-only table styling or collapsible structure.

---

# 3. What needs to change for DOCX to always work

## 3.1 Add central output-format helper functions
Create a small helper layer, probably in `reports/plot_functions.R` and/or `reports/report_functions.R`, with functions such as:

- `is_html_output_safe()`
- `is_docx_output()`
- `is_pdf_output()`
- `render_plot_for_output(plot_obj, tooltip = NULL)`
- `render_table_for_output(df, caption = NULL, ...)`
- `section_start(title)`
- `section_end()`

Purpose:
- HTML → interactive Plotly + HTML conveniences
- DOCX/PDF → static ggplot + plain pandoc-safe tables + plain headings

This avoids scattered format checks throughout the templates.

## 3.2 Replace direct Plotly rendering with helper-based rendering
Audit both report templates and replace all direct uses of:
- `plotly::ggplotly(...)`
- `print(htmltools::tagList(...))`

with centralized helper-based rendering.

### Desired behavior
- HTML: return interactive plotly object
- DOCX: print static ggplot
- PDF: print static ggplot

## 3.3 Make collapsible sections format-aware
Current templates use many literal `<details>` / `<summary>` blocks.

These are acceptable in HTML, but they are not ideal for DOCX/PDF.

Introduce helper functions like:
- `section_start(title, collapsible = TRUE)`
- `section_end()`

Behavior:
- HTML → emit `<details><summary>...` wrappers
- DOCX/PDF → emit plain markdown heading / subheading only

This preserves readability across all formats.

## 3.4 Ensure tables are safe for all formats
Where tables still use `kableExtra` or HTML-oriented rendering, switch to:
- plain `knitr::kable(..., format = "pandoc")` for DOCX/PDF
- enhanced styling only for HTML

The multi-wavelength template already partially follows this pattern and can be used as the model.

## 3.5 Ensure child reports are also format-safe
The multi-wavelength template uses `knitr::knit_child()` on `unified_analysis_template.Rmd`.

That means:
- the child template must also be completely DOCX/PDF safe,
- otherwise the parent multi-wavelength report can still fail.

So the **unified template** is the critical path.

---

# 4. What needs to change to enable PDF support cleanly

## 4.1 Add PDF to the app export options
In `app.R`, extend the export format selector to include:
- `"PDF" = "pdf"`

## 4.2 Confirm render pipeline supports PDF
In `report_pipeline.R`, confirm or extend mapping from app format keys to rmarkdown formats:
- `html` -> `html_document`
- `docx` -> `word_document`
- `pdf` -> `pdf_document`

## 4.3 Add runtime PDF capability detection
Before attempting PDF rendering, check whether a TeX engine is available.

Suggested helper in `report_pipeline.R`:
- `pdf_render_available()`

Suggested detection logic:
1. if `tinytex` is installed and `tinytex::is_tinytex()` is TRUE → available
2. else if any of `pdflatex`, `xelatex`, `lualatex` is found in `Sys.which(...)` → available
3. else → unavailable

## 4.4 Add graceful fallback to HTML
If the user selects PDF but `pdf_render_available()` is FALSE:
- notify the user clearly,
- remove PDF from the selected render list,
- ensure HTML is included,
- continue rendering HTML and/or DOCX instead of failing.

### Recommended behavior
If user selected only PDF:
- silently convert to HTML + message

If user selected PDF + HTML:
- keep HTML, drop PDF + message

If user selected PDF + DOCX:
- keep DOCX, optionally also add HTML depending on preferred UX

Recommended simple rule:
- if PDF is unavailable, remove it
- if nothing remains, add HTML

## 4.5 Add environment note to the roadmap/user messaging
Document clearly that:
- PDF requires a local TeX engine
- the app scaffold supports PDF rendering
- but environments without TeX will fall back gracefully to HTML

---

# 5. Deep-dive file audit targets

## High-priority files to change
- `reports/unified_analysis_template.Rmd`
- `reports/multiwavelength_analysis_template.Rmd`
- `reports/plot_functions.R`
- `reports/report_functions.R`
- `report_pipeline.R`
- `app.R`
- optionally `style.css`

## What to inspect in the templates
### In `reports/unified_analysis_template.Rmd`
Audit all chunks that:
- call `plotly::ggplotly(...)`
- call `htmltools::tagList(...)`
- emit raw HTML wrappers
- rely on HTML-only styled table output

Particularly important:
- weighting comparison plot
- dose-response plot
- sample variability plot
- DRC with samples plot
- any chunk currently wrapped in `if (knitr::is_html_output())`

### In `reports/multiwavelength_analysis_template.Rmd`
Audit:
- parent-level plotting behavior
- child-template rendering interactions
- any remaining HTML-specific assumptions
- whether all tables and plots degrade cleanly to DOCX/PDF-safe output

---

# 6. Minor task: make Advanced Options more prominent

## Problem
The current **Advanced Options** section in **4. Analysis Settings** is too easy to overlook.
Even experienced users may miss it.

## Goal
Make it clearly visible without disrupting the workflow.

## Recommended UI changes
### 6.1 Replace the understated collapsible style with a stronger settings card
Wrap the advanced section in a visually highlighted panel.
Possible treatment:
- tinted background
- border-left accent
- stronger heading
- gear/sliders icon

### 6.2 Make the label more explicit
Replace generic text like:
- `Advanced Options`

with something clearer, for example:
- `Advanced Options — weighting, CI, outliers, and QC thresholds`

### 6.3 Add a short explanatory line above it
Suggested text:
- `Important: this section controls weighting comparison, confidence intervals, outlier detection, and QC thresholds.`

### 6.4 Consider opening it by default
This is the most direct visibility fix.
Recommended approach:
- either open by default,
- or use a highly visible collapsed card.

My recommendation: **open by default** unless there is a strong reason not to.

### 6.5 Optional: add a subtle callout after upload
A small notice near the Convert workflow could remind users that advanced analysis settings are available in Tab 4.

---

# 7. Recommended implementation phases

## Phase 1 — make DOCX reliable
1. Add format helper functions
2. Refactor all Plotly paths to static-output fallbacks for non-HTML
3. Refactor collapsible sections to be format-aware
4. Make tables consistently DOCX-safe
5. Test single-wave and multi-wave DOCX generation

## Phase 2 — scaffold PDF properly
1. Add PDF to the export options in the app
2. Add runtime PDF capability detection
3. Add graceful fallback to HTML when PDF is unavailable
4. Test behavior with and without TeX installed
5. If TeX is unavailable, verify fallback messaging and HTML output

## Phase 3 — improve Analysis Settings visibility
1. Restyle Advanced Options into a highlighted card
2. Strengthen the label and helper text
3. Decide whether it should default to open
4. Test discoverability and visual clarity

---

# 8. Acceptance criteria

## DOCX
- DOCX report renders without HTML-widget errors
- All plots appear as static figures
- Tables render cleanly
- No broken sections due to HTML-specific wrappers

## PDF
- PDF appears as an export option in the app
- If TeX is available, PDF renders successfully
- If TeX is not available, app falls back gracefully and informs the user
- No hard failure when PDF is selected in an unsupported environment

## HTML
- HTML remains fully interactive where intended
- Existing Plotly interactivity is preserved for HTML output

## UI
- Advanced Options are much more noticeable in Tab 4
- Users can immediately tell what that section controls
- The visibility improvement does not make the settings page feel cluttered

---

# 9. Codex task breakdown

## Task 1
Create a format-aware rendering helper layer and refactor report templates so DOCX and PDF always use static plots and non-HTML-safe structures.

## Task 2
Make collapsible report sections format-aware: HTML uses `<details>`, DOCX/PDF use plain headings.

## Task 3
Add PDF as an app export option, detect whether PDF rendering is supported locally, and implement graceful fallback to HTML when it is not.

## Task 4
Improve the visibility and prominence of Advanced Options in 4. Analysis Settings.

---

# One-line summary

The correct long-term design is: HTML keeps Plotly interactivity, DOCX and PDF always use static output, PDF is exposed in the app with runtime capability detection, and unsupported environments fall back gracefully to HTML instead of failing.

# Bioassay Suite — GUI Refresh · Implementation Guide **v3.2**

> **Read this first.** v3.1 polish landed successfully — form widths, header alignment, radio centering, and the `[object Object]` button-label bug are all resolved. v3.2 is a **second polish patch** (eight micro-fixes) plus **Phase A of i18n expansion** (French full release + Russian/Chinese beta).
>
> **Ground rules (unchanged):**
> 1. **No business logic changes.** Do not touch DRC fitting, outlier detection, tissue normalization, multi-wavelength concordance, auto-save, file-import logic, output-folder naming, or `runApp()` entry points.
> 2. All four plate matrices stay with their observers and reactives intact.
> 3. The bilingual i18n engine stays. You may **add** keys and add languages; never rename existing keys.
> 4. If anything in this guide conflicts with existing code, **stop and ask**.
> 5. **Hard-reload after every CSS change** (Cmd+Shift+R / Ctrl+Shift+F5). The cache-busting query string in `app.R`'s `<link rel="stylesheet">` tag should already be in place from v3.1.

---

## Section overview

| § | What | Files touched |
|---|---|---|
| 1 | Native `<select>` artifact + chevron clipping (Tab 1 + topbar) | `www/style.css` |
| 2 | Tab 2 plate-layout header — vertical alignment of select + file input | `www/style.css` |
| 3 | Move Undo/Redo from import header into Bulk Actions panel | `app.R` (markup) |
| 4 | Tidy Analysis Settings — collapse Advanced Options + DRC weighting layout | `app.R` (markup) + `www/style.css` |
| 5 | Report format segmented control + Give Feedback button centering | `www/style.css` |
| 6 | Topbar language select centering | `www/style.css` |
| 7 | i18n expansion Phase A — French (full) + Russian (beta) + Chinese (beta) | `R/i18n.R` (or equivalent), `www/style.css`, Rmd report templates, `app.R` |

§ 3, § 4, § 7 touch R / Rmd. Everything else is CSS appended to `www/style.css`.

---

## § 1 · Native `<select>` artifact + long-option clipping

### Problem in detail

Shiny's `selectInput` ships **two** elements per control: the pretty `selectize.js` UI (what the user interacts with) and the underlying native `<select>` (used for form value submission). Selectize is supposed to position the native `<select>` invisibly via `position: absolute; opacity: 0`. Some CSS rule (likely from earlier polish iterations) lets the native element show through as a thin black-bordered white rectangle behind the selectize text.

Separately, in Tab 1 the "ELISA (Enzyme-Linked Immunosorbent Assay)" option's text overflows past the dropdown chevron because `text-overflow: ellipsis` doesn't engage when the inner `.item` element extends across the right-padding gutter.

### 1.1 — Append to `www/style.css`

```css
/* ============================================
   v3.2 §1 — Hide native <select> + ellipsis on long options
   Global scope (NOT .bs-card-scoped) so it covers the topbar
   language select AND every form select in the wizard.
   ============================================ */

/* Hide the native <select> that selectize.js wraps. selectize keeps
   it in the DOM for form-value submission but it should never paint. */
.selectize-control > select.shiny-bound-input,
.selectize-hidden-accessible {
  position: absolute !important;
  left: -9999px !important;
  opacity: 0 !important;
  width: 1px !important;
  height: 1px !important;
  pointer-events: none !important;
  border: 0 !important;
}

/* Long-option ellipsis: force the .item element to respect the
   right-padding gutter so text truncates before hitting the chevron. */
.bs-card .selectize-input {
  padding-right: 28px !important;
}
.bs-card .selectize-input > .item {
  max-width: calc(100% - 8px);
  overflow: hidden;
  text-overflow: ellipsis;
  white-space: nowrap;
  display: inline-block;
}
```

### 1.2 — Acceptance for § 1

- No black-bordered white rectangle visible behind any select control (Tab 1, Tab 2 header, topbar language picker).
- "ELISA (Enzyme-Linked Immunosorbent Assay)" displays cleanly with ellipsis if it overflows the 320px-min-width control; the chevron remains fully visible.
- `document.querySelectorAll('select.shiny-bound-input').forEach(s => console.log(getComputedStyle(s).opacity))` returns `"0"` for every entry.

---

## § 2 · Tab 2 plate-layout header vertical alignment

### Problem in detail

The green header bar contains four cells: "Load Preset Layout" select (with label above), "Import Layout" file input (with label above), "Save Layout" button (no label), and Undo/Redo buttons (no label — these will be moved out by § 3, but the alignment fix below applies to the remaining controls).

`align-items: center` on the row centers each cell's bounding box on the row's vertical midline. But cells with stacked label-then-control have a different bounding-box center than cells with just a control — so the *control* portions don't line up visually.

### 2.1 — Append to `www/style.css`

```css
/* ============================================
   v3.2 §2 — Plate Layout header: bottom-align controls
   Each cell becomes a flex column; controls bottom-align so they
   share a baseline regardless of whether the cell has a label.
   ============================================ */

.bs-layout-header,
.bs-card .bs-layout-header {
  display: flex;
  align-items: stretch;       /* was: center — switch to stretch */
  gap: 24px;
  padding: 12px 18px !important;
  min-height: 72px;
}

/* Each direct child becomes a flex column with controls bottom-aligned. */
.bs-layout-header > .form-group,
.bs-layout-header > .btn,
.bs-layout-header > .action-button,
.bs-layout-header > div {
  display: flex;
  flex-direction: column;
  justify-content: flex-end;   /* controls sit on the bottom edge */
  margin: 0 !important;
}

/* Labels stay above their control; tighten the label-to-control gap. */
.bs-layout-header .form-group .control-label {
  margin: 0 0 4px 0 !important;
  font-size: 11px;
  line-height: 1.2;
  color: var(--c-ink-3);
  text-transform: uppercase;
  letter-spacing: 0.04em;
  font-weight: 600;
}

/* Buttons in the header (Save Layout, etc.) — match control height. */
.bs-layout-header > .btn,
.bs-layout-header > .action-button {
  height: 36px;
  align-self: flex-end;
}
```

### 2.2 — Acceptance for § 2

- All controls in the green header bar share the same bottom baseline.
- Labels render in small uppercase grey above their controls.
- "Save Layout" button is 36 px tall and aligned with the bottom of the select / file input.

---

## § 3 · Move Undo/Redo from header to Bulk Actions panel

### Problem in detail

The Undo/Redo buttons currently live in `.bs-layout-header` next to the file-import controls. Semantically they belong with the **plate matrix** (they undo/redo cell edits), not with file import. Move them into the Bulk Actions panel below the "Set all wells" / "Clear all" controls.

### 3.1 — Edit `app.R`

Find the `.bs-layout-header` block. Locate the Undo/Redo wrapper (likely `div(class = "bs-undo-redo", actionButton("undo_layout", ...), actionButton("redo_layout", ...))`). **Cut** that block out of the header.

Find the Bulk Actions panel (search for `bs-bulk-actions` or the `actionButton("set_all_wells", ...)` block). At the bottom of that panel, **paste**:

```r
div(
  class = "bs-undo-redo",
  style = "margin-top: 12px; padding-top: 12px; border-top: 1px solid var(--c-line); display: flex; gap: 8px;",
  actionButton("undo_layout", "Undo", icon = icon("rotate-left"), class = "btn-default btn-sm"),
  actionButton("redo_layout", "Redo", icon = icon("rotate-right"), class = "btn-default btn-sm")
),
```

### 3.2 — Drop the now-obsolete CSS

If `www/style.css` has a `.bs-layout-header .bs-undo-redo { ... }` rule from v3.1, delete it (the wrapper is no longer in that header).

### 3.3 — Acceptance for § 3

- Undo/Redo buttons no longer appear in the green import header.
- They appear below "Set all wells" / "Clear all" in the Bulk Actions panel, separated by a hairline divider.
- Clicking Undo still rolls back the last cell edit on the active matrix layer.
- Clicking Redo still re-applies it.

---

## § 4 · Tidy Analysis Settings (Tab 4)

### Problem in detail

The Analysis Settings panel currently shows everything at once — DRC weighting checkboxes, then orange "Advanced Options" section with %B/B0 bounds, CI method, outlier detection, and max CV — all in a single scroll. Users see too much. Two changes:

**4a.** Collapse Advanced Options behind a disclosure (`<details>`).
**4b.** Force the four DRC weighting checkboxes onto one-per-line so "1/Y (moderate)" gets its own row.

### 4.1 — Edit `app.R` — collapse Advanced Options

Find the Analysis Settings block. Locate the Advanced Options section (search for "Advanced Options" or the orange-styled header). Wrap its body in `tags$details(...)`:

```r
tags$details(
  class = "bs-advanced",
  open = FALSE,   # collapsed by default
  tags$summary(
    class = "bs-advanced-summary",
    icon("sliders-h"),
    span(class = "title", tr("advanced_options", lang)),
    span(class = "sub", tr("advanced_options_sub", lang))
  ),
  div(
    class = "bs-advanced-body",
    # ... existing Lower/Upper %B/B0 bound numericInputs ...
    # ... existing CI method radioButtons ...
    # ... existing outlier detection checkbox ...
    # ... existing max CV numericInput ...
  )
),
```

Add to the i18n dictionary (EN baseline):
```r
i18n$en$advanced_options     <- "Advanced Options"
i18n$en$advanced_options_sub <- "weighting, CI, outliers, and QC thresholds"
i18n$es$advanced_options     <- "Opciones avanzadas"
i18n$es$advanced_options_sub <- "ponderación, IC, outliers y umbrales QC"
```

### 4.2 — Edit `app.R` — wrap DRC weighting in a class

Find the `checkboxGroupInput("drc_weighting", ...)` call. Wrap it (or its parent `div`) so it carries `class = "bs-drc-weighting"`:
```r
div(
  class = "bs-drc-weighting",
  checkboxGroupInput("drc_weighting", tr("drc_weighting_label", lang),
                     choices = c(...), selected = ...)
),
```

### 4.3 — Append to `www/style.css`

```css
/* ============================================
   v3.2 §4 — Analysis Settings tidy
   ============================================ */

/* Advanced Options disclosure */
.bs-advanced {
  margin-top: 18px;
  border-top: 1px solid var(--c-line);
  padding-top: 14px;
}
.bs-advanced > summary {
  cursor: pointer;
  list-style: none;
  display: flex;
  align-items: baseline;
  gap: 10px;
  padding: 6px 0;
  color: #c75c1c;
  font-weight: 600;
  user-select: none;
}
.bs-advanced > summary::-webkit-details-marker { display: none; }
.bs-advanced > summary::before {
  content: "▸";
  display: inline-block;
  margin-right: 2px;
  transition: transform 120ms;
  color: #c75c1c;
  font-size: 11px;
}
.bs-advanced[open] > summary::before { transform: rotate(90deg); }
.bs-advanced .title { font-size: 14px; }
.bs-advanced .sub {
  font-size: 12px;
  color: var(--c-ink-3);
  font-weight: 400;
}
.bs-advanced-body {
  padding: 12px 0 4px 22px;
}

/* DRC weighting: force one checkbox per line */
.bs-card .bs-drc-weighting .checkbox,
.bs-card .bs-drc-weighting .shiny-options-group > .checkbox {
  display: flex !important;
  width: 100%;
  margin: 4px 0;
}
.bs-card .bs-drc-weighting label {
  width: 100%;
}
```

### 4.4 — Acceptance for § 4

- Analysis Settings shows only DRC weighting + the chevron'd "Advanced Options" disclosure on first paint.
- Click the disclosure: %B/B0 bounds, CI method, outlier detection, max CV expand smoothly.
- Click again: collapses.
- The four DRC weighting checkboxes (Unweighted, 1/Y (moderate), 1/Y² (recommended for immunoassays), Auto (data-driven)) each occupy their own row.

---

## § 5 · Report format + Give Feedback button centering

### Problem in detail

Two button-centering issues remain after v3.1:
- The "HTML / Word (DOCX) / PDF" segmented control on Tab 5: each label left-aligns inside its segment.
- "Give Feedback" full-width cyan button: text + icon sit slightly above the vertical midline because Bootstrap's default `line-height: 1.42857143` adds asymmetric phantom space.

### 5.1 — Append to `www/style.css`

```css
/* ============================================
   v3.2 §5 — Report format + Give Feedback centering
   ============================================ */

/* Report format segmented control: center each label inside its segment.
   If your markup uses a class other than .bs-report-format on the
   wrapper, replace it accordingly. */
.bs-card .bs-report-format .radio-inline,
.bs-card .bs-report-format > label,
.bs-card .bs-report-format .shiny-options-group label {
  display: inline-flex !important;
  align-items: center;
  justify-content: center;
  text-align: center;
  min-width: 88px;
  padding: 6px 14px;
  margin: 0 !important;
}

/* Give Feedback: symmetric padding + line-height: 1 to neutralize
   Bootstrap's 1.428 line-height that pushes content above midline. */
.bs-card #give_feedback,
.bs-card .bs-feedback-btn {
  display: flex !important;
  align-items: center !important;
  justify-content: center !important;
  gap: 8px;
  height: 44px;
  padding: 0 16px !important;
  line-height: 1 !important;
}
.bs-card #give_feedback > i,
.bs-card .bs-feedback-btn > i {
  display: inline-flex;
  align-items: center;
  line-height: 1;
}
```

### 5.2 — Acceptance for § 5

- Each report-format segment ("HTML", "Word (DOCX)", "PDF") has its label horizontally and vertically centered.
- "Give Feedback" icon + label sit on the exact vertical midline of the cyan button.

---

## § 6 · Topbar language select centering

### Problem in detail

The "English" text in the topbar language picker is not vertically centered inside the 28 px pill. Same line-height issue as Give Feedback — Bootstrap's default `line-height: 1.42857143` against a fixed-height 28 px container produces asymmetric vertical padding.

### 6.1 — Append to `www/style.css`

```css
/* ============================================
   v3.2 §6 — Topbar language select centering
   ============================================ */

.bs-topbar .actions .bs-lang-select .selectize-input {
  display: flex !important;
  align-items: center;
  justify-content: flex-start;
  padding: 0 24px 0 10px !important;
  line-height: 1 !important;
}
.bs-topbar .actions .bs-lang-select .selectize-input > .item {
  line-height: 1;
  display: inline-flex;
  align-items: center;
}
```

### 6.2 — Acceptance for § 6

- "English" / "Español" / "Français" / etc. text sits on the exact vertical midline of the 28 px topbar language pill.
- No native-`<select>` artifact behind the pill (handled by § 1).

---

## § 7 · i18n expansion Phase A — French full + Russian/Chinese beta

> **Phase B (Arabic) is intentionally out of scope here** — RTL layout work needs its own focused session. We'll cover it in a future v3.3.

### 7.1 — Add three language keys to the i18n dictionary

Open `R/i18n.R` (or wherever `tr()` reads from). For every existing key, add three new entries:

```r
i18n$fr <- list(
  # Topbar
  app_title              = "Suite de bioessais",
  start_tour             = "Visite guidée",
  docs                   = "Docs",

  # Wizard tabs
  tab_config             = "Configuration",
  tab_layout             = "Disposition de plaque",
  tab_upload             = "Importer",
  tab_analysis           = "Analyse",
  tab_report             = "Rapport",

  # Tab 1
  type_of_assay          = "Type de dosage",
  analyte                = "Analyte",
  std_units              = "Unités de concentration des standards",
  num_standards          = "Nombre de standards",

  # Quick Start
  quick_start            = "Démarrage rapide",
  qs_rba_stx_title       = "RBA · Saxitoxine",
  qs_rba_stx_sub         = "Test de liaison au récepteur · 8 standards · triplicat",
  qs_elisa_cortisol_title = "ELISA · Cortisol",
  qs_elisa_cortisol_sub  = "ELISA compétitif · 7 standards · duplicat",
  qs_elisa_custom_title  = "ELISA · Personnalisé",
  qs_elisa_custom_sub    = "Modèle ELISA vide · à configurer",
  qs_demo_included       = "Données de démonstration incluses",
  qs_no_demo             = "Pas de données de démonstration",

  # Analysis settings
  drc_weighting_label    = "Pondération de la régression DRC :",
  drc_unweighted         = "Non pondérée",
  drc_1y                 = "1/Y (modérée)",
  drc_1y2                = "1/Y² (recommandée pour les immunoessais)",
  drc_auto               = "Auto (basée sur les données)",
  advanced_options       = "Options avancées",
  advanced_options_sub   = "pondération, IC, outliers et seuils QC",
  lower_bound            = "Limite inférieure %B/B0 :",
  upper_bound            = "Limite supérieure %B/B0 :",
  ci_method              = "Méthode d'intervalle de confiance :",
  ci_t_dist              = "Distribution t (par défaut)",
  ci_bootstrap           = "Bootstrap (1000 rééchantillonnages)",
  enable_outlier         = "Activer la détection d'outliers",
  max_cv_standards       = "CV maximum pour les standards (%) :",

  # Tab 5 — Report
  report_format          = "Formats de rapport :",
  generate_report        = "Générer le rapport",
  give_feedback          = "Donner un avis",

  # Layout / actions
  load_preset            = "Charger un modèle",
  import_layout          = "Importer la disposition",
  save_layout            = "Enregistrer la disposition",
  undo                   = "Annuler",
  redo                   = "Rétablir",
  set_all_wells          = "Tout définir",
  clear_all              = "Tout effacer",

  # Beta banner (not used for FR — full release)
  beta_warning           = ""
)
```

Then create **`i18n$ru`** (Russian, beta) and **`i18n$zh`** (Chinese Simplified, beta) with the same key list. **Translate in chunks of 30–50 keys at a time** to keep terminological consistency. For each chunk, the prompt to use:

> "Translate the following keys from English to **[Russian / Simplified Chinese]**. Domain: competitive bioassay (ELISA, RBA) analysis software for environmental and pharmaceutical labs. Tone: formal, professional. Keep these terms in their original form (do not translate): LLOQ, ULOQ, NSB, B0, CV, EC50, R², CSV, DOCX, PDF, ELISA, RBA, DRC. For Chinese: use Simplified (PRC convention). For Russian: use scientific Russian, formal register."

For the **beta banner** keys:
```r
i18n$ru$beta_warning <- "Этот перевод сгенерирован машиной и может содержать ошибки. Проверяйте критические значения по английской версии."
i18n$zh$beta_warning <- "此翻译由机器生成，可能包含错误。请对照英文版本核对关键数值。"
```

### 7.2 — Add the language picker entries

In `app.R`, find the topbar `selectInput("app_language", ...)` and extend the choices:

```r
selectInput("app_language", NULL,
            choices = c(
              "English"            = "en",
              "Español"            = "es",
              "Français"           = "fr",
              "Русский (бета)"     = "ru",
              "中文 (测试版)"       = "zh"
            ),
            selected = "en",
            width = "140px")
```

The two beta languages have a "(beta)" marker in their **own** language. Bump the `bs-lang-select` width from 110 px to 140 px to fit "Русский (бета)":

```css
/* Append to www/style.css */
.bs-topbar .actions .bs-lang-select .selectize-control {
  width: 140px !important;
  min-width: 140px;
}
```

### 7.3 — Beta banner (one-time per language per browser)

Add to `app.R`'s `ui` (top of page, conditionally rendered):

```r
# Inserted near the top of the UI, after the topbar, before the wizard.
div(
  id = "bs-beta-banner",
  class = "bs-beta-banner",
  style = "display: none;",  # JS reveals it when needed
  span(class = "icon", icon("exclamation-triangle")),
  span(class = "msg", textOutput("beta_warning_text", inline = TRUE)),
  tags$button(
    type = "button",
    class = "bs-beta-dismiss",
    onclick = "dismissBetaBanner()",
    "×"
  )
),
```

In `server.R`:
```r
output$beta_warning_text <- renderText({ tr("beta_warning", input$app_language) })
```

JS to add to `app.R` (inside the existing `tags$script(HTML(...))` block or a new one):
```js
window.dismissBetaBanner = function() {
  var lang = $('#app_language').val();
  localStorage.setItem('bs_beta_dismissed_' + lang, '1');
  $('#bs-beta-banner').hide();
};

function refreshBetaBanner() {
  var lang = $('#app_language').val();
  var isBeta = (lang === 'ru' || lang === 'zh');
  var dismissed = localStorage.getItem('bs_beta_dismissed_' + lang) === '1';
  if (isBeta && !dismissed) {
    $('#bs-beta-banner').show();
  } else {
    $('#bs-beta-banner').hide();
  }
}

$(function() {
  refreshBetaBanner();
  $(document).on('change', '#app_language', function() {
    setTimeout(refreshBetaBanner, 100);
  });
});
```

CSS:
```css
.bs-beta-banner {
  display: flex;
  align-items: center;
  gap: 12px;
  padding: 10px 18px;
  background: var(--c-warn-bg, #fef3df);
  border-bottom: 1px solid var(--c-warn-border, #e6c97a);
  color: var(--c-warn-ink, #6a4c0e);
  font-family: var(--ff-sans);
  font-size: 13px;
}
.bs-beta-banner .icon { color: #c75c1c; flex-shrink: 0; }
.bs-beta-banner .msg { flex: 1; }
.bs-beta-banner .bs-beta-dismiss {
  background: transparent;
  border: 0;
  font-size: 18px;
  line-height: 1;
  color: var(--c-warn-ink, #6a4c0e);
  cursor: pointer;
  padding: 4px 8px;
  flex-shrink: 0;
}
.bs-beta-banner .bs-beta-dismiss:hover {
  background: rgba(0,0,0,0.06);
  border-radius: 4px;
}
```

### 7.4 — Translate the Rmd report templates

For each Rmd report template in the project (likely `reports/*.Rmd`), the translation strategy depends on how text is currently embedded:

**Option A (preferred):** the template uses `tr()` calls inline. Then translation is automatic — the new dictionary entries cover it.

**Option B:** the template has hardcoded English text. Refactor to use `tr()`:
```r
`r tr("report_section_introduction", lang)`
```
…and add the corresponding keys to the dictionary.

Translate the report-specific keys as a separate chunk per language with this domain hint:

> "Translate scientific report section headers and prose for a competitive bioassay quality-control report. Keep tables, formulas, and chemical names untranslated. Tone: formal scientific."

### 7.5 — Acceptance for § 7

- Topbar language picker shows five entries: English, Español, Français, Русский (бета), 中文 (测试版).
- Switching to Français: every UI string flips to French. No `[object Object]`. No untranslated keys.
- Switching to Russian or Chinese: same, plus a yellow beta banner appears below the topbar with the localized warning text.
- Click the banner's `×`: it dismisses and stays dismissed for that language across page reloads (`localStorage`).
- Switch from Russian to Chinese: banner reappears (different language, not yet dismissed).
- Generate a report in each language: the Rmd output shows translated section headers and prose. Numbers, units, and chemical names remain in their canonical form.
- Switch back to English: banner hides; no residual beta styling anywhere.

---

## § 8 · Apply order

Apply in this order — each builds on the prior:

1. **§ 1** — global hide rule for native `<select>`. Verify the artifact is gone everywhere.
2. **§ 2** — Tab 2 header alignment. Verify controls share a baseline.
3. **§ 3** — Move Undo/Redo into Bulk Actions. Verify clicks still work.
4. **§ 4** — Analysis Settings tidy. Verify disclosure expands and DRC checkboxes stack.
5. **§ 5** — Report format + Give Feedback centering. Verify with hard-reload.
6. **§ 6** — Topbar language select centering. Verify "English" centers in the pill.
7. **§ 7** — i18n expansion. Apply in this sub-order:
   - 7.1 — French dictionary first (full release). Test EN → FR → EN.
   - 7.1 — Russian + Chinese dictionaries. Test EN → RU and EN → ZH.
   - 7.2 — Language picker entries.
   - 7.3 — Beta banner.
   - 7.4 — Rmd template translations.

After each section: hard-reload and run the relevant happy path. After § 7, run RBA Saxitoxin → Generate Report in **each of the five languages**.

---

## § 9 · Acceptance checklist (full v3.2)

### CSS / Layout
- [ ] No native `<select>` artifact behind any control.
- [ ] Long option names ellipsise without overlapping the chevron.
- [ ] Tab 2 plate-layout header: select + file input + button share a baseline.
- [ ] "English" centers in topbar language pill.
- [ ] Report format segments have centered labels.
- [ ] "Give Feedback" content is on the exact vertical midline.

### Markup
- [ ] Undo/Redo buttons are in Bulk Actions panel, not in the import header.
- [ ] Advanced Options collapses behind a chevron disclosure.
- [ ] DRC weighting checkboxes stack one-per-line.

### i18n
- [ ] Five languages selectable in topbar dropdown.
- [ ] French translations complete; no key falls back to English.
- [ ] Russian + Chinese translations complete; no key falls back to English.
- [ ] Beta banner appears for RU/ZH only and dismisses per-language to localStorage.
- [ ] All 5 languages produce a generated report with translated content.

### Regression
- [ ] RBA Saxitoxin end-to-end produces unchanged numbers in EN, ES, FR.
- [ ] ELISA Cortisol end-to-end produces unchanged numbers in EN, ES, FR.
- [ ] Auto-save still restores layout on relaunch.
- [ ] No new console errors in DevTools across any of the 5 languages.

---

## § 10 · Out of scope for v3.2

- **Arabic translation + RTL layout** (deferred to v3.3 as Phase B).
- Any business logic changes.
- Any new wizard tabs or step reordering.
- Any change to plate matrix rendering, Quick Start tile design, or wizard stepper visuals.

---

*v3.2 supersedes v3.1 only for the items above; v3.1 (and v3, v2) remain the spec for everything else.*

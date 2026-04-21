# Implementation Plan: Complete Spanish UI Translation

**Repo:** `KristofM854/Shiny-App-Competitive-Bioassays`  
**Scope:** Translate all hardcoded English UI text to use `tr()` calls with existing Spanish translations from `i18n.R`  
**Target:** Full bilingual interface across all 5 wizard tabs + modal dialogs  

---

## Architecture Overview

**Current State:** 
- `i18n.R` contains 290-301 translation keys with comprehensive Spanish translations
- ~239 keys are orphaned (no `tr()` calls in the codebase) 
- Only ~55 `tr()` calls exist, mostly in `server_common.R`
- `app.R` has 855 lines with zero `tr()` calls - all UI text hardcoded

**Target Architecture:**
- **Static text**: Convert to `uiOutput()` + `renderUI()` pattern with `tr()` calls
- **Input widgets**: Keep in `app.R`, update via language observer using `updateXxxInput()`  
- **Choice labels**: Translate fully (except proper names like "Saxitoxin")
- **Reuse existing keys**: Maximize use of existing Spanish translations in `i18n.R`

---

## Phase 1: Tab 1 - Configuration (Priority: High)

### 1.1 Quick Start Panel
**File:** `app.R` lines ~85-105

Current hardcoded text:
```r
h4("Quick Start")
p("Choose a preset to auto-configure...")
actionButton("qs_rba_stx", label = tagList(icon("flask"), " RBA Saxitoxin"))
actionButton("qs_elisa_cortisol", label = tagList(icon("vial"), " ELISA Cortisol"))  
actionButton("qs_elisa_custom", label = tagList(icon("cog"), " ELISA Custom"))
```

**Action:**
1. Convert `h4()` and `p()` to `uiOutput("quickstart_header")`
2. Add `renderUI()` block in `server_common.R`
3. Update button labels via language observer using existing keys:
   - `qs_rba_stx` → reuse key `assay_rba` + "Saxitoxin" 
   - `qs_elisa_cortisol` → reuse key `assay_elisa` + "Cortisol"
   - `qs_elisa_custom` → reuse key `assay_elisa` + new key `custom`

### 1.2 Assay Type Selection
**File:** `app.R` lines ~115-135

Current:
```r
h5("Select Assay Type")
selectInput("assay_type", "Type of assay:", 
           choices = c("Receptor Binding Assay (RBA)" = "rba",
                      "ELISA (Enzyme-Linked Immunosorbent Assay)" = "elisa"))
```

**Action:**
1. Convert heading to `uiOutput("assay_type_header")`
2. Update `selectInput` choices in language observer:
   ```r
   updateSelectInput(session, "assay_type", 
                     label = tr("assay_type_label", lang),
                     choices = setNames(c("rba", "elisa"), 
                                       c(tr("assay_rba", lang), tr("assay_elisa", lang))))
   ```
3. Reuse existing keys: `assay_type_label`, `assay_rba`, `assay_elisa`

### 1.3 Standard Concentrations
**File:** `app.R` lines ~180-200

Current:
```r
p(tags$b("Standard Concentrations"))
p("Specify the number of standards, then enter each concentration.")
selectInput("num_standards", "Number of standards:", choices = 0:12, selected = 8)
```

**Action:**
1. Convert to `uiOutput("standards_section")`
2. Reuse existing keys: `std_concentrations`, `std_concentrations_desc`, `num_standards`

---

## Phase 2: Tab 2 - Plate Layout (Priority: High)

### 2.1 Matrix Section Headers  
**File:** `app.R` lines ~250-350

Current hardcoded headers:
- "1. Sample Type"
- "2. Sample ID" 
- "3. Dilution Fraction"
- "4. Replicate Groups"

**Action:**
1. Convert each `h5()` to `uiOutput("matrix_X_header")`
2. Reuse existing keys: `type_matrix`, `id_matrix`, `dilution_matrix_label`, `replicate_matrix`

### 2.2 ELISA Control Wells Help
**File:** `app.R` lines ~265-275

Current:
```r
div(style = "background-color: #FFF9E6; ...",
    tags$b("ELISA Controls: "),
    "Blank | NSB | B0 | TotalActivity (col 1)")
```

**Action:**
1. Convert to `uiOutput("elisa_controls_help")`
2. Reuse existing keys: `elisa_controls_title`, `blank_desc`, `nsb_desc`, `b0_desc`

### 2.3 Preset Layout Controls
**File:** `app.R` lines ~235-250

Current:
```r
selectInput("preset_layout", "Load Preset Layout:", choices = c(...))
actionButton("layout_save", label = tagList(icon("save"), "Save Layout"))
```

**Action:**
1. Update via language observer
2. Reuse existing keys: `layout_import_title`, `layout_save_btn`, `layout_load_btn`

---

## Phase 3: Tab 3 - Upload & Preview (Priority: Medium)

### 3.1 Upload Controls
**File:** `app.R` lines ~400-430

Current:
```r
fileInput("upload_counts", "Upload Bioassay Results", accept = c(...))
radioButtons("import_method", "Import method:", 
            choices = c("Classic Import" = "classic", "Visual Plate Selector" = "visual"))
```

**Action:**
1. Update via language observer
2. Reuse existing keys: `upload_label`, `upload_or_visual`, `import_classic`, `import_visual`

### 3.2 Visual Selector Instructions
**File:** `app.R` lines ~445-465

Current:
```r
h5("Visual Plate Selector")
p("After uploading a file, a preview will appear below...")
```

**Action:**
1. Convert to `uiOutput("visual_selector_header")`
2. Reuse existing keys: `visual_selector_title`, `visual_instructions`

---

## Phase 4: Tab 4 - Analysis Settings (Priority: Medium)

### 4.1 Main Settings Panel
**File:** `app.R` lines ~500-540

Current:
```r
h4("Analysis Settings")
checkboxGroupInput("regression_weight", "DRC regression weighting:", ...)
numericInput("quant_range_min", "Lower %B/B0 bound:", ...)
```

**Action:**
1. Convert header to `uiOutput("analysis_settings_header")`
2. Update inputs via language observer
3. Reuse existing keys: `analysis_settings_title`, `regression_weight_label`, `quant_range_min_label`

### 4.2 Advanced Options Collapsible
**File:** `app.R` lines ~545-590

Current:
```r
h4("Advanced Options — weighting, CI, outliers, and QC thresholds")
radioButtons("ci_method", "Confidence interval method:", ...)
checkboxInput("enable_outlier_detection", "Enable outlier detection", ...)
```

**Action:**
1. Convert to `uiOutput("advanced_options_panel")`
2. Reuse existing keys: `ci_method_label`, `outlier_detection_label`, `cv_limit_label`

---

## Phase 5: Tab 5 - Generate Report (Priority: High)

### 5.1 Report Format Selection
**File:** `app.R` lines ~620-650

Current:
```r
h4("Report Output")
checkboxGroupInput("export_formats", "Report formats:", 
                   choices = c("HTML" = "html", "Word (DOCX)" = "docx", "PDF" = "pdf"))
selectInput("report_language", "Report language:", ...)
```

**Action:**
1. Convert header to `uiOutput("report_output_header")`
2. Update inputs via language observer  
3. Reuse existing keys: `report_formats`, `report_language`

### 5.2 Generate Button & Notes
**File:** `app.R` lines ~655-675

Current:
```r
actionButton("convert", label = tagList(icon("file-arrow-down"), "Generate Report"))
textAreaInput("notes", "Notes (optional) - will appear in the report:", ...)
```

**Action:**
1. Update via language observer
2. Reuse existing keys: `generate_report`, `notes_label`, `notes_placeholder`

---

## Phase 6: Modal Dialogs (Priority: Low)

### 6.1 Welcome Modal  
**File:** `server_common.R` lines ~100-120

Current:
```r
showModal(modalDialog(
  title = tagList(icon("flask"), " Competitive Binding Assay Analysis Suite"),
  div("Analyze RBA and ELISA plate reader data..."),
  footer = modalButton("Get Started")
))
```

**Action:**
1. Extract all text to `tr()` calls
2. Add new keys if needed: `welcome_title`, `welcome_body`, `get_started_button`

### 6.2 Layout Import/Save Modals
**File:** `server_layout.R` various lines

**Action:**
1. Update modal titles and content via `tr()` calls
2. Reuse existing keys where available

---

## Implementation Strategy

### Execution Order
1. **Phase 1 (Tab 1)** - Most visible, highest user impact
2. **Phase 5 (Tab 5)** - Report generation is core workflow  
3. **Phase 2 (Tab 2)** - Complex plate layout needs careful testing
4. **Phase 4 (Tab 4)** - Analysis settings, medium complexity
5. **Phase 3 (Tab 3)** - Upload flow, medium complexity  
6. **Phase 6 (Modals)** - Lower priority, occasional use

### File Modification Pattern
For each phase:

1. **Identify reusable keys** in `i18n.R` (maximize reuse of existing Spanish translations)
2. **Convert static text** from hardcoded to `uiOutput()` + `renderUI()` pattern
3. **Update language observer** in `server_common.R` to handle input widgets
4. **Test bilingual switching** for each modified section
5. **Verify no regressions** in English mode

### Language Observer Updates
Extend existing observer at `server_common.R:247-268`:

```r
observeEvent(input$app_language, {
  lang <- input$app_language
  
  # Tab 1 updates
  updateSelectInput(session, "assay_type", 
                    label = tr("assay_type_label", lang),
                    choices = setNames(c("rba", "elisa"), 
                                      c(tr("assay_rba", lang), tr("assay_elisa", lang))))
  
  # Tab 2 updates  
  updateSelectInput(session, "preset_layout", 
                    label = tr("preset_layout_label", lang), ...)
  
  # Continue for all tabs...
})
```

### New renderUI Blocks
Add to appropriate server files:

```r
output$quickstart_header <- renderUI({
  lang <- input$app_language %||% "en"
  div(
    h4(tr("quickstart_title", lang)),
    p(tr("quickstart_desc", lang))
  )
})

output$assay_type_header <- renderUI({
  lang <- input$app_language %||% "en"  
  h5(tr("select_assay_type", lang))
})
# Continue for all sections...
```

---

## Testing & Validation

### Manual Testing Checklist
For each completed phase:

- [ ] Switch language from English to Spanish - all text updates immediately
- [ ] Switch back to English - all text reverts correctly  
- [ ] No layout shifts or UI breaks during language changes
- [ ] All button/input functionality preserved
- [ ] No console errors during language switching
- [ ] Proper text encoding (accented characters display correctly)

### Regression Testing  
- [ ] Existing English UI functionality unchanged
- [ ] Tour still works (separate issue, but don't break it further)
- [ ] Report generation in both languages  
- [ ] All existing automated tests pass: `testthat::test_dir("tests/testthat")`

---

## Key Reuse Strategy

**Maximize existing translation reuse:**
- `i18n.R` contains ~239 orphaned keys with Spanish translations already done
- Pattern match text content to existing keys before creating new ones
- For missing keys, add to BOTH `en` and `es` blocks in `i18n.R`

**Common reusable keys identified:**
- Navigation: `tour_next`, `tour_prev`, `tour_skip`, `tour_done`
- Actions: `generate_report`, `upload_label`, `clear_file`, `reset_default`  
- Labels: `assay_type`, `analysis_settings_title`, `report_formats`
- Descriptions: `std_concentrations_desc`, `qc_params`, `notes_label`

---

## Acceptance Criteria

### Functional Requirements
- [x] All 5 wizard tabs fully translated
- [x] Language toggle works instantly (no page refresh)  
- [x] Modal dialogs translated
- [x] Input choice labels translated (except proper names)
- [x] Help text and descriptions translated

### Quality Requirements  
- [x] No English text visible in Spanish mode
- [x] No Spanish text leaking into English mode
- [x] Consistent terminology across all sections
- [x] Proper Spanish grammar and accents
- [x] No UI layout issues from longer Spanish text

### Technical Requirements
- [x] Maximum reuse of existing `i18n.R` translations  
- [x] Clean separation: static text via `renderUI()`, inputs via `updateXxxInput()`
- [x] Language observer handles all dynamic updates
- [x] No hardcoded Spanish text in R files
- [x] All existing functionality preserved

---

## Out of Scope

**Explicitly NOT included:**
- Report content translation (Rmd templates) - separate future work
- Data validation messages - keep in English for now  
- Console/log messages - development artifacts, English OK
- Tour content revision - separate from this translation work
- Adding new languages beyond Spanish
- Right-to-left language support

**Future work:**
- Guided tour cleanup (tracked in `followup_plan_v4_tour_silent.md`)
- Rmd report template translation  
- Server-side validation message translation
- CSV export header translation

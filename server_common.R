# ==============================================================================
# server_common.R
# Common server logic for the Competitive Bioassay Analysis Suite.
#
# Contains: auto-save/restore system, welcome modal, wizard tab navigation
# (with ELISA validation), header renderUIs, language observer, and guided tour.
#
# Called from the main server function as:
#   server_common(input, output, session, shared)
#
# All matrix state is accessed via the `shared` list (e.g. shared$matrix_type(),
# shared$matrix_measresults(), shared$rv$is_multiwavelength).
# ==============================================================================

server_common <- function(input, output, session, shared) {

  # --------------------------------------------------------------------------
  # Session State Auto-Save (every 60 seconds)
  # --------------------------------------------------------------------------

  autosave_path <- file.path(tempdir(), paste0("bioassay_autosave_", session$token, ".rds"))

  # Check for a previous auto-save and offer to restore
  observe({
    # Run once on startup — look for any recent autosave (< 2 hours old)
    candidates <- list.files(tempdir(), pattern = "^bioassay_autosave_.*\\.rds$", full.names = TRUE)
    if (length(candidates) > 0) {
      # Find the most recent file that is less than 2 hours old
      info <- file.info(candidates)
      recent <- candidates[difftime(Sys.time(), info$mtime, units = "hours") < 2]
      if (length(recent) > 0) {
        newest <- recent[which.max(file.info(recent)$mtime)]
        saved <- tryCatch(readRDS(newest), error = function(e) NULL)
        if (!is.null(saved) && !is.null(saved$timestamp)) {
          showModal(modalDialog(
            title = "Restore Previous Session?",
            paste0("An auto-saved session from ",
                   format(saved$timestamp, "%Y-%m-%d %H:%M:%S"),
                   " was found. Would you like to restore the plate layout?"),
            footer = tagList(
              actionButton("restore_autosave", "Restore", class = "btn-primary"),
              modalButton("Start Fresh")
            ),
            easyClose = TRUE
          ))
          session$userData$pending_restore <- saved
        }
      }
    }
  }) |> bindEvent(session$clientData$url_protocol, once = TRUE)

  observeEvent(input$restore_autosave, {
    saved <- session$userData$pending_restore
    if (!is.null(saved)) {
      if (!is.null(saved$matrix_type)) shared$matrix_type(saved$matrix_type)
      if (!is.null(saved$matrix_id)) shared$matrix_id(saved$matrix_id)
      if (!is.null(saved$matrix_dilution)) shared$matrix_dilution(saved$matrix_dilution)
      if (!is.null(saved$matrix_replicate)) shared$matrix_replicate(saved$matrix_replicate)
      if (!is.null(saved$std_concentrations)) {
        for (i in seq_along(saved$std_concentrations)) {
          updateTextInput(session, paste0("std", i), value = saved$std_concentrations[i])
        }
      }
      showNotification("Session restored successfully.", type = "message", duration = 3)
    }
    session$userData$pending_restore <- NULL
    removeModal()
  })

  autoSaveTimer <- reactiveTimer(60000)
  observe({
    autoSaveTimer()
    tryCatch({
      # Collect standard concentrations
      n_std <- as.integer(isolate(input$num_standards) %||% 0)
      std_vals <- if (n_std > 0) {
        sapply(seq_len(n_std), function(i) isolate(input[[paste0("std", i)]]) %||% "")
      } else character(0)

      state <- list(
        matrix_type = isolate(shared$matrix_type()),
        matrix_id = isolate(shared$matrix_id()),
        matrix_dilution = isolate(shared$matrix_dilution()),
        matrix_replicate = isolate(shared$matrix_replicate()),
        assay_type = isolate(input$assay_type),
        num_standards = isolate(input$num_standards),
        std_concentrations = std_vals,
        elisa_analyte = isolate(input$elisa_analyte),
        toxin_class = isolate(input$toxin_class),
        timestamp = Sys.time()
      )
      saveRDS(state, autosave_path)
    }, error = function(e) {
      # Silent failure for auto-save — don't disrupt the user
    })
  })

  # --------------------------------------------------------------------------
  # Welcome Modal (shown once on first launch)
  # --------------------------------------------------------------------------

  observe({
    showModal(modalDialog(
      title = tagList(icon("flask"), " Competitive Binding Assay Analysis Suite"),
      size = "l",
      easyClose = TRUE,
      div(
        style = "font-size: 14px;",
        p("Analyze RBA and ELISA plate reader data with 4-parameter logistic curve fitting."),
        hr(),
        tags$b("Quick Start:"),
        tags$ol(
          tags$li("Choose an assay type above, or click a Quick Start button"),
          tags$li("Upload your plate reader file (.xlsx, .csv, .txt)"),
          tags$li("Click Generate Report")
        ),
        hr(),
        p(style = "color: #666; font-size: 12px;",
          "Example datasets are included in the ", tags$code("examples/"), " folder. ",
          "For questions: kr.moeller@iaea.org")
      ),
      footer = modalButton("Get Started")
    ))
  }) |> bindEvent(session$clientData$url_protocol, once = TRUE)

  # --------------------------------------------------------------------------
  # Tab Navigation
  # --------------------------------------------------------------------------

  # Helper: validate prerequisites before advancing to Analysis Settings
  validate_before_analysis <- function() {
    if (input$assay_type == "elisa") {
      type_mat <- shared$matrix_type()
      if (!is.null(type_mat)) {
        well_types <- as.character(unlist(type_mat))
        required_controls <- c("Blank", "NSB", "B0")
        missing_controls <- required_controls[!required_controls %in% well_types]
        if (length(missing_controls) > 0) {
          showModal(modalDialog(
            title = "Missing ELISA Control Wells",
            paste0("Your plate layout is missing required control wells: ",
                   paste(missing_controls, collapse = ", "), ". ",
                   "Please go back to Plate Layout and assign these ",
                   "well types in the Type matrix."),
            footer = modalButton("OK"), easyClose = TRUE
          ))
          return(FALSE)
        }
      }
    }
    plate <- shared$matrix_measresults()
    if (is.null(plate) || !any(!is.na(as.numeric(unlist(plate))))) {
      showModal(modalDialog(
        title = "No Plate Data",
        "Please upload plate reader data before generating a report.",
        footer = modalButton("OK"), easyClose = TRUE
      ))
      return(FALSE)
    }
    TRUE
  }

  # Bottom navigation buttons
  observeEvent(input$next_to_layout, {
    updateTabsetPanel(session, "wizard_tabs", selected = "tab_layout")
  })
  observeEvent(input$back_to_config, {
    updateTabsetPanel(session, "wizard_tabs", selected = "tab_config")
  })
  observeEvent(input$next_to_upload, {
    updateTabsetPanel(session, "wizard_tabs", selected = "tab_upload")
  })
  observeEvent(input$back_to_layout, {
    updateTabsetPanel(session, "wizard_tabs", selected = "tab_layout")
  })
  observeEvent(input$next_to_analysis, {
    if (validate_before_analysis()) {
      updateTabsetPanel(session, "wizard_tabs", selected = "tab_analysis")
    }
  })
  observeEvent(input$back_to_upload, {
    updateTabsetPanel(session, "wizard_tabs", selected = "tab_upload")
  })
  observeEvent(input$next_to_report, {
    updateTabsetPanel(session, "wizard_tabs", selected = "tab_report")
  })
  observeEvent(input$back_to_analysis, {
    updateTabsetPanel(session, "wizard_tabs", selected = "tab_analysis")
  })

  # Top navigation buttons (duplicate _top IDs)
  observeEvent(input$next_to_layout_top, {
    updateTabsetPanel(session, "wizard_tabs", selected = "tab_layout")
  })
  observeEvent(input$back_to_config_top, {
    updateTabsetPanel(session, "wizard_tabs", selected = "tab_config")
  })
  observeEvent(input$next_to_upload_top, {
    updateTabsetPanel(session, "wizard_tabs", selected = "tab_upload")
  })
  observeEvent(input$back_to_layout_top, {
    updateTabsetPanel(session, "wizard_tabs", selected = "tab_layout")
  })
  observeEvent(input$next_to_analysis_top, {
    if (validate_before_analysis()) {
      updateTabsetPanel(session, "wizard_tabs", selected = "tab_analysis")
    }
  })
  observeEvent(input$back_to_upload_top, {
    updateTabsetPanel(session, "wizard_tabs", selected = "tab_upload")
  })
  observeEvent(input$next_to_report_top, {
    updateTabsetPanel(session, "wizard_tabs", selected = "tab_report")
  })
  observeEvent(input$back_to_analysis_top, {
    updateTabsetPanel(session, "wizard_tabs", selected = "tab_analysis")
  })

  # --------------------------------------------------------------------------
  # Language-reactive Header UIs
  # --------------------------------------------------------------------------

  output$app_title_ui <- renderUI({
    lang <- input$app_language %||% "en"
    titlePanel(tr("app_title", lang))
  })

  output$step0_header <- renderUI({
    lang <- input$app_language %||% "en"
    h4(tr("step0_title", lang))
  })

  output$step1_header <- renderUI({
    lang <- input$app_language %||% "en"
    h4(tr("step1_title", lang))
  })

  output$step2_header <- renderUI({
    lang <- input$app_language %||% "en"
    h4(tr("step2_title", lang))
  })

  # ---- Tab 1: Configuration static headings ----

  output$quickstart_heading_ui <- renderUI({
    lang <- input$app_language %||% "en"
    tagList(
      h4(style = "margin-top: 0;", tr("quickstart_title", lang)),
      p(tr("quickstart_desc", lang))
    )
  })

  output$quickstart_manual_note_ui <- renderUI({
    lang <- input$app_language %||% "en"
    tags$small(style = "color: #666;", tr("quickstart_or_manual", lang))
  })

  output$select_assay_type_heading_ui <- renderUI({
    lang <- input$app_language %||% "en"
    h5(tags$b(tr("select_assay_type", lang)))
  })

  output$std_concentrations_heading_ui <- renderUI({
    lang <- input$app_language %||% "en"
    tagList(
      p(tags$b(tr("std_concentrations", lang))),
      p(tr("std_concentrations_desc", lang))
    )
  })

  # ---- Tab 5: Generate Report static headings & helpers ----

  output$report_output_heading_ui <- renderUI({
    lang <- input$app_language %||% "en"
    h4(tr("report_output_heading", lang))
  })

  output$report_formats_help_ui <- renderUI({
    lang <- input$app_language %||% "en"
    tags$small(class = "text-muted",
               style = "display: block; margin-top: 4px; line-height: 1.5;",
               tr("report_formats_help", lang),
               tags$br(),
               tr("report_formats_pdf_note", lang))
  })

  output$notes_feedback_heading_ui <- renderUI({
    lang <- input$app_language %||% "en"
    h4(tr("notes_feedback_heading", lang))
  })

  output$preflight_heading_ui <- renderUI({
    lang <- input$app_language %||% "en"
    h5(style = "margin-top: 0;", tr("preflight_heading", lang))
  })

  output$download_report_ui <- renderUI({
    lang <- input$app_language %||% "en"
    downloadButton("download_report", tr("download_last_report", lang),
                   class = "btn btn-success btn-lg",
                   style = "width: 100%;")
  })

  output$give_feedback_ui <- renderUI({
    lang <- input$app_language %||% "en"
    tags$a(href = "https://forms.office.com/e/q8eqJfp4QM",
           target = "_blank", class = "btn btn-info btn-block",
           icon("comment"), " ", tr("give_feedback", lang))
  })

  # ---- Tab 2: Plate Layout static headings & banners ----

  output$sample_type_heading_ui <- renderUI({
    lang <- input$app_language %||% "en"
    h5(tr("sample_type_label", lang))
  })

  output$sample_id_heading_ui <- renderUI({
    lang <- input$app_language %||% "en"
    h5(tr("id_matrix", lang))
  })

  output$replicate_heading_ui <- renderUI({
    lang <- input$app_language %||% "en"
    h5(tr("replicate_label", lang))
  })

  output$qc_params_heading_ui <- renderUI({
    lang <- input$app_language %||% "en"
    h5(tr("qc_params_label", lang))
  })

  output$tissue_weight_heading_ui <- renderUI({
    lang <- input$app_language %||% "en"
    h5(tr("tissue_weight_label", lang))
  })

  output$elisa_controls_banner_ui <- renderUI({
    lang <- input$app_language %||% "en"
    div(
      style = paste("background-color: #FFF9E6; padding: 8px; margin: 8px 0;",
                    "border-left: 4px solid #FFC107; font-size: 12px;"),
      tags$b(tr("elisa_controls_title", lang), " "),
      tr("elisa_controls_banner_body", lang)
    )
  })

  output$tissue_weight_banner_ui <- renderUI({
    lang <- input$app_language %||% "en"
    div(
      style = paste("background-color: #FFF3E0; padding: 8px; margin: 8px 0;",
                    "border-left: 4px solid #FF9800; font-size: 12px;"),
      tags$b(tr("tissue_banner_prefix", lang)),
      tr("tissue_banner_body", lang)
    )
  })

  output$set_all_extraction_label_ui <- renderUI({
    lang <- input$app_language %||% "en"
    tags$label(tr("set_all_extraction_label", lang), `for` = "uniform_extraction",
               style = "margin: 0; white-space: nowrap; font-size: 12px;")
  })

  output$scroll_right_hint_ui <- renderUI({
    lang <- input$app_language %||% "en"
    tags$small(style = "color: #888;", tr("scroll_right_hint", lang))
  })

  output$layout_import_file_ui <- renderUI({
    lang <- input$app_language %||% "en"
    fileInput("layout_import_file", tr("layout_import_label", lang),
              accept = c(".csv", ".xlsx", ".xls"),
              width = "100%")
  })

  # --------------------------------------------------------------------------
  # Language Observer — update all input labels on language change
  # --------------------------------------------------------------------------

  observeEvent(input$app_language, {
    lang <- input$app_language
    updateSelectInput(session, "report_language", selected = lang)
    updateActionButton(session, "start_tour", label = tr("start_tour", lang))
    updateActionButton(session, "convert",
                       label = tagList(icon("file-arrow-down"), " ",
                                       tr("generate_report", lang)))

    # Tab 1: Quick Start buttons (icon + translated label)
    updateActionButton(session, "qs_rba_stx",
                       label = tagList(icon("flask"), " ", tr("preset_rba_stx_btn", lang)))
    updateActionButton(session, "qs_elisa_cortisol",
                       label = tagList(icon("vial"), " ", tr("preset_elisa_cortisol_btn", lang)))
    updateActionButton(session, "qs_elisa_custom",
                       label = tagList(icon("cog"), " ", tr("preset_elisa_custom_btn", lang)))

    # Tab 1: Assay type / toxin / analyte / units / num_standards
    updateSelectInput(session, "assay_type",
                      label = tr("assay_type_label", lang),
                      choices = tr_choices(c("rba", "elisa"),
                                           c("assay_rba", "assay_elisa"), lang),
                      selected = input$assay_type %||% "rba")
    updateSelectInput(session, "toxin_class",
                      label = tr("toxin_standard", lang),
                      choices = setNames(
                        c("Saxitoxin", "Brevetoxin", "Ciguatoxin", "Custom"),
                        c("Saxitoxin", "Brevetoxin", "Ciguatoxin",
                          tr("custom_choice_label", lang))
                      ),
                      selected = input$toxin_class %||% "Saxitoxin")
    updateTextInput(session, "toxin_custom_name",
                    label = tr("custom_standard_name_label", lang),
                    placeholder = tr("custom_standard_name_placeholder", lang))
    updateSelectInput(session, "elisa_analyte",
                      label = tr("analyte_label", lang),
                      choices = setNames(
                        c("cortisol", "testosterone", "estradiol", "custom"),
                        c("Cortisol", "Testosterone", "Estradiol",
                          tr("custom_choice_label", lang))
                      ),
                      selected = input$elisa_analyte %||% "cortisol")
    updateTextInput(session, "elisa_custom_name",
                    label = tr("custom_name", lang),
                    placeholder = tr("custom_analyte_placeholder", lang))
    updateSelectInput(session, "elisa_units",
                      label = tr("units_label", lang),
                      selected = input$elisa_units %||% "pg/mL")
    updateSelectInput(session, "num_standards",
                      label = tr("num_standards", lang),
                      selected = input$num_standards %||% 8)

    # Tab 2: Plate Layout widgets
    updateSelectInput(session, "preset_layout",
                      label = tr("preset_layout_label", lang),
                      choices = tr_choices(
                        c("", "rba_stx_triplicate", "elisa_cortisol_cayman", "elisa_custom_blank"),
                        c("preset_select_placeholder",
                          "preset_rba_stx_tri",
                          "preset_elisa_cortisol_cayman",
                          "preset_elisa_custom_blank"), lang),
                      selected = input$preset_layout %||% "")
    updateActionButton(session, "layout_save",
                       label = tagList(icon("save"), " ", tr("save_layout_short", lang)))
    updateActionButton(session, "undo_layout",
                       label = tagList(icon("undo"), " ", tr("undo_btn", lang)))
    updateActionButton(session, "redo_layout",
                       label = tagList(icon("redo"), " ", tr("redo_btn", lang)))
    updateActionButton(session, "apply_uniform_dilution", label = tr("apply_btn", lang))
    updateActionButton(session, "apply_uniform_extraction", label = tr("apply_btn", lang))
    updateActionButton(session, "reset_type", label = tr("reset_btn", lang))
    updateActionButton(session, "reset_id", label = tr("reset_btn", lang))
    updateActionButton(session, "reset_dilution", label = tr("reset_btn", lang))
    updateActionButton(session, "reset_replicate", label = tr("reset_btn", lang))
    updateCheckboxInput(session, "advanced_dilution", label = tr("per_well_label", lang))

    updateRadioButtons(session, "import_method", label = tr("upload_or_visual", lang),
                      choices = setNames(c("classic", "visual"),
                                        c(tr("import_classic", lang), tr("import_visual", lang))),
                      selected = input$import_method %||% "classic",
                      inline = TRUE)
    updateCheckboxGroupInput(session, "export_formats",
                             label = tr("report_formats", lang),
                             choices = tr_choices(
                               c("html", "docx", "pdf"),
                               c("format_html", "format_docx", "format_pdf"), lang),
                             selected = input$export_formats %||% "html")
    updateSelectInput(session, "report_language", label = tr("report_language", lang))
    updateTextAreaInput(session, "notes", label = tr("notes_full_label", lang),
                       placeholder = tr("notes_report_placeholder", lang))
    updateCheckboxGroupInput(session, "regression_weight", label = tr("regression_weight_label", lang))
    updateNumericInput(session, "quant_range_min", label = tr("quant_range_min_label", lang))
    updateNumericInput(session, "quant_range_max", label = tr("quant_range_max_label", lang))
    updateRadioButtons(session, "ci_method", label = tr("ci_method_label", lang))
    updateCheckboxInput(session, "enable_outlier_detection", label = tr("outlier_detection_label", lang))
    updateNumericInput(session, "outlier_min_n", label = tr("outlier_min_n_label", lang))
    updateRadioButtons(session, "normality_assumption", label = tr("normality_assumption_label", lang),
                       choices = c(setNames("assume", tr("normality_assume", lang)),
                                   setNames("test_shapiro", tr("normality_test_shapiro", lang))),
                       selected = input$normality_assumption %||% "assume")
  })

  # --------------------------------------------------------------------------
  # Guided Tour (language-reactive, spans the 5 wizard tabs)
  # --------------------------------------------------------------------------
  # The tour walks through every wizard tab and highlights the relevant UI
  # section on each tab. Because non-active tabs are display:none in
  # tabsetPanel, the onbeforechange JS callback asks R to activate the
  # matching tab before each highlight. R updates the tab reactively, then
  # intro.js resumes.

  observeEvent(input$start_tour, {
    lang <- input$app_language %||% "en"
    assay <- input$assay_type %||% "rba"

    # ------------------------------------------------------------------
    # [TOUR DIAGNOSTIC v4 §1.1/1.3] Temporary instrumentation. Remove
    # after the tour-silence bug is diagnosed and fixed (plan §3.3).
    # ------------------------------------------------------------------
    shinyjs::runjs(
      "window.addEventListener('error', function(e) {
         console.error('[TOUR DIAGNOSTIC] window.error:',
                       e.message, 'at', e.filename + ':' + e.lineno);
       }, {once: false});"
    )
    shinyjs::runjs(sprintf(
      "console.group('[TOUR DIAGNOSTIC]');
       console.log('tour trigger fired at', new Date().toISOString());
       console.log('assay_type:', %s);
       console.log('app_language:', %s);
       console.log('introJs available:', typeof introJs);
       console.log('rintrojs global:', typeof rintrojs);
       console.log('jQuery available:', typeof jQuery);
       console.groupEnd();",
      jsonlite::toJSON(assay, auto_unbox = TRUE),
      jsonlite::toJSON(lang,  auto_unbox = TRUE)
    ))
    # ------------------------------------------------------------------

    # Each step carries an optional `position` hint for intro.js. "auto"
    # is the default (let intro.js decide); override for wide / edge-case
    # targets where the auto pick lands in an awkward spot.

    # --- Tab 1: Configuration --------------------------------------------
    tour_steps <- data.frame(
      element = c("#language_toggle_section",
                  "#quickstart_section",
                  "#step0_section"),
      intro = c(
        tr("tour_language_toggle", lang),
        tr("tour_quickstart", lang),
        tr("tour_config", lang)
      ),
      tab = c("tab_config", "tab_config", "tab_config"),
      position = c("auto", "auto", "auto"),
      stringsAsFactors = FALSE
    )

    # --- Tab 2: Plate Layout ---------------------------------------------
    # Follow the numeric labels shown on the matrix headers in app.R:
    #   1. Sample Type -> 2. Sample ID -> 3. Dilution Fraction -> 4. Replicate
    # (matches the numbering a user reads on the UI rather than the 2x2
    # visual grid order).
    layout_steps <- data.frame(
      element = c("#preset_layout_section",
                  "#matrix_type_section",
                  "#matrix_id_section",
                  "#matrix_dilution_section",
                  "#matrix_replicate_section"),
      intro = c(
        tr("tour_preset_layout", lang),
        tr("tour_matrix_type", lang),
        tr("tour_matrix_id", lang),
        tr("tour_matrix_dilution", lang),
        tr("tour_matrix_replicate", lang)
      ),
      tab = "tab_layout",
      # preset_layout_section spans the full page width; center the tooltip
      # below the bar so it points cleanly at the preset controls.
      position = c("bottom-middle-aligned", "auto", "auto", "auto", "auto"),
      stringsAsFactors = FALSE
    )
    # Assay-specific layout extras
    if (assay == "rba") {
      layout_steps <- rbind(layout_steps, data.frame(
        element = "#qc_section",
        intro = tr("tour_qc_rba", lang),
        tab = "tab_layout",
        position = "auto",
        stringsAsFactors = FALSE
      ))
    } else if (assay == "elisa") {
      layout_steps <- rbind(layout_steps, data.frame(
        element = "#tissue_weight_section",
        intro = tr("tour_tissue_weights", lang),
        tab = "tab_layout",
        position = "auto",
        stringsAsFactors = FALSE
      ))
    }
    tour_steps <- rbind(tour_steps, layout_steps)

    # --- Tab 3: Upload & Preview -----------------------------------------
    tour_steps <- rbind(tour_steps, data.frame(
      element = c("#upload_section", "#heatmap_preview_section"),
      intro = c(
        tr("tour_upload", lang),
        tr("tour_heatmap_preview", lang)
      ),
      tab = "tab_upload",
      position = c("auto", "auto"),
      stringsAsFactors = FALSE
    ))

    # --- Tab 4: Analysis Settings ----------------------------------------
    tour_steps <- rbind(tour_steps, data.frame(
      element = "#analysis_settings_section",
      intro = tr("tour_analysis", lang),
      tab = "tab_analysis",
      position = "auto",
      stringsAsFactors = FALSE
    ))

    # --- Tab 5: Generate Report ------------------------------------------
    tour_steps <- rbind(tour_steps, data.frame(
      element = c("#preflight_section", "#convert_section", "#notes_feedback_section"),
      intro = c(
        tr("tour_preflight", lang),
        tr("tour_convert", lang),
        tr("tour_notes", lang)
      ),
      tab = "tab_report",
      position = c("auto", "auto", "auto"),
      stringsAsFactors = FALSE
    ))

    # Defensive selector filter (plan v4 §4, B.5): drop any step whose
    # element id is NA/empty or not a valid '#name' selector, with a
    # warning on the R console so regressions are visible.
    valid_mask <- !is.na(tour_steps$element) & nzchar(tour_steps$element) &
                  grepl("^#[A-Za-z][-_A-Za-z0-9]*$", tour_steps$element)
    if (!all(valid_mask)) {
      dropped <- tour_steps$element[!valid_mask]
      warning("Dropping invalid tour selectors: ",
              paste(dropped, collapse = ", "))
      tour_steps <- tour_steps[valid_mask, , drop = FALSE]
    }

    # Start from the Configuration tab so the first element is visible.
    updateTabsetPanel(session, "wizard_tabs", selected = "tab_config")

    # ------------------------------------------------------------------
    # [TOUR DIAGNOSTIC v4 §1.2] Dump the resolved step list and inspect
    # each target's DOM presence + visibility. Remove after diagnosis.
    # ------------------------------------------------------------------
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
      })();",
      jsonlite::toJSON(tour_steps[, c("element", "intro")],
                       auto_unbox = FALSE)
    ))
    # ------------------------------------------------------------------

    # intro.js cannot highlight elements inside an inactive .tab-pane.
    # rintrojs::readCallback("switchTabs") returns the body of a callback
    # that calls jQuery.fn.tab('show') synchronously on the nav link
    # matching the target's [data-value] pane, so the correct pane is
    # .active before intro.js reads geometry. Replaces the earlier
    # inline onbeforechange string which triggered a ReferenceError
    # inside intro.js's eval path.
    introjs(session,
            options = list(
              steps = tour_steps[, c("element", "intro", "position")],
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
  })
}

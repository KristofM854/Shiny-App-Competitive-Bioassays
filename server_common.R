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
    # Validate ELISA prerequisites before allowing analysis settings tab
    if (input$assay_type == "elisa") {
      type_mat <- shared$matrix_type()
      if (!is.null(type_mat)) {
        well_types <- as.character(unlist(type_mat))
        required_controls <- c("Blank", "NSB", "B0")
        missing_controls <- required_controls[!required_controls %in% well_types]

        if (length(missing_controls) > 0) {
          showModal(modalDialog(
            title = "Missing ELISA Control Wells",
            paste0(
              "Your plate layout is missing required control wells: ",
              paste(missing_controls, collapse = ", "), ". ",
              "Please go back to Plate Layout and assign these ",
              "well types in the Type matrix."
            ),
            footer = modalButton("OK"),
            easyClose = TRUE
          ))
          return()
        }
      }
    }

    # Check that plate data has been uploaded
    plate <- shared$matrix_measresults()
    if (is.null(plate) || !any(!is.na(as.numeric(unlist(plate))))) {
      showModal(modalDialog(
        title = "No Plate Data",
        "Please upload plate reader data before generating a report.",
        footer = modalButton("OK"),
        easyClose = TRUE
      ))
      return()
    }

    updateTabsetPanel(session, "wizard_tabs", selected = "tab_analysis")
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

  # --------------------------------------------------------------------------
  # Language Observer — update all input labels on language change
  # --------------------------------------------------------------------------

  observeEvent(input$app_language, {
    lang <- input$app_language
    updateSelectInput(session, "report_language", selected = lang)
    updateActionButton(session, "start_tour", label = tr("start_tour", lang))
    updateActionButton(session, "convert", label = tr("generate_report", lang))
    updateRadioButtons(session, "import_method", label = tr("upload_or_visual", lang),
                      choices = setNames(c("classic", "visual"),
                                        c(tr("import_classic", lang), tr("import_visual", lang))))
    updateCheckboxGroupInput(session, "export_formats", label = tr("report_formats", lang))
    updateSelectInput(session, "report_language", label = tr("report_language", lang))
    updateTextAreaInput(session, "notes", label = tr("notes_label", lang),
                       placeholder = tr("notes_placeholder", lang))
    updateCheckboxGroupInput(session, "regression_weight", label = tr("regression_weight_label", lang))
    updateNumericInput(session, "quant_range_min", label = tr("quant_range_min_label", lang))
    updateNumericInput(session, "quant_range_max", label = tr("quant_range_max_label", lang))
    updateRadioButtons(session, "ci_method", label = tr("ci_method_label", lang))
    updateCheckboxInput(session, "enable_outlier_detection", label = tr("outlier_detection_label", lang))
    updateNumericInput(session, "outlier_min_n", label = tr("outlier_min_n_label", lang))
    updateRadioButtons(session, "normality_assumption", label = tr("normality_assumption_label", lang),
                       choices = c(setNames("assume", tr("normality_assume", lang)),
                                   setNames("test_shapiro", tr("normality_test_shapiro", lang))))
  })

  # --------------------------------------------------------------------------
  # Guided Tour (language-reactive)
  # --------------------------------------------------------------------------

  observeEvent(input$start_tour, {
    lang <- input$app_language %||% "en"
    assay <- input$assay_type %||% "rba"

    # Base tour steps (always shown)
    tour_steps <- data.frame(
      element = c("#step0_section", "#matrix_type_section", "#matrix_id_section",
                  "#matrix_dilution_section", "#matrix_replicate_section"),
      intro = c(
        tr("tour_step0", lang),
        tr("tour_step1_type", lang),
        tr("tour_step1_id", lang),
        tr("tour_step1_dilution", lang),
        tr("tour_step1_replicate", lang)
      ),
      stringsAsFactors = FALSE
    )

    # ELISA-only: tissue weight section
    if (assay == "elisa") {
      tour_steps <- rbind(tour_steps, data.frame(
        element = "#tissue_weight_section",
        intro = tr("tour_step1_tissue", lang),
        stringsAsFactors = FALSE
      ))
    }

    # Remaining common steps
    tour_steps <- rbind(tour_steps, data.frame(
      element = c("#upload_section", "#notes_feedback_section",
                  "#analysis_settings_section",
                  "#language_toggle_section", "#convert_section"),
      intro = c(
        tr("tour_step2_upload", lang),
        tr("tour_step2_notes", lang),
        tr("tour_step2_analysis", lang),
        tr("tour_language", lang),
        tr("tour_step3", lang)
      ),
      stringsAsFactors = FALSE
    ))

    introjs(session, options = list(
      steps = tour_steps,
      nextLabel = tr("tour_next", lang),
      prevLabel = tr("tour_prev", lang),
      skipLabel = tr("tour_skip", lang),
      doneLabel = tr("tour_done", lang),
      showProgress = TRUE
    ))
  })
}

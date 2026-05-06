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
  #
  # M2: autosaves live in a per-user directory under tools::R_user_dir so
  # user A can never be offered user B's layout on a shared machine. The
  # legacy behaviour (write into tempdir()) meant a shared Rstudio Server
  # or SLURM allocation would leak sessions across OS users because the
  # tempdir() scan was system-wide. The scan now restricts itself to the
  # current user's data dir.

  autosave_dir <- tryCatch(
    tools::R_user_dir("bioassay-analysis", which = "data"),
    error = function(e) tempdir()
  )
  dir.create(autosave_dir, showWarnings = FALSE, recursive = TRUE)
  autosave_path <- file.path(autosave_dir,
                             paste0("autosave_", session$token, ".rds"))

  # Startup cleanup: remove autosaves older than 7 days so the user-scoped
  # directory does not grow unbounded across sessions.
  tryCatch({
    old <- list.files(autosave_dir, pattern = "^autosave_.*\\.rds$",
                      full.names = TRUE)
    if (length(old) > 0) {
      ages <- difftime(Sys.time(), file.info(old)$mtime, units = "days")
      expired <- old[!is.na(ages) & ages > 7]
      if (length(expired) > 0) unlink(expired)
    }
  }, error = function(e) invisible(NULL))

  # Check for a previous auto-save and offer to restore. Scan only the
  # user-scoped directory (was tempdir() before M2).
  observe({
    candidates <- list.files(autosave_dir, pattern = "^autosave_.*\\.rds$",
                             full.names = TRUE)
    if (length(candidates) > 0) {
      info <- file.info(candidates)
      recent <- candidates[difftime(Sys.time(), info$mtime, units = "hours") < 2]
      if (length(recent) > 0) {
        newest <- recent[which.max(file.info(recent)$mtime)]
        saved <- tryCatch(readRDS(newest), error = function(e) NULL)
        if (!is.null(saved) && !is.null(saved$timestamp)) {
          lang <- isolate(input$app_language %||% "en")
          showModal(modalDialog(
            title = tr("restore_title", lang),
            tr("restore_body", lang, format(saved$timestamp, "%Y-%m-%d %H:%M:%S")),
            footer = tagList(
              actionButton("restore_autosave", tr("restore_btn", lang), class = "btn-primary"),
              modalButton(tr("start_fresh_btn", lang))
            ),
            easyClose = TRUE
          ))
          # Remember which file was offered so we can delete it on accept or
          # on Start Fresh — prevents it being re-offered next launch.
          session$userData$pending_restore <- saved
          session$userData$pending_restore_path <- newest
        }
      }
    }
  }) |> bindEvent(session$clientData$url_protocol, once = TRUE)

  # Helper: delete the offered autosave file (used by both accept + dismiss).
  drop_pending_restore <- function() {
    p <- session$userData$pending_restore_path
    if (!is.null(p) && file.exists(p)) {
      tryCatch(unlink(p), error = function(e) invisible(NULL))
    }
    session$userData$pending_restore <- NULL
    session$userData$pending_restore_path <- NULL
  }

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
    drop_pending_restore()
    removeModal()
  })

  # Dismissal via Start Fresh also drops the file so the next launch does
  # not re-offer the same restore.
  observeEvent(session$input$.shinymodal_dismissed, {
    drop_pending_restore()
  }, ignoreNULL = TRUE, ignoreInit = TRUE)

  autoSaveTimer <- reactiveTimer(60000)
  observe({
    autoSaveTimer()
    tryCatch({
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
    lang <- isolate(input$app_language %||% "en")
    showModal(modalDialog(
      title = tagList(icon("flask"), " ", tr("welcome_title", lang)),
      size = "l",
      easyClose = TRUE,
      div(
        style = "font-size: 14px;",
        p(tr("welcome_intro", lang)),
        hr(),
        tags$b(tr("welcome_qs_label", lang)),
        tags$ol(
          tags$li(tr("welcome_step1", lang)),
          tags$li(tr("welcome_step2", lang)),
          tags$li(tr("welcome_step3", lang))
        ),
        hr(),
        p(style = "color: #666; font-size: 12px;",
          tr("welcome_examples_note", lang, "examples/"))
      ),
      footer = modalButton(tr("get_started_btn", lang))
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
          lang <- isolate(input$app_language %||% "en")
          showModal(modalDialog(
            title = tr("missing_elisa_title", lang),
            tr("missing_elisa_body", lang, paste(missing_controls, collapse = ", ")),
            footer = modalButton(tr("modal_ok_btn", lang)), easyClose = TRUE
          ))
          return(FALSE)
        }
      }
    }
    plate <- shared$matrix_measresults()
    if (is.null(plate) || !any(!is.na(as.numeric(unlist(plate))))) {
      lang <- isolate(input$app_language %||% "en")
      showModal(modalDialog(
        title = tr("no_plate_title", lang),
        tr("no_plate_body", lang),
        footer = modalButton(tr("modal_ok_btn", lang)), easyClose = TRUE
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

  # GUI refresh v2 §2.4 — stepper progress (cosmetic only). Each
  # tab change recomputes which pills are "done" (i.e. before the
  # currently active one) and adds the .is-done class so the CSS
  # paints them with a navy filled circle + check icon. No business
  # logic; no input rewiring; just visual progress.
  observe({
    current <- input$wizard_tabs
    if (is.null(current)) return()
    steps <- c("tab_config", "tab_layout", "tab_upload",
               "tab_analysis", "tab_report")
    idx <- match(current, steps)
    if (is.na(idx)) return()
    shinyjs::runjs(sprintf(
      paste("$('.nav-pills > li').removeClass('is-done');",
            "$('.nav-pills > li').slice(0, %d).addClass('is-done');",
            sep = "\n"),
      idx - 1
    ))
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
      p(class = "muted", style = "font-size: var(--t-small);",
        tr("quickstart_desc", lang))
    )
  })

  # GUI refresh §3.1: <details> "More options" disclosure summary text
  # for the manual-configure fallbacks. Translation key falls back to a
  # sensible English literal if the key is not yet present in i18n.R.
  output$quickstart_more_options_summary_ui <- renderUI({
    lang <- input$app_language %||% "en"
    HTML(tr("quickstart_more_options", lang))
  })

  output$quickstart_manual_note_ui <- renderUI({
    lang <- input$app_language %||% "en"
    tags$small(style = "color: var(--c-ink-3);", tr("quickstart_or_manual", lang))
  })

  # H1: row headers for the 2x3 Quick Start grid
  output$quickstart_demo_row_label_ui <- renderUI({
    lang <- input$app_language %||% "en"
    tags$h5(style = "margin-top: 12px; margin-bottom: 6px; color: #0D47A1;",
            tr("quickstart_demo_row_label", lang))
  })
  output$quickstart_manual_row_label_ui <- renderUI({
    lang <- input$app_language %||% "en"
    tags$h5(style = "margin-top: 14px; margin-bottom: 6px; color: #4A148C;",
            tr("quickstart_manual_row_label", lang))
  })
  # H1: "No example data — use Configure manually" placeholder in the empty
  # ELISA-Custom Instant-demo slot.
  output$quickstart_no_demo_slot_ui <- renderUI({
    lang <- input$app_language %||% "en"
    div(
      style = paste("display: flex; align-items: center; justify-content: center;",
                    "width: 100%; height: 48px; margin-bottom: 8px; padding: 0 10px;",
                    "border: 2px dashed #BDBDBD; border-radius: 4px;",
                    "color: #757575; font-size: 13px; font-style: italic;"),
      tr("quickstart_no_demo_available", lang)
    )
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

  # #4: small help text under the language checkbox group
  output$report_languages_help_ui <- renderUI({
    lang <- input$app_language %||% "en"
    tags$small(class = "text-muted",
               style = "display: block; margin-top: -4px; margin-bottom: 8px;",
               tr("report_languages_help", lang))
  })

  # #3: compact-vs-detailed explainer block, sits above the Generate button
  output$report_variants_explainer_ui <- renderUI({
    lang <- input$app_language %||% "en"
    div(
      style = paste0(
        "background: #f5f7fa; border-left: 4px solid #1f77b4; ",
        "padding: 10px 14px; margin: 8px 0; border-radius: 4px; ",
        "font-size: 13px; line-height: 1.5;"
      ),
      tags$div(
        style = "font-weight: 600; margin-bottom: 6px;",
        tr("report_variants_explainer_title", lang)
      ),
      tags$div(
        style = "margin-bottom: 4px;",
        HTML(tr("report_variants_explainer_compact", lang))
      ),
      tags$div(
        HTML(tr("report_variants_explainer_full", lang))
      )
    )
  })

  output$notes_feedback_heading_ui <- renderUI({
    lang <- input$app_language %||% "en"
    h4(tr("notes_feedback_heading", lang))
  })

  output$preflight_heading_ui <- renderUI({
    lang <- input$app_language %||% "en"
    h5(style = "margin-top: 0;", tr("preflight_heading", lang))
  })

  # H4: compact download (default / recommended)
  output$download_report_ui <- renderUI({
    lang <- input$app_language %||% "en"
    downloadButton("download_report",
                   tr("download_last_report_compact", lang),
                   class = "btn btn-success btn-lg",
                   style = "width: 100%;")
  })

  # H4: full / detailed download (full audit report)
  output$download_report_full_ui <- renderUI({
    lang <- input$app_language %||% "en"
    downloadButton("download_report_full",
                   tr("download_last_report_full", lang),
                   class = "btn btn-default btn-lg",
                   style = "width: 100%; margin-top: 8px;")
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

  # ---- Tab 4: Analysis Settings static headings & helpText ----

  output$analysis_settings_heading_ui <- renderUI({
    lang <- input$app_language %||% "en"
    h4(id = "analysis_settings_title", tr("analysis_settings_title", lang))
  })

  output$regression_weight_help_ui <- renderUI({
    lang <- input$app_language %||% "en"
    helpText(tr("regression_weight_help", lang))
  })

  output$advanced_options_heading_ui <- renderUI({
    lang <- input$app_language %||% "en"
    tagList(
      icon("sliders-h"),
      span(class = "title", tr("advanced_options", lang)),
      span(class = "sub",   tr("advanced_options_sub", lang))
    )
  })

  output$beta_warning_text <- renderText({
    tr("beta_warning", input$app_language %||% "en")
  })

  output$advanced_options_intro_ui <- renderUI({
    lang <- input$app_language %||% "en"
    tags$p(style = "margin: 0 0 12px 0; color: #555; font-size: 13px;",
           tr("advanced_options_intro", lang))
  })

  output$quant_range_help_ui <- renderUI({
    lang <- input$app_language %||% "en"
    helpText(tr("quant_range_help", lang))
  })

  output$outlier_help_ui <- renderUI({
    lang <- input$app_language %||% "en"
    helpText(tr("outlier_help", lang))
  })

  output$normality_shapiro_help_ui <- renderUI({
    lang <- input$app_language %||% "en"
    helpText(tr("normality_shapiro_help", lang))
  })

  output$cv_limit_help_ui <- renderUI({
    lang <- input$app_language %||% "en"
    helpText(tr("cv_limit_help", lang))
  })

  # ---- Tab 3: Upload tab chrome ----

  output$upload_counts_ui <- renderUI({
    lang <- input$app_language %||% "en"
    fileInput("upload_counts", tr("upload_label", lang),
              accept = c(".txt", ".csv", ".xlsx"))
  })

  output$clear_upload_ui <- renderUI({
    lang <- input$app_language %||% "en"
    actionButton("clear_upload", "", icon = icon("trash"),
                 title = tr("clear_file", lang),
                 style = "background-color:#f8d7da; border:none;")
  })

  output$download_plate_template_ui <- renderUI({
    lang <- input$app_language %||% "en"
    downloadButton("download_plate_template", tr("download_example_file", lang),
                   class = "btn btn-default btn-sm")
  })

  output$visual_selector_heading_ui <- renderUI({
    lang <- input$app_language %||% "en"
    h5(tags$b(tr("visual_selector_title", lang)))
  })

  output$visual_selector_intro_ui <- renderUI({
    lang <- input$app_language %||% "en"
    p(tr("visual_instructions", lang))
  })

  output$heatmap_title_ui <- renderUI({
    lang <- input$app_language %||% "en"
    h5(tr("heatmap_title", lang))
  })

  output$heatmap_caption_ui <- renderUI({
    lang <- input$app_language %||% "en"
    tr("heatmap_desc", lang)
  })

  # --------------------------------------------------------------------------
  # H2: "Auto" regression weighting is mutually exclusive with the three
  # manual choices. Toggle observer uses a guard flag to avoid infinite
  # recursion when we update the very input we are watching.
  # --------------------------------------------------------------------------

  auto_weight_guard <- reactiveVal(FALSE)
  observeEvent(input$regression_weight, ignoreInit = TRUE, {
    if (isTRUE(auto_weight_guard())) {
      auto_weight_guard(FALSE)
      return()
    }
    sel <- input$regression_weight
    if (is.null(sel) || !length(sel)) return()
    # If "auto" was just enabled alongside other choices, drop the others.
    if ("auto" %in% sel && length(sel) > 1) {
      auto_weight_guard(TRUE)
      updateCheckboxGroupInput(session, "regression_weight", selected = "auto")
      return()
    }
    # If a manual choice was enabled while "auto" was selected, drop "auto".
    if ("auto" %in% sel && length(sel) == 1) return()  # auto-only: fine
    if ("auto" %in% sel) {
      auto_weight_guard(TRUE)
      updateCheckboxGroupInput(session, "regression_weight",
                               selected = setdiff(sel, "auto"))
    }
  })

  # --------------------------------------------------------------------------
  # Language Observer — update all input labels on language change
  # --------------------------------------------------------------------------

  observeEvent(input$app_language, {
    lang <- input$app_language
    # #4: report_languages (checkboxGroupInput) is now an independent selection
    # — the UI language doesn't force which language(s) get rendered, the user
    # ticks them explicitly.
    # start_tour is now a hand-rolled tags$button (v3 §1). updateActionButton
    # only works reliably on Shiny's own actionButton structure; use
    # shinyjs::html() to update the innerHTML of our plain <button> instead.
    shinyjs::html("start_tour", tr("start_tour", lang))
    updateActionButton(session, "convert",
                       label = tr("generate_report", lang),
                       icon  = icon("file-arrow-down"))

    # Tab 1: Quick Start tiles (v3 §3) are now hand-rolled tags$a elements
    # with a structured interior (rail + body divs). updateActionButton would
    # replace the entire innerHTML with just icon + text, destroying the card
    # layout. Instead, send only the inner text via a targeted custom message
    # that walks [data-i18n] attributes and swaps textContent — the tile
    # structure is never touched.
    session$sendCustomMessage("bs_tile_i18n", list(
      qs_tile_rba_title    = tr("qs_tile_rba_title", lang),
      qs_tile_rba_sub      = tr("qs_tile_rba_sub", lang),
      qs_tile_elisa_co_title = tr("qs_tile_elisa_co_title", lang),
      qs_tile_elisa_co_sub   = tr("qs_tile_elisa_co_sub", lang),
      qs_tile_elisa_cu_title = tr("qs_tile_elisa_cu_title", lang),
      qs_tile_elisa_cu_sub   = tr("qs_tile_elisa_cu_sub", lang),
      qs_tile_demo_meta    = tr("qs_tile_demo_meta", lang),
      qs_tile_no_demo_meta = tr("qs_tile_no_demo_meta", lang)
    ))

    # The two manual-configure buttons inside <details> are still real Shiny
    # actionButtons and CAN be safely updated.
    updateActionButton(session, "qs_rba_stx_manual",
                       label = tr("preset_rba_stx_btn", lang),
                       icon  = icon("flask"))
    updateActionButton(session, "qs_elisa_cortisol_manual",
                       label = tr("preset_elisa_cortisol_btn", lang),
                       icon  = icon("vial"))

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
                       label = tr("save_layout_short", lang),
                       icon  = icon("save"))
    updateActionButton(session, "undo_layout",
                       label = tr("undo_btn", lang),
                       icon  = icon("undo"))
    updateActionButton(session, "redo_layout",
                       label = tr("redo_btn", lang),
                       icon  = icon("redo"))
    updateActionButton(session, "apply_uniform_dilution", label = tr("apply_btn", lang))
    updateActionButton(session, "apply_uniform_extraction", label = tr("apply_btn", lang))
    updateActionButton(session, "reset_type", label = tr("reset_btn", lang))
    updateActionButton(session, "reset_id", label = tr("reset_btn", lang))
    updateActionButton(session, "reset_dilution", label = tr("reset_btn", lang))
    updateActionButton(session, "reset_replicate", label = tr("reset_btn", lang))
    updateCheckboxInput(session, "advanced_dilution", label = tr("per_well_label", lang))
    updateTextInput(session, "expected_hill", label = tr("expected_hill", lang))

    updateRadioButtons(session, "import_method", label = tr("upload_or_visual", lang),
                      choices = setNames(c("classic", "visual"),
                                        c(tr("import_classic", lang), tr("import_visual", lang))),
                      selected = input$import_method %||% "classic",
                      inline = TRUE)
    updateActionButton(session, "show_sample_layout", label = tr("show_layout", lang))
    updateCheckboxGroupInput(session, "export_formats",
                             label = tr("report_formats", lang),
                             choices = tr_choices(
                               c("html", "docx", "pdf"),
                               c("format_html", "format_docx", "format_pdf"), lang),
                             selected = input$export_formats %||% "html")
    updateCheckboxInput(session, "generate_compact",
                        label = tr("generate_compact_label", lang))
    updateCheckboxGroupInput(session, "report_languages",
                             label = tr("report_languages_label", lang),
                             choices = c("English" = "en", "Español" = "es",
                                         "Français" = "fr",
                                         "Русский" = "ru",
                                         "中文" = "zh"),
                             selected = input$report_languages %||% "en",
                             inline = TRUE)
    updateTextAreaInput(session, "notes", label = tr("notes_full_label", lang),
                       placeholder = tr("notes_report_placeholder", lang))
    updateCheckboxGroupInput(session, "regression_weight",
                             label = tr("regression_weight_label", lang),
                             choices = tr_choices(
                               c("none", "inv_y", "inv_y2", "auto"),
                               c("weight_unweighted", "weight_inv_y",
                                 "weight_inv_y2", "weight_auto"),
                               lang),
                             selected = input$regression_weight %||% "none")
    updateNumericInput(session, "quant_range_min", label = tr("quant_range_min_label", lang))
    updateNumericInput(session, "quant_range_max", label = tr("quant_range_max_label", lang))
    updateRadioButtons(session, "ci_method", label = tr("ci_method_label", lang),
                       choices = tr_choices(
                         c("t_dist", "bootstrap"),
                         c("ci_t_dist", "ci_bootstrap_choice"), lang),
                       selected = input$ci_method %||% "t_dist",
                       inline = TRUE)
    updateCheckboxInput(session, "enable_outlier_detection", label = tr("outlier_detection_label", lang))
    updateNumericInput(session, "outlier_min_n", label = tr("outlier_min_n_label", lang))
    updateNumericInput(session, "cv_limit", label = tr("cv_limit_label", lang))
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
        # #qc_section sits below the matrix grid; "top" anchors the tooltip
        # above the section so the two input fields remain visible below.
        position = "top",
        stringsAsFactors = FALSE
      ))
    } else if (assay == "elisa") {
      layout_steps <- rbind(layout_steps, data.frame(
        element = "#tissue_weight_section",
        intro = tr("tour_tissue_weights", lang),
        tab = "tab_layout",
        position = "top",
        stringsAsFactors = FALSE
      ))
    }
    tour_steps <- rbind(tour_steps, layout_steps)

    # --- Tab 3: Upload & Preview -----------------------------------------
    # Target the narrower #upload_controls wrapper (import method + file input
    # + download example) rather than the full #upload_section, which is very
    # tall and pushes the tooltip off-screen under intro.js "auto" placement.
    tour_steps <- rbind(tour_steps, data.frame(
      element = c("#upload_controls", "#heatmap_preview_section"),
      intro = c(
        tr("tour_upload", lang),
        tr("tour_heatmap_preview", lang)
      ),
      tab = "tab_upload",
      position = c("bottom", "top"),
      stringsAsFactors = FALSE
    ))

    # --- Tab 4: Analysis Settings ----------------------------------------
    tour_steps <- rbind(tour_steps, data.frame(
      element = "#analysis_settings_section",
      intro = tr("tour_analysis", lang),
      tab = "tab_analysis",
      position = "top",
      stringsAsFactors = FALSE
    ))

    # --- Tab 5: Generate Report ------------------------------------------
    # Explicit positions: preflight sits below the two columns so "top"
    # anchors above it; convert_section is the column(8) block so "right"
    # points the tooltip at it from the notes side; notes_feedback_section
    # is the column(4) block on the right so "left" anchors the tooltip
    # into the page rather than off the right edge.
    tour_steps <- rbind(tour_steps, data.frame(
      element = c("#preflight_section", "#convert_section", "#notes_feedback_section"),
      intro = c(
        tr("tour_preflight", lang),
        tr("tour_convert", lang),
        tr("tour_notes", lang)
      ),
      tab = "tab_report",
      position = c("top", "right", "left"),
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

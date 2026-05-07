# server_config.R
# Assay configuration server logic: quick start buttons, assay description,
# matrix initialization, QC inputs/warnings, toxin/analyte config, and
# standard concentration inputs.

server_config <- function(input, output, session, shared) {

  # --------------------------------------------------------------------------
  # Quick Start Buttons
  # --------------------------------------------------------------------------

  # Load a preset plate layout + (optionally) preload example data.
  # H1: five Quick Start buttons call this with different arg combinations.
  #
  # assay         One of "rba" or "elisa".
  # analyte       For RBA: "Saxitoxin". For ELISA: "cortisol" or "custom".
  # load_example  If TRUE, also read the matching example CSV into
  #               shared$matrix_measresults() and jump to the Upload tab.
  #               If FALSE, jump to the Plate Layout tab for manual config.
  load_preset <- function(assay, analyte, load_example = FALSE) {
    updateSelectInput(session, "assay_type", selected = assay)

    if (assay == "elisa") {
      updateSelectInput(session, "elisa_analyte", selected = analyte)
      n <- if (identical(analyte, "custom")) 0L else 8L
      shared$matrix_type(create_type_matrix("elisa", n))
      shared$matrix_id(create_id_matrix("elisa", n))
      shared$matrix_dilution(create_dilution_matrix())
      shared$matrix_replicate(create_replicate_matrix("elisa"))
    } else {
      # RBA: use the existing preset RDS via the preset_layout selector so
      # the standard concentrations + dilution grid come from the same
      # source of truth used when the user picks the preset manually.
      updateSelectInput(session, "preset_layout", selected = "rba_stx_triplicate")
    }

    if (load_example) {
      lang <- input$app_language %||% "en"
      example_file <- switch(paste(assay, analyte, sep = "/"),
        "rba/Saxitoxin"    = "examples/rba_stx_example.csv",
        "elisa/cortisol"   = "examples/elisa_cortisol_example.csv",
        NULL
      )
      loaded <- FALSE
      if (!is.null(example_file) && file.exists(example_file)) {
        parsed <- tryCatch(parse_plate_file(example_file),
                           error = function(e) NULL)
        if (!is.null(parsed)) {
          shared$rv$is_multiwavelength <- isTRUE(parsed$is_multiwavelength)
          shared$rv$wavelengths        <- parsed$wavelengths
          shared$rv$wavelength_plates  <- if (isTRUE(parsed$is_multiwavelength)) parsed$plates else NULL
          shared$matrix_measresults(parsed$plates[[1]])
          loaded <- TRUE
        }
      }
      if (!loaded) {
        showNotification(
          "Example file not found — please upload your own data.",
          type = "warning", duration = 5
        )
      }
      updateTabsetPanel(session, "wizard_tabs", selected = "tab_upload")
    } else {
      updateTabsetPanel(session, "wizard_tabs", selected = "tab_layout")
    }

    # User-facing notification
    msg_key <- if (load_example) {
      paste0("qs_notify_", assay, "_", analyte, "_demo")
    } else {
      paste0("qs_notify_", assay, "_", analyte, "_manual")
    }
    lang <- input$app_language %||% "en"
    showNotification(tr(msg_key, lang), type = "message", duration = 5)
  }

  # H1 Row 1 (Instant demo) observers
  observeEvent(input$qs_rba_stx_demo, {
    load_preset("rba", "Saxitoxin", load_example = TRUE)
  })
  observeEvent(input$qs_elisa_cortisol_demo, {
    load_preset("elisa", "cortisol", load_example = TRUE)
  })

  # H1 Row 2 (Configure manually) observers
  observeEvent(input$qs_rba_stx_manual, {
    load_preset("rba", "Saxitoxin", load_example = FALSE)
  })
  observeEvent(input$qs_elisa_cortisol_manual, {
    load_preset("elisa", "cortisol", load_example = FALSE)
  })
  observeEvent(input$qs_elisa_custom_manual, {
    load_preset("elisa", "custom", load_example = FALSE)
  })

  # --------------------------------------------------------------------------
  # Assay Description
  # --------------------------------------------------------------------------

  output$assay_description <- renderUI({
    lang <- input$app_language %||% "en"
    if (input$assay_type == "rba") {
      div(
        style = "font-style: italic; color: #666; margin-top: 10px;",
        tr("rba_description", lang)
      )
    } else {
      div(
        style = "font-style: italic; color: #666; margin-top: 10px;",
        tr("elisa_description", lang)
      )
    }
  })

  # --------------------------------------------------------------------------
  # Initialize Matrices (Assay-Type Aware)
  # --------------------------------------------------------------------------

  prev_assay_type <- reactiveVal(NULL)
  prev_num_standards <- reactiveVal(NULL)

  observeEvent(list(input$num_standards, input$assay_type), {
    n <- as.integer(input$num_standards)
    req(!is.na(n))

    assay <- input$assay_type %||% "rba"

    # Only reset matrices when assay type or num_standards actually changed
    # This prevents flickering from reactive invalidation cascades
    if (identical(assay, isolate(prev_assay_type())) &&
        identical(n, isolate(prev_num_standards()))) {
      return()
    }

    prev_assay_type(assay)
    prev_num_standards(n)

    type_mat <- create_type_matrix(assay, n)
    id_mat <- create_id_matrix(assay, n)
    replicate_mat <- create_replicate_matrix(assay)  # Pass assay type

    shared$matrix_type(type_mat)
    shared$matrix_id(id_mat)
    shared$matrix_replicate(replicate_mat)  # Update replicate matrix too
  })

  # --------------------------------------------------------------------------
  # QC Inputs (Dynamic based on assay type)
  # --------------------------------------------------------------------------

  output$qc_concentration_input <- renderUI({
    lang <- input$app_language %||% "en"
    assay <- input$assay_type %||% "rba"

    if (assay == "elisa") {
      units <- input$elisa_units %||% "pg/mL"
      default_val <- if (input$elisa_analyte == "cortisol") "100" else "50"
      textInput(
        "qc_conc",
        tr("qc_conc_label", lang, units),
        value = default_val,
        placeholder = default_val
      )
    } else {
      textInput(
        "qc_conc",
        tr("qc_conc_label", lang, "mol/L"),
        value = "3e-9",
        placeholder = "3e-9"
      )
    }
  })

  # --------------------------------------------------------------------------
  # QC Validation (Enhanced for both assay types)
  # --------------------------------------------------------------------------

  output$qc_warnings <- renderUI({
    lang <- input$app_language %||% "en"
    qc_val <- input$qc_conc
    assay <- input$assay_type %||% "rba"

    if (is.null(qc_val) || qc_val == "") {
      return(tags$div(style = "color: red; font-weight: bold;",
                     tr("qc_required", lang)))
    }

    if (assay == "rba") {
      # RBA: Expect scientific notation
      if (!grepl("^[0-9.]+[eE][-+]?[0-9]+$", trimws(qc_val))) {
        return(tags$div(style = "color: red; font-weight: bold;",
                       tr("sci_notation", lang)))
      }
      num <- as.numeric(qc_val)
      if (num < 1e-12 || num > 1e-6) {
        return(tags$div(style = "color: orange; font-weight: bold;",
                       tr("outside_rba_range", lang)))
      }
    } else {
      # ELISA: Expect regular number
      num <- suppressWarnings(as.numeric(qc_val))
      if (is.na(num)) {
        return(tags$div(style = "color: red; font-weight: bold;",
                       tr("must_be_numeric", lang)))
      }
      if (num < 0.1 || num > 10000) {
        return(tags$div(style = "color: orange; font-weight: bold;",
                       tr("outside_elisa_range", lang)))
      }
    }

    NULL
  })

  # --------------------------------------------------------------------------
  # Toxin/Analyte Configuration
  # --------------------------------------------------------------------------

  output$toxin_variant_ui <- renderUI({
    lang <- input$app_language %||% "en"
    if (input$toxin_class == "Ciguatoxin") {
      selectInput("toxin_variant", tr("toxin_variant_label", lang), choices = CTX_VARIANTS)
    } else if (input$toxin_class == "Brevetoxin") {
      selectInput("toxin_variant", tr("toxin_variant_label", lang), choices = BTX_VARIANTS)
    } else {
      NULL
    }
  })

  chosen_standard_label <- reactive({
    cls <- input$toxin_class
    if (cls == "Ciguatoxin") {
      input$toxin_variant %||% "Ciguatoxin"
    } else if (cls == "Brevetoxin") {
      input$toxin_variant %||% "Brevetoxin"
    } else if (cls == "Custom") {
      trimws(input$toxin_custom_name %||% "Custom")
    } else {
      "Saxitoxin"
    }
  })

  needs_manual_mw <- reactive({
    lab <- chosen_standard_label()
    input$toxin_class == "Custom" || !(lab %in% names(MW_LOOKUP))
  })

  output$mw_box_ui <- renderUI({
    lang <- input$app_language %||% "en"
    lab <- chosen_standard_label()

    if (needs_manual_mw()) {
      numericInput("mw_manual", tr("molecular_weight_label", lang, lab),
                  value = if (!is.na(shared$mw_g_mol())) shared$mw_g_mol() else NULL,
                  min = 0, step = 0.01)
    } else {
      div(class = "well", style = "padding: 10px;",
          tags$b(tr("molecular_weight_readonly", lang), " "),
          sprintf("%.2f", MW_LOOKUP[[lab]]))
    }
  })

  observeEvent(input$mw_manual, {
    if (needs_manual_mw()) shared$mw_g_mol(as.numeric(input$mw_manual))
  })

  observeEvent(list(input$toxin_class, input$toxin_variant), {
    cls <- input$toxin_class
    lab <- chosen_standard_label()
    if (cls != "Custom" && lab %in% names(MW_LOOKUP)) {
      shared$mw_g_mol(MW_LOOKUP[[lab]])
    }
  }, ignoreInit = FALSE)

  # --------------------------------------------------------------------------
  # Standard Inputs
  # --------------------------------------------------------------------------

  output$std_inputs <- renderUI({
    req(as.integer(input$num_standards) > 0)
    n <- as.integer(input$num_standards)
    assay <- input$assay_type %||% "rba"

    # Choose defaults based on assay type
    defaults <- if (assay == "elisa") {
      analyte <- input$elisa_analyte %||% "cortisol"
      if (analyte == "testosterone") {
        DEFAULT_TESTOSTERONE_CONC[1:n]
      } else if (analyte == "estradiol") {
        DEFAULT_ESTRADIOL_CONC[1:n]
      } else {
        DEFAULT_CORTISOL_CONC[1:n]
      }
    } else {
      DEFAULT_STX_CONC[1:n]
    }

    # Choose units based on assay
    unit_label <- if (assay == "elisa") {
      input$elisa_units %||% "pg/mL"
    } else {
      "mol/L"
    }

    rows_list <- lapply(seq_len(ceiling(n/4)), function(r) {
      start <- (r - 1) * 4 + 1
      end <- min(r * 4, n)
      cols <- lapply(start:end, function(i) {
        default_val <- if (i <= length(defaults)) defaults[i] else {
          if (assay == "elisa") 10^(3-i) else 10^(-(i+4))
        }

        column(width = 3,
               textInput(
                 paste0("std", i),
                 paste0("S", i, " (", unit_label, ")"),
                 value = if (assay == "elisa") {
                   format(default_val, digits = 4, nsmall = 0)
                 } else {
                   format(default_val, scientific = TRUE)
                 }
               ))
      })
      fluidRow(cols)
    })
    do.call(tagList, rows_list)
  })

  std_conc_raw <- reactive({
    n <- as.integer(input$num_standards)
    assay <- input$assay_type %||% "rba"

    elisa_defaults <- function() {
      analyte <- input$elisa_analyte %||% "cortisol"
      if (analyte == "testosterone") DEFAULT_TESTOSTERONE_CONC
      else if (analyte == "estradiol") DEFAULT_ESTRADIOL_CONC
      else DEFAULT_CORTISOL_CONC
    }

    if (is.null(n) || n == 0) {
      return(if (assay == "elisa") elisa_defaults()[1:8] else DEFAULT_STX_CONC[1:8])
    }

    sapply(seq_len(n), function(i) {
      val <- input[[paste0("std", i)]]
      if (is.null(val) || val == "") {
        if (assay == "elisa") elisa_defaults()[i] else DEFAULT_STX_CONC[i]
      } else {
        as.numeric(val)
      }
    })
  })
  std_conc <- std_conc_raw %>% debounce(400)

  # --------------------------------------------------------------------------
  # Return computed reactives needed by other server modules
  # --------------------------------------------------------------------------

  list(
    std_conc = std_conc,              # debounced — for UI reactives / pre-flight
    std_conc_raw = std_conc_raw,      # synchronous — for export at Convert time
    chosen_standard_label = chosen_standard_label
  )
}

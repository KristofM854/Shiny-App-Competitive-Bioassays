# ==============================================================================
# RBA Analysis App (Modular Version)
# Author: Arnold Molina Porras (UCR) & Kristof Moeller (IAEA)
# Version: 2.0 - Modular architecture with smart import
# ==============================================================================

# IMPORTANT: Source global.R first (contains PLATE_NROW, etc.)
if (!exists("PLATE_NROW")) {
  source("global.R")
}

# Source utility modules (these depend on global.R constants)
source("utils_import_v3.R")
source("utils_plate.R")
source("utils_import_multiwavelength.R")  
source("utils_normalization.R")
source("i18n.R")

# Get output directory from environment (set by run_analysis.R)
output_dir <- Sys.getenv("RBA_OUTPUT_DIR", ".")

# ============================================================================
# UI DEFINITION
# ============================================================================

ui <- fluidPage(
  useShinyjs(),
  introjsUI(),
  shinyFeedback::useShinyFeedback(),
  
  titlePanel("RBA Analysis – Microplate Processing & Curve Fitting"),
  br(),
  
  # ------------------------------
  # Language Toggle & Guided Tour
  # ------------------------------
  div(
    style = "display: flex; justify-content: flex-start; gap: 15px; margin-bottom: 20px; align-items: center;",
    actionButton("start_tour", "🚀 Start Guided Tour", 
                class = "btn btn-lg btn-info",
                style = "font-size: 18px; padding: 15px 30px;"),
    div(
      id = "language_toggle_section",
      style = "display: flex; align-items: center; gap: 8px;",
      tags$span("🌐", style = "font-size: 20px;"),
      selectInput("app_language", NULL,
                  choices = c("English" = "en", "Español" = "es"),
                  selected = "en",
                  width = "130px")
    )
  ),
  
  # ------------------------------
  # Step 0: Assay Configuration
  # ------------------------------
  introBox(
    div(
      id = "step0_section",
      column(12, h4("Step 0: Assay Configuration")),
      
      fluidRow(
        column(
          width = 6,
          
          # NEW: Assay type selection
          wellPanel(
            style = "background-color: #E3F2FD; border-left: 4px solid #2196F3;",
            h5(tags$b("Select Assay Type")),
            selectInput(
              "assay_type",
              "Type of assay:",
              choices = c(
                "Receptor Binding Assay (RBA)" = "rba",
                "ELISA (Enzyme-Linked Immunosorbent Assay)" = "elisa"
              ),
              selected = "rba"
            ),
            uiOutput("assay_description")
          ),
          
          # Conditional: RBA-specific inputs
          conditionalPanel(
            condition = "input.assay_type == 'rba'",
            
            selectInput(
              "toxin_class",
              "Toxin standard used:",
              choices = c("Saxitoxin", "Brevetoxin", "Ciguatoxin", "Custom"),
              selected = "Saxitoxin"
            ),
            
            uiOutput("toxin_variant_ui"),
            
            conditionalPanel(
              condition = "input.toxin_class == 'Custom'",
              textInput("toxin_custom_name", "Custom standard name:",
                       placeholder = "e.g., GTX2/3 mix")
            ),
            
            div(
              style = "max-width: 420px;",
              uiOutput("mw_box_ui")
            )
          ),
          
          # Conditional: ELISA-specific inputs
          conditionalPanel(
            condition = "input.assay_type == 'elisa'",
            
            selectInput(
              "elisa_analyte",
              "Analyte:",
              choices = c(
                "Cortisol" = "cortisol",
                "Testosterone" = "testosterone",
                "Custom" = "custom"
              ),
              selected = "cortisol"
            ),
            
            conditionalPanel(
              condition = "input.elisa_analyte == 'custom'",
              textInput("elisa_custom_name", "Custom analyte name:",
                       placeholder = "e.g., Estradiol")
            ),
            
            # ELISA: Units are typically pg/mL or ng/mL
            selectInput(
              "elisa_units",
              "Standard concentration units:",
              choices = c(
                "pg/mL" = "pg/mL",
                "ng/mL" = "ng/ml",
                "µg/mL" = "ug/ml"
              ),
              selected = "pg/mL"
            )
          ),
          
          # Standard concentrations (shown for both assay types)
          hr(),
          p(tags$b("Standard Concentrations")),
          p("Specify the number of standards, then enter each concentration."),
          
          # Dynamic unit guidance
          uiOutput("concentration_unit_guidance"),
          
          selectInput("num_standards", "Number of standards:",
                     choices = 0:12, selected = 8),
          
          div(
            style = "display:flex; flex-wrap: wrap; gap: 10px;",
            uiOutput("std_inputs")
          ),
          
          uiOutput("std_error_feedback")
        ),
        column(width = 6)
      )
    ),
    data.step = 0,
    data.intro = "Define your standard concentrations here."
  ),
  br(),
  
  # ------------------------------
  # Main Content: Matrices + Upload
  # ------------------------------
  fluidRow(
    # LEFT COLUMN: Matrices
    column(
      width = 6,
      id = "step1_section",
      style = "padding-bottom: 100px;",
      
      h4("Step 1: Edit Microplate Matrices & QC Parameters"),
      
      # Type matrix
      div(
        id = "matrix_type_section",
        h5("1. Sample Type (Standard, Sample, QC, Blank, Other)"),
        
        # ELISA helper panel
        conditionalPanel(
          condition = "input.assay_type == 'elisa'",
          div(
            style = "background-color: #FFF9E6; padding: 10px; margin: 10px 0; border-left: 4px solid #FFC107;",
            tags$b("ELISA Control Wells:"),
            tags$ul(
              tags$li(tags$b("Blank:"), " No enzyme, no antibody (background absorbance)"),
              tags$li(tags$b("NSB:"), " Non-specific binding (enzyme only, no antibody)"),
              tags$li(tags$b("B0:"), " Maximum binding (no competing analyte)"),
              tags$li(tags$b("TotalActivity:"), " Total enzyme activity (optional)")
            ),
            tags$p(
              style = "margin-top: 10px; font-style: italic;",
              "💡 Tip: Typically assign Blank/NSB/B0 in Column 1, rows A-G. Standards go in columns 2-3."
            )
          )
        ),
        
        rHandsontableOutput("matrix_type")
      ),
      br(),
      
      # ID matrix
      div(
        id = "matrix_id_section",
        h5("2. Sample ID"),
        actionButton("reset_id", "Reset to Default"),
        rHandsontableOutput("matrix_id")
      ),
      br(),
      
      # QC fields (RBA only - ELISA uses different QC approach)
      conditionalPanel(
        condition = "input.assay_type == 'rba'",
        div(
          id = "qc_section",
          h5("3. Quality Control Parameters"),
          
          # QC concentration input (units depend on assay type)
          uiOutput("qc_concentration_input"),
          
          textInput("expected_hill", "Expected Hill slope:", 
                   value = "1", placeholder = "1"),
          uiOutput("qc_warnings"),
          uiOutput("hill_warning")
        )
      ),
      br(),
      
      # Dilution matrix
      div(
        id = "matrix_dilution_section",
        h5("4. Dilution Factors (numeric or ratio like 1:2)"),
        uiOutput("dilution_error_feedback"),
        actionButton("reset_dilution", "Reset to Default"),
        rHandsontableOutput("matrix_dilution")
      ),
      br(),
      
      # Replicate matrix
      div(
        id = "matrix_replicate_section",
        h5("5. Replicate Groups"),
        actionButton("reset_replicate", "Reset to Default"),
        rHandsontableOutput("matrix_replicate")
      ),
      br(),
      
      # Tissue weight input (ELISA only)
      conditionalPanel(
        condition = "input.assay_type == 'elisa'",
        div(
          id = "tissue_weight_section",
          h5("6. Tissue Weights & Extraction Volume (optional)"),
          div(
            style = "background-color: #FFF3E0; padding: 10px; margin: 10px 0; border-left: 4px solid #FF9800;",
            tags$small(
              tags$b("Tissue-based calculation: "),
              "Enter tissue weight (mg) per replicate group to calculate pg cortisol / g tissue. ",
              "Leave blank if not applicable."
            )
          ),
          numericInput("extraction_volume", "Extraction volume (µL):",
                      value = 500, min = 1, max = 10000, step = 50),
          rHandsontableOutput("tissue_weight_table")
        )
      )
    ),
    
    # RIGHT COLUMN: Upload + Report
    column(
      width = 6,
      style = "padding-bottom: 100px;",
      
      # Upload section
      div(
        id = "upload_section",
        h4("Step 2: Upload Plate Data"),
        
        # Import method toggle
        radioButtons("import_method", "Import method:",
                    choices = c("Classic Import" = "classic", 
                                "Visual Plate Selector" = "visual"),
                    selected = "classic", inline = TRUE),
        
        div(
          style = "display:flex; gap:10px;",
          fileInput("upload_counts", "Upload Bioassay Results",
                   accept = c(".txt", ".csv", ".xlsx")),
          actionButton("clear_upload", "", icon = icon("trash"),
                      title = "Remove file",
                      style = "margin-top: 30px; background-color:#f8d7da; border:none;")
        ),
        
        # Visual plate selector panel (shown when visual import selected)
        conditionalPanel(
          condition = "input.import_method == 'visual'",
          div(
            id = "visual_selector_section",
            style = "border: 2px dashed #2196F3; padding: 15px; margin: 10px 0; border-radius: 8px;",
            h5(tags$b("Visual Plate Selector")),
            p("After uploading a file, a preview will appear below. Click and drag to select 8×12 plate regions."),
            uiOutput("visual_file_preview"),
            uiOutput("visual_plate_selections"),
            uiOutput("visual_well_exclusion")
          )
        ),
        
        actionButton("show_sample_layout", "📊 Show default plate layout",
                    class = "btn btn-sm btn-secondary",
                    style = "margin-top: -10px;")
      ),
      
      div(id = "upload_preview_section", 
          uiOutput("upload_summary"),
          tableOutput("meas_preview")),
      br(),
      
      # Notes
      div(
        id = "notes_feedback_section",
        textAreaInput("notes", "Notes (optional):",
                     value = "", placeholder = "Observations...", rows = 5),
        tags$a(href = "https://forms.office.com/e/q8eqJfp4QM",
              target = "_blank", class = "btn btn-lg btn-info",
              style = "margin-top: 20px;", "Give Feedback")
      ),
      br(), br(),
      
      # Report generation
      div(
        id = "convert_section",
        checkboxGroupInput("export_formats", "Report formats:",
                          choices = c("HTML" = "html", "Word" = "docx"),
                          selected = "html"),
        selectInput("report_language", "Report language:",
                   choices = c("English" = "en", "Español" = "es"),
                   selected = "en",
                   width = "200px"),
        actionButton("convert", 
                    label = tagList(icon("file-arrow-down"), 
                                   "Step 3: Generate Report"),
                    class = "btn btn-primary btn-lg",
                    style = "width: 100%; font-size: 20px; font-weight: 700; 
                            padding: 14px; border-radius: 12px;")
      )
    )
  )
)

# ============================================================================
# SERVER LOGIC
# ============================================================================

server <- function(input, output, session) {
  
  # --------------------------------------------------------------------------
  # Reactive Values
  # --------------------------------------------------------------------------
  
  matrix_type <- reactiveVal()
  matrix_id <- reactiveVal()
  matrix_dilution <- reactiveVal(create_dilution_matrix())
  matrix_measresults <- reactiveVal(create_plate_matrix())
  matrix_replicate <- reactiveVal(create_replicate_matrix("rba"))  # Initialize with RBA default
  
  # Raw dilution input (strings) and validity
  raw_matrix_dilution <- reactiveVal(default_raw_dilution())
  dilution_validity <- reactiveVal(matrix(TRUE, nrow = 8, ncol = 12))
  dilution_error <- reactiveVal(FALSE)
  
  # Molecular weight (RBA only)
  mw_g_mol <- reactiveVal(299.29)  # Default: Saxitoxin
  
  rv <- reactiveValues(
    plate_data = NULL,
    plate_layout = NULL,
    # NEW: Multi-wavelength variables
    is_multiwavelength = FALSE,
    wavelengths = NULL,
    wavelength_plates = NULL,
    # ... existing variables
  )
  
  # --------------------------------------------------------------------------
  # Assay Type Configuration
  # --------------------------------------------------------------------------
  
  output$assay_description <- renderUI({
    if (input$assay_type == "rba") {
      div(
        style = "font-style: italic; color: #666; margin-top: 10px;",
        "Receptor binding assays measure displacement of radioligand or fluorescent ligand."
      )
    } else {
      div(
        style = "font-style: italic; color: #666; margin-top: 10px;",
        "ELISA measures analyte concentration via antibody-enzyme reactions."
      )
    }
  })
  
  # --------------------------------------------------------------------------
  # Initialize Matrices (Assay-Type Aware)
  # --------------------------------------------------------------------------
  
  observeEvent(list(input$num_standards, input$assay_type), {
    n <- as.integer(input$num_standards)
    req(!is.na(n))
    
    assay <- input$assay_type %||% "rba"
    
    type_mat <- create_type_matrix(assay, n)
    id_mat <- create_id_matrix(assay, n)
    replicate_mat <- create_replicate_matrix(assay)  # Pass assay type
    
    matrix_type(type_mat)
    matrix_id(id_mat)
    matrix_replicate(replicate_mat)  # Update replicate matrix too
  })
  
  # --------------------------------------------------------------------------
  # QC Inputs (Dynamic based on assay type)
  # --------------------------------------------------------------------------
  
  output$qc_concentration_input <- renderUI({
    assay <- input$assay_type %||% "rba"
    
    if (assay == "elisa") {
      units <- input$elisa_units %||% "pg/mL"
      default_val <- if (input$elisa_analyte == "cortisol") "100" else "50"
      textInput(
        "qc_conc", 
        paste0("QC concentration (", units, "):"),
        value = default_val, 
        placeholder = default_val
      )
    } else {
      textInput(
        "qc_conc", 
        "QC concentration (mol/L):",
        value = "3e-9", 
        placeholder = "3e-9"
      )
    }
  })
  
  # --------------------------------------------------------------------------
  # QC Validation (Enhanced for both assay types)
  # --------------------------------------------------------------------------
  
  output$qc_warnings <- renderUI({
    qc_val <- input$qc_conc
    assay <- input$assay_type %||% "rba"
    
    if (is.null(qc_val) || qc_val == "") {
      return(tags$div(style = "color: red; font-weight: bold;",
                     "⚠️ QC concentration required"))
    }
    
    if (assay == "rba") {
      # RBA: Expect scientific notation
      if (!grepl("^[0-9.]+[eE][-+]?[0-9]+$", trimws(qc_val))) {
        return(tags$div(style = "color: red; font-weight: bold;",
                       "⚠️ Use scientific notation (e.g., 3e-9)"))
      }
      num <- as.numeric(qc_val)
      if (num < 1e-12 || num > 1e-6) {
        return(tags$div(style = "color: orange; font-weight: bold;",
                       "⚠️ Outside typical RBA range (1e-12 to 1e-6 mol/L)"))
      }
    } else {
      # ELISA: Expect regular number
      num <- suppressWarnings(as.numeric(qc_val))
      if (is.na(num)) {
        return(tags$div(style = "color: red; font-weight: bold;",
                       "⚠️ Must be a numeric value"))
      }
      if (num < 0.1 || num > 10000) {
        return(tags$div(style = "color: orange; font-weight: bold;",
                       "⚠️ Outside typical ELISA range (0.1-10000)"))
      }
    }
    
    NULL
  })
  
  # --------------------------------------------------------------------------
  # Toxin/Analyte Configuration
  # --------------------------------------------------------------------------
  
  output$toxin_variant_ui <- renderUI({
    if (input$toxin_class == "Ciguatoxin") {
      selectInput("toxin_variant", "Variant:", choices = CTX_VARIANTS)
    } else if (input$toxin_class == "Brevetoxin") {
      selectInput("toxin_variant", "Variant:", choices = BTX_VARIANTS)
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
    lab <- chosen_standard_label()
    
    if (needs_manual_mw()) {
      numericInput("mw_manual", paste0("Molecular weight [g/mol] for ", lab, ":"),
                  value = if (!is.na(mw_g_mol())) mw_g_mol() else NULL,
                  min = 0, step = 0.01)
    } else {
      div(class = "well", style = "padding: 10px;",
          tags$b("Molecular weight [g/mol]: "),
          sprintf("%.2f", MW_LOOKUP[[lab]]))
    }
  })
  
  observeEvent(input$mw_manual, {
    if (needs_manual_mw()) mw_g_mol(as.numeric(input$mw_manual))
  })
  
  observeEvent(list(input$toxin_class, input$toxin_variant), {
    cls <- input$toxin_class
    lab <- chosen_standard_label()
    if (cls != "Custom" && lab %in% names(MW_LOOKUP)) {
      mw_g_mol(MW_LOOKUP[[lab]])
    }
  }, ignoreInit = FALSE)
  
  # --------------------------------------------------------------------------
  # Standard Inputs
  # --------------------------------------------------------------------------
  
  output$concentration_unit_guidance <- renderUI({
    assay <- input$assay_type %||% "rba"
    
    if (assay == "elisa") {
      units <- input$elisa_units %||% "pg/mL"
      div(
        style = "background-color: #E3F2FD; padding: 8px; margin: 8px 0; border-left: 3px solid #2196F3;",
        tags$small(
          tags$b("ELISA Standards: "), 
          sprintf("Enter concentrations in %s (e.g., 4000, 1600, 640...)", units)
        )
      )
    } else {
      div(
        style = "background-color: #E8F5E9; padding: 8px; margin: 8px 0; border-left: 3px solid #4CAF50;",
        tags$small(
          tags$b("RBA Standards: "), 
          "Enter concentrations in mol/L using scientific notation (e.g., 1e-6, 3e-8...)"
        )
      )
    }
  })
  
  output$std_inputs <- renderUI({
    req(as.integer(input$num_standards) > 0)
    n <- as.integer(input$num_standards)
    assay <- input$assay_type %||% "rba"
    
    # Choose defaults based on assay type
    defaults <- if (assay == "elisa") {
      if (input$elisa_analyte == "cortisol") {
        DEFAULT_CORTISOL_CONC[1:n]
      } else {
        # Generic ELISA defaults (adjust for other analytes)
        c(4000, 1600, 640, 256, 102.4, 41.0, 16.4, 6.6)[1:n]
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
  
  std_conc <- reactive({
    n <- as.integer(input$num_standards)
    assay <- input$assay_type %||% "rba"
    
    if (is.null(n) || n == 0) {
      return(if (assay == "elisa") DEFAULT_CORTISOL_CONC[1:8] else DEFAULT_STX_CONC[1:8])
    }
    
    sapply(seq_len(n), function(i) {
      val <- input[[paste0("std", i)]]
      if (is.null(val) || val == "") {
        if (assay == "elisa") DEFAULT_CORTISOL_CONC[i] else DEFAULT_STX_CONC[i]
      } else {
        as.numeric(val)
      }
    })
  })
  
  # --------------------------------------------------------------------------
  # Matrix Renderers
  # --------------------------------------------------------------------------
  
  output$matrix_type <- renderRHandsontable({
    req(matrix_type())
    mat <- matrix_type()
    mat[] <- lapply(mat, as.character)
    
    # Dynamic dropdown based on assay type
    type_choices <- if (input$assay_type == "elisa") {
      c("Standard", "Sample", "QC", "Blank", "NSB", "B0", "TotalActivity", "Other")
    } else {
      c("Standard", "Sample", "QC", "Blank", "Other")
    }
    
    rhandsontable(mat, rowHeaderWidth = 60, rowHeaders = ROW_NAMES) %>%
      hot_col(col = 1:12, type = "dropdown",
              source = type_choices) %>%
      hot_col(col = 1:12, renderer = "
        function(instance, td, row, col, prop, value, cellProperties) {
          Handsontable.renderers.TextRenderer.apply(this, arguments);
          var colors = {
            'Standard': '#90EE90', 
            'QC': '#ADD8E6', 
            'Blank': '#FFE4E1',
            'NSB': '#FFDAB9',
            'B0': '#F0E68C',
            'TotalActivity': '#DDA0DD',
            'Sample': '#E6E6FA', 
            'Other': '#FFD700'
          };
          td.style.backgroundColor = colors[value] || '#FFFFFF';
        }")
  })
  
  output$matrix_id <- renderRHandsontable({
    req(matrix_id())
    mat <- matrix_id()
    mat[] <- lapply(mat, as.character)
    rhandsontable(mat, rowHeaderWidth = 60, rowHeaders = ROW_NAMES)
  })
  
  output$matrix_dilution <- renderRHandsontable({
    req(raw_matrix_dilution())
    df <- raw_matrix_dilution()
    val <- dilution_validity()
    
    rhandsontable(df, rowHeaderWidth = 60, rowHeaders = ROW_NAMES) %>%
      hot_col(col = 1:12, type = "text",
              renderer = htmlwidgets::JS(sprintf(
                "function(instance, td, row, col, prop, value, cellProperties) {
                  Handsontable.renderers.TextRenderer.apply(this, arguments);
                  var valid = %s;
                  if (!valid[row][col]) td.style.background = '#F8D7DA';
                  else td.style.background = '';
                }", jsonlite::toJSON(val))))
  })
  
  output$matrix_replicate <- renderRHandsontable({
    mat <- matrix_replicate()
    mat[] <- lapply(mat, as.character)
    rhandsontable(mat, rowHeaderWidth = 60, rowHeaders = ROW_NAMES)
  })
  
  # --------------------------------------------------------------------------
  # Matrix Observers
  # --------------------------------------------------------------------------
  
  observeEvent(input$matrix_type, {
    if (!is.null(input$matrix_type)) {
      matrix_type(hot_to_r(input$matrix_type))
    }
  })
  
  observeEvent(input$matrix_id, {
    if (!is.null(input$matrix_id)) {
      mat <- hot_to_r(input$matrix_id)
      mat[] <- lapply(mat, as.character)
      matrix_id(mat)
    }
  })
  
  observeEvent(input$matrix_replicate, {
    if (!is.null(input$matrix_replicate)) {
      mat <- hot_to_r(input$matrix_replicate)
      mat[] <- lapply(mat, as.character)
      matrix_replicate(mat)
    }
  })
  
  observeEvent(input$matrix_dilution, {
    req(input$matrix_dilution)
    
    raw <- hot_to_r(input$matrix_dilution)
    raw <- enforce_plate_shape(raw)
    raw_matrix_dilution(raw)
    
    # Parse each cell
    parsed <- matrix(NA_real_, nrow = 8, ncol = 12)
    validity <- matrix(TRUE, nrow = 8, ncol = 12)
    
    for (r in 1:8) {
      for (c in 1:12) {
        res <- parse_dilution_cell(raw[r, c])
        parsed[r, c] <- res$value
        validity[r, c] <- res$valid
      }
    }
    
    matrix_dilution(enforce_plate_shape(as.data.frame(parsed)))
    dilution_validity(validity)
    dilution_error(any(!validity))
  })
  
  # Reset buttons
  observeEvent(input$reset_id, {
    n <- as.integer(input$num_standards)
    matrix_id(create_id_matrix("rba", n))
  })
  
  observeEvent(input$reset_dilution, {
    raw_matrix_dilution(default_raw_dilution())
    matrix_dilution(create_dilution_matrix())
    dilution_validity(matrix(TRUE, nrow = 8, ncol = 12))
    dilution_error(FALSE)
  })
  
  observeEvent(input$reset_replicate, {
    assay <- input$assay_type %||% "rba"
    matrix_replicate(create_replicate_matrix(assay))
  })
  
  # --------------------------------------------------------------------------
  # Tissue Weight Table (ELISA only)
  # --------------------------------------------------------------------------
  
  # Reactive: get unique replicate groups from the replicate matrix  
  replicate_groups <- reactive({
    req(matrix_replicate())
    rep_mat <- matrix_replicate()
    type_mat <- matrix_type()
    
    # Get replicate labels for Sample wells only
    groups <- c()
    for (r in 1:nrow(rep_mat)) {
      for (cc in 1:ncol(rep_mat)) {
        lbl <- as.character(rep_mat[r, cc])
        tp <- as.character(type_mat[r, cc])
        if (!is.na(lbl) && lbl != "" && tp == "Sample") {
          groups <- c(groups, lbl)
        }
      }
    }
    sort(unique(groups))
  })
  
  # Tissue weight reactive storage
  tissue_weights_rv <- reactiveVal(list())
  
  output$tissue_weight_table <- renderRHandsontable({
    req(input$assay_type == "elisa")
    groups <- replicate_groups()
    if (length(groups) == 0) return(NULL)
    
    # Build data frame with current stored values
    tw <- tissue_weights_rv()
    df <- data.frame(
      Replicate = groups,
      Weight_mg = sapply(groups, function(g) {
        val <- tw[[g]]
        if (is.null(val) || is.na(val)) NA_real_ else as.numeric(val)
      }),
      stringsAsFactors = FALSE
    )
    
    rhandsontable(df, rowHeaders = FALSE, width = 350) %>%
      hot_col("Replicate", readOnly = TRUE) %>%
      hot_col("Weight_mg", type = "numeric", format = "0.0")
  })
  
  observeEvent(input$tissue_weight_table, {
    req(input$tissue_weight_table)
    df <- hot_to_r(input$tissue_weight_table)
    tw <- as.list(setNames(df$Weight_mg, df$Replicate))
    tissue_weights_rv(tw)
  })
  
  # --------------------------------------------------------------------------
  # Visual Plate Selector (File Preview)
  # --------------------------------------------------------------------------
  
  # Reactive to store raw Excel content for preview
  rv_file_preview <- reactiveValues(
    raw_data = NULL,
    file_path = NULL,
    detected_plates = list(),
    selected_plates = list(),
    excluded_wells = list()
  )
  
  # Visual file preview: render the uploaded file as an interactive grid
  output$visual_file_preview <- renderUI({
    req(input$upload_counts)
    req(input$import_method == "visual")
    
    file_path <- input$upload_counts$datapath
    ext <- tools::file_ext(input$upload_counts$name)
    
    # Read raw file content
    raw <- tryCatch({
      if (ext %in% c("xlsx", "xls")) {
        suppressMessages(readxl::read_excel(file_path, col_names = FALSE, .name_repair = "minimal"))
      } else if (ext == "csv") {
        read.csv(file_path, header = FALSE, stringsAsFactors = FALSE)
      } else {
        read.table(file_path, header = FALSE, sep = "\t", stringsAsFactors = FALSE)
      }
    }, error = function(e) NULL)
    
    if (is.null(raw)) {
      return(tags$p(style = "color: red;", "Could not read file for preview."))
    }
    
    rv_file_preview$raw_data <- raw
    rv_file_preview$file_path <- file_path
    
    # Auto-detect plate regions
    mat <- as.matrix(raw)
    detected <- list()
    plate_idx <- 1
    
    # Search for 8-row blocks with row labels A-H
    for (i in 1:(nrow(mat) - 7)) {
      potential_rows <- trimws(as.character(mat[i:(i+7), 1]))
      if (identical(potential_rows, LETTERS[1:8])) {
        # Check for numeric data in columns 2+
        test_data <- suppressWarnings(as.numeric(mat[i, 2:min(13, ncol(mat))]))
        num_valid <- sum(!is.na(test_data))
        if (num_valid >= 4) {
          detected[[plate_idx]] <- list(
            start_row = i,
            start_col = 2,
            nrows = 8,
            ncols = min(12, num_valid),
            label = paste0("Plate ", plate_idx, " (rows ", i, "-", i+7, ")")
          )
          plate_idx <- plate_idx + 1
        }
      }
    }
    
    rv_file_preview$detected_plates <- detected
    
    if (length(detected) == 0) {
      return(tags$p(style = "color: orange;", 
                    "No 8×12 plate regions auto-detected. Use Classic Import, or check file format."))
    }
    
    # Build the preview UI with highlighted plate regions
    tagList(
      tags$p(style = "color: green; font-weight: bold;",
             sprintf("✅ %d plate region(s) detected in file.", length(detected))),
      tags$div(
        style = "margin: 10px 0;",
        lapply(seq_along(detected), function(idx) {
          pl <- detected[[idx]]
          checked <- idx <= length(rv_file_preview$selected_plates) || 
                    length(rv_file_preview$selected_plates) == 0
          div(
            style = "display: inline-flex; align-items: center; gap: 8px; margin: 5px 10px;",
            checkboxInput(paste0("select_plate_", idx), 
                         pl$label, value = TRUE),
            tags$small(sprintf("(%d×%d)", pl$nrows, pl$ncols))
          )
        })
      ),
      tags$p(tags$small(style = "color: #666;",
                        "Uncheck plates you do not want to analyze."))
    )
  })
  
  # Well exclusion UI
  output$visual_well_exclusion <- renderUI({
    req(input$import_method == "visual")
    req(length(rv_file_preview$detected_plates) > 0)
    
    tagList(
      hr(),
      h6("Exclude individual wells:"),
      tags$p(tags$small("Enter well positions to exclude (e.g., A1, B3, H12). Separate with commas.")),
      textInput("excluded_wells_input", NULL,
               placeholder = "e.g., A1, B3, H12",
               width = "100%")
    )
  })
  
  # --------------------------------------------------------------------------
  # FILE UPLOAD (NEW: Smart Import)
  # --------------------------------------------------------------------------
  
  observeEvent(input$upload_counts, {
    req(input$upload_counts)
    
    file_path <- input$upload_counts$datapath
    import_mode <- input$import_method %||% "classic"
    
    if (import_mode == "visual") {
      # Visual mode: just show the preview, don't auto-import
      # The visual preview UI is rendered reactively above
      showNotification("File loaded. Select plate regions in the Visual Plate Selector.",
                      type = "message", duration = 5)
      return()
    }
    
    # Classic import mode
    # Check if file contains multiple wavelengths
    is_multiwave <- tryCatch({
      wavelengths <- detect_multiwavelength_plates(file_path)
      !is.null(wavelengths) && length(wavelengths) > 1
    }, error = function(e) FALSE)
    
    if (is_multiwave) {
      rv$is_multiwavelength <- TRUE
      plates <- import_multiwavelength_plates(file_path)
      rv$wavelength_plates <- plates
      rv$wavelengths <- names(plates)
      plate_data <- plates[[1]]
      info <- base::attr(plate_data, "import_info")
      matrix_measresults(plate_data)
      
      showNotification(
        sprintf("✅ Multi-wavelength file detected: %s\n%d wells detected per plate\nFormat: %s%s",
                paste(names(plates), collapse = ", "),
                if (!is.null(info)) info$detected_wells else sum(!is.na(plate_data)),
                if (!is.null(info)) info$format else "Excel",
                if (!is.null(info) && info$partial_plate) " (partial)" else ""),
        type = "message",
        duration = 7
      )
    } else {
      rv$is_multiwavelength <- FALSE
      
      plate <- tryCatch({
        import_plate_data(file_path)
      }, error = function(e) {
        showNotification(paste("Import failed:", e$message), type = "error", duration = 15)
        return(NULL)
      })
      
      req(!is.null(plate))
      
      # Apply well exclusions if any were specified
      excluded_input <- input$excluded_wells_input
      if (!is.null(excluded_input) && nchar(trimws(excluded_input)) > 0) {
        wells <- trimws(strsplit(excluded_input, ",")[[1]])
        for (w in wells) {
          w <- toupper(trimws(w))
          if (grepl("^[A-H][0-9]{1,2}$", w)) {
            row_idx <- match(substr(w, 1, 1), LETTERS[1:8])
            col_idx <- as.integer(substr(w, 2, nchar(w)))
            if (!is.na(row_idx) && !is.na(col_idx) && col_idx >= 1 && col_idx <= 12) {
              plate[row_idx, col_idx] <- NA_real_
            }
          }
        }
        rv_file_preview$excluded_wells <- wells
      }
      
      matrix_measresults(plate)
      info <- base::attr(plate, "import_info")
      
      showNotification(
        sprintf("✅ Imported: %s\n%d wells detected\nFormat: %s%s",
                info$file, info$detected_wells, info$format,
                if (info$partial_plate) " (partial)" else ""),
        type = "message",
        duration = 5
      )
    }
  })
  
  # Handle visual import confirmation
  observeEvent(input$confirm_visual_import, {
    req(rv_file_preview$raw_data)
    req(length(rv_file_preview$detected_plates) > 0)
    
    file_path <- rv_file_preview$file_path
    raw <- rv_file_preview$raw_data
    detected <- rv_file_preview$detected_plates
    
    # Determine which plates are selected
    selected_indices <- c()
    for (idx in seq_along(detected)) {
      checkbox_val <- input[[paste0("select_plate_", idx)]]
      if (isTRUE(checkbox_val)) {
        selected_indices <- c(selected_indices, idx)
      }
    }
    
    if (length(selected_indices) == 0) {
      showNotification("Please select at least one plate.", type = "warning")
      return()
    }
    
    # Extract selected plates
    selected_plates <- detected[selected_indices]
    
    if (length(selected_plates) == 1) {
      # Single plate: standard import
      rv$is_multiwavelength <- FALSE
      pl <- selected_plates[[1]]
      plate_data <- raw[pl$start_row:(pl$start_row + pl$nrows - 1),
                        pl$start_col:(pl$start_col + pl$ncols - 1), drop = FALSE]
      plate_numeric <- suppressWarnings(
        as.data.frame(apply(plate_data, 2, as.numeric), stringsAsFactors = FALSE)
      )
      plate_numeric <- enforce_plate_shape(plate_numeric)
      matrix_measresults(plate_numeric)
      showNotification("✅ Single plate imported from visual selection.", type = "message")
      
    } else {
      # Multiple plates: treat as multi-wavelength
      rv$is_multiwavelength <- TRUE
      plates <- list()
      for (idx in seq_along(selected_plates)) {
        pl <- selected_plates[[idx]]
        plate_data <- raw[pl$start_row:(pl$start_row + pl$nrows - 1),
                          pl$start_col:(pl$start_col + pl$ncols - 1), drop = FALSE]
        plate_numeric <- suppressWarnings(
          as.data.frame(apply(plate_data, 2, as.numeric), stringsAsFactors = FALSE)
        )
        plate_numeric <- enforce_plate_shape(plate_numeric)
        plates[[paste0("Plate_", idx)]] <- plate_numeric
      }
      
      rv$wavelength_plates <- plates
      rv$wavelengths <- names(plates)
      matrix_measresults(plates[[1]])
      
      showNotification(
        sprintf("✅ %d plates imported from visual selection.", length(plates)),
        type = "message"
      )
    }
  })
  
  observeEvent(input$clear_upload, {
    shinyjs::reset("upload_counts")
    matrix_measresults(create_plate_matrix())
    showNotification("File cleared", type = "message")
  })
  
  # Upload preview
  output$upload_summary <- renderUI({
    req(matrix_measresults())
    plate <- matrix_measresults()
    info <- base::attr(plate, "import_info")  # Use base R attr() to avoid xfun warning
    
    if (!is.null(info)) {
      # Check actual data - count non-NA wells
      actual_wells <- sum(!is.na(plate))
      is_partial <- actual_wells < 96  # True partial plate check
      
      div(
        style = "background-color: #E8F5E9; padding: 10px; margin: 10px 0; border-left: 4px solid #4CAF50;",
        tags$b("Import Summary:"),
        tags$ul(
          tags$li(sprintf("Format: %s", info$format)),
          tags$li(sprintf("Wells: %d / 96", actual_wells)),
          tags$li(sprintf("Partial: %s", if (is_partial) "Yes" else "No"))
        )
      )
    }
  })
  
  output$meas_preview <- renderTable({
    req(matrix_measresults())
    head(matrix_measresults(), 3)
  })
  
  # --------------------------------------------------------------------------
  # Sample Layout Modal
  # --------------------------------------------------------------------------
  
  observeEvent(input$show_sample_layout, {
    dot <- "<span style='color:#999;'>•</span>"
    sample_df <- data.frame(
      Row = LETTERS[1:8],
      matrix(rep(dot, 96), nrow = 8, ncol = 12),
      stringsAsFactors = FALSE
    )
    
    showModal(modalDialog(
      title = "Sample Plate Layout",
      size = "l",
      easyClose = TRUE,
      HTML("<p>Expected: Row labels (A–H) + 12 numeric columns.<br>
            Do not include column names.</p>"),
      HTML("<style>.sample_table thead { display: none; }</style>"),
      HTML(knitr::kable(sample_df, format = "html",
                       table.attr = "class='table table-bordered'",
                       escape = FALSE))
    ))
  })
  
  output$hill_warning <- renderUI({
    raw <- input$expected_hill
    if (is.null(raw) || trimws(raw) == "") {
      return(tags$div(style = "color: red; font-weight: bold;",
                     "⚠️ Hill slope required"))
    }
    val <- suppressWarnings(as.numeric(raw))
    if (is.na(val)) {
      return(tags$div(style = "color: red; font-weight: bold;",
                     "⚠️ Must be numeric"))
    }
    if (val < 0.5 || val > 1.5) {
      return(tags$div(style = "color: orange; font-weight: bold;",
                     "⚠️ Outside expected range (0.5–1.5)"))
    }
    NULL
  })
  
  output$dilution_error_feedback <- renderUI({
    if (dilution_error()) {
      tags$div(style = "color: red; font-weight: bold;",
              "⚠️ Invalid dilution entries (red cells)")
    }
  })
  
  # --------------------------------------------------------------------------
  # Convert Button Enable/Disable
  # --------------------------------------------------------------------------
  
  observe({
    assay <- input$assay_type %||% "rba"
    
    # File upload required
    file_ok <- !is.null(input$upload_counts)
    
    # Dilution validity
    dilution_ok <- !dilution_error()
    
    # QC validation - only required for RBA
    if (assay == "rba") {
      qc_ok <- !is.null(input$qc_conc) && input$qc_conc != "" &&
               !is.na(suppressWarnings(as.numeric(input$qc_conc)))
      hill_ok <- !is.null(input$expected_hill) && input$expected_hill != "" &&
                 !is.na(suppressWarnings(as.numeric(input$expected_hill)))
    } else {
      # ELISA: QC not required
      qc_ok <- TRUE
      hill_ok <- TRUE
    }
    
    # Enable button if all conditions met
    if (file_ok && dilution_ok && qc_ok && hill_ok) {
      shinyjs::enable("convert")
    } else {
      shinyjs::disable("convert")
    }
  })
  
  # --------------------------------------------------------------------------
  # Report Generation
  # --------------------------------------------------------------------------

  observeEvent(input$convert, {
    
    withProgress(message = "Generating report...", value = 0, {
      
      # Convert to long format
      df_long <- matrix_to_long(
        matrix_type(), matrix_id(), matrix_dilution(),
        matrix_replicate(), matrix_measresults(), std_conc()
      )
      
      incProgress(0.2)
      
      # Apply normalization based on assay type
      df_normalized <- tryCatch({
        
        if (input$assay_type == "elisa") {
          # ELISA: Apply %B/B0 normalization
          detection_method <- "absorbance"
          normalize_data(df_long, "elisa", detection_method)
        } else {
          # RBA: Direct measurement (CPM or RFU)
          detection_method <- input$detection_method %||% "radioligand"
          normalize_data(df_long, "rba", detection_method)
        }
        
      }, error = function(e) {
        
        # If normalization fails, show warning but continue with raw data
        showNotification(
          paste("Normalization warning:", e$message),
          type = "warning",
          duration = 8
        )
        
        # Return original data with basic normalization info
        df_long %>%
          mutate(
            NormalizedValue = MeasurementValue,
            ResponseUnit = if (input$assay_type == "elisa") "Absorbance" else "CPM"
          )
      })
      
      incProgress(0.4)
      
      # Save main CSV (for single wavelength OR first wavelength of multi-wavelength)
      csv_path <- Sys.getenv("RBA_CSV_PATH")
      write.csv(df_normalized, csv_path, row.names = FALSE)
      
      # NEW: If multi-wavelength, save additional wavelength CSVs
      if (isTRUE(rv$is_multiwavelength) && !is.null(rv$wavelengths)) {
        
        message(sprintf("Processing %d wavelengths...", length(rv$wavelengths)))
        
        # Save each wavelength as separate CSV
        for (wl in rv$wavelengths) {
          
          # Get the plate data for this wavelength
          plate_wl <- rv$wavelength_plates[[wl]]
          
          # Convert to long format using SAME plate layout
          # We use matrix_to_long with the plate data for this wavelength
          # but all other matrices (IDs, types, dilutions, etc.) stay the same!
          df_long_wl <- matrix_to_long(
            type_mat = matrix_type(),
            id_mat = matrix_id(),
            dilution_mat = matrix_dilution(),
            replicate_mat = matrix_replicate(),
            measurement_mat = plate_wl,  # <-- Different plate data!
            std_conc = std_conc()
          )
          
          # Apply normalization if ELISA
          df_normalized_wl <- tryCatch({
            
            if (input$assay_type == "elisa") {
              detection_method <- "absorbance"
              normalize_data(df_long_wl, "elisa", detection_method)
            } else {
              detection_method <- input$detection_method %||% "radioligand"
              normalize_data(df_long_wl, "rba", detection_method)
            }
            
          }, error = function(e) {
            
            # Fallback if normalization fails
            df_long_wl %>%
              mutate(
                NormalizedValue = MeasurementValue,
                ResponseUnit = if (input$assay_type == "elisa") "Absorbance" else "CPM"
              )
          })
          
          # Save with wavelength suffix
          csv_path_wl <- file.path(output_dir, paste0("long_data_output_", wl, ".csv"))
          write.csv(df_normalized_wl, csv_path_wl, row.names = FALSE)
          
          message(sprintf("Saved: %s", basename(csv_path_wl)))
        }
        
        # Save wavelength manifest
        write_json_safe(
          list(wavelengths = rv$wavelengths),
          file.path(output_dir, "wavelength_manifest.json")
        )
        
        showNotification(
          sprintf("Saved data for %d wavelengths", length(rv$wavelengths)),
          type = "message",
          duration = 5
        )
      }
      
      # Save formats
      write_json_safe(input$export_formats, Sys.getenv("RBA_FMT_JSON"))
      
      # Save notes
      write_json_safe(list(notes = input$notes %||% ""), 
                      Sys.getenv("RBA_NOTES_FILE"))
      
      # Save QC params (conditional based on assay type)
      qc_params <- if (input$assay_type == "rba") {
        list(
          qc_concentration = input$qc_conc,
          expected_hill = input$expected_hill,
          assay_type = input$assay_type,
          detection_method = input$detection_method,
          analyte = chosen_standard_label()
        )
      } else {
        # ELISA: Different QC approach
        list(
          assay_type = input$assay_type,
          detection_method = "absorbance",
          analyte = input$elisa_analyte,
          units = input$elisa_units %||% "pg/mL",
          normalization = "percent_b_b0"
        )
      }
      
      write_json_safe(qc_params, file.path(output_dir, "qc_params.json"))
      
      # Save assay configuration
      assay_config <- if (input$assay_type == "elisa") {
        list(
          assay_type = "elisa",
          analyte = input$elisa_analyte,
          units = input$elisa_units %||% "pg/mL",
          detection_method = "absorbance"
        )
      } else {
        list(
          assay_type = "rba",
          toxin_class = input$toxin_class,
          toxin_variant = input$toxin_variant %||% NA,
          toxin_standard_label = chosen_standard_label(),
          molecular_weight_g_mol = mw_g_mol(),
          detection_method = input$detection_method,
          units = "mol/L"
        )
      }
      
      write_json_safe(assay_config, file.path(output_dir, "assay_config.json"))
      
      # Save tissue weights (ELISA only)
      if (input$assay_type == "elisa") {
        tw <- tissue_weights_rv()
        if (length(tw) > 0) {
          write_json_safe(tw, file.path(output_dir, "tissue_weights.json"))
        }
        
        # Save extraction volume in sample processing config
        extraction_vol <- input$extraction_volume %||% 500
        processing_config <- list(
          extraction_volume_ul = extraction_vol,
          sample_type = "extracted",
          notes = "Set from Shiny app"
        )
        write_json_safe(processing_config, 
                       file.path(output_dir, "sample_processing_config.json"))
      }
      
      # Save excluded wells
      excluded_input <- input$excluded_wells_input
      if (!is.null(excluded_input) && nchar(trimws(excluded_input)) > 0) {
        wells <- trimws(strsplit(excluded_input, ",")[[1]])
        wells <- toupper(wells)
        write_json_safe(list(excluded_wells = wells),
                       file.path(output_dir, "excluded_wells.json"))
      }
      
      # Save report language preference
      write_json_safe(list(lang = input$report_language %||% "en"),
                     file.path(output_dir, "report_language.json"))
      
      incProgress(0.6)
      
      showNotification(paste("Data saved to:", csv_path), 
                       type = "message", duration = 5)
      
      incProgress(1)
      
      Sys.sleep(2)
      stopApp()  # <-- App ends here. Rendering happens in run_analysis_modular.R
    })
  })
      
  # --------------------------------------------------------------------------
  # Guided Tour
  # --------------------------------------------------------------------------
  
  tour_steps <- data.frame(
    element = c("#step0_section", "#matrix_type_section", "#matrix_id_section",
                "#matrix_dilution_section", "#matrix_replicate_section",
                "#tissue_weight_section", "#upload_section", "#visual_selector_section",
                "#language_toggle_section", "#convert_section"),
    intro = c(
      "Step 0: Configure your assay type and standard concentrations. Choose between RBA and ELISA, select the analyte, and enter standard concentrations.",
      "Step 1: Define the plate layout. Assign each well as Standard, Sample, QC, Blank, NSB, B0, etc.",
      "Assign sample IDs to each well. Standards are automatically labeled S1, S2, etc.",
      "Enter dilution factors for each well. Supports numeric values (0.5) and ratios (1:2).",
      "Define replicate groups. Wells with the same label are treated as replicates.",
      "For ELISA: enter tissue weights (mg) per replicate group and extraction volume for pg/g tissue calculations.",
      "Step 2: Upload your plate reader data file (.xlsx, .csv, or .txt).",
      "Use the Visual Plate Selector to preview the file and select plate regions. You can also exclude individual wells.",
      "Switch between English and Spanish for the interface and reports.",
      "Step 3: Choose report format(s) and language, then generate the report."
    ),
    stringsAsFactors = FALSE
  )
  
  introjs(session, options = list(
    nextLabel = "Next", prevLabel = "Back", skipLabel = "Exit",
    doneLabel = "Finish", showProgress = TRUE
  ))
  
  observeEvent(input$start_tour, {
    introjs(session, options = list(steps = tour_steps))
  })
}

# ============================================================================
# Run App
# ============================================================================

shinyApp(ui = ui, server = server)

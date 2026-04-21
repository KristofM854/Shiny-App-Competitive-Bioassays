# ==============================================================================
# Competitive Binding Assay Analysis App
# Authors: Arnold Molina Porras (UCR) & Kristof Moeller (IAEA)
# Version: 2.0
#
# Main Shiny application for RBA and ELISA competitive binding assay analysis.
# Supports:
#   - 96-well plate layout configuration (Sample Type, ID, Dilution, Replicates)
#   - Smart plate reader file import (.xlsx, .csv, .txt) with auto-detection
#   - Multi-wavelength data handling with concordance analysis
#   - ELISA %B/B0 normalization (Blank, NSB, B0 control wells)
#   - 4-parameter logistic (4PL) dose-response curve fitting
#   - Multiple DRC regression weightings (unweighted, 1/Y, 1/Y^2) for comparison
#   - Configurable quantification range (LLOQ/ULOQ) with %B/B0 bounds
#   - Bootstrap and t-distribution confidence intervals
#   - Outlier detection (Dixon's Q / Grubbs' test)
#   - Tissue weight normalization for pg/g tissue calculations
#   - Bilingual reports (EN/ES) in HTML and Word formats
#   - Guided tour (rintrojs) and plate layout save/load
#
# Architecture:
#   global.R              -> Shared packages, constants, theme
#   utils_plate.R         -> Plate matrix creation and conversion
#   utils_import_v3.R     -> Smart plate reader file import
#   utils_import_multiwavelength.R -> Multi-wavelength Excel parsing
#   utils_normalization.R -> ELISA %B/B0 normalization
#   i18n.R                -> Bilingual translation keys (480+)
#   server_common.R       -> Auto-save, navigation, language, guided tour
#   server_config.R       -> Tab 1: Assay config, standards, QC
#   server_layout.R       -> Tab 2: Matrix editors, presets, save/load, tissue weights
#   server_upload.R       -> Tab 3: File import, heatmap, visual selector
#   server_report.R       -> Tab 4-5: Validation, pre-flight, report generation
#   reports/              -> Rmd templates, report functions, plot functions
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

# Source modular server logic
source("server_common.R")
source("server_config.R")
source("layout_history.R")
source("server_layout.R")
source("server_upload.R")
source("report_pipeline.R")
source("server_report.R")

# Auto-generate preset .rds files if they don't exist
if (!file.exists("presets/rba_stx_triplicate.rds")) {
  tryCatch(source("presets/generate_presets.R"), error = function(e) {
    message("Could not generate presets: ", e$message)
  })
}

# Get output directory from environment (set by run_analysis_modular.R)
# If not set OR if set by a previous standalone run, create a fresh dated folder
standalone_mode <- (Sys.getenv("RBA_OUTPUT_DIR") == "" ||
                    Sys.getenv("RBA_STANDALONE") == "1")

if (standalone_mode) {
  # In standalone mode (runGitHub), let user pick their output folder.
  # Try native folder picker; fall back to Documents folder.
  app_root <- NULL

  # Try RStudio dialog
  if (is.null(app_root) && requireNamespace("rstudioapi", quietly = TRUE)) {
    app_root <- tryCatch(
      rstudioapi::selectDirectory(
        caption = "Select output folder for reports",
        label = "Select"
      ),
      error = function(e) NULL
    )
  }

  # Try Windows folder picker (base R)
  if (is.null(app_root) && .Platform$OS.type == "windows") {
    app_root <- tryCatch(
      utils::choose.dir(
        default = path.expand("~/Documents"),
        caption = "Select output folder for reports"
      ),
      error = function(e) NULL
    )
  }

  # Fall back to ~/Documents/App Competitive Bioassays
  if (is.null(app_root) || is.na(app_root)) {
    app_root <- file.path(path.expand("~"), "Documents", "App Competitive Bioassays")
    dir.create(app_root, showWarnings = FALSE, recursive = TRUE)
    message("Using default output location: ", app_root)
  }

  base_name <- format(Sys.Date(), "%Y-%m-%d")
  base_output_dir <- file.path(app_root, base_name)

  # Always create a unique directory - never reuse an existing one.
  # Check if the base dir or any suffixed variant already exists.
  if (!dir.exists(base_output_dir)) {
    output_dir <- base_output_dir
  } else {
    # Find next available suffix by scanning existing directories
    suffix <- 1
    repeat {
      candidate <- paste0(base_output_dir, "_", sprintf("%02d", suffix))
      if (!dir.exists(candidate)) {
        output_dir <- candidate
        break
      }
      suffix <- suffix + 1
      if (suffix > 999) {
        output_dir <- paste0(base_output_dir, "_", format(Sys.time(), "%H%M%S"))
        break
      }
    }
  }

  dir.create(output_dir, showWarnings = FALSE, recursive = TRUE)
  csv_path <- file.path(output_dir, "long_data_output.csv")
  Sys.setenv(RBA_OUTPUT_DIR = output_dir)
  Sys.setenv(RBA_CSV_PATH = normalizePath(csv_path, winslash = "/", mustWork = FALSE))
  Sys.setenv(RBA_FMT_JSON = normalizePath(file.path(output_dir, "selected_formats.json"), winslash = "/", mustWork = FALSE))
  Sys.setenv(RBA_NOTES_FILE = normalizePath(file.path(output_dir, "notes.json"), winslash = "/", mustWork = FALSE))
  Sys.setenv(RBA_STANDALONE = "1")
  message("Standalone mode - output directory: ", output_dir)
}
output_dir <- Sys.getenv("RBA_OUTPUT_DIR")

# ============================================================================
# UI DEFINITION
# ============================================================================

ui <- fluidPage(
  useShinyjs(),
  introjsUI(),
  shinyFeedback::useShinyFeedback(),
  tags$head(
    tags$link(rel = "stylesheet", type = "text/css", href = "style.css")
  ),

  uiOutput("app_title_ui"),
  br(),

  # ------------------------------
  # Language Toggle & Guided Tour
  # ------------------------------
  div(
    style = "display: flex; justify-content: flex-start; gap: 15px; margin-bottom: 20px; align-items: center;",
    actionButton("start_tour", "\U0001F680 Start Guided Tour",
                class = "btn btn-lg btn-info",
                style = "font-size: 18px; padding: 15px 30px;"),
    div(
      id = "language_toggle_section",
      style = "display: flex; align-items: center; gap: 8px;",
      tags$span("\U0001F310", style = "font-size: 20px;"),
      selectInput("app_language", NULL,
                  choices = c("English" = "en", "Español" = "es"),
                  selected = "en",
                  width = "130px")
    )
  ),

  # ==========================================================================
  # WIZARD-STYLE TABBED INTERFACE
  # ==========================================================================
  tabsetPanel(
    id = "wizard_tabs",
    type = "pills",

    # ======================================================================
    # TAB 1: Assay Configuration
    # ======================================================================
    tabPanel(
      "1. Configuration",
      value = "tab_config",
      br(),
      div(
        style = "display: flex; justify-content: flex-end; padding: 0 15px 10px;",
        actionButton("next_to_layout_top", "Next: Plate Layout \u2192",
                    class = "btn btn-primary btn-lg")
      ),

      # Quick Start panel
      div(
        id = "quickstart_section",
        style = "background: linear-gradient(135deg, #E3F2FD 0%, #F3E5F5 100%); padding: 20px; border-radius: 8px; margin-bottom: 20px;",
        h4(style = "margin-top: 0;", "Quick Start"),
        p("Choose a preset to auto-configure the assay type, plate layout, and standard concentrations:"),
        fluidRow(
          column(4,
            actionButton("qs_rba_stx", label = tagList(icon("flask"), " RBA Saxitoxin"),
                        class = "btn btn-lg btn-primary", style = "width: 100%; margin-bottom: 8px;")
          ),
          column(4,
            actionButton("qs_elisa_cortisol", label = tagList(icon("vial"), " ELISA Cortisol"),
                        class = "btn btn-lg btn-success", style = "width: 100%; margin-bottom: 8px;")
          ),
          column(4,
            actionButton("qs_elisa_custom", label = tagList(icon("cog"), " ELISA Custom"),
                        class = "btn btn-lg btn-default",
                        style = "width: 100%; margin-bottom: 8px; border: 2px solid #9C27B0; color: #9C27B0;")
          )
        ),
        tags$small(style = "color: #666;", "Or configure manually below.")
      ),

      introBox(
        div(
          id = "step0_section",
          column(12, uiOutput("step0_header")),

          fluidRow(
            column(
              width = 6,

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
                    "Estradiol" = "estradiol",
                    "Custom" = "custom"
                  ),
                  selected = "cortisol"
                ),

                conditionalPanel(
                  condition = "input.elisa_analyte == 'custom'",
                  textInput("elisa_custom_name", "Custom analyte name:",
                           placeholder = "e.g., Estradiol")
                ),

                selectInput(
                  "elisa_units",
                  "Standard concentration units:",
                  choices = c(
                    "pg/mL" = "pg/mL",
                    "ng/mL" = "ng/ml",
                    "\u00b5g/mL" = "ug/ml"
                  ),
                  selected = "pg/mL"
                )
              ),

              # Standard concentrations (shown for both assay types)
              hr(),
              p(tags$b("Standard Concentrations")),
              p("Specify the number of standards, then enter each concentration."),

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
      div(
        style = "text-align: right; padding: 15px;",
        actionButton("next_to_layout", "Next: Plate Layout \u2192",
                    class = "btn btn-primary btn-lg")
      )
    ),

    # ======================================================================
    # TAB 2: Plate Layout
    # ======================================================================
    tabPanel(
      "2. Plate Layout",
      value = "tab_layout",
      br(),

      div(
        id = "step1_section",
        style = "padding-bottom: 40px;",

        uiOutput("step1_header"),
        div(
          style = "display: flex; justify-content: space-between; padding: 0 15px 10px;",
          actionButton("back_to_config_top", "\u2190 Back: Configuration",
                      class = "btn btn-default btn-lg"),
          actionButton("next_to_upload_top", "Next: Upload & Preview \u2192",
                      class = "btn btn-primary btn-lg")
        ),

        # Preset plate layouts + layout management
        div(
          id = "preset_layout_section",
          style = "background-color: #E8F5E9; padding: 12px; margin: 10px 0; border-left: 4px solid #4CAF50; border-radius: 4px;",
          fluidRow(
            column(3,
              selectInput("preset_layout", "Load Preset Layout:",
                         choices = c(
                           "-- Select Preset --" = "",
                           "RBA: STX 8 standards (triplicate)" = "rba_stx_triplicate",
                           "ELISA: Cortisol (Cayman kit, 8-point, duplicate)" = "elisa_cortisol_cayman",
                           "ELISA: Custom (blank template)" = "elisa_custom_blank"
                         ),
                         selected = "")
            ),
            column(3,
              fileInput("layout_import_file", "Import Layout (CSV/Excel):",
                        accept = c(".csv", ".xlsx", ".xls"),
                        width = "100%")
            ),
            column(2,
              div(style = "margin-top: 25px;",
                actionButton("layout_save", label = tagList(icon("save"), "Save Layout"),
                            class = "btn btn-success btn-sm", style = "width: 100%;")
              )
            ),
            column(2,
              div(style = "margin-top: 25px;",
                uiOutput("layout_load_ui")
              )
            ),
            column(2,
              div(style = "margin-top: 25px; display: flex; gap: 6px;",
                actionButton("undo_layout", label = tagList(icon("undo"), "Undo"),
                            class = "btn btn-default btn-sm"),
                actionButton("redo_layout", label = tagList(icon("redo"), "Redo"),
                            class = "btn btn-default btn-sm")
              )
            )
          )
        ),
        br(),

        # ---- MATRIX PAIRS (CSS Grid) ----
        div(
          class = "matrix-pairs",

          # === Pair 1: Sample Type | Sample ID ===
          tags$section(
            class = "matrix-pair",

            # Left: Sample Type
            div(
              id = "matrix_type_section", role = "grid", `aria-label` = "Sample type matrix: 8 rows by 12 columns",
              h5("1. Sample Type"),
              actionButton("reset_type", "Reset", class = "btn btn-xs"),

              conditionalPanel(
                condition = "input.assay_type == 'elisa'",
                div(
                  style = "background-color: #FFF9E6; padding: 8px; margin: 8px 0; border-left: 4px solid #FFC107; font-size: 12px;",
                  tags$b("ELISA Controls: "),
                  "Blank | NSB | B0 | TotalActivity (col 1)"
                )
              ),

              shinycssloaders::withSpinner(rHandsontableOutput("matrix_type"), type = 6, color = "#1976D2")
            ),

            # Right: Sample ID
            div(
              id = "matrix_dilution_section", role = "grid", `aria-label` = "Dilution fraction matrix: 8 rows by 12 columns",
              class = "matrix-bottom-cell",
              uiOutput("dilution_matrix_header"),
              div(
                style = "display: flex; align-items: center; gap: 10px; margin-bottom: 6px;",
                div(
                  style = "display: flex; align-items: center; gap: 6px;",
                  uiOutput("dilution_set_all_label", inline = TRUE),
                  tags$input(type = "number", id = "uniform_dilution", value = "1",
                             min = "0", step = "0.1", class = "form-control",
                             style = "width: 70px; height: 30px; padding: 2px 6px;")
                ),
                actionButton("apply_uniform_dilution", "Apply",
                             class = "btn btn-sm btn-info",
                             style = "height: 30px; padding: 2px 12px;"),
                checkboxInput("advanced_dilution", "Per-well", value = TRUE)
              ),
              conditionalPanel(
                condition = "input.advanced_dilution == true",
                div(`aria-live` = "polite", uiOutput("dilution_error_feedback")),
                div(`aria-live` = "polite", uiOutput("dilution_gt1_warning")),
                actionButton("reset_dilution", "Reset", class = "btn btn-xs"),
                div(class = "matrix-table-anchor",
                  shinycssloaders::withSpinner(rHandsontableOutput("matrix_dilution"), type = 6, color = "#1976D2")),
                uiOutput("dilution_matrix_help")
              )
            )
          ),

          # === Pair 2: Dilution Factors | Replicate Groups ===
          tags$section(
            class = "matrix-pair",

            # Left: Dilution Factors
            div(
              id = "matrix_id_section", role = "grid", `aria-label` = "Sample ID matrix: 8 rows by 12 columns",
              h5("2. Sample ID"),
              actionButton("reset_id", "Reset", class = "btn btn-xs"),
              shinycssloaders::withSpinner(rHandsontableOutput("matrix_id"), type = 6, color = "#1976D2")
            ),

            # Right: Replicate Groups
            div(
              id = "matrix_replicate_section", role = "grid", `aria-label` = "Replicate group matrix: 8 rows by 12 columns",
              class = "matrix-bottom-cell",
              h5("4. Replicate Groups"),
              actionButton("reset_replicate", "Reset", class = "btn btn-xs"),
              div(class = "matrix-table-anchor",
                shinycssloaders::withSpinner(rHandsontableOutput("matrix_replicate"), type = 6, color = "#1976D2"))
            )
          )
        ),
        br(),

        # QC fields (RBA only) — below the matrix grid
        conditionalPanel(
          condition = "input.assay_type == 'rba'",
          div(
            id = "qc_section",
            # Narrow the section so the guided-tour highlight wraps tightly
            # around the two input fields rather than the full page width.
            style = "max-width: 520px;",
            h5("5. Quality Control Parameters"),
            div(
              style = "display: flex; gap: 20px; align-items: flex-start;",
              div(style = "flex: 0 0 220px;", uiOutput("qc_concentration_input")),
              div(style = "flex: 0 0 220px;", textInput("expected_hill", "Expected Hill slope:",
                                                         value = "1", placeholder = "1"))
            ),
            uiOutput("qc_warnings"),
            uiOutput("hill_warning")
          )
        ),
        br(),

        # Tissue weight + extraction volume (ELISA only)
        conditionalPanel(
          condition = "input.assay_type == 'elisa'",
          div(
            id = "tissue_weight_section",
            h5("6. Tissue Weights & Extraction Volume (optional)"),
            div(
              style = "background-color: #FFF3E0; padding: 8px; margin: 8px 0; border-left: 4px solid #FF9800; font-size: 12px;",
              tags$b("Tissue-based calculation: "),
              "Enter tissue weight (mg) and extraction volume (\u00b5L) per replicate group. ",
              "Leave blank if not applicable. Default extraction volume: 500 \u00b5L."
            ),
            div(
              style = "display: flex; align-items: center; gap: 10px; margin-bottom: 6px;",
              div(
                style = "display: flex; align-items: center; gap: 6px;",
                tags$label("Set all extraction vol to:", `for` = "uniform_extraction",
                           style = "margin: 0; white-space: nowrap; font-size: 12px;"),
                tags$input(type = "number", id = "uniform_extraction", value = "500",
                           min = "0", step = "10", class = "form-control",
                           style = "width: 80px; height: 30px; padding: 2px 6px;")
              ),
              actionButton("apply_uniform_extraction", "Apply",
                           class = "btn btn-sm btn-info",
                           style = "height: 30px; padding: 2px 12px;")
            ),
            rHandsontableOutput("tissue_weight_table"),
            tags$small(style = "color: #888;", "Scroll right if more groups are present."),
            uiOutput("extraction_volume_help")
          )
        )
      ),
      br(),
      div(
        style = "display: flex; justify-content: space-between; padding: 15px;",
        actionButton("back_to_config", "\u2190 Back: Configuration",
                    class = "btn btn-default btn-lg"),
        actionButton("next_to_upload", "Next: Upload & Preview \u2192",
                    class = "btn btn-primary btn-lg")
      )
    ),

    # ======================================================================
    # TAB 3: Upload & Preview
    # ======================================================================
    tabPanel(
      "3. Upload & Preview",
      value = "tab_upload",
      br(),

      div(
        id = "upload_section",
        uiOutput("step2_header"),
        div(
          style = "display: flex; justify-content: space-between; padding: 0 15px 10px;",
          actionButton("back_to_layout_top", "\u2190 Back: Plate Layout",
                      class = "btn btn-default btn-lg"),
          actionButton("next_to_analysis_top", "Next: Analysis Settings \u2192",
                      class = "btn btn-primary btn-lg")
        ),

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
        downloadButton("download_plate_template", "Download Example File",
                       class = "btn btn-default btn-sm", style = "margin-top: 5px;"),

        # Visual plate selector panel
        conditionalPanel(
          condition = "input.import_method == 'visual'",
          div(
            id = "visual_selector_section",
            style = "border: 2px dashed #2196F3; padding: 15px; margin: 10px 0; border-radius: 8px;",
            h5(tags$b("Visual Plate Selector")),
            p("After uploading a file, a preview will appear below. Click and drag to select 8\u00D712 plate regions."),
            uiOutput("visual_file_preview"),
            uiOutput("visual_plate_selections"),
            uiOutput("visual_well_exclusion")
          )
        ),

        actionButton("show_sample_layout", "Show default plate layout",
                    class = "btn btn-sm btn-secondary",
                    style = "margin-top: -10px;")
      ),

      div(id = "upload_preview_section",
          uiOutput("upload_summary"),
          tableOutput("meas_preview")),
      br(),

      # Data heatmap preview
      tags$figure(
        id = "heatmap_preview_section",
        style = "max-width: 700px;",
        `aria-describedby` = "plate_heatmap_desc_text",
        role = "figure",
        h5("Plate Data Heatmap"),
        tags$figcaption(id = "plate_heatmap_desc_text",
          "Visual verification of uploaded plate data. Wells are colored by measurement value."
        ),
        plotly::plotlyOutput("plate_heatmap", height = "300px"),
        div(class = "sr-only", `aria-live` = "polite", textOutput("plate_heatmap_description"))
      ),
      br(),
      div(
        style = "display: flex; justify-content: space-between; padding: 15px;",
        actionButton("back_to_layout", "\u2190 Back: Plate Layout",
                    class = "btn btn-default btn-lg"),
        actionButton("next_to_analysis", "Next: Analysis Settings \u2192",
                    class = "btn btn-primary btn-lg")
      )
    ),

    # ======================================================================
    # TAB 4: Analysis Settings
    # ======================================================================
    tabPanel(
      "4. Analysis Settings",
      value = "tab_analysis",
      br(),
      div(
        style = "display: flex; justify-content: space-between; padding: 0 15px 10px;",
        actionButton("back_to_upload_top", "\u2190 Back: Upload & Preview",
                    class = "btn btn-default btn-lg"),
        actionButton("next_to_report_top", "Next: Generate Report \u2192",
                    class = "btn btn-primary btn-lg")
      ),

      div(
        id = "analysis_settings_section",
        style = "max-width: 700px;",
        h4(id = "analysis_settings_title", "Analysis Settings"),

        # Primary setting (always visible)
        checkboxGroupInput("regression_weight", "DRC regression weighting:",
                   choices = c("Unweighted" = "none",
                               "1/Y (moderate)" = "inv_y",
                               "1/Y\u00B2 (recommended for immunoassays)" = "inv_y2"),
                   selected = "none"),
        helpText("Select multiple weightings to compare results side by side."),

        # Advanced analysis options — open by default for visibility
        div(
          style = paste0(
            "margin-top: 18px; padding: 14px 16px; background: #FFF8E1; ",
            "border-radius: 6px; border-left: 4px solid #FFA000; ",
            "box-shadow: 0 1px 3px rgba(0,0,0,0.08);"
          ),
          h4(style = "margin: 0 0 4px 0; color: #E65100;",
             icon("sliders-h"), " Advanced Options \u2014 weighting, CI, outliers, and QC thresholds"),
          tags$p(style = "margin: 0 0 12px 0; color: #555; font-size: 13px;",
            "These settings control confidence interval method, outlier detection, quantification range, and quality thresholds."
          ),

          fluidRow(
            column(6,
              numericInput("quant_range_min", "Lower %B/B0 bound:",
                          value = 20, min = 5, max = 50, step = 5)
            ),
            column(6,
              numericInput("quant_range_max", "Upper %B/B0 bound:",
                          value = 80, min = 50, max = 95, step = 5)
            )
          ),
          helpText("Samples outside this range are flagged as <LLOQ or >ULOQ."),

          hr(),

          radioButtons("ci_method", "Confidence interval method:",
                      choices = c("t-distribution (default)" = "t_dist",
                                  "Bootstrap (1000 resamples)" = "bootstrap"),
                      selected = "t_dist", inline = TRUE),

          checkboxInput("enable_outlier_detection", "Enable outlier detection", value = FALSE),
          conditionalPanel(
            condition = "input.enable_outlier_detection == true",
            numericInput("outlier_min_n", "Minimum replicates for outlier test:",
                        value = 3, min = 3, max = 10, step = 1),
            helpText("Dixon's Q-test for n=3-5, Grubbs' test for n\u22656. Outliers are flagged, not removed."),
            hr(),
            radioButtons("normality_assumption", "Normality assumption for outlier detection:",
                        choices = c("Assume normality (default)" = "assume",
                                    "Test with Shapiro-Wilk" = "test_shapiro"),
                        selected = "assume"),
            conditionalPanel(
              condition = "input.normality_assumption == 'test_shapiro'",
              helpText("Shapiro-Wilk test is run on each replicate group. If p < 0.05 (non-normal), MAD-based detection replaces Grubbs' test.")
            )
          ),
          hr(),
          numericInput("cv_limit", "Maximum CV for standards (%):",
                      value = 30, min = 5, max = 50, step = 5),
          helpText("Standards exceeding this CV% threshold are flagged as high-variability.")
        )
      ),
      br(),
      div(
        style = "display: flex; justify-content: space-between; padding: 15px;",
        actionButton("back_to_upload", "\u2190 Back: Upload & Preview",
                    class = "btn btn-default btn-lg"),
        actionButton("next_to_report", "Next: Generate Report \u2192",
                    class = "btn btn-primary btn-lg")
      )
    ),

    # ======================================================================
    # TAB 5: Generate Report
    # ======================================================================
    tabPanel(
      "5. Generate Report",
      value = "tab_report",
      br(),
      div(
        style = "display: flex; justify-content: flex-start; padding: 0 15px 10px;",
        actionButton("back_to_analysis_top", "\u2190 Back: Analysis Settings",
                    class = "btn btn-default btn-lg")
      ),

      fluidRow(
        column(8,
          # Report generation
          div(
            id = "convert_section",
            h4("Report Output"),
            checkboxGroupInput("export_formats", "Report formats:",
                              choices = c("HTML" = "html", "Word (DOCX)" = "docx", "PDF" = "pdf"),
                              selected = "html"),
            tags$small(class = "text-muted", style = "display: block; margin-top: 4px; line-height: 1.5;",
              "HTML reports have interactive plots. Word and PDF use static figures.",
              tags$br(),
              "PDF requires a LaTeX engine (e.g. TinyTeX). If unavailable, the app will fall back to HTML."),
            selectInput("report_language", "Report language:",
                       choices = c("English" = "en", "Espa\u00f1ol" = "es"),
                       selected = "en",
                       width = "200px"),
            br(),
            actionButton("convert",
                        label = tagList(icon("file-arrow-down"),
                                       "Generate Report"),
                        class = "btn btn-primary btn-lg",
                        style = "width: 100%; font-size: 20px; font-weight: 700;
                                padding: 14px; border-radius: 12px;"),
            br(), br(),
            downloadButton("download_report", "Download Last Report",
                          class = "btn btn-success btn-lg",
                          style = "width: 100%;")
          )
        ),
        column(4,
          # Notes & Feedback
          div(
            id = "notes_feedback_section",
            h4("Notes & Feedback"),
            textAreaInput("notes", "Notes (optional) - will appear in the report:",
                         value = "", placeholder = "Observations, sample info, run notes...",
                         rows = 8),
            br(),
            tags$a(href = "https://forms.office.com/e/q8eqJfp4QM",
                  target = "_blank", class = "btn btn-info btn-block",
                  icon("comment"), " Give Feedback")
          )
        )
      ),
      br(),

      # Pre-Flight Check panel (full-width below the two columns)
      div(
        id = "preflight_section",
        style = "background-color: #FFF8E1; padding: 15px; border-radius: 8px; border-left: 4px solid #FFC107; margin-bottom: 15px;",
        h5(style = "margin-top: 0;", "Pre-Flight Check"),
        div(`aria-live` = "polite", uiOutput("preflight_checks"))
      ),
      br(),
      div(
        style = "text-align: left; padding: 15px;",
        actionButton("back_to_analysis", "\u2190 Back: Analysis Settings",
                    class = "btn btn-default btn-lg")
      )
    )
  )
)

# ============================================================================
# SERVER LOGIC
# ============================================================================

server <- function(input, output, session) {

  # --------------------------------------------------------------------------
  # Shared Reactive State (passed to all server_*.R functions)
  # --------------------------------------------------------------------------

  shared <- list(
    # Plate layout matrices
    matrix_type        = reactiveVal(),
    matrix_id          = reactiveVal(),
    matrix_dilution    = reactiveVal(create_dilution_matrix()),
    matrix_replicate   = reactiveVal(create_replicate_matrix("rba")),
    matrix_measresults = reactiveVal(create_plate_matrix()),

    # Dilution parsing state
    raw_matrix_dilution = reactiveVal(default_raw_dilution()),
    dilution_validity   = reactiveVal(matrix(TRUE, nrow = 8, ncol = 12)),
    dilution_error      = reactiveVal(FALSE),

    # Molecular weight (RBA only)
    mw_g_mol = reactiveVal(299.29),

    # Multi-wavelength state
    rv = reactiveValues(
      is_multiwavelength = FALSE,
      wavelengths = NULL,
      wavelength_plates = NULL
    ),

    # Tissue weights (ELISA only)
    tissue_weights_rv = reactiveVal(list())
  )

  # Track previous assay config to avoid unnecessary matrix resets
  prev_assay_type <- reactiveVal(NULL)
  prev_num_standards <- reactiveVal(NULL)

  # Session-scoped paths (replaces Sys.setenv/getenv for concurrency safety)
  session$userData$output_dir <- Sys.getenv("RBA_OUTPUT_DIR")
  session$userData$csv_path <- Sys.getenv("RBA_CSV_PATH")
  session$userData$fmt_json <- Sys.getenv("RBA_FMT_JSON")
  session$userData$notes_file <- Sys.getenv("RBA_NOTES_FILE")


  # --------------------------------------------------------------------------
  # Modular Server Logic
  # --------------------------------------------------------------------------
  # Server logic is split across 5 files for maintainability.
  # Each function receives input, output, session, and the shared state.

  config_reactives <- server_config(input, output, session, shared)
  server_layout(input, output, session, shared)
  server_upload(input, output, session, shared)
  server_report(input, output, session, shared, config_reactives)
  server_common(input, output, session, shared)
}

# ============================================================================
# Run App
# ============================================================================

shinyApp(ui = ui, server = server)

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
source("utils/utils_import_v3.R")
source("utils/utils_plate.R")
source("utils/utils_import_multiwavelength.R")
source("utils/utils_normalization.R")
source("server/i18n.R")

# Source modular server logic
source("server/server_common.R")
source("server/server_config.R")
source("server/layout_history.R")
source("server/server_layout.R")
source("server/server_upload.R")
source("server/report_pipeline.R")
source("server/server_report.R")

# Auto-generate preset .rds files if they don't exist
if (!file.exists("presets/rba_stx_triplicate.rds")) {
  tryCatch(source("presets/generate_presets.R"), error = function(e) {
    message("Could not generate presets: ", e$message)
  })
}

# Get output directory from environment (set by an external wrapper, if any).
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
    # GUI refresh: IBM Plex font stack (Sans for body / labels / buttons,
    # Mono for numbers / hex / file names, Serif for the report title only).
    tags$link(rel = "preconnect", href = "https://fonts.googleapis.com"),
    tags$link(rel = "preconnect", href = "https://fonts.gstatic.com",
              crossorigin = NA),
    tags$link(rel = "stylesheet",
              href = "https://fonts.googleapis.com/css2?family=IBM+Plex+Mono:wght@400;500;600&family=IBM+Plex+Sans:wght@400;500;600&family=IBM+Plex+Serif:wght@400;500&display=swap"),
    tags$link(rel = "stylesheet", type = "text/css",
              href = paste0("style.css?v=", as.numeric(Sys.time())))
  ),

  div(
    class = "bs-topbar",
    div(
      class = "brand",
      div(class = "mono-tile", "B"),
      span(class = "wordmark", "Bioassay Suite"),
      span(class = "version", "v0.9.0")
    ),
    div(
      class = "actions",
      # Hand-rolled <button> so we can control the class list precisely.
      # Keeps id="start_tour" and the action-button class so the existing
      # observeEvent(input$start_tour, ...) handler still fires.
      tags$button(
        id = "start_tour",
        type = "button",
        class = "action-button bs-quiet-btn",
        "Guided tour"
      ),
      tags$a(
        href = "https://github.com/KristofM854/Shiny-App-Competitive-Bioassays",
        target = "_blank",
        rel = "noopener noreferrer",
        class = "bs-quiet-btn",
        "Docs"
      ),
      div(class = "divider"),
      # Wrap selectInput in a span we can target. The .bs-lang-select
      # class lets the CSS un-block-ify Shiny's .shiny-input-container
      # without affecting any other selectInput in the app.
      div(
        class = "bs-lang-select",
        selectInput("app_language", NULL,
                    choices = c(
                      "English"          = "en",
                      "Español"          = "es",
                      "Français"         = "fr",
                      "Русский (бета)"   = "ru",
                      "中文 (测试版)"     = "zh"
                    ),
                    selected = "en",
                    width = "140px")
      )
    )
  ),

  tags$script(HTML("
$(function() {
  // Stamp data-step on each pill <a> for accessibility (screen readers
  // can announce 'Step 02') and for testing. The CSS counter is the
  // visual source of truth, so this script can't visually break the
  // stepper anymore.
  function stampSteps() {
    $('.nav-pills > li').each(function(i) {
      var n = (i + 1).toString().padStart(2, '0');
      $(this).children('a').attr('data-step', n);
    });
  }
  stampSteps();

  // Mark steps as done when the user advances past them.
  // Listen on the document so the handler survives Shiny re-renders.
  $(document).on('shown.bs.tab', '.nav-pills > li > a', function(e) {
    stampSteps();
    var $items = $('.nav-pills > li');
    var activeIdx = $items.index($(this).closest('li'));
    $items.each(function(i) {
      $(this).toggleClass('is-done', i < activeIdx);
      $(this).toggleClass('is-current', i === activeIdx);
    });
  });

  // Re-stamp after any Shiny output finishes rendering, in case the
  // tabset got rebuilt (defensive — the CSS counter handles the
  // visual side regardless).
  $(document).on('shiny:value', function() { stampSteps(); });
});
")),

  # v3 §3 tile i18n — walks [data-i18n] attrs and swaps textContent only,
  # so the card structure (rail + body) is never touched by the server.
  tags$script(HTML("
Shiny.addCustomMessageHandler('bs_tile_i18n', function(msg) {
  Object.keys(msg).forEach(function(key) {
    document.querySelectorAll('[data-i18n=\"' + key + '\"]').forEach(function(el) {
      el.textContent = msg[key];
    });
  });
});
")),

  # v3.2 §7.3 — Beta banner (shown for RU/ZH; dismissed per-language to localStorage)
  div(
    id = "bs-beta-banner",
    class = "bs-beta-banner",
    style = "display: none;",
    span(class = "icon", icon("exclamation-triangle")),
    span(class = "msg", textOutput("beta_warning_text", inline = TRUE)),
    tags$button(
      type = "button",
      class = "bs-beta-dismiss",
      onclick = "dismissBetaBanner()",
      "×"
    )
  ),

  tags$script(HTML("
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
")),

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
      "Configuration",
      value = "tab_config",
      br(),
      div(
        style = "display: flex; justify-content: flex-end; padding: 0 15px 10px;",
        actionButton("next_to_layout_top", "Next: Plate Layout \u2192",
                    class = "btn btn-primary btn-lg")
      ),

      # GUI refresh v3 §3 — Quick Start as three hand-rolled <a> tiles.
      # Hand-rolled (not actionLink) so we control the class list and
      # the inner DOM exactly. Keeps the action-button class + id so
      # observeEvent(input$qs_*, ...) handlers still fire.
      div(
        id = "quickstart_section",
        class = "bs-card",
        h3("Quick Start"),
        p(class = "bs-card-sub",
          "Choose a preset to auto-configure the assay type, plate layout, ",
          "and standard concentrations."),

        div(
          class = "bs-quickstart",

          tags$a(
            id = "qs_rba_stx_demo",
            href = "#",
            class = "action-button preset-tile",
            onclick = "return false;",
            div(class = "rail rail-blue"),
            div(
              class = "body",
              div(class = "title", `data-i18n` = "qs_tile_rba_title",
                  "RBA · Saxitoxin"),
              div(class = "sub", `data-i18n` = "qs_tile_rba_sub",
                  "Receptor binding assay · 8 standards · triplicate"),
              div(class = "meta", `data-i18n` = "qs_tile_demo_meta",
                  "Demo data included")
            )
          ),

          tags$a(
            id = "qs_elisa_cortisol_demo",
            href = "#",
            class = "action-button preset-tile",
            onclick = "return false;",
            div(class = "rail rail-green"),
            div(
              class = "body",
              div(class = "title", `data-i18n` = "qs_tile_elisa_co_title",
                  "ELISA · Cortisol"),
              div(class = "sub", `data-i18n` = "qs_tile_elisa_co_sub",
                  "Competitive ELISA · 7 standards · duplicate"),
              div(class = "meta", `data-i18n` = "qs_tile_demo_meta",
                  "Demo data included")
            )
          ),

          tags$a(
            id = "qs_elisa_custom_manual",
            href = "#",
            class = "action-button preset-tile",
            onclick = "return false;",
            div(class = "rail rail-purple"),
            div(
              class = "body",
              div(class = "title", `data-i18n` = "qs_tile_elisa_cu_title",
                  "ELISA · Custom"),
              div(class = "sub", `data-i18n` = "qs_tile_elisa_cu_sub",
                  "Blank ELISA template · configure from scratch"),
              div(class = "meta", `data-i18n` = "qs_tile_no_demo_meta",
                  "No demo data")
            )
          )
        ),

        # Manual-configure fallbacks for STX and Cortisol stay in a
        # quiet disclosure to avoid cluttering the primary path.
        tags$details(
          class = "bs-quickstart-more",
          tags$summary("More options — configure manually without demo data"),
          div(
            style = "padding-top: 10px; display: flex; gap: 8px; flex-wrap: wrap;",
            actionButton("qs_rba_stx_manual",
                         "RBA Saxitoxin — manual",
                         class = "btn-default"),
            actionButton("qs_elisa_cortisol_manual",
                         "ELISA Cortisol — manual",
                         class = "btn-default")
          )
        )
      ),

      # GUI refresh v2 §3.2 — Assay & Standards section. Single bs-card
      # holding two bs-section-row blocks. Each row has a left
      # label-col (heading + 1-line description) and a right input-col
      # (the controls). Replaces the previous half-fluidRow blue
      # wellPanel layout. All input IDs preserved.
      introBox(
        div(
          id = "step0_section",
          class = "bs-card",

          # ---- Row 1: Assay type ----
          div(
            class = "bs-section-row",
            div(
              class = "label-col",
              h4("Assay type"),
              p("Receptor binding or competitive ELISA \u2014 determines which curve model and toxin standards apply.")
            ),
            div(
              class = "input-col",
              selectInput(
                "assay_type", NULL,
                choices = c(
                  "Receptor Binding Assay (RBA)"              = "rba",
                  "ELISA (Enzyme-Linked Immunosorbent Assay)" = "elisa"
                ),
                selected = "rba"
              ),
              uiOutput("assay_description"),
              conditionalPanel(
                condition = "input.assay_type == 'rba'",
                selectInput(
                  "toxin_class", "Toxin standard used:",
                  choices = c("Saxitoxin", "Brevetoxin", "Ciguatoxin", "Custom"),
                  selected = "Saxitoxin"
                ),
                uiOutput("toxin_variant_ui"),
                conditionalPanel(
                  condition = "input.toxin_class == 'Custom'",
                  textInput("toxin_custom_name", "Custom standard name:",
                           placeholder = "e.g., GTX2/3 mix")
                ),
                uiOutput("mw_box_ui")
              ),
              conditionalPanel(
                condition = "input.assay_type == 'elisa'",
                selectInput(
                  "elisa_analyte", "Analyte:",
                  choices = c(
                    "Cortisol"     = "cortisol",
                    "Testosterone" = "testosterone",
                    "Estradiol"    = "estradiol",
                    "Custom"       = "custom"
                  ),
                  selected = "cortisol"
                ),
                conditionalPanel(
                  condition = "input.elisa_analyte == 'custom'",
                  textInput("elisa_custom_name", "Custom analyte name:",
                           placeholder = "e.g., Estradiol")
                ),
                selectInput(
                  "elisa_units", "Standard concentration units:",
                  choices = c(
                    "pg/mL" = "pg/mL",
                    "ng/mL" = "ng/ml",
                    "\u00b5g/mL" = "ug/ml"
                  ),
                  selected = "pg/mL"
                )
              )
            )
          ),

          # ---- Row 2: Standard concentrations ----
          div(
            class = "bs-section-row",
            div(
              class = "label-col",
              h4("Standard concentrations"),
              p("Specify the number of standards, then enter each concentration in mol/L using scientific notation (e.g. 1e-6, 3e-8)."),
              uiOutput("concentration_unit_guidance")
            ),
            div(
              class = "input-col",
              selectInput("num_standards", "Number of standards:",
                         choices = 0:12, selected = 8),
              div(
                class = "bs-std-grid",
                uiOutput("std_inputs")
              ),
              uiOutput("std_error_feedback")
            )
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
      "Plate Layout",
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
              uiOutput("layout_import_file_ui")
            ),
            column(2,
              div(style = "margin-top: 25px;",
                actionButton("layout_save", label = "Save Layout",
                            icon  = icon("save"),
                            class = "btn btn-success btn-sm", style = "width: 100%;")
              )
            ),
            column(2,
              div(style = "margin-top: 25px;",
                uiOutput("layout_load_ui")
              )
            )
          )
        ),
        br(),

        # GUI refresh v2 §4 — Plate Layout layer switcher (verbatim
        # from spec). Two-pane shell: bs-layer-switcher on the left
        # (240 px) holding a radioButtons() group whose choiceNames
        # carry data-step spans (the CSS reads them via
        # selectors targeting span[data-step]) plus an adaptive
        # bs-layer-bulk panel that shows the right control set per
        # active layer. bs-matrix-viewport on the right holds the
        # four conditionalPanel-wrapped matrix divs.
        # All matrix_*_section IDs preserved; reactives untouched.
        div(
          class = "bs-layout-shell",

          # ---- Left pane: layer switcher + bulk action ----
          div(
            class = "bs-layer-switcher",
            h5("Layers"),
            radioButtons(
              "active_layer", NULL,
              choiceNames = list(
                tagList(span(`data-step` = "01"), span("Sample Type")),
                tagList(span(`data-step` = "02"), span("Sample ID")),
                tagList(span(`data-step` = "03"), span("Dilution")),
                tagList(span(`data-step` = "04"), span("Replicates"))
              ),
              choiceValues = c("type", "id", "dilution", "rep"),
              selected = "type"
            ),
            div(
              class = "bs-layer-bulk",
              h6("Bulk action"),
              conditionalPanel(
                "input.active_layer == 'type'",
                selectInput("bulk_type", NULL,
                            choices = c("Standard", "Sample", "Blank", "NSB", "B0")),
                actionButton("apply_uniform_type", "Set all wells",
                             class = "btn-default btn-block")
              ),
              conditionalPanel(
                "input.active_layer == 'id'",
                textInput("bulk_id_prefix", NULL, placeholder = "S"),
                actionButton("apply_uniform_id", "Set all wells",
                             class = "btn-default btn-block")
              ),
              conditionalPanel(
                "input.active_layer == 'dilution'",
                # Existing apply_uniform_dilution observer wired in
                # server_layout.R; numericInput keeps its widget ID.
                numericInput("uniform_dilution", NULL,
                             value = 1, min = 0, max = 1, step = 0.01),
                actionButton("apply_uniform_dilution", "Set all wells",
                             class = "btn-default btn-block")
              ),
              conditionalPanel(
                "input.active_layer == 'rep'",
                textInput("bulk_rep_group", NULL, placeholder = "A"),
                actionButton("apply_uniform_rep", "Set all wells",
                             class = "btn-default btn-block")
              ),
              div(
                class = "bs-undo-redo",
                style = "margin-top: 12px; padding-top: 12px; border-top: 1px solid var(--c-line); display: flex; gap: 8px;",
                actionButton("undo_layout", "Undo", icon = icon("rotate-left"), class = "btn-default btn-sm"),
                actionButton("redo_layout", "Redo", icon = icon("rotate-right"), class = "btn-default btn-sm")
              )
            )
          ),

          # ---- Right pane: active matrix viewport ----
          div(
            class = "bs-matrix-viewport",
            conditionalPanel(
              "input.active_layer == 'type'",
              div(
                id = "matrix_type_section", role = "grid",
                `aria-label` = "Sample type matrix: 8 rows by 12 columns",
                h4("Sample type"),
                conditionalPanel(
                  condition = "input.assay_type == 'elisa'",
                  uiOutput("elisa_controls_banner_ui")
                ),
                shinycssloaders::withSpinner(
                  rHandsontableOutput("matrix_type"),
                  type = 6, color = "#1f3a5f"),
                div(
                  class = "bs-legend",
                  span(class = "chip", span(class = "dot",
                                             style = "background:rgba(0,158,115,0.4)"),
                       "Standard"),
                  span(class = "chip", span(class = "dot",
                                             style = "background:rgba(0,114,178,0.4)"),
                       "Sample"),
                  span(class = "chip", span(class = "dot",
                                             style = "background:#f1f3f5"),
                       "Blank"),
                  span(class = "chip", span(class = "dot",
                                             style = "background:rgba(204,121,167,0.4)"),
                       "NSB"),
                  span(class = "chip", span(class = "dot",
                                             style = "background:rgba(230,159,0,0.4)"),
                       "B0")
                )
              )
            ),
            conditionalPanel(
              "input.active_layer == 'id'",
              div(
                id = "matrix_id_section", role = "grid",
                `aria-label` = "Sample ID matrix: 8 rows by 12 columns",
                h4("Sample ID"),
                shinycssloaders::withSpinner(
                  rHandsontableOutput("matrix_id"),
                  type = 6, color = "#1f3a5f")
              )
            ),
            conditionalPanel(
              "input.active_layer == 'dilution'",
              div(
                id = "matrix_dilution_section", role = "grid",
                `aria-label` = "Dilution fraction matrix: 8 rows by 12 columns",
                h4("Dilution"),
                # Existing per-well controls preserved
                div(`aria-live` = "polite", uiOutput("dilution_error_feedback")),
                div(`aria-live` = "polite", uiOutput("dilution_gt1_warning")),
                actionButton("reset_dilution", "Reset", class = "btn-default btn-xs"),
                shinycssloaders::withSpinner(
                  rHandsontableOutput("matrix_dilution"),
                  type = 6, color = "#1f3a5f"),
                div(class = "bs-pill is-info", style = "margin-top: 10px;",
                    "Enter the fraction of original sample strength remaining (1 = undiluted).")
              )
            ),
            conditionalPanel(
              "input.active_layer == 'rep'",
              div(
                id = "matrix_replicate_section", role = "grid",
                `aria-label` = "Replicate group matrix: 8 rows by 12 columns",
                h4("Replicates"),
                shinycssloaders::withSpinner(
                  rHandsontableOutput("matrix_replicate"),
                  type = 6, color = "#1f3a5f")
              )
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
            uiOutput("qc_params_heading_ui"),
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
            uiOutput("tissue_weight_heading_ui"),
            uiOutput("tissue_weight_banner_ui"),
            div(
              style = "display: flex; align-items: center; gap: 10px; margin-bottom: 6px;",
              div(
                style = "display: flex; align-items: center; gap: 6px;",
                uiOutput("set_all_extraction_label_ui", inline = TRUE),
                tags$input(type = "number", id = "uniform_extraction", value = "500",
                           min = "0", step = "10", class = "form-control",
                           style = "width: 80px; height: 30px; padding: 2px 6px;")
              ),
              actionButton("apply_uniform_extraction", "Apply",
                           class = "btn btn-sm btn-info",
                           style = "height: 30px; padding: 2px 12px;")
            ),
            rHandsontableOutput("tissue_weight_table"),
            uiOutput("scroll_right_hint_ui"),
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
      "Upload & Preview",
      value = "tab_upload",
      br(),

      fluidRow(
        # ---- LEFT (8 cols): file picker + preview + heatmap ----
        column(
          width = 8,
          div(
            class = "bs-card",
            h3("Upload plate data"),
            p(class = "bs-card-sub",
              "Upload your raw absorbance / counts file. We'll validate and preview before you continue."),
            radioButtons(
              "import_method", NULL,
              choices = c("Classic import" = "classic",
                          "Visual plate selector" = "visual"),
              selected = "classic", inline = TRUE
            ),
            div(
              style = "display: flex; gap: 12px; flex-wrap: wrap;",
              uiOutput("upload_counts_ui"),
              uiOutput("clear_upload_ui"),
              uiOutput("download_plate_template_ui")
            ),
            actionButton("show_sample_layout", "Show default plate layout",
                         class = "btn-default btn-sm",
                         style = "margin-top: 8px;")
          ),

          # Visual plate selector (shown only when method = visual)
          conditionalPanel(
            condition = "input.import_method == 'visual'",
            div(
              id = "visual_selector_section",
              class = "bs-card",
              style = "background: var(--c-accent-soft);",
              uiOutput("visual_selector_heading_ui"),
              uiOutput("visual_selector_intro_ui"),
              uiOutput("visual_file_preview"),
              uiOutput("visual_plate_selections"),
              uiOutput("visual_well_exclusion")
            )
          ),

          div(
            class = "bs-card",
            h4("Plate preview & heatmap"),
            fluidRow(
              column(
                width = 6,
                h5("Raw values"),
                tableOutput("meas_preview")
              ),
              column(
                width = 6,
                h5("Heatmap"),
                tags$figure(
                  id = "heatmap_preview_section",
                  style = "margin: 0;",
                  `aria-describedby` = "plate_heatmap_desc_text",
                  role = "figure",
                  uiOutput("heatmap_title_ui"),
                  tags$figcaption(id = "plate_heatmap_desc_text",
                                  uiOutput("heatmap_caption_ui", inline = TRUE)),
                  plotly::plotlyOutput("plate_heatmap", height = "320px"),
                  div(class = "sr-only", `aria-live` = "polite",
                      textOutput("plate_heatmap_description"))
                )
              )
            )
          )
        ),

        # ---- RIGHT (4 cols): import summary + validation ----
        column(
          width = 4,
          div(
            class = "bs-card",
            h4("Import summary"),
            uiOutput("upload_summary")
          )
        )
      ),
      br(),
      div(
        style = "display: flex; justify-content: space-between; padding: 15px;",
        actionButton("back_to_layout", "\u2190 Back: Plate Layout",
                     class = "btn-default btn-lg"),
        actionButton("next_to_analysis", "Next: Analysis Settings \u2192",
                     class = "btn-primary btn-lg")
      )
    ),


    # ======================================================================
    # TAB 4: Analysis Settings
    # ======================================================================
    tabPanel(
      "Analysis Settings",
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
        class = "bs-card",
        style = "max-width: 880px; padding: 20px 24px;",
        uiOutput("analysis_settings_heading_ui"),

        # Primary setting (always visible)
        div(
          class = "bs-drc-weighting",
          checkboxGroupInput("regression_weight", "DRC regression weighting:",
                     choices = c("Unweighted" = "none",
                                 "1/Y (moderate)" = "inv_y",
                                 "1/Y\u00B2 (recommended for immunoassays)" = "inv_y2",
                                 "Auto (data-driven)" = "auto"),
                     selected = "none")
        ),
        uiOutput("regression_weight_help_ui"),

        # Advanced Options collapsed behind a disclosure chevron (v3.2 §4)
        tags$details(
          class = "bs-advanced",
          tags$summary(
            class = "bs-advanced-summary",
            uiOutput("advanced_options_heading_ui")
          ),
          div(
            class = "bs-advanced-body",
            uiOutput("advanced_options_intro_ui"),

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
            uiOutput("quant_range_help_ui"),

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
              uiOutput("outlier_help_ui"),
              hr(),
              radioButtons("normality_assumption", "Normality assumption for outlier detection:",
                          choices = c("Assume normality (default)" = "assume",
                                      "Test with Shapiro-Wilk" = "test_shapiro"),
                          selected = "assume"),
              conditionalPanel(
                condition = "input.normality_assumption == 'test_shapiro'",
                uiOutput("normality_shapiro_help_ui")
              )
            ),
            hr(),
            numericInput("cv_limit", "Maximum CV for standards (%):",
                        value = 30, min = 5, max = 50, step = 5),
            uiOutput("cv_limit_help_ui")
          )
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
      "Generate Report",
      value = "tab_report",
      br(),
      div(
        style = "display: flex; justify-content: flex-start; padding: 0 15px 10px;",
        actionButton("back_to_analysis_top", "\u2190 Back: Analysis Settings",
                    class = "btn btn-default btn-lg")
      ),

      fluidRow(
        column(8,
          # GUI refresh \u00a75.3: convert_section becomes a bs-card. Format /
          # language checkbox groups receive `format-tiles` /
          # `lang-tiles` class hooks so the CSS in www/style.css can
          # style them as tiles without re-architecting the inputs.
          div(
            id = "convert_section",
            class = "bs-card",
            style = "padding: 20px 24px;",
            uiOutput("report_output_heading_ui"),
            div(class = "format-tiles",
                checkboxGroupInput("export_formats", "Report formats:",
                                  choices = c("HTML" = "html", "Word (DOCX)" = "docx", "PDF" = "pdf"),
                                  selected = "html",
                                  inline = TRUE)),
            uiOutput("report_formats_help_ui"),
            div(class = "lang-tiles",
                checkboxGroupInput("report_languages", "Report language(s):",
                                  choices = c("English" = "en", "Espa\u00f1ol" = "es",
                                              "Fran\u00e7ais" = "fr", "\u0420\u0443\u0441\u0441\u043a\u0438\u0439" = "ru", "\u4e2d\u6587" = "zh"),
                                  selected = "en",
                                  inline = TRUE)),
            uiOutput("report_languages_help_ui"),
            checkboxInput("generate_compact",
                          "Generate compact report",
                          value = TRUE),
            br(),
            uiOutput("report_variants_explainer_ui"),
            br(),
            # btn-primary now maps to navy via the design tokens; drop
            # the heavy inline overrides (font-weight 700, 12 px radius)
            # and let the design system drive the look.
            actionButton("convert",
                        label = "Generate Report",
                        icon  = icon("file-arrow-down"),
                        class = "btn btn-primary btn-lg",
                        style = "width: 100%;"),
            br(), br(),
            uiOutput("download_report_ui"),
            uiOutput("download_report_full_ui")
          )
        ),
        column(4,
          # Notes & Feedback
          div(
            id = "notes_feedback_section",
            uiOutput("notes_feedback_heading_ui"),
            textAreaInput("notes", "Notes (optional) - will appear in the report:",
                         value = "", placeholder = "Observations, sample info, run notes...",
                         rows = 8),
            br(),
            uiOutput("give_feedback_ui")
          )
        )
      ),
      br(),

      # GUI refresh §5.3: pre-flight panel as a quiet bs-card with an
      # info-soft accent rail instead of the warning-yellow tinted
      # wellPanel. Status of individual checks (pass / warn / fail) is
      # already encoded by the renderer's chip-glyphs.
      div(
        id = "preflight_section",
        class = "bs-card",
        style = paste0("padding: 16px 18px; margin-bottom: 16px; ",
                       "border-left: 4px solid var(--c-info);"),
        uiOutput("preflight_heading_ui"),
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
  # Task 1: session_root is the per-launch container; every Generate Report
  # click creates a fresh run_HHMMSS subfolder under it (see server_report.R)
  # so a second report doesn't overwrite the first.
  session$userData$session_root <- session$userData$output_dir


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

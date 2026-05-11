# report_pipeline.R
# ============================================================================
# Staged pipeline helpers for report generation.
# Each function accepts only what it needs and returns its result for the
# next stage, keeping the orchestrator in server_report.R slim.
#
# Called from server_report.R via observeEvent(input$convert). Each stage
# is a pure function that can be tested in isolation.
# ============================================================================

# --------------------------------------------------------------------------
# PDF capability detection
# --------------------------------------------------------------------------

#' Check whether the current environment can render PDF reports
#'
#' Tests for a TeX engine in this order:
#' 1. TinyTeX installed and available
#' 2. System pdflatex / xelatex / lualatex on PATH
#'
#' @return TRUE if PDF rendering should work, FALSE otherwise
pdf_render_available <- function() {
  # Check TinyTeX first (most common in R setups)
  if (requireNamespace("tinytex", quietly = TRUE)) {
    if (tryCatch(tinytex::is_tinytex(), error = function(e) FALSE)) {
      return(TRUE)
    }
  }
  # Fall back to system TeX
  any(nzchar(Sys.which(c("pdflatex", "xelatex", "lualatex"))))
}

# --------------------------------------------------------------------------
# Stage 1: Flush pending layout state
# --------------------------------------------------------------------------
#' Synchronously flush any debounced dilution edits so the Convert observer
#' always sees the latest cell values.
#'
#' @param input  Shiny input object (needs $matrix_dilution)
#' @param shared Shared reactive state list
flush_latest_layout_state <- function(input, shared) {
  if (!is.null(input$matrix_dilution)) {
    raw <- hot_to_r(input$matrix_dilution)
    raw <- enforce_plate_shape(raw)
    shared$raw_matrix_dilution(raw)
    result <- parse_dilution_matrix(raw)
    shared$matrix_dilution(enforce_plate_shape(as.data.frame(result$values)))
    shared$dilution_validity(result$validity)
    shared$dilution_error(any(!result$validity))
  }
}

# --------------------------------------------------------------------------
# Stage 2: Build long-format data from plate matrices
# --------------------------------------------------------------------------
#' Convert plate matrices to a long-format data frame.
#'
#' @param shared       Shared reactive state list
#' @param std_conc_raw Synchronous standard concentration vector
#' @return A data frame in long format (one row per well)
build_long_data <- function(shared, std_conc_raw) {
  matrix_to_long(
    shared$matrix_type(),
    shared$matrix_id(),
    shared$matrix_dilution(),
    shared$matrix_replicate(),
    shared$matrix_measresults(),
    std_conc_raw
  )
}

# --------------------------------------------------------------------------
# Stage 3: Normalize assay data
# --------------------------------------------------------------------------
#' Apply assay-specific normalization to long-format data.
#'
#' For ELISA: pass-through (normalization handled by Rmd template).
#' For RBA: apply normalize_data() with radioligand detection method.
#'
#' @param df_long    Long-format data frame from build_long_data()
#' @param assay_type Character: "elisa" or "rba"
#' @param type_mat   Type matrix (used for ELISA control validation)
#' @param session    Shiny session (for notifications on error/warning)
#' @return Normalized data frame with NormalizedValue and ResponseUnit columns,
#'         or NULL if ELISA controls are missing (caller should abort).
normalize_assay_data <- function(df_long, assay_type, type_mat, session) {


  # Validate ELISA control wells exist before normalization
  if (assay_type == "elisa") {
    well_types <- as.character(unlist(type_mat))
    required_controls <- c("Blank", "NSB", "B0")
    missing_controls <- required_controls[!required_controls %in% well_types]
    if (length(missing_controls) > 0) {
      showNotification(
        paste("ELISA requires control wells:", paste(missing_controls, collapse = ", "),
              "- please assign them in the Type matrix before generating a report."),
        type = "error", duration = 10
      )
      return(NULL)
    }
  }

  tryCatch({
    if (assay_type == "elisa") {
      # ELISA normalization (%B/B0) is handled by the Rmd template via
      # calculate_elisa_bb0(), which follows the Cayman protocol with
      # proper Blank correction. Pass through raw absorbance here.
      df_long %>%
        dplyr::mutate(
          NormalizedValue = MeasurementValue,
          ResponseUnit = "Raw Absorbance"
        )
    } else {
      # RBA: Direct measurement (CPM or RFU)
      normalize_data(df_long, "rba", "radioligand")
    }
  }, error = function(e) {
    # If normalization fails, show warning but continue with raw data
    showNotification(
      paste("Normalization warning:", e$message),
      type = "warning", duration = 8
    )
    df_long %>%
      dplyr::mutate(
        NormalizedValue = MeasurementValue,
        ResponseUnit = if (assay_type == "elisa") "Absorbance" else "CPM"
      )
  })
}

# --------------------------------------------------------------------------
# Stage 4: Save analysis artifacts (CSV, config JSONs)
# --------------------------------------------------------------------------
#' Write CSV data and configuration JSON files to the output directory.
#'
#' @param df_normalized  Normalized long-format data frame
#' @param config         Named list of all configuration values extracted from
#'                       input/shared *before* this function is called:
#'   \itemize{
#'     \item csv_path, output_dir, fmt_json, notes_file -- session paths
#'     \item export_formats, notes, assay_type
#'     \item is_multiwavelength, wavelengths, wavelength_plates
#'     \item matrix_type, matrix_id, matrix_dilution, matrix_replicate,
#'           std_conc_raw -- layout matrices (for multi-wavelength re-pivot)
#'     \item qc_params, assay_config, analysis_config
#'     \item report_language
#'     \item tissue_weights (ELISA only, may be NULL)
#'   }
#' @param session  Shiny session (for notifications)
#' @return Invisible NULL
save_analysis_artifacts <- function(df_normalized, config, session) {


  # ---- Main CSV ----
  write.csv(df_normalized, config$csv_path, row.names = FALSE)

  # ---- Multi-wavelength CSVs ----
  if (isTRUE(config$is_multiwavelength) && !is.null(config$wavelengths)) {
    message(sprintf("Processing %d wavelengths...", length(config$wavelengths)))

    converter <- matrix_to_long_with_cached_layout(
      config$matrix_type, config$matrix_id, config$matrix_dilution,
      config$matrix_replicate, config$std_conc_raw
    )

    for (wl in config$wavelengths) {
      plate_wl <- config$wavelength_plates[[wl]]
      df_long_wl <- converter(plate_wl)

      df_normalized_wl <- tryCatch({
        if (config$assay_type == "elisa") {
          df_long_wl %>%
            dplyr::mutate(
              NormalizedValue = MeasurementValue,
              ResponseUnit = "Raw Absorbance"
            )
        } else {
          normalize_data(df_long_wl, "rba", "radioligand")
        }
      }, error = function(e) {
        df_long_wl %>%
          dplyr::mutate(
            NormalizedValue = MeasurementValue,
            ResponseUnit = if (config$assay_type == "elisa") "Absorbance" else "CPM"
          )
      })

      wl_safe <- gsub("[/\\\\:*?\"<>|]", "_", wl)
      csv_path_wl <- file.path(config$output_dir, paste0("long_data_output_", wl_safe, ".csv"))
      write.csv(df_normalized_wl, csv_path_wl, row.names = FALSE)
      message(sprintf("Saved: %s", basename(csv_path_wl)))
    }

    write_json_safe(
      list(wavelengths = config$wavelengths),
      file.path(config$output_dir, "wavelength_manifest.json")
    )

    showNotification(
      sprintf("Saved data for %d wavelengths", length(config$wavelengths)),
      type = "message", duration = 5
    )
  }

  # ---- Config JSONs ----
  write_json_safe(config$export_formats, config$fmt_json)
  write_json_safe(list(notes = config$notes), config$notes_file)
  write_json_safe(config$qc_params, file.path(config$output_dir, "qc_params.json"))
  write_json_safe(config$assay_config, file.path(config$output_dir, "assay_config.json"))
  write_json_safe(config$analysis_config, file.path(config$output_dir, "analysis_config.json"))

  # ---- Tissue weights (ELISA only) ----
  if (config$assay_type == "elisa") {
    if (!is.null(config$tissue_weights) && length(config$tissue_weights) > 0) {
      write_json_safe(config$tissue_weights, file.path(config$output_dir, "tissue_weights.json"))
    }
    processing_config <- list(
      extraction_volume_ul = 500,
      sample_type = "extracted",
      notes = "Per-sample extraction volumes stored in tissue_weights.json"
    )
    write_json_safe(processing_config,
                    file.path(config$output_dir, "sample_processing_config.json"))
  }

  # ---- Report language(s) ----
  # #4: config$report_languages is a vector of every language the user ticked.
  # The sidecar JSON keeps the historical `lang` scalar (first ticked language)
  # for back-compat with run_analysis_modular.R, plus a `langs` vector field.
  langs_for_sidecar <- config$report_languages %||% config$report_language %||% "en"
  write_json_safe(list(lang  = langs_for_sidecar[[1]],
                       langs = as.list(langs_for_sidecar)),
                  file.path(config$output_dir, "report_language.json"))

  showNotification(paste("Data saved to:", config$csv_path),
                   type = "message", duration = 5)

  invisible(NULL)
}

# --------------------------------------------------------------------------
# Stage 5: Render Rmd reports
# --------------------------------------------------------------------------
#' Render report templates in the requested output formats.
#'
#' @param params  Named list:
#'   \itemize{
#'     \item output_dir   -- absolute path to output directory
#'     \item report_lang  -- "en" or "es"
#'     \item is_multiwavelength -- logical
#'     \item wavelengths  -- character vector (multi-wavelength only)
#'     \item selected_formats -- character vector, subset of c("html","docx")
#'     \item csv_path     -- path to main CSV (used to locate app root fallback)
#'   }
#' @param session Shiny session (for notifications)
#' @return Named list of successfully rendered output paths
render_reports <- function(params, session) {

  # Build format specifications.  For DOCX we use an explicit
  # rmarkdown::word_document() call so we can guarantee fig_caption, PNG
  # device, and high-DPI figures even if the YAML header is overridden.
  ref_docx_path <- file.path(
    if (file.exists("reports")) "reports" else file.path(dirname(params$csv_path), "reports"),
    "reference.docx"
  )
  docx_fmt <- rmarkdown::word_document(
    toc = TRUE,
    fig_caption = TRUE,
    fig_width = 6,
    fig_height = 4,
    reference_docx = if (file.exists(ref_docx_path)) ref_docx_path else NULL
  )
  formats_map <- list(html = "html_document", pdf = "pdf_document", docx = docx_fmt)
  is_mw <- isTRUE(params$is_multiwavelength)

  # --- Graceful PDF fallback ---
  selected <- params$selected_formats
  if ("pdf" %in% selected && !pdf_render_available()) {
    showNotification(
      paste("PDF rendering is not available in this environment",
            "because no LaTeX engine was detected.",
            "An HTML report will be generated instead."),
      type = "warning", duration = 12
    )
    selected <- setdiff(selected, "pdf")
    if (length(selected) == 0) selected <- "html"
    if (!"html" %in% selected) selected <- c(selected, "html")
  }
  params$selected_formats <- selected

  # Locate template directory

  app_root <- if (file.exists("reports")) "." else dirname(params$csv_path)
  template_dir <- file.path(app_root, "reports")
  if (!dir.exists(template_dir)) template_dir <- "reports"

  if (is_mw) {
    report_template_full    <- file.path(template_dir, "multiwavelength_analysis_template.Rmd")
    report_template_compact <- report_template_full   # multi-wave has no compact variant
  } else {
    # H4 split: each variant has its own entry-point template. The compact
    # template is a thin wrapper that child-includes the detailed template
    # with `compact: true` in its params block.
    report_template_full    <- file.path(template_dir, "unified_analysis_template.Rmd")
    report_template_compact <- file.path(template_dir, "unified_analysis_template_compact.Rmd")
  }

  for (tpl in unique(c(report_template_full, report_template_compact))) {
    if (!file.exists(tpl)) {
      showNotification(
        "Report template not found - data saved but no report generated.",
        type = "warning", duration = 8
      )
      message("Template not found at: ", tpl)
      return(list())
    }
  }

  report_template_full    <- normalizePath(report_template_full,
                                           winslash = "/", mustWork = TRUE)
  report_template_compact <- normalizePath(report_template_compact,
                                           winslash = "/", mustWork = TRUE)
  out_dir_abs <- normalizePath(params$output_dir, winslash = "/", mustWork = TRUE)

  output_paths <- list()

  # H4 perf cache: unified_analysis_template.Rmd's model-fitting and
  # sample-analysis chunks now serialise their results to .rds files inside
  # output_dir on first render and read them back on subsequent renders.
  # All variants (compact/full × html/docx) of one render_reports() call
  # share the same fit + quantification, so this trims n>1 renders to a
  # single compute pass. For multi-wavelength runs the cache lives in each
  # per-plate `Plate N/` subdir (because the child render's output_dir is
  # the per-plate dir), so we walk the output_dir recursively to wipe any
  # stale entries before we start a fresh run.
  for (cache_pattern in c("^\\.fit_all_models_cache\\.rds$",
                          "^\\.quantify_samples_cache\\.rds$")) {
    hits <- list.files(out_dir_abs, pattern = cache_pattern,
                       recursive = TRUE, all.files = TRUE,
                       full.names = TRUE, include.dirs = FALSE)
    for (h in hits) try(file.remove(h), silent = TRUE)
  }

  # H4: render 1-2 variants per format. If generate_compact is TRUE the app
  # renders a compact version (suffix -compact) alongside the detailed one
  # (suffix -full). If FALSE only the detailed report is produced.
  #
  # The compact/full split is a feature of unified_analysis_template.Rmd only.
  # multiwavelength_analysis_template.Rmd doesn't declare a `compact` param
  # and has no compact-mode logic, so for multi-wavelength data we always
  # render a single "full" variant and never pass `compact` through.
  #
  # #4: report_langs is a vector of language codes the user ticked. We loop
  # over it so each variant × format is rendered in every requested language;
  # output filenames carry a -<lang> suffix so the HTML/DOCX files can sit
  # side-by-side in the same output_dir. The H4 cache lives in output_dir
  # and is language-agnostic, so multi-language renders share one compute
  # pass.
  want_compact <- isTRUE(params$generate_compact) && !is_mw
  variants <- if (want_compact) c("compact", "full") else "full"
  langs <- params$report_langs %||% params$report_lang %||% "en"

  for (fmt in params$selected_formats) {
    for (variant in variants) {
      for (lang_code in langs) {
        is_compact_variant <- identical(variant, "compact")
        showNotification(sprintf("Rendering %s %s report (%s)...",
                                 toupper(fmt),
                                 if (is_compact_variant) "compact" else "detailed",
                                 toupper(lang_code)),
                         type = "message", duration = 3)

        render_ok <- tryCatch({
          render_params <- list(output_dir = out_dir_abs,
                                lang = lang_code)
          if (is_mw) render_params$wavelengths <- params$wavelengths

          # H4 split: each variant has its own entry-point template; the
          # compact param now lives in the compact template's YAML
          # default, so we no longer pass it through render_params.
          this_template <- if (is_compact_variant) report_template_compact
                           else report_template_full

          base_out <- if (is_mw) "Multi-Wavelength-Analysis-Report" else "RBA-results-report"
          out_name <- paste0(base_out, "-", variant, "-", lang_code)

          out_file <- rmarkdown::render(
            input = this_template,
            output_format = formats_map[[fmt]],
            output_file = out_name,
            output_dir = out_dir_abs,
            params = render_params,
            knit_root_dir = dirname(this_template),
            envir = new.env(parent = globalenv())
          )
          output_paths[[paste(fmt, variant, lang_code, sep = "-")]] <- out_file
          TRUE
        }, error = function(e) {
          showNotification(
            sprintf("Report rendering failed (%s %s %s): %s",
                    toupper(fmt), variant, lang_code, e$message),
            type = "error", duration = 10
          )
          message(sprintf("Report render error (%s %s %s): %s",
                          fmt, variant, lang_code, e$message))
          FALSE
        })

        # --- Graceful PDF-to-HTML fallback on render failure ---
        # #4: keyed by (variant, lang_code) so each language gets its own
        # fallback attempt rather than one accidentally shadowing the other.
        fallback_key <- paste("html", variant, lang_code, sep = "-")
        if (!render_ok && fmt == "pdf" &&
            !(fallback_key %in% names(output_paths))) {
          showNotification(
            sprintf("PDF compilation failed (%s). Generating HTML report as fallback...",
                    toupper(lang_code)),
            type = "warning", duration = 8
          )
          html_ok <- tryCatch({
            render_params <- list(output_dir = out_dir_abs, lang = lang_code)
            if (is_mw) render_params$wavelengths <- params$wavelengths
            # Match the variant -> template selection from the primary
            # render path so the fallback writes to the correct file.
            this_template <- if (is_compact_variant) report_template_compact
                             else report_template_full
            base_out <- if (is_mw) "Multi-Wavelength-Analysis-Report" else "RBA-results-report"
            out_name <- paste0(base_out, "-", variant, "-", lang_code)
            out_file <- rmarkdown::render(
              input = this_template,
              output_format = formats_map[["html"]],
              output_file = out_name,
              output_dir = out_dir_abs,
              params = render_params,
              knit_root_dir = dirname(this_template),
              envir = new.env(parent = globalenv())
            )
            output_paths[[fallback_key]] <- out_file
            TRUE
          }, error = function(e2) {
            showNotification(
              sprintf("HTML fallback also failed (%s): %s",
                      toupper(lang_code), e2$message),
              type = "error", duration = 10
            )
            FALSE
          })
          if (html_ok) {
            showNotification(sprintf("HTML report created as PDF fallback (%s).",
                                     toupper(lang_code)),
                             type = "message", duration = 5)
          }
        }

        if (render_ok) {
          showNotification(sprintf("%s %s %s report created!",
                                   toupper(fmt), variant, toupper(lang_code)),
                           type = "message", duration = 5)
        }
      }  # end for (lang_code in langs)
    }  # end for (variant in variants)
  }  # end for (fmt in params$selected_formats)

  showNotification(paste("Reports saved to:", out_dir_abs),
                   type = "message", duration = 8)

  output_paths
}

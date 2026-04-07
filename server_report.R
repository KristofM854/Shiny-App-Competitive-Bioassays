# server_report.R
# ============================================================================
# Report generation and pre-flight validation logic extracted from app.R
# ============================================================================
# This module provides:
#   - Hill slope warning UI
#   - Dilution error feedback UI
#   - Pre-flight checks UI
#   - Convert button enable/disable observer
#   - Report generation observer (observeEvent on input$convert)
#
# Parameters:
#   input, output, session  — standard Shiny server arguments
#   shared                  — shared reactive state (matrix data, rv, etc.)
#   config_reactives        — list with:
#       $std_conc                — reactive returning standard concentration vector
#       $chosen_standard_label   — reactive returning the toxin label string
# ============================================================================

server_report <- function(input, output, session, shared, config_reactives) {

  # --------------------------------------------------------------------------
  # Hill Slope Warning
  # --------------------------------------------------------------------------

  output$hill_warning <- renderUI({
    raw <- input$expected_hill
    if (is.null(raw) || trimws(raw) == "") {
      return(tags$div(style = "color: red; font-weight: bold;",
                     "\u26a0\ufe0f Hill slope required"))
    }
    val <- suppressWarnings(as.numeric(raw))
    if (is.na(val)) {
      return(tags$div(style = "color: red; font-weight: bold;",
                     "\u26a0\ufe0f Must be numeric"))
    }
    if (val < 0.5 || val > 1.5) {
      return(tags$div(style = "color: orange; font-weight: bold;",
                     "\u26a0\ufe0f Outside expected range (0.5\u20131.5)"))
    }
    NULL
  })

  # --------------------------------------------------------------------------
  # Dilution Error Feedback
  # --------------------------------------------------------------------------

  output$dilution_error_feedback <- renderUI({
    if (shared$dilution_error()) {
      tags$div(style = "color: red; font-weight: bold;",
              "\u26a0\ufe0f Invalid dilution entries (red cells)")
    }
  })

  # --------------------------------------------------------------------------
  # Pre-Flight Check Panel
  # --------------------------------------------------------------------------

  output$preflight_checks <- renderUI({
    assay <- input$assay_type %||% "rba"
    checks <- list()

    # Check 1: Plate data uploaded
    plate <- shared$matrix_measresults()
    plate_ok <- !is.null(plate) && any(!is.na(as.numeric(unlist(plate))))
    checks[[length(checks) + 1]] <- if (plate_ok) {
      tags$div(style = "color: #388E3C;", icon("check-circle"), " Plate data uploaded")
    } else {
      tags$div(style = "color: #D32F2F;", icon("times-circle"), " No plate data - go to Upload tab")
    }

    # Check 2: Standards defined
    type_mat <- shared$matrix_type()
    n_std <- if (!is.null(type_mat)) sum(unlist(type_mat) == "Standard", na.rm = TRUE) else 0
    checks[[length(checks) + 1]] <- if (n_std >= 4) {
      tags$div(style = "color: #388E3C;", icon("check-circle"), paste(" ", n_std, "standard wells defined"))
    } else {
      tags$div(style = "color: #D32F2F;", icon("times-circle"), paste(" Only", n_std, "standard wells (need >= 4)"))
    }

    # Check 3: ELISA controls
    if (assay == "elisa" && !is.null(type_mat)) {
      well_types <- as.character(unlist(type_mat))
      has_blank <- "Blank" %in% well_types
      has_nsb <- "NSB" %in% well_types
      has_b0 <- "B0" %in% well_types
      if (has_blank && has_nsb && has_b0) {
        checks[[length(checks) + 1]] <- tags$div(
          style = "color: #388E3C;", icon("check-circle"), " ELISA controls present (Blank, NSB, B0)")
      } else {
        missing <- c()
        if (!has_blank) missing <- c(missing, "Blank")
        if (!has_nsb) missing <- c(missing, "NSB")
        if (!has_b0) missing <- c(missing, "B0")
        checks[[length(checks) + 1]] <- tags$div(
          style = "color: #D32F2F;", icon("times-circle"),
          paste(" Missing ELISA controls:", paste(missing, collapse = ", ")))
      }
    }

    # Check 4: Dilution validity
    dil_ok <- !shared$dilution_error()
    checks[[length(checks) + 1]] <- if (dil_ok) {
      tags$div(style = "color: #388E3C;", icon("check-circle"), " Dilution factors valid")
    } else {
      tags$div(style = "color: #FF9800;", icon("exclamation-triangle"), " Some dilution entries are invalid")
    }

    do.call(tagList, checks)
  })

  # --------------------------------------------------------------------------
  # Convert Button Enable/Disable
  # --------------------------------------------------------------------------

  observe({
    assay <- input$assay_type %||% "rba"

    # Plate data must be confirmed/imported (not just file selected)
    plate <- shared$matrix_measresults()
    plate_data_ok <- !is.null(plate) && any(!is.na(plate))

    # Dilution validity
    dilution_ok <- !shared$dilution_error()

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
    if (plate_data_ok && dilution_ok && qc_ok && hill_ok) {
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
        shared$matrix_type(), shared$matrix_id(), shared$matrix_dilution(),
        shared$matrix_replicate(), shared$matrix_measresults(), config_reactives$std_conc()
      )

      incProgress(0.2, detail = "Validating data...")

      # Validate ELISA control wells exist before normalization
      if (input$assay_type == "elisa") {
        type_mat <- shared$matrix_type()
        well_types <- as.character(type_mat)
        required_controls <- c("Blank", "NSB", "B0")
        missing_controls <- required_controls[!required_controls %in% well_types]
        if (length(missing_controls) > 0) {
          showNotification(
            paste("ELISA requires control wells:", paste(missing_controls, collapse = ", "),
                  "- please assign them in the Type matrix before generating a report."),
            type = "error", duration = 10
          )
          return()
        }
      }

      # Apply normalization based on assay type
      df_normalized <- tryCatch({

        if (input$assay_type == "elisa") {
          # ELISA normalization (%B/B0) is handled by the Rmd template via
          # calculate_elisa_bb0(), which follows the Cayman protocol with
          # proper Blank correction. Pass through raw absorbance here.
          detection_method <- "absorbance"
          df_long %>%
            dplyr::mutate(
              NormalizedValue = MeasurementValue,
              ResponseUnit = "Raw Absorbance"
            )
        } else {
          # RBA: Direct measurement (CPM or RFU)
          detection_method <- "radioligand"
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

      incProgress(0.4, detail = "Saving data files...")

      # Save main CSV (for single wavelength OR first wavelength of multi-wavelength)
      csv_path <- session$userData$csv_path
      write.csv(df_normalized, csv_path, row.names = FALSE)

      # NEW: If multi-wavelength, save additional wavelength CSVs
      if (isTRUE(shared$rv$is_multiwavelength) && !is.null(shared$rv$wavelengths)) {

        message(sprintf("Processing %d wavelengths...", length(shared$rv$wavelengths)))

        # Build the converter once — layout matrices are identical across
        # wavelengths, so pivoting them inside the loop would be redundant.
        converter <- matrix_to_long_with_cached_layout(
          shared$matrix_type(), shared$matrix_id(), shared$matrix_dilution(),
          shared$matrix_replicate(), config_reactives$std_conc()
        )

        # Save each wavelength as separate CSV
        for (wl in shared$rv$wavelengths) {

          # Get the plate data for this wavelength
          plate_wl <- shared$rv$wavelength_plates[[wl]]

          # Convert to long format reusing the cached layout
          df_long_wl <- converter(plate_wl)

          # Apply normalization
          df_normalized_wl <- tryCatch({

            if (input$assay_type == "elisa") {
              # Pass raw absorbance — normalization (%B/B0) is performed by
              # calculate_elisa_bb0() in the Rmd template, which is the single
              # source of truth for ELISA normalization.
              df_long_wl %>%
                dplyr::mutate(
                  NormalizedValue = MeasurementValue,
                  ResponseUnit = "Raw Absorbance"
                )
            } else {
              detection_method <- "radioligand"
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
          csv_path_wl <- file.path(session$userData$output_dir, paste0("long_data_output_", wl, ".csv"))
          write.csv(df_normalized_wl, csv_path_wl, row.names = FALSE)

          message(sprintf("Saved: %s", basename(csv_path_wl)))
        }

        # Save wavelength manifest
        write_json_safe(
          list(wavelengths = shared$rv$wavelengths),
          file.path(session$userData$output_dir, "wavelength_manifest.json")
        )

        showNotification(
          sprintf("Saved data for %d wavelengths", length(shared$rv$wavelengths)),
          type = "message",
          duration = 5
        )
      }

      # Save formats (use session-scoped paths for concurrency safety)
      write_json_safe(input$export_formats, session$userData$fmt_json)

      # Save notes
      write_json_safe(list(notes = input$notes %||% ""),
                      session$userData$notes_file)

      # Save QC params (conditional based on assay type)
      qc_params <- if (input$assay_type == "rba") {
        list(
          qc_concentration = input$qc_conc,
          expected_hill = input$expected_hill,
          assay_type = input$assay_type,
          detection_method = "radioligand",
          analyte = config_reactives$chosen_standard_label()
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

      write_json_safe(qc_params, file.path(session$userData$output_dir, "qc_params.json"))

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
          toxin_standard_label = config_reactives$chosen_standard_label(),
          molecular_weight_g_mol = shared$mw_g_mol(),
          detection_method = "radioligand",
          units = "mol/L"
        )
      }

      write_json_safe(assay_config, file.path(session$userData$output_dir, "assay_config.json"))

      # Save analysis settings
      # regression_weight is now a vector (user can select multiple for comparison)
      sel_weights <- input$regression_weight
      if (is.null(sel_weights) || length(sel_weights) == 0) sel_weights <- "none"
      analysis_config <- list(
        regression_weight = sel_weights,
        quant_range_min = input$quant_range_min %||% 20,
        quant_range_max = input$quant_range_max %||% 80,
        ci_method = input$ci_method %||% "t_dist",
        enable_outlier_detection = isTRUE(input$enable_outlier_detection),
        outlier_min_n = input$outlier_min_n %||% 3,
        normality_assumption = input$normality_assumption %||% "assume",
        cv_limit = input$cv_limit %||% 30
      )
      write_json_safe(analysis_config, file.path(session$userData$output_dir, "analysis_config.json"))

      # Save tissue weights (ELISA only)
      if (input$assay_type == "elisa") {
        tw <- shared$tissue_weights_rv()
        if (length(tw) > 0) {
          # New format: {"R1": {"weight": 50.0, "extraction_uL": 500}, ...}
          write_json_safe(tw, file.path(session$userData$output_dir, "tissue_weights.json"))
        }

        # Save default processing config (per-sample volumes are in tissue_weights.json)
        processing_config <- list(
          extraction_volume_ul = 500,
          sample_type = "extracted",
          notes = "Per-sample extraction volumes stored in tissue_weights.json"
        )
        write_json_safe(processing_config,
                       file.path(session$userData$output_dir, "sample_processing_config.json"))
      }

      # Save report language preference
      write_json_safe(list(lang = input$report_language %||% "en"),
                     file.path(session$userData$output_dir, "report_language.json"))

      incProgress(0.6, detail = "Saving configuration...")

      showNotification(paste("Data saved to:", csv_path),
                       type = "message", duration = 5)

      incProgress(0.7, detail = "Rendering report (this may take a minute)...")

      # ----- Render reports inside the app -----
      report_lang <- input$report_language %||% "en"
      selected_formats <- input$export_formats
      formats_map <- list(html = "html_document", pdf = "pdf_document", docx = "word_document")

      # Determine template
      is_mw <- isTRUE(shared$rv$is_multiwavelength)
      app_root <- if (file.exists("reports")) "." else dirname(session$userData$csv_path)
      template_dir <- file.path(app_root, "reports")
      if (!dir.exists(template_dir)) template_dir <- "reports"

      if (is_mw) {
        report_template <- file.path(template_dir, "multiwavelength_analysis_template.Rmd")
      } else {
        report_template <- file.path(template_dir, "unified_analysis_template.Rmd")
      }

      if (file.exists(report_template)) {
        report_template <- normalizePath(report_template, winslash = "/", mustWork = TRUE)
        out_dir_abs <- normalizePath(session$userData$output_dir, winslash = "/", mustWork = TRUE)

        for (fmt in selected_formats) {
          showNotification(sprintf("Rendering %s report...", toupper(fmt)), type = "message", duration = 3)

          render_ok <- tryCatch({
            render_params <- list(
              output_dir = out_dir_abs,
              lang = report_lang
            )
            if (is_mw) render_params$wavelengths <- shared$rv$wavelengths

            out_name <- if (is_mw) "Multi-Wavelength-Analysis-Report" else "RBA-results-report"

            rmarkdown::render(
              input = report_template,
              output_format = formats_map[[fmt]],
              output_file = out_name,
              output_dir = out_dir_abs,
              params = render_params,
              knit_root_dir = dirname(report_template),
              envir = new.env(parent = globalenv())
            )
            TRUE
          }, error = function(e) {
            showNotification(
              sprintf("Report rendering failed (%s): %s", toupper(fmt), e$message),
              type = "error", duration = 10
            )
            message(sprintf("Report render error (%s): %s", fmt, e$message))
            FALSE
          })

          if (render_ok) {
            showNotification(sprintf("%s report created!", toupper(fmt)), type = "message", duration = 5)
          }
        }

        showNotification(
          paste("Reports saved to:", out_dir_abs),
          type = "message", duration = 8
        )
      } else {
        showNotification("Report template not found - data saved but no report generated.", type = "warning", duration = 8)
        message("Template not found at: ", report_template)
      }

      incProgress(1, detail = "Done!")

      # stopApp() removed: let users iterate without restarting the app
    })
  })

}

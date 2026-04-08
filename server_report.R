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

    # Check 5: Standards count consistency
    num_std_input <- as.integer(input$num_standards %||% 0)
    if (!is.null(type_mat) && num_std_input > 0) {
      rep_mat <- shared$matrix_replicate()
      type_vec <- as.character(unlist(type_mat))
      rep_vec <- as.character(unlist(rep_mat))
      std_mask <- type_vec == "Standard" & !is.na(type_vec)
      unique_std_groups <- length(unique(rep_vec[std_mask]))
      if (unique_std_groups == num_std_input) {
        checks[[length(checks) + 1]] <- tags$div(
          style = "color: #388E3C;", icon("check-circle"),
          paste0(" Standard count matches: ", unique_std_groups, " groups for ", num_std_input, " standards"))
      } else {
        checks[[length(checks) + 1]] <- tags$div(
          style = "color: #FF9800;", icon("exclamation-triangle"),
          paste0(" Standard groups (", unique_std_groups, ") differs from configured standards (", num_std_input, ")"))
      }
    }

    # Check 6: Replicate-group consistency (no mixed types within a group)
    if (!is.null(type_mat)) {
      rep_mat <- shared$matrix_replicate()
      type_vec <- as.character(unlist(type_mat))
      rep_vec <- as.character(unlist(rep_mat))
      non_empty <- !is.na(type_vec) & type_vec != "" & !is.na(rep_vec) & rep_vec != ""
      if (any(non_empty)) {
        group_df <- data.frame(type = type_vec[non_empty], rep = rep_vec[non_empty],
                               stringsAsFactors = FALSE)
        mixed_groups <- character(0)
        for (g in unique(group_df$rep)) {
          types_in_group <- unique(group_df$type[group_df$rep == g])
          if (length(types_in_group) > 1) mixed_groups <- c(mixed_groups, g)
        }
        if (length(mixed_groups) == 0) {
          checks[[length(checks) + 1]] <- tags$div(
            style = "color: #388E3C;", icon("check-circle"), " Replicate groups are consistent")
        } else {
          checks[[length(checks) + 1]] <- tags$div(
            style = "color: #FF9800;", icon("exclamation-triangle"),
            paste0(" Mixed well types in replicate group(s): ", paste(mixed_groups, collapse = ", ")))
        }
      }
    }

    # Check 7: Empty sample IDs
    if (!is.null(type_mat)) {
      id_mat <- shared$matrix_id()
      type_vec <- as.character(unlist(type_mat))
      id_vec <- as.character(unlist(id_mat))
      sample_mask <- type_vec == "Sample" & !is.na(type_vec)
      if (any(sample_mask)) {
        empty_ids <- sum(is.na(id_vec[sample_mask]) | trimws(id_vec[sample_mask]) == "")
        if (empty_ids == 0) {
          checks[[length(checks) + 1]] <- tags$div(
            style = "color: #388E3C;", icon("check-circle"), " All sample wells have IDs")
        } else {
          checks[[length(checks) + 1]] <- tags$div(
            style = "color: #FF9800;", icon("exclamation-triangle"),
            paste0(" ", empty_ids, " sample well(s) have empty IDs"))
        }
      }
    }

    # Check 8: ELISA control count plausibility
    if (assay == "elisa" && !is.null(type_mat)) {
      well_types <- as.character(unlist(type_mat))
      control_types <- c("Blank", "NSB", "B0")
      low_rep_controls <- character(0)
      for (ct in control_types) {
        ct_count <- sum(well_types == ct, na.rm = TRUE)
        if (ct_count > 0 && ct_count < 2) {
          low_rep_controls <- c(low_rep_controls, paste0(ct, " (", ct_count, ")"))
        }
      }
      if (length(low_rep_controls) == 0) {
        has_any_control <- any(well_types %in% control_types, na.rm = TRUE)
        if (has_any_control) {
          checks[[length(checks) + 1]] <- tags$div(
            style = "color: #388E3C;", icon("check-circle"), " ELISA controls have adequate replicates")
        }
      } else {
        checks[[length(checks) + 1]] <- tags$div(
          style = "color: #FF9800;", icon("exclamation-triangle"),
          paste0(" Low replicates for ELISA control(s): ", paste(low_rep_controls, collapse = ", "), " (recommend >= 2)"))
      }
    }

    # Check 9: Missing tissue weights when tissue normalization expected
    if (assay == "elisa" && !is.null(type_mat)) {
      type_vec <- as.character(unlist(type_mat))
      has_samples <- any(type_vec == "Sample", na.rm = TRUE)
      tw <- shared$tissue_weights_rv()
      has_weights <- length(tw) > 0 && any(sapply(tw, function(x) !is.null(x) && x > 0))
      if (has_samples && !has_weights) {
        checks[[length(checks) + 1]] <- tags$div(
          style = "color: #FF9800;", icon("exclamation-triangle"),
          " No tissue weights entered (needed for tissue-normalized ELISA results)")
      }
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
  # Report Generation (orchestrator — see report_pipeline.R for stage helpers)
  # --------------------------------------------------------------------------

  observeEvent(input$convert, {

    withProgress(message = "Generating report...", value = 0, {

      # Stage 1: Flush pending layout state
      flush_latest_layout_state(input, shared)

      # Stage 2: Build long-format data
      df_long <- build_long_data(shared, config_reactives$std_conc_raw())
      incProgress(0.2, detail = "Validating data...")

      # Stage 3: Normalize
      df_normalized <- normalize_assay_data(
        df_long, input$assay_type, shared$matrix_type(), session
      )
      if (is.null(df_normalized)) return()
      incProgress(0.4, detail = "Saving data files...")

      # Stage 4: Collect configuration and save artifacts
      assay <- input$assay_type
      sel_weights <- input$regression_weight
      if (is.null(sel_weights) || length(sel_weights) == 0) sel_weights <- "none"

      artifact_config <- list(
        csv_path       = session$userData$csv_path,
        output_dir     = session$userData$output_dir,
        fmt_json       = session$userData$fmt_json,
        notes_file     = session$userData$notes_file,
        export_formats = input$export_formats,
        notes          = input$notes %||% "",
        assay_type     = assay,
        is_multiwavelength = isTRUE(shared$rv$is_multiwavelength),
        wavelengths        = shared$rv$wavelengths,
        wavelength_plates  = shared$rv$wavelength_plates,
        matrix_type      = shared$matrix_type(),
        matrix_id        = shared$matrix_id(),
        matrix_dilution  = shared$matrix_dilution(),
        matrix_replicate = shared$matrix_replicate(),
        std_conc_raw     = config_reactives$std_conc_raw(),
        qc_params = if (assay == "rba") {
          list(qc_concentration = input$qc_conc, expected_hill = input$expected_hill,
               assay_type = assay, detection_method = "radioligand",
               analyte = config_reactives$chosen_standard_label())
        } else {
          list(assay_type = assay, detection_method = "absorbance",
               analyte = input$elisa_analyte, units = input$elisa_units %||% "pg/mL",
               normalization = "percent_b_b0")
        },
        assay_config = if (assay == "elisa") {
          list(assay_type = "elisa", analyte = input$elisa_analyte,
               units = input$elisa_units %||% "pg/mL", detection_method = "absorbance")
        } else {
          list(assay_type = "rba", toxin_class = input$toxin_class,
               toxin_variant = input$toxin_variant %||% NA,
               toxin_standard_label = config_reactives$chosen_standard_label(),
               molecular_weight_g_mol = shared$mw_g_mol(),
               detection_method = "radioligand", units = "mol/L")
        },
        analysis_config = list(
          regression_weight = sel_weights,
          quant_range_min = input$quant_range_min %||% 20,
          quant_range_max = input$quant_range_max %||% 80,
          ci_method = input$ci_method %||% "t_dist",
          enable_outlier_detection = isTRUE(input$enable_outlier_detection),
          outlier_min_n = input$outlier_min_n %||% 3,
          normality_assumption = input$normality_assumption %||% "assume",
          cv_limit = input$cv_limit %||% 30
        ),
        report_language = input$report_language %||% "en",
        tissue_weights  = if (assay == "elisa") shared$tissue_weights_rv() else NULL
      )

      save_analysis_artifacts(df_normalized, artifact_config, session)
      incProgress(0.7, detail = "Rendering report (this may take a minute)...")

      # Stage 5: Render reports
      render_reports(
        params = list(
          output_dir         = session$userData$output_dir,
          report_lang        = input$report_language %||% "en",
          is_multiwavelength = isTRUE(shared$rv$is_multiwavelength),
          wavelengths        = shared$rv$wavelengths,
          selected_formats   = input$export_formats,
          csv_path           = session$userData$csv_path
        ),
        session = session
      )

      incProgress(1, detail = "Done!")
    })
  })

}

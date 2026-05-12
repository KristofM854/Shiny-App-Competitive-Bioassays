# server_report.R
# ============================================================================
# Report generation and pre-flight validation logic extracted from app.R
# ============================================================================
# Pipeline stage helpers (flush_latest_layout_state, build_long_data,
# normalize_assay_data, save_analysis_artifacts, render_reports) are
# defined in report_pipeline.R.
#
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
    lang <- input$app_language %||% "en"
    raw <- input$expected_hill
    if (is.null(raw) || trimws(raw) == "") {
      return(tags$div(style = "color: red; font-weight: bold;",
                     tr("hill_required", lang)))
    }
    val <- suppressWarnings(as.numeric(raw))
    if (is.na(val)) {
      return(tags$div(style = "color: red; font-weight: bold;",
                     tr("must_be_numeric", lang)))
    }
    if (val < 0.5 || val > 1.5) {
      return(tags$div(style = "color: orange; font-weight: bold;",
                     tr("hill_outside_range", lang)))
    }
    NULL
  })

  # --------------------------------------------------------------------------
  # Dilution Error Feedback
  # --------------------------------------------------------------------------

  output$dilution_error_feedback <- renderUI({
    lang <- input$app_language %||% "en"
    if (shared$dilution_error()) {
      tags$div(style = "color: red; font-weight: bold;",
              tr("invalid_dilution", lang))
    }
  })

  # --------------------------------------------------------------------------
  # Dilution Fraction Label & Help (Phase 1.1: semantic relabeling)
  # --------------------------------------------------------------------------

  output$dilution_matrix_header <- renderUI({
    lang <- input$app_language %||% "en"
    h5(tr("dilution_matrix_label", lang))
  })

  output$dilution_set_all_label <- renderUI({
    lang <- input$app_language %||% "en"
    tags$label(tr("dilution_set_all_label", lang), `for` = "uniform_dilution",
               style = "margin: 0; white-space: nowrap;")
  })

  output$dilution_matrix_help <- renderUI({
    lang <- input$app_language %||% "en"
    tags$div(
      style = paste("background-color: #F5F5F5; padding: 8px; margin-top: 8px;",
                    "border-left: 4px solid #1976D2; font-size: 12px;"),
      tr("dilution_matrix_help", lang)
    )
  })

  # Phase 1.3: Extraction volume guidance shown below the tissue table
  output$extraction_volume_help <- renderUI({
    lang <- input$app_language %||% "en"
    tags$div(
      style = paste("background-color: #F5F5F5; padding: 8px; margin-top: 8px;",
                    "border-left: 4px solid #1976D2; font-size: 12px;"),
      tr("extraction_volume_help", lang)
    )
  })

  # Non-blocking warning when any well parses to > 1 and was not entered as
  # ratio (i.e., contains no ':'). Nudges users who typed "2" meaning "2-fold".
  output$dilution_gt1_warning <- renderUI({
    raw <- shared$raw_matrix_dilution()
    if (is.null(raw)) return(NULL)
    cells <- as.character(unlist(raw))
    numeric_cells <- suppressWarnings(as.numeric(cells))
    has_gt1 <- !is.na(numeric_cells) & numeric_cells > 1 & !grepl(":", cells)
    if (!any(has_gt1)) return(NULL)
    lang <- input$app_language %||% "en"
    tags$div(
      style = paste("background-color: #FFF4E5; padding: 8px; margin: 8px 0;",
                    "border-left: 4px solid #FF9800; font-size: 12px;",
                    "color: #663C00;"),
      tr("dilution_gt1_warning", lang)
    )
  })

  # --------------------------------------------------------------------------
  # Pre-Flight Check Panel
  # --------------------------------------------------------------------------

  output$preflight_checks <- renderUI({
    lang <- input$app_language %||% "en"
    assay <- input$assay_type %||% "rba"
    checks <- list()

    # Check 1: Plate data uploaded
    plate <- shared$matrix_measresults()
    plate_ok <- !is.null(plate) && any(!is.na(as.numeric(unlist(plate))))
    checks[[length(checks) + 1]] <- if (plate_ok) {
      pf_line("check-circle", "#388E3C", "pf_plate_ok", lang)
    } else {
      pf_line("times-circle", "#D32F2F", "pf_plate_missing", lang)
    }

    # Check 2: Standards defined
    type_mat <- shared$matrix_type()
    n_std <- if (!is.null(type_mat)) sum(unlist(type_mat) == "Standard", na.rm = TRUE) else 0
    checks[[length(checks) + 1]] <- if (n_std >= 4) {
      pf_line("check-circle", "#388E3C", "pf_std_count_ok", lang, n_std)
    } else {
      pf_line("times-circle", "#D32F2F", "pf_std_count_low", lang, n_std)
    }

    # Check 3: ELISA controls
    if (assay == "elisa" && !is.null(type_mat)) {
      well_types <- as.character(unlist(type_mat))
      has_blank <- "Blank" %in% well_types
      has_nsb <- "NSB" %in% well_types
      has_b0 <- "B0" %in% well_types
      if (has_blank && has_nsb && has_b0) {
        checks[[length(checks) + 1]] <- pf_line("check-circle", "#388E3C",
                                                      "pf_elisa_controls_ok", lang)
      } else {
        missing <- c()
        if (!has_blank) missing <- c(missing, "Blank")
        if (!has_nsb) missing <- c(missing, "NSB")
        if (!has_b0) missing <- c(missing, "B0")
        checks[[length(checks) + 1]] <- pf_line("times-circle", "#D32F2F",
                                                      "pf_elisa_controls_missing", lang,
                                                      paste(missing, collapse = ", "))
      }
    }

    # Check 4: Dilution validity
    dil_ok <- !shared$dilution_error()
    checks[[length(checks) + 1]] <- if (dil_ok) {
      pf_line("check-circle", "#388E3C", "pf_dilution_ok", lang)
    } else {
      pf_line("exclamation-triangle", "#FF9800", "pf_dilution_invalid", lang)
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
        checks[[length(checks) + 1]] <- pf_line("check-circle", "#388E3C",
                                                      "pf_std_count_match", lang,
                                                      unique_std_groups, num_std_input)
      } else {
        checks[[length(checks) + 1]] <- pf_line("exclamation-triangle", "#FF9800",
                                                      "pf_std_count_mismatch", lang,
                                                      unique_std_groups, num_std_input)
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
          checks[[length(checks) + 1]] <- pf_line("check-circle", "#388E3C",
                                                        "pf_rep_groups_ok", lang)
        } else {
          checks[[length(checks) + 1]] <- pf_line("exclamation-triangle", "#FF9800",
                                                        "pf_rep_groups_mixed", lang,
                                                        paste(mixed_groups, collapse = ", "))
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
          checks[[length(checks) + 1]] <- pf_line("check-circle", "#388E3C",
                                                        "pf_ids_ok", lang)
        } else {
          checks[[length(checks) + 1]] <- pf_line("exclamation-triangle", "#FF9800",
                                                        "pf_ids_empty", lang, empty_ids)
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
          checks[[length(checks) + 1]] <- pf_line("check-circle", "#388E3C",
                                                        "pf_elisa_reps_ok", lang)
        }
      } else {
        checks[[length(checks) + 1]] <- pf_line("exclamation-triangle", "#FF9800",
                                                      "pf_elisa_reps_low", lang,
                                                      paste(low_rep_controls, collapse = ", "))
      }
    }

    # Check 9: Missing tissue weights when tissue normalization expected
    if (assay == "elisa" && !is.null(type_mat)) {
      type_vec <- as.character(unlist(type_mat))
      has_samples <- any(type_vec == "Sample", na.rm = TRUE)
      tw <- shared$tissue_weights_rv()
      has_weights <- length(tw) > 0 && any(sapply(tw, function(x) {
        if (is.list(x)) !is.null(x$weight) && !is.na(x$weight) && x$weight > 0
        else !is.null(x) && !is.na(x) && x > 0
      }))
      if (has_samples && !has_weights) {
        checks[[length(checks) + 1]] <- pf_line("exclamation-triangle", "#FF9800",
                                                      "pf_tissue_missing", lang)
      }
    }

    # --- Summary badge showing overall severity ---
    has_errors <- any(sapply(checks, function(ch) grepl("#D32F2F", as.character(ch))))
    has_warnings <- any(sapply(checks, function(ch) grepl("#FF9800", as.character(ch))))

    if (has_errors) {
      badge <- tags$div(
        style = "padding: 8px 12px; margin-bottom: 10px; border-radius: 6px; background: #FFEBEE; border-left: 4px solid #D32F2F; font-weight: bold; color: #C62828;",
        icon("exclamation-circle"), " ", tr("pf_badge_errors", lang)
      )
    } else if (has_warnings) {
      badge <- tags$div(
        style = "padding: 8px 12px; margin-bottom: 10px; border-radius: 6px; background: #FFF3E0; border-left: 4px solid #FF9800; font-weight: bold; color: #E65100;",
        icon("exclamation-triangle"), " ", tr("pf_badge_warnings", lang)
      )
    } else {
      badge <- tags$div(
        style = "padding: 8px 12px; margin-bottom: 10px; border-radius: 6px; background: #E8F5E9; border-left: 4px solid #388E3C; font-weight: bold; color: #2E7D32;",
        icon("check-circle"), " ", tr("pf_badge_ok", lang)
      )
    }

    # Drive the Generate Report button: disable when any check is blocking.
    if (has_errors) {
      shinyjs::disable("convert")
    } else {
      shinyjs::enable("convert")
    }

    do.call(tagList, c(list(badge), checks))
  })

  # --------------------------------------------------------------------------
  # Convert Button Enable/Disable
  # --------------------------------------------------------------------------

  observe({
    assay <- input$assay_type %||% "rba"

    # Task 2: also re-fire whenever the user lands on a different wizard tab.
    # The button can drift to "disabled" when shared$dilution_error() flips
    # transiently during normalization or layout edits while preflight stays
    # green; depending on tab navigation alone for a fresh evaluation
    # ensures the convert button always reflects the latest state when the
    # user is looking at it on tab 5.
    invisible(input$wizard_tabs)

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
    lang <- input$app_language %||% "en"

    # A3: disable button immediately to prevent double-clicks
    shinyjs::disable("convert")
    updateActionButton(session, "convert",
      label = tr("rendering_report_btn", lang),
      icon  = icon("spinner", class = "fa-spin"))
    shared$rv$analysis_state <- "running"

    # A3: single persistent notification, updated through pipeline stages
    .notif_id <- "report_progress"

    # Initialised here so they remain accessible after withProgress exits.
    # .stats_env is an R environment passed as an Rmd param; the template
    # assigns model_stats into it directly so we avoid a fragile JSON round-trip.
    .stats_env        <- new.env(parent = emptyenv())
    .render_paths     <- list()
    .final_output_dir <- NULL
    .captured_std_range <- NULL

    tryCatch({
      withProgress(message = tr("generating_report", lang), value = 0, {

        showNotification(tr("notif_validating", lang),
                         id = .notif_id, duration = NULL, type = "message")

        # Task 1: every Generate Report click writes into a fresh
        # run_HHMMSS/ subdir under the session container so a second click
        # does not overwrite the first. session_root was captured at session
        # start (app.R) and stays put for the whole session; output_dir +
        # csv_path are repointed at the new run dir for downstream stages.
        .session_root <- session$userData$session_root %||%
                         session$userData$output_dir
        .ts <- format(Sys.time(), "%H%M%S")
        .run_dir <- file.path(.session_root, paste0("run_", .ts))
        .suffix <- 1
        while (dir.exists(.run_dir)) {
          .run_dir <- file.path(.session_root,
                                paste0("run_", .ts, "_", .suffix))
          .suffix <- .suffix + 1
        }
        dir.create(.run_dir, recursive = TRUE, showWarnings = FALSE)
        session$userData$output_dir <- .run_dir
        session$userData$csv_path   <- file.path(.run_dir, "long_data_output.csv")
        message(sprintf("[generate-report] writing artifacts to %s", .run_dir))

        # Stage 1: Flush pending layout state
        flush_latest_layout_state(input, shared)

        # Stage 2: Build long-format data
        df_long <- build_long_data(shared, config_reactives$std_conc_raw())
        incProgress(0.2)

        showNotification(tr("notif_fitting", lang),
                         id = .notif_id, duration = NULL, type = "message")

        # Stage 3: Normalize
        df_normalized <- normalize_assay_data(
          df_long, input$assay_type, shared$matrix_type(), session
        )
        if (is.null(df_normalized)) {
          removeNotification(.notif_id)
          shared$rv$analysis_state <- "failed"
          return()
        }
        incProgress(0.4)

        showNotification(tr("notif_quantifying", lang),
                         id = .notif_id, duration = NULL, type = "message")

        # Stage 4: Collect configuration and save artifacts
        assay <- input$assay_type
        sel_weights <- input$regression_weight
        if (is.null(sel_weights) || length(sel_weights) == 0) sel_weights <- "none"

        artifact_config <- list(
          csv_path       = session$userData$csv_path,
          output_dir     = session$userData$output_dir,
          # Task 1: redirect the two JSON sidecars into the per-run dir so each
          # run is a self-contained bundle (instead of every run overwriting
          # the session-root copies).
          fmt_json       = file.path(session$userData$output_dir,
                                     basename(session$userData$fmt_json)),
          notes_file     = file.path(session$userData$output_dir,
                                     basename(session$userData$notes_file)),
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
          # #4: report_languages is a vector of "en" / "es" the user ticked. If
          # they unticked both we fall back to "en" so a render still happens.
          report_languages = if (length(input$report_languages) > 0)
                               input$report_languages else "en",
          tissue_weights   = if (assay == "elisa") shared$tissue_weights_rv() else NULL
        )

        # Cache std range — captured as a plain value for post-withProgress assignment
        std_raw <- config_reactives$std_conc_raw()
        if (!is.null(std_raw) && length(std_raw) > 0) {
          .captured_std_range <- list(
            min = min(std_raw, na.rm = TRUE),
            max = max(std_raw, na.rm = TRUE)
          )
        }

        save_analysis_artifacts(df_normalized, artifact_config, session)
        incProgress(0.7)

        showNotification(tr("notif_rendering", lang),
                         id = .notif_id, duration = NULL, type = "message")

        # Stage 5: Render reports (always generate both compact + detailed).
        # stats_env is passed through to the Rmd; the template assigns
        # model_stats into it directly (env-handoff, avoids JSON round-trip).
        .render_paths <- render_reports(
          params = list(
            output_dir         = session$userData$output_dir,
            report_langs       = if (length(input$report_languages) > 0)
                                   input$report_languages else "en",
            is_multiwavelength = isTRUE(shared$rv$is_multiwavelength),
            wavelengths        = shared$rv$wavelengths,
            selected_formats   = input$export_formats,
            generate_compact   = TRUE,
            csv_path           = session$userData$csv_path,
            stats_env          = .stats_env
          ),
          session = session
        )

        incProgress(1)
        removeNotification(.notif_id)

        # Capture output_dir for post-withProgress reactive assignments (Item 5).
        .final_output_dir <- session$userData$output_dir
      })

      # Set reactive state AFTER withProgress exits — reactive assignments here
      # invalidate cleanly outside the withProgress context.

      # KPI strip: prefer the env-handoff (model_stats assigned by the Rmd
      # into .stats_env); fall back to reading model_stats.json if the env
      # is empty (e.g. older code path or multi-wavelength template).
      if (exists("model_stats", envir = .stats_env, inherits = FALSE)) {
        shared$rv$last_model_stats <- .stats_env$model_stats
        message("[kpi-strip] model_stats populated via env-handoff: r2=",
                .stats_env$model_stats$r_squared %||% "NULL",
                " rmse=", .stats_env$model_stats$rmse %||% "NULL")
      } else {
        .stats_path <- file.path(.final_output_dir, "model_stats.json")
        if (file.exists(.stats_path)) {
          shared$rv$last_model_stats <- tryCatch(
            jsonlite::fromJSON(.stats_path, simplifyVector = TRUE),
            error = function(e) {
              message("[kpi-strip] JSON read failed: ", e$message); NULL
            }
          )
        } else {
          message("[kpi-strip] No model_stats found — KPI strip will stay empty. ",
                  "Check that the save-model-stats chunk ran at: ", .final_output_dir)
        }
      }

      if (!is.null(.captured_std_range)) {
        shared$rv$last_std_range <- .captured_std_range
      }
      shared$rv$last_render_paths <- .render_paths
      shared$rv$last_report_dir   <- .final_output_dir
      shared$rv$analysis_state    <- "done"

    }, error = function(e) {
      removeNotification(.notif_id)
      shared$rv$analysis_state <- "failed"
      showNotification(paste("Report generation failed:", e$message),
                       type = "error", duration = 10)
      message("[generate-report] ERROR: ", e$message)

    }, finally = {
      # A3: always re-enable the button
      shinyjs::enable("convert")
      updateActionButton(session, "convert",
        label = tr("generate_report", lang),
        icon  = icon("file-arrow-down"))
    })
  })

}

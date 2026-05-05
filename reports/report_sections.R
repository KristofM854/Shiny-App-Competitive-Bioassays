# =============================================================================
# reports/report_sections.R
#
# Presentation-layer helpers for the analysis Rmd templates. Each function
# here accepts already-computed data frames and emits markdown via cat() and
# render_table() (IMPLEMENTATION_PLAN.md §M1.6).
#
# Status:
#   render_exclusion_audit_section()         - extracted
#   render_tissue_normalization_section()    - extracted
#
# Pending (deferred to a session where rmarkdown::render() can run end-to-end
# to verify byte-identical output):
#   render_executive_summary_section()
#   render_qc_traffic_light_section()
# =============================================================================

# Render the "Exclusion Audit" section. Mirrors the body of the exclusion-audit
# chunk in unified_analysis_template.Rmd.
#
# Uses build_exclusion_audit() from reports/report_functions.R and render_table()
# also from there. Wraps the work in tryCatch() so a malformed exclusion record
# never crashes the report.
render_exclusion_audit_section <- function(data_long,
                                           outlier_flags = NULL,
                                           high_var_standards = NULL,
                                           lang = "en") {
  tryCatch({
    ol_flags_for_audit <- if (!is.null(outlier_flags) &&
                              is.data.frame(outlier_flags) &&
                              nrow(outlier_flags) > 0) {
      outlier_flags
    } else {
      NULL
    }

    hv_for_audit <- if (!is.null(high_var_standards) &&
                        is.data.frame(high_var_standards) &&
                        nrow(high_var_standards) > 0) {
      high_var_standards
    } else {
      NULL
    }

    exclusion_audit <- build_exclusion_audit(
      data_long = data_long,
      high_var_standards = hv_for_audit,
      outlier_flags = ol_flags_for_audit
    )

    if (nrow(exclusion_audit) > 0) {
      cat(sprintf("**%d exclusion(s) documented across %d unique well(s).**\n\n",
                  nrow(exclusion_audit), length(unique(exclusion_audit$Well))))
      render_table(exclusion_audit, caption = tr("exclusion_audit_caption", lang))
    } else {
      cat("*No exclusions were applied during this analysis.*\n\n")
    }
  }, error = function(e) {
    cat(sprintf("\n\n*Exclusion audit could not be generated: %s*\n\n",
                e$message))
  })
  invisible(NULL)
}


# Render the "Tissue Normalization Method" section (ELISA + tissue only).
# Mirrors the body of the tissue-normalization-traceability chunk in
# unified_analysis_template.Rmd, including the worked-example block.
#
# Args:
#   is_elisa            logical scalar
#   tissue_weights      named list (per-replicate) of either numeric weights
#                       or list(weight=, extraction_uL=)
#   processing_config   list with optional extraction_volume_ul element
#   replicate_stats     data frame with Replicate, mean_conc, and (optionally)
#                       concentration_pg_per_g columns
#   lang                language code consumed by tr()
render_tissue_normalization_section <- function(is_elisa,
                                                tissue_weights,
                                                processing_config,
                                                replicate_stats,
                                                lang) {
  tryCatch({
    if (isTRUE(is_elisa) && !is.null(tissue_weights) &&
        length(tissue_weights) > 0) {
      section_start(tr("section_tissue_norm", lang))
      emit_heading(tr("section_tissue_norm", lang), 4)

      cat(tr("tissue_norm_formula_intro", lang), "\n\n", sep = "")
      cat("$$C_{tissue}\\ (\\mathrm{pg/g}) = \\frac{C_{extract}\\ (\\mathrm{pg/mL}) \\times \\left[V_{extraction}\\ (\\mathrm{\\mu L}) \\,/\\, 1000\\right]}{m_{tissue}\\ (\\mathrm{mg}) \\,/\\, 1000}$$\n\n")
      cat(tr("where_label", lang), "\n\n")
      cat("- $C_{extract}$ = concentration in the original (undiluted) extract, from the curve (pg/mL)\n")
      cat("- $V_{extraction}$ = total volume the tissue was extracted into (µL, converted to mL by /1000)\n")
      cat("- $m_{tissue}$ = mass of tissue extracted (mg, converted to g by /1000)\n")
      cat("- $C_{tissue}$ = final concentration (pg/g tissue)\n\n")

      cat("> ", tr("extraction_volume_report_note", lang), "\n\n", sep = "")

      tw_trace <- data.frame(
        Replicate = names(tissue_weights),
        stringsAsFactors = FALSE
      )
      tw_trace$TissueMass_mg <- sapply(tissue_weights, function(x) {
        if (is.list(x)) as.numeric(x$weight) else as.numeric(x)
      })
      tw_trace$Extraction_uL <- sapply(tissue_weights, function(x) {
        if (is.list(x) && !is.null(x$extraction_uL)) as.numeric(x$extraction_uL)
        else processing_config$extraction_volume_ul %||% 500
      })
      tw_trace <- tw_trace[!is.na(tw_trace$TissueMass_mg) &
                             tw_trace$TissueMass_mg > 0, , drop = FALSE]

      if (nrow(tw_trace) > 0) {
        tw_trace$TissueMass_g <- tw_trace$TissueMass_mg / 1000
        tw_trace$Extraction_mL <- tw_trace$Extraction_uL / 1000

        display_tw <- tw_trace %>%
          dplyr::select(Replicate, TissueMass_mg, Extraction_uL,
                        TissueMass_g, Extraction_mL)
        names(display_tw) <- c(tr("col_replicate_group", lang),
                               tr("col_tissue_mass_mg", lang),
                               tr("col_extraction_vol_ul", lang),
                               tr("col_tissue_mass_g", lang),
                               tr("col_extraction_vol_ml", lang))
        render_table(display_tw, caption = tr("tissue_norm_params_caption", lang))

        cat(sprintf("\n**Units:** Final tissue concentrations are reported as pg/g tissue (= pg analyte per gram of tissue).\n\n"))

        if (!is.null(replicate_stats) &&
            is.data.frame(replicate_stats) &&
            nrow(replicate_stats) > 0 &&
            all(c("Replicate", "mean_conc") %in% names(replicate_stats)) &&
            "concentration_pg_per_g" %in% names(replicate_stats)) {
          tw_example <- replicate_stats[
            replicate_stats$Replicate %in% tw_trace$Replicate &
              !is.na(replicate_stats$mean_conc) &
              !is.na(replicate_stats$concentration_pg_per_g), , drop = FALSE]
          if (nrow(tw_example) > 0) {
            ex <- tw_example[1, , drop = FALSE]
            ex_trace <- tw_trace[tw_trace$Replicate == ex$Replicate, ,
                                 drop = FALSE][1, , drop = FALSE]
            c_extract <- as.numeric(ex$mean_conc)
            v_ul <- as.numeric(ex_trace$Extraction_uL)
            m_mg <- as.numeric(ex_trace$TissueMass_mg)
            v_ml <- v_ul / 1000
            m_g <- m_mg / 1000
            amount_pg <- c_extract * v_ml
            c_tissue <- amount_pg / m_g
            cat("**Worked example (replicate group `",
                as.character(ex$Replicate), "`):**\n\n", sep = "")
            cat(sprintf(
              "1. $C_{extract}$ = %s pg/mL (from the fitted curve)\n",
              format(c_extract, big.mark = ",", scientific = FALSE)))
            cat(sprintf(
              "2. $V_{extraction}$ = %s µL = %s mL (divide by 1000)\n",
              format(v_ul, big.mark = ",", scientific = FALSE),
              format(v_ml, big.mark = ",", scientific = FALSE)))
            cat(sprintf(
              "3. $m_{tissue}$ = %s mg = %s g (divide by 1000)\n",
              format(m_mg, big.mark = ",", scientific = FALSE),
              format(m_g, big.mark = ",", scientific = FALSE)))
            cat(sprintf(
              "4. Amount in extract = %s pg/mL × %s mL = %s pg\n",
              format(c_extract, big.mark = ",", scientific = FALSE),
              format(v_ml, big.mark = ",", scientific = FALSE),
              format(amount_pg, big.mark = ",", scientific = FALSE)))
            cat(sprintf(
              "5. $C_{tissue}$ = %s pg / %s g = **%s pg/g tissue**\n\n",
              format(amount_pg, big.mark = ",", scientific = FALSE),
              format(m_g, big.mark = ",", scientific = FALSE),
              format(c_tissue, big.mark = ",", scientific = FALSE)))
          }
        }
      }
      section_end()
    }
  }, error = function(e) {
    cat(sprintf("\n\n*Tissue normalization traceability could not be generated: %s*\n\n",
                e$message))
  })
  invisible(NULL)
}


# =============================================================================
# M1.6 (continued): exec-summary + QC traffic light extraction
# =============================================================================

# Pre-fit executive summary block (top of the report). Mirrors the body of
# the `executive-summary` chunk in unified_analysis_template.Rmd.
#
# Renders a small pre-fit info box: assay type, analyte, analysis date,
# standard concentrations count (with high-variability exclusion note), and
# total sample-well count. The post-fit overall-status box that lives
# further down in the report is rendered by render_overall_status_box().
render_pre_fit_executive_summary <- function(assay_config, data_long, cv_limit) {
  tryCatch({
    exec_assay_type <- toupper(assay_config$assay_type %||% "Unknown")
    exec_analyte <- assay_config$analyte %||%
                    assay_config$toxin_standard_label %||%
                    "Unknown"

    n_std_concs <- length(unique(
      data_long$StandardConc[data_long$SampleType == "Standard" &
                             !is.na(data_long$StandardConc)]
    ))
    n_hv <- nrow(identify_high_variability_standards(data_long,
                                                     cv_limit = cv_limit))
    std_note <- sprintf("<strong>Standards:</strong> %d concentration levels",
                        n_std_concs)
    if (n_hv > 0) {
      std_note <- paste0(std_note,
                         sprintf(" (%d excluded for high CV)", n_hv))
    }

    n_samples <- sum(data_long$SampleType == "Sample", na.rm = TRUE)

    lines <- c(
      '<strong style="font-size: 1.1em;">Executive Summary</strong><br><br>',
      sprintf("<strong>Assay:</strong> %s &mdash; %s<br>",
              exec_assay_type, exec_analyte),
      sprintf("<strong>Analysis date:</strong> %s<br>", Sys.Date()),
      paste0(std_note, "<br>"),
      sprintf("<strong>Sample wells:</strong> %d<br>", n_samples),
      '<em>Model results and overall status appear after curve fitting below.</em>'
    )

    emit_styled_block(
      lines,
      html_style = paste0("border: 2px solid #2196F3; padding: 15px; ",
                          "border-radius: 8px; background: #F5F9FF; ",
                          "margin: 10px 0 20px 0;")
    )
  }, error = function(e) {
    cat(sprintf("\n\n*Executive summary could not be generated: %s*\n\n",
                e$message))
  })
  invisible(NULL)
}


# Post-fit overall-status box. Mirrors the body of the `exec-summary-box`
# chunk in unified_analysis_template.Rmd. Renders the big green/yellow/red
# pass-fail card with R^2, RMSE, model description, EC20-EC80 quantifiable
# range, and (for ELISA) control-well summary.
render_overall_status_box <- function(drc_failed_completely, R2, RMSE,
                                      assay_config, all_models, primary_key,
                                      weight_desc, high_var_standards,
                                      standards_for_model, standards_data,
                                      ec20, ec80, is_elisa, data_long,
                                      lang, control_summary = NULL) {
  if (drc_failed_completely || is.null(R2) || is.na(R2)) {
    return(invisible(NULL))
  }

  qc_profile <- get_qc_profile(assay_config$assay_type %||% "rba")

  exec_warnings <- character(0)
  if (R2 >= 0.99) {
    overall_status <- "Pass"
    status_icon <- "\U0001F7E2"
  } else if (R2 >= qc_profile$r2_threshold) {
    overall_status <- "Qualified Pass"
    status_icon <- "\U0001F7E1"
  } else {
    overall_status <- "Review Required"
    status_icon <- "\U0001F534"
    # overall_status kept as English key for comparisons below
    exec_warnings <- c(exec_warnings,
                       sprintf("R² below threshold (%.4f < %.2f)",
                               R2, qc_profile$r2_threshold))
  }

  primary_model_entry <- if (!drc_failed_completely) all_models[[primary_key]] else NULL
  if (!is.null(primary_model_entry$convergence) &&
      !primary_model_entry$convergence$se_available) {
    exec_warnings <- c(exec_warnings, "Standard errors could not be computed")
    if (overall_status == "Pass") {
      overall_status <- "Qualified Pass"
      status_icon <- "\U0001F7E1"
    }
  }
  if (!is.null(primary_model_entry$lack_of_fit) &&
      !is.null(primary_model_entry$lack_of_fit$p_value) &&
      primary_model_entry$lack_of_fit$p_value < 0.05) {
    exec_warnings <- c(exec_warnings,
                       sprintf("Lack-of-fit (p = %.4f)",
                               primary_model_entry$lack_of_fit$p_value))
    if (overall_status == "Pass") {
      overall_status <- "Qualified Pass"
      status_icon <- "\U0001F7E1"
    }
  }
  if (nrow(high_var_standards) > 0) {
    exec_warnings <- c(exec_warnings,
                       sprintf("%d standard level(s) excluded for high CV",
                               nrow(high_var_standards)))
  }

  if (is_html_out()) {
    cat('<div style="background: linear-gradient(135deg, #E8F5E9 0%, #F1F8E9 100%); ',
        'padding: 16px 20px; border-radius: 8px; border-left: 4px solid #4CAF50; ',
        'margin: 15px 0 25px 0; font-size: 14px;">\n\n')
  } else {
    cat("---\n\n")
  }

  overall_status_display <- switch(overall_status,
    "Pass"             = tr("qc_green", lang),
    "Qualified Pass"   = tr("qc_qualified", lang),
    "Review Required"  = tr("qc_review", lang),
    overall_status)
  cat(sprintf("**Overall status:** %s %s\n\n", status_icon, overall_status_display))
  cat(sprintf("**Assay:** %s — %s | **Model:** %s\n\n",
              toupper(assay_config$assay_type %||% "Unknown"),
              assay_config$analyte %||%
                assay_config$toxin_standard_label %||% "Unknown",
              weight_desc))
  cat(sprintf("**Curve fit:** R² = %.4f | RMSE = %.3f\n\n", R2, RMSE))

  cat(sprintf("**Standards:** %d wells used (of %d unique concentrations)",
              nrow(standards_for_model),
              length(unique(standards_data$StandardConc))))
  if (nrow(high_var_standards) > 0) {
    cat(sprintf(" | %d excluded", nrow(high_var_standards)))
  }
  cat("\n\n")

  if (!is.null(ec20) && !is.null(ec80) && !is.na(ec20) && !is.na(ec80)) {
    conc_unit <- if (isTRUE(is_elisa)) assay_config$units %||% "pg/mL" else "mol/L"
    lloq_fmt <- if (isTRUE(is_elisa)) format(ec20, digits = 4, big.mark = ",")
                else format(ec20, scientific = TRUE, digits = 3)
    uloq_fmt <- if (isTRUE(is_elisa)) format(ec80, digits = 4, big.mark = ",")
                else format(ec80, scientific = TRUE, digits = 3)
    cat(sprintf("**Quantifiable range:** LLOQ = %s %s | ULOQ = %s %s\n\n",
                lloq_fmt, conc_unit, uloq_fmt, conc_unit))
  }

  n_sample_wells <- sum(data_long$SampleType == "Sample", na.rm = TRUE)
  cat(sprintf("**Sample wells:** %d\n\n", n_sample_wells))

  if (isTRUE(is_elisa) && !is.null(control_summary)) {
    cat(sprintf("**Controls:** Blank=%.3f | NSB=%.3f | B0=%.3f | Hierarchy: %s\n\n",
                as.numeric(control_summary$blank_avg),
                as.numeric(control_summary$nsb_avg),
                as.numeric(control_summary$b0_avg),
                if (control_summary$hierarchy_valid) tr("control_hierarchy_valid", lang) else paste0("**", tr("control_hierarchy_invalid", lang), "**")))
  }

  if (length(exec_warnings) > 0) {
    cat(sprintf("**Top warnings:** %s\n\n",
                paste(exec_warnings, collapse = "; ")))
  }

  if (is_html_out()) {
    cat('</div>\n\n')
  } else {
    cat("---\n\n")
  }

  invisible(NULL)
}


# One-line overall accuracy summary appended after the status box. Mirrors
# the body of the `exec-summary-accuracy` chunk in
# unified_analysis_template.Rmd. Quietly does nothing if recovery_summary
# was not produced (e.g. compact mode without standard back-calculation).
render_overall_accuracy <- function(recovery_summary) {
  if (is.null(recovery_summary) || !is.data.frame(recovery_summary) ||
      nrow(recovery_summary) == 0) {
    return(invisible(NULL))
  }
  mean_recovery <- mean(recovery_summary$mean_recovery, na.rm = TRUE)
  sd_recovery   <- sd(recovery_summary$mean_recovery,   na.rm = TRUE)
  n_levels      <- sum(!is.na(recovery_summary$mean_recovery))
  cat(sprintf("**Overall accuracy:** Mean recovery %.1f%% ± %.1f%% across %d standard levels\n\n",
              mean_recovery, sd_recovery, n_levels))
  invisible(NULL)
}


# Bullet list of high-variability standard concentrations (CV > limit).
# Mirrors the body of the `variability-info` chunk in
# unified_analysis_template.Rmd. Falls back to an "all standards
# acceptable" line when the input is empty.
render_high_variability_info <- function(high_var_standards, is_elisa,
                                         assay_config, lang) {
  if (!is.null(high_var_standards) &&
      is.data.frame(high_var_standards) &&
      nrow(high_var_standards) > 0) {
    cat("**", tr("high_var_standards", lang), "**\n\n", sep = "")
    for (i in seq_len(nrow(high_var_standards))) {
      if (isTRUE(is_elisa)) {
        cat(sprintf("- Concentration %.1f %s: %.1f%% CV\n",
                    high_var_standards$StandardConc[i],
                    assay_config$units %||% "pg/mL",
                    high_var_standards$cv[i]))
      } else {
        cat(sprintf("- Concentration %.2e mol/L: %.1f%% CV\n",
                    high_var_standards$StandardConc[i],
                    high_var_standards$cv[i]))
      }
    }
    cat("\n")
  } else {
    cat(tr("all_std_acceptable", lang), "\n\n")
  }
  invisible(NULL)
}


# QC traffic-light card (R^2 / Hill / max CV / mean recovery, each with a
# green/amber/red emoji). Mirrors the body of the `qc-traffic-light` chunk
# in unified_analysis_template.Rmd. Wraps the whole block in tryCatch so a
# malformed model_fit (e.g. interpolation fallback with no coef()) cannot
# crash the report; instead a one-line "QC summary could not be generated"
# message is emitted.
render_qc_traffic_light_section <- function(model_fit, standards_for_model,
                                            response_var, replicate_stats,
                                            R2, weight_desc, assay_config,
                                            lang) {
  section_start(tr("qc_card_title", lang))
  tryCatch({
    coefs <- coef(model_fit)
    hill_slope <- abs(coefs[1])

    std_backcalc <- tryCatch({
      pred_conc <- predict(model_fit,
                           newdata = data.frame(concentration = standards_for_model$concentration),
                           type = "response")
      data.frame(
        nominal            = standards_for_model$concentration,
        predicted_response = pred_conc,
        observed_response  = standards_for_model[[response_var]]
      )
    }, error = function(e) NULL)

    mean_recovery <- NA_real_
    if (!is.null(std_backcalc)) {
      backcalc_conc <- tryCatch({
        sapply(std_backcalc$observed_response, function(resp) {
          tryCatch({
            ed <- ED(model_fit, respLev = resp, type = "absolute",
                     display = FALSE)
            as.numeric(ed[1, 1])
          }, error = function(e) NA_real_)
        })
      }, error = function(e) rep(NA_real_, nrow(std_backcalc)))

      recovery_pct <- (backcalc_conc / std_backcalc$nominal) * 100
      recovery_pct <- recovery_pct[is.finite(recovery_pct) &
                                     recovery_pct > 0 &
                                     recovery_pct < 500]
      if (length(recovery_pct) > 0) mean_recovery <- mean(recovery_pct, na.rm = TRUE)
    }

    max_cv <- if (!is.null(replicate_stats) &&
                  is.data.frame(replicate_stats) &&
                  nrow(replicate_stats) > 0 &&
                  "cv_percent" %in% names(replicate_stats)) {
      max(replicate_stats$cv_percent, na.rm = TRUE)
    } else NA_real_

    tl_qc_profile <- get_qc_profile(assay_config$assay_type %||% "rba")
    tl_hill_range <- sort(abs(tl_qc_profile$hill_slope_range))
    tl_recovery   <- tl_qc_profile$recovery_range

    qc_items <- list(
      list(metric = tr("qc_r2", lang), value = round(R2, 4),
           status = if (R2 >= 0.99) "green"
                    else if (R2 >= tl_qc_profile$r2_threshold) "amber"
                    else "red"),
      list(metric = tr("qc_hill", lang), value = round(hill_slope, 3),
           status = if (hill_slope >= tl_hill_range[1] &&
                        hill_slope <= tl_hill_range[2]) "green"
                    else if (hill_slope >= tl_hill_range[1] * 0.6 &&
                             hill_slope <= tl_hill_range[2] * 1.3) "amber"
                    else "red")
    )
    if (!is.na(max_cv) && is.finite(max_cv)) {
      qc_items[[length(qc_items) + 1]] <- list(
        metric = tr("qc_max_cv", lang),
        value = paste0(round(max_cv, 1), "%"),
        status = if (max_cv <= tl_qc_profile$cv_limit) "green"
                 else if (max_cv <= tl_qc_profile$cv_limit * 2) "amber"
                 else "red"
      )
    }
    if (!is.na(mean_recovery) && is.finite(mean_recovery)) {
      rec_tight_lo <- tl_recovery[1] + (100 - tl_recovery[1]) * 0.5
      rec_tight_hi <- tl_recovery[2] - (tl_recovery[2] - 100) * 0.5
      qc_items[[length(qc_items) + 1]] <- list(
        metric = tr("qc_recovery", lang),
        value = paste0(round(mean_recovery, 1), "%"),
        status = if (mean_recovery >= rec_tight_lo &&
                     mean_recovery <= rec_tight_hi) "green"
                 else if (mean_recovery >= tl_recovery[1] &&
                          mean_recovery <= tl_recovery[2]) "amber"
                 else "red"
      )
    }

    status_icon <- function(s) switch(s, green = "\U0001F7E2",
                                      amber = "\U0001F7E1",
                                      red = "\U0001F534", "❓")
    status_label <- function(s) switch(s, green = tr("qc_green", lang),
                                       amber = tr("qc_amber", lang),
                                       red = tr("qc_red", lang), "?")

    qc_df <- data.frame(
      Metric = sapply(qc_items, function(x) x$metric),
      Value  = sapply(qc_items, function(x) as.character(x$value)),
      Status = sapply(qc_items,
                      function(x) paste(status_icon(x$status),
                                        status_label(x$status))),
      stringsAsFactors = FALSE
    )
    names(qc_df) <- c(tr("qc_metric", lang), tr("qc_value", lang),
                      tr("qc_status", lang))

    emit_heading(tr("qc_card_title", lang), 4)
    cat(sprintf("*%s: %s*\n\n", tr("regression_label", lang), weight_desc))
    render_table(qc_df, caption = tr("qc_card_title", lang))
  }, error = function(e) {
    cat("\n\n*QC summary could not be generated.*\n\n")
  })
  section_end()
  invisible(NULL)
}


# =============================================================================
# GUI refresh §6.2: KPI strip
# =============================================================================

# Render the 5-tile KPI strip immediately under the report title:
#   [ R² · curve fit ] [ RMSE ] [ LLOQ ] [ ULOQ ] [ ● Overall ]
# The first four tiles are mono-numeric values; the last is a colored
# pass / warn / fail dot + word.
#
# Pulled values:
#   R2, RMSE                 - from fit_all_models() result
#   formal_lloq, formal_uloq - from determine_lloq_uloq() result
#   overall_status           - same R^2 / threshold logic as the
#                              render_overall_status_box() helper
#
# Inputs that may be NA (LLOQ / ULOQ when too few standards pass the
# accuracy gate) render as an em-dash in the tile so the layout stays
# stable across runs.
render_kpi_strip <- function(R2, RMSE, formal_lloq, formal_uloq,
                             assay_config, is_elisa) {
  fmt_or_dash <- function(x, fn = identity) {
    if (is.null(x) || is.na(x) || !is.finite(x)) return("—")
    fn(x)
  }
  fmt_r2   <- fmt_or_dash(R2,   function(v) sprintf("%.4f", v))
  fmt_rmse <- fmt_or_dash(RMSE, function(v) sprintf("%.3f", v))
  conc_unit <- if (isTRUE(is_elisa)) assay_config$units %||% "pg/mL" else "mol/L"
  fmt_conc <- function(v) {
    if (is.null(v) || is.na(v) || !is.finite(v)) return("—")
    if (isTRUE(is_elisa)) format(v, digits = 4, big.mark = ",")
    else                  format(v, scientific = TRUE, digits = 3)
  }
  fmt_lloq <- fmt_conc(formal_lloq)
  fmt_uloq <- fmt_conc(formal_uloq)

  qc_profile <- get_qc_profile(assay_config$assay_type %||% "rba")
  if (is.null(R2) || is.na(R2)) {
    overall_word <- "Unknown"; overall_role <- "ink-3"
  } else if (R2 >= 0.99) {
    overall_word <- "Pass"; overall_role <- "pass"
  } else if (R2 >= qc_profile$r2_threshold) {
    overall_word <- "Qualified"; overall_role <- "warn"
  } else {
    overall_word <- "Review"; overall_role <- "fail"
  }

  # HTML strip: 5 tiles in a flex row. CSS (var(--c-pass) etc.) lives
  # in www/style.css; for the report's stand-alone HTML the colors are
  # also re-declared inline so the strip looks right when emailed or
  # printed without the app stylesheet.
  cat('<div class="bs-kpi-strip" style="display: flex; gap: 12px; margin: 14px 0 22px 0; flex-wrap: wrap;">\n')

  tile <- function(label, value, unit = "", color_role = NULL) {
    border_left <- if (!is.null(color_role)) {
      sprintf("border-left: 4px solid var(--c-%s, #5b6573);", color_role)
    } else {
      ""
    }
    cat(sprintf(paste0(
      '<div class="bs-kpi-tile" style="flex: 1; min-width: 140px;',
      ' background: #fff; border: 1px solid #e3e6ea; %s',
      ' border-radius: 6px; padding: 10px 14px;">',
      '<div style="font-family: \'IBM Plex Mono\', monospace; font-size: 11px;',
      ' color: #5b6573; text-transform: uppercase; letter-spacing: 0.04em;">%s</div>',
      '<div style="font-family: \'IBM Plex Mono\', monospace; font-size: 22px;',
      ' font-weight: 500; color: #0f1722; margin-top: 2px;">%s</div>'),
      border_left, label, value))
    if (nzchar(unit)) {
      cat(sprintf(paste0(
        '<div style="font-family: \'IBM Plex Mono\', monospace; font-size: 11px;',
        ' color: #8a93a0;">%s</div>'), unit))
    }
    cat('</div>\n')
  }

  tile("R² · curve fit", fmt_r2, "")
  tile("RMSE",                     fmt_rmse, "")
  tile("LLOQ",                     fmt_lloq, conc_unit)
  tile("ULOQ",                     fmt_uloq, conc_unit)

  # Status tile: colored dot + word
  dot_color <- switch(overall_role,
                      pass = "#009e73", warn = "#e69f00",
                      fail = "#d55e00", "#5b6573")
  cat(sprintf(paste0(
    '<div class="bs-kpi-tile" style="flex: 1; min-width: 140px;',
    ' background: #fff; border: 1px solid #e3e6ea;',
    ' border-left: 4px solid %s; border-radius: 6px; padding: 10px 14px;">',
    '<div style="font-family: \'IBM Plex Mono\', monospace; font-size: 11px;',
    ' color: #5b6573; text-transform: uppercase; letter-spacing: 0.04em;">Overall</div>',
    '<div style="font-size: 18px; font-weight: 500; color: #0f1722; margin-top: 2px;">',
    '<span style="color: %s;">●</span> %s</div></div>\n'),
    dot_color, dot_color, overall_word))

  cat('</div>\n')

  invisible(NULL)
}

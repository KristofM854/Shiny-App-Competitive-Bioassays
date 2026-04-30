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
                                           high_var_standards = NULL) {
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
      render_table(exclusion_audit, caption = "Exclusion Audit Trail")
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
      section_start("Click to expand tissue normalization traceability")
      emit_heading("Tissue Normalization Method", 4)

      cat("Tissue-based concentrations are calculated using the following formula ",
          "with explicit unit conversions:\n\n", sep = "")
      cat("$$C_{tissue}\\ (\\mathrm{pg/g}) = \\frac{C_{extract}\\ (\\mathrm{pg/mL}) \\times \\left[V_{extraction}\\ (\\mathrm{\\mu L}) \\,/\\, 1000\\right]}{m_{tissue}\\ (\\mathrm{mg}) \\,/\\, 1000}$$\n\n")
      cat("Where:\n\n")
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
        names(display_tw) <- c("Replicate Group", "Tissue Mass (mg)",
                               "Extraction Vol. (uL)",
                               "Tissue Mass (g)", "Extraction Vol. (mL)")
        render_table(display_tw, caption = "Tissue Normalization Parameters")

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

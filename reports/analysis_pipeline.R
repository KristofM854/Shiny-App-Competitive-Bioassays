# =============================================================================
# reports/analysis_pipeline.R
#
# Compute-layer for the analysis Rmd templates. Pure-compute helpers extracted
# from reports/unified_analysis_template.Rmd so the template shrinks to an
# orchestration + presentation layer (IMPLEMENTATION_PLAN.md, §M1).
#
# Each function in this file MUST preserve the numeric output of the
# pre-refactor template byte-for-byte (verified by tests/testthat/
# test-report-numbers.R for the RBA path, and by archived "before" reports
# for the ELISA path).
#
# Status:
#   M1.5 assess_plate_positional()   - implemented
#
#   M1.1 quantify_samples()          - pending
#   M1.2 fit_all_models()            - pending (folds in H2 auto-weighting)
#   M1.3 determine_lloq_uloq()       - pending
#   M1.4 compute_standard_recovery() - pending
# =============================================================================

# Plate positional QC: per-row and per-column response statistics, flagging
# rows or columns whose mean response deviates more than 2 SD from the
# plate-wide mean (a common edge-effect heuristic).
#
# Args:
#   data_long: long-format plate data with at minimum Row, Column, and
#              MeasurementValue columns. Row/Column are typically character.
#
# Returns:
#   NULL if `data_long` lacks MeasurementValue or has no finite values.
#   Otherwise a list:
#     plate_mean   numeric scalar, plate-wide mean of finite values
#     plate_sd     numeric scalar, plate-wide SD of finite values
#     row_stats    tibble: Row, Mean, SD, Flag  (Flag = "⚠" or "")
#     col_stats    tibble: Column, Mean, SD, Flag
#     flagged_rows integer count of rows with non-empty Flag
#     flagged_cols integer count of columns with non-empty Flag
#
# Mirrors the in-line computation of the `plate-positional-qc` chunk in
# reports/unified_analysis_template.Rmd. The Rmd remains responsible for
# rendering decisions (which table to print, what wording to emit).
assess_plate_positional <- function(data_long) {
  if (!is.data.frame(data_long) ||
      !"MeasurementValue" %in% names(data_long)) {
    return(NULL)
  }
  plate_data <- dplyr::filter(data_long, is.finite(.data$MeasurementValue))
  if (nrow(plate_data) == 0) return(NULL)

  plate_mean <- mean(plate_data$MeasurementValue, na.rm = TRUE)
  plate_sd   <- sd(plate_data$MeasurementValue,   na.rm = TRUE)

  flag_char <- "⚠"

  row_stats <- plate_data %>%
    dplyr::group_by(.data$Row) %>%
    dplyr::summarise(Mean = mean(.data$MeasurementValue, na.rm = TRUE),
                     SD   = sd(.data$MeasurementValue,   na.rm = TRUE),
                     .groups = "drop") %>%
    dplyr::mutate(Flag = ifelse(abs(.data$Mean - plate_mean) > 2 * plate_sd,
                                flag_char, ""))

  col_stats <- plate_data %>%
    dplyr::group_by(.data$Column) %>%
    dplyr::summarise(Mean = mean(.data$MeasurementValue, na.rm = TRUE),
                     SD   = sd(.data$MeasurementValue,   na.rm = TRUE),
                     .groups = "drop") %>%
    dplyr::mutate(Flag = ifelse(abs(.data$Mean - plate_mean) > 2 * plate_sd,
                                flag_char, ""))

  list(
    plate_mean   = plate_mean,
    plate_sd     = plate_sd,
    row_stats    = row_stats,
    col_stats    = col_stats,
    flagged_rows = sum(row_stats$Flag == flag_char),
    flagged_cols = sum(col_stats$Flag == flag_char)
  )
}

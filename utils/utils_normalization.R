# ==============================================================================
# Normalization Strategies
# Purpose: Apply RBA-specific data transformations.
#
# For RBA: Returns raw measurement values (CPM/RFU) directly.
# For ELISA: %B/B0 is handled by calculate_elisa_bb0() in report_functions.R.
#
# Functions:
#   get_normalization_strategy() - Factory: returns RBA strategy
#   normalize_data()             - Wrapper: RBA pass-through
#
# NOTE: Uses dplyr::filter() explicitly to avoid MASS::filter masking.
# ==============================================================================

#' Normalization Strategy Factory
#'
#' @param assay_type Character: "rba". For ELISA use calculate_elisa_bb0().
#' @param detection_method Character: "radioligand" or "fluorescent"
#' @return List with: name, calculate, response_unit, y_axis_label
get_normalization_strategy <- function(assay_type = "rba",
                                      detection_method = "radioligand") {
  if (assay_type == "rba") {
    return(list(
      name = "Direct measurement (RBA)",
      calculate = function(data) {
        data %>%
          mutate(
            NormalizedValue = MeasurementValue,
            ResponseUnit = case_when(
              detection_method == "radioligand" ~ "CPM",
              detection_method == "fluorescent" ~ "RFU",
              TRUE ~ "Signal"
            )
          )
      },
      response_unit = if (detection_method == "radioligand") "CPM" else "RFU",
      y_axis_label = if (detection_method == "radioligand") {
        "Counts per minute (CPM)"
      } else {
        "Relative Fluorescence Units (RFU)"
      }
    ))
  }
  stop("normalize_data() only handles assay_type='rba'. ",
       "Use calculate_elisa_bb0() for ELISA %B/B0 normalization.")
}

#' Apply normalization to long-format RBA data
#'
#' @param data_long Data frame with columns: Well, SampleType, MeasurementValue
#' @param assay_type "rba"
#' @param detection_method "radioligand" or "fluorescent"
#' @return Data frame with added NormalizedValue and ResponseUnit columns
normalize_data <- function(data_long, assay_type = "rba",
                          detection_method = "radioligand") {
  strategy   <- get_normalization_strategy(assay_type, detection_method)
  normalized <- strategy$calculate(data_long)
  attr(normalized, "normalization_strategy") <- strategy$name
  attr(normalized, "response_unit")          <- strategy$response_unit
  normalized
}

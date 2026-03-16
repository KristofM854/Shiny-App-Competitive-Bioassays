# ==============================================================================
# Core Report Functions
# Purpose: Main data processing and analysis functions for RBA/ELISA reports
# ==============================================================================

# Source constants
if (!exists("MOLECULAR_WEIGHTS")) {
  source("report_constants.R")
}

#' Validate input data structure and content
#' @param data_long Long format data from CSV
#' @return TRUE if valid, stops with error if invalid
validate_input_data <- function(data_long) {

  # Check required columns
  missing_cols <- setdiff(VALIDATION_RULES$required_columns, names(data_long))
  if (length(missing_cols) > 0) {
    stop("Missing required columns: ", paste(missing_cols, collapse = ", "))
  }

  # Check for reasonable data ranges
  if (any(data_long$DilutionFactor <= 0, na.rm = TRUE)) {
    stop("Invalid dilution factors found (must be > 0)")
  }

  # Check for valid sample types
  invalid_types <- setdiff(data_long$SampleType, VALIDATION_RULES$sample_types)
  if (length(invalid_types) > 0) {
    warning("Unknown sample types found: ", paste(invalid_types, collapse = ", "))
  }

  # Check for standards
  standards <- data_long[data_long$SampleType == "Standard" & !is.na(data_long$StandardConc), ]
  if (nrow(standards) < 4) {
    stop("Insufficient standards for curve fitting (need at least 4)")
  }

  return(TRUE)
}

#' Load and validate assay configuration
#' @param output_dir Path to output directory
#' @return List with assay configuration
load_assay_config <- function(output_dir) {
  config_file <- file.path(output_dir, OUTPUT_FILES$assay_config)

  if (file.exists(config_file)) {
    config <- jsonlite::fromJSON(config_file)
  } else {
    # Fallback to RBA defaults
    warning("No assay config found, defaulting to RBA")
    config <- list(
      assay_type = "rba",
      detection_method = "radioligand",
      toxin_standard_label = "Saxitoxin",
      molecular_weight_g_mol = 299.29
    )
  }

  return(config)
}

#' Get appropriate axis labels based on assay configuration
#' @param assay_config Assay configuration list
#' @return List with x_label, y_label, and units
get_axis_labels <- function(assay_config) {

  if (assay_config$assay_type == "elisa") {

    if (assay_config$detection_method == "absorbance") {
      y_label <- "Response (%B/B0)"
      y_unit <- "%B/B0"
    } else {
      y_label <- "Absorbance"
      y_unit <- "Abs"
    }

    x_label <- paste0("Standard concentration (", assay_config$units %||% "pg/mL", ")")

  } else {
    # RBA
    if (assay_config$detection_method == "radioligand") {
      y_label <- "Counts per minute (CPM)"
      y_unit <- "CPM"
    } else {
      y_label <- "Relative fluorescence units (RFU)"
      y_unit <- "RFU"
    }

    x_label <- "Standard concentration (mol/L)"
  }

  return(list(
    x_label = x_label,
    y_label = y_label,
    y_unit = y_unit
  ))
}

#' Calculate coefficient of variation
#' @param x Numeric vector
#' @return CV as percentage
coefficient_of_variation <- function(x) {
  m <- mean(x, na.rm = TRUE)
  if (is.na(m) || m == 0) return(NA_real_)
  sd(x, na.rm = TRUE) / m * 100
}

#' Identify standards with high variability
#' @param data_long Data frame with standards
#' @param cv_limit CV percentage threshold (default from constants)
#' @return Data frame with high variability standards
identify_high_variability_standards <- function(data_long, cv_limit = QC_THRESHOLDS$cv_limit) {

  response_var <- if ("NormalizedValue" %in% names(data_long)) "NormalizedValue" else "MeasurementValue"

  high_var <- data_long %>%
    dplyr::filter(SampleType == "Standard", !is.na(StandardConc)) %>%
    group_by(StandardConc) %>%
    summarise(
      cv = coefficient_of_variation(.data[[response_var]]),
      n_replicates = n(),
      .groups = "drop"
    ) %>%
    dplyr::filter(cv >= cv_limit | n_replicates < 2)

  return(high_var)
}

#' Load or create sample processing configuration
#' @param output_dir Directory containing analysis files
#' @return List with extraction and tissue processing parameters
load_sample_processing_config <- function(output_dir) {

  config_file <- file.path(output_dir, "sample_processing_config.json")

  # Default configuration
  default_config <- list(
    extraction_volume_ul = 500,          # Default extraction volume in microliters
    tissue_mass_mg = NULL,               # Tissue mass in milligrams (NULL = not tissue-based)
    sample_type = "extracted",           # "extracted", "direct", "tissue"
    extraction_method = "standard",      # For documentation
    notes = "Default extraction parameters"
  )

  if (file.exists(config_file)) {
    # Load existing configuration
    tryCatch({
      loaded_config <- jsonlite::fromJSON(config_file, simplifyVector = FALSE)
      # Merge with defaults to handle missing fields
      config <- modifyList(default_config, loaded_config)
      return(config)
    }, error = function(e) {
      warning("Could not load sample processing config: ", e$message, ". Using defaults.")
      return(default_config)
    })
  } else {
    # Create default configuration file
    jsonlite::write_json(default_config, config_file, pretty = TRUE, auto_unbox = TRUE)
    message("Created default sample processing config at: ", config_file)
    return(default_config)
  }
}

#' Calculate proper B/B0 values for ELISA following Cayman protocol
#' @param data_long Data frame with ELISA data including control wells
#' @param use_percent Whether to return %B/B0 (TRUE) or B/B0 (FALSE)
#' @return Data frame with calculated B/B0 values
calculate_elisa_bb0 <- function(data_long, use_percent = TRUE) {

  # Step 1: Identify control wells and their values
  blank_wells <- data_long %>%
    dplyr::filter(SampleType == "Blank") %>%
    pull(MeasurementValue)

  nsb_wells <- data_long %>%
    dplyr::filter(SampleType == "NSB") %>%
    pull(MeasurementValue)

  b0_wells <- data_long %>%
    dplyr::filter(SampleType == "B0") %>%
    pull(MeasurementValue)

  if (length(blank_wells) == 0 || length(nsb_wells) == 0 || length(b0_wells) == 0) {
    stop("Missing control wells. Need Blank, NSB, and B0 wells for ELISA analysis.")
  }

  # Step 2: Calculate averages (Cayman protocol page 29)
  blank_avg <- mean(blank_wells, na.rm = TRUE)
  nsb_avg <- mean(nsb_wells, na.rm = TRUE) - blank_avg  # Blank-corrected NSB
  b0_avg <- mean(b0_wells, na.rm = TRUE) - blank_avg   # Blank-corrected B0

  # Step 3: Calculate corrected B0 (maximum binding)
  corrected_b0 <- b0_avg - nsb_avg

  if (corrected_b0 <= 0) {
    stop("Invalid control well hierarchy. B0 must be > NSB after blank correction.")
  }

  # Step 4: Calculate B/B0 for all wells
  data_with_bb0 <- data_long %>%
    mutate(
      blank_corrected = MeasurementValue - blank_avg,
      nsb_corrected = blank_corrected - nsb_avg,
      b_b0_ratio = nsb_corrected / corrected_b0,
      calculated_bb0 = if (use_percent) b_b0_ratio * 100 else b_b0_ratio
    ) %>%
    dplyr::select(-blank_corrected, -nsb_corrected, -b_b0_ratio)

  # Add control well summary for QC
  attr(data_with_bb0, "control_summary") <- list(
    blank_avg = blank_avg,
    nsb_avg = nsb_avg + blank_avg,  # Report original NSB average
    b0_avg = b0_avg + blank_avg,    # Report original B0 average
    corrected_b0 = corrected_b0,
    hierarchy_valid = (b0_avg + blank_avg) > (nsb_avg + blank_avg)
  )

  return(data_with_bb0)
}

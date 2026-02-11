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

#' Get molecular weight for current assay
#' @param assay_config Assay configuration list
#' @return Molecular weight in g/mol
get_molecular_weight <- function(assay_config) {
  
  if (assay_config$assay_type == "elisa") {
    analyte <- assay_config$analyte %||% "cortisol"
    mw <- MOLECULAR_WEIGHTS[[analyte]]
    if (is.null(mw)) {
      warning("Unknown ELISA analyte: ", analyte, ", using cortisol MW")
      return(MOLECULAR_WEIGHTS$cortisol)
    }
    return(mw)
    
  } else {
    # RBA
    toxin <- tolower(assay_config$toxin_standard_label %||% "saxitoxin")
    mw <- MOLECULAR_WEIGHTS[[toxin]]
    if (is.null(mw)) {
      # Use from config if available
      return(assay_config$molecular_weight_g_mol %||% MOLECULAR_WEIGHTS$saxitoxin)
    }
    return(mw)
  }
}

#' Get appropriate response variable based on assay type
#' @param data_long Data frame
#' @param assay_config Assay configuration
#' @return String name of response variable column
get_response_variable <- function(data_long, assay_config) {
  
  if (assay_config$assay_type == "elisa" && "NormalizedValue" %in% names(data_long)) {
    return("NormalizedValue")
  } else {
    return("MeasurementValue")
  }
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
    filter(SampleType == "Standard", !is.na(StandardConc)) %>%
    group_by(StandardConc) %>%
    summarise(
      cv = coefficient_of_variation(.data[[response_var]]),
      n_replicates = n(),
      .groups = "drop"
    ) %>%
    filter(cv >= cv_limit | n_replicates < 2)
  
  return(high_var)
}

#' Prepare standards data for model fitting
#' @param data_long Full dataset 
#' @param exclude_high_var Whether to exclude high variability standards
#' @return Filtered dataset ready for DRC modeling
prepare_standards_for_modeling <- function(data_long, exclude_high_var = TRUE) {
  
  response_var <- get_response_variable(data_long, list())  # Will use default logic
  
  # Add concentration column for standards
  data_prepared <- data_long %>%
    mutate(concentration = ifelse(SampleType == "Standard", StandardConc, NA_real_))
  
  if (exclude_high_var) {
    high_var_standards <- identify_high_variability_standards(data_long)
    
    if (nrow(high_var_standards) > 0) {
      data_prepared <- data_prepared %>%
        mutate(
          high_variability = ifelse(
            concentration %in% high_var_standards$StandardConc,
            "High Variability",
            "Normal Variability"
          )
        ) %>%
        filter(SampleType == "Standard", high_variability == "Normal Variability")
    } else {
      data_prepared <- data_prepared %>%
        mutate(high_variability = "Normal Variability") %>%
        filter(SampleType == "Standard")
    }
  } else {
    data_prepared <- data_prepared %>%
      mutate(high_variability = "Normal Variability") %>%
      filter(SampleType == "Standard")  
  }
  
  return(data_prepared)
}

#' Predict sample concentrations using fitted model
#' @param model Fitted DRC model
#' @param samples Data frame with unknown samples
#' @param assay_config Assay configuration
#' @return Data frame with predictions and dilution corrections
predict_sample_concentrations <- function(model, samples, assay_config) {
  
  response_var <- get_response_variable(samples, assay_config)
  
  if (assay_config$assay_type == "elisa") {
    # ELISA-specific prediction using proper B/B0 calculation
    
    # Calculate proper B/B0 values if not already done
    if (!"calculated_bb0" %in% names(samples)) {
      # Need to calculate B/B0 using control wells from the full dataset
      # This requires access to the full dataset with control wells
      stop("ELISA analysis requires proper B/B0 calculation. ",
           "Use calculate_elisa_bb0() on the full dataset first.")
    }
    
    response_values <- samples$calculated_bb0
    
    # Filter out invalid response values
    valid_idx <- is.finite(response_values) & !is.na(response_values)
    
    if (sum(valid_idx) == 0) {
      # No valid samples to predict
      results <- samples %>%
        mutate(
          estimated_concentration = NA_real_,
          lower_CI = NA_real_,
          upper_CI = NA_real_,
          estimated_concentration_mass = NA_real_,
          mass_unit = "ng/mL",
          estimated_concentration_formatted = "NA",
          estimated_concentration_mass_formatted = "NA", 
          lower_CI_formatted = "NA",
          upper_CI_formatted = "NA"
        )
      return(results)
    }
    
    # Use inverse 4PL prediction for ELISA with proper B/B0 values
    tryCatch({
      # Get valid response values for prediction
      valid_responses <- response_values[valid_idx]
      
      # Apply inverse prediction using proper B/B0 values
      predicted_conc <- inv_ll4_elisa(valid_responses, model, flag_out_of_range = TRUE)
      
      # Check for out-of-range warnings
      out_of_range <- attr(predicted_conc, "out_of_range")
      if (!is.null(out_of_range) && any(out_of_range, na.rm = TRUE)) {
        range_info <- attr(predicted_conc, "recommended_range")
        warning("Some samples outside recommended range (", range_info, " B/B0). ",
                "Consider dilution adjustment for optimal quantification.")
      }
      
      # Create full prediction vector (with NAs for invalid responses)
      full_predictions <- rep(NA_real_, length(response_values))
      full_predictions[valid_idx] <- predicted_conc
      
      # Create results data frame with replicate-based tissue mass handling
      results <- samples %>%
        mutate(
          estimated_concentration = full_predictions / as.numeric(DilutionFactor)
        )
      
      # Handle tissue mass - check if TissueWeight_mg column exists (from enhanced plate)
      if ("TissueWeight_mg" %in% names(samples)) {
        # Use sample-specific tissue weights from the enhanced dilution/tissue plate
        tissue_samples <- results %>%
          filter(!is.na(TissueWeight_mg) & TissueWeight_mg > 0)
        
        if (nrow(tissue_samples) > 0) {
          # Get extraction volume from config (default 500 µL)
          processing_config <- tryCatch({
            load_sample_processing_config(output_dir)
          }, error = function(e) {
            list(extraction_volume_ul = 500)
          })
          
          # Calculate tissue-based concentrations for samples with tissue weights
          tissue_results <- tissue_samples %>%
            mutate(
              # Total amount in extraction (pg)
              total_amount_pg = estimated_concentration * (processing_config$extraction_volume_ul / 1000),
              # Tissue concentration (ng/g)
              concentration_ng_per_g = (total_amount_pg / 1000) / (TissueWeight_mg / 1000)
            )
          
          # Merge back tissue results
          results <- results %>%
            left_join(
              tissue_results %>% dplyr::select(SampleID, concentration_ng_per_g),
              by = "SampleID"
            )
        }
      }
      
      # Return only essential columns
      results <- results %>%
        dplyr::select(
          Well, SampleID, SampleType, Replicate, DilutionFactor,
          estimated_concentration,
          if ("TissueWeight_mg" %in% names(samples)) "TissueWeight_mg" else NULL,
          if ("concentration_ng_per_g" %in% names(results)) "concentration_ng_per_g" else NULL
        )
        
    }, error = function(e) {
      warning("ELISA inverse prediction failed: ", e$message)
      # Return samples with NA predictions
      results <- samples %>%
        mutate(
          estimated_concentration = NA_real_,
          lower_CI = NA_real_,
          upper_CI = NA_real_,
          estimated_concentration_mass = NA_real_,
          mass_unit = "ng/mL"
        )
      return(results)
    })
    
  } else {
    # RBA prediction using drc::ED function
    predicted_conc <- as.data.frame(
      drc::ED(
        model,
        samples[[response_var]], 
        interval = "delta",
        type = "absolute", 
        display = FALSE,
        bound = FALSE
      )
    ) %>%
      tibble::rownames_to_column("ResponseValue") %>%
      mutate(ResponseValue = as.numeric(stringr::str_sub(ResponseValue, start = 5)))
    
    # Join predictions with sample data
    results <- samples %>%
      dplyr::select(Well, SampleID, SampleType, Replicate, DilutionFactor, all_of(response_var)) %>%
      rename(Response = all_of(response_var)) %>%
      left_join(predicted_conc, by = c("Response" = "ResponseValue")) %>%
      mutate(
        # Convert to numeric  
        Estimate = as.numeric(Estimate),
        Lower = as.numeric(Lower),
        Upper = as.numeric(Upper),
        DilutionFactor = as.numeric(DilutionFactor),
        
        # Apply dilution correction and rename to match ELISA structure
        estimated_concentration = Estimate / DilutionFactor
      ) %>%
      dplyr::select(Well, SampleID, SampleType, Replicate, DilutionFactor, estimated_concentration)
  }
  
  return(results)
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

#' Convert ELISA concentrations to tissue-based units
#' @param concentration_pg_ml Concentration in pg/mL
#' @param extraction_volume_ul Extraction volume in microliters  
#' @param tissue_mass_mg Tissue mass in milligrams (optional)
#' @return List with various concentration units
convert_elisa_concentrations <- function(concentration_pg_ml, extraction_volume_ul = 500, tissue_mass_mg = NULL) {
  
  # Total amount in extraction
  total_pg <- concentration_pg_ml * (extraction_volume_ul / 1000)  # Convert µL to mL
  
  results <- list(
    concentration_pg_ml = concentration_pg_ml,
    concentration_ng_ml = concentration_pg_ml / 1000,
    total_amount_pg = total_pg,
    total_amount_ng = total_pg / 1000
  )
  
  # If tissue mass provided, calculate tissue-based concentrations
  if (!is.null(tissue_mass_mg) && !is.na(tissue_mass_mg) && tissue_mass_mg > 0) {
    tissue_mass_g <- tissue_mass_mg / 1000
    results$concentration_pg_per_g <- total_pg / tissue_mass_g
    results$concentration_ng_per_g <- (total_pg / 1000) / tissue_mass_g
    results$tissue_mass_mg <- tissue_mass_mg
    results$tissue_mass_g <- tissue_mass_g
  }
  
  return(results)
}

#' Calculate proper B/B0 values for ELISA following Cayman protocol
#' @param data_long Data frame with ELISA data including control wells
#' @param use_percent Whether to return %B/B0 (TRUE) or B/B0 (FALSE)
#' @return Data frame with calculated B/B0 values
calculate_elisa_bb0 <- function(data_long, use_percent = TRUE) {
  
  # Step 1: Identify control wells and their values
  blank_wells <- data_long %>% 
    filter(SampleType == "Blank") %>%
    pull(MeasurementValue)
  
  nsb_wells <- data_long %>%
    filter(SampleType == "NSB") %>%
    pull(MeasurementValue)
  
  b0_wells <- data_long %>%
    filter(SampleType == "B0" | SampleType == "MaximumBinding") %>%
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

#' ELISA inverse LL.4 prediction function with proper B/B0 input
#' Convert B/B0 values to concentrations using 4PL model coefficients
#' @param bb0_values Vector of B/B0 values (same scale as model fitting)
#' @param model Fitted drc model object
#' @param flag_out_of_range Whether to flag values outside 20-80% range
#' @return Vector of predicted concentrations with quality flags
inv_ll4_elisa <- function(bb0_values, model, flag_out_of_range = TRUE) {
  
  # Extract model coefficients (DRC LL.4 parameter order: b, c, d, e)
  co <- coef(model)
  b <- unname(co[1])  # Hill slope (steepness)
  c <- unname(co[2])  # Bottom (lower asymptote)  
  d <- unname(co[3])  # Top (upper asymptote)
  e <- unname(co[4])  # IC50 (inflection point)
  
  # For competitive ELISA: high %B/B0 = low concentration
  # Determine if we're working with % or ratio based on typical values
  is_percent <- any(bb0_values > 2, na.rm = TRUE)  # If any value > 2, likely %B/B0
  
  # Flag out-of-range values (Cayman recommendation: 20-80% B/B0)
  if (flag_out_of_range) {
    range_min <- if (is_percent) 20 else 0.20
    range_max <- if (is_percent) 80 else 0.80
    
    out_of_range <- bb0_values < range_min | bb0_values > range_max
    if (any(out_of_range, na.rm = TRUE)) {
      n_out_of_range <- sum(out_of_range, na.rm = TRUE)
      warning("Found ", n_out_of_range, " samples outside recommended range (", 
              range_min, "-", range_max, if (is_percent) "%" else "", " B/B0). ",
              "Consider dilution adjustment.")
    }
  }
  
  # Clamp values to prevent mathematical errors (but warn about out-of-range)
  y_min <- min(c, d) + 1e-6
  y_max <- max(c, d) - 1e-6
  y_clamped <- pmin(pmax(bb0_values, y_min), y_max)
  
  # Apply inverse 4PL equation: x = e * ((d-c)/(y-c) - 1)^(1/b)
  ratio <- (d - c) / (y_clamped - c) - 1
  
  # Ensure positive ratios for log transformation
  ratio <- pmax(ratio, 1e-10)
  
  # Calculate concentration using inverse formula
  x <- exp(log(e) + (1/b) * log(ratio))
  
  # Return concentrations, preserving NA values from original input
  result <- ifelse(is.finite(bb0_values), x, NA_real_)
  
  # Add quality flags as attributes
  if (flag_out_of_range) {
    attr(result, "out_of_range") <- out_of_range
    attr(result, "recommended_range") <- paste0(range_min, "-", range_max, if (is_percent) "%" else "")
  }
  
  return(result)
}

#' Determine if concentration falls within reliable range
#' @param concentration Numeric concentration value
#' @param ec20 EC20 value from model
#' @param ec80 EC80 value from model  
#' @return String indicating dilution status
determine_dilution_status <- function(concentration, ec20, ec80) {
  
  if (is.na(concentration) || !is.finite(concentration)) {
    return("Missing")
  }
  
  if (concentration <= ec20) {
    return("Out of Range")  # too diluted
  } else if (concentration >= ec80) {
    return("Out of Range")  # too concentrated  
  } else {
    return("Within Range")  # correct dilution
  }
}

#' Calculate replicate statistics for samples
#' @param sample_results Results from predict_sample_concentrations
#' @return Data frame with replicate statistics
calculate_replicate_stats <- function(sample_results) {
  
  stats <- sample_results %>%
    group_by(SampleType, Replicate) %>%
    summarise(
      sampleID = paste(unique(SampleID), collapse = ","),
      mean_conc = mean(estimated_concentration, na.rm = TRUE),
      se_conc = sd(estimated_concentration, na.rm = TRUE) / sqrt(n()),
      n_replicates = n(),
      .groups = "drop"
    ) %>%
    mutate(
      mean_conc_formatted = format(mean_conc, scientific = TRUE, digits = 2),
      se_conc_formatted = format(se_conc, scientific = TRUE, digits = 2)
    )
  
  return(stats)
}

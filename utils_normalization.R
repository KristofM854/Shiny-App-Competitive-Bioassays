# ==============================================================================
# Normalization Strategies
# Purpose: Apply assay-specific data transformations
# ==============================================================================

#' Normalization Strategy Factory
#' 
#' Returns a list of functions for normalizing plate reader data
#' based on assay type
#' 
#' @param assay_type Character: "rba" or "elisa"
#' @param detection_method Character: "radioligand" or "fluorescent" (RBA only)
#' @return List with: name, calculate, requires_controls, response_unit
get_normalization_strategy <- function(assay_type = "rba", 
                                      detection_method = "radioligand") {
  
  if (assay_type == "rba") {
    
    return(list(
      name = "Direct measurement (RBA)",
      
      calculate = function(data, controls = NULL) {
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
      
      requires_controls = FALSE,
      response_unit = if (detection_method == "radioligand") "CPM" else "RFU",
      y_axis_label = if (detection_method == "radioligand") {
        "Counts per minute (CPM)"
      } else {
        "Relative Fluorescence Units (RFU)"
      }
    ))
    
  } else if (assay_type == "elisa") {
    
    return(list(
      name = "Percent B/B0 normalization (ELISA)",
      
      calculate = function(data, controls) {
        
        # Validate required controls exist
        required_controls <- c("Blank", "NSB", "B0")
        missing <- setdiff(required_controls, names(controls))
        
        if (length(missing) > 0) {
          stop(sprintf(
            "Missing required ELISA controls: %s\n",
            paste(missing, collapse = ", ")
          ))
        }
        
        # Calculate means
        blank_mean <- mean(controls$Blank, na.rm = TRUE)
        nsb_mean   <- mean(controls$NSB, na.rm = TRUE)
        b0_mean    <- mean(controls$B0, na.rm = TRUE)
        
        # Validate hierarchy: B0 > NSB > Blank
        if (!(b0_mean > nsb_mean && nsb_mean > blank_mean)) {
          warning(sprintf(
            "Control hierarchy violated!\n  Expected: B0 > NSB > Blank\n  Observed: B0=%.3f, NSB=%.3f, Blank=%.3f",
            b0_mean, nsb_mean, blank_mean
          ))
        }
        
        # Apply %B/B0 formula
        data %>%
          mutate(
            NormalizedValue = 100 * (MeasurementValue - nsb_mean) / (b0_mean - nsb_mean),
            ResponseUnit = "%B/B0",
            # Store control values for QC reporting
            ControlValues_B0 = b0_mean,
            ControlValues_NSB = nsb_mean,
            ControlValues_Blank = blank_mean
          )
      },
      
      requires_controls = TRUE,
      response_unit = "%B/B0",
      y_axis_label = "Bound / Maximum Bound (%B/B0)"
    ))
    
  } else {
    stop("Unknown assay_type: ", assay_type)
  }
}

#' Extract control wells from plate data
#' 
#' @param data_long Long-format data frame with SampleType column
#' @return Named list of control values (Blank, NSB, B0, TotalActivity)
extract_controls <- function(data_long) {
  
  controls <- list(
    Blank = data_long %>% 
      filter(SampleType == "Blank") %>% 
      pull(MeasurementValue),
    
    NSB = data_long %>% 
      filter(SampleType == "NSB") %>% 
      pull(MeasurementValue),
    
    B0 = data_long %>% 
      filter(SampleType == "B0") %>% 
      pull(MeasurementValue),
    
    TotalActivity = data_long %>% 
      filter(SampleType == "TotalActivity") %>% 
      pull(MeasurementValue)
  )
  
  # Remove empty controls
  controls <- controls[sapply(controls, length) > 0]
  
  return(controls)
}

#' Validate control wells
#' 
#' Checks for minimum replicates and reasonable values
#' 
#' @param controls List from extract_controls()
#' @param min_replicates Minimum wells per control (default 2)
#' @return List with status ("pass", "warn", "fail") and messages
validate_controls <- function(controls, min_replicates = 2) {
  
  issues <- list()
  status <- "pass"
  
  # Check replicate counts
  required <- c("Blank", "NSB", "B0")
  for (ctrl in required) {
    n <- length(controls[[ctrl]] %||% numeric(0))
    
    if (n == 0) {
      issues[[length(issues) + 1]] <- sprintf(
        "❌ %s: No wells found", ctrl
      )
      status <- "fail"
      
    } else if (n < min_replicates) {
      issues[[length(issues) + 1]] <- sprintf(
        "⚠️ %s: Only %d well(s) (recommended: ≥%d)", 
        ctrl, n, min_replicates
      )
      if (status != "fail") status <- "warn"
    }
  }
  
  # Check hierarchy if all controls present
  if (all(required %in% names(controls))) {
    b0_mean <- mean(controls$B0, na.rm = TRUE)
    nsb_mean <- mean(controls$NSB, na.rm = TRUE)
    blank_mean <- mean(controls$Blank, na.rm = TRUE)
    
    if (!(b0_mean > nsb_mean && nsb_mean > blank_mean)) {
      issues[[length(issues) + 1]] <- sprintf(
        "⚠️ Control hierarchy violated: B0 (%.3f) > NSB (%.3f) > Blank (%.3f)",
        b0_mean, nsb_mean, blank_mean
      )
      if (status != "fail") status <- "warn"
    }
  }
  
  # Check for outliers (CV > 30%)
  for (ctrl_name in names(controls)) {
    vals <- controls[[ctrl_name]]
    if (length(vals) >= 2) {
      cv <- (sd(vals) / mean(vals)) * 100
      if (cv > 30) {
        issues[[length(issues) + 1]] <- sprintf(
          "⚠️ %s: High variability (CV = %.1f%%)",
          ctrl_name, cv
        )
        if (status != "fail") status <- "warn"
      }
    }
  }
  
  return(list(
    status = status,
    messages = if (length(issues) > 0) issues else list("✅ All control wells valid")
  ))
}

#' Apply normalization to long-format data
#' 
#' Wrapper function that selects strategy and applies normalization
#' 
#' @param data_long Data frame with columns: Well, SampleType, MeasurementValue
#' @param assay_type "rba" or "elisa"
#' @param detection_method "radioligand" or "fluorescent" (RBA only)
#' @return Data frame with added NormalizedValue and ResponseUnit columns
normalize_data <- function(data_long, assay_type = "rba", 
                          detection_method = "radioligand") {
  
  strategy <- get_normalization_strategy(assay_type, detection_method)
  
  if (strategy$requires_controls) {
    # Extract and validate controls
    controls <- extract_controls(data_long)
    validation <- validate_controls(controls)
    
    if (validation$status == "fail") {
      stop("Control validation failed:\n", 
           paste(validation$messages, collapse = "\n"))
    }
    
    if (validation$status == "warn") {
      warning("Control validation warnings:\n",
              paste(validation$messages, collapse = "\n"))
    }
    
    # Apply normalization with controls
    normalized <- strategy$calculate(data_long, controls)
    
  } else {
    # Direct normalization (RBA)
    normalized <- strategy$calculate(data_long)
  }
  
  # Add metadata
  attr(normalized, "normalization_strategy") <- strategy$name
  attr(normalized, "response_unit") <- strategy$response_unit
  
  return(normalized)
}

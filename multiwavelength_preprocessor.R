# ==============================================================================
# Multi-Wavelength Data Preprocessor
# Purpose: Convert multi-wavelength Excel file to format expected by Shiny app
# ==============================================================================

#' Process multi-wavelength file and create wavelength-specific CSV files
#' 
#' @param file_path Path to Excel file with multiple wavelengths
#' @param output_dir Directory to save processed files
#' @param plate_layout Data frame with plate layout (from Shiny app)
#' @return List with wavelengths detected and file paths created
#' @export
process_multiwavelength_file <- function(file_path, output_dir, plate_layout) {
  
  # Source the multi-wavelength import function
  source("utils_import_multiwavelength.R")
  
  # Import all wavelength plates
  plates <- import_multiwavelength_plates(file_path)
  
  wavelengths <- names(plates)
  message(sprintf("Processing %d wavelengths: %s", 
                 length(wavelengths), 
                 paste(wavelengths, collapse = ", ")))
  
  # For each wavelength, create a long-format CSV file
  output_files <- list()
  
  for (wl in wavelengths) {
    
    plate_data <- plates[[wl]]
    
    # Convert plate to long format using the plate_layout
    long_data <- plate_to_long_format(plate_data, plate_layout, wavelength = wl)
    
    # Save to CSV with wavelength suffix
    output_file <- file.path(output_dir, paste0("long_data_output_", wl, ".csv"))
    write.csv(long_data, output_file, row.names = FALSE)
    
    message(sprintf("Created: %s (%d rows)", basename(output_file), nrow(long_data)))
    
    output_files[[wl]] <- output_file
  }
  
  # Create a manifest file listing all wavelengths
  manifest <- data.frame(
    wavelength = wavelengths,
    file_path = unlist(output_files),
    n_wells = sapply(plates, function(p) sum(!is.na(p))),
    stringsAsFactors = FALSE
  )
  
  manifest_file <- file.path(output_dir, "wavelength_manifest.json")
  jsonlite::write_json(manifest, manifest_file, pretty = TRUE)
  
  return(list(
    wavelengths = wavelengths,
    output_files = output_files,
    manifest_file = manifest_file
  ))
}

#' Convert plate matrix to long format
#' 
#' @param plate_data 8x12 matrix of plate readings
#' @param plate_layout Data frame with plate layout
#' @param wavelength Wavelength label for this data
#' @return Long format data frame
plate_to_long_format <- function(plate_data, plate_layout, wavelength = NULL) {
  
  # Convert plate matrix to long format
  long_data <- data.frame(
    Row = rep(rownames(plate_data), each = ncol(plate_data)),
    Column = rep(colnames(plate_data), times = nrow(plate_data)),
    MeasurementValue = as.vector(t(plate_data)),
    stringsAsFactors = FALSE
  )
  
  # Create Well identifier (e.g., "A1", "A2", ...)
  long_data$Well <- paste0(long_data$Row, long_data$Column)
  
  # Merge with plate layout
  if (!is.null(plate_layout) && nrow(plate_layout) > 0) {
    long_data <- long_data %>%
      dplyr::left_join(plate_layout, by = "Well")
  }
  
  # Add wavelength column if provided
  if (!is.null(wavelength)) {
    long_data$Wavelength <- wavelength
  }
  
  # Remove rows with NA measurements (empty wells)
  long_data <- long_data %>%
    dplyr::filter(!is.na(MeasurementValue))
  
  return(long_data)
}

#' Check if file contains multiple wavelengths
#' 
#' @param file_path Path to Excel file
#' @return TRUE if multiple wavelengths detected, FALSE otherwise
#' @export
is_multiwavelength_file <- function(file_path) {
  
  source("utils_import_multiwavelength.R")
  
  wavelength_locations <- detect_multiwavelength_plates(file_path)
  
  return(!is.null(wavelength_locations) && length(wavelength_locations) > 1)
}

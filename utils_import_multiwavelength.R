# ==============================================================================
# Multi-Wavelength File Import Utilities
# Purpose: Parse plate reader files with multiple wavelength measurements
# Version: 4.0 - Handles multiple wavelengths in single file
# ==============================================================================

#' Detect all wavelength plates in a file
#' 
#' Scans for multiple plate data blocks labeled with wavelengths
#' Looks for pattern: "Raw Data (XXX)" where XXX is wavelength in nm
#' 
#' @param file_path Path to Excel or CSV file
#' @param sheet Sheet name or index (for Excel files)
#' @return List of wavelength detections, each with location info
#' @export
detect_multiwavelength_plates <- function(file_path, sheet = 1) {
  
  ext <- tools::file_ext(file_path)
  
  # Read file without headers
  raw <- if (ext %in% c("xlsx", "xls")) {
    suppressMessages(
      readxl::read_excel(file_path, sheet = sheet, col_names = FALSE, 
                        .name_repair = "minimal")
    )
  } else if (ext == "csv") {
    read.csv(file_path, header = FALSE, stringsAsFactors = FALSE)
  } else if (ext == "txt") {
    read.table(file_path, header = FALSE, sep = "\t", stringsAsFactors = FALSE)
  } else {
    stop("Unsupported file format: ", ext)
  }
  
  # Convert to matrix for easier scanning
  mat <- as.matrix(raw)
  
  # Search for wavelength markers
  wavelength_locations <- list()
  
  for (i in 1:nrow(mat)) {
    # Look for "Raw Data (XXX)" pattern
    cell_text <- as.character(mat[i, 2])  # Often in second column
    
    if (!is.na(cell_text) && grepl("Raw Data \\(\\d+\\)", cell_text, ignore.case = TRUE)) {
      
      # Extract wavelength
      wavelength <- as.numeric(gsub(".*\\((\\d+)\\).*", "\\1", cell_text))
      
      # Plate data should be 2 rows below (based on your file structure)
      plate_start_row <- i + 2
      
      if (plate_start_row + 7 <= nrow(mat)) {
        
        # Check if we have row labels A-H
        potential_rows <- mat[plate_start_row:(plate_start_row+7), 1]
        potential_rows <- trimws(as.character(potential_rows))
        
        if (identical(potential_rows, LETTERS[1:8])) {
          
          # Found valid plate! Check for column headers
          header_row <- plate_start_row - 1
          potential_cols <- mat[header_row, 2:ncol(mat)]
          potential_cols <- suppressWarnings(as.numeric(potential_cols))
          
          # Count sequential column numbers
          ncols_found <- 0
          for (j in seq_along(potential_cols)) {
            if (!is.na(potential_cols[j]) && potential_cols[j] == j) {
              ncols_found <- ncols_found + 1
            } else {
              break
            }
          }
          
          if (ncols_found >= 4) {
            wavelength_locations[[length(wavelength_locations) + 1]] <- list(
              wavelength = wavelength,
              wavelength_label = paste0(wavelength, "nm"),
              marker_row = i,
              start_row = plate_start_row,
              header_row = header_row,
              start_col = 2,
              nrows = 8,
              ncols = ncols_found,
              partial_plate = ncols_found < 12
            )
          }
        }
      }
    }
  }
  
  if (length(wavelength_locations) == 0) {
    return(NULL)
  }
  
  # Sort by wavelength
  wavelengths <- sapply(wavelength_locations, function(x) x$wavelength)
  wavelength_locations <- wavelength_locations[order(wavelengths)]
  
  return(wavelength_locations)
}

#' Import all wavelength plates from a file
#' 
#' @param file_path Path to file
#' @param sheet Sheet name/index (Excel only)
#' @param expected_rows Expected number of rows (default 8)
#' @param expected_cols Expected number of columns (default 12)
#' @return Named list of data frames, one per wavelength
#' @export
import_multiwavelength_plates <- function(file_path, sheet = 1, 
                                          expected_rows = 8, expected_cols = 12) {
  
  # Detect all wavelengths
  wavelength_locations <- detect_multiwavelength_plates(file_path, sheet)
  
  if (is.null(wavelength_locations)) {
    # Fallback to single plate detection (backward compatibility)
    message("No multi-wavelength markers found. Attempting single plate import...")
    
    source("/mnt/user-data/uploads/utils_import_v3.R")
    single_plate <- import_plate_data(file_path, sheet, expected_rows, expected_cols)
    
    return(list(
      "single" = single_plate
    ))
  }
  
  message(sprintf("Detected %d wavelength plates: %s", 
                 length(wavelength_locations),
                 paste(sapply(wavelength_locations, function(x) x$wavelength_label), 
                       collapse = ", ")))
  
  # Read raw data
  ext <- tools::file_ext(file_path)
  raw <- if (ext %in% c("xlsx", "xls")) {
    suppressMessages(
      readxl::read_excel(file_path, sheet = sheet, col_names = FALSE,
                        .name_repair = "minimal")
    )
  } else if (ext == "csv") {
    read.csv(file_path, header = FALSE, stringsAsFactors = FALSE)
  } else {
    read.table(file_path, header = FALSE, sep = "\t", stringsAsFactors = FALSE)
  }
  
  # Extract each wavelength plate
  plates <- list()
  
  for (loc in wavelength_locations) {
    
    # Extract plate region
    row_indices <- loc$start_row:(loc$start_row + loc$nrows - 1)
    col_indices <- loc$start_col:(loc$start_col + loc$ncols - 1)
    
    plate_data <- raw[row_indices, col_indices, drop = FALSE]
    
    # Convert to numeric matrix
    plate_numeric <- suppressWarnings(
      as.data.frame(
        apply(plate_data, 2, as.numeric),
        stringsAsFactors = FALSE
      )
    )
    
    # Pad with NA if partial plate
    if (ncol(plate_numeric) < expected_cols) {
      n_missing <- expected_cols - ncol(plate_numeric)
      for (i in 1:n_missing) {
        plate_numeric[[ncol(plate_numeric) + 1]] <- NA_real_
      }
    }
    
    if (nrow(plate_numeric) < expected_rows) {
      n_missing <- expected_rows - nrow(plate_numeric)
      empty_rows <- as.data.frame(
        matrix(NA_real_, nrow = n_missing, ncol = ncol(plate_numeric))
      )
      plate_numeric <- rbind(plate_numeric, empty_rows)
    }
    
    # Trim if too large
    plate_numeric <- plate_numeric[1:expected_rows, 1:expected_cols, drop = FALSE]
    
    # Set standard row/column names
    rownames(plate_numeric) <- LETTERS[1:expected_rows]
    colnames(plate_numeric) <- as.character(1:expected_cols)
    
    # Add metadata
    attr(plate_numeric, "import_info") <- list(
      file = basename(file_path),
      wavelength = loc$wavelength,
      wavelength_label = loc$wavelength_label,
      detected_location = loc,
      import_time = Sys.time(),
      detected_wells = sum(!is.na(plate_numeric)),
      partial_plate = loc$partial_plate
    )
    
    # Add to list using wavelength label as name
    plates[[loc$wavelength_label]] <- plate_numeric
  }
  
  return(plates)
}

#' Preview multi-wavelength import
#' 
#' @param file_path Path to file
#' @param sheet Sheet name/index
#' @return List with summary info for each wavelength
#' @export
preview_multiwavelength_import <- function(file_path, sheet = 1) {
  
  wavelength_locations <- detect_multiwavelength_plates(file_path, sheet)
  
  if (is.null(wavelength_locations)) {
    return(list(
      status = "single_plate",
      message = "No multi-wavelength markers detected. File appears to contain single plate.",
      wavelengths = NULL
    ))
  }
  
  wavelengths_detected <- sapply(wavelength_locations, function(x) x$wavelength_label)
  
  # Try to import all
  plates <- tryCatch(
    import_multiwavelength_plates(file_path, sheet),
    error = function(e) NULL
  )
  
  if (is.null(plates)) {
    return(list(
      status = "error",
      message = "Multi-wavelength import failed during extraction.",
      wavelengths = wavelengths_detected
    ))
  }
  
  # Summarize each wavelength
  summaries <- lapply(names(plates), function(wl) {
    plate <- plates[[wl]]
    info <- attr(plate, "import_info")
    
    list(
      wavelength = wl,
      detected_wells = info$detected_wells,
      partial_plate = info$partial_plate,
      preview_head = head(plate, 2),
      preview_tail = tail(plate, 2)
    )
  })
  
  names(summaries) <- names(plates)
  
  return(list(
    status = "success",
    n_wavelengths = length(plates),
    wavelengths = wavelengths_detected,
    summaries = summaries
  ))
}

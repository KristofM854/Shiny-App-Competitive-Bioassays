# ==============================================================================
# File Import Utilities
# Purpose: Intelligently parse plate reader output files (.xlsx, .csv, .txt).
#
# Uses two detection strategies to locate 8x12 plate data:
#   Strategy 1: Look for row labels A-H in first column
#   Strategy 2: Find largest contiguous 8xN numeric block (>=70% valid cells)
#
# Functions:
#   read_file_raw()           - Read a plate reader file into a raw data.frame
#   detect_plate_location()   - Find plate data region in file (or raw data)
#   import_plate_data()       - Import and validate plate as 8x12 matrix
#   parse_plate_file()        - Unified single-pass entry point
#   preview_import()          - Quick validation preview
# ==============================================================================

#' Read a plate reader file into a raw data.frame (no headers)
#'
#' Shared low-level reader used by both detection and import functions
#' so the file is read exactly once per import attempt.
#'
#' @param file_path Path to Excel, CSV, or TXT file
#' @param sheet Sheet name or index (Excel only, default 1)
#' @return A data.frame of raw file contents (character columns)
#' @export
read_file_raw <- function(file_path, sheet = 1) {
  ext <- tolower(tools::file_ext(file_path))
  if (ext %in% c("xlsx", "xls")) {
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
}

#' Detect plate data location in Excel file
#' 
#' Scans an Excel file to find the 96-well plate data block using multiple strategies:
#' 1. Look for row labels (A-H) in first column (with or without column headers)
#' 2. Look for purely numeric 8×N array (no labels, accepts partial plates)
#' 
#' @param file_path Path to Excel or CSV file
#' @param sheet Sheet name or index (for Excel files)
#' @return List with: start_row, start_col, nrows, ncols, format, or NULL if not found
#' @export
detect_plate_location <- function(file_path, sheet = 1, raw_data = NULL) {

  # Use pre-read data if supplied; otherwise read from disk
  raw <- if (!is.null(raw_data)) raw_data else read_file_raw(file_path, sheet)

  # Convert to matrix for easier scanning
  mat <- as.matrix(raw)
  
  # ============================================================================
  # STRATEGY 1: Search for row labels A-H in first column
  # ============================================================================
  for (i in 1:(nrow(mat) - 7)) {
    
    potential_rows <- mat[i:(i+7), 1]
    potential_rows <- trimws(as.character(potential_rows))
    
    if (identical(potential_rows, LETTERS[1:8])) {
      
      # Found row labels! Check for column headers above
      if (i > 1) {
        potential_cols <- mat[i-1, 2:ncol(mat)]
        potential_cols <- suppressWarnings(as.numeric(potential_cols))
        
        # Count sequential column numbers (1, 2, 3, ...)
        ncols_found <- 0
        for (j in seq_along(potential_cols)) {
          if (!is.na(potential_cols[j]) && potential_cols[j] == j) {
            ncols_found <- ncols_found + 1
          } else {
            break
          }
        }
        
        if (ncols_found >= 4) {
          return(list(
            start_row = i,
            header_row = i - 1,
            start_col = 2,
            nrows = 8,
            ncols = ncols_found,
            format = "labeled_with_headers",
            partial_plate = ncols_found < 12
          ))
        }
      }
      
      # No headers, but A-H found - check for numeric data
      test_data <- suppressWarnings(as.numeric(mat[i, 2:min(13, ncol(mat))]))
      num_valid <- sum(!is.na(test_data))
      
      if (num_valid >= 4) {
        return(list(
          start_row = i,
          header_row = NULL,
          start_col = 2,
          nrows = 8,
          ncols = min(12, num_valid),
          format = "labeled_no_headers",
          partial_plate = num_valid < 12
        ))
      }
    }
  }
  
  # ============================================================================
  # STRATEGY 2: Search for pure numeric 8×N array (no labels)
  # ============================================================================
  
  for (i in 1:(nrow(mat) - 7)) {
    
    # Try different starting columns (skip empty leading columns)
    for (col_offset in 0:min(3, ncol(mat) - 4)) {
      
      max_cols <- min(12, ncol(mat) - col_offset)
      if (max_cols < 4) next
      
      block <- mat[i:(i+7), (col_offset+1):(col_offset+max_cols), drop = FALSE]
      
      # Convert to numeric
      numeric_block <- suppressWarnings(
        matrix(as.numeric(block), nrow = 8, ncol = ncol(block))
      )
      
      # CRITICAL: Check each row individually
      # Require at least 4 valid numbers per row
      valid_per_row <- rowSums(!is.na(numeric_block))
      
      # Only accept if ALL 8 rows have ≥4 valid numbers
      if (all(valid_per_row >= 4)) {
        
        # Additionally, find which columns are consistently good
        valid_per_col <- colSums(!is.na(numeric_block))
        good_cols <- which(valid_per_col >= 6)  # At least 6/8 rows valid
        
        if (length(good_cols) >= 4) {
          
          # Extract contiguous block of good columns
          first_good <- min(good_cols)
          last_good <- max(good_cols)
          
          # Allow max 1 gap in column sequence
          if ((last_good - first_good + 1) <= (length(good_cols) + 1)) {
            
            final_block <- numeric_block[, first_good:last_good, drop = FALSE]
            valid_count <- sum(!is.na(final_block))
            total_cells <- 8 * ncol(final_block)
            valid_pct <- valid_count / total_cells
            
            # Final validation: >70% valid, ≥4 cols, ≥32 valid cells
            if (valid_pct > 0.70 && ncol(final_block) >= 4 && valid_count >= 32) {
              return(list(
                start_row = i,
                header_row = NULL,
                start_col = col_offset + first_good,
                nrows = 8,
                ncols = ncol(final_block),
                format = "unlabeled_array",
                valid_cells = valid_count,
                total_cells = total_cells,
                partial_plate = ncol(final_block) < 12
              ))
            }
          }
        }
      }
    }
  }
  
  # Not found
  return(NULL)
}

#' Import plate data from file
#' 
#' @param file_path Path to file
#' @param sheet Sheet name/index (Excel only)
#' @param expected_rows Expected number of rows (default 8)
#' @param expected_cols Expected number of columns (default 12)
#' @return Data frame with row names A-H and column names 1-12
#' @export
import_plate_data <- function(file_path, sheet = 1,
                             expected_rows = 8, expected_cols = 12,
                             raw_data = NULL, location = NULL) {

  # Use pre-read data if supplied; otherwise read from disk
  raw <- if (!is.null(raw_data)) raw_data else read_file_raw(file_path, sheet)
  used_sheet <- sheet

  # Use pre-detected location if supplied; otherwise detect
  if (is.null(location)) {
    location <- detect_plate_location(file_path, sheet, raw_data = raw)

    if (is.null(location)) {
      ext <- tools::file_ext(file_path)
      if (ext %in% c("xlsx", "xls")) {
        # Try all other sheets
        all_sheets <- readxl::excel_sheets(file_path)
        for (s in seq_along(all_sheets)) {
          if (s == sheet) next
          other_raw <- read_file_raw(file_path, sheet = s)
          location <- tryCatch(
            detect_plate_location(file_path, sheet = s, raw_data = other_raw),
            error = function(e) NULL
          )
          if (!is.null(location)) {
            raw <- other_raw
            used_sheet <- s
            message("Plate data found on sheet '", all_sheets[s], "' (sheet ", s, ").")
            break
          }
        }
      }
    }
  }

  if (is.null(location)) {
    stop(
      "Could not detect plate data in file.\n",
      "Expected: 8 rows \u00D7 4+ columns of numeric data.\n",
      "Check that your file contains a data table with measurements.\n",
      "Supported formats:\n",
      "  - With row labels (A-H) in first column\n",
      "  - Without labels (pure numeric array)\n",
      "  - With or without column headers (1-12)"
    )
  }
  
  # Extract plate region
  row_indices <- location$start_row:(location$start_row + location$nrows - 1)
  col_indices <- location$start_col:(location$start_col + location$ncols - 1)

  plate_data <- raw[row_indices, col_indices, drop = FALSE]

  # Handle overflow/saturation markers before numeric conversion
  overflow_markers <- c("#SAT", "OVER", "ERR", "****", "Overfl", "Sat")
  overflow_count <- 0
  for (r in 1:nrow(plate_data)) {
    for (cc in 1:ncol(plate_data)) {
      cell_val <- trimws(as.character(plate_data[r, cc]))
      if (toupper(cell_val) %in% toupper(overflow_markers)) {
        plate_data[r, cc] <- NA
        overflow_count <- overflow_count + 1
      }
    }
  }
  if (overflow_count > 0) {
    warning(overflow_count, " well(s) contained overflow/saturation markers and were set to NA.")
  }

  # Locale-aware decimal parsing: detect comma decimals (European format)
  all_cells <- as.character(unlist(plate_data))
  has_comma_decimal <- any(grepl("[0-9],[0-9]", all_cells), na.rm = TRUE)
  has_dot_decimal <- any(grepl("[0-9]\\.[0-9]", all_cells), na.rm = TRUE)
  if (has_comma_decimal && !has_dot_decimal) {
    # European locale: swap comma for period
    plate_data <- as.data.frame(
      apply(plate_data, 2, function(col) gsub(",", ".", as.character(col))),
      stringsAsFactors = FALSE
    )
    warning("Detected European decimal format (comma separator). Values converted automatically.")
  }

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
    detected_location = location,
    import_time = Sys.time(),
    detected_wells = sum(!is.na(plate_numeric)),
    partial_plate = location$partial_plate,
    format = location$format
  )
  
  return(plate_numeric)
}

#' Unified single-pass plate file parser
#'
#' Detects file format, checks for multi-wavelength content (Excel), and parses
#' all plate data in a single pass.  Returns a normalized result object suitable
#' for direct use in the upload server logic.
#'
#' @param file_path Path to the plate reader file (CSV, TXT, XLS, XLSX)
#' @param sheet     Sheet name or index (Excel only, default 1)
#' @return A list with elements:
#'   \describe{
#'     \item{is_multiwavelength}{Logical}
#'     \item{format}{"Excel", "CSV", or "TXT"}
#'     \item{detected_wells}{Integer – wells with data in the primary plate}
#'     \item{partial_plate}{Logical – TRUE when fewer than 96 wells detected}
#'     \item{wavelengths}{Character vector of wavelength labels, or NULL}
#'     \item{plates}{Named list of data-frame plates (single-element for
#'       single-wavelength files)}
#'     \item{import_info}{List of metadata for notifications}
#'   }
#' @export
parse_plate_file <- function(file_path, sheet = 1) {

  ext <- tolower(tools::file_ext(file_path))

  fmt <- if (ext %in% c("xlsx", "xls")) "Excel" else if (ext == "csv") "CSV" else "TXT"

  # ==================================================================
  # SINGLE FILE READ — all subsequent logic uses this in-memory copy
  # ==================================================================
  raw <- read_file_raw(file_path, sheet)

  # ------------------------------------------------------------------
  # Excel files: try multi-wavelength detection first
  # ------------------------------------------------------------------
  if (ext %in% c("xlsx", "xls")) {
    raw_mat <- as.matrix(raw)
    mw <- tryCatch(
      detect_and_import_multiwavelength(file_path, sheet, raw_matrix = raw_mat),
      error = function(e) list(wavelengths = NULL, plates = NULL)
    )

    if (!is.null(mw$wavelengths) && length(mw$wavelengths) > 1) {
      primary   <- mw$plates[[1]]
      info      <- base::attr(primary, "import_info")
      wells     <- if (!is.null(info)) info$detected_wells else sum(!is.na(primary))
      partial   <- if (!is.null(info)) info$partial_plate  else (wells < 96)

      return(list(
        is_multiwavelength = TRUE,
        format             = fmt,
        detected_wells     = wells,
        partial_plate      = partial,
        wavelengths        = mw$wavelengths,
        plates             = mw$plates,
        import_info        = list(
          file        = basename(file_path),
          format      = fmt,
          wells       = wells,
          partial     = partial,
          wavelengths = mw$wavelengths
        )
      ))
    }
  }

  # ------------------------------------------------------------------
  # Single-plate path (CSV / TXT / Excel without multi-wave markers)
  # Reuses the same in-memory raw data — no second file read
  # ------------------------------------------------------------------
  plate <- import_plate_data(file_path, sheet, raw_data = raw)
  info  <- base::attr(plate, "import_info")

  wells   <- if (!is.null(info)) info$detected_wells else sum(!is.na(plate))
  partial <- if (!is.null(info)) info$partial_plate  else (wells < 96)
  det_fmt <- if (!is.null(info)) info$format         else "unknown"

  list(
    is_multiwavelength = FALSE,
    format             = fmt,
    detected_wells     = wells,
    partial_plate      = partial,
    wavelengths        = NULL,
    plates             = list(primary = plate),
    import_info        = list(
      file        = basename(file_path),
      format      = det_fmt,
      wells       = wells,
      partial     = partial,
      wavelengths = NULL
    )
  )
}

#' Preview file import
#'
#' Quick check to see what would be imported
#' 
#' @param file_path Path to file
#' @param sheet Sheet name/index
#' @return List with summary info
#' @export
preview_import <- function(file_path, sheet = 1) {

  raw <- read_file_raw(file_path, sheet)
  location <- detect_plate_location(file_path, sheet, raw_data = raw)

  if (is.null(location)) {
    return(list(
      status = "error",
      message = "Plate data not detected in file. Expected 8 rows × 4+ columns of numeric data."
    ))
  }

  # Try to import (reuse raw + location)
  plate <- tryCatch(
    import_plate_data(file_path, sheet, raw_data = raw, location = location),
    error = function(e) NULL
  )
  
  if (is.null(plate)) {
    return(list(
      status = "error",
      message = "Import failed during extraction. Check file format."
    ))
  }
  
  info <- attr(plate, "import_info")
  
  return(list(
    status = "success",
    format = info$format,
    detected_wells = info$detected_wells,
    partial_plate = info$partial_plate,
    location = location,
    preview_head = head(plate, 3),
    preview_tail = tail(plate, 3)
  ))
}

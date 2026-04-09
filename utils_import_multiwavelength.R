# ==============================================================================
# Multi-Wavelength File Import Utilities
# Purpose: Parse plate reader Excel files containing multiple wavelength plates.
#          Detects "Raw Data (XXXnm)" markers and extracts each wavelength's
#          8x12 plate data independently.
#
# Functions:
#   detect_and_import_multiwavelength() - Single-pass: read file once, return
#                                         wavelength locations AND parsed plates
#   detect_multiwavelength_plates()     - Thin wrapper (backward-compatible)
#   import_multiwavelength_plates()     - Thin wrapper (backward-compatible)
#   preview_multiwavelength_import()    - Quick preview of detected wavelengths
#
# Falls back to single-plate import (utils_import_v3.R) if no wavelength
# markers are found.
# ==============================================================================

#' Read a plate file into a raw matrix (internal helper)
#'
#' @param file_path Path to Excel or CSV file
#' @param sheet Sheet name or index (for Excel files)
#' @return Character matrix of the raw file contents
#' @keywords internal
.read_raw_matrix <- function(file_path, sheet = 1) {
  ext <- tools::file_ext(file_path)

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

  as.matrix(raw)
}

#' Scan a raw matrix for wavelength marker locations (internal helper)
#'
#' @param mat Character matrix produced by `.read_raw_matrix()`
#' @return List of wavelength location records (unsorted), or empty list
#' @keywords internal
.scan_wavelength_locations <- function(mat) {
  wavelength_locations <- list()

  for (i in 1:nrow(mat)) {
    cell_text <- as.character(mat[i, 2])  # marker is typically in column 2

    if (!is.na(cell_text) && grepl("Raw Data \\(\\d+\\)", cell_text, ignore.case = TRUE)) {

      wavelength <- as.numeric(gsub(".*\\((\\d+)\\).*", "\\1", cell_text))
      plate_start_row <- i + 2

      if (plate_start_row + 7 <= nrow(mat)) {

        potential_rows <- trimws(as.character(mat[plate_start_row:(plate_start_row + 7), 1]))

        if (identical(potential_rows, LETTERS[1:8])) {

          header_row <- plate_start_row - 1
          potential_cols <- suppressWarnings(as.numeric(mat[header_row, 2:ncol(mat)]))

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
              wavelength       = wavelength,
              wavelength_label = paste0(wavelength, "nm"),
              marker_row       = i,
              start_row        = plate_start_row,
              header_row       = header_row,
              start_col        = 2,
              nrows            = 8,
              ncols            = ncols_found,
              partial_plate    = ncols_found < 12
            )
          }
        }
      }
    }
  }

  wavelength_locations
}

#' Extract plate data frames from a raw matrix (internal helper)
#'
#' @param mat     Character matrix produced by `.read_raw_matrix()`
#' @param locations Sorted list of wavelength location records
#' @param file_path Original file path (used for metadata only)
#' @param expected_rows Expected plate row count (default 8)
#' @param expected_cols Expected plate column count (default 12)
#' @return Named list of data frames, one per wavelength
#' @keywords internal
.extract_plates <- function(mat, locations, file_path,
                            expected_rows = 8, expected_cols = 12) {
  plates <- list()

  for (loc in locations) {
    row_indices <- loc$start_row:(loc$start_row + loc$nrows - 1)
    col_indices <- loc$start_col:(loc$start_col + loc$ncols - 1)

    plate_data <- mat[row_indices, col_indices, drop = FALSE]

    plate_numeric <- suppressWarnings(
      as.data.frame(
        apply(plate_data, 2, as.numeric),
        stringsAsFactors = FALSE
      )
    )

    # Pad columns if partial plate
    if (ncol(plate_numeric) < expected_cols) {
      n_missing <- expected_cols - ncol(plate_numeric)
      for (i in seq_len(n_missing)) {
        plate_numeric[[ncol(plate_numeric) + 1]] <- NA_real_
      }
    }

    # Pad rows if needed
    if (nrow(plate_numeric) < expected_rows) {
      n_missing <- expected_rows - nrow(plate_numeric)
      empty_rows <- as.data.frame(
        matrix(NA_real_, nrow = n_missing, ncol = ncol(plate_numeric))
      )
      plate_numeric <- rbind(plate_numeric, empty_rows)
    }

    # Trim if oversized
    plate_numeric <- plate_numeric[1:expected_rows, 1:expected_cols, drop = FALSE]

    rownames(plate_numeric) <- LETTERS[1:expected_rows]
    colnames(plate_numeric) <- as.character(1:expected_cols)

    attr(plate_numeric, "import_info") <- list(
      file              = basename(file_path),
      wavelength        = loc$wavelength,
      wavelength_label  = loc$wavelength_label,
      detected_location = loc,
      import_time       = Sys.time(),
      detected_wells    = sum(!is.na(plate_numeric)),
      partial_plate     = loc$partial_plate
    )

    plates[[loc$wavelength_label]] <- plate_numeric
  }

  plates
}

# ==============================================================================
# Public API
# ==============================================================================

#' Detect wavelengths and import plates in a single file read
#'
#' Reads the file exactly once, locates all wavelength markers, and extracts
#' every plate data block.  This is the canonical implementation; the older
#' \code{detect_multiwavelength_plates()} and
#' \code{import_multiwavelength_plates()} functions are thin wrappers around
#' this one.
#'
#' @param file_path     Path to Excel or CSV file
#' @param sheet         Sheet name or index (Excel only, default 1)
#' @param expected_rows Expected plate row count (default 8)
#' @param expected_cols Expected plate column count (default 12)
#' @return A named list with two elements:
#'   \describe{
#'     \item{wavelengths}{Character vector of detected wavelength labels, e.g.
#'       \code{c("450nm", "630nm")}, or \code{NULL} when none are found.}
#'     \item{plates}{Named list of data frames (one per wavelength), or
#'       \code{NULL} when no wavelength markers were found.}
#'   }
#' @export
detect_and_import_multiwavelength <- function(file_path, sheet = 1,
                                              expected_rows = 8,
                                              expected_cols = 12,
                                              raw_matrix = NULL) {
  # --- single file read (skipped if pre-read matrix supplied) ---
  mat <- if (!is.null(raw_matrix)) raw_matrix else .read_raw_matrix(file_path, sheet)

  # --- locate markers ---
  locations <- .scan_wavelength_locations(mat)

  if (length(locations) == 0) {
    return(list(wavelengths = NULL, plates = NULL))
  }

  # Sort by wavelength value
  order_idx <- order(sapply(locations, function(x) x$wavelength))
  locations <- locations[order_idx]

  # --- extract plates ---
  plates <- .extract_plates(mat, locations, file_path, expected_rows, expected_cols)

  wavelengths <- sapply(locations, function(x) x$wavelength_label)

  list(wavelengths = wavelengths, plates = plates)
}

# ==============================================================================
# Backward-compatible wrappers
# ==============================================================================

#' Detect all wavelength plates in a file
#'
#' Thin wrapper around \code{detect_and_import_multiwavelength()}.  Returns
#' only the wavelength location metadata list for backward compatibility.
#'
#' @param file_path Path to Excel or CSV file
#' @param sheet     Sheet name or index (for Excel files)
#' @return List of wavelength detections (each with location info), or
#'   \code{NULL} if none found.
#' @export
detect_multiwavelength_plates <- function(file_path, sheet = 1) {
  result <- detect_and_import_multiwavelength(file_path, sheet)

  if (is.null(result$wavelengths)) {
    return(NULL)
  }

  # Reconstruct the location-list format originally returned by this function.
  # The location metadata is stored in each plate's import_info attribute.
  lapply(result$plates, function(plate) {
    attr(plate, "import_info")$detected_location
  })
}

#' Import all wavelength plates from a file
#'
#' Thin wrapper around \code{detect_and_import_multiwavelength()}.  Returns
#' a named list of data frames (one per wavelength) for backward compatibility.
#'
#' @param file_path     Path to file
#' @param sheet         Sheet name/index (Excel only)
#' @param expected_rows Expected number of rows (default 8)
#' @param expected_cols Expected number of columns (default 12)
#' @return Named list of data frames, one per wavelength
#' @export
import_multiwavelength_plates <- function(file_path, sheet = 1,
                                          expected_rows = 8,
                                          expected_cols = 12) {
  result <- detect_and_import_multiwavelength(file_path, sheet,
                                              expected_rows, expected_cols)

  if (is.null(result$plates)) {
    # Fallback to single plate detection (backward compatibility)
    message("No multi-wavelength markers found. Attempting single plate import...")

    v3_candidates <- c(
      file.path(getwd(), "utils_import_v3.R"),
      file.path(dirname(sys.frame(1)$ofile %||% ""), "utils_import_v3.R"),
      "utils_import_v3.R"
    )
    v3_path <- Filter(file.exists, v3_candidates)[1]
    if (is.na(v3_path)) stop("Cannot find utils_import_v3.R")
    source(v3_path)
    single_plate <- import_plate_data(file_path, sheet, expected_rows, expected_cols)

    return(list("single" = single_plate))
  }

  message(sprintf("Detected %d wavelength plates: %s",
                  length(result$plates),
                  paste(result$wavelengths, collapse = ", ")))

  result$plates
}

#' Preview multi-wavelength import
#'
#' @param file_path Path to file
#' @param sheet     Sheet name/index
#' @return List with summary info for each wavelength
#' @export
preview_multiwavelength_import <- function(file_path, sheet = 1) {

  result <- detect_and_import_multiwavelength(file_path, sheet)

  if (is.null(result$wavelengths)) {
    return(list(
      status  = "single_plate",
      message = "No multi-wavelength markers detected. File appears to contain single plate.",
      wavelengths = NULL
    ))
  }

  plates <- tryCatch(result$plates, error = function(e) NULL)

  if (is.null(plates)) {
    return(list(
      status      = "error",
      message     = "Multi-wavelength import failed during extraction.",
      wavelengths = result$wavelengths
    ))
  }

  summaries <- lapply(names(plates), function(wl) {
    plate <- plates[[wl]]
    info  <- attr(plate, "import_info")

    list(
      wavelength     = wl,
      detected_wells = info$detected_wells,
      partial_plate  = info$partial_plate,
      preview_head   = head(plate, 2),
      preview_tail   = tail(plate, 2)
    )
  })
  names(summaries) <- names(plates)

  list(
    status        = "success",
    n_wavelengths = length(plates),
    wavelengths   = result$wavelengths,
    summaries     = summaries
  )
}

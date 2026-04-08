# ==============================================================================
# Plate Matrix Utilities
# Purpose: Create and manipulate 96-well plate layout matrices.
#
# Functions:
#   create_plate_matrix()     - Empty 8x12 matrix
#   create_type_matrix()      - Pre-filled Sample Type layout (RBA/ELISA)
#   create_id_matrix()        - Pre-filled Sample ID layout
#   create_dilution_matrix()  - Dilution factor matrix (default 1)
#   create_replicate_matrix() - Replicate group labels (paired wells)
#   enforce_plate_shape()     - Pad/trim data frame to 8x12
#   parse_dilution_cell()     - Parse "1:2" or "0.5" dilution formats
#   parse_dilution_matrix()   - Vectorized version of parse_dilution_cell()
#   matrix_to_long()          - Convert 4 plate matrices + measurements to
#                               long format for downstream analysis
#   matrix_to_long_with_cached_layout() - Optimized version that caches
#                               layout columns across multiple wavelengths
#
# NOTE: matrix_to_long() uses dplyr::select() explicitly to avoid
#       MASS::select masking from the drc package.
# ==============================================================================

#' Create empty plate matrix
#' 
#' @param fill Default value for cells (default NA_real_)
#' @return Data frame with 8 rows (A-H) and 12 columns (1-12)
create_plate_matrix <- function(fill = NA_real_) {
  mat <- matrix(as.numeric(fill), nrow = PLATE_NROW, ncol = PLATE_NCOL)
  dimnames(mat) <- list(ROW_NAMES, COL_NAMES)
  as.data.frame(mat, stringsAsFactors = FALSE)
}

#' Create type matrix based on assay configuration
#' 
#' @param assay_type "rba" or "elisa"
#' @param num_standards Number of standards (0-12)
#' @return Data frame with sample types
create_type_matrix <- function(assay_type = "rba", num_standards = 8) {
  
  mat <- matrix("Sample", nrow = PLATE_NROW, ncol = PLATE_NCOL,
                dimnames = list(ROW_NAMES, COL_NAMES))
  
  if (assay_type == "rba") {
    # RBA: Standards in columns 1-3, rows A-H (first 8 standards)
    # Additional standards 9-12 in columns 4-6, rows A-D
    
    if (num_standards >= 1) {
      max_first_block <- min(num_standards, 8)
      for (s in 1:max_first_block) {
        mat[s, 1:3] <- "Standard"
      }
    }
    
    if (num_standards > 8) {
      for (s in 9:min(num_standards, 12)) {
        r <- s - 8  # 9→A, 10→B, 11→C, 12→D
        mat[r, 4:6] <- "Standard"
      }
    }
    
  } else if (assay_type == "elisa") {
    # ELISA: Column 1 pre-filled with control pattern, standards in columns 2-3
    
    # Column 1: Pre-fill with your standard control pattern
    control_pattern <- c("Blank", "Blank", "NSB", "NSB", "B0", "B0", "B0", "TotalActivity")
    for (r in 1:PLATE_NROW) {
      mat[r, 1] <- control_pattern[r]
    }
    
    # Standards in columns 2-3, rows A-H (or as many as specified)
    if (num_standards >= 1) {
      max_standards <- min(num_standards, 8)
      for (s in 1:max_standards) {
        mat[s, 2:3] <- "Standard"
      }
    }
  }
  
  as.data.frame(mat, stringsAsFactors = FALSE)
}

#' Create sample ID matrix
#' 
#' @param assay_type "rba" or "elisa"
#' @param num_standards Number of standards
#' @return Data frame with sample IDs
create_id_matrix <- function(assay_type = "rba", num_standards = 8) {
  
  # Default: well coordinates (A1, A2, ...)
  mat <- matrix(
    paste0(rep(ROW_NAMES, each = PLATE_NCOL), rep(COL_NAMES, times = PLATE_NROW)),
    nrow = PLATE_NROW,
    ncol = PLATE_NCOL,
    byrow = TRUE,
    dimnames = list(ROW_NAMES, COL_NAMES)
  )
  
  # Assign standard IDs based on type matrix
  type_mat <- create_type_matrix(assay_type, num_standards)
  
  if (assay_type == "rba") {
    # Standards 1-8 in columns 1-3
    if (num_standards >= 1) {
      max_first <- min(num_standards, 8)
      for (s in 1:max_first) {
        mat[s, 1:3] <- paste0("S", s)
      }
    }
    
    # Standards 9-12 in columns 4-6
    if (num_standards > 8) {
      for (s in 9:min(num_standards, 12)) {
        r <- s - 8
        mat[r, 4:6] <- paste0("S", s)
      }
    }
    
  } else if (assay_type == "elisa") {
    # Standards in columns 2-3
    if (num_standards >= 1) {
      max_standards <- min(num_standards, 8)
      for (s in 1:max_standards) {
        mat[s, 2:3] <- paste0("S", s)
      }
    }
  }
  
  as.data.frame(mat, stringsAsFactors = FALSE)
}

#' Create dilution factor matrix
#' 
#' @param fill Default dilution factor (default 1)
#' @return Data frame with dilution factors
create_dilution_matrix <- function(fill = 1) {
  mat <- matrix(as.numeric(fill), nrow = PLATE_NROW, ncol = PLATE_NCOL)
  dimnames(mat) <- list(ROW_NAMES, COL_NAMES)
  as.data.frame(mat, stringsAsFactors = FALSE)
}

#' Create default raw dilution matrix (strings, for UI)
#' 
#' @return Data frame with "1" strings (8×12)
default_raw_dilution <- function() {
  df <- as.data.frame(
    matrix("1", nrow = PLATE_NROW, ncol = PLATE_NCOL),
    stringsAsFactors = FALSE
  )
  colnames(df) <- COL_NAMES
  rownames(df) <- ROW_NAMES
  df
}

#' Create replicate group matrix
#' 
#' Generates replicate labels based on assay type:
#' - RBA: Triplicates (AA, AB, AC, AD per row - 3 wells each)
#' - ELISA: Duplicates for standards, singles for controls (ELISA layout)
#' 
#' @param assay_type "rba" or "elisa"
#' @return Data frame with replicate labels
create_replicate_matrix <- function(assay_type = "rba") {
  
  mat <- matrix("", nrow = PLATE_NROW, ncol = PLATE_NCOL,
                dimnames = list(ROW_NAMES, COL_NAMES))
  
  if (assay_type == "rba") {
    # RBA: Row-wise triplicates (AA, AB, AC, AD per row)
    first_letters <- LETTERS[1:PLATE_NROW]
    second_letters <- LETTERS[1:4]
    rep_ids <- as.vector(outer(first_letters, second_letters, paste0))
    
    id_index <- 1
    for (r in 1:PLATE_NROW) {
      for (tri in 1:4) {  # 4 triplicate groups per row
        label <- rep_ids[id_index]
        cols <- ((tri - 1) * 3 + 1):((tri - 1) * 3 + 3)
        mat[r, cols] <- label
        id_index <- id_index + 1
      }
    }
    
  } else if (assay_type == "elisa") {
    # ELISA layout per your specification:
    # Col 1: Generic control IDs (actual types set in type matrix: Blank/NSB/B0/TA)  
    # Cols 2-3: Standards S1-S8 duplicated
    # Cols 4+: Sample pattern following your specification
    
    # Column 1: Control replicate groups (prefixed to distinguish from sample groups)
    control_pattern <- c("Ctrl_Blank", "Ctrl_Blank", "Ctrl_NSB", "Ctrl_NSB",
                         "Ctrl_B0", "Ctrl_B0", "Ctrl_B0", "Ctrl_TA")
    for (r in 1:PLATE_NROW) {
      mat[r, 1] <- control_pattern[r]
    }
      
    # Columns 2-3: Standards S1-S8 duplicated
    for (r in 1:PLATE_NROW) {
      mat[r, 2:3] <- paste0("S", r)
    }
    
    # Columns 4+: Sample naming per your specification
    # Your desired pattern:
    # Col 4-5: AA,EA,AB,EB,AC,EC,AD,ED 
    # Col 6-7: BA,FA,BB,FB,BC,FC,BD,FD  
    # Col 8-9: CA,GA,CB,GB,CC,GC,CD,GD
    # Col 10-11: DA,HA,DB,HB,DC,HC,DD,HD
    # Col 12: EA,EA,EB,EB,EC,EC,ED,ED

    # Column pairs and their base letters
    column_pairs <- list(
      c(4,5),   # A series: AA,AE,AB,AF,AC,AG,AD,AH
      c(6,7),   # B series: BA,BE,BB,BF,BC,BG,BD,BH
      c(8,9),   # C series: CA,CE,CB,CF,CC,CG,CD,CH
      c(10,11), # D series: DA,DE,DB,DF,DC,DG,DD,DH
      c(12)     # E series: EA,EA,EB,EB,EC,EC,ED,ED (single column, paired)
    )

    pair_letters <- c("A", "B", "C", "D", "E")

    for (pair_idx in seq_along(column_pairs)) {
      cols <- column_pairs[[pair_idx]]
      base_letter <- pair_letters[pair_idx]

      if (length(cols) == 1) {
        # Single column: paired duplicates EA,EA,EB,EB,EC,EC,ED,ED
        for (r in 1:PLATE_NROW) {
          second_letter <- LETTERS[((r - 1) %/% 2) + 1]  # 1,2→A; 3,4→B; 5,6→C; 7,8→D
          mat[r, cols] <- paste0(base_letter, second_letter)
        }
      } else {
        # Regular column pairs: alternating A,E,B,E,C,E,D,E pattern
        for (r in 1:PLATE_NROW) {
          second_letter <- if (r %% 2 == 1) {
            # Odd rows (1,3,5,7): A,B,C,D 
            LETTERS[((r - 1) %/% 2) + 1]  # 1→A, 3→B, 5→C, 7→D
          } else {
            # Even rows (2,4,6,8): E,F,G,H
            LETTERS[4 + (r %/% 2)]        # 2→E, 4→F, 6→G, 8→H
          }
          
          sample_label <- paste0(base_letter, second_letter)
          mat[r, cols] <- sample_label
        }
      }
    }
  }
  
  as.data.frame(mat, stringsAsFactors = FALSE)
}

#' Enforce plate dimensions
#' 
#' Pads or trims a data frame to exactly 8×12
#' 
#' @param df Data frame to enforce
#' @return Data frame with guaranteed 8 rows × 12 columns
enforce_plate_shape <- function(df) {
  df <- as.data.frame(df, stringsAsFactors = FALSE)
  
  r <- nrow(df)
  c <- ncol(df)
  
  # Pad rows if needed
  if (r < PLATE_NROW) {
    df[(r+1):PLATE_NROW, ] <- NA
  }

  # Pad columns if needed
  if (c < PLATE_NCOL) {
    df[, (c+1):PLATE_NCOL] <- NA
  }
  
  # Trim to exact size
  df <- df[1:PLATE_NROW, 1:PLATE_NCOL, drop = FALSE]
  
  # Set standard names
  colnames(df) <- COL_NAMES
  rownames(df) <- ROW_NAMES
  
  df
}

#' Parse dilution cell value
#' 
#' Handles numeric (0.5), ratio (1:2), and scientific notation (3e-9)
#' 
#' @param cell Raw cell value (character or numeric)
#' @return List with: value (numeric), valid (logical)
parse_dilution_cell <- function(cell) {
  
  if (is.null(cell) || identical(cell, "")) {
    return(list(value = NA_real_, valid = FALSE))
  }
  
  cell_chr <- trimws(as.character(cell))
  
  # Handle ratio input: "1:2" → 0.5
  if (grepl(":", cell_chr)) {
    parts <- suppressWarnings(as.numeric(strsplit(cell_chr, ":")[[1]]))
    if (length(parts) == 2 && all(is.finite(parts)) && all(parts > 0)) {
      return(list(value = parts[1] / parts[2], valid = TRUE))
    } else {
      return(list(value = NA_real_, valid = FALSE))
    }
  }
  
  # Handle numeric (including scientific notation)
  num <- suppressWarnings(as.numeric(cell_chr))
  if (is.na(num) || num <= 0) {
    return(list(value = NA_real_, valid = FALSE))
  }
  
  return(list(value = num, valid = TRUE))
}

#' Parse an entire 8x12 dilution matrix at once (vectorized)
#'
#' Processes all cells of a raw dilution matrix using vectorized operations
#' instead of nested loops. Drop-in replacement for calling parse_dilution_cell()
#' in a loop.
#'
#' @param raw_matrix Data frame or matrix (8x12) of raw dilution strings
#' @return List with: $values (numeric matrix 8x12), $validity (logical matrix 8x12)
parse_dilution_matrix <- function(raw_matrix) {
  raw_matrix <- as.data.frame(raw_matrix, stringsAsFactors = FALSE)

  # Flatten to character vector for vectorized processing
  cells <- trimws(as.character(as.matrix(raw_matrix)))
  n <- length(cells)

  values <- rep(NA_real_, n)
  validity <- rep(FALSE, n)

  # Identify empty/null cells (leave as NA/FALSE)
  non_empty <- nchar(cells) > 0 & !is.na(cells)

  # Identify ratio cells (contain ":")
  is_ratio <- non_empty & grepl(":", cells)

  # Process ratio cells vectorized
  if (any(is_ratio)) {
    ratio_cells <- cells[is_ratio]
    # Split all ratios at once
    parts_list <- strsplit(ratio_cells, ":")
    # Extract numerator and denominator
    nums <- suppressWarnings(vapply(parts_list, function(p) {
      if (length(p) != 2) return(NA_real_)
      vals <- as.numeric(p)
      if (all(is.finite(vals)) && all(vals > 0)) vals[1] / vals[2] else NA_real_
    }, numeric(1)))
    valid_ratios <- is.finite(nums)
    values[is_ratio] <- nums
    validity[is_ratio] <- valid_ratios
  }

  # Process numeric cells (non-ratio, non-empty) vectorized
  is_numeric <- non_empty & !is_ratio
  if (any(is_numeric)) {
    nums <- suppressWarnings(as.numeric(cells[is_numeric]))
    valid_nums <- !is.na(nums) & nums > 0
    values[is_numeric] <- ifelse(valid_nums, nums, NA_real_)
    validity[is_numeric] <- valid_nums
  }

  # Reshape back to 8x12 matrices
  list(
    values = matrix(values, nrow = PLATE_NROW, ncol = PLATE_NCOL),
    validity = matrix(validity, nrow = PLATE_NROW, ncol = PLATE_NCOL)
  )
}

#' Convert plate matrix to long format
#' 
#' @param type_mat Type matrix (from create_type_matrix)
#' @param id_mat ID matrix (from create_id_matrix)
#' @param dilution_mat Dilution matrix
#' @param replicate_mat Replicate matrix
#' @param measurement_mat Measurement values
#' @param std_conc Vector of standard concentrations (aligned with S1, S2, ...)
#' @return Long-format data frame
matrix_to_long <- function(type_mat, id_mat, dilution_mat,
                          replicate_mat, measurement_mat, std_conc = NULL) {

  # Build Row and Column vectors once (deterministic order: row-major)
  rows <- rep(ROW_NAMES, times = PLATE_NCOL)
  cols <- rep(COL_NAMES, each = PLATE_NROW)
  wells <- paste0(rows, cols)

  # Flatten all matrices to vectors in the same order (column-major is default
  # for as.matrix, which matches rep(ROW_NAMES, times=NCOL) / rep(COL_NAMES, each=NROW))
  df_long <- data.frame(
    Well           = wells,
    Row            = rows,
    Column         = cols,
    SampleType     = as.character(as.matrix(type_mat)),
    SampleID       = as.character(as.matrix(id_mat)),
    DilutionFactor = as.numeric(as.matrix(dilution_mat)),
    Replicate      = as.character(as.matrix(replicate_mat)),
    MeasurementValue = as.numeric(as.matrix(measurement_mat)),
    stringsAsFactors = FALSE
  )

  # Add standard concentrations if provided
  if (!is.null(std_conc)) {
    is_std <- grepl("^S[0-9]+$", df_long$SampleID)
    std_idx <- rep(NA_integer_, nrow(df_long))
    std_idx[is_std] <- as.integer(sub("^S", "", df_long$SampleID[is_std]))
    df_long$StandardConc <- ifelse(
      is_std & !is.na(std_idx) & std_idx <= length(std_conc),
      std_conc[std_idx],
      NA_real_
    )
  }

  # Reorder columns
  df_long %>%
    dplyr::select(Well, Row, Column, SampleType, SampleID,
           any_of("StandardConc"), DilutionFactor, Replicate, MeasurementValue)
}

#' Convert plate matrices to long format with cached layout
#'
#' For multi-wavelength data where layout matrices (type, id, dilution,
#' replicate) are identical across wavelengths, this function computes the
#' layout long-format once and reuses it. Call it once to get back a closure
#' that efficiently converts each wavelength's measurement matrix.
#'
#' @param type_mat Type matrix
#' @param id_mat ID matrix
#' @param dilution_mat Dilution matrix
#' @param replicate_mat Replicate matrix
#' @param std_conc Vector of standard concentrations
#' @return A function(measurement_mat) that returns a long-format data frame
#'   with the cached layout columns plus MeasurementValue from the supplied matrix
matrix_to_long_with_cached_layout <- function(type_mat, id_mat, dilution_mat,
                                               replicate_mat, std_conc = NULL) {

  # Pre-compute layout columns once
  rows <- rep(ROW_NAMES, times = PLATE_NCOL)
  cols <- rep(COL_NAMES, each = PLATE_NROW)

  layout <- data.frame(
    Well           = paste0(rows, cols),
    Row            = rows,
    Column         = cols,
    SampleType     = as.character(as.matrix(type_mat)),
    SampleID       = as.character(as.matrix(id_mat)),
    DilutionFactor = as.numeric(as.matrix(dilution_mat)),
    Replicate      = as.character(as.matrix(replicate_mat)),
    stringsAsFactors = FALSE
  )

  if (!is.null(std_conc)) {
    is_std <- grepl("^S[0-9]+$", layout$SampleID)
    std_idx <- rep(NA_integer_, nrow(layout))
    std_idx[is_std] <- as.integer(sub("^S", "", layout$SampleID[is_std]))
    layout$StandardConc <- ifelse(
      is_std & !is.na(std_idx) & std_idx <= length(std_conc),
      std_conc[std_idx],
      NA_real_
    )
  }

  # Return a closure that merges measurement data with the cached layout
  function(measurement_mat) {
    df <- layout
    df$MeasurementValue <- as.numeric(as.matrix(measurement_mat))
    df %>%
      dplyr::select(Well, Row, Column, SampleType, SampleID,
             any_of("StandardConc"), DilutionFactor, Replicate, MeasurementValue)
  }
}

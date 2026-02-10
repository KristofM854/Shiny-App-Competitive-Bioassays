# ==============================================================================
# Plate Matrix Utilities
# Purpose: Create and manipulate plate layout matrices
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
    
    # Column 1: Generic control IDs (user assigns Blank/NSB/B0/TA in TYPE matrix)
    control_pattern <- c("Blank", "Blank", "NSB", "NSB", "B0", "B0", "B0", "TotalActivity")
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
    # Col 12: JA,JA,JB,JB,JC,JC,JD,JD
    
    # Column pairs and their base letters
    column_pairs <- list(
      c(4,5),   # A series: AA,EA,AB,EB,AC,EC,AD,ED
      c(6,7),   # B series: BA,FA,BB,FB,BC,FC,BD,FD  
      c(8,9),   # C series: CA,GA,CB,GB,CC,GC,CD,GD
      c(10,11), # D series: DA,HA,DB,HB,DC,HC,DD,HD
      c(12)     # J series: JA,JA,JB,JB,JC,JC,JD,JD (single column)
    )
    
    pair_letters <- c("A", "B", "C", "D", "J")
    
    for (pair_idx in seq_along(column_pairs)) {
      cols <- column_pairs[[pair_idx]]
      base_letter <- pair_letters[pair_idx]
      
      if (base_letter == "J") {
        # Special case for column 12: JA,JA,JB,JB,JC,JC,JD,JD
        for (r in 1:PLATE_NROW) {
          # Pairs: rows 1&2→JA, rows 3&4→JB, rows 5&6→JC, rows 7&8→JD
          second_letter <- LETTERS[((r - 1) %/% 2) + 1]  # 1,2→A; 3,4→B; 5,6→C; 7,8→D
          sample_label <- paste0("J", second_letter)
          mat[r, cols] <- sample_label
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
    df[(r+1):PLATE_NROW, ] <- NA_character_
  }
  
  # Pad columns if needed
  if (c < PLATE_NCOL) {
    df[, (c+1):PLATE_NCOL] <- NA_character_
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
  
  # Convert each matrix to long format
  df_type <- type_mat %>% 
    rownames_to_column("Row") %>% 
    pivot_longer(-Row, names_to = "Column", values_to = "SampleType")
  
  df_id <- id_mat %>% 
    rownames_to_column("Row") %>% 
    pivot_longer(-Row, names_to = "Column", values_to = "SampleID")
  
  df_dilution <- dilution_mat %>% 
    rownames_to_column("Row") %>% 
    pivot_longer(-Row, names_to = "Column", values_to = "DilutionFactor")
  
  df_replicate <- replicate_mat %>% 
    rownames_to_column("Row") %>% 
    pivot_longer(-Row, names_to = "Column", values_to = "Replicate")
  
  df_meas <- measurement_mat %>% 
    rownames_to_column("Row") %>% 
    pivot_longer(-Row, names_to = "Column", values_to = "MeasurementValue")
  
  # Combine
  df_long <- df_type %>%
    left_join(df_id, by = c("Row", "Column")) %>%
    left_join(df_dilution, by = c("Row", "Column")) %>%
    left_join(df_replicate, by = c("Row", "Column")) %>%
    left_join(df_meas, by = c("Row", "Column")) %>%
    mutate(
      Well = paste0(Row, Column),
      MeasurementValue = as.numeric(MeasurementValue),
      DilutionFactor = as.numeric(DilutionFactor)
    )
  
  # Add standard concentrations if provided
  if (!is.null(std_conc)) {
    df_long <- df_long %>%
      mutate(
        StandardConc = case_when(
          grepl("^S[0-9]+$", SampleID) ~ {
            idx <- as.integer(str_extract(SampleID, "[0-9]+"))
            ifelse(idx <= length(std_conc), std_conc[idx], NA_real_)
          },
          TRUE ~ NA_real_
        )
      )
  }
  
  # Reorder columns
  df_long %>%
    select(Well, Row, Column, SampleType, SampleID, 
           any_of("StandardConc"), DilutionFactor, Replicate, MeasurementValue)
}

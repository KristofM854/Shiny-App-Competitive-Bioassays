# ==============================================================================
# Multi-Wavelength Template Preprocessor
# Purpose: Add unique chunk label prefixes when rendering child templates
# ==============================================================================

#' Preprocess Rmd template to add unique chunk label prefix
#' 
#' @param template_path Path to the Rmd template file
#' @param label_prefix Prefix to add to all chunk labels (e.g., "wl405nm")
#' @param temp_dir Directory to save the preprocessed template
#' @return Path to the preprocessed template file
#' 
preprocess_template_chunks <- function(template_path, label_prefix, temp_dir = tempdir()) {
  
  # Read the template
  template_lines <- readLines(template_path, warn = FALSE)
  
  # Find all chunk headers (```{r chunk-name, ...})
  chunk_pattern <- "^```\\{r\\s+([^,}]+)"
  
  # Process each line
  processed_lines <- sapply(template_lines, function(line) {
    if (grepl(chunk_pattern, line)) {
      # Extract chunk name
      chunk_match <- regmatches(line, regexpr(chunk_pattern, line, perl = TRUE))
      
      if (length(chunk_match) > 0) {
        # Get the chunk name
        chunk_name <- sub("^```\\{r\\s+", "", chunk_match)
        
        # Add prefix to chunk name
        new_chunk_name <- paste0(label_prefix, "-", chunk_name)
        
        # Replace in the line
        line <- sub(
          paste0("(```\\{r\\s+)", chunk_name),
          paste0("\\1", new_chunk_name),
          line
        )
      }
    }
    return(line)
  }, USE.NAMES = FALSE)
  
  # Save to temporary file
  temp_file <- file.path(temp_dir, paste0(label_prefix, "_", basename(template_path)))
  writeLines(processed_lines, temp_file)
  
  return(temp_file)
}

# ==============================================================================
# Report Constants and Configuration
# Purpose: Central location for all hardcoded values and configuration
# ==============================================================================

# Define %||% operator (null coalescing)
`%||%` <- function(x, y) if (is.null(x)) y else x

# Molecular weights (g/mol)
MOLECULAR_WEIGHTS <- list(
  # RBA toxins
  saxitoxin = 299.29,
  brevetoxin = 895.08,
  ciguatoxin = 1111.68,
  
  # ELISA analytes  
  cortisol = 362.46,
  testosterone = 288.42,
  estradiol = 272.38
)

# QC thresholds and validation parameters
QC_THRESHOLDS <- list(
  cv_limit = 30,                    # CV% limit for standards
  hill_slope_tolerance = 0.2,       # ±20% tolerance for Hill slope
  qc_deviation_limit = 30,          # ±30% tolerance for QC samples
  ec50_se_limit = 0.3              # EC50 standard error limit
)

# Plot configuration
PLOT_CONFIG <- list(
  # Standard plot dimensions
  width = 10,
  height = 7,
  
  # Color schemes
  colors = list(
    rba = list(
      standard = "darkblue",
      high_variability = "darkred",
      within_range = "blue",
      out_of_range = "orange"
    ),
    elisa = list(
      standard = "darkgreen", 
      high_variability = "red",
      within_range = "blue",
      out_of_range = "orange"
    )
  ),
  
  # Axis limits and breaks
  x_limits = c(1e-12, 1e-5),
  x_breaks = 10^seq(-12, -5, by = 1)
)

# Units and conversions
UNIT_CONFIG <- list(
  rba = list(
    concentration_unit = "mol/L",
    mass_unit = "μg/L", 
    response_unit = "CPM",
    mass_conversion_factor = 1e6  # mol/L to μg/L
  ),
  
  elisa = list(
    concentration_unit = "pg/mL",
    mass_unit = "ng/mL",
    response_unit = "%B/B0",
    mass_conversion_factor = 1e3   # pg/mL to ng/mL
  )
)

# Response variable mapping
RESPONSE_VARIABLES <- list(
  rba = "MeasurementValue",     # Direct CPM/RFU values
  elisa = "NormalizedValue"     # %B/B0 normalized values
)

# Default standard concentrations
DEFAULT_STANDARDS <- list(
  rba_saxitoxin = c(1e-6, 3e-7, 1e-7, 3e-8, 1e-8, 3e-9, 1e-9, 3e-10),
  
  elisa_cortisol = c(4000, 1600, 640, 256, 102.4, 41.0, 16.4, 6.6), # pg/mL
  
  elisa_testosterone = c(10000, 4000, 1600, 640, 256, 102.4, 41.0, 16.4), # pg/mL
  
  elisa_estradiol = c(2000, 800, 320, 128, 51.2, 20.5, 8.2, 3.3)  # pg/mL
)

# Table styling configuration  
TABLE_CONFIG <- list(
  html_options = c("striped", "hover", "condensed", "responsive"),
  full_width = FALSE,
  position = "center",
  digits = 2
)

# Validation rules
VALIDATION_RULES <- list(
  required_columns = c("SampleID", "MeasurementValue", "DilutionFactor", 
                      "StandardConc", "SampleType", "Replicate"),
  
  optional_columns = c("NormalizedValue", "ResponseUnit"),
  
  numeric_columns = c("MeasurementValue", "DilutionFactor", "StandardConc"),
  
  positive_columns = c("DilutionFactor"),  # Must be > 0
  
  sample_types = c("Standard", "Sample", "QC", "Blank", "NSB", "B0", "TotalActivity", "Other")
)

# File paths (relative to output directory)
OUTPUT_FILES <- list(
  long_data = "long_data_output.csv",
  formats_json = "selected_formats.json", 
  notes_file = "notes.json",
  qc_params = "qc_params.json",
  assay_config = "assay_config.json",
  unknown_results = "unknown_results.csv"
)

# Report metadata
REPORT_INFO <- list(
  version = "2.0",
  authors = c("Kristof Moeller (IAEA)", "Arnold Molina Porras (UCR)"),
  contact = "kr.moeller@iaea.org",
  feedback_url = "https://forms.office.com/e/q8eqJfp4QM"
)

# ==============================================================================
# Report Constants and Configuration
# Purpose: Central location for report-specific values and configuration.
#          When running via the Shiny app, global.R is already loaded and
#          provides shared constants (MW_LOOKUP, QC_THRESHOLDS, etc.).
#          When rendering standalone, this file provides self-contained defaults.
# ==============================================================================

# %||% operator is defined in global.R (loaded before this file)

# Molecular weights (g/mol) - used by report_functions.R for unit conversion.
# Only define if not already set by global.R.
if (!exists("MOLECULAR_WEIGHTS")) {
  MOLECULAR_WEIGHTS <- list(
    saxitoxin = 299.29,
    brevetoxin = 895.08,
    ciguatoxin = 1111.68,
    cortisol = 362.46,
    testosterone = 288.42,
    estradiol = 272.38
  )
}

# QC thresholds - used by report for flagging and quality checks
if (!exists("QC_THRESHOLDS") || is.null(QC_THRESHOLDS$cv_limit)) {
  QC_THRESHOLDS <- list(
    cv_limit = 30,                    # CV% limit for standards
    hill_slope_tolerance = 0.2,       # +/-20% tolerance for Hill slope
    qc_deviation_limit = 30,          # +/-30% tolerance for QC samples
    ec50_se_limit = 0.3               # EC50 standard error limit
  )
}

# Statistical thresholds used across the report template.
# Values chosen based on common immunoassay-analysis conventions; change
# in one place to propagate everywhere.
STATS_CONFIG <- list(
  # Reproducibility seed — used by set.seed() in the Rmd setup chunk so that
  # every rendered report uses the same RNG state for bootstrap CIs.
  report_seed = 42L,

  # Bootstrap resampling
  bootstrap_iterations = 1000,     # per-replicate-group percentile bootstrap

  # Outlier detection
  mad_outlier_threshold = 3,       # MAD z-score cutoff for non-normal data
                                   # (Leys et al. 2013, "very conservative" = 3)
  dixon_alpha = 0.05,              # Dixon's Q-test significance level
  shapiro_alpha = 0.05,            # Shapiro-Wilk normality pre-test level

  # ED() calls: drc::ED measures response *reduction* from the top asymptote,
  # so respLev = 80 returns the concentration where response has dropped to
  # 20% (EC20), and respLev = 20 returns EC80.
  ec20_resp_level = 80,
  ec80_resp_level = 20,

  # Heteroscedasticity variance-ratio heuristic (fallback when formal
  # Brown-Forsythe is not feasible)
  heteroscedasticity_variance_ratio_strong = 10,
  heteroscedasticity_variance_ratio_moderate = 3,

  # Display conventions
  ci_truncation_floor = 0          # negative lower bounds displayed as 0
)

# Plot configuration - colors and axis settings for report figures
PLOT_CONFIG <- list(
  width = 10,
  height = 7,
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
  x_limits = c(1e-12, 1e-5),
  x_breaks = 10^seq(-12, -5, by = 1)
)

# Units and conversion factors per assay type
UNIT_CONFIG <- list(
  rba = list(
    concentration_unit = "mol/L",
    mass_unit = "\u00B5g/L",
    response_unit = "CPM",
    mass_conversion_factor = 1e6
  ),
  elisa = list(
    concentration_unit = "pg/mL",
    mass_unit = "ng/mL",
    response_unit = "%B/B0",
    mass_conversion_factor = 1e3
  )
)

# Response variable mapping
RESPONSE_VARIABLES <- list(
  rba = "MeasurementValue",
  elisa = "NormalizedValue"
)

# Default standard concentrations live in global.R (DEFAULT_STX_CONC /
# DEFAULT_CORTISOL_CONC / DEFAULT_TESTOSTERONE_CONC / DEFAULT_ESTRADIOL_CONC)
# — the single source of truth. A second, unused copy here previously
# diverged from global.R (AUDIT-036); do not reintroduce one.

# Table styling configuration
TABLE_CONFIG <- list(
  html_options = c("striped", "hover", "responsive"),
  full_width = FALSE,
  position = "center",
  digits = 2
)

# Validation rules for input data
VALIDATION_RULES <- list(
  required_columns = c("SampleID", "MeasurementValue", "DilutionFactor",
                       "StandardConc", "SampleType", "Replicate"),
  optional_columns = c("NormalizedValue", "ResponseUnit"),
  numeric_columns = c("MeasurementValue", "DilutionFactor", "StandardConc"),
  positive_columns = c("DilutionFactor"),
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
  version = tryCatch(as.character(utils::packageVersion("RBAElisaApp")), error = function(e) "1.0.0"),
  authors = c("Kristof Moeller (IAEA)", "Arnold Molina Porras (UCR)"),
  contact = "kr.moeller@iaea.org",
  feedback_url = "https://forms.office.com/e/q8eqJfp4QM"
)

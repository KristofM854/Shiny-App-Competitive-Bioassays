# ==============================================================================
# Core Report Functions
# Purpose: Main data processing and analysis functions for RBA/ELISA reports
# ==============================================================================

# Source constants
if (!exists("MOLECULAR_WEIGHTS")) {
  source("report_constants.R")
}

#' Validate input data structure and content
#' @param data_long Long format data from CSV
#' @return TRUE if valid, stops with error if invalid
validate_input_data <- function(data_long) {

  # Check required columns
  missing_cols <- setdiff(VALIDATION_RULES$required_columns, names(data_long))
  if (length(missing_cols) > 0) {
    stop("Missing required columns: ", paste(missing_cols, collapse = ", "))
  }

  # Check for reasonable data ranges
  if (any(data_long$DilutionFactor <= 0, na.rm = TRUE)) {
    stop("Invalid dilution factors found (must be > 0)")
  }

  # Check for valid sample types
  invalid_types <- setdiff(data_long$SampleType, VALIDATION_RULES$sample_types)
  if (length(invalid_types) > 0) {
    warning("Unknown sample types found: ", paste(invalid_types, collapse = ", "))
  }

  # Check for standards
  standards <- data_long[data_long$SampleType == "Standard" & !is.na(data_long$StandardConc), ]
  if (nrow(standards) < 4) {
    stop("Insufficient standards for curve fitting (need at least 4)")
  }

  return(TRUE)
}

#' Load and validate assay configuration
#' @param output_dir Path to output directory
#' @return List with assay configuration
load_assay_config <- function(output_dir) {
  config_file <- file.path(output_dir, OUTPUT_FILES$assay_config)

  if (file.exists(config_file)) {
    config <- jsonlite::fromJSON(config_file)
  } else {
    # Fallback to RBA defaults
    warning("No assay config found, defaulting to RBA")
    config <- list(
      assay_type = "rba",
      detection_method = "radioligand",
      toxin_standard_label = "Saxitoxin",
      molecular_weight_g_mol = 299.29
    )
  }

  return(config)
}

#' Get appropriate axis labels based on assay configuration
#' @param assay_config Assay configuration list
#' @return List with x_label, y_label, and units
get_axis_labels <- function(assay_config) {

  if (assay_config$assay_type == "elisa") {

    if (assay_config$detection_method == "absorbance") {
      y_label <- "Response (%B/B0)"
      y_unit <- "%B/B0"
    } else {
      y_label <- "Absorbance"
      y_unit <- "Abs"
    }

    x_label <- paste0("Standard concentration (", assay_config$units %||% "pg/mL", ")")

  } else {
    # RBA
    if (assay_config$detection_method == "radioligand") {
      y_label <- "Counts per minute (CPM)"
      y_unit <- "CPM"
    } else {
      y_label <- "Relative fluorescence units (RFU)"
      y_unit <- "RFU"
    }

    x_label <- "Standard concentration (mol/L)"
  }

  return(list(
    x_label = x_label,
    y_label = y_label,
    y_unit = y_unit
  ))
}

#' Calculate coefficient of variation
#' @param x Numeric vector
#' @return CV as percentage
coefficient_of_variation <- function(x) {
  m <- mean(x, na.rm = TRUE)
  if (is.na(m) || m == 0) return(NA_real_)
  sd(x, na.rm = TRUE) / m * 100
}

#' Identify standards with high variability
#' @param data_long Data frame with standards
#' @param cv_limit CV percentage threshold (default from constants)
#' @return Data frame with high variability standards
identify_high_variability_standards <- function(data_long, cv_limit = QC_THRESHOLDS$cv_limit) {

  response_var <- if ("NormalizedValue" %in% names(data_long)) "NormalizedValue" else "MeasurementValue"

  high_var <- data_long %>%
    dplyr::filter(SampleType == "Standard", !is.na(StandardConc)) %>%
    group_by(StandardConc) %>%
    summarise(
      cv = coefficient_of_variation(.data[[response_var]]),
      n_replicates = n(),
      .groups = "drop"
    ) %>%
    dplyr::filter(cv >= cv_limit | n_replicates < 2)

  return(high_var)
}

#' Load or create sample processing configuration
#' @param output_dir Directory containing analysis files
#' @return List with extraction and tissue processing parameters
load_sample_processing_config <- function(output_dir) {

  config_file <- file.path(output_dir, "sample_processing_config.json")

  # Default configuration
  default_config <- list(
    extraction_volume_ul = 500,          # Default extraction volume in microliters
    tissue_mass_mg = NULL,               # Tissue mass in milligrams (NULL = not tissue-based)
    sample_type = "extracted",           # "extracted", "direct", "tissue"
    extraction_method = "standard",      # For documentation
    notes = "Default extraction parameters"
  )

  if (file.exists(config_file)) {
    # Load existing configuration
    tryCatch({
      loaded_config <- jsonlite::fromJSON(config_file, simplifyVector = FALSE)
      # Merge with defaults to handle missing fields
      config <- modifyList(default_config, loaded_config)
      return(config)
    }, error = function(e) {
      warning("Could not load sample processing config: ", e$message, ". Using defaults.")
      return(default_config)
    })
  } else {
    # Create default configuration file
    jsonlite::write_json(default_config, config_file, pretty = TRUE, auto_unbox = TRUE)
    message("Created default sample processing config at: ", config_file)
    return(default_config)
  }
}

#' Calculate proper B/B0 values for ELISA following Cayman protocol
#' @param data_long Data frame with ELISA data including control wells
#' @param use_percent Whether to return %B/B0 (TRUE) or B/B0 (FALSE)
#' @return Data frame with calculated B/B0 values
calculate_elisa_bb0 <- function(data_long, use_percent = TRUE) {

  # Step 1: Identify control wells and their values
  blank_wells <- data_long %>%
    dplyr::filter(SampleType == "Blank") %>%
    pull(MeasurementValue)

  nsb_wells <- data_long %>%
    dplyr::filter(SampleType == "NSB") %>%
    pull(MeasurementValue)

  b0_wells <- data_long %>%
    dplyr::filter(SampleType == "B0") %>%
    pull(MeasurementValue)

  if (length(blank_wells) == 0 || length(nsb_wells) == 0 || length(b0_wells) == 0) {
    stop("Missing control wells. Need Blank, NSB, and B0 wells for ELISA analysis.")
  }

  # Step 2: Calculate averages (Cayman protocol page 29)
  blank_avg <- mean(blank_wells, na.rm = TRUE)
  nsb_avg <- mean(nsb_wells, na.rm = TRUE) - blank_avg  # Blank-corrected NSB
  b0_avg <- mean(b0_wells, na.rm = TRUE) - blank_avg   # Blank-corrected B0

  # Step 3: Calculate corrected B0 (maximum binding)
  corrected_b0 <- b0_avg - nsb_avg

  if (corrected_b0 <= 0) {
    stop("Invalid control well hierarchy. B0 must be > NSB after blank correction.")
  }

  # Step 4: Calculate B/B0 for all wells
  data_with_bb0 <- data_long %>%
    mutate(
      blank_corrected = MeasurementValue - blank_avg,
      nsb_corrected = blank_corrected - nsb_avg,
      b_b0_ratio = nsb_corrected / corrected_b0,
      calculated_bb0 = if (use_percent) b_b0_ratio * 100 else b_b0_ratio
    ) %>%
    dplyr::select(-blank_corrected, -nsb_corrected, -b_b0_ratio)

  # Add control well summary for QC
  attr(data_with_bb0, "control_summary") <- list(
    blank_avg = blank_avg,
    nsb_avg = nsb_avg + blank_avg,  # Report original NSB average
    b0_avg = b0_avg + blank_avg,    # Report original B0 average
    corrected_b0 = corrected_b0,
    hierarchy_valid = (b0_avg + blank_avg) > (nsb_avg + blank_avg)
  )

  return(data_with_bb0)
}

#' Assess heteroscedasticity in residuals from an unweighted DRC model
#'
#' Runs a Brown-Forsythe / Levene-type test on squared residuals grouped by
#' standard concentration level.
#' Falls back to a variance-ratio heuristic when there are too few groups or
#' replicates for a formal test (< 3 groups or < 2 replicates per group).
#'
#' @param model A fitted drc model object (unweighted)
#' @param data_long Data frame of standards used for fitting
#' @param response_var Name of the response variable column
#' @return A list with test_name, statistic, p_value, interpretation,
#'         recommendation, and variance_ratio
assess_heteroscedasticity <- function(model, data_long, response_var) {

  result <- list(
    test_name = NA_character_,
    statistic = NA_real_,
    p_value = NA_real_,
    interpretation = "Could not assess heteroscedasticity.",
    recommendation = "No recommendation available.",
    variance_ratio = NA_real_
  )

  tryCatch({
    # Extract residuals
    resid_vals <- residuals(model)
    conc_levels <- data_long$StandardConc

    if (length(resid_vals) != nrow(data_long)) {
      result$interpretation <- "Residual count does not match data rows; skipping test."
      return(result)
    }

    # Compute per-group variance statistics
    groups <- split(resid_vals, conc_levels)
    groups <- groups[sapply(groups, length) >= 1]  # drop empty

    if (length(groups) < 2) {
      result$interpretation <- "Fewer than 2 concentration groups; cannot assess heteroscedasticity."
      return(result)
    }

    group_vars <- sapply(groups, function(g) if (length(g) >= 2) var(g) else NA_real_)
    valid_vars <- group_vars[!is.na(group_vars) & group_vars > 0]
    if (length(valid_vars) >= 2) {
      result$variance_ratio <- max(valid_vars) / min(valid_vars)
    }

    # Determine if formal test is feasible:
    # Need >= 3 groups each with >= 2 replicates
    reps_per_group <- sapply(groups, length)
    formal_feasible <- length(groups) >= 3 && all(reps_per_group >= 2)

    if (formal_feasible) {
      # Brown-Forsythe / Levene test (using median, robust to non-normality)
      # Try car::leveneTest first, fall back to manual implementation
      bf_result <- tryCatch({
        if (requireNamespace("car", quietly = TRUE)) {
          df_test <- data.frame(
            residual = resid_vals,
            group = factor(conc_levels)
          )
          lt <- car::leveneTest(residual ~ group, data = df_test, center = "median")
          list(
            test_name = "Brown-Forsythe (Levene, median)",
            statistic = lt[["F value"]][1],
            p_value = lt[["Pr(>F)"]][1]
          )
        } else {
          NULL
        }
      }, error = function(e) NULL)

      if (is.null(bf_result)) {
        # Manual Brown-Forsythe implementation
        group_medians <- sapply(groups, median)
        abs_devs <- mapply(function(g, med) abs(g - med),
                           groups, group_medians, SIMPLIFY = FALSE)
        all_devs <- unlist(abs_devs)
        group_labels <- rep(names(groups), times = sapply(abs_devs, length))
        group_factor <- factor(group_labels)

        grand_mean <- mean(all_devs)
        k <- nlevels(group_factor)
        n_total <- length(all_devs)

        group_means <- tapply(all_devs, group_factor, mean)
        group_ns <- tapply(all_devs, group_factor, length)

        ss_between <- sum(group_ns * (group_means - grand_mean)^2)
        ss_within <- sum((all_devs - group_means[group_factor])^2)

        df_between <- k - 1
        df_within <- n_total - k

        if (df_within > 0 && ss_within > 0) {
          f_stat <- (ss_between / df_between) / (ss_within / df_within)
          p_val <- pf(f_stat, df_between, df_within, lower.tail = FALSE)
          bf_result <- list(
            test_name = "Brown-Forsythe (manual)",
            statistic = f_stat,
            p_value = p_val
          )
        }
      }

      if (!is.null(bf_result)) {
        result$test_name <- bf_result$test_name
        result$statistic <- bf_result$statistic
        result$p_value <- bf_result$p_value

        if (!is.na(bf_result$p_value) && bf_result$p_value < 0.05) {
          result$interpretation <- sprintf(
            "Significant heteroscedasticity detected (F = %.2f, p = %.4f). Residual variance differs across concentration levels.",
            bf_result$statistic, bf_result$p_value)
          result$recommendation <- "Weighted regression (1/Y or 1/Y^2) is recommended to account for unequal variance."
        } else {
          result$interpretation <- sprintf(
            "No significant heteroscedasticity detected (F = %.2f, p = %.4f). Residual variance is approximately constant.",
            bf_result$statistic, bf_result$p_value)
          result$recommendation <- "Unweighted regression is appropriate for these data."
        }
        return(result)
      }
    }

    # Fallback: variance-ratio heuristic
    result$test_name <- "Variance-ratio heuristic"
    if (!is.na(result$variance_ratio)) {
      result$statistic <- result$variance_ratio
      if (result$variance_ratio > 10) {
        result$interpretation <- sprintf(
          "Large variance ratio (%.1f) across concentration levels suggests strong heteroscedasticity.",
          result$variance_ratio)
        result$recommendation <- "Weighted regression (1/Y or 1/Y^2) is strongly recommended."
      } else if (result$variance_ratio > 3) {
        result$interpretation <- sprintf(
          "Moderate variance ratio (%.1f) across concentration levels suggests possible heteroscedasticity.",
          result$variance_ratio)
        result$recommendation <- "Weighted regression (1/Y or 1/Y^2) may improve the fit."
      } else {
        result$interpretation <- sprintf(
          "Low variance ratio (%.1f) across concentration levels; variance appears approximately constant.",
          result$variance_ratio)
        result$recommendation <- "Unweighted regression is appropriate for these data."
      }
    } else {
      result$interpretation <- "Insufficient replicates to compute per-group variance."
      result$recommendation <- "Collect additional replicates for a robust heteroscedasticity assessment."
    }

  }, error = function(e) {
    result$interpretation <<- paste("Heteroscedasticity assessment failed:", e$message)
  })

  return(result)
}

#' Assess model stability and convergence quality for a fitted DRC model
#'
#' Extracts convergence status, standard-error availability, parameter
#' magnitudes, boundary-hitting indicators, and captured warnings to assign
#' an overall stability grade.
#'
#' @param fit_object A single entry from the all_models list (contains $model,
#'        $fit_method, $coefs, $R2, etc.)
#' @param model_name A human-readable label for the model
#' @return A list with converged, fit_method, se_available, boundary_flag,
#'         suspicious_params, warnings, and stability_grade
assess_model_stability <- function(fit_object, model_name) {

  result <- list(
    model_name = model_name,
    converged = FALSE,
    fit_method = NA_character_,
    se_available = FALSE,
    boundary_flag = FALSE,
    suspicious_params = character(0),
    warnings = character(0),
    stability_grade = "failed"
  )

  tryCatch({
    # Handle interpolation fallback
    if (is.null(fit_object$model)) {
      result$fit_method <- fit_object$fit_method %||% "interpolation"
      result$converged <- FALSE
      result$stability_grade <- "failed"
      result$warnings <- c(result$warnings,
                           "No parametric model could be fitted; using interpolation fallback.")
      return(result)
    }

    fit <- fit_object$model
    result$fit_method <- fit_object$fit_method %||% "LL.4"

    # Convergence: drc models that return without error have converged
    result$converged <- TRUE

    # Standard errors
    se_info <- tryCatch({
      s <- summary(fit)
      se_vals <- s$coefficients[, "Std. Error"]
      list(available = all(!is.na(se_vals) & is.finite(se_vals)),
           values = se_vals)
    }, error = function(e) {
      list(available = FALSE, values = NULL)
    })
    result$se_available <- se_info$available

    if (!result$se_available) {
      result$warnings <- c(result$warnings,
                           "Standard errors could not be computed; parameter uncertainty is unknown.")
    }

    # Check parameter magnitudes for suspiciously large values
    coefs <- coef(fit)
    param_names <- c("Hill slope (b)", "Bottom (c)", "Top (d)", "IC50 (e)")
    if (length(coefs) >= 4) {
      hill <- abs(coefs[1])
      bottom <- coefs[2]
      top <- coefs[3]
      ic50 <- coefs[4]

      if (hill > 10) {
        result$suspicious_params <- c(result$suspicious_params,
                                      sprintf("Hill slope = %.2f (unusually steep)", hill))
      }
      if (hill < 0.3) {
        result$suspicious_params <- c(result$suspicious_params,
                                      sprintf("Hill slope = %.2f (unusually shallow)", hill))
      }
      if (!is.na(ic50) && ic50 <= 0) {
        result$suspicious_params <- c(result$suspicious_params,
                                      sprintf("IC50 = %.4g (non-positive)", ic50))
      }

      # Check if SE >> estimate (parameter poorly determined)
      if (result$se_available && !is.null(se_info$values)) {
        for (i in seq_along(coefs)) {
          if (i <= length(se_info$values) && i <= length(param_names)) {
            est <- abs(coefs[i])
            se <- se_info$values[i]
            if (!is.na(se) && !is.na(est) && est > 0 && se / est > 2) {
              result$suspicious_params <- c(result$suspicious_params,
                                            sprintf("%s: SE/|estimate| = %.1f (poorly determined)",
                                                    param_names[i], se / est))
            }
          }
        }
      }
    }

    # Boundary-hitting check: compare estimates to any bounds that were used
    # drc stores bounds in fit$lowerl / fit$upperl if set
    lowerl <- tryCatch(fit$lowerl, error = function(e) NULL)
    upperl <- tryCatch(fit$upperl, error = function(e) NULL)

    if (!is.null(lowerl) && length(lowerl) == length(coefs)) {
      for (i in seq_along(coefs)) {
        if (!is.na(lowerl[i]) && abs(coefs[i] - lowerl[i]) < 1e-6) {
          result$boundary_flag <- TRUE
          result$warnings <- c(result$warnings,
                               sprintf("Parameter %d is at its lower bound (%.4g).",
                                       i, lowerl[i]))
        }
      }
    }
    if (!is.null(upperl) && length(upperl) == length(coefs)) {
      for (i in seq_along(coefs)) {
        if (!is.na(upperl[i]) && abs(coefs[i] - upperl[i]) < 1e-6) {
          result$boundary_flag <- TRUE
          result$warnings <- c(result$warnings,
                               sprintf("Parameter %d is at its upper bound (%.4g).",
                                       i, upperl[i]))
        }
      }
    }

    # Determine stability grade
    n_issues <- length(result$suspicious_params) + length(result$warnings)

    if (!result$converged) {
      result$stability_grade <- "failed"
    } else if (result$fit_method == "LL.3") {
      # LL.3 is a fallback; at best "acceptable"
      if (result$se_available && n_issues == 0) {
        result$stability_grade <- "acceptable"
      } else {
        result$stability_grade <- "unstable"
      }
    } else if (!result$se_available || result$boundary_flag) {
      result$stability_grade <- "unstable"
    } else if (n_issues >= 3) {
      result$stability_grade <- "unstable"
    } else if (n_issues >= 1) {
      result$stability_grade <- "acceptable"
    } else {
      result$stability_grade <- "good"
    }

  }, error = function(e) {
    result$stability_grade <<- "failed"
    result$warnings <<- c(result$warnings, paste("Stability assessment error:", e$message))
  })

  return(result)
}

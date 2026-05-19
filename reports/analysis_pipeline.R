# =============================================================================
# reports/analysis_pipeline.R
#
# Compute-layer for the analysis Rmd templates. Pure-compute helpers extracted
# from reports/unified_analysis_template.Rmd so the template shrinks to an
# orchestration + presentation layer (IMPLEMENTATION_PLAN.md, §M1).
#
# Each function in this file MUST preserve the numeric output of the
# pre-refactor template byte-for-byte (verified by tests/testthat/
# test-report-numbers.R for the RBA path, and by archived "before" reports
# for the ELISA path).
#
# Status:
#   M1.5 assess_plate_positional()   - implemented
#
#   M1.1 quantify_samples()          - implemented
#   M1.2 fit_all_models()            - implemented (folds in H2 auto-weighting)
#   M1.3 determine_lloq_uloq()       - implemented
#   M1.4 compute_standard_recovery() - implemented
# =============================================================================

# Plate positional QC: per-row and per-column response statistics, flagging
# rows or columns whose mean response deviates more than 2 SD from the
# plate-wide mean (a common edge-effect heuristic).
#
# Args:
#   data_long: long-format plate data with at minimum Row, Column, and
#              MeasurementValue columns. Row/Column are typically character.
#
# Returns:
#   NULL if `data_long` lacks MeasurementValue or has no finite values.
#   Otherwise a list:
#     plate_mean   numeric scalar, plate-wide mean of finite values
#     plate_sd     numeric scalar, plate-wide SD of finite values
#     row_stats    tibble: Row, Mean, SD, Flag  (Flag = "⚠" or "")
#     col_stats    tibble: Column, Mean, SD, Flag
#     flagged_rows integer count of rows with non-empty Flag
#     flagged_cols integer count of columns with non-empty Flag
#
# Mirrors the in-line computation of the `plate-positional-qc` chunk in
# reports/unified_analysis_template.Rmd. The Rmd remains responsible for
# rendering decisions (which table to print, what wording to emit).
assess_plate_positional <- function(data_long) {
  if (!is.data.frame(data_long) ||
      !"MeasurementValue" %in% names(data_long)) {
    return(NULL)
  }
  plate_data <- dplyr::filter(data_long, is.finite(.data$MeasurementValue))
  if (nrow(plate_data) == 0) return(NULL)

  plate_mean <- mean(plate_data$MeasurementValue, na.rm = TRUE)
  plate_sd   <- sd(plate_data$MeasurementValue,   na.rm = TRUE)

  flag_char <- '<span class="bs-status-pill is-warn">flag</span>'

  row_stats <- plate_data %>%
    dplyr::group_by(.data$Row) %>%
    dplyr::summarise(Mean = mean(.data$MeasurementValue, na.rm = TRUE),
                     SD   = sd(.data$MeasurementValue,   na.rm = TRUE),
                     .groups = "drop") %>%
    dplyr::mutate(Flag = ifelse(abs(.data$Mean - plate_mean) > 2 * plate_sd,
                                flag_char, ""))

  col_stats <- plate_data %>%
    dplyr::group_by(.data$Column) %>%
    dplyr::summarise(Mean = mean(.data$MeasurementValue, na.rm = TRUE),
                     SD   = sd(.data$MeasurementValue,   na.rm = TRUE),
                     .groups = "drop") %>%
    dplyr::mutate(Flag = ifelse(abs(.data$Mean - plate_mean) > 2 * plate_sd,
                                flag_char, ""))

  list(
    plate_mean   = plate_mean,
    plate_sd     = plate_sd,
    row_stats    = row_stats,
    col_stats    = col_stats,
    flagged_rows = sum(row_stats$Flag == flag_char),
    flagged_cols = sum(col_stats$Flag == flag_char)
  )
}


# Standard back-calculation: predict each standard well's concentration from
# the fitted model and compute % recovery vs nominal. Mirrors the compute
# block of the `standard-backcalculation` chunk in unified_analysis_template.Rmd.
#
# Returns a list with two data frames:
#   standards_backcalc: per-well back-calculated concentrations + % recovery
#   recovery_summary:   per-nominal-concentration mean / sd of back-calculated
#                       conc + mean recovery (%), arranged by descending
#                       nominal concentration
#
# The Rmd chunk consumes recovery_summary for rendering and for the executive
# summary's accuracy block.
compute_standard_recovery <- function(data_long, model_fit, response_var,
                                      is_elisa) {
  standards_backcalc <- data_long %>%
    dplyr::filter(SampleType == "Standard",
                  !is.na(concentration),
                  !is.na(.data[[response_var]])) %>%
    dplyr::rowwise() %>%
    dplyr::mutate(
      backcalc_conc = tryCatch({
        pred <- ED(model_fit, respLev = .data[[response_var]],
                   type = "absolute", display = FALSE)
        as.numeric(pred[1, 1])
      }, error = function(e) NA_real_),
      recovery_pct = if (!is.na(backcalc_conc) && concentration > 0) {
        (backcalc_conc / concentration) * 100
      } else {
        NA_real_
      }
    ) %>%
    dplyr::ungroup()

  recovery_summary <- standards_backcalc %>%
    dplyr::group_by(concentration) %>%
    dplyr::summarise(
      n = dplyr::n(),
      nominal = dplyr::first(concentration),
      mean_backcalc = mean(backcalc_conc, na.rm = TRUE),
      sd_backcalc = sd(backcalc_conc, na.rm = TRUE),
      mean_recovery = mean(recovery_pct, na.rm = TRUE),
      .groups = "drop"
    ) %>%
    dplyr::arrange(dplyr::desc(nominal))

  list(
    standards_backcalc = standards_backcalc,
    recovery_summary   = recovery_summary
  )
}


# Formal LLOQ/ULOQ determination by back-calculated standard accuracy.
# Mirrors the compute block of the `lloq-uloq` chunk in
# unified_analysis_template.Rmd: back-calculate each standard well, aggregate
# by nominal concentration, mark levels passing the 80-120% / CV<20% gate,
# and return min/max of passing levels as formal LLOQ/ULOQ.
#
# Inputs:
#   standards_for_model: data frame of standards used to fit the model
#                        (must include `concentration` and the column named
#                        by `response_var`).
#   model_fit:           a fitted drc model (or NULL if fitting failed).
#   response_var:        name of the response column in standards_for_model.
#   lang:                language code consumed by tr() in the chunk for
#                        the accuracy column label. The function returns the
#                        unlocalised pass/fail flag separately so the chunk
#                        can build the localised label from it.
#
# Returns:
#   list(
#     backcalc_results: per-row predictions and recovery,
#     backcalc_summary: aggregated by nominal_conc with `passes` flag and
#                       `recovery`, `cv_backcalc`, etc.,
#     formal_lloq:      min(passing nominal); NA_real_ if <2 passing levels,
#     formal_uloq:      max(passing nominal); NA_real_ if <2 passing levels,
#     passing_concs:    numeric vector of nominal concentrations passing the
#                       accuracy / precision gate
#   )
determine_lloq_uloq <- function(standards_for_model, model_fit, response_var,
                                is_elisa) {
  backcalc_results <- data.frame(
    nominal_conc      = standards_for_model$concentration,
    observed_response = standards_for_model[[response_var]],
    stringsAsFactors  = FALSE
  )

  backcalc_results$backcalc_conc <- sapply(
    seq_len(nrow(backcalc_results)),
    function(i) {
      tryCatch({
        resp <- backcalc_results$observed_response[i]
        ed <- ED(model_fit, respLev = resp, type = "absolute", display = FALSE)
        as.numeric(ed[1, 1])
      }, error = function(e) NA_real_)
    }
  )

  backcalc_results$recovery <-
    (backcalc_results$backcalc_conc / backcalc_results$nominal_conc) * 100

  backcalc_summary <- backcalc_results %>%
    dplyr::group_by(nominal_conc) %>%
    dplyr::summarise(
      n             = dplyr::n(),
      mean_backcalc = mean(backcalc_conc, na.rm = TRUE),
      mean_recovery = mean(recovery,      na.rm = TRUE),
      cv_backcalc   = ifelse(
        dplyr::n() >= 2,
        (sd(backcalc_conc, na.rm = TRUE) / mean(backcalc_conc, na.rm = TRUE)) * 100,
        NA_real_
      ),
      .groups = "drop"
    ) %>%
    dplyr::filter(is.finite(mean_recovery) & mean_recovery > 0) %>%
    dplyr::mutate(
      passes = (mean_recovery >= 80 & mean_recovery <= 120) &
        (is.na(cv_backcalc) | cv_backcalc < 20)
    )

  passing_concs <- if (nrow(backcalc_summary) > 0) {
    backcalc_summary %>%
      dplyr::filter(passes) %>%
      dplyr::pull(nominal_conc)
  } else {
    numeric(0)
  }

  if (length(passing_concs) >= 2) {
    formal_lloq <- min(passing_concs)
    formal_uloq <- max(passing_concs)
  } else {
    formal_lloq <- NA_real_
    formal_uloq <- NA_real_
  }

  list(
    backcalc_results = backcalc_results,
    backcalc_summary = backcalc_summary,
    formal_lloq      = formal_lloq,
    formal_uloq      = formal_uloq,
    passing_concs    = passing_concs
  )
}


# Fit dose-response model(s) and derive everything downstream chunks need.
# Mirrors the body of the `model-fitting` chunk in unified_analysis_template.Rmd:
#   * mark high-variability standards on data_long
#   * filter standards used for fitting
#   * build per-weight weight vectors (none / 1/Y / 1/Y^2)
#   * H2 auto: pick a weight via heteroscedasticity diagnostic if "auto" is in
#     selected_weights
#   * fit each selected weight with the LL.4 -> LL.3 chain; stop() if all fail
#   * compute R^2, RMSE, AIC, lack-of-fit, convergence info per fit
#   * pick the primary model (first successfully fitted weight) and surface
#     its R^2/RMSE/weight_desc/model_fit at the top level
#   * generate prediction grid for plotting
#   * compute EC20 / EC80, swapping if inverted
#   * for ELISA, surface bb0_lower / bb0_upper for the closure helpers below
#
# The chunk re-creates the closure helpers classify_range() and flag_range()
# locally so they capture the chunk environment (not this function's), and
# remain consistent with the legacy behaviour.
#
# Inputs: the chunk-local variables the original block read.
# Returns a named list. The caller is expected to inject these names into the
# chunk environment via `list2env(result, envir = environment())`.
fit_all_models <- function(data_long, response_var, analysis_config,
                           is_elisa, lang, cv_limit, STATS_CONFIG) {
  # Add concentration column and identify high variability standards.
  data_long <- data_long %>%
    dplyr::mutate(concentration = ifelse(SampleType == "Standard",
                                         StandardConc, NA_real_))

  high_var_standards <- identify_high_variability_standards(
    data_long, cv_limit = cv_limit
  )

  data_long <- data_long %>%
    dplyr::mutate(
      high_variability = ifelse(
        SampleType == "Standard" &
          concentration %in% high_var_standards$StandardConc,
        "High Variability",
        "Normal Variability"
      )
    )

  standards_for_model <- data_long %>%
    dplyr::filter(
      SampleType == "Standard",
      !is.na(concentration),
      high_variability == "Normal Variability",
      !is.na(.data[[response_var]])
    )

  # Build list of weightings to fit
  weight_options <- list(
    none   = list(weights = NULL, label = "Unweighted"),
    inv_y  = list(label = "1/Y weighted"),
    inv_y2 = list(label = "1/Y² weighted")
  )

  resp_vals <- standards_for_model[[response_var]]
  weight_options$inv_y$weights  <- ifelse(resp_vals > 0, 1 / resp_vals,        1)
  weight_options$inv_y2$weights <- ifelse(resp_vals > 0, 1 / (resp_vals^2),    1)

  stopifnot(length(weight_options$inv_y$weights)  == nrow(standards_for_model))
  stopifnot(length(weight_options$inv_y2$weights) == nrow(standards_for_model))

  selected_weights <- analysis_config$regression_weight
  if (is.null(selected_weights) || length(selected_weights) == 0) {
    selected_weights <- "none"
  }
  if (is.character(selected_weights)) {
    selected_weights <- as.character(selected_weights)
  }

  multi_weight_mode <- length(selected_weights) > 1

  # H2: auto (data-driven) weighting
  auto_weighting_result <- NULL
  if ("auto" %in% selected_weights) {
    initial_fml <- as.formula(paste(response_var, "~ concentration"))
    initial_fit <- tryCatch(
      drc::drm(initial_fml, data = standards_for_model, fct = drc::LL.4()),
      error = function(e) NULL
    )
    if (is.null(initial_fit)) {
      auto_weighting_result <- list(
        chosen_weight  = "inv_y2",
        fallback       = TRUE,
        degenerate     = FALSE,
        test_name      = NA_character_, statistic = NA_real_,
        p_value        = NA_real_,      variance_ratio = NA_real_
      )
    } else {
      hetero_auto <- assess_heteroscedasticity(initial_fit, standards_for_model,
                                               response_var)
      chosen <- if (!is.na(hetero_auto$p_value) && hetero_auto$p_value < 0.01 &&
                    !is.na(hetero_auto$variance_ratio) &&
                    hetero_auto$variance_ratio > 10) {
        "inv_y2"
      } else if (!is.na(hetero_auto$p_value) && hetero_auto$p_value < 0.05) {
        "inv_y"
      } else if (!is.na(hetero_auto$variance_ratio) &&
                 hetero_auto$variance_ratio > 10) {
        "inv_y2"
      } else if (!is.na(hetero_auto$variance_ratio) &&
                 hetero_auto$variance_ratio >= 3) {
        "inv_y"
      } else {
        "none"
      }
      auto_weighting_result <- list(
        chosen_weight  = chosen,
        fallback       = FALSE,
        degenerate     = isTRUE(hetero_auto$degenerate),
        test_name      = hetero_auto$test_name,
        statistic      = hetero_auto$statistic,
        p_value        = hetero_auto$p_value,
        variance_ratio = hetero_auto$variance_ratio
      )
    }
    selected_weights  <- auto_weighting_result$chosen_weight
    multi_weight_mode <- FALSE
  }

  # Fit ALL selected models with the LL.4 -> LL.3 chain.
  all_models <- list()
  for (wt_key in selected_weights) {
    wt_info <- weight_options[[wt_key]]
    if (is.null(wt_info)) next

    fml <- as.formula(paste(response_var, "~ concentration"))
    fit <- NULL
    fit_method <- "LL.4"

    fit <- tryCatch({
      if (isTRUE(is_elisa)) {
        if (is.null(wt_info$weights)) {
          drc::drm(fml, data = standards_for_model, fct = drc::LL.4(),
                   lowerl = c(NA, 0, NA, NA))
        } else {
          drc::drm(fml, data = standards_for_model, fct = drc::LL.4(),
                   weights = wt_info$weights,
                   lowerl = c(NA, 0, NA, NA))
        }
      } else {
        if (is.null(wt_info$weights)) {
          drc::drm(fml, data = standards_for_model, fct = drc::LL.4())
        } else {
          drc::drm(fml, data = standards_for_model, fct = drc::LL.4(),
                   weights = wt_info$weights)
        }
      }
    }, error = function(e) {
      warning("LL.4() failed for weight '", wt_key, "': ",
              e$message, ". Trying LL.3()...")
      NULL
    })

    if (!is.null(fit) && isTRUE(is_elisa) && fit_method == "LL.4") {
      d_val <- tryCatch(coef(fit)[["d:(Intercept)"]], error = function(e) NA_real_)
      if (!is.na(d_val) && (d_val > 120 || d_val < 80)) {
        warning("ELISA upper asymptote (d = ", round(d_val, 1),
                ") outside expected range [80, 120] for %B/B0 data.")
      }
    }

    if (is.null(fit)) {
      fit_method <- "LL.3"
      fit <- tryCatch({
        if (is.null(wt_info$weights)) {
          drc::drm(fml, data = standards_for_model, fct = drc::LL.3())
        } else {
          drc::drm(fml, data = standards_for_model, fct = drc::LL.3(),
                   weights = wt_info$weights)
        }
      }, error = function(e) {
        warning("LL.3() also failed for weight '", wt_key, "': ",
                e$message, ".")
        NULL
      })
    }

    if (!is.null(fit)) {
      resid     <- residuals(fit)
      resp_vals <- standards_for_model[[response_var]]

      if (!is.null(wt_info$weights)) {
        w <- wt_info$weights
        ss_res <- sum(w * resid^2)
        ss_tot <- sum(w * (resp_vals - weighted.mean(resp_vals, w))^2)
      } else {
        ss_res <- sum(resid^2)
        ss_tot <- sum((resp_vals - mean(resp_vals))^2)
      }

      if (ss_tot > 0) {
        r2 <- 1 - ss_res / ss_tot
      } else {
        r2 <- NA_real_
        warning(tr("r2_identical_warning", lang))
      }
      rmse  <- sqrt(mean(resid^2))
      coefs <- coef(fit)

      conv_info <- tryCatch({
        s <- summary(fit)
        se_vals <- s$coefficients[, "Std. Error"]
        list(converged = TRUE, se_available = all(!is.na(se_vals)),
             max_se = max(se_vals, na.rm = TRUE))
      }, error = function(e) list(converged = TRUE, se_available = FALSE,
                                  max_se = NA_real_))

      lof_result <- tryCatch({
        lof <- modelFit(fit)
        list(f_stat = lof[2, "F value"], p_value = lof[2, "p value"])
      }, error = function(e) NULL)

      aic_val <- tryCatch(AIC(fit), error = function(e) NA_real_)

      all_models[[wt_key]] <- list(
        model = fit, label = paste0(wt_info$label, " (", fit_method, ")"),
        R2 = r2, RMSE = rmse, coefs = coefs,
        AIC = aic_val,
        fit_method = fit_method,
        convergence = conv_info,
        lack_of_fit = lof_result
      )
    } else {
      warning("LL.4 and LL.3 both failed for weight '", wt_key,
              "'. Skipping this weighting option.")
    }
  }

  drc_failed_completely <- (length(all_models) == 0)
  if (drc_failed_completely) {
    stop("Dose-response curve fitting failed: LL.4 and LL.3 both failed for all ",
         "weighting options. Verify that standards have at least 4 unique ",
         "concentrations with valid numeric responses.")
  }
  if (!drc_failed_completely) {
    primary_key <- names(all_models)[[1]]
    model_fit   <- all_models[[primary_key]]$model
    weight_desc <- all_models[[primary_key]]$label
    R2          <- all_models[[primary_key]]$R2
    RMSE        <- all_models[[primary_key]]$RMSE
  } else {
    primary_key <- NA_character_
    model_fit   <- NULL
    weight_desc <- "None (all models failed)"
    R2          <- NA_real_
    RMSE        <- NA_real_
  }

  # Plotting prediction grid
  if (isTRUE(is_elisa)) {
    conc_min <- min(standards_for_model$concentration, na.rm = TRUE) * 0.1
    conc_max <- max(standards_for_model$concentration, na.rm = TRUE) * 10
    conc_range <- exp(seq(log(conc_min), log(conc_max), length.out = 1000))
  } else {
    conc_range <- seq(1e-12, 1e-5, length.out = 1000)
  }

  model_fits <- data.frame(conc = conc_range)
  if (!drc_failed_completely && !is.null(model_fit)) {
    model_fits$p <- predict(model_fit, newdata = model_fits)
  } else {
    model_fits$p <- NA_real_
  }

  # EC20 / EC80 (concentrations corresponding to STATS_CONFIG response levels)
  if (isTRUE(is_elisa)) {
    bb0_lower <- analysis_config$quant_range_min %||% 20
    bb0_upper <- analysis_config$quant_range_max %||% 80
  } else {
    bb0_lower <- NULL
    bb0_upper <- NULL
  }

  ec20 <- tryCatch({
    ed_result <- ED(model_fit, respLev = STATS_CONFIG$ec20_resp_level,
                    type = "absolute", display = FALSE)
    as.numeric(ed_result[1, 1])
  }, error = function(e) NA_real_)
  ec80 <- tryCatch({
    ed_result <- ED(model_fit, respLev = STATS_CONFIG$ec80_resp_level,
                    type = "absolute", display = FALSE)
    as.numeric(ed_result[1, 1])
  }, error = function(e) NA_real_)
  if (!is.na(ec20) && !is.na(ec80) && ec20 > ec80) {
    .tmp <- ec20; ec20 <- ec80; ec80 <- .tmp
  }

  result <- list(
    data_long             = data_long,
    high_var_standards    = high_var_standards,
    standards_for_model   = standards_for_model,
    weight_options        = weight_options,
    selected_weights      = selected_weights,
    multi_weight_mode     = multi_weight_mode,
    auto_weighting_result = auto_weighting_result,
    all_models            = all_models,
    drc_failed_completely = drc_failed_completely,
    primary_key           = primary_key,
    model_fit             = model_fit,
    weight_desc           = weight_desc,
    R2                    = R2,
    RMSE                  = RMSE,
    conc_range            = conc_range,
    model_fits            = model_fits,
    ec20                  = ec20,
    ec80                  = ec80
  )
  if (!is.null(bb0_lower)) result$bb0_lower <- bb0_lower
  if (!is.null(bb0_upper)) result$bb0_upper <- bb0_upper
  result
}


# Quantify all sample wells: predict concentration with delta-method CIs from
# the fitted model, apply dilution corrections, run optional outlier detection
# (Shapiro-Wilk + MAD/Dixon's Q/Grubbs), aggregate per replicate group with
# layered uncertainty (model + replicate + raw before truncation), tag
# within-range / LLOQ-ULOQ flags using the supplied closures, and (for ELISA
# with a tissue_weights.json sidecar) compute pg/g tissue.
#
# Mirrors the body of the `sample-analysis` chunk in
# unified_analysis_template.Rmd. The chunk now calls this function and
# list2env()s the result.
#
# Inputs:
#   data_long, response_var, model_fit, standards_for_model
#     analysis_config, STATS_CONFIG, is_elisa, output_dir
#       — direct chunk-local variables.
#   classify_range, flag_range
#       — closures defined in the chunk so they capture chunk-env state
#         (bb0_lower / bb0_upper / ec20 / ec80). Passed in so the function
#         can call them without re-creating the closures.
#
# Returns a list with these names (NULL where not produced):
#   sample_results, sample_results_clean, replicate_stats, replicate_summary
#   outlier_flags, outlier_method_log
#   tissue_weights, processing_config
quantify_samples <- function(data_long, response_var, model_fit,
                             standards_for_model, analysis_config,
                             STATS_CONFIG, is_elisa, output_dir,
                             classify_range, flag_range) {

  samples_to_analyze <- data_long %>%
    dplyr::filter(
      SampleType == "Sample",
      !is.na(.data[[response_var]]),
      is.finite(.data[[response_var]])
    )

  if (nrow(samples_to_analyze) == 0) {
    # Empty-result branch: return the same empty data frames the original
    # chunk's else-block created plus an empty outlier_flags so downstream
    # `nrow(outlier_flags) > 0` checks don't crash on NULL. The optional
    # fields (outlier_method_log, tissue_weights, processing_config) are
    # *omitted* from the list so list2env() doesn't shadow the chunk env's
    # original `exists()` semantics for them.
    return(list(
      samples_to_analyze   = samples_to_analyze,
      sample_results       = data.frame(),
      sample_results_clean = data.frame(),
      replicate_stats      = data.frame(),
      replicate_summary    = data.frame(),
      outlier_flags        = data.frame(Well = character(),
                                        Replicate = character(),
                                        is_outlier = logical(),
                                        method = character(),
                                        stringsAsFactors = FALSE)
    ))
  }

  # Per-well delta-method prediction
  sample_predictions_list <- lapply(seq_len(nrow(samples_to_analyze)), function(i) {
    response_value <- samples_to_analyze[[response_var]][i]
    dilution       <- samples_to_analyze$DilutionFactor[i]

    tryCatch({
      pred_result <- ED(model_fit, respLev = response_value, type = "absolute",
                        interval = "delta", level = 0.95, display = FALSE)

      estimate_diluted <- as.numeric(pred_result[1, 1])
      ci_lower_diluted <- as.numeric(pred_result[1, 3])
      ci_upper_diluted <- as.numeric(pred_result[1, 4])

      data.frame(
        Well                    = samples_to_analyze$Well[i],
        SampleID                = samples_to_analyze$SampleID[i],
        SampleType              = samples_to_analyze$SampleType[i],
        Replicate               = samples_to_analyze$Replicate[i],
        DilutionFactor          = dilution,
        response_value          = response_value,
        concentration_diluted   = estimate_diluted,
        estimated_concentration = estimate_diluted / dilution,
        ci_lower_individual     = ci_lower_diluted / dilution,
        ci_upper_individual     = ci_upper_diluted / dilution,
        stringsAsFactors        = FALSE
      )
    }, error = function(e) {
      data.frame(
        Well                    = samples_to_analyze$Well[i],
        SampleID                = samples_to_analyze$SampleID[i],
        SampleType              = samples_to_analyze$SampleType[i],
        Replicate               = samples_to_analyze$Replicate[i],
        DilutionFactor          = dilution,
        response_value          = response_value,
        concentration_diluted   = NA_real_,
        estimated_concentration = NA_real_,
        ci_lower_individual     = NA_real_,
        ci_upper_individual     = NA_real_,
        stringsAsFactors        = FALSE
      )
    })
  })

  sample_results <- do.call(rbind, sample_predictions_list)

  # M6: interpolation vs extrapolation labels
  tryCatch({
    min_std_conc <- min(standards_for_model$concentration, na.rm = TRUE)
    max_std_conc <- max(standards_for_model$concentration, na.rm = TRUE)
    sample_results$quantification_status <- dplyr::case_when(
      is.na(sample_results$estimated_concentration) ~ "Not estimable",
      sample_results$estimated_concentration < min_std_conc ~ "Extrapolated (below)",
      sample_results$estimated_concentration > max_std_conc ~ "Extrapolated (above)",
      TRUE ~ "Interpolated"
    )
  }, error = function(e) {
    # <<- targets the enclosing function's local `sample_results` (defined
    # above the tryCatch), not the global environment.
    sample_results$quantification_status <<- "Unknown"
  })

  # Optional outlier detection (Shapiro pre-test -> MAD/Dixon's Q/Grubbs)
  outlier_flags <- data.frame(Well = character(), Replicate = character(),
                              is_outlier = logical(), method = character(),
                              stringsAsFactors = FALSE)
  outlier_method_log <- NULL

  if (isTRUE(analysis_config$enable_outlier_detection)) {
    outlier_min  <- analysis_config$outlier_min_n %||% 3
    use_shapiro  <- isTRUE(analysis_config$normality_assumption == "test_shapiro")
    outlier_method_log <- data.frame(Replicate = character(),
                                     method_used = character(),
                                     shapiro_p = numeric(),
                                     stringsAsFactors = FALSE)

    outlier_groups <- sample_results %>%
      dplyr::filter(!is.na(estimated_concentration)) %>%
      dplyr::group_by(Replicate) %>%
      dplyr::filter(dplyr::n() >= outlier_min) %>%
      dplyr::ungroup()

    if (nrow(outlier_groups) > 0) {
      for (rep_grp in unique(outlier_groups$Replicate)) {
        grp_data <- outlier_groups %>% dplyr::filter(Replicate == rep_grp)
        n_grp <- nrow(grp_data)
        vals  <- grp_data$estimated_concentration

        use_mad  <- FALSE
        shapiro_p <- NA_real_
        if (use_shapiro && n_grp >= 3) {
          shap_test <- tryCatch(shapiro.test(vals),
                                error = function(e) list(p.value = 1))
          shapiro_p <- shap_test$p.value
          if (shapiro_p < STATS_CONFIG$shapiro_alpha) {
            use_mad <- TRUE
          }
        }

        if (use_mad) {
          med_val <- median(vals)
          mad_val <- mad(vals, constant = 1.4826)
          if (mad_val > 0) {
            mad_scores  <- abs(vals - med_val) / mad_val
            outlier_idx <- which(mad_scores > STATS_CONFIG$mad_outlier_threshold)
            if (length(outlier_idx) > 0) {
              for (oi in outlier_idx) {
                outlier_flags <- rbind(outlier_flags, data.frame(
                  Well = grp_data$Well[oi], Replicate = rep_grp,
                  is_outlier = TRUE, method = "MAD",
                  stringsAsFactors = FALSE))
              }
            }
          }
          if (use_shapiro) {
            outlier_method_log <- rbind(outlier_method_log, data.frame(
              Replicate = rep_grp, method_used = "MAD",
              shapiro_p = shapiro_p, stringsAsFactors = FALSE))
          }
        } else if (n_grp >= 3 && n_grp <= 5) {
          sorted_vals <- sort(vals)
          range_val   <- max(sorted_vals) - min(sorted_vals)
          if (range_val > 0) {
            q_low  <- (sorted_vals[2] - sorted_vals[1]) / range_val
            q_high <- (sorted_vals[n_grp] - sorted_vals[n_grp - 1]) / range_val
            q_crit <- c(`3` = 0.970, `4` = 0.829, `5` = 0.710)
            crit   <- q_crit[as.character(n_grp)]
            if (q_low > crit) {
              outlier_flags <- rbind(outlier_flags, data.frame(
                Well = grp_data$Well[which.min(vals)], Replicate = rep_grp,
                is_outlier = TRUE, method = "Dixon Q",
                stringsAsFactors = FALSE))
            }
            if (q_high > crit) {
              outlier_flags <- rbind(outlier_flags, data.frame(
                Well = grp_data$Well[which.max(vals)], Replicate = rep_grp,
                is_outlier = TRUE, method = "Dixon Q",
                stringsAsFactors = FALSE))
            }
          }
          if (use_shapiro) {
            outlier_method_log <- rbind(outlier_method_log, data.frame(
              Replicate = rep_grp, method_used = "Dixon Q",
              shapiro_p = shapiro_p, stringsAsFactors = FALSE))
          }
        } else if (n_grp >= 6) {
          mean_val <- mean(vals)
          sd_val   <- sd(vals)
          if (sd_val > 0) {
            g_stats <- abs(vals - mean_val) / sd_val
            t_crit  <- qt(1 - 0.05 / (2 * n_grp), n_grp - 2)
            g_crit  <- ((n_grp - 1) / sqrt(n_grp)) *
              sqrt(t_crit^2 / (n_grp - 2 + t_crit^2))
            outlier_idx <- which(g_stats > g_crit)
            if (length(outlier_idx) > 0) {
              for (oi in outlier_idx) {
                outlier_flags <- rbind(outlier_flags, data.frame(
                  Well = grp_data$Well[oi], Replicate = rep_grp,
                  is_outlier = TRUE, method = "Grubbs",
                  stringsAsFactors = FALSE))
              }
            }
          }
          if (use_shapiro) {
            outlier_method_log <- rbind(outlier_method_log, data.frame(
              Replicate = rep_grp, method_used = "Grubbs",
              shapiro_p = shapiro_p, stringsAsFactors = FALSE))
          }
        }
      }
    }
  }

  if (nrow(outlier_flags) > 0) {
    sample_results$is_outlier <- sample_results$Well %in% outlier_flags$Well
  } else {
    sample_results$is_outlier <- FALSE
  }

  # Replicate-group statistics (excluding flagged outliers)
  stats_data <- sample_results %>%
    dplyr::filter(!is.na(estimated_concentration) & !is_outlier)

  # Layered uncertainty per replicate group (Task A: HP5)
  replicate_groups <- unique(stats_data$Replicate)
  layered_ci_list <- lapply(replicate_groups, function(rep_grp) {
    tryCatch({
      grp_data <- stats_data %>% dplyr::filter(Replicate == rep_grp)
      well_preds <- data.frame(
        well            = grp_data$Well,
        predicted_conc  = grp_data$estimated_concentration,
        ci_lower_model  = grp_data$ci_lower_individual,
        ci_upper_model  = grp_data$ci_upper_individual,
        stringsAsFactors = FALSE
      )
      ci_method <- analysis_config$ci_method %||% "t_dist"
      lu <- compute_layered_uncertainty(well_preds,
                                        grp_data$estimated_concentration,
                                        ci_method = ci_method)
      data.frame(
        Replicate           = rep_grp,
        ci_lower_model      = lu$ci_lower_model,
        ci_upper_model      = lu$ci_upper_model,
        ci_lower_replicate  = lu$ci_lower_replicate,
        ci_upper_replicate  = lu$ci_upper_replicate,
        ci_lower_combined   = lu$ci_lower_combined,
        ci_upper_combined   = lu$ci_upper_combined,
        stringsAsFactors    = FALSE
      )
    }, error = function(e) {
      data.frame(
        Replicate           = rep_grp,
        ci_lower_model      = NA_real_, ci_upper_model      = NA_real_,
        ci_lower_replicate  = NA_real_, ci_upper_replicate  = NA_real_,
        ci_lower_combined   = NA_real_, ci_upper_combined   = NA_real_,
        stringsAsFactors    = FALSE
      )
    })
  })
  layered_ci_df <- do.call(rbind, layered_ci_list)

  replicate_stats <- stats_data %>%
    dplyr::group_by(Replicate) %>%
    dplyr::summarise(
      sampleID     = paste(unique(SampleID), collapse = ","),
      sample_type  = dplyr::first(SampleType),
      n_replicates = dplyr::n(),
      mean_conc    = mean(estimated_concentration, na.rm = TRUE),
      sd_conc      = sd(estimated_concentration,   na.rm = TRUE),
      se_conc      = sd_conc / sqrt(n_replicates),
      cv_percent   = (sd_conc / mean_conc) * 100,
      .groups      = "drop"
    ) %>%
    dplyr::left_join(layered_ci_df, by = "Replicate") %>%
    dplyr::mutate(
      # HP6: preserve raw CI before truncation
      ci_lower_raw   = ci_lower_combined,
      ci_upper_raw   = ci_upper_combined,
      ci_lower       = pmax(ci_lower_combined,
                            STATS_CONFIG$ci_truncation_floor),
      ci_upper       = ci_upper_combined,
      ci_truncated   = !is.na(ci_lower_raw) &
                       ci_lower_raw < STATS_CONFIG$ci_truncation_floor,
      mean_conc_formatted = if (isTRUE(is_elisa)) {
        format(mean_conc, digits = 4, nsmall = 1, big.mark = ",")
      } else {
        format(mean_conc, scientific = TRUE, digits = 3)
      },
      ci_formatted = if (isTRUE(is_elisa)) {
        paste0("[", format(ci_lower, digits = 3, nsmall = 1), " - ",
               format(ci_upper, digits = 3, nsmall = 1), "]")
      } else {
        paste0("[", format(ci_lower, scientific = TRUE, digits = 2), " - ",
               format(ci_upper, scientific = TRUE, digits = 2), "]")
      },
      cv_formatted = format(cv_percent, digits = 3, nsmall = 1)
    )

  # Range / LLOQ-ULOQ classification per replicate group
  replicate_range <- sample_results %>%
    dplyr::filter(!is.na(estimated_concentration)) %>%
    dplyr::group_by(Replicate) %>%
    dplyr::summarise(
      mean_response      = mean(response_value, na.rm = TRUE),
      mean_conc_for_flag = mean(estimated_concentration, na.rm = TRUE),
      quant_status = if ("quantification_status" %in% names(sample_results)) {
        names(sort(table(quantification_status), decreasing = TRUE))[1]
      } else {
        "Unknown"
      },
      .groups = "drop"
    ) %>%
    dplyr::mutate(
      within_range = mapply(classify_range, mean_response, mean_conc_for_flag,
                            MoreArgs = list(is_elisa_assay = is_elisa)),
      range_flag = mapply(flag_range, mean_response, mean_conc_for_flag,
                          MoreArgs = list(is_elisa_assay = is_elisa))
    )

  replicate_stats <- replicate_stats %>%
    dplyr::left_join(
      replicate_range %>% dplyr::select(Replicate, mean_response,
                                        within_range, range_flag, quant_status),
      by = "Replicate"
    )

  # Tissue-weight-based concentration (ELISA + sidecar JSON only)
  processing_config <- NULL
  tissue_weights    <- NULL
  if (isTRUE(is_elisa)) {
    processing_config <- tryCatch({
      load_sample_processing_config(output_dir)
    }, error = function(e) {
      list(extraction_volume_ul = 500)
    })

    tissue_weights_file <- file.path(output_dir, "tissue_weights.json")
    if (file.exists(tissue_weights_file)) {
      tissue_weights <- tryCatch({
        jsonlite::fromJSON(tissue_weights_file, simplifyVector = FALSE)
      }, error = function(e) NULL)
    }

    if (!is.null(tissue_weights) && length(tissue_weights) > 0) {
      tw_df <- data.frame(Replicate = names(tissue_weights),
                          stringsAsFactors = FALSE)
      tw_df$TissueWeight_mg <- sapply(tissue_weights, function(x) {
        if (is.list(x)) as.numeric(x$weight) else as.numeric(x)
      })
      tw_df$Extraction_uL <- sapply(tissue_weights, function(x) {
        if (is.list(x) && !is.null(x$extraction_uL)) as.numeric(x$extraction_uL)
        else processing_config$extraction_volume_ul %||% 500
      })
      tw_df <- tw_df %>%
        dplyr::filter(!is.na(TissueWeight_mg) & TissueWeight_mg > 0)

      if (nrow(tw_df) > 0) {
        replicate_stats <- replicate_stats %>%
          dplyr::left_join(tw_df, by = "Replicate") %>%
          dplyr::mutate(
            effective_ext_uL = ifelse(!is.na(Extraction_uL), Extraction_uL,
                                      processing_config$extraction_volume_ul %||% 500),
            total_amount_pg = mean_conc * (effective_ext_uL / 1000),
            concentration_pg_per_g = ifelse(
              !is.na(TissueWeight_mg) & TissueWeight_mg > 0,
              total_amount_pg / (TissueWeight_mg / 1000),
              NA_real_
            )
          )

        sample_results <- sample_results %>%
          dplyr::left_join(tw_df, by = "Replicate") %>%
          dplyr::mutate(
            effective_ext_uL = ifelse(!is.na(Extraction_uL), Extraction_uL,
                                      processing_config$extraction_volume_ul %||% 500),
            total_amount_pg = estimated_concentration * (effective_ext_uL / 1000),
            concentration_pg_per_g = ifelse(
              !is.na(TissueWeight_mg) & TissueWeight_mg > 0,
              total_amount_pg / (TissueWeight_mg / 1000),
              NA_real_
            )
          )
      }
    }
  }

  sample_results_clean <- sample_results %>%
    dplyr::rename(concentration = estimated_concentration)

  replicate_summary <- replicate_stats %>%
    dplyr::select(
      Replicate, sampleID, sample_type, n_replicates,
      mean_conc, sd_conc, se_conc, ci_lower, ci_upper,
      ci_lower_model, ci_upper_model, ci_lower_replicate, ci_upper_replicate,
      ci_lower_raw, ci_upper_raw, ci_truncated, cv_percent,
      dplyr::any_of(c("mean_response", "within_range", "range_flag",
                      "quant_status", "TissueWeight_mg",
                      "concentration_pg_per_g"))
    ) %>%
    dplyr::rename(
      replicate_group    = Replicate,
      sample_ids         = sampleID,
      mean_concentration = mean_conc,
      std_dev            = sd_conc,
      std_error          = se_conc,
      ci_lower_95        = ci_lower,
      ci_upper_95        = ci_upper,
      cv_percent         = cv_percent
    )

  # Conditional injection: outlier_method_log only exists in the chunk env
  # when outlier detection actually ran; tissue_weights / processing_config
  # only exist when ELISA + sidecar JSON loaded. Omitting NULLs from the
  # result list preserves the original chunk's `exists()` semantics so
  # later `if (exists("outlier_method_log") && nrow(outlier_method_log) > 0)`
  # checks behave identically.
  result <- list(
    samples_to_analyze   = samples_to_analyze,
    sample_results       = sample_results,
    sample_results_clean = sample_results_clean,
    replicate_stats      = replicate_stats,
    replicate_summary    = replicate_summary,
    outlier_flags        = outlier_flags
  )
  if (!is.null(outlier_method_log)) result$outlier_method_log <- outlier_method_log
  if (!is.null(tissue_weights))     result$tissue_weights     <- tissue_weights
  if (!is.null(processing_config))  result$processing_config  <- processing_config
  result
}

# ==============================================================================
# Internationalization (i18n) Module
# Purpose: Bilingual translation keys (English/Spanish) for both the Shiny UI
#          and the rendered Rmd reports.
#
# Contains 480+ translation keys covering:
#   - App UI labels (buttons, inputs, tabs, guided tour)
#   - Report sections (titles, captions, QC messages, statistical notes)
#   - Analysis-specific terms (DRC, LLOQ/ULOQ, outlier, concordance)
#
# Usage:
#   tr("key_name", lang)           -> translated string
#   tr("key_with_%s", lang, value) -> formatted string via sprintf
# ==============================================================================

#' Get all UI and report translations
#' @return Named list with "en" and "es" sub-lists
get_translations <- function() {
  list(
    en = list(
      # App title and metadata
      app_title = "RBA Analysis \u2013 Microplate Processing & Curve Fitting",
      start_tour = "\U0001F680 Start Guided Tour",
      
      # Step 0: Assay Configuration
      step0_title = "Step 1: Assay Configuration",
      select_assay_type = "Select Assay Type",
      assay_type_label = "Type of assay:",
      assay_rba = "Receptor Binding Assay (RBA)",
      assay_elisa = "ELISA (Enzyme-Linked Immunosorbent Assay)",
      rba_description = "Receptor binding assays measure displacement of radioligand or fluorescent ligand.",
      elisa_description = "ELISA measures analyte concentration via antibody-enzyme reactions.",
      toxin_standard = "Toxin standard used:",
      analyte_label = "Analyte:",
      custom_name = "Custom analyte name:",
      units_label = "Standard concentration units:",
      std_concentrations = "Standard Concentrations",
      std_concentrations_desc = "Specify the number of standards, then enter each concentration.",
      num_standards = "Number of standards:",
      elisa_std_guidance = "Enter concentrations in %s (e.g., 4000, 1600, 640...)",
      rba_std_guidance = "Enter concentrations in mol/L using scientific notation (e.g., 1e-6, 3e-8...)",
      
      # ELISA control wells help
      elisa_controls_title = "ELISA Control Wells:",
      blank_desc = "No enzyme, no antibody (background absorbance)",
      nsb_desc = "Non-specific binding (enzyme only, no antibody)",
      b0_desc = "Maximum binding (no competing analyte)",
      ta_desc = "Total enzyme activity (optional)",
      elisa_tip = "\U0001F4A1 Tip: Typically assign Blank/NSB/B0 in Column 1, rows A-G. Standards go in columns 2-3.",

      # Quick Start panel (Tab 1)
      quickstart_title = "Quick Start",
      quickstart_desc = "Choose a preset to auto-configure the assay type, plate layout, and standard concentrations:",
      quickstart_or_manual = "Or configure manually below.",
      preset_rba_stx_btn = "RBA Saxitoxin",
      preset_elisa_cortisol_btn = "ELISA Cortisol",
      preset_elisa_custom_btn = "ELISA Custom",

      # Custom standard/analyte names (Tab 1)
      custom_standard_name_label = "Custom standard name:",
      custom_standard_name_placeholder = "e.g., GTX2/3 mix",
      custom_analyte_placeholder = "e.g., Estradiol",
      custom_choice_label = "Custom",

      # Toxin variant and molecular weight (Tab 1)
      toxin_variant_label = "Variant:",
      molecular_weight_label = "Molecular weight [g/mol] for %s:",
      molecular_weight_readonly = "Molecular weight [g/mol]:",

      # Step 1: Matrices
      step1_title = "Step 2: Plate Layout & QC Parameters",
      type_matrix = "1. Sample Type (Standard, Sample, QC, Blank, Other)",
      id_matrix = "2. Sample ID",
      qc_params = "3. Quality Control Parameters",
      dilution_matrix = "4. Dilution Factors (numeric or ratio like 1:2)",
      dilution_matrix_label = "3. Dilution Fraction (sample strength remaining: 1 = undiluted, 0.5 = diluted 1:2)",
      dilution_set_all_label = "Set all wells to fraction:",
      dilution_matrix_help = "Enter the fraction of original sample strength remaining after dilution. Examples: undiluted \u2192 1; diluted 1:2 \u2192 0.5 (or type \"1:2\" directly); diluted 1:10 \u2192 0.1. Values greater than 1 are interpreted as pre-concentration (e.g., a sample concentrated 2\u00d7 from its original form \u2192 2). When unsure, use ratio notation \"1:N\" \u2014 the app converts automatically.",
      dilution_gt1_warning = "\u26a0\ufe0f One or more wells have dilution fraction > 1. This indicates pre-concentration and will reduce reported concentrations. If you meant a 2-fold dilution, enter \"1:2\" instead of \"2\".",
      replicate_matrix = "5. Replicate Groups",
      tissue_weight_title = "6. Tissue Weights (ELISA only)",
      tissue_weight_desc = "Enter tissue weight (mg) per replicate group for pg/g tissue calculation.",
      extraction_vol_label = "Extraction volume (\u00B5L):",
      extraction_volume_help = "Extraction volume = total volume the tissue was extracted into, before any plate-loading dilutions. Example: 50 mg tissue homogenized in 500 \u00B5L buffer, then diluted 1:10 before plate loading \u2192 enter 500 here, and record the 1:10 in the DilutionFactor matrix.",
      extraction_volume_report_note = "**Note on extraction volume:** $V_{extraction}$ is the *total* volume the tissue was extracted into, before any plate-loading dilutions. For example, 50 mg of tissue homogenized in 500 \u00B5L of buffer, then diluted 1:10 before plate loading, is recorded as $V_{extraction}$ = 500 \u00B5L here and the 1:10 dilution is recorded in the DilutionFactor matrix (as 0.1 or `1:10`).",
      reset_default = "Reset to Default",
      
      # Step 2: Upload
      step2_title = "Step 3: Upload Plate Data",
      upload_label = "Upload Bioassay Results",
      upload_or_visual = "Import method:",
      import_classic = "Classic Import",
      import_visual = "Visual Plate Selector",
      clear_file = "Remove file",
      show_layout = "\U0001F4CA Show default plate layout",
      import_summary = "Import Summary:",
      format_label = "Format:",
      wells_label = "Wells:",
      partial_label = "Partial:",
      
      # Visual plate selector
      visual_selector_title = "Visual Plate Selector",
      visual_instructions = "The uploaded file is displayed below. Click and drag to select an 8\u00D712 plate region. You can select multiple plates.",
      plate_n = "Plate %d",
      remove_plate = "Remove",
      confirm_selection = "Confirm Selection",
      cancel_selection = "Cancel",
      excluded_wells_label = "Click individual cells to exclude them (they will be marked as NA).",
      wells_excluded = "%d well(s) excluded",

      # Tab 2: Plate Layout static text, preset dropdown, matrix headings
      preset_layout_label = "Load Preset Layout:",
      preset_select_placeholder = "-- Select Preset --",
      preset_rba_stx_tri = "RBA: STX 8 standards (triplicate)",
      preset_elisa_cortisol_cayman = "ELISA: Cortisol (Cayman kit, 8-point, duplicate)",
      preset_elisa_custom_blank = "ELISA: Custom (blank template)",
      layout_import_label = "Import Layout (CSV/Excel):",
      save_layout_short = "Save Layout",
      undo_btn = "Undo",
      redo_btn = "Redo",
      apply_btn = "Apply",
      reset_btn = "Reset",
      per_well_label = "Per-well",
      sample_type_label = "1. Sample Type",
      replicate_label = "4. Replicate Groups",
      qc_params_label = "5. Quality Control Parameters",
      tissue_weight_label = "6. Tissue Weights & Extraction Volume (optional)",
      elisa_controls_banner_body = "Blank | NSB | B0 | TotalActivity (col 1)",
      tissue_banner_prefix = "Tissue-based calculation: ",
      tissue_banner_body = "Enter tissue weight (mg) and extraction volume (\u00b5L) per replicate group. Leave blank if not applicable. Default extraction volume: 500 \u00b5L.",
      set_all_extraction_label = "Set all extraction vol to:",
      scroll_right_hint = "Scroll right if more groups are present.",

      # Notes and feedback
      notes_label = "Notes (optional):",
      notes_placeholder = "Observations...",
      give_feedback = "Give Feedback",
      
      # Step 3: Report generation
      report_formats = "Report formats:",
      report_language = "Report language:",
      generate_report = "Step 3: Generate Report",
      generating_report = "Generating report...",

      # Tab 5: Report Output & Notes panels
      report_output_heading = "Report Output",
      notes_feedback_heading = "Notes & Feedback",
      preflight_heading = "Pre-Flight Check",
      format_html = "HTML",
      format_docx = "Word (DOCX)",
      format_pdf = "PDF",
      report_formats_help = "HTML reports have interactive plots. Word and PDF use static figures.",
      report_formats_pdf_note = "PDF requires a LaTeX engine (e.g. TinyTeX). If unavailable, the app will fall back to HTML.",
      notes_full_label = "Notes (optional) - will appear in the report:",
      notes_report_placeholder = "Observations, sample info, run notes...",
      download_last_report = "Download Last Report",

      # Pre-flight check panel (Tab 5 preflight_checks renderUI)
      pf_plate_ok = "Plate data uploaded",
      pf_plate_missing = "No plate data - go to Upload tab",
      pf_std_count_ok = "%d standard wells defined",
      pf_std_count_low = "Only %d standard wells (need >= 4)",
      pf_elisa_controls_ok = "ELISA controls present (Blank, NSB, B0)",
      pf_elisa_controls_missing = "Missing ELISA controls: %s",
      pf_dilution_ok = "Dilution factors valid",
      pf_dilution_invalid = "Some dilution entries are invalid",
      pf_std_count_match = "Standard count matches: %d groups for %d standards",
      pf_std_count_mismatch = "Standard groups (%d) differs from configured standards (%d)",
      pf_rep_groups_ok = "Replicate groups are consistent",
      pf_rep_groups_mixed = "Mixed well types in replicate group(s): %s",
      pf_ids_ok = "All sample wells have IDs",
      pf_ids_empty = "%d sample well(s) have empty IDs",
      pf_elisa_reps_ok = "ELISA controls have adequate replicates",
      pf_elisa_reps_low = "Low replicates for ELISA control(s): %s (recommend >= 2)",
      pf_tissue_missing = "No tissue weights entered (needed for tissue-normalized ELISA results)",
      pf_badge_errors = "Blocking issues found — resolve before generating report",
      pf_badge_warnings = "Warnings found — report can be generated but review recommended",
      pf_badge_ok = "All checks passed — ready to generate report",

      # QC
      qc_conc_label = "QC concentration (%s):",
      expected_hill = "Expected Hill slope:",
      qc_required = "\u26A0\uFE0F QC concentration required",
      sci_notation = "\u26A0\uFE0F Use scientific notation (e.g., 3e-9)",
      must_be_numeric = "\u26A0\uFE0F Must be a numeric value",
      outside_rba_range = "\u26A0\uFE0F Outside typical RBA range (1e-12 to 1e-6 mol/L)",
      outside_elisa_range = "\u26A0\uFE0F Outside typical ELISA range (0.1-10000)",
      hill_required = "\u26A0\uFE0F Hill slope required",
      hill_outside_range = "\u26A0\uFE0F Outside expected range (0.5\u20131.5)",
      invalid_dilution = "\u26A0\uFE0F Invalid dilution entries (red cells)",
      
      # Notifications
      multiwave_detected = "\u2705 Multi-wavelength file detected: %s\n%d wells detected per plate\nFormat: %s%s",
      import_success = "\u2705 Imported: %s\n%d wells detected\nFormat: %s%s",
      import_failed = "Import failed: %s",
      file_cleared = "File cleared",
      data_saved = "Data saved to: %s",
      saved_wavelengths = "Saved data for %d wavelengths",
      
      # Plate layout modal
      layout_title = "Sample Plate Layout",
      layout_desc = "Expected: Row labels (A\u2013H) + 12 numeric columns.<br>Do not include column names.",
      
      # Language
      language_label = "Language / Idioma",
      
      # Guided tour
      tour_next = "Next",
      tour_prev = "Back",
      tour_skip = "Exit",
      tour_done = "Finish",

      # Guided tour \u2014 5-tab wizard walkthrough
      tour_language_toggle = "Language switch. Use this dropdown at any time to toggle the entire interface (and the report you generate) between English and Spanish.",
      tour_quickstart = "Quick Start. One click here loads a preset (RBA Saxitoxin, ELISA Cortisol, or ELISA Custom) \u2014 it fills in the assay type, standard concentrations, and plate layout so you can jump straight to uploading your data.",
      tour_config = "Tab 1 \u2013 Configuration. Pick RBA or ELISA, choose the analyte, set the number of standards, and enter standard concentrations. RBA also lets you set an expected Hill slope and QC concentration.",
      tour_preset_layout = "Plate layout presets and import/export. Load a saved layout, import one from CSV/Excel, or export your current layout to share with collaborators.",
      tour_matrix_type = "Sample Type matrix (1). Assign each well as Standard, Sample, QC, Blank, NSB, B0, or TotalActivity. For ELISA, the control wells (Blank / NSB / B0) are typically placed in column 1.",
      tour_matrix_id = "Sample ID matrix (2). Enter a short label per well. Standards are auto-named S1, S2, \u2026 matching the standard concentrations on the Configuration tab.",
      tour_matrix_dilution = "Dilution Fraction matrix (3). Each cell is the fraction of the original sample strength remaining after dilution: undiluted = 1, diluted 1:2 = 0.5, diluted 1:10 = 0.1. You can also type ratio notation like \"1:2\" \u2014 the app converts automatically.",
      tour_matrix_replicate = "Replicate Groups matrix (4). Wells that share a label are treated as replicates of the same sample for statistics. Use different labels for different samples.",
      tour_qc_rba = "RBA Quality Control parameters. Set the expected Hill slope and the QC concentration used to monitor assay performance.",
      tour_tissue_weights = "Tissue weights & extraction volume (ELISA only). Enter tissue mass (mg) per replicate group and the extraction volume (\u00b5L) so the report can calculate final concentrations in pg/g tissue.",
      tour_upload = "Tab 3 \u2013 Upload & Preview. Drop in your plate reader file (.xlsx, .csv, or .txt). Classic Import auto-detects the plate region; switch to Visual Plate Selector to pick the region by drag.",
      tour_heatmap_preview = "Heatmap preview. After upload, this heatmap shows your measurement values so you can visually confirm the correct plate region was detected before running the analysis.",
      tour_analysis = "Tab 4 \u2013 Analysis Settings. Choose DRC regression weightings (unweighted, 1/Y, 1/Y\u00b2), set the quantifiable %B/B0 range (LLOQ/ULOQ), pick the confidence-interval method, and configure outlier detection.",
      tour_preflight = "Pre-flight checks. A quick traffic-light summary of what is and isn't ready: plate data, standards, ELISA controls, QC parameters. Fix anything red before generating a report.",
      tour_convert = "Tab 5 \u2013 Generate Report. Pick one or more output formats (HTML, DOCX, PDF) and a report language, then click Generate Report. PDF falls back to HTML automatically if no LaTeX engine is detected.",
      tour_notes = "Notes & Feedback. Anything you type here is embedded in the generated report for documentation. Use the Feedback button to reach the developers.",
      
      # ---- REPORT TRANSLATIONS ----
      report_title = "Bioassay Analysis Report",
      report_title_multi = "Multi-Wavelength Bioassay Analysis Report",
      report_elisa = "ELISA",
      report_rba = "RBA",
      analysis_report = "Analysis Report",
      assay_type = "Assay Type:",
      analysis_date = "Analysis Date:",
      analyst = "Analyst:",
      elisa_intro = "This report analyzes ELISA data using a four-parameter logistic dose\u2013response curve to estimate concentrations of %s in unknown samples.",
      elisa_method = "Method: Competitive enzyme-linked immunosorbent assay with %B/B0 normalization",
      rba_intro = "This report analyzes Receptor Binding Assay (RBA) data using a four-parameter logistic dose\u2013response curve to estimate concentrations of %s in unknown samples.",
      rba_method = "Method: Competitive receptor binding assay with %s detection",
      analysis_notes = "Analysis Notes",
      no_notes = "No additional notes provided.",
      std_curve_config = "Standard Curve Configuration",
      std_concentrations_table = "Standard Concentrations",
      drc_analysis = "Dose\u2013Response Curve Analysis",
      all_std_acceptable = "\u2705 All standards show acceptable variability (<30%% CV).",
      high_var_standards = "Standards with high variability (>30%% CV):",
      model_parameters = "Model Parameters",
      four_pl_coefficients = "Four-Parameter Logistic Model Coefficients",
      hill_slope = "Hill Slope",
      bottom = "Bottom",
      top = "Top",
      ic50 = "IC50",
      model_fit_stats = "Model Fit Statistics:",
      standards_used = "Standards Used",
      std_backcalc_title = "Standard Back-Calculation and Recovery",
      std_backcalc_caption = "Standard Back-Calculation and Recovery",
      overall_recovery = "Overall Mean Recovery: %.1f%%",
      recovery_acceptable = "\u2705 Overall recovery is within acceptable range (80\u2013120%%).",
      recovery_outside = "\u26A0\uFE0F Overall recovery is outside the typical acceptable range (80\u2013120%%). Review curve fit.",
      sample_results = "Sample Concentration Results",
      sample_results_caption = "Sample Quantification Results - %s",
      with_tissue = "(with tissue normalization)",
      output_files_created = "Output Files Created:",
      individual_results = "`unknown_results.csv` - Individual well results",
      summary_results = "`unknown_results_summary.csv` - Replicate group statistics with confidence intervals",
      quality_alert = "\u26A0\uFE0F Quality Alert:",
      high_cv_groups = "Replicate groups with high variability (CV > 30%%): %s",
      check_preparation = "Consider checking sample preparation or dilution consistency",
      quality_pass = "\u2705 Quality Check: All replicate groups show acceptable variability (CV \u2264 30%%)",
      no_samples_quantified = "No samples could be quantified.",
      detailed_summary = "Detailed Sample Results Summary",
      detailed_caption = "Detailed Sample Results with Model-Based Confidence Intervals",
      sample_variability = "Sample Variability Visualization",
      sample_variability_desc = "Box-and-jitter plot showing estimated concentrations per replicate group. Points are colored by whether they fall within the validated linear range. Use this to assess replicate consistency and identify outliers.",
      drc_with_samples = "Dose-Response Curve with Unknown Samples",
      drc_combined_title = "Dose-Response Curve with Standards and Unknown Samples",
      within_range = "Within Range",
      out_of_range = "Out of Range",
      unknown_range = "Unknown",
      flag_above_uloq = ">ULOQ",
      flag_below_lloq = "<LLOQ",
      report_generated = "Report Generated:",
      contact = "Contact:",
      feedback = "Feedback:",
      online_form = "Online Form",
      automated_analysis = "Automated bioassay analysis using modular system v2.0",
      automated_multi = "Automated multi-wavelength bioassay analysis using modular system v2.0",
      
      # Multi-wavelength specific
      multi_overview = "Multi-Wavelength Analysis Overview",
      multi_overview_desc = "This report contains analyses for **%d wavelengths**: %s.",
      multi_compare = "Each wavelength is analyzed independently using the same plate layout but different absorbance readings. This allows you to:",
      multi_benefit1 = "Compare dose-response curve quality across wavelengths",
      multi_benefit2 = "Identify the optimal wavelength for your assay",
      multi_benefit3 = "Verify sample concentrations are consistent across readings",
      multi_sections = "Sections:",
      multi_exec_summary = "Executive Summary (below) - Quick comparison across wavelengths",
      multi_detailed = "Detailed Analysis for each wavelength (following sections)",
      exec_summary_title = "Executive Summary: Wavelength Comparison",
      wavelength_analysis = "Wavelength %s Analysis",
      analysis_n_of = "Analysis %d of %d",
      data_overview = "Data Overview by Wavelength",
      overall_conclusions = "Overall Conclusions",
      wavelength_performance = "Wavelength Performance Summary",
      recommendations = "Recommendations:",
      rec_r2 = "Choose the wavelength with highest R\u00B2 (best curve fit)",
      rec_cv = "Verify sample CVs are acceptable (<20%% preferred)",
      rec_separation = "Consider which wavelength gives the most reliable separation between samples",
      recommended_wavelength = "\U0001F31F **Recommended:** %s (lowest RMSE: %.3f)",
      
      # Table column headers
      col_replicate_group = "Replicate Group",
      col_sample_ids = "Sample IDs",
      col_sample_type = "Sample Type",
      col_n = "n",
      col_mean = "Mean (%s)",
      col_sd = "SD (%s)",
      col_se = "SE (%s)",
      col_ci = "95%% CI",
      col_cv = "CV%%",
      col_range_flag = "Range",
      col_tissue_conc = "Conc. (pg/g tissue)",
      col_tissue_mass = "Tissue Mass",
      col_nominal = "Nominal (%s)",
      col_backcalc = "Back-Calc. (%s)",
      col_recovery = "Recovery (%%)",
      col_parameter = "Parameter",
      col_estimate = "Estimate",
      col_std_error = "Std. Error",
      col_pvalue = "p-value",
      col_wavelength = "Wavelength",
      col_standards = "Standards",
      col_samples = "Samples",
      col_quantified = "Quantified",
      col_r2 = "R\u00B2",
      col_rmse = "RMSE",
      col_ic50 = "IC50",
      col_mean_cv = "Mean CV%%",
      
      # Analysis Settings (app UI)
      analysis_settings_title = "Analysis Settings",
      regression_weight_label = "DRC regression weighting:",
      quant_range_min_label = "Lower %B/B0 bound:",
      quant_range_max_label = "Upper %B/B0 bound:",
      quant_range_help = "Samples outside this range are flagged as <LLOQ or >ULOQ.",
      ci_method_label = "Confidence interval method:",
      outlier_detection_label = "Enable outlier detection",
      outlier_min_n_label = "Minimum replicates for outlier test:",
      outlier_help = "Dixon's Q-test for n=3-5, Grubbs' test for n>=6. Outliers are flagged, not removed.",
      normality_assumption_label = "Normality assumption for outlier detection:",
      normality_assume = "Assume normality (default)",
      normality_test_shapiro = "Test with Shapiro-Wilk",
      normality_shapiro_help = "Shapiro-Wilk test is run on each replicate group. If p < 0.05 (non-normal), MAD-based detection replaces Grubbs' test.",
      outlier_method_mad = "MAD-based (non-normal distribution detected)",
      cv_limit_label = "Maximum CV for standards (%):",
      cv_limit_help = "Standards exceeding this CV% threshold are flagged as high-variability.",
      advanced_options_heading = "Advanced Options \u2014 weighting, CI, outliers, and QC thresholds",
      advanced_options_intro = "These settings control confidence interval method, outlier detection, quantification range, and quality thresholds.",
      regression_weight_help = "Select multiple weightings to compare results side by side.",
      weight_unweighted = "Unweighted",
      weight_inv_y = "1/Y (moderate)",
      weight_inv_y2 = "1/Y\u00B2 (recommended for immunoassays)",
      ci_t_dist = "t-distribution (default)",
      ci_bootstrap_choice = "Bootstrap (1000 resamples)",

      # Tab titles
      tab_analysis_title = "4. Analysis Settings",
      tab_report_title = "5. Generate Report",

      # Statistical warnings and footnotes
      r2_identical_warning = "R-squared could not be computed: all response values are identical (zero total variance). Check assay integrity.",
      ci_asymmetric_footnote = "Lower confidence bounds are constrained to zero, as negative concentrations have no biological meaning. This may result in asymmetric confidence intervals.",

      # Report sections
      summary_title = "Summary",
      interpretation_title = "Interpretation & Recommendations",
      interpretation_pass = "All quality criteria met. Results are suitable for reporting.",
      interpretation_warn = "Quality warnings detected: %s. Review flagged items before use.",
      methods_title = "Methods",
      plate_positional_title = "Plate Positional Quality",
      data_quality_title = "Data Quality Overview",

      # Traffic-light QC card (report)
      qc_card_title = "Quality Control Summary",
      qc_metric = "Metric",
      qc_value = "Value",
      qc_status = "Status",
      qc_r2 = "R-squared",
      qc_hill = "Hill slope",
      qc_max_cv = "Max replicate CV",
      qc_recovery = "Mean standard recovery",
      qc_green = "Pass",
      qc_amber = "Warning",
      qc_red = "Fail",

      # LLOQ/ULOQ determination (report)
      lloq_uloq_title = "Limits of Quantification",
      lloq_label = "LLOQ (Lower Limit of Quantification)",
      uloq_label = "ULOQ (Upper Limit of Quantification)",
      lloq_uloq_desc = "Determined by back-calculated standard accuracy (recovery 80-120%%, CV <20%%).",
      lloq_uloq_none = "Could not determine quantification limits from available standards.",
      backcalc_title = "Standard Back-Calculation",
      col_accuracy = "Accuracy",

      # Outlier detection (report)
      outlier_title = "Outlier Detection",
      outlier_desc = "Statistical outlier testing applied to replicate groups (n >= %d).",
      outlier_none = "No outliers detected.",
      outlier_found = "%d outlier(s) flagged across %d replicate group(s).",
      outlier_method_dixon = "Dixon's Q-test (n=3-5)",
      outlier_method_grubbs = "Grubbs' test (n>=6)",
      outlier_flagged = "Flagged",
      outlier_flagged_not_removed_note = "Flagged outliers remain visible in the per-well detailed results table and in CSV exports, but are excluded from the calculation of replicate-group mean, SD, CV, and confidence intervals. This preserves full raw-data visibility while preventing outlier contamination of summary statistics.",

      # Range indicator explanation (Phase 1.4)
      range_indicators_explanation = paste0(
        "> **Two independent range indicators appear in this report:**\n>\n",
        "> - **Interpolated / Extrapolated** refers to whether the estimated ",
        "concentration falls within the range of fitted standard concentrations ",
        "on this plate. This is a statement about curve coverage.\n>\n",
        "> - **Within range / <LLOQ / >ULOQ** refers to whether the estimate ",
        "falls within the validated linear (quantifiable) range of the ",
        "dose-response curve, defined by EC20/EC80 for RBA or %B/B0 bounds ",
        "(default 20\u201380%) for ELISA. This is a statement about reporting ",
        "quality.\n>\n",
        "> A sample can be interpolated but outside the quantifiable range ",
        "(e.g., the response falls on the flat portion of the curve near the ",
        "top or bottom asymptote), or within the quantifiable range but ",
        "technically extrapolated (if the user provided few standards). Both ",
        "flags should be considered when interpreting results."),

      # Bootstrap CI (report)
      ci_bootstrap_note = "95%% confidence intervals calculated using bootstrap resampling (1000 iterations).",
      ci_tdist_note = "95%% confidence intervals calculated using t-distribution.",
      ci_delta_method_note = "Individual sample concentrations include CIs from inverse prediction (delta method), accounting for curve-fitting uncertainty.",

      # Methods section (report)
      methods_title = "Methods",
      methods_drc_citation = "Dose-response analysis performed using the drc package (Ritz et al., 2015, *PLOS ONE* 10(12):e0146021). ",
      methods_outlier_citation = "Outlier detection: Dixon's Q-test (Dixon, 1950) for n=3-5, Grubbs' test (Grubbs, 1950) for n\u22656. ",

      # Weight comparison (report)
      weight_comparison_title = "DRC Weighting Comparison",
      weight_comparison_desc = "Comparison of dose-response curve fits using different regression weightings. The overlay plot and parameter table help identify which weighting best fits the data.",
      weight_comparison_primary = "Sample concentrations in this report were calculated using the **%s** model (first selected).",

      # Plate heatmap (report)
      heatmap_title = "Plate Heatmap",
      heatmap_desc = "Visual representation of raw measurement values across the plate.",

      # Cross-wavelength concordance (multi-WL report)
      concordance_title = "Cross-Wavelength Concordance",
      concordance_desc = "Comparison of sample concentrations estimated at different wavelengths.",
      concordance_ccc = "Lin's Concordance Correlation Coefficient (CCC)",
      concordance_ccc_value = "CCC = %.4f [95%% CI: %.4f - %.4f]",
      concordance_bland_altman = "Bland-Altman Analysis",
      bland_altman_explanation = "Each plot compares calculated sample concentrations between two wavelengths: the dashed line shows the mean bias (systematic difference), while the red dotted lines mark the limits of agreement (mean +/- 1.96 SD). If all points fall within the limits of agreement and the bias is close to zero, the two wavelengths can be considered interchangeable for quantification purposes.",
      concordance_bias = "Mean bias: %.4f",
      concordance_loa = "Limits of agreement: [%.4f, %.4f]",
      concordance_no_data = "Insufficient paired sample data for concordance analysis.",
      concordance_excellent = "Excellent agreement (CCC > 0.99)",
      concordance_good = "Good agreement (CCC 0.95-0.99)",
      concordance_moderate = "Moderate agreement (CCC 0.90-0.95)",
      concordance_poor = "Poor agreement (CCC < 0.90)",

      # Parallelism / relative potency (report)
      parallelism_title = "Parallelism and Relative Potency",
      parallelism_not_applicable = "Not applicable: %s",
      parallelism_no_model = "Not applicable: No fitted model available for parallelism assessment.",

      # Plate layout import/save (app UI)
      layout_import_title = "Import Plate Layout",
      layout_import_desc = "Upload a CSV or Excel file with plate layout (SampleType, SampleID, Dilution, Replicate matrices).",
      layout_import_btn = "Import Layout",
      layout_save_btn = "Save Current Layout",
      layout_load_btn = "Load Saved Layout",
      layout_saved_msg = "Layout saved successfully.",
      layout_loaded_msg = "Layout loaded successfully.",
      layout_import_success = "Plate layout imported from file.",
      layout_no_saved = "No saved layouts found.",

      # Omitted wells
      omitted_by_user = "Omitted from analysis by user"
    ),
    
    es = list(
      # App title and metadata
      app_title = "An\u00E1lisis RBA \u2013 Procesamiento de Microplacas y Ajuste de Curvas",
      start_tour = "\U0001F680 Iniciar Gu\u00EDa Interactiva",
      
      # Step 0: Assay Configuration
      step0_title = "Paso 1: Configuraci\u00F3n del Ensayo",
      select_assay_type = "Seleccionar Tipo de Ensayo",
      assay_type_label = "Tipo de ensayo:",
      assay_rba = "Ensayo de Uni\u00F3n a Receptor (RBA)",
      assay_elisa = "ELISA (Ensayo Inmunoenzim\u00E1tico)",
      rba_description = "Los ensayos de uni\u00F3n a receptor miden el desplazamiento del radioligando o ligando fluorescente.",
      elisa_description = "ELISA mide la concentraci\u00F3n del analito mediante reacciones anticuerpo-enzima.",
      toxin_standard = "Est\u00E1ndar de toxina utilizado:",
      analyte_label = "Analito:",
      custom_name = "Nombre personalizado del analito:",
      units_label = "Unidades de concentraci\u00F3n est\u00E1ndar:",
      std_concentrations = "Concentraciones Est\u00E1ndar",
      std_concentrations_desc = "Especifique el n\u00FAmero de est\u00E1ndares e ingrese cada concentraci\u00F3n.",
      num_standards = "N\u00FAmero de est\u00E1ndares:",
      elisa_std_guidance = "Ingrese concentraciones en %s (ej., 4000, 1600, 640...)",
      rba_std_guidance = "Ingrese concentraciones en mol/L usando notaci\u00F3n cient\u00EDfica (ej., 1e-6, 3e-8...)",
      
      # ELISA control wells help
      elisa_controls_title = "Pozos de Control ELISA:",
      blank_desc = "Sin enzima, sin anticuerpo (absorbancia de fondo)",
      nsb_desc = "Uni\u00F3n no espec\u00EDfica (solo enzima, sin anticuerpo)",
      b0_desc = "Uni\u00F3n m\u00E1xima (sin analito competidor)",
      ta_desc = "Actividad enzim\u00E1tica total (opcional)",
      elisa_tip = "\U0001F4A1 Consejo: Normalmente asigne Blanco/NSB/B0 en la Columna 1, filas A-G. Est\u00E1ndares en columnas 2-3.",

      # Panel de Inicio R\u00E1pido (Pesta\u00F1a 1)
      quickstart_title = "Inicio R\u00E1pido",
      quickstart_desc = "Elija un preajuste para configurar autom\u00E1ticamente el tipo de ensayo, el dise\u00F1o de placa y las concentraciones est\u00E1ndar:",
      quickstart_or_manual = "O configure manualmente abajo.",
      preset_rba_stx_btn = "RBA Saxitoxina",
      preset_elisa_cortisol_btn = "ELISA Cortisol",
      preset_elisa_custom_btn = "ELISA Personalizado",

      # Nombres personalizados de est\u00E1ndar/analito (Pesta\u00F1a 1)
      custom_standard_name_label = "Nombre personalizado del est\u00E1ndar:",
      custom_standard_name_placeholder = "ej., mezcla GTX2/3",
      custom_analyte_placeholder = "ej., Estradiol",
      custom_choice_label = "Personalizado",

      # Variante de toxina y peso molecular (Pesta\u00F1a 1)
      toxin_variant_label = "Variante:",
      molecular_weight_label = "Peso molecular [g/mol] para %s:",
      molecular_weight_readonly = "Peso molecular [g/mol]:",
      
      # Step 1: Matrices
      step1_title = "Paso 2: Dise\u00F1o de Placa y Par\u00E1metros QC",
      type_matrix = "1. Tipo de Muestra (Est\u00E1ndar, Muestra, QC, Blanco, Otro)",
      id_matrix = "2. ID de Muestra",
      qc_params = "3. Par\u00E1metros de Control de Calidad",
      dilution_matrix = "4. Factores de Diluci\u00F3n (num\u00E9rico o raz\u00F3n como 1:2)",
      dilution_matrix_label = "3. Fracci\u00F3n de Diluci\u00F3n (concentraci\u00F3n de muestra remanente: 1 = sin diluir, 0.5 = diluido 1:2)",
      dilution_set_all_label = "Aplicar fracci\u00F3n a todos los pozos:",
      dilution_matrix_help = "Ingrese la fracci\u00F3n de concentraci\u00F3n original de muestra remanente despu\u00E9s de la diluci\u00F3n. Ejemplos: sin diluir \u2192 1; diluido 1:2 \u2192 0.5 (o escriba \"1:2\" directamente); diluido 1:10 \u2192 0.1. Valores mayores a 1 se interpretan como pre-concentraci\u00F3n (p. ej., una muestra concentrada 2\u00d7 respecto a su forma original \u2192 2). En caso de duda, use la notaci\u00F3n de raz\u00F3n \"1:N\" \u2014 la aplicaci\u00F3n convierte autom\u00E1ticamente.",
      dilution_gt1_warning = "\u26a0\ufe0f Uno o m\u00E1s pozos tienen una fracci\u00F3n de diluci\u00F3n > 1. Esto indica pre-concentraci\u00F3n y reducir\u00E1 las concentraciones reportadas. Si quiso indicar una diluci\u00F3n 1:2, ingrese \"1:2\" en lugar de \"2\".",
      replicate_matrix = "5. Grupos de R\u00E9plicas",
      tissue_weight_title = "6. Pesos de Tejido (solo ELISA)",
      tissue_weight_desc = "Ingrese el peso del tejido (mg) por grupo de r\u00E9plica para el c\u00E1lculo de pg/g de tejido.",
      extraction_vol_label = "Volumen de extracci\u00F3n (\u00B5L):",
      extraction_volume_help = "Volumen de extracci\u00F3n = volumen total en el que se extrajo el tejido, antes de cualquier diluci\u00F3n en la placa. Ejemplo: 50 mg de tejido homogeneizado en 500 \u00B5L de buffer, luego diluido 1:10 antes de cargar la placa \u2192 ingrese 500 aqu\u00ED y registre la diluci\u00F3n 1:10 en la matriz DilutionFactor.",
      extraction_volume_report_note = "**Nota sobre el volumen de extracci\u00F3n:** $V_{extraction}$ es el volumen *total* en el que se extrajo el tejido, antes de cualquier diluci\u00F3n previa a la carga en la placa. Por ejemplo, 50 mg de tejido homogeneizado en 500 \u00B5L de buffer y luego diluido 1:10 antes de cargar la placa se registra como $V_{extraction}$ = 500 \u00B5L aqu\u00ED, y la diluci\u00F3n 1:10 se registra en la matriz DilutionFactor (como 0.1 o `1:10`).",
      reset_default = "Restablecer Valores",
      
      # Step 2: Upload
      step2_title = "Paso 3: Cargar Datos de Placa",
      upload_label = "Cargar Resultados de Bioensayo",
      upload_or_visual = "M\u00E9todo de importaci\u00F3n:",
      import_classic = "Importaci\u00F3n Cl\u00E1sica",
      import_visual = "Selector Visual de Placa",
      clear_file = "Eliminar archivo",
      show_layout = "\U0001F4CA Mostrar dise\u00F1o de placa predeterminado",
      import_summary = "Resumen de Importaci\u00F3n:",
      format_label = "Formato:",
      wells_label = "Pozos:",
      partial_label = "Parcial:",
      
      # Visual plate selector
      visual_selector_title = "Selector Visual de Placa",
      visual_instructions = "El archivo cargado se muestra a continuaci\u00F3n. Haga clic y arrastre para seleccionar una regi\u00F3n de placa de 8\u00D712. Puede seleccionar m\u00FAltiples placas.",
      plate_n = "Placa %d",
      remove_plate = "Eliminar",
      confirm_selection = "Confirmar Selecci\u00F3n",
      cancel_selection = "Cancelar",
      excluded_wells_label = "Haga clic en celdas individuales para excluirlas (se marcar\u00E1n como NA).",
      wells_excluded = "%d pozo(s) excluido(s)",

      # Pesta\u00F1a 2: Dise\u00F1o de placa, preajustes, encabezados de matriz
      preset_layout_label = "Cargar Dise\u00F1o Predefinido:",
      preset_select_placeholder = "-- Seleccione Preajuste --",
      preset_rba_stx_tri = "RBA: STX 8 est\u00E1ndares (triplicado)",
      preset_elisa_cortisol_cayman = "ELISA: Cortisol (kit Cayman, 8 puntos, duplicado)",
      preset_elisa_custom_blank = "ELISA: Personalizado (plantilla en blanco)",
      layout_import_label = "Importar Dise\u00F1o (CSV/Excel):",
      save_layout_short = "Guardar Dise\u00F1o",
      undo_btn = "Deshacer",
      redo_btn = "Rehacer",
      apply_btn = "Aplicar",
      reset_btn = "Reiniciar",
      per_well_label = "Por pozo",
      sample_type_label = "1. Tipo de Muestra",
      replicate_label = "4. Grupos de R\u00E9plicas",
      qc_params_label = "5. Par\u00E1metros de Control de Calidad",
      tissue_weight_label = "6. Pesos de Tejido y Volumen de Extracci\u00F3n (opcional)",
      elisa_controls_banner_body = "Blanco | NSB | B0 | TotalActivity (col 1)",
      tissue_banner_prefix = "C\u00E1lculo basado en tejido: ",
      tissue_banner_body = "Ingrese el peso del tejido (mg) y el volumen de extracci\u00F3n (\u00b5L) por grupo de r\u00E9plica. Deje en blanco si no aplica. Volumen de extracci\u00F3n por defecto: 500 \u00b5L.",
      set_all_extraction_label = "Aplicar volumen de extracci\u00F3n a todos:",
      scroll_right_hint = "Desplace a la derecha si hay m\u00E1s grupos.",

      # Notes and feedback
      notes_label = "Notas (opcional):",
      notes_placeholder = "Observaciones...",
      give_feedback = "Dar Retroalimentaci\u00F3n",
      
      # Step 3: Report generation
      report_formats = "Formatos de reporte:",
      report_language = "Idioma del reporte:",
      generate_report = "Paso 3: Generar Reporte",
      generating_report = "Generando reporte...",

      # Pestaña 5: Panel de Salida de Reporte y Notas
      report_output_heading = "Salida del Reporte",
      notes_feedback_heading = "Notas y Retroalimentaci\u00F3n",
      preflight_heading = "Verificaci\u00F3n Previa",
      format_html = "HTML",
      format_docx = "Word (DOCX)",
      format_pdf = "PDF",
      report_formats_help = "Los reportes HTML tienen gr\u00E1ficos interactivos. Word y PDF usan figuras est\u00E1ticas.",
      report_formats_pdf_note = "PDF requiere un motor LaTeX (ej., TinyTeX). Si no est\u00E1 disponible, la aplicaci\u00F3n cambiar\u00E1 a HTML.",
      notes_full_label = "Notas (opcional) - aparecer\u00E1n en el reporte:",
      notes_report_placeholder = "Observaciones, informaci\u00F3n de muestras, notas de corrida...",
      download_last_report = "Descargar \u00DAltimo Reporte",

      # Panel de verificación previa (Tab 5)
      pf_plate_ok = "Datos de placa cargados",
      pf_plate_missing = "Sin datos de placa - vaya a la pesta\u00F1a Cargar",
      pf_std_count_ok = "%d pozos est\u00E1ndar definidos",
      pf_std_count_low = "Solo %d pozos est\u00E1ndar (se necesitan >= 4)",
      pf_elisa_controls_ok = "Controles ELISA presentes (Blanco, NSB, B0)",
      pf_elisa_controls_missing = "Faltan controles ELISA: %s",
      pf_dilution_ok = "Factores de diluci\u00F3n v\u00E1lidos",
      pf_dilution_invalid = "Algunas entradas de diluci\u00F3n son inv\u00E1lidas",
      pf_std_count_match = "Cantidad de est\u00E1ndares coincide: %d grupos para %d est\u00E1ndares",
      pf_std_count_mismatch = "Grupos de est\u00E1ndar (%d) difieren de los est\u00E1ndares configurados (%d)",
      pf_rep_groups_ok = "Los grupos de r\u00E9plicas son consistentes",
      pf_rep_groups_mixed = "Tipos de pozos mezclados en grupo(s) de r\u00E9plicas: %s",
      pf_ids_ok = "Todos los pozos de muestra tienen ID",
      pf_ids_empty = "%d pozo(s) de muestra sin ID",
      pf_elisa_reps_ok = "Los controles ELISA tienen r\u00E9plicas adecuadas",
      pf_elisa_reps_low = "Pocas r\u00E9plicas para control(es) ELISA: %s (se recomienda >= 2)",
      pf_tissue_missing = "No se ingresaron pesos de tejido (necesarios para resultados ELISA normalizados por tejido)",
      pf_badge_errors = "Problemas bloqueantes detectados \u2014 res\u00FAelvalos antes de generar el reporte",
      pf_badge_warnings = "Advertencias detectadas \u2014 se puede generar el reporte pero se recomienda revisar",
      pf_badge_ok = "Todas las verificaciones pasaron \u2014 listo para generar el reporte",

      # QC
      qc_conc_label = "Concentraci\u00F3n QC (%s):",
      expected_hill = "Pendiente de Hill esperada:",
      qc_required = "\u26A0\uFE0F Concentraci\u00F3n QC requerida",
      sci_notation = "\u26A0\uFE0F Use notaci\u00F3n cient\u00EDfica (ej., 3e-9)",
      must_be_numeric = "\u26A0\uFE0F Debe ser un valor num\u00E9rico",
      outside_rba_range = "\u26A0\uFE0F Fuera del rango t\u00EDpico de RBA (1e-12 a 1e-6 mol/L)",
      outside_elisa_range = "\u26A0\uFE0F Fuera del rango t\u00EDpico de ELISA (0.1-10000)",
      hill_required = "\u26A0\uFE0F Pendiente de Hill requerida",
      hill_outside_range = "\u26A0\uFE0F Fuera del rango esperado (0.5\u20131.5)",
      invalid_dilution = "\u26A0\uFE0F Entradas de diluci\u00F3n inv\u00E1lidas (celdas rojas)",
      
      # Notifications
      multiwave_detected = "\u2705 Archivo multi-longitud de onda detectado: %s\n%d pozos detectados por placa\nFormato: %s%s",
      import_success = "\u2705 Importado: %s\n%d pozos detectados\nFormato: %s%s",
      import_failed = "Importaci\u00F3n fallida: %s",
      file_cleared = "Archivo eliminado",
      data_saved = "Datos guardados en: %s",
      saved_wavelengths = "Datos guardados para %d longitudes de onda",
      
      # Plate layout modal
      layout_title = "Dise\u00F1o de Placa de Muestras",
      layout_desc = "Esperado: Etiquetas de fila (A\u2013H) + 12 columnas num\u00E9ricas.<br>No incluya nombres de columna.",
      
      # Language
      language_label = "Idioma / Language",
      
      # Guided tour
      tour_next = "Siguiente",
      tour_prev = "Atr\u00E1s",
      tour_skip = "Salir",
      tour_done = "Finalizar",

      # Recorrido guiado \u2014 flujo de trabajo de 5 pesta\u00F1as
      tour_language_toggle = "Selector de idioma. Use este men\u00FA en cualquier momento para alternar toda la interfaz (y el reporte generado) entre ingl\u00E9s y espa\u00F1ol.",
      tour_quickstart = "Inicio r\u00E1pido. Un clic aqu\u00ED carga un ajuste predefinido (RBA Saxitoxina, ELISA Cortisol o ELISA Personalizado): completa el tipo de ensayo, las concentraciones est\u00E1ndar y el dise\u00F1o de placa para que pueda pasar directamente a cargar sus datos.",
      tour_config = "Pesta\u00F1a 1 \u2013 Configuraci\u00F3n. Elija RBA o ELISA, seleccione el analito, defina el n\u00FAmero de est\u00E1ndares e ingrese sus concentraciones. Para RBA tambi\u00E9n puede fijar la pendiente de Hill esperada y la concentraci\u00F3n QC.",
      tour_preset_layout = "Plantillas de placa e importaci\u00F3n/exportaci\u00F3n. Cargue un dise\u00F1o guardado, imp\u00F3rtelo desde CSV/Excel o exporte el dise\u00F1o actual para compartirlo.",
      tour_matrix_type = "Matriz de Tipo de Muestra (1). Asigne cada pozo como Est\u00E1ndar, Muestra, QC, Blanco, NSB, B0 o TotalActivity. En ELISA los controles (Blanco / NSB / B0) suelen ir en la columna 1.",
      tour_matrix_id = "Matriz de ID de Muestra (2). Ingrese una etiqueta corta por pozo. Los est\u00E1ndares se nombran autom\u00E1ticamente S1, S2, \u2026 coincidiendo con las concentraciones de la pesta\u00F1a de Configuraci\u00F3n.",
      tour_matrix_dilution = "Matriz de Fracci\u00F3n de Diluci\u00F3n (3). Cada celda es la fracci\u00F3n de concentraci\u00F3n original remanente tras la diluci\u00F3n: sin diluir = 1, 1:2 = 0.5, 1:10 = 0.1. Tambi\u00E9n puede escribir la notaci\u00F3n en raz\u00F3n \"1:N\" \u2014 la app convierte autom\u00E1ticamente.",
      tour_matrix_replicate = "Matriz de Grupos de R\u00E9plicas (4). Los pozos con la misma etiqueta se tratan como r\u00E9plicas de la misma muestra para las estad\u00EDsticas. Use etiquetas diferentes para muestras diferentes.",
      tour_qc_rba = "Par\u00E1metros de Control de Calidad de RBA. Establezca la pendiente de Hill esperada y la concentraci\u00F3n QC usada para monitorear el desempe\u00F1o del ensayo.",
      tour_tissue_weights = "Pesos de tejido y volumen de extracci\u00F3n (solo ELISA). Ingrese la masa de tejido (mg) por grupo de r\u00E9plica y el volumen de extracci\u00F3n (\u00b5L) para que el reporte calcule las concentraciones finales en pg/g de tejido.",
      tour_upload = "Pesta\u00F1a 3 \u2013 Cargar y Previsualizar. Cargue su archivo del lector de placas (.xlsx, .csv o .txt). La Importaci\u00F3n Cl\u00E1sica detecta la regi\u00F3n de placa autom\u00E1ticamente; alterne a Selector Visual de Placa para elegirla arrastrando.",
      tour_heatmap_preview = "Vista previa del mapa de calor. Despu\u00E9s de cargar, este mapa muestra los valores de medici\u00F3n para confirmar visualmente que se detect\u00F3 la regi\u00F3n correcta antes de correr el an\u00E1lisis.",
      tour_analysis = "Pesta\u00F1a 4 \u2013 Configuraci\u00F3n de An\u00E1lisis. Elija ponderaciones DRC (sin ponderar, 1/Y, 1/Y\u00b2), fije el rango cuantificable de %B/B0 (LLOQ/ULOQ), escoja el m\u00E9todo de intervalo de confianza y configure la detecci\u00F3n de at\u00EDpicos.",
      tour_preflight = "Verificaciones previas al vuelo. Un resumen tipo sem\u00E1foro de qu\u00E9 est\u00E1 listo y qu\u00E9 falta: datos de placa, est\u00E1ndares, controles ELISA, par\u00E1metros QC. Resuelva los elementos en rojo antes de generar el reporte.",
      tour_convert = "Pesta\u00F1a 5 \u2013 Generar Reporte. Elija uno o m\u00E1s formatos de salida (HTML, DOCX, PDF) y el idioma del reporte, luego haga clic en Generar Reporte. PDF cambia autom\u00E1ticamente a HTML si no se detecta un motor LaTeX.",
      tour_notes = "Notas y Retroalimentaci\u00F3n. Lo que escriba aqu\u00ED se incluye en el reporte generado para documentaci\u00F3n. Use el bot\u00F3n de Retroalimentaci\u00F3n para contactar a los desarrolladores.",
      
      # ---- REPORT TRANSLATIONS ----
      report_title = "Reporte de An\u00E1lisis de Bioensayo",
      report_title_multi = "Reporte de An\u00E1lisis de Bioensayo Multi-Longitud de Onda",
      report_elisa = "ELISA",
      report_rba = "RBA",
      analysis_report = "Reporte de An\u00E1lisis",
      assay_type = "Tipo de Ensayo:",
      analysis_date = "Fecha de An\u00E1lisis:",
      analyst = "Analista:",
      elisa_intro = "Este reporte analiza datos de ELISA usando una curva dosis\u2013respuesta log\u00EDstica de cuatro par\u00E1metros para estimar concentraciones de %s en muestras desconocidas.",
      elisa_method = "M\u00E9todo: Ensayo inmunoenzim\u00E1tico competitivo con normalizaci\u00F3n %B/B0",
      rba_intro = "Este reporte analiza datos de Ensayo de Uni\u00F3n a Receptor (RBA) usando una curva dosis\u2013respuesta log\u00EDstica de cuatro par\u00E1metros para estimar concentraciones de %s en muestras desconocidas.",
      rba_method = "M\u00E9todo: Ensayo competitivo de uni\u00F3n a receptor con detecci\u00F3n %s",
      analysis_notes = "Notas del An\u00E1lisis",
      no_notes = "No se proporcionaron notas adicionales.",
      std_curve_config = "Configuraci\u00F3n de la Curva Est\u00E1ndar",
      std_concentrations_table = "Concentraciones Est\u00E1ndar",
      drc_analysis = "An\u00E1lisis de Curva Dosis\u2013Respuesta",
      all_std_acceptable = "\u2705 Todos los est\u00E1ndares muestran variabilidad aceptable (<30%% CV).",
      high_var_standards = "Est\u00E1ndares con alta variabilidad (>30%% CV):",
      model_parameters = "Par\u00E1metros del Modelo",
      four_pl_coefficients = "Coeficientes del Modelo Log\u00EDstico de Cuatro Par\u00E1metros",
      hill_slope = "Pendiente de Hill",
      bottom = "L\u00EDmite Inferior",
      top = "L\u00EDmite Superior",
      ic50 = "IC50",
      model_fit_stats = "Estad\u00EDsticas de Ajuste del Modelo:",
      standards_used = "Est\u00E1ndares Utilizados",
      std_backcalc_title = "Retroc\u00E1lculo y Recuperaci\u00F3n de Est\u00E1ndares",
      std_backcalc_caption = "Retroc\u00E1lculo y Recuperaci\u00F3n de Est\u00E1ndares",
      overall_recovery = "Recuperaci\u00F3n Media Global: %.1f%%",
      recovery_acceptable = "\u2705 La recuperaci\u00F3n global est\u00E1 dentro del rango aceptable (80\u2013120%%).",
      recovery_outside = "\u26A0\uFE0F La recuperaci\u00F3n global est\u00E1 fuera del rango t\u00EDpico aceptable (80\u2013120%%). Revise el ajuste de curva.",
      sample_results = "Resultados de Concentraci\u00F3n de Muestras",
      sample_results_caption = "Resultados de Cuantificaci\u00F3n de Muestras - %s",
      with_tissue = "(con normalizaci\u00F3n por tejido)",
      output_files_created = "Archivos de Salida Creados:",
      individual_results = "`unknown_results.csv` - Resultados individuales por pozo",
      summary_results = "`unknown_results_summary.csv` - Estad\u00EDsticas por grupo de r\u00E9plica con intervalos de confianza",
      quality_alert = "\u26A0\uFE0F Alerta de Calidad:",
      high_cv_groups = "Grupos de r\u00E9plicas con alta variabilidad (CV > 30%%): %s",
      check_preparation = "Considere verificar la preparaci\u00F3n de muestras o la consistencia de diluci\u00F3n",
      quality_pass = "\u2705 Control de Calidad: Todos los grupos de r\u00E9plicas muestran variabilidad aceptable (CV \u2264 30%%)",
      no_samples_quantified = "No se pudieron cuantificar muestras.",
      detailed_summary = "Resumen Detallado de Resultados de Muestras",
      detailed_caption = "Resultados Detallados con Intervalos de Confianza Basados en el Modelo",
      sample_variability = "Visualizaci\u00F3n de Variabilidad de Muestras",
      sample_variability_desc = "Gr\u00E1fico de caja y dispersi\u00F3n que muestra las concentraciones estimadas por grupo de r\u00E9plica. Los puntos se colorean seg\u00FAn si caen dentro del rango lineal validado. \u00DAselo para evaluar la consistencia de r\u00E9plicas e identificar valores at\u00EDpicos.",
      drc_with_samples = "Curva Dosis-Respuesta con Muestras Desconocidas",
      drc_combined_title = "Curva Dosis-Respuesta con Est\u00E1ndares y Muestras Desconocidas",
      within_range = "Dentro del Rango",
      out_of_range = "Fuera del Rango",
      unknown_range = "Desconocido",
      flag_above_uloq = ">ULOQ",
      flag_below_lloq = "<LLOQ",
      report_generated = "Reporte Generado:",
      contact = "Contacto:",
      feedback = "Retroalimentaci\u00F3n:",
      online_form = "Formulario en L\u00EDnea",
      automated_analysis = "An\u00E1lisis automatizado de bioensayo usando sistema modular v2.0",
      automated_multi = "An\u00E1lisis automatizado multi-longitud de onda usando sistema modular v2.0",
      
      # Multi-wavelength specific
      multi_overview = "Resumen de An\u00E1lisis Multi-Longitud de Onda",
      multi_overview_desc = "Este reporte contiene an\u00E1lisis para **%d longitudes de onda**: %s.",
      multi_compare = "Cada longitud de onda se analiza independientemente usando el mismo dise\u00F1o de placa pero diferentes lecturas de absorbancia. Esto le permite:",
      multi_benefit1 = "Comparar la calidad de la curva dosis-respuesta entre longitudes de onda",
      multi_benefit2 = "Identificar la longitud de onda \u00F3ptima para su ensayo",
      multi_benefit3 = "Verificar que las concentraciones de muestras sean consistentes entre lecturas",
      multi_sections = "Secciones:",
      multi_exec_summary = "Resumen Ejecutivo (abajo) - Comparaci\u00F3n r\u00E1pida entre longitudes de onda",
      multi_detailed = "An\u00E1lisis Detallado para cada longitud de onda (secciones siguientes)",
      exec_summary_title = "Resumen Ejecutivo: Comparaci\u00F3n de Longitudes de Onda",
      wavelength_analysis = "An\u00E1lisis de Longitud de Onda %s",
      analysis_n_of = "An\u00E1lisis %d de %d",
      data_overview = "Resumen de Datos por Longitud de Onda",
      overall_conclusions = "Conclusiones Generales",
      wavelength_performance = "Resumen de Rendimiento por Longitud de Onda",
      recommendations = "Recomendaciones:",
      rec_r2 = "Elija la longitud de onda con mayor R\u00B2 (mejor ajuste de curva)",
      rec_cv = "Verifique que los CV de muestras sean aceptables (<20%% preferido)",
      rec_separation = "Considere qu\u00E9 longitud de onda da la separaci\u00F3n m\u00E1s confiable entre muestras",
      recommended_wavelength = "\U0001F31F **Recomendada:** %s (menor RMSE: %.3f)",
      
      # Table column headers
      col_replicate_group = "Grupo de R\u00E9plica",
      col_sample_ids = "IDs de Muestra",
      col_sample_type = "Tipo de Muestra",
      col_n = "n",
      col_mean = "Media (%s)",
      col_sd = "DE (%s)",
      col_se = "EE (%s)",
      col_ci = "IC 95%%",
      col_cv = "CV%%",
      col_range_flag = "Rango",
      col_tissue_conc = "Conc. (pg/g tejido)",
      col_tissue_mass = "Masa de Tejido",
      col_nominal = "Nominal (%s)",
      col_backcalc = "Retroc\u00E1lc. (%s)",
      col_recovery = "Recuperaci\u00F3n (%%)",
      col_parameter = "Par\u00E1metro",
      col_estimate = "Estimaci\u00F3n",
      col_std_error = "Error Est\u00E1ndar",
      col_pvalue = "Valor p",
      col_wavelength = "Longitud de Onda",
      col_standards = "Est\u00E1ndares",
      col_samples = "Muestras",
      col_quantified = "Cuantificadas",
      col_r2 = "R\u00B2",
      col_rmse = "RMSE",
      col_ic50 = "IC50",
      col_mean_cv = "CV%% Medio",
      
      # Analysis Settings (app UI)
      analysis_settings_title = "Configuraci\u00F3n de An\u00E1lisis",
      regression_weight_label = "Ponderaci\u00F3n de regresi\u00F3n DRC:",
      quant_range_min_label = "L\u00EDmite inferior %B/B0:",
      quant_range_max_label = "L\u00EDmite superior %B/B0:",
      quant_range_help = "Las muestras fuera de este rango se marcan como <LLOQ o >ULOQ.",
      ci_method_label = "M\u00E9todo de intervalo de confianza:",
      outlier_detection_label = "Activar detecci\u00F3n de valores at\u00EDpicos",
      outlier_min_n_label = "R\u00E9plicas m\u00EDnimas para prueba de valores at\u00EDpicos:",
      outlier_help = "Prueba Q de Dixon para n=3-5, prueba de Grubbs para n>=6. Los at\u00EDpicos se marcan, no se eliminan.",
      normality_assumption_label = "Supuesto de normalidad para detecci\u00F3n de at\u00EDpicos:",
      normality_assume = "Asumir normalidad (predeterminado)",
      normality_test_shapiro = "Probar con Shapiro-Wilk",
      normality_shapiro_help = "Se ejecuta la prueba de Shapiro-Wilk en cada grupo de r\u00E9plicas. Si p < 0.05 (no normal), la detecci\u00F3n basada en MAD reemplaza la prueba de Grubbs.",
      outlier_method_mad = "Basado en MAD (distribuci\u00F3n no normal detectada)",
      cv_limit_label = "CV m\u00E1ximo para est\u00E1ndares (%):",
      cv_limit_help = "Los est\u00E1ndares que excedan este umbral de CV% se marcan como alta variabilidad.",
      advanced_options_heading = "Opciones Avanzadas \u2014 ponderaci\u00F3n, IC, at\u00EDpicos y umbrales de QC",
      advanced_options_intro = "Estos ajustes controlan el m\u00E9todo de intervalo de confianza, la detecci\u00F3n de at\u00EDpicos, el rango de cuantificaci\u00F3n y los umbrales de calidad.",
      regression_weight_help = "Seleccione m\u00FAltiples ponderaciones para comparar resultados lado a lado.",
      weight_unweighted = "Sin ponderar",
      weight_inv_y = "1/Y (moderada)",
      weight_inv_y2 = "1/Y\u00B2 (recomendada para inmunoensayos)",
      ci_t_dist = "distribuci\u00F3n t (predeterminada)",
      ci_bootstrap_choice = "Bootstrap (1000 remuestreos)",

      # Tab titles
      tab_analysis_title = "4. Configuraci\u00F3n de An\u00E1lisis",
      tab_report_title = "5. Generar Reporte",

      # Statistical warnings and footnotes
      r2_identical_warning = "No se pudo calcular R-cuadrado: todos los valores de respuesta son id\u00E9nticos (varianza total cero). Verifique la integridad del ensayo.",
      ci_asymmetric_footnote = "Los l\u00EDmites inferiores de confianza est\u00E1n restringidos a cero, ya que las concentraciones negativas no tienen significado biol\u00F3gico. Esto puede resultar en intervalos de confianza asim\u00E9tricos.",

      # Report sections
      summary_title = "Resumen",
      interpretation_title = "Interpretaci\u00F3n y Recomendaciones",
      interpretation_pass = "Todos los criterios de calidad cumplidos. Los resultados son aptos para reporte.",
      interpretation_warn = "Advertencias de calidad detectadas: %s. Revise los elementos marcados antes de usar.",
      methods_title = "M\u00E9todos",
      plate_positional_title = "Calidad Posicional de la Placa",
      data_quality_title = "Resumen de Calidad de Datos",

      # Traffic-light QC card (report)
      qc_card_title = "Resumen de Control de Calidad",
      qc_metric = "M\u00E9trica",
      qc_value = "Valor",
      qc_status = "Estado",
      qc_r2 = "R-cuadrado",
      qc_hill = "Pendiente de Hill",
      qc_max_cv = "CV m\u00E1ximo de r\u00E9plicas",
      qc_recovery = "Recuperaci\u00F3n media de est\u00E1ndares",
      qc_green = "Aprobado",
      qc_amber = "Advertencia",
      qc_red = "Fallo",

      # LLOQ/ULOQ determination (report)
      lloq_uloq_title = "L\u00EDmites de Cuantificaci\u00F3n",
      lloq_label = "LLOQ (L\u00EDmite Inferior de Cuantificaci\u00F3n)",
      uloq_label = "ULOQ (L\u00EDmite Superior de Cuantificaci\u00F3n)",
      lloq_uloq_desc = "Determinados por la precisi\u00F3n de los est\u00E1ndares retro-calculados (recuperaci\u00F3n 80-120%%, CV <20%%).",
      lloq_uloq_none = "No se pudieron determinar los l\u00EDmites de cuantificaci\u00F3n a partir de los est\u00E1ndares disponibles.",
      backcalc_title = "Retro-C\u00E1lculo de Est\u00E1ndares",
      col_accuracy = "Precisi\u00F3n",

      # Outlier detection (report)
      outlier_title = "Detecci\u00F3n de Valores At\u00EDpicos",
      outlier_desc = "Prueba estad\u00EDstica de valores at\u00EDpicos aplicada a grupos de r\u00E9plicas (n >= %d).",
      outlier_none = "No se detectaron valores at\u00EDpicos.",
      outlier_found = "%d valor(es) at\u00EDpico(s) marcado(s) en %d grupo(s) de r\u00E9plicas.",
      outlier_method_dixon = "Prueba Q de Dixon (n=3-5)",
      outlier_method_grubbs = "Prueba de Grubbs (n>=6)",
      outlier_flagged = "Marcado",
      outlier_flagged_not_removed_note = "Los valores at\u00EDpicos marcados permanecen visibles en la tabla de resultados detallados por pozo y en las exportaciones a CSV, pero se excluyen del c\u00E1lculo de la media, DE, CV e intervalos de confianza del grupo de r\u00E9plicas. Esto preserva la visibilidad completa de los datos crudos y evita la contaminaci\u00F3n de los estad\u00EDsticos resumen por valores at\u00EDpicos.",

      # Range indicator explanation (Phase 1.4)
      range_indicators_explanation = paste0(
        "> **Dos indicadores de rango independientes aparecen en este reporte:**\n>\n",
        "> - **Interpolado / Extrapolado** se refiere a si la concentraci\u00F3n ",
        "estimada cae dentro del rango de concentraciones est\u00E1ndar ajustadas en ",
        "esta placa. Este es un enunciado sobre la cobertura de la curva.\n>\n",
        "> - **Dentro del rango / <LLOQ / >ULOQ** se refiere a si la estimaci\u00F3n ",
        "cae dentro del rango lineal validado (cuantificable) de la curva ",
        "dosis-respuesta, definido por EC20/EC80 para RBA o los l\u00EDmites ",
        "%B/B0 (por defecto 20\u201380%) para ELISA. Este es un enunciado sobre la ",
        "calidad del reporte.\n>\n",
        "> Una muestra puede estar interpolada pero fuera del rango cuantificable ",
        "(p. ej., la respuesta cae en la porci\u00F3n plana de la curva cerca del ",
        "asintota superior o inferior), o dentro del rango cuantificable pero ",
        "t\u00E9cnicamente extrapolada (si el usuario proporcion\u00F3 pocos ",
        "est\u00E1ndares). Ambos indicadores deben considerarse al interpretar los ",
        "resultados."),

      # Bootstrap CI (report)
      ci_bootstrap_note = "Intervalos de confianza del 95%% calculados mediante remuestreo bootstrap (1000 iteraciones).",
      ci_tdist_note = "Intervalos de confianza del 95%% calculados mediante distribuci\u00F3n t.",
      ci_delta_method_note = "Las concentraciones individuales de muestras incluyen IC a partir de predicci\u00F3n inversa (m\u00E9todo delta), considerando la incertidumbre del ajuste de la curva.",

      # Methods section (report)
      methods_title = "M\u00E9todos",
      methods_drc_citation = "An\u00E1lisis dosis-respuesta realizado con el paquete drc (Ritz et al., 2015, *PLOS ONE* 10(12):e0146021). ",
      methods_outlier_citation = "Detecci\u00F3n de valores at\u00EDpicos: prueba Q de Dixon (Dixon, 1950) para n=3-5, prueba de Grubbs (Grubbs, 1950) para n\u22656. ",

      # Weight comparison (report)
      weight_comparison_title = "Comparaci\u00F3n de Ponderaci\u00F3n DRC",
      weight_comparison_desc = "Comparaci\u00F3n de ajustes de curva dosis-respuesta usando diferentes ponderaciones de regresi\u00F3n. El gr\u00E1fico superpuesto y la tabla de par\u00E1metros ayudan a identificar qu\u00E9 ponderaci\u00F3n se ajusta mejor a los datos.",
      weight_comparison_primary = "Las concentraciones de muestras en este reporte se calcularon usando el modelo **%s** (primer seleccionado).",

      # Plate heatmap (report)
      heatmap_title = "Mapa de Calor de Placa",
      heatmap_desc = "Representaci\u00F3n visual de los valores de medici\u00F3n crudos en la placa.",

      # Cross-wavelength concordance (multi-WL report)
      concordance_title = "Concordancia entre Longitudes de Onda",
      concordance_desc = "Comparaci\u00F3n de concentraciones de muestras estimadas a diferentes longitudes de onda.",
      concordance_ccc = "Coeficiente de Correlaci\u00F3n de Concordancia de Lin (CCC)",
      concordance_ccc_value = "CCC = %.4f [IC 95%%: %.4f - %.4f]",
      concordance_bland_altman = "An\u00E1lisis de Bland-Altman",
      bland_altman_explanation = "Cada gr\u00E1fico compara las concentraciones calculadas de las muestras entre dos longitudes de onda: la l\u00EDnea discontinua muestra el sesgo medio (diferencia sistem\u00E1tica), mientras que las l\u00EDneas rojas punteadas marcan los l\u00EDmites de acuerdo (media +/- 1.96 DE). Si todos los puntos caen dentro de los l\u00EDmites de acuerdo y el sesgo es cercano a cero, las dos longitudes de onda pueden considerarse intercambiables para fines de cuantificaci\u00F3n.",
      concordance_bias = "Sesgo medio: %.4f",
      concordance_loa = "L\u00EDmites de acuerdo: [%.4f, %.4f]",
      concordance_no_data = "Datos pareados insuficientes para an\u00E1lisis de concordancia.",
      concordance_excellent = "Excelente concordancia (CCC > 0.99)",
      concordance_good = "Buena concordancia (CCC 0.95-0.99)",
      concordance_moderate = "Concordancia moderada (CCC 0.90-0.95)",
      concordance_poor = "Concordancia pobre (CCC < 0.90)",

      # Parallelism / relative potency (report)
      parallelism_title = "Paralelismo y Potencia Relativa",
      parallelism_not_applicable = "No aplicable: %s",
      parallelism_no_model = "No aplicable: No hay modelo ajustado disponible para la evaluaci\u00F3n de paralelismo.",

      # Plate layout import/save (app UI)
      layout_import_title = "Importar Dise\u00F1o de Placa",
      layout_import_desc = "Cargue un archivo CSV o Excel con el dise\u00F1o de placa (matrices de TipoMuestra, IDMuestra, Diluci\u00F3n, R\u00E9plica).",
      layout_import_btn = "Importar Dise\u00F1o",
      layout_save_btn = "Guardar Dise\u00F1o Actual",
      layout_load_btn = "Cargar Dise\u00F1o Guardado",
      layout_saved_msg = "Dise\u00F1o guardado exitosamente.",
      layout_loaded_msg = "Dise\u00F1o cargado exitosamente.",
      layout_import_success = "Dise\u00F1o de placa importado del archivo.",
      layout_no_saved = "No se encontraron dise\u00F1os guardados.",

      # Omitted wells
      omitted_by_user = "Omitido del an\u00E1lisis por el usuario"
    )
  )
}

#' Get translation for a specific key
#' @param key Translation key
#' @param lang Language code ("en" or "es")
#' @param ... Arguments for sprintf formatting
#' @return Translated string
tr <- function(key, lang = "en", ...) {
  translations <- get_translations()
  text <- translations[[lang]][[key]]
  if (is.null(text)) {
    warning(sprintf("Missing translation for key '%s' in language '%s'", key, lang))
    text <- translations[["en"]][[key]]
    if (is.null(text)) return(paste0("[", key, "]"))
  }
  if (length(list(...)) > 0) {
    return(sprintf(text, ...))
  }
  return(text)
}

#' Build a translated choice vector for selectInput / radioButtons / checkboxGroupInput.
#' @param ids Character vector of choice values (stored in input$...).
#' @param label_keys Character vector of translation keys, same length as ids.
#' @param lang Language code.
#' @return A named character vector suitable for `choices = ...`.
tr_choices <- function(ids, label_keys, lang = "en") {
  stopifnot(length(ids) == length(label_keys))
  setNames(ids, vapply(label_keys, tr, character(1), lang = lang))
}

#' Build a coloured pre-flight check line with an icon and translated message.
#' @param icon_name Font Awesome icon name passed to shiny::icon().
#' @param color CSS colour string.
#' @param key Translation key for the message.
#' @param lang Language code.
#' @param ... Arguments forwarded to sprintf inside tr().
pf_line <- function(icon_name, color, key, lang, ...) {
  shiny::tags$div(style = paste0("color: ", color, ";"),
                  shiny::icon(icon_name), " ", tr(key, lang, ...))
}

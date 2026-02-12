# ==============================================================================
# Internationalization (i18n) Module
# Purpose: Provide English and Spanish translations for the Shiny app and reports
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
      step0_title = "Step 0: Assay Configuration",
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
      
      # Step 1: Matrices
      step1_title = "Step 1: Edit Microplate Matrices & QC Parameters",
      type_matrix = "1. Sample Type (Standard, Sample, QC, Blank, Other)",
      id_matrix = "2. Sample ID",
      qc_params = "3. Quality Control Parameters",
      dilution_matrix = "4. Dilution Factors (numeric or ratio like 1:2)",
      replicate_matrix = "5. Replicate Groups",
      tissue_weight_title = "6. Tissue Weights (ELISA only)",
      tissue_weight_desc = "Enter tissue weight (mg) per replicate group for pg/g tissue calculation.",
      extraction_vol_label = "Extraction volume (\u00B5L):",
      reset_default = "Reset to Default",
      
      # Step 2: Upload
      step2_title = "Step 2: Upload Plate Data",
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
      
      # Notes and feedback
      notes_label = "Notes (optional):",
      notes_placeholder = "Observations...",
      give_feedback = "Give Feedback",
      
      # Step 3: Report generation
      report_formats = "Report formats:",
      report_language = "Report language:",
      generate_report = "Step 3: Generate Report",
      generating_report = "Generating report...",
      
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
      tour_step0 = "Step 0: Configure your assay type and standard concentrations. Choose between RBA and ELISA, select the analyte, and enter standard concentrations.",
      tour_step1_type = "Step 1: Define the plate layout. Assign each well as Standard, Sample, QC, Blank, NSB, B0, etc. For ELISA, control wells (Blank, NSB, B0) are pre-filled in column 1.",
      tour_step1_id = "Assign sample IDs to each well. Standards are automatically labeled S1, S2, etc.",
      tour_step1_dilution = "Enter dilution factors for each well. Supports numeric values (0.5) and ratios (1:2).",
      tour_step1_replicate = "Define replicate groups. Wells with the same label are treated as replicates.",
      tour_step1_tissue = "For ELISA cortisol: enter tissue weights (mg) per replicate group. This enables pg/g tissue calculations in the report.",
      tour_step1_extraction = "Set the extraction volume used in sample preparation. Default is 500 \u00B5L.",
      tour_step2_upload = "Step 2: Upload your plate reader data file (.xlsx, .csv, or .txt). The app auto-detects plate regions and multi-wavelength data.",
      tour_step2_visual = "Alternatively, use the Visual Plate Selector to preview the file and manually select plate regions by dragging over them.",
      tour_step3 = "Step 3: Choose report format(s) and language, then generate the report. The analysis includes dose-response curve fitting, sample quantification, and quality assessment.",
      tour_language = "Switch between English and Spanish for the entire interface.",
      
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
      elisa_method = "Method: Competitive enzyme-linked immunosorbent assay with %%B/B0 normalization",
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
      col_pvalue = "P-value",
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
      quant_range_min_label = "Lower %%B/B0 bound:",
      quant_range_max_label = "Upper %%B/B0 bound:",
      quant_range_help = "Samples outside this range are flagged as <LLOQ or >ULOQ.",
      ci_method_label = "Confidence interval method:",
      outlier_detection_label = "Enable outlier detection",
      outlier_min_n_label = "Minimum replicates for outlier test:",
      outlier_help = "Dixon's Q-test for n=3-5, Grubbs' test for n>=6. Outliers are flagged, not removed.",

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

      # Bootstrap CI (report)
      ci_bootstrap_note = "95%% confidence intervals calculated using bootstrap resampling (1000 iterations).",
      ci_tdist_note = "95%% confidence intervals calculated using t-distribution.",

      # Plate heatmap (report)
      heatmap_title = "Plate Heatmap",
      heatmap_desc = "Visual representation of raw measurement values across the plate.",

      # Cross-wavelength concordance (multi-WL report)
      concordance_title = "Cross-Wavelength Concordance",
      concordance_desc = "Comparison of sample concentrations estimated at different wavelengths.",
      concordance_ccc = "Lin's Concordance Correlation Coefficient (CCC)",
      concordance_ccc_value = "CCC = %.4f [95%% CI: %.4f - %.4f]",
      concordance_bland_altman = "Bland-Altman Analysis",
      concordance_bias = "Mean bias: %.4f",
      concordance_loa = "Limits of agreement: [%.4f, %.4f]",
      concordance_no_data = "Insufficient paired sample data for concordance analysis.",
      concordance_excellent = "Excellent agreement (CCC > 0.99)",
      concordance_good = "Good agreement (CCC 0.95-0.99)",
      concordance_moderate = "Moderate agreement (CCC 0.90-0.95)",
      concordance_poor = "Poor agreement (CCC < 0.90)",

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
      step0_title = "Paso 0: Configuraci\u00F3n del Ensayo",
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
      
      # Step 1: Matrices
      step1_title = "Paso 1: Editar Matrices de Microplaca y Par\u00E1metros QC",
      type_matrix = "1. Tipo de Muestra (Est\u00E1ndar, Muestra, QC, Blanco, Otro)",
      id_matrix = "2. ID de Muestra",
      qc_params = "3. Par\u00E1metros de Control de Calidad",
      dilution_matrix = "4. Factores de Diluci\u00F3n (num\u00E9rico o raz\u00F3n como 1:2)",
      replicate_matrix = "5. Grupos de R\u00E9plicas",
      tissue_weight_title = "6. Pesos de Tejido (solo ELISA)",
      tissue_weight_desc = "Ingrese el peso del tejido (mg) por grupo de r\u00E9plica para el c\u00E1lculo de pg/g de tejido.",
      extraction_vol_label = "Volumen de extracci\u00F3n (\u00B5L):",
      reset_default = "Restablecer Valores",
      
      # Step 2: Upload
      step2_title = "Paso 2: Cargar Datos de Placa",
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
      
      # Notes and feedback
      notes_label = "Notas (opcional):",
      notes_placeholder = "Observaciones...",
      give_feedback = "Dar Retroalimentaci\u00F3n",
      
      # Step 3: Report generation
      report_formats = "Formatos de reporte:",
      report_language = "Idioma del reporte:",
      generate_report = "Paso 3: Generar Reporte",
      generating_report = "Generando reporte...",
      
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
      tour_step0 = "Paso 0: Configure su tipo de ensayo y concentraciones est\u00E1ndar. Elija entre RBA y ELISA, seleccione el analito e ingrese las concentraciones est\u00E1ndar.",
      tour_step1_type = "Paso 1: Defina el dise\u00F1o de la placa. Asigne cada pozo como Est\u00E1ndar, Muestra, QC, Blanco, NSB, B0, etc. Para ELISA, los pozos de control se pre-llenan en la columna 1.",
      tour_step1_id = "Asigne IDs de muestra a cada pozo. Los est\u00E1ndares se etiquetan autom\u00E1ticamente como S1, S2, etc.",
      tour_step1_dilution = "Ingrese factores de diluci\u00F3n para cada pozo. Soporta valores num\u00E9ricos (0.5) y razones (1:2).",
      tour_step1_replicate = "Defina grupos de r\u00E9plicas. Los pozos con la misma etiqueta se tratan como r\u00E9plicas.",
      tour_step1_tissue = "Para ELISA cortisol: ingrese pesos de tejido (mg) por grupo de r\u00E9plica. Esto permite c\u00E1lculos de pg/g de tejido en el reporte.",
      tour_step1_extraction = "Establezca el volumen de extracci\u00F3n utilizado en la preparaci\u00F3n de muestras. El valor predeterminado es 500 \u00B5L.",
      tour_step2_upload = "Paso 2: Cargue su archivo de datos del lector de placas (.xlsx, .csv o .txt). La app detecta autom\u00E1ticamente las regiones de placa y datos multi-longitud de onda.",
      tour_step2_visual = "Alternativamente, use el Selector Visual de Placa para previsualizar el archivo y seleccionar manualmente las regiones de placa arrastrando sobre ellas.",
      tour_step3 = "Paso 3: Elija formato(s) de reporte e idioma, luego genere el reporte. El an\u00E1lisis incluye ajuste de curva dosis-respuesta, cuantificaci\u00F3n de muestras y evaluaci\u00F3n de calidad.",
      tour_language = "Cambie entre ingl\u00E9s y espa\u00F1ol para toda la interfaz.",
      
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
      elisa_method = "M\u00E9todo: Ensayo inmunoenzim\u00E1tico competitivo con normalizaci\u00F3n %%B/B0",
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
      quant_range_min_label = "L\u00EDmite inferior %%B/B0:",
      quant_range_max_label = "L\u00EDmite superior %%B/B0:",
      quant_range_help = "Las muestras fuera de este rango se marcan como <LLOQ o >ULOQ.",
      ci_method_label = "M\u00E9todo de intervalo de confianza:",
      outlier_detection_label = "Activar detecci\u00F3n de valores at\u00EDpicos",
      outlier_min_n_label = "R\u00E9plicas m\u00EDnimas para prueba de valores at\u00EDpicos:",
      outlier_help = "Prueba Q de Dixon para n=3-5, prueba de Grubbs para n>=6. Los at\u00EDpicos se marcan, no se eliminan.",

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

      # Bootstrap CI (report)
      ci_bootstrap_note = "Intervalos de confianza del 95%% calculados mediante remuestreo bootstrap (1000 iteraciones).",
      ci_tdist_note = "Intervalos de confianza del 95%% calculados mediante distribuci\u00F3n t.",

      # Plate heatmap (report)
      heatmap_title = "Mapa de Calor de Placa",
      heatmap_desc = "Representaci\u00F3n visual de los valores de medici\u00F3n crudos en la placa.",

      # Cross-wavelength concordance (multi-WL report)
      concordance_title = "Concordancia entre Longitudes de Onda",
      concordance_desc = "Comparaci\u00F3n de concentraciones de muestras estimadas a diferentes longitudes de onda.",
      concordance_ccc = "Coeficiente de Correlaci\u00F3n de Concordancia de Lin (CCC)",
      concordance_ccc_value = "CCC = %.4f [IC 95%%: %.4f - %.4f]",
      concordance_bland_altman = "An\u00E1lisis de Bland-Altman",
      concordance_bias = "Sesgo medio: %.4f",
      concordance_loa = "L\u00EDmites de acuerdo: [%.4f, %.4f]",
      concordance_no_data = "Datos pareados insuficientes para an\u00E1lisis de concordancia.",
      concordance_excellent = "Excelente concordancia (CCC > 0.99)",
      concordance_good = "Buena concordancia (CCC 0.95-0.99)",
      concordance_moderate = "Concordancia moderada (CCC 0.90-0.95)",
      concordance_poor = "Concordancia pobre (CCC < 0.90)",

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

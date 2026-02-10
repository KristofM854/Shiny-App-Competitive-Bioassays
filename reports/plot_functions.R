# ==============================================================================
# Table and Plot Functions  
# Purpose: Standardized rendering functions for consistent report output
# ==============================================================================

# Source constants and core functions
if (!exists("TABLE_CONFIG")) {
  source("report_constants.R")
}

# ==============================================================================
# TABLE FUNCTIONS
# ==============================================================================

#' Render table with consistent styling
#' @param data Data frame to display
#' @param caption Table caption
#' @param col_names Custom column names (optional)
#' @param digits Number of decimal places
#' @return Formatted table (HTML or simple based on output format)
render_table <- function(data, caption, col_names = NULL, digits = TABLE_CONFIG$digits) {
  
  if (knitr::is_html_output()) {
    
    tbl <- if (is.null(col_names)) {
      knitr::kable(data, caption = caption, digits = digits)
    } else {
      knitr::kable(data, caption = caption, col.names = col_names, digits = digits)
    }
    
    tbl %>%
      kableExtra::kable_styling(
        bootstrap_options = TABLE_CONFIG$html_options,
        full_width = TABLE_CONFIG$full_width,
        position = TABLE_CONFIG$position
      )
      
  } else {
    # PDF/Word output - simple table
    if (is.null(col_names)) {
      knitr::kable(data, caption = caption, digits = digits)
    } else {
      knitr::kable(data, caption = caption, col.names = col_names, digits = digits)  
    }
  }
}

#' Render QC results table with colored status indicators
#' @param qc_results Data frame with Check, Result, Pass columns
#' @return Formatted QC table
render_qc_table <- function(qc_results) {
  
  if (knitr::is_html_output()) {
    
    # Color code results based on Pass status
    qc_html <- qc_results
    qc_html$Result <- ifelse(
      qc_results$Pass,
      kableExtra::cell_spec(qc_results$Result, "html", color = "green", bold = TRUE),
      kableExtra::cell_spec(qc_results$Result, "html", color = "orange", bold = TRUE)
    )
    
    knitr::kable(
      qc_html[, c("Check", "Result")],  # Drop Pass column
      format = "html",
      escape = FALSE,
      align = c("l", "l"),
      caption = "Quality Control Checks"
    ) %>%
      kableExtra::kable_styling(
        full_width = FALSE, 
        bootstrap_options = c("striped", "hover")
      )
      
  } else {
    # Plain text for PDF/Word
    cat("Quality Control Checks\n\n")
    cat(sprintf("%-60s %-30s\n", "Check", "Result"))
    cat(strrep("-", 90), "\n")
    
    for (i in seq_len(nrow(qc_results))) {
      cat(sprintf("%-60s %-30s\n", qc_results$Check[i], qc_results$Result[i]))
    }
  }
}

# ==============================================================================
# PLOT FUNCTIONS  
# ==============================================================================

#' Get consistent plot theme based on assay type
#' @param assay_config Assay configuration list
#' @return ggplot2 theme
get_plot_theme <- function(assay_config) {
  
  theme_classic() +
    theme(
      legend.title = element_blank(),
      axis.title.x = ggtext::element_markdown(size = 12),
      axis.title.y = ggtext::element_markdown(size = 12), 
      axis.text.x = ggtext::element_markdown(size = 12, angle = 45, margin = margin(t = 10)),
      axis.text.y = ggtext::element_markdown(size = 12),
      legend.text = ggtext::element_markdown(lineheight = 1.5, size = 12),
      legend.position = "top"
    )
}

#' Get color scheme for plots based on assay type
#' @param assay_config Assay configuration list
#' @return Named list of colors
get_color_scheme <- function(assay_config) {
  
  assay_type <- assay_config$assay_type %||% "rba"
  return(PLOT_CONFIG$colors[[assay_type]])
}

#' Create dose-response curve plot
#' @param standards_data Data frame with standards
#' @param model_fits Data frame with fitted curve
#' @param assay_config Assay configuration
#' @param response_var Name of response variable column
#' @return ggplot2 object
create_dose_response_plot <- function(standards_data, model_fits, assay_config, response_var = "MeasurementValue") {
  
  labels <- get_axis_labels(assay_config)
  colors <- get_color_scheme(assay_config)
  theme <- get_plot_theme(assay_config)
  
  ggplot(data = standards_data %>% filter(!is.na(concentration)),
         aes(x = concentration, y = .data[[response_var]], color = high_variability)) +
    geom_point(aes(text = paste0(
      "Conc.: ", scales::scientific(concentration, digits = 2), "<br>",
      labels$y_unit, ": ", .data[[response_var]], "<br>",
      "Variability: ", high_variability
    ))) +
    geom_line(data = model_fits, aes(x = conc, y = p), color = "black", inherit.aes = FALSE) +
    scale_color_manual(
      name = NULL,
      values = c(
        "Normal Variability" = colors$standard,
        "High Variability" = colors$high_variability
      )
    ) +
    scale_x_log10(
      limits = PLOT_CONFIG$x_limits,
      name = labels$x_label,
      breaks = PLOT_CONFIG$x_breaks,
      labels = scales::scientific_format(digits = 2)
    ) +
    ylab(labels$y_label) +
    ggtitle("Dose Response Curve") +
    theme
}

#' Create standards variability boxplot
#' @param standards_data Data frame with standards
#' @param assay_config Assay configuration
#' @param response_var Name of response variable column
#' @return ggplot2 object  
create_standards_boxplot <- function(standards_data, assay_config, response_var = "MeasurementValue") {
  
  labels <- get_axis_labels(assay_config)
  colors <- get_color_scheme(assay_config)
  theme <- get_plot_theme(assay_config)
  
  ggplot(data = standards_data %>% filter(!is.na(concentration)),
         aes(x = concentration, y = .data[[response_var]])) +
    geom_boxplot(aes(group = concentration), outlier.shape = NA) +
    geom_jitter(aes(color = high_variability,
                   text = paste0(
                     "Conc.: ", concentration, "<br>",
                     labels$y_unit, ": ", .data[[response_var]], "<br>", 
                     "Variability: ", high_variability
                   )),
               width = 0.15, size = 1) +
    scale_color_manual(
      name = NULL,
      values = c(
        "Normal Variability" = colors$standard,
        "High Variability" = colors$high_variability  
      )
    ) +
    scale_x_log10(
      limits = PLOT_CONFIG$x_limits,
      name = labels$x_label,
      breaks = PLOT_CONFIG$x_breaks, 
      labels = scales::scientific_format(digits = 2)
    ) +
    labs(
      x = labels$x_label,
      y = labels$y_unit,
      title = "Standard Variability"
    ) +
    theme
}

#' Create residuals plot
#' @param fitted_vals Fitted values from model
#' @param residuals_vals Residual values from model
#' @return ggplot2 object
create_residuals_plot <- function(fitted_vals, residuals_vals) {
  
  ggplot(data = data.frame(fitted = fitted_vals, residuals = residuals_vals),
         aes(x = fitted, y = residuals)) +
    geom_point(aes(text = paste0(
      "Fitted: ", round(fitted, digits = 2), "<br>",
      "Residuals: ", round(residuals, digits = 2)
    )), color = "darkblue") +
    geom_hline(yintercept = 0, linetype = "dashed") +
    labs(
      x = "Fitted values",
      y = "Residuals", 
      title = "Fitted vs Residuals"
    ) +
    theme_classic() +
    theme(
      axis.title.x = ggtext::element_markdown(size = 12),
      axis.title.y = ggtext::element_markdown(size = 12),
      axis.text = ggtext::element_markdown(size = 12)
    )
}

#' Create unknown samples boxplot
#' @param unknown_results Data frame with sample results
#' @param assay_config Assay configuration
#' @return ggplot2 object
create_samples_boxplot <- function(unknown_results, assay_config) {
  
  labels <- get_axis_labels(assay_config)
  
  # Wrap long sample IDs
  unknown_results$sampleID_wrapped <- stringr::str_wrap(unknown_results$SampleID, width = 10)
  
  # Calculate dynamic margins
  max_label_len <- max(nchar(as.character(unknown_results$sampleID_wrapped)), na.rm = TRUE)
  dynamic_margin <- 10 + max_label_len * 2
  
  ggplot(
    data = unknown_results %>% filter(Dilution != "Missing"),
    aes(x = factor(sampleID_wrapped), y = estimated_concentration)
  ) +
    geom_boxplot() +
    geom_jitter(
      aes(color = Dilution,
          text = paste0(
            "Sample: ", sampleID_wrapped, "<br>",
            "Conc.: ", scales::scientific(estimated_concentration, digits = 2), "<br>",
            "Dilution: ", Dilution
          )),
      width = 0.15, size = 1.5
    ) +
    scale_y_log10(
      labels = scales::scientific_format(digits = 2),
      breaks = scales::trans_breaks("log10", function(x) 10^x)
    ) +
    scale_color_discrete(name = NULL) +
    xlab("Sample ID") +
    ylab(paste0("Estimated concentration [log(", 
               if (assay_config$assay_type == "elisa") assay_config$units %||% "pg/mL" else "mol", ")]")) +
    theme_classic() +
    theme(
      legend.title = element_blank(),
      axis.title.x = ggtext::element_markdown(size = 12),
      axis.title.y = ggtext::element_markdown(size = 12),
      axis.text.x = ggtext::element_markdown(size = 12, angle = 45, margin = margin(t = dynamic_margin)),
      axis.text.y = ggtext::element_markdown(size = 12),
      legend.text = ggtext::element_markdown(lineheight = 1.5, size = 12),
      legend.position = "top"
    )
}

#' Create combined dose-response curve with unknown samples
#' @param model_fits Model fitted curve data
#' @param unknown_results Unknown sample results
#' @param assay_config Assay configuration  
#' @param response_var Response variable name
#' @return ggplot2 object
create_combined_drc_plot <- function(model_fits, unknown_results, assay_config, response_var) {
  
  labels <- get_axis_labels(assay_config)
  colors <- get_color_scheme(assay_config)
  
  # Prepare unknown data for plotting
  unknown_plot_data <- unknown_results %>%
    filter(!is.na(estimated_concentration), is.finite(estimated_concentration))
  
  ggplot() +
    geom_line(data = model_fits, aes(x = conc, y = p), color = "black") +
    geom_point(
      data = unknown_plot_data,
      aes(x = estimated_concentration, y = Response, color = Dilution,
          text = paste0(
            "Conc.: ", scales::scientific(estimated_concentration, digits = 2), "<br>",
            labels$y_unit, ": ", Response, "<br>",
            "Status: ", Dilution
          )),
      size = 2
    ) +
    scale_color_manual(
      name = NULL,
      values = c(
        "Within Range" = colors$within_range,
        "Out of Range" = colors$out_of_range,
        "Missing" = "grey"
      ),
      breaks = c("Within Range", "Out of Range")  # Don't show Missing in legend
    ) +
    scale_x_log10(
      limits = PLOT_CONFIG$x_limits,
      breaks = PLOT_CONFIG$x_breaks,
      labels = scales::scientific_format(digits = 2)
    ) +
    labs(
      x = labels$x_label,
      y = labels$y_label,
      title = "Dose-Response Curve with Unknown Samples"
    ) +
    get_plot_theme(assay_config)
}

#' Make plot interactive if HTML output
#' @param plot ggplot2 object
#' @param tooltip Tooltip specification for plotly
#' @return plotly object (HTML) or ggplot2 object (PDF/Word)
make_interactive <- function(plot, tooltip = "text") {
  
  if (knitr::is_html_output()) {
    plotly::ggplotly(plot, tooltip = tooltip)
  } else {
    plot
  }
}

#' Create plot list for combined display
#' @param plots List of ggplot2 objects
#' @return htmltools::tagList (HTML) or prints plots (PDF/Word)
display_plots <- function(plots) {
  
  if (knitr::is_html_output()) {
    interactive_plots <- lapply(plots, make_interactive)
    htmltools::tagList(interactive_plots)
  } else {
    for (plot in plots) {
      print(plot)
    }
  }
}

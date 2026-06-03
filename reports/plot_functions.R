# ==============================================================================
# Table and Plot Functions
# Purpose: Standardized rendering functions for consistent report output
# ==============================================================================

# Source constants and core functions
if (!exists("TABLE_CONFIG")) {
  source("report_constants.R")
}

# ==============================================================================
# FORMAT-AWARE OUTPUT HELPERS
# Central helpers for HTML / DOCX / PDF format switching.
# All report templates should use these instead of scattering ad-hoc
# knitr::is_html_output() checks.
# ==============================================================================

#' Check if the current output format supports HTML widgets
#' @return TRUE for HTML output, FALSE for DOCX/PDF/other
is_html_out <- function() isTRUE(knitr::is_html_output())

#' Check if the current output format is Word (DOCX)
#' @return TRUE for DOCX output, FALSE otherwise
is_docx_out <- function() {
  fmt <- knitr::pandoc_to()
  !is.null(fmt) && fmt == "docx"
}

#' Render a ggplot object in a format-appropriate way
#'
#' HTML  -> interactive Plotly (with optional tooltip)
#' DOCX / PDF -> static ggplot print
#'
#' Important: the return value must be a **visible expression** in the calling
#' chunk so knitr's knit_print mechanism handles it.  Do NOT assign the result
#' to a variable — use  render_plot(...)  as a bare statement.
#'
#' @param gg A ggplot object
#' @param tooltip Character vector of aesthetics for Plotly tooltip (HTML only)
#' @param height Plotly height in pixels (HTML only, default NULL = auto)
#' @param plotly_layout Named list of additional plotly::layout() args (HTML only)
#' @return For HTML: an htmltools tagList (knitr renders as interactive widget).
#'         For DOCX/PDF: invisible NULL (static plot is a side-effect of print()).
render_plot <- function(gg, tooltip = NULL, height = NULL, plotly_layout = NULL,
                        compact = FALSE, plotly_postprocess = NULL) {
  # Apply the house theme per-call (AUDIT-011): avoids relying on the
  # process-global theme_set() that was removed from global.R, and works in
  # standalone Rmd renders where global.R is NOT sourced.
  if (exists("theme_rba", mode = "function")) gg <- gg + theme_rba()

  if (is_html_out()) {
    # Strip the ggplot title before conversion: plotly renders it inside the
    # figure frame which clashes with the surrounding Rmd section heading.
    gg <- gg + ggplot2::labs(title = NULL)

    p <- tryCatch({
      px <- if (!is.null(tooltip)) {
        plotly::ggplotly(gg, tooltip = tooltip, height = height)
      } else {
        plotly::ggplotly(gg, height = height)
      }
      # Apply default plotly layout: tighter margins and consistent legend.
      default_layout <- list(
        margin = list(t = 30, r = 20, b = 50, l = 60),
        legend = list(orientation = "h", x = 0, y = 1.08)
      )
      combined_layout <- utils::modifyList(default_layout, plotly_layout %||% list())
      px_laid <- do.call(plotly::layout, c(list(px), combined_layout))
      if (is.function(plotly_postprocess)) plotly_postprocess(px_laid) else px_laid
    }, error = function(e) {
      warning("render_plot: ggplotly() conversion failed (", conditionMessage(e),
              "). Falling back to static plot.")
      NULL
    })

    # Return the tagList — knitr's knit_print dispatches on the visible
    # return value, which correctly handles widget HTML + JS dependencies
    # in both results='asis' and default chunk modes.
    if (!is.null(p)) {
      htmltools::tagList(p)
    } else {
      print(gg)
      invisible(NULL)
    }
  } else {
    print(gg)
    invisible(NULL)
  }
}

#' Emit a collapsible section start (cat-based, for use inside R chunks)
#'
#' HTML  -> <details><summary>title</summary>  (or <details open> when default_open=TRUE)
#' DOCX / PDF -> ### title (plain markdown heading)
#'
#' @param title Section title text
#' @param heading_level Markdown heading level for non-HTML (default "###")
#' @param default_open If TRUE, render <details open> so the section starts expanded
section_start <- function(title, heading_level = "###", default_open = FALSE) {
  if (is_html_out()) {
    open_attr <- if (isTRUE(default_open)) " open" else ""
    cat(sprintf('\n<details%s>\n<summary>%s</summary>\n\n', open_attr, title))
  } else {
    cat(sprintf('\n%s %s\n\n', heading_level, title))
  }
}

#' Emit a collapsible section end (cat-based, for use inside R chunks)
#'
#' HTML  -> </details>
#' DOCX / PDF -> (no-op)
section_end <- function() {
  if (is_html_out()) {
    cat('\n</details>\n\n')
  }
}

#' Return collapsible section open tag (string-based, for inline R in markdown)
#'
#' @param title Section title text
#' @param heading_level Markdown heading level for non-HTML (default "###")
#' @param default_open If TRUE, render <details open> so the section starts expanded
#' @return A character string (HTML or markdown)
section_open <- function(title, heading_level = "###", default_open = FALSE) {
  if (is_html_out()) {
    open_attr <- if (isTRUE(default_open)) " open" else ""
    sprintf('<details%s>\n<summary>%s</summary>', open_attr, title)
  } else {
    sprintf('%s %s', heading_level, title)
  }
}

#' Return collapsible section close tag (string-based, for inline R in markdown)
#' @return HTML </details> or empty string
section_close <- function() {
  if (is_html_out()) '</details>' else ''
}

#' Emit an HTML heading or markdown heading, depending on output format
#'
#' HTML  -> <h4>title</h4>  (or specified level)
#' DOCX / PDF -> #### title  (markdown heading)
#'
#' @param title Heading text
#' @param level Integer heading level (default 4)
emit_heading <- function(title, level = 4L) {
  if (is_html_out()) {
    cat(sprintf("\n\n<h%d>%s</h%d>\n\n", level, title, level))
  } else {
    hashes <- paste(rep("#", level), collapse = "")
    cat(sprintf("\n\n%s %s\n\n", hashes, title))
  }
}

#' Emit inline-styled HTML block or plain markdown paragraph
#'
#' For styled executive-summary boxes, data-quality panels, etc.
#' HTML  -> <div style="..."> content </div>
#' DOCX / PDF -> plain markdown block (bold heading + body)
#'
#' @param content Character vector of lines (already formatted)
#' @param html_style CSS style string for the HTML wrapper
#' @param html_tag Tag name for HTML wrapper (default "div")
emit_styled_block <- function(content, html_style = NULL, html_tag = "div") {
  if (is_html_out() && !is.null(html_style)) {
    cat(sprintf('\n<%s style="%s">\n', html_tag, html_style))
    cat(paste(content, collapse = "\n"))
    cat(sprintf('\n</%s>\n\n', html_tag))
  } else {
    # Strip HTML tags and convert entities for plain markdown output
    plain <- gsub("<br\\s*/?>", "\n", paste(content, collapse = "\n"))
    plain <- gsub("<strong>|</strong>", "**", plain)
    plain <- gsub("<em>|</em>", "*", plain)
    plain <- gsub("<[^>]+>", "", plain)
    # Convert common HTML entities to Unicode
    plain <- gsub("&mdash;", "\u2014", plain)
    plain <- gsub("&ndash;", "\u2013", plain)
    plain <- gsub("&emsp;", " ", plain)
    plain <- gsub("&amp;", "&", plain)
    cat(plain)
    cat("\n\n")
  }
}

# ==============================================================================
# TABLE FUNCTIONS
#' Render table with consistent styling
#' @param data Data frame to display
#' @param caption Table caption
#' @param col_names Custom column names (optional)
#' @param digits Number of decimal places
#' @return Formatted table (HTML or Word/PDF)
render_table <- function(data, caption, col_names = NULL, digits = TABLE_CONFIG$digits,
                         escape = TRUE) {

  ncols <- ncol(data)

  if (knitr::is_html_output()) {
    tbl <- if (is.null(col_names)) {
      knitr::kable(data, format = "html", caption = caption, digits = digits,
                   row.names = FALSE, escape = escape)
    } else {
      knitr::kable(data, format = "html", caption = caption, col.names = col_names,
                   digits = digits, row.names = FALSE, escape = escape)
    }

    # For wide tables use full page width; narrower tables stay centered.
    # Every cell gets white-space:nowrap + a per-column min-width so content
    # can never be squeezed below a readable size.
    use_full_width <- ncols > 8
    col_min_px <- if (ncols > 10) "65px" else "75px"

    result <- tbl %>%
      kableExtra::kable_styling(
        bootstrap_options = c("striped", "hover", "responsive"),
        full_width = use_full_width,
        position = TABLE_CONFIG$position
      ) %>%
      kableExtra::column_spec(
        1:ncols,
        extra_css = paste0("white-space: nowrap; min-width: ", col_min_px, ";")
      )

    result

  } else if (is_docx_out()) {
    # Word output: sanitize | in character columns (pipe tables would otherwise
    # misparse | as a column separator, breaking the rendered table).
    # Also strip HTML tags so any HTML-formatted cells (e.g. status pills
    # when escape=FALSE is used for HTML output) render as plain text in Word.
    sanitize <- function(x) {
      x <- gsub("|",        "│", x, fixed = TRUE)  # pipe  -> │
      x <- gsub("<[^>]+>",  "",       x, perl  = TRUE)  # strip HTML tags
      x
    }
    data <- dplyr::mutate(data, dplyr::across(where(is.character), sanitize))
    if (!is.null(col_names)) col_names <- sanitize(col_names)

    # Return plain kable (no kableExtra): pandoc converts to a native Word table.
    if (is.null(col_names)) {
      knitr::kable(data, caption = caption, digits = digits, row.names = FALSE)
    } else {
      knitr::kable(data, caption = caption, col.names = col_names,
                   digits = digits, row.names = FALSE)
    }

  } else {
    # PDF (LaTeX) output:
    # kableExtra::kable_styling(full_width = TRUE) stretches the table and
    # column_spec() with width = "Xcm" sets guaranteed minimum widths.
    tbl <- if (is.null(col_names)) {
      knitr::kable(data, caption = caption, digits = digits, row.names = FALSE)
    } else {
      knitr::kable(data, caption = caption, col.names = col_names,
                   digits = digits, row.names = FALSE)
    }

    styled <- tbl %>%
      kableExtra::kable_styling(full_width = TRUE)

    # Widen the last two columns if the table is wide enough to have tissue data
    if (ncols >= 11) {
      styled <- styled %>%
        kableExtra::column_spec(ncols - 1, width = "2.2cm") %>%  # Tissue Mass [mg]
        kableExtra::column_spec(ncols,     width = "2.5cm")       # Conc. (pg/g)
    } else if (ncols >= 9) {
      # Narrow tables: just ensure the last column has room for numeric values
      styled <- styled %>%
        kableExtra::column_spec(ncols, width = "2.2cm")
    }

    styled
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
      
  } else if (is_docx_out()) {
    # Word output: use plain kable (pandoc converts to native Word table)
    qc_display <- qc_results[, c("Check", "Result")]
    qc_display$Status <- ifelse(qc_results$Pass, "\u2705 Pass", "\u26a0 Review")
    knitr::kable(
      qc_display,
      caption = "Quality Control Checks",
      align = c("l", "l", "l"),
      row.names = FALSE
    )
  } else {
    # PDF (LaTeX) output: use kable with kableExtra styling
    qc_display <- qc_results[, c("Check", "Result")]
    qc_display$Status <- ifelse(qc_results$Pass, "Pass", "Review")
    knitr::kable(
      qc_display,
      caption = "Quality Control Checks",
      align = c("l", "l", "l"),
      row.names = FALSE
    ) %>%
      kableExtra::kable_styling(full_width = FALSE)
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
create_dose_response_plot <- function(standards_data, model_fits, assay_config, response_var = "MeasurementValue", lang = "en") {

  labels <- get_axis_labels(assay_config, lang = lang)
  colors <- get_color_scheme(assay_config)
  theme <- get_plot_theme(assay_config)

  ggplot(data = standards_data %>% dplyr::filter(!is.na(concentration)),
         aes(x = concentration, y = .data[[response_var]], color = high_variability)) +
    geom_point(aes(text = paste0(
      "Conc.: ", scales::scientific(concentration, digits = 2), "<br>",
      labels$y_unit, ": ", .data[[response_var]], "<br>",
      "Variability: ", high_variability
    ))) +
    geom_line(data = model_fits, aes(x = conc, y = p), color = "black", inherit.aes = FALSE) +
    scale_color_manual(
      name = tr("legend_std_quality", lang),
      values = c(
        "Normal Variability" = colors$standard,
        "High Variability" = colors$high_variability
      ),
      labels = c(
        "Normal Variability" = tr("variability_normal", lang),
        "High Variability"   = tr("variability_high", lang)
      )
    ) +
    scale_x_log10(
      limits = PLOT_CONFIG$x_limits,
      name = labels$x_label,
      breaks = PLOT_CONFIG$x_breaks,
      labels = scales::scientific_format(digits = 2)
    ) +
    ylab(labels$y_label) +
    theme
}

#' Create standards variability boxplot
#' @param standards_data Data frame with standards
#' @param assay_config Assay configuration
#' @param response_var Name of response variable column
#' @return ggplot2 object  
create_standards_boxplot <- function(standards_data, assay_config, response_var = "MeasurementValue", lang = "en") {

  labels <- get_axis_labels(assay_config, lang = lang)
  colors <- get_color_scheme(assay_config)
  theme <- get_plot_theme(assay_config)

  ggplot(data = standards_data %>% dplyr::filter(!is.na(concentration)),
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
      name = tr("legend_std_quality", lang),
      values = c(
        "Normal Variability" = colors$standard,
        "High Variability" = colors$high_variability
      ),
      labels = c(
        "Normal Variability" = tr("variability_normal", lang),
        "High Variability"   = tr("variability_high", lang)
      )
    ) +
    scale_x_log10(
      limits = PLOT_CONFIG$x_limits,
      name = labels$x_label,
      breaks = PLOT_CONFIG$x_breaks,
      labels = scales::scientific_format(digits = 2)
    ) +
    labs(x = labels$x_label, y = labels$y_unit) +
    theme
}

#' Create residuals plot
#' @param fitted_vals Fitted values from model
#' @param residuals_vals Residual values from model
#' @return ggplot2 object
create_residuals_plot <- function(fitted_vals, residuals_vals, lang = "en") {

  ggplot(data = data.frame(fitted = fitted_vals, residuals = residuals_vals),
         aes(x = fitted, y = residuals)) +
    geom_point(aes(text = paste0(
      "Fitted: ", round(fitted, digits = 2), "<br>",
      "Residuals: ", round(residuals, digits = 2)
    )), color = "darkblue") +
    geom_hline(yintercept = 0, linetype = "dashed") +
    labs(x = tr("col_fitted_values", lang), y = tr("col_residuals", lang)) +
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
    data = unknown_results %>% dplyr::filter(Dilution != "Missing"),
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
create_combined_drc_plot <- function(model_fits, unknown_results, assay_config, response_var, lang = "en") {

  labels <- get_axis_labels(assay_config, lang = lang)
  colors <- get_color_scheme(assay_config)
  
  # Prepare unknown data for plotting
  unknown_plot_data <- unknown_results %>%
    dplyr::filter(!is.na(estimated_concentration), is.finite(estimated_concentration))
  
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
    labs(x = labels$x_label, y = labels$y_label) +
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

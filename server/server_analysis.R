server_analysis <- function(input, output, session, shared) {

  # --------------------------------------------------------------------------
  # A4 — KPI strip reactive on last_model_stats (populated after render)
  # --------------------------------------------------------------------------
  output$analysis_kpi_strip <- renderUI({
    lang  <- input$app_language %||% "en"
    stats <- shared$rv$last_model_stats

    # Helper: format value or dash
    fmt_or_dash <- function(x, fn = identity) {
      if (is.null(x) || length(x) != 1L || !is.numeric(x) || is.na(x) || !is.finite(x)) return("—")
      tryCatch(fn(x), error = function(e) "—")
    }

    # Helper: QC classification for R2
    r2_role <- function(r2) {
      if (is.null(r2) || length(r2) != 1L || !is.numeric(r2) || is.na(r2) || !is.finite(r2)) return(NULL)
      if (r2 >= 0.99) "pass" else if (r2 >= 0.95) "warn" else "fail"
    }

    kpi_tile <- function(label, value, unit = "", role = NULL, empty = FALSE) {
      tile_cls <- paste0("bs-kpi-tile",
                         if (!is.null(role)) paste0(" is-", role) else "",
                         if (empty) " is-empty" else "")
      tags$div(
        class = tile_cls,
        tags$div(class = "bs-kpi-label", label),
        tags$div(class = "bs-kpi-value", value),
        if (nzchar(unit)) tags$div(class = "bs-kpi-unit", unit)
      )
    }

    if (is.null(stats)) {
      # Placeholder strip — no fit yet
      hint_tile <- tags$div(
        class = "bs-kpi-tile is-empty",
        style = "font-size: 12px; color: var(--c-ink-3); align-self: center; padding: 12px 14px; font-style: italic;",
        tr("kpi_run_hint", lang)
      )
      return(tags$div(
        class = "bs-kpi-strip",
        kpi_tile("R² · curve fit", "—", empty = TRUE),
        kpi_tile("RMSE",     "—", empty = TRUE),
        kpi_tile("Cal. low", "—", empty = TRUE),
        kpi_tile("Cal. high", "—", empty = TRUE),
        kpi_tile("Overall",  "—", empty = TRUE),
        hint_tile
      ))
    }

    r2   <- stats$r_squared
    rmse <- stats$rmse
    role <- r2_role(r2)
    fmt_r2   <- fmt_or_dash(r2,   function(v) sprintf("%.4f", v))
    fmt_rmse <- fmt_or_dash(rmse, function(v) sprintf("%.3f", v))

    # Cal. low / Cal. high from stored std range
    std_range <- shared$rv$last_std_range
    fmt_low  <- if (!is.null(std_range)) format(std_range$min, scientific = TRUE, digits = 3) else "—"
    fmt_high <- if (!is.null(std_range)) format(std_range$max, scientific = TRUE, digits = 3) else "—"
    conc_unit <- if (isTRUE(input$assay_type == "elisa")) {
      input$elisa_units %||% "pg/mL"
    } else {
      "mol/L"
    }

    overall_word <- switch(role %||% "",
      pass = "Pass", warn = "Qualified", fail = "Review", "Unknown")
    overall_role <- role

    tags$div(
      class = "bs-kpi-strip",
      kpi_tile("R² · curve fit", fmt_r2),
      kpi_tile("RMSE",     fmt_rmse),
      kpi_tile("Cal. low",  fmt_low,  conc_unit),
      kpi_tile("Cal. high", fmt_high, conc_unit),
      tags$div(
        class = paste0("bs-kpi-tile", if (!is.null(overall_role)) paste0(" is-", overall_role) else ""),
        tags$div(class = "bs-kpi-label", "Overall"),
        tags$div(style = "margin-top: 4px;",
                 tags$span(class = paste0("bs-status-pill",
                                          if (!is.null(overall_role)) paste0(" is-", overall_role) else ""),
                           overall_word))
      )
    )
  })

  output$weighting_preview <- renderPlot({
    weights <- input$regression_weight
    if (is.null(weights) || length(weights) == 0) return(NULL)

    # Synthetic 4PL standards — fixed, not user data
    log_x <- seq(-10, -5, length.out = 8)
    x     <- 10 ^ log_x
    # 4PL with bottom=5, top=95, ec50=1e-7, hill=1
    y_true <- 5 + (95 - 5) / (1 + (x / 1e-7) ^ 1)
    set.seed(42)
    # heteroscedastic noise (variance grows with y)
    y_obs <- y_true + rnorm(8, 0, sd = y_true * 0.05)
    sd_per_point <- y_true * 0.05  # stable; computed before the weights loop

    # Color per weighting (matches the four checkbox values)
    colors <- c(none = "#1f3a5f", inv_y = "#009E73",
                inv_y2 = "#D97757", auto = "#9966CC")

    par(mar = c(4, 4, 1.5, 1), cex.axis = 0.85, cex.lab = 0.9)
    plot(log_x, y_obs, pch = 19, cex = 1.1, col = "#444",
         xlab = "log(concentration)", ylab = "%B/B0",
         ylim = c(0, 100), bty = "n")
    arrows(x0 = log_x, y0 = y_obs - sd_per_point,
           x1 = log_x, y1 = y_obs + sd_per_point,
           code = 3, angle = 90, length = 0.04, col = "#888", lwd = 1.2)

    # Draw one fitted curve per ticked weighting.
    # Slight shift per weighting shows that the choice changes the fit.
    fine_x <- seq(min(log_x), max(log_x), length.out = 200)
    for (w in weights) {
      shift <- switch(w,
                      none   = c(ec50 = 1.0e-7, hill = 1.00),
                      inv_y  = c(ec50 = 1.1e-7, hill = 1.05),
                      inv_y2 = c(ec50 = 0.9e-7, hill = 1.15),
                      auto   = c(ec50 = 1.0e-7, hill = 1.10))
      fine_y <- 5 + (95 - 5) / (1 + (10 ^ fine_x / shift["ec50"]) ^ shift["hill"])
      lines(fine_x, fine_y, col = colors[[w]], lwd = 2)
    }
    legend("topright",
           legend = c("±1 SD bars (heteroscedastic noise, fixed seed)", weights),
           col    = c("#888888", colors[weights]),
           lty    = c(1, rep(1, length(weights))),
           lwd    = c(1.2, rep(2, length(weights))),
           bty    = "n", cex = 0.85)
  }, height = 280)
}

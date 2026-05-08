server_analysis <- function(input, output, session) {

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

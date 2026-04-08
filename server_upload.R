# ===========================================================================
# server_upload.R
# Upload-related server logic extracted from app.R
# Includes: plate heatmap, download handlers, visual plate selector,
#           file upload observer, visual import confirmation, clear upload,
#           and upload preview outputs.
# ===========================================================================

server_upload <- function(input, output, session, shared) {

  # --------------------------------------------------------------------------
  # Plate Heatmap Preview
  # --------------------------------------------------------------------------

  output$plate_heatmap <- plotly::renderPlotly({
    meas_mat <- shared$matrix_measresults()
    req(meas_mat)

    # Safe numeric conversion: data.frame -> matrix (column-by-column)
    num_mat <- matrix(NA_real_, nrow = PLATE_NROW, ncol = PLATE_NCOL)
    for (cc in 1:PLATE_NCOL) {
      num_mat[, cc] <- suppressWarnings(as.numeric(as.character(meas_mat[[cc]])))
    }
    rownames(num_mat) <- ROW_NAMES
    colnames(num_mat) <- COL_NAMES

    if (all(is.na(num_mat))) return(NULL)

    # Build hover text using cbind() for element-wise matrix indexing
    # (outer() passes vectorized args; plain matrix[vec, vec] creates a cross-product)
    hover_text <- outer(ROW_NAMES, COL_NAMES, function(r, c) {
      ri <- match(r, ROW_NAMES)
      ci <- match(c, COL_NAMES)
      vals <- num_mat[cbind(ri, ci)]
      paste0(r, c, "<br>Value: ", round(vals, 3))
    })

    plotly::plot_ly(
      z = num_mat[rev(seq_len(PLATE_NROW)), ],
      x = COL_NAMES,
      y = rev(ROW_NAMES),
      text = hover_text[rev(seq_len(PLATE_NROW)), ],
      hoverinfo = "text",
      type = "heatmap",
      colorscale = list(
        c(0, "#4575b4"),
        c(0.25, "#91bfdb"),
        c(0.5, "#fee090"),
        c(0.75, "#fc8d59"),
        c(1, "#d73027")
      ),
      showscale = TRUE
    ) %>%
      plotly::layout(
        xaxis = list(title = "Column", dtick = 1),
        yaxis = list(title = "Row", dtick = 1),
        margin = list(l = 40, r = 10, t = 10, b = 40)
      )
  })

  # --------------------------------------------------------------------------
  # Plate Heatmap Accessible Description
  # --------------------------------------------------------------------------

  output$plate_heatmap_description <- renderText({
    mat <- shared$matrix_measresults()
    req(mat)
    vals <- unlist(mat)
    n_filled <- sum(!is.na(vals) & vals != "")
    range_text <- if (any(!is.na(suppressWarnings(as.numeric(vals))))) {
      nums <- suppressWarnings(as.numeric(vals[!is.na(vals)]))
      nums <- nums[!is.na(nums)]
      paste0("Range: ", round(min(nums, na.rm = TRUE), 2), " to ", round(max(nums, na.rm = TRUE), 2))
    } else {
      "No numeric values"
    }
    paste0("Plate heatmap showing ", n_filled, " of 96 wells with data. ", range_text)
  })

  # --------------------------------------------------------------------------
  # Download Report Handler
  # --------------------------------------------------------------------------

  output$download_report <- downloadHandler(
    filename = function() {
      fmt <- input$export_formats[1] %||% "html"
      ext <- if (fmt == "docx") "docx" else "html"
      paste0("bioassay_report_", format(Sys.Date(), "%Y%m%d"), ".", ext)
    },
    content = function(file) {
      out_dir <- session$userData$output_dir
      # Find the most recent report file
      fmt <- input$export_formats[1] %||% "html"
      ext <- if (fmt == "docx") "docx" else "html"
      report_files <- list.files(out_dir, pattern = paste0("\\.", ext, "$"), full.names = TRUE)
      if (length(report_files) > 0) {
        # Get most recent
        newest <- report_files[which.max(file.info(report_files)$mtime)]
        file.copy(newest, file)
      } else {
        showNotification("No report found. Generate a report first.", type = "warning")
      }
    }
  )

  output$download_plate_template <- downloadHandler(
    filename = function() "plate_reader_template.csv",
    content = function(file) {
      file.copy("examples/plate_template.csv", file)
    }
  )

  # --------------------------------------------------------------------------
  # Visual Plate Selector (File Preview)
  # --------------------------------------------------------------------------

  # Reactive to store raw Excel content for preview (LOCAL to this module)

  # Task 11 & 12: Stable plate identity model with cached preview data.
  # - plate_registry: data.frame with stable plate_id, sheet, start_row,
  #   start_col, label, nrow, ncol (built once per file upload)
  # - exclusions: named list keyed by stable plate_id -> set of excluded
  #   well coordinates (survives re-renders)
  # - preview_cache: pre-built HTML for the file preview table (computed
  #   once per upload, not on every render)
  rv_file_preview <- reactiveValues(
    raw_data       = NULL,         # raw imported data (matrix)
    file_path      = NULL,         # datapath of the current upload
    plate_registry = NULL,         # data.frame of detected plates with stable IDs
    exclusions     = list(),       # named list keyed by plate_id -> vector of excluded well coords
    preview_cache  = NULL          # cached HTML tagList for file preview table
  )

  # ---------- Detection observer (runs ONCE per new file upload) ----------
  # Reads the file, detects plates, assigns stable IDs, and caches the

  # file-preview HTML.  Rendering outputs below read from cache only.
  observeEvent(input$upload_counts, {
    req(input$import_method == "visual")

    file_path <- input$upload_counts$datapath
    ext <- tools::file_ext(input$upload_counts$name)

    # Skip re-detection if the same file is already loaded
    if (!is.null(rv_file_preview$file_path) &&
        identical(rv_file_preview$file_path, file_path)) {
      return()
    }

    # Read raw file content
    raw <- tryCatch({
      if (ext %in% c("xlsx", "xls")) {
        suppressMessages(readxl::read_excel(file_path, col_names = FALSE, .name_repair = "minimal"))
      } else if (ext == "csv") {
        read.csv(file_path, header = FALSE, stringsAsFactors = FALSE)
      } else {
        read.table(file_path, header = FALSE, sep = "\t", stringsAsFactors = FALSE)
      }
    }, error = function(e) NULL)

    if (is.null(raw)) {
      rv_file_preview$raw_data       <- NULL
      rv_file_preview$file_path      <- file_path
      rv_file_preview$plate_registry <- NULL
      rv_file_preview$exclusions     <- list()
      rv_file_preview$preview_cache  <- NULL
      return()
    }

    # Store raw data and path
    rv_file_preview$raw_data  <- raw
    rv_file_preview$file_path <- file_path

    # Clear stale exclusions from previous upload (BUG_005 fix)
    rv_file_preview$exclusions <- list()

    # ---- Auto-detect plate regions ----
    mat <- as.matrix(raw)
    registry_rows <- list()
    plate_seq <- 1

    for (i in 1:(nrow(mat) - 7)) {
      potential_rows <- trimws(as.character(mat[i:(i + 7), 1]))
      if (identical(potential_rows, LETTERS[1:8])) {
        test_data <- suppressWarnings(as.numeric(mat[i, 2:min(13, ncol(mat))]))
        num_valid <- sum(!is.na(test_data))
        if (num_valid >= 4) {
          # Try to find a wavelength label above the plate
          wl_label <- ""
          if (i >= 3) {
            for (look_back in 1:min(3, i - 1)) {
              above_text <- trimws(as.character(mat[i - look_back, 1]))
              if (grepl("Raw Data|\\d{3}", above_text)) {
                wl_label <- paste0(" - ", above_text)
                break
              }
            }
          }

          # Stable ID derived from position in file (not sequential index)
          stable_id <- paste0("sheet1_row", i, "_col2")

          registry_rows[[length(registry_rows) + 1]] <- data.frame(
            plate_id  = stable_id,
            sheet     = "sheet1",
            start_row = i,
            start_col = 2L,
            label     = paste0("Plate ", plate_seq, wl_label),
            nrows     = 8L,
            ncols     = min(12L, as.integer(num_valid)),
            stringsAsFactors = FALSE
          )
          plate_seq <- plate_seq + 1
        }
      }
    }

    if (length(registry_rows) > 0) {
      rv_file_preview$plate_registry <- do.call(rbind, registry_rows)
    } else {
      rv_file_preview$plate_registry <- NULL
    }

    # ---- Build and cache the file-preview HTML table ----
    registry <- rv_file_preview$plate_registry
    if (!is.null(registry) && nrow(registry) > 0) {
      plate_row_ranges <- lapply(seq_len(nrow(registry)), function(p) {
        registry$start_row[p]:(registry$start_row[p] + registry$nrows[p] - 1)
      })
      plate_colors <- c("#E3F2FD", "#FFF3E0", "#E8F5E9", "#FCE4EC", "#F3E5F5", "#E0F7FA")

      n_preview_rows <- min(nrow(mat), max(unlist(plate_row_ranges)) + 2)
      n_preview_cols <- min(ncol(mat), 14)

      preview_rows <- lapply(1:n_preview_rows, function(r) {
        bg <- "transparent"
        for (p_idx in seq_along(plate_row_ranges)) {
          if (r %in% plate_row_ranges[[p_idx]]) {
            bg <- plate_colors[(p_idx - 1) %% length(plate_colors) + 1]
            break
          }
        }
        cells <- lapply(1:n_preview_cols, function(cc) {
          val <- if (cc <= ncol(mat)) as.character(mat[r, cc]) else ""
          if (is.na(val)) val <- ""
          tags$td(style = paste0("padding:2px 6px; font-size:11px; border:1px solid #ddd; background:", bg),
                  val)
        })
        tags$tr(cells)
      })

      rv_file_preview$preview_cache <- tags$div(
        style = "max-height: 300px; overflow: auto; border: 1px solid #ccc; margin: 10px 0; border-radius: 4px;",
        tags$table(
          style = "border-collapse: collapse; width: 100%;",
          preview_rows
        )
      )
    } else {
      rv_file_preview$preview_cache <- NULL
    }
  }, priority = 10)  # high priority so detection runs before renders

  # ---------- Visual file preview: renders from cached data ----------
  output$visual_file_preview <- renderUI({
    req(input$upload_counts)
    req(input$import_method == "visual")
    lang <- input$app_language %||% "en"

    # Wait for detection to populate cache
    registry <- rv_file_preview$plate_registry

    if (is.null(rv_file_preview$raw_data)) {
      return(tags$p(style = "color: red;", "Could not read file for preview."))
    }

    if (is.null(registry) || nrow(registry) == 0) {
      return(tags$p(style = "color: orange;",
                    "No 8\u00D712 plate regions auto-detected. Use Classic Import, or check file format."))
    }

    plate_colors <- c("#E3F2FD", "#FFF3E0", "#E8F5E9", "#FCE4EC", "#F3E5F5", "#E0F7FA")

    tagList(
      tags$p(style = "color: green; font-weight: bold;",
             sprintf("\u2705 %d plate region(s) detected.", nrow(registry))),

      # Cached file preview table
      rv_file_preview$preview_cache,

      # Plate checkboxes (keyed by stable plate_id)
      tags$div(
        style = "margin: 10px 0;",
        lapply(seq_len(nrow(registry)), function(idx) {
          pl <- registry[idx, ]
          bg <- plate_colors[(idx - 1) %% length(plate_colors) + 1]
          div(
            style = paste0("display: inline-flex; align-items: center; gap: 8px; margin: 5px 10px; ",
                          "padding: 4px 10px; border-radius: 4px; background:", bg, ";"),
            checkboxInput(paste0("select_plate_", pl$plate_id),
                         pl$label, value = TRUE),
            tags$small(sprintf("(%d\u00D7%d)", pl$nrows, pl$ncols))
          )
        })
      ),
      tags$p(tags$small(style = "color: #666;",
                        "Uncheck plates you do not want to analyze.")),

      # Well grid section for each selected plate
      uiOutput("visual_well_grids"),

      # Confirm button
      actionButton("confirm_visual_import",
                   tr("confirm_selection", lang),
                   class = "btn btn-success",
                   style = "width: 100%; margin-top: 10px; font-size: 16px; padding: 10px;")
    )
  })

  # ---------- Render interactive well grids from cached plate data ----------
  output$visual_well_grids <- renderUI({
    req(rv_file_preview$plate_registry)
    lang <- input$app_language %||% "en"
    registry <- rv_file_preview$plate_registry
    raw <- rv_file_preview$raw_data
    if (is.null(raw) || is.null(registry) || nrow(registry) == 0) return(NULL)

    mat <- as.matrix(raw)
    plate_colors <- c("#E3F2FD", "#FFF3E0", "#E8F5E9", "#FCE4EC", "#F3E5F5", "#E0F7FA")

    plate_grids <- lapply(seq_len(nrow(registry)), function(idx) {
      pl <- registry[idx, ]
      plate_id <- pl$plate_id

      # Check if this plate is selected (checkbox keyed by stable plate_id)
      checkbox_val <- input[[paste0("select_plate_", plate_id)]]
      if (!isTRUE(checkbox_val)) return(NULL)

      bg <- plate_colors[(idx - 1) %% length(plate_colors) + 1]

      # Extract the plate data from cached raw matrix
      plate_data <- mat[pl$start_row:(pl$start_row + pl$nrows - 1),
                        pl$start_col:(pl$start_col + pl$ncols - 1), drop = FALSE]

      # Current exclusions for this plate
      plate_excl <- rv_file_preview$exclusions[[plate_id]]
      if (is.null(plate_excl)) plate_excl <- character(0)

      # Build an 8xncol interactive grid
      grid_rows <- lapply(1:8, function(r) {
        row_letter <- LETTERS[r]
        cells <- lapply(1:pl$ncols, function(cc) {
          well_id <- paste0(row_letter, cc)
          val <- tryCatch(as.character(plate_data[r, cc]), error = function(e) "")
          if (is.na(val)) val <- ""
          num_val <- suppressWarnings(as.numeric(val))
          display_val <- if (!is.na(num_val)) round(num_val, 3) else val

          # Check if this well is excluded (keyed by stable plate_id)
          is_excluded <- well_id %in% plate_excl

          cell_style <- if (is_excluded) {
            "padding:3px 5px; font-size:10px; border:1px solid #ccc; cursor:pointer; background:#ffcdd2; color:#999; text-decoration:line-through; text-align:center; min-width:48px;"
          } else {
            paste0("padding:3px 5px; font-size:10px; border:1px solid #ccc; cursor:pointer; background:", bg, "; text-align:center; min-width:48px;")
          }

          tags$td(
            style = cell_style,
            onclick = sprintf("Shiny.setInputValue('toggle_well', {plate_id: '%s', well: '%s', ts: Date.now()});",
                             plate_id, well_id),
            title = paste0(well_id, if (is_excluded) " (excluded)" else ""),
            display_val
          )
        })
        tags$tr(
          tags$td(style = "padding:3px 5px; font-weight:bold; font-size:11px; text-align:center;", row_letter),
          cells
        )
      })

      # Column headers
      col_header <- tags$tr(
        tags$th(style = "padding:3px 5px; font-size:10px;", ""),
        lapply(1:pl$ncols, function(cc) {
          tags$th(style = "padding:3px 5px; font-size:10px; text-align:center;", cc)
        })
      )

      div(
        style = "margin: 10px 0;",
        tags$b(pl$label),
        tags$small(style = "color: #666; margin-left: 10px;",
                   tr("excluded_wells_label", lang)),
        tags$div(
          style = "overflow-x: auto; margin-top: 5px;",
          tags$table(
            style = "border-collapse: collapse;",
            tags$thead(col_header),
            tags$tbody(grid_rows)
          )
        )
      )
    })

    tagList(
      hr(),
      tags$b(style = "font-size: 14px;", "Step 2: ", tr("excluded_wells_label", lang)),
      plate_grids
    )
  })

  # Handle well toggle clicks from JavaScript (keyed by stable plate_id)
  observeEvent(input$toggle_well, {
    info <- input$toggle_well
    if (is.null(info)) return()
    plate_id <- info$plate_id
    well <- info$well

    current_excl <- rv_file_preview$exclusions[[plate_id]]
    if (is.null(current_excl)) current_excl <- character(0)

    if (well %in% current_excl) {
      # Remove from exclusions
      rv_file_preview$exclusions[[plate_id]] <- setdiff(current_excl, well)
    } else {
      # Add to exclusions
      rv_file_preview$exclusions[[plate_id]] <- c(current_excl, well)
    }
  })

  # --------------------------------------------------------------------------
  # FILE UPLOAD (Smart Import)
  # --------------------------------------------------------------------------

  observeEvent(input$upload_counts, {
    req(input$upload_counts)

    file_path <- input$upload_counts$datapath
    import_mode <- input$import_method %||% "classic"

    if (import_mode == "visual") {
      # Visual mode: just show the preview, don't auto-import
      # The visual preview UI is rendered reactively above
      showNotification("File loaded. Select plate regions in the Visual Plate Selector.",
                      type = "message", duration = 5)
      return()
    }

    # Classic import mode — single-pass pipeline
    result <- tryCatch(
      parse_plate_file(file_path),
      error = function(e) {
        showNotification(paste("Import failed:", e$message), type = "error", duration = 15)
        return(NULL)
      }
    )

    req(!is.null(result))

    shared$rv$is_multiwavelength <- result$is_multiwavelength
    shared$rv$wavelengths        <- result$wavelengths
    shared$rv$wavelength_plates  <- if (result$is_multiwavelength) result$plates else NULL
    shared$matrix_measresults(result$plates[[1]])

    # Unified notification
    wave_msg <- if (result$is_multiwavelength) {
      paste0("Wavelengths: ", paste(result$wavelengths, collapse = ", "), "\n")
    } else {
      ""
    }
    showNotification(
      sprintf("\u2705 Imported: %s\n%s%d wells detected\nFormat: %s%s",
              result$import_info$file,
              wave_msg,
              result$detected_wells,
              result$format,
              if (result$partial_plate) " (partial)" else ""),
      type = "message",
      duration = if (result$is_multiwavelength) 7 else 5
    )
  })

  # --------------------------------------------------------------------------
  # Visual Import Confirmation
  # --------------------------------------------------------------------------

  # Handle visual import confirmation
  observeEvent(input$confirm_visual_import, {
    req(rv_file_preview$raw_data)
    registry <- rv_file_preview$plate_registry
    req(!is.null(registry), nrow(registry) > 0)
    lang <- input$app_language %||% "en"

    raw <- rv_file_preview$raw_data

    # Determine which plates are selected (checkboxes keyed by stable plate_id)
    selected_indices <- c()
    for (idx in seq_len(nrow(registry))) {
      checkbox_val <- input[[paste0("select_plate_", registry$plate_id[idx])]]
      if (isTRUE(checkbox_val)) {
        selected_indices <- c(selected_indices, idx)
      }
    }

    if (length(selected_indices) == 0) {
      showNotification("Please select at least one plate.", type = "warning")
      return()
    }

    # Helper: apply well exclusions to a plate using stable plate_id
    apply_exclusions <- function(plate_numeric, plate_id) {
      excluded <- rv_file_preview$exclusions[[plate_id]]
      if (length(excluded) > 0) {
        for (well_id in excluded) {
          r <- match(substr(well_id, 1, 1), LETTERS)
          cc <- as.integer(sub("^[A-H]", "", well_id))
          if (!is.na(r) && !is.na(cc) && r <= nrow(plate_numeric) && cc <= ncol(plate_numeric)) {
            plate_numeric[r, cc] <- NA_real_
          }
        }
      }
      plate_numeric
    }

    # Helper: extract and convert a plate from raw matrix
    extract_plate <- function(reg_row) {
      plate_data <- raw[reg_row$start_row:(reg_row$start_row + reg_row$nrows - 1),
                        reg_row$start_col:(reg_row$start_col + reg_row$ncols - 1), drop = FALSE]
      plate_numeric <- suppressWarnings(
        as.data.frame(apply(plate_data, 2, as.numeric), stringsAsFactors = FALSE)
      )
      enforce_plate_shape(plate_numeric)
    }

    sel_registry <- registry[selected_indices, , drop = FALSE]

    if (nrow(sel_registry) == 1) {
      shared$rv$is_multiwavelength <- FALSE
      reg_row <- sel_registry[1, ]
      plate_numeric <- extract_plate(reg_row)
      plate_numeric <- apply_exclusions(plate_numeric, reg_row$plate_id)
      shared$matrix_measresults(plate_numeric)

      n_excluded <- length(rv_file_preview$exclusions[[reg_row$plate_id]])
      msg <- "\u2705 Single plate imported from visual selection."
      if (n_excluded > 0) msg <- paste0(msg, sprintf(" (%d wells excluded)", n_excluded))
      showNotification(msg, type = "message")

    } else {
      shared$rv$is_multiwavelength <- TRUE
      plates <- list()
      for (i in seq_len(nrow(sel_registry))) {
        reg_row <- sel_registry[i, ]
        plate_numeric <- extract_plate(reg_row)
        plate_numeric <- apply_exclusions(plate_numeric, reg_row$plate_id)
        plate_label <- if (!is.null(reg_row$label) && nzchar(reg_row$label)) {
          reg_row$label
        } else {
          paste0("Plate_", i)
        }
        plates[[plate_label]] <- plate_numeric
      }

      shared$rv$wavelength_plates <- plates
      shared$rv$wavelengths <- names(plates)
      shared$matrix_measresults(plates[[1]])

      showNotification(
        sprintf("\u2705 %d plates imported from visual selection.", length(plates)),
        type = "message"
      )
    }
  })

  # --------------------------------------------------------------------------
  # Clear Upload
  # --------------------------------------------------------------------------

  observeEvent(input$clear_upload, {
    shinyjs::reset("upload_counts")
    shared$matrix_measresults(create_plate_matrix())
    showNotification("File cleared", type = "message")
  })

  # --------------------------------------------------------------------------
  # Upload Preview Outputs
  # --------------------------------------------------------------------------

  # Upload preview
  output$upload_summary <- renderUI({
    req(shared$matrix_measresults())
    plate <- shared$matrix_measresults()
    info <- base::attr(plate, "import_info")  # Use base R attr() to avoid xfun warning

    if (!is.null(info)) {
      # Check actual data - count non-NA wells
      actual_wells <- sum(!is.na(plate))
      is_partial <- actual_wells < 96  # True partial plate check

      div(
        style = "background-color: #E8F5E9; padding: 10px; margin: 10px 0; border-left: 4px solid #4CAF50;",
        tags$b("Import Summary:"),
        tags$ul(
          tags$li(sprintf("Format: %s", info$format)),
          tags$li(sprintf("Wells: %d / 96", actual_wells)),
          tags$li(sprintf("Partial: %s", if (is_partial) "Yes" else "No"))
        )
      )
    }
  })

  output$meas_preview <- renderTable({
    req(shared$matrix_measresults())
    head(shared$matrix_measresults(), 3)
  })

  # --------------------------------------------------------------------------
  # Sample Layout Modal
  # --------------------------------------------------------------------------

  observeEvent(input$show_sample_layout, {
    dot <- "<span style='color:#999;'>\u2022</span>"
    sample_df <- data.frame(
      Row = LETTERS[1:8],
      matrix(rep(dot, 96), nrow = 8, ncol = 12),
      stringsAsFactors = FALSE
    )

    showModal(modalDialog(
      title = "Sample Plate Layout",
      size = "l",
      easyClose = TRUE,
      HTML("<p>Expected: Row labels (A\u2013H) + 12 numeric columns.<br>
            Do not include column names.</p>"),
      HTML("<style>.sample_table thead { display: none; }</style>"),
      HTML(knitr::kable(sample_df, format = "html",
                       table.attr = "class='table table-bordered'",
                       escape = FALSE))
    ))
  })

}

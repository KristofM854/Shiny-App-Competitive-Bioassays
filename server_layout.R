# server_layout.R
# Server logic for plate layout management: preset loading, matrix rendering,
# matrix change observers, reset buttons, layout save/load/import, and
# tissue weight table.

server_layout <- function(input, output, session, shared) {

  # --------------------------------------------------------------------------
  # Preset Plate Layouts
  # --------------------------------------------------------------------------

  observeEvent(input$preset_layout, {
    req(input$preset_layout != "")
    preset_file <- file.path("presets", paste0(input$preset_layout, ".rds"))
    if (file.exists(preset_file)) {
      preset <- readRDS(preset_file)
      if (!is.null(preset$type_matrix)) shared$matrix_type(preset$type_matrix)
      if (!is.null(preset$id_matrix)) shared$matrix_id(preset$id_matrix)
      if (!is.null(preset$dilution_matrix)) shared$matrix_dilution(preset$dilution_matrix)
      if (!is.null(preset$replicate_matrix)) shared$matrix_replicate(preset$replicate_matrix)
      showNotification("Preset layout loaded successfully.", type = "message", duration = 3)
    } else {
      showNotification("Preset file not found.", type = "warning", duration = 3)
    }
    # Reset dropdown to allow re-selection
    updateSelectInput(session, "preset_layout", selected = "")
  })

  # --------------------------------------------------------------------------
  # Uniform Dilution Shortcut
  # --------------------------------------------------------------------------

  observeEvent(input$apply_uniform_dilution, {
    val <- input$uniform_dilution
    req(is.numeric(val), val > 0)
    new_dil <- matrix(val, nrow = PLATE_NROW, ncol = PLATE_NCOL)
    rownames(new_dil) <- ROW_NAMES
    colnames(new_dil) <- COL_NAMES
    shared$matrix_dilution(new_dil)
    # Also update raw dilution strings
    raw_dil <- matrix(as.character(val), nrow = PLATE_NROW, ncol = PLATE_NCOL)
    rownames(raw_dil) <- ROW_NAMES
    colnames(raw_dil) <- COL_NAMES
    shared$raw_matrix_dilution(raw_dil)
    shared$dilution_validity(matrix(TRUE, nrow = PLATE_NROW, ncol = PLATE_NCOL))
    shared$dilution_error(FALSE)
    showNotification(paste("All dilutions set to", val), type = "message", duration = 2)
  })

  # --------------------------------------------------------------------------
  # Auto-fill ID Matrix from Type Matrix
  # --------------------------------------------------------------------------

  observeEvent(shared$matrix_type(), {
    type_mat <- shared$matrix_type()
    req(type_mat)
    id_mat <- shared$matrix_id()
    if (is.null(id_mat)) return()

    # Auto-generate standard IDs: all Standard wells in the same row
    # get the same S{n} label (replicates of one concentration level)
    std_row_count <- 0
    changed <- FALSE

    for (r in 1:nrow(type_mat)) {
      row_has_standard <- FALSE
      for (c in 1:ncol(type_mat)) {
        if (!is.na(type_mat[r, c]) && type_mat[r, c] == "Standard") {
          row_has_standard <- TRUE
          break
        }
      }

      if (row_has_standard) {
        std_row_count <- std_row_count + 1
        new_id <- paste0("S", std_row_count)

        for (c in 1:ncol(type_mat)) {
          if (!is.na(type_mat[r, c]) && type_mat[r, c] == "Standard") {
            # Only auto-fill if ID is empty or matches default S-pattern
            if (is.na(id_mat[r, c]) || id_mat[r, c] == "" || grepl("^S[0-9]+$", id_mat[r, c])) {
              id_mat[r, c] <- new_id
              changed <- TRUE
            }
          }
        }
      }
    }

    if (changed) shared$matrix_id(id_mat)
  }, ignoreInit = TRUE)

  # --------------------------------------------------------------------------
  # Matrix Renderers
  # --------------------------------------------------------------------------

  output$matrix_type <- renderRHandsontable({
    req(shared$matrix_type())
    mat <- shared$matrix_type()
    mat[] <- lapply(mat, as.character)

    # Dynamic dropdown based on assay type
    type_choices <- if (input$assay_type == "elisa") {
      c("Standard", "Sample", "QC", "Blank", "NSB", "B0", "TotalActivity", "Other")
    } else {
      c("Standard", "Sample", "QC", "Blank", "Other")
    }

    rhandsontable(mat, rowHeaderWidth = 30, rowHeaders = ROW_NAMES,
                  height = 250, stretchH = "all", overflow = "hidden") %>%
      hot_col(col = 1:12, type = "dropdown",
              source = type_choices) %>%
      hot_col(col = 1:12, renderer = "
        function(instance, td, row, col, prop, value, cellProperties) {
          Handsontable.renderers.TextRenderer.apply(this, arguments);
          var colors = {
            'Standard': '#90EE90',
            'QC': '#ADD8E6',
            'Blank': '#FFE4E1',
            'NSB': '#FFDAB9',
            'B0': '#F0E68C',
            'TotalActivity': '#DDA0DD',
            'Sample': '#E6E6FA',
            'Other': '#FFD700'
          };
          td.style.backgroundColor = colors[value] || '#FFFFFF';
        }")
  })

  output$matrix_id <- renderRHandsontable({
    req(shared$matrix_id())
    mat <- shared$matrix_id()
    mat[] <- lapply(mat, as.character)
    rhandsontable(mat, rowHeaderWidth = 30, rowHeaders = ROW_NAMES,
                  height = 250, stretchH = "all", overflow = "hidden")
  })

  output$matrix_dilution <- renderRHandsontable({
    req(shared$raw_matrix_dilution())
    df <- shared$raw_matrix_dilution()
    val <- shared$dilution_validity()

    rhandsontable(df, rowHeaderWidth = 30, rowHeaders = ROW_NAMES,
                  height = 250, stretchH = "all", overflow = "hidden") %>%
      hot_col(col = 1:12, type = "text",
              renderer = htmlwidgets::JS(sprintf(
                "function(instance, td, row, col, prop, value, cellProperties) {
                  Handsontable.renderers.TextRenderer.apply(this, arguments);
                  var valid = %s;
                  if (!valid[row][col]) td.style.background = '#F8D7DA';
                  else td.style.background = '';
                }", jsonlite::toJSON(val))))
  })

  output$matrix_replicate <- renderRHandsontable({
    mat <- shared$matrix_replicate()
    mat[] <- lapply(mat, as.character)
    rhandsontable(mat, rowHeaderWidth = 30, rowHeaders = ROW_NAMES,
                  height = 250, stretchH = "all", overflow = "hidden")
  })

  # --------------------------------------------------------------------------
  # Matrix Observers
  # --------------------------------------------------------------------------

  observeEvent(input$matrix_type, {
    if (!is.null(input$matrix_type)) {
      shared$matrix_type(hot_to_r(input$matrix_type))
    }
  })

  observeEvent(input$matrix_id, {
    if (!is.null(input$matrix_id)) {
      mat <- hot_to_r(input$matrix_id)
      mat[] <- lapply(mat, as.character)
      shared$matrix_id(mat)
    }
  })

  observeEvent(input$matrix_replicate, {
    if (!is.null(input$matrix_replicate)) {
      mat <- hot_to_r(input$matrix_replicate)
      mat[] <- lapply(mat, as.character)
      shared$matrix_replicate(mat)
    }
  })

  matrix_dilution_debounced <- reactive({ input$matrix_dilution }) %>% debounce(300)
  observeEvent(matrix_dilution_debounced(), {
    req(input$matrix_dilution)

    raw <- hot_to_r(input$matrix_dilution)
    raw <- enforce_plate_shape(raw)
    shared$raw_matrix_dilution(raw)

    # Parse all cells at once using vectorized parse_dilution_matrix()
    result <- parse_dilution_matrix(raw)
    parsed <- result$values
    validity <- result$validity

    shared$matrix_dilution(enforce_plate_shape(as.data.frame(parsed)))
    shared$dilution_validity(validity)
    shared$dilution_error(any(!validity))
  })

  # --------------------------------------------------------------------------
  # Reset Buttons
  # --------------------------------------------------------------------------

  observeEvent(input$reset_type, {
    assay <- input$assay_type %||% "rba"
    n <- as.integer(input$num_standards %||% 8)
    shared$matrix_type(create_type_matrix(assay, n))
  })

  observeEvent(input$reset_id, {
    assay <- input$assay_type %||% "rba"
    n <- as.integer(input$num_standards %||% 8)
    shared$matrix_id(create_id_matrix(assay, n))
  })

  observeEvent(input$reset_dilution, {
    shared$raw_matrix_dilution(default_raw_dilution())
    shared$matrix_dilution(create_dilution_matrix())
    shared$dilution_validity(matrix(TRUE, nrow = 8, ncol = 12))
    shared$dilution_error(FALSE)
  })

  observeEvent(input$reset_replicate, {
    assay <- input$assay_type %||% "rba"
    shared$matrix_replicate(create_replicate_matrix(assay))
  })

  # --------------------------------------------------------------------------
  # Plate Layout Import / Save / Load
  # --------------------------------------------------------------------------

  # Directory for saved layouts
  layouts_dir <- file.path(path.expand("~"), ".bioassay_layouts")

  # Save current layout
  observeEvent(input$layout_save, {
    dir.create(layouts_dir, recursive = TRUE, showWarnings = FALSE)

    layout <- list(
      type = as.data.frame(shared$matrix_type()),
      id = as.data.frame(shared$matrix_id()),
      dilution = as.data.frame(shared$raw_matrix_dilution()),
      replicate = as.data.frame(shared$matrix_replicate()),
      assay_type = input$assay_type %||% "rba",
      num_standards = as.integer(input$num_standards),
      saved_at = Sys.time()
    )

    # Generate filename from assay type + timestamp
    fname <- paste0("layout_", input$assay_type, "_",
                    format(Sys.time(), "%Y%m%d_%H%M%S"), ".rds")
    saveRDS(layout, file.path(layouts_dir, fname))

    showNotification(tr("layout_saved_msg", input$app_language %||% "en"),
                     type = "message", duration = 3)
  })

  # Dynamic UI for load button (shows saved layouts)
  output$layout_load_ui <- renderUI({
    # Trigger refresh on save
    input$layout_save

    if (dir.exists(layouts_dir)) {
      saved_files <- list.files(layouts_dir, pattern = "\\.rds$", full.names = FALSE)
      if (length(saved_files) > 0) {
        # Show most recent first, display readable names
        saved_files <- rev(sort(saved_files))
        labels <- gsub("^layout_", "", gsub("\\.rds$", "", saved_files))
        labels <- gsub("_", " ", labels)
        names(saved_files) <- labels

        tagList(
          selectInput("layout_load_select", NULL,
                      choices = saved_files, width = "100%"),
          actionButton("layout_load", label = tagList(icon("folder-open"), "Load"),
                       class = "btn btn-info btn-sm", style = "width: 100%;")
        )
      } else {
        helpText(tr("layout_no_saved", input$app_language %||% "en"))
      }
    } else {
      helpText(tr("layout_no_saved", input$app_language %||% "en"))
    }
  })

  # Load saved layout
  observeEvent(input$layout_load, {
    req(input$layout_load_select)
    fpath <- file.path(layouts_dir, input$layout_load_select)

    if (file.exists(fpath)) {
      layout <- tryCatch(readRDS(fpath), error = function(e) NULL)
      if (!is.null(layout)) {
        if (!is.null(layout$type)) shared$matrix_type(enforce_plate_shape(layout$type))
        if (!is.null(layout$id)) shared$matrix_id(enforce_plate_shape(layout$id))
        if (!is.null(layout$dilution)) {
          shared$raw_matrix_dilution(enforce_plate_shape(layout$dilution))
          # Re-parse dilution values
          raw <- enforce_plate_shape(layout$dilution)
          result <- parse_dilution_matrix(raw)
          shared$matrix_dilution(enforce_plate_shape(as.data.frame(result$values)))
          shared$dilution_validity(result$validity)
          shared$dilution_error(any(!result$validity))
        }
        if (!is.null(layout$replicate)) shared$matrix_replicate(enforce_plate_shape(layout$replicate))

        showNotification(tr("layout_loaded_msg", input$app_language %||% "en"),
                         type = "message", duration = 3)
      }
    }
  })

  # Import layout from CSV/Excel file
  observeEvent(input$layout_import_file, {
    req(input$layout_import_file)
    fpath <- input$layout_import_file$datapath
    fname <- input$layout_import_file$name

    tryCatch({
      # Read file
      if (grepl("\\.(xlsx|xls)$", fname, ignore.case = TRUE)) {
        if (!requireNamespace("readxl", quietly = TRUE)) {
          showNotification("Package 'readxl' needed for Excel import. Install it with install.packages('readxl').",
                           type = "error", duration = 8)
          return()
        }
        sheets <- readxl::excel_sheets(fpath)

        # Expect sheets or columns: SampleType, SampleID, Dilution, Replicate
        import_sheet <- function(sheet_name) {
          if (sheet_name %in% sheets) {
            d <- as.data.frame(readxl::read_excel(fpath, sheet = sheet_name, col_names = TRUE))
            # Remove row names column if present
            if (ncol(d) > 12) d <- d[, 1:12]
            rownames(d) <- LETTERS[1:min(nrow(d), 8)]
            colnames(d) <- as.character(1:ncol(d))
            enforce_plate_shape(d)
          } else NULL
        }

        type_imported <- import_sheet("SampleType")
        id_imported <- import_sheet("SampleID")
        dil_imported <- import_sheet("Dilution")
        rep_imported <- import_sheet("Replicate")

      } else {
        # CSV: expect a specific format
        # Try reading as a multi-section CSV (sections separated by blank lines)
        raw_lines <- readLines(fpath, warn = FALSE)

        # Parse CSV sections (look for headers: SampleType, SampleID, Dilution, Replicate)
        sections <- list()
        current_section <- NULL
        current_lines <- c()

        for (line in raw_lines) {
          trimmed <- trimws(line)
          if (trimmed %in% c("SampleType", "SampleID", "Dilution", "Replicate")) {
            if (!is.null(current_section) && length(current_lines) > 0) {
              sections[[current_section]] <- current_lines
            }
            current_section <- trimmed
            current_lines <- c()
          } else if (nchar(trimmed) > 0 && !is.null(current_section)) {
            current_lines <- c(current_lines, line)
          }
        }
        if (!is.null(current_section) && length(current_lines) > 0) {
          sections[[current_section]] <- current_lines
        }

        parse_section <- function(lines) {
          if (length(lines) == 0) return(NULL)
          tc <- textConnection(paste(lines, collapse = "\n"))
          d <- tryCatch(read.csv(tc, header = FALSE, stringsAsFactors = FALSE),
                        error = function(e) NULL)
          close(tc)
          if (is.null(d)) return(NULL)
          if (ncol(d) > 12) d <- d[, 1:12]
          if (nrow(d) > 8) d <- d[1:8, ]
          rownames(d) <- LETTERS[1:nrow(d)]
          colnames(d) <- as.character(1:ncol(d))
          enforce_plate_shape(d)
        }

        type_imported <- parse_section(sections[["SampleType"]])
        id_imported <- parse_section(sections[["SampleID"]])
        dil_imported <- parse_section(sections[["Dilution"]])
        rep_imported <- parse_section(sections[["Replicate"]])
      }

      # Apply imported matrices
      if (!is.null(type_imported)) shared$matrix_type(type_imported)
      if (!is.null(id_imported)) shared$matrix_id(id_imported)
      if (!is.null(dil_imported)) {
        shared$raw_matrix_dilution(dil_imported)
        result <- parse_dilution_matrix(dil_imported)
        shared$matrix_dilution(enforce_plate_shape(as.data.frame(result$values)))
        shared$dilution_validity(result$validity)
        shared$dilution_error(any(!result$validity))
      }
      if (!is.null(rep_imported)) shared$matrix_replicate(rep_imported)

      showNotification(tr("layout_import_success", input$app_language %||% "en"),
                       type = "message", duration = 3)

    }, error = function(e) {
      showNotification(paste("Import error:", e$message), type = "error", duration = 8)
    })
  })

  # --------------------------------------------------------------------------
  # Tissue Weight Table (ELISA only)
  # --------------------------------------------------------------------------

  # Reactive: get unique replicate groups from the replicate matrix
  replicate_groups <- reactive({
    req(shared$matrix_replicate())
    rep_mat <- shared$matrix_replicate()
    type_mat <- shared$matrix_type()

    # Get replicate labels for Sample wells only
    groups <- c()
    for (r in 1:nrow(rep_mat)) {
      for (cc in 1:ncol(rep_mat)) {
        lbl <- as.character(rep_mat[r, cc])
        tp <- as.character(type_mat[r, cc])
        if (!is.na(lbl) && lbl != "" && tp == "Sample") {
          groups <- c(groups, lbl)
        }
      }
    }
    sort(unique(groups))
  })

  output$tissue_weight_table <- renderRHandsontable({
    req(input$assay_type == "elisa")
    groups <- replicate_groups()
    if (length(groups) == 0) return(NULL)

    # Use isolate() to avoid feedback loop on user edits
    tw <- isolate(shared$tissue_weights_rv())

    # Build wide-format: rows = parameters, columns = replicate groups
    weight_vals <- sapply(groups, function(g) {
      v <- tw[[g]]
      if (is.list(v)) {
        if (is.null(v$weight) || is.na(v$weight)) NA_real_ else as.numeric(v$weight)
      } else {
        if (is.null(v) || is.na(v)) NA_real_ else as.numeric(v)
      }
    })
    extraction_vals <- sapply(groups, function(g) {
      v <- tw[[g]]
      if (is.list(v) && !is.null(v$extraction_uL)) as.numeric(v$extraction_uL) else 500
    })

    df <- data.frame(
      Parameter = c("Weight (mg)", "Extraction vol (\u00b5L)"),
      stringsAsFactors = FALSE
    )
    for (i in seq_along(groups)) {
      df[[groups[i]]] <- c(weight_vals[i], extraction_vals[i])
    }

    rhandsontable(df, rowHeaders = FALSE, stretchH = "all",
                  height = 100, overflow = "hidden") %>%
      hot_col("Parameter", readOnly = TRUE) %>%
      hot_cols(type = "numeric", format = "0.0")
  })

  observeEvent(input$tissue_weight_table, {
    req(input$tissue_weight_table)
    df <- hot_to_r(input$tissue_weight_table)
    groups <- colnames(df)[-1]  # All columns except "Parameter"

    tw <- list()
    for (g in groups) {
      tw[[g]] <- list(
        weight = df[1, g],          # Row 1 = Weight (mg)
        extraction_uL = df[2, g]    # Row 2 = Extraction vol (uL)
      )
    }
    shared$tissue_weights_rv(tw)
  })
}

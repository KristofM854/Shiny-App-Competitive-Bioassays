# layout_history.R
# Undo/redo history stack for plate layout editing.
# Captures snapshots of the four layout matrices (type, id, raw_dilution,
# replicate) and allows stepping backward/forward through them.

create_layout_history <- function(max_size = 20) {
  stack <- reactiveValues(
    history = list(),   # list of state snapshots
    position = 0L       # current position (1-based); 0 = empty

  )

  # Capture current layout state into a plain list

  snapshot_state <- function(shared) {
    list(
      matrix_type         = shared$matrix_type(),
      matrix_id           = shared$matrix_id(),
      raw_matrix_dilution = shared$raw_matrix_dilution(),
      matrix_replicate    = shared$matrix_replicate()
    )
  }

  # Compare two snapshots for equality (avoid duplicate pushes)
  snapshots_equal <- function(a, b) {
    if (is.null(a) || is.null(b)) return(FALSE)
    identical(a$matrix_type, b$matrix_type) &&
      identical(a$matrix_id, b$matrix_id) &&
      identical(a$raw_matrix_dilution, b$raw_matrix_dilution) &&
      identical(a$matrix_replicate, b$matrix_replicate)
  }

  # Push the current state onto the history stack.
  # Truncates any "future" entries beyond the current position.

  push <- function(shared) {
    snap <- snapshot_state(shared)

    # Skip if identical to current position
    if (stack$position > 0L &&
        stack$position <= length(stack$history) &&
        snapshots_equal(stack$history[[stack$position]], snap)) {
      return(invisible(NULL))
    }

    # Truncate any redo history beyond current position
    if (stack$position < length(stack$history)) {
      stack$history <- stack$history[seq_len(stack$position)]
    }

    # Append new snapshot
    stack$history <- c(stack$history, list(snap))

    # Enforce max size (drop oldest entries)
    if (length(stack$history) > max_size) {
      drop <- length(stack$history) - max_size
      stack$history <- stack$history[(drop + 1L):length(stack$history)]
    }

    stack$position <- length(stack$history)
    invisible(NULL)
  }

  can_undo <- reactive({
    stack$position > 1L
  })

  can_redo <- reactive({
    stack$position < length(stack$history)
  })

  # Restore a snapshot to the shared reactive values
  restore_state <- function(shared, snap) {
    if (!is.null(snap$matrix_type))         shared$matrix_type(snap$matrix_type)
    if (!is.null(snap$matrix_id))           shared$matrix_id(snap$matrix_id)
    if (!is.null(snap$raw_matrix_dilution)) {
      shared$raw_matrix_dilution(snap$raw_matrix_dilution)
      # Re-parse dilution values so the numeric matrix stays in sync
      result <- parse_dilution_matrix(snap$raw_matrix_dilution)
      shared$matrix_dilution(enforce_plate_shape(as.data.frame(result$values)))
      shared$dilution_validity(result$validity)
      shared$dilution_error(any(!result$validity))
    }
    if (!is.null(snap$matrix_replicate))    shared$matrix_replicate(snap$matrix_replicate)
  }

  undo <- function(shared) {
    if (stack$position <= 1L) return(invisible(NULL))
    stack$position <- stack$position - 1L
    restore_state(shared, stack$history[[stack$position]])
    invisible(NULL)
  }

  redo <- function(shared) {
    if (stack$position >= length(stack$history)) return(invisible(NULL))
    stack$position <- stack$position + 1L
    restore_state(shared, stack$history[[stack$position]])
    invisible(NULL)
  }

  list(
    push           = push,
    undo           = undo,
    redo           = redo,
    can_undo       = can_undo,
    can_redo       = can_redo,
    snapshot_state = snapshot_state
  )
}

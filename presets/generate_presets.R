# Script to generate preset .rds files for plate layouts
# Run from the repo root: source("presets/generate_presets.R")

dir.create("presets", showWarnings = FALSE)

PLATE_NROW <- 8
PLATE_NCOL <- 12
ROW_NAMES <- LETTERS[1:8]
COL_NAMES <- as.character(1:12)

# Helper: create named matrix as data frame
make_mat <- function(fill = "Sample") {
  mat <- matrix(fill, nrow = PLATE_NROW, ncol = PLATE_NCOL,
                dimnames = list(ROW_NAMES, COL_NAMES))
  as.data.frame(mat, stringsAsFactors = FALSE)
}

# ===== Preset 1: RBA STX 8 standards (triplicate) =====
rba_type <- make_mat("Sample")
rba_id <- make_mat("")
rba_rep <- make_mat("")
for (s in 1:8) {
  rba_type[s, 1:3] <- "Standard"
  rba_id[s, 1:3] <- paste0("S", s)
  rba_rep[s, 1:3] <- paste0(ROW_NAMES[s], "A")
}
# Samples in cols 4-12
sample_idx <- 1
for (c in 4:12) {
  for (r in 1:8) {
    rba_id[r, c] <- paste0("Smp", sample_idx)
    rba_rep[r, c] <- paste0("R", ceiling(sample_idx / 3))
    sample_idx <- sample_idx + 1
  }
}
rba_dil <- make_mat("1")
for (r in 1:nrow(rba_dil)) for (c in 1:ncol(rba_dil)) rba_dil[r, c] <- 1

saveRDS(list(
  type_matrix = rba_type,
  id_matrix = rba_id,
  dilution_matrix = as.data.frame(matrix(1, nrow = 8, ncol = 12, dimnames = list(ROW_NAMES, COL_NAMES))),
  replicate_matrix = rba_rep
), file.path("presets", "rba_stx_triplicate.rds"))

# ===== Preset 2: ELISA Cortisol Cayman kit =====
elisa_type <- make_mat("Sample")
elisa_id <- make_mat("")
elisa_rep <- make_mat("")

# Column 1: Controls
control_pattern <- c("Blank", "Blank", "NSB", "NSB", "B0", "B0", "B0", "TotalActivity")
control_ids <- c("Blank", "Blank", "NSB", "NSB", "B0", "B0", "B0", "TA")
ctrl_rep_ids <- c("Ctrl_Blank", "Ctrl_Blank", "Ctrl_NSB", "Ctrl_NSB",
                  "Ctrl_B0", "Ctrl_B0", "Ctrl_B0", "Ctrl_TA")
for (r in 1:8) {
  elisa_type[r, 1] <- control_pattern[r]
  elisa_id[r, 1] <- control_ids[r]
  elisa_rep[r, 1] <- ctrl_rep_ids[r]
}

# Columns 2-3: Standards (duplicate)
for (s in 1:8) {
  elisa_type[s, 2:3] <- "Standard"
  elisa_id[s, 2:3] <- paste0("S", s)
  elisa_rep[s, 2:3] <- paste0("S", s)
}

# Columns 4-12: Samples (duplicate pairs)
sample_idx <- 1
for (c_pair in seq(4, 12, by = 2)) {
  if (c_pair + 1 > 12) break
  for (r in 1:8) {
    sid <- paste0("Smp", sample_idx)
    elisa_id[r, c_pair] <- sid
    elisa_id[r, c_pair + 1] <- sid
    elisa_rep[r, c_pair] <- paste0("R", sample_idx)
    elisa_rep[r, c_pair + 1] <- paste0("R", sample_idx)
    sample_idx <- sample_idx + 1
  }
}

saveRDS(list(
  type_matrix = elisa_type,
  id_matrix = elisa_id,
  dilution_matrix = as.data.frame(matrix(1, nrow = 8, ncol = 12, dimnames = list(ROW_NAMES, COL_NAMES))),
  replicate_matrix = elisa_rep
), file.path("presets", "elisa_cortisol_cayman.rds"))

# ===== Preset 3: ELISA Custom (blank template) =====
custom_type <- make_mat("Sample")
custom_id <- make_mat("")
custom_rep <- make_mat("")

# Just the control pattern in column 1
for (r in 1:8) {
  custom_type[r, 1] <- control_pattern[r]
  custom_id[r, 1] <- control_ids[r]
  custom_rep[r, 1] <- ctrl_rep_ids[r]
}

saveRDS(list(
  type_matrix = custom_type,
  id_matrix = custom_id,
  dilution_matrix = as.data.frame(matrix(1, nrow = 8, ncol = 12, dimnames = list(ROW_NAMES, COL_NAMES))),
  replicate_matrix = custom_rep
), file.path("presets", "elisa_custom_blank.rds"))

cat("Presets generated successfully!\n")

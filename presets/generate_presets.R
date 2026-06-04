# Script to generate preset .rds files for plate layouts
# Run from the repo root: source("presets/generate_presets.R")
#
# IMPORTANT: Calls create_id_matrix() and create_replicate_matrix()
# from utils_plate.R so presets always match defaults for cols 4-12.
# Each matrix is normalised to a clean 8×12 data frame with canonical
# ROW_NAMES / COL_NAMES so rHandsontable renders at a consistent height.

dir.create("presets", showWarnings = FALSE)

# Source dependencies if not already loaded
if (!exists("PLATE_NROW")) source("global.R")
if (!exists("create_type_matrix")) source("utils/utils_plate.R")

# Helper: ensure clean 8×12 data frame with canonical row/col names.
.norm <- function(m) {
  m <- enforce_plate_shape(as.data.frame(m, stringsAsFactors = FALSE))
  rownames(m) <- ROW_NAMES
  colnames(m) <- COL_NAMES
  m
}

# ===== Preset 1: RBA STX 8 standards (triplicate) =====
saveRDS(list(
  type_matrix      = .norm(create_type_matrix("rba", 8)),
  id_matrix        = .norm(create_id_matrix("rba", 8)),
  dilution_matrix  = .norm(create_dilution_matrix()),
  replicate_matrix = .norm(create_replicate_matrix("rba"))
), file.path("presets", "rba_stx_triplicate.rds"))

# ===== Preset 2: ELISA Cortisol Cayman kit =====
saveRDS(list(
  type_matrix      = .norm(create_type_matrix("elisa", 8)),
  id_matrix        = .norm(create_id_matrix("elisa", 8)),
  dilution_matrix  = .norm(create_dilution_matrix()),
  replicate_matrix = .norm(create_replicate_matrix("elisa"))
), file.path("presets", "elisa_cortisol_cayman.rds"))

# ===== Preset 3: ELISA Custom (blank template) =====
# Controls in col 1 but no standards pre-assigned
custom_type <- create_type_matrix("elisa", 0)
control_pattern <- c("Blank", "Blank", "NSB", "NSB", "B0", "B0", "B0", "TotalActivity")
for (r in 1:PLATE_NROW) custom_type[r, 1] <- control_pattern[r]

custom_id <- create_id_matrix("elisa", 0)
control_ids <- c("Blank", "Blank", "NSB", "NSB", "B0", "B0", "B0", "TA")
for (r in 1:PLATE_NROW) custom_id[r, 1] <- control_ids[r]

custom_rep <- create_replicate_matrix("elisa")
for (r in 1:PLATE_NROW) {
  custom_rep[r, 2] <- ""
  custom_rep[r, 3] <- ""
}

saveRDS(list(
  type_matrix      = .norm(custom_type),
  id_matrix        = .norm(custom_id),
  dilution_matrix  = .norm(create_dilution_matrix()),
  replicate_matrix = .norm(custom_rep)
), file.path("presets", "elisa_custom_blank.rds"))

cat("Presets generated successfully!\n")

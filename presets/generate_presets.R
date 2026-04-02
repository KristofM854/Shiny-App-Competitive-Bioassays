# Script to generate preset .rds files for plate layouts
# Run from the repo root: source("presets/generate_presets.R")
#
# IMPORTANT: Calls create_id_matrix() and create_replicate_matrix()
# from utils_plate.R so presets always match defaults for cols 4-12.

dir.create("presets", showWarnings = FALSE)

# Source dependencies if not already loaded
if (!exists("PLATE_NROW")) source("global.R")
if (!exists("create_type_matrix")) source("utils_plate.R")

# ===== Preset 1: RBA STX 8 standards (triplicate) =====
saveRDS(list(
  type_matrix      = create_type_matrix("rba", 8),
  id_matrix        = create_id_matrix("rba", 8),
  dilution_matrix  = create_dilution_matrix(),
  replicate_matrix = create_replicate_matrix("rba")
), file.path("presets", "rba_stx_triplicate.rds"))

# ===== Preset 2: ELISA Cortisol Cayman kit =====
saveRDS(list(
  type_matrix      = create_type_matrix("elisa", 8),
  id_matrix        = create_id_matrix("elisa", 8),
  dilution_matrix  = create_dilution_matrix(),
  replicate_matrix = create_replicate_matrix("elisa")
), file.path("presets", "elisa_cortisol_cayman.rds"))

# ===== Preset 3: ELISA Custom (blank template) =====
# Controls in col 1 but no standards pre-assigned
custom_type <- create_type_matrix("elisa", 0)
# Re-add the control pattern in column 1 (create_type_matrix with 0 stds sets all to Sample)
control_pattern <- c("Blank", "Blank", "NSB", "NSB", "B0", "B0", "B0", "TotalActivity")
for (r in 1:PLATE_NROW) custom_type[r, 1] <- control_pattern[r]

custom_id <- create_id_matrix("elisa", 0)
# Label control wells in col 1
control_ids <- c("Blank", "Blank", "NSB", "NSB", "B0", "B0", "B0", "TA")
for (r in 1:PLATE_NROW) custom_id[r, 1] <- control_ids[r]

custom_rep <- create_replicate_matrix("elisa")
# Clear standard replicate labels from cols 2-3 (no standards in custom)
for (r in 1:PLATE_NROW) {
  custom_rep[r, 2] <- ""
  custom_rep[r, 3] <- ""
}

saveRDS(list(
  type_matrix      = custom_type,
  id_matrix        = custom_id,
  dilution_matrix  = create_dilution_matrix(),
  replicate_matrix = custom_rep
), file.path("presets", "elisa_custom_blank.rds"))

cat("Presets generated successfully!\n")

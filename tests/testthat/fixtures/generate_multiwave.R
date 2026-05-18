# =============================================================================
# Generator for the synthetic multi-wavelength fixture used by
# tests/testthat/test-shinytest-multiwavelength.R.
#
# Produces tests/testthat/fixtures/multiwave_synthetic.csv. Two plate blocks
# separated by "Raw Data (450)" and "Raw Data (630)" marker rows matching the
# format parsed by utils_import_multiwavelength.R. The 630 nm plate is a
# plausible reference-wavelength copy of the 450 nm plate: 0.92 x the response
# plus a small per-well offset, so Lin's CCC should be > 0.99 and the
# Bland-Altman bias band should be tight.
#
# Re-run this script (from the repo root, inside R) whenever the fixture needs
# refreshing:
#   source("tests/testthat/fixtures/generate_multiwave.R")
# =============================================================================

set.seed(20260422)

# ELISA layout matching create_type_matrix("elisa", 8):
#   Col 1: A,B=Blank; C,D=NSB; E,F,G=B0; H=TotalActivity
#   Col 2-3: 8 standards (S1-S8, rows A-H) in duplicate
#   Col 4-12: samples in triplicate groups

plate_450 <- matrix(NA_real_, nrow = 8, ncol = 12,
                    dimnames = list(LETTERS[1:8], as.character(1:12)))

# Column 1 controls — must match the app's default ELISA layout from
# create_type_matrix("elisa", 8):  A,B=Blank; C,D=NSB; E,F,G=B0; H=TotalActivity.
# NSB must be < B0 after blank correction so calculate_elisa_bb0() succeeds.
plate_450["A", 1] <- 0.045   # Blank
plate_450["B", 1] <- 0.052   # Blank
plate_450["C", 1] <- 0.090   # NSB
plate_450["D", 1] <- 0.088   # NSB
plate_450["E", 1] <- 1.280   # B0
plate_450["F", 1] <- 1.310   # B0
plate_450["G", 1] <- 1.295   # B0
plate_450["H", 1] <- 2.140   # TotalActivity

# Standard curve in columns 2-3 (duplicate). B0 response ~1.3, Blank ~0.05.
# Classic competitive ELISA: response drops as analyte increases.
std_curve <- c(0.128, 0.194, 0.353, 0.646, 0.977, 1.175, 1.260, 1.272)
plate_450[, 2] <- std_curve + rnorm(8, 0, 0.008)
plate_450[, 3] <- std_curve + rnorm(8, 0, 0.008)

# Samples in columns 4-12: three triplicate groups spaced across the curve
sample_means <- c(0.42, 0.42, 0.42,   # col 4-6: mid-curve (quantifiable)
                  0.85, 0.85, 0.85,   # col 7-9: upper-curve (low analyte)
                  0.20, 0.20, 0.20)   # col 10-12: below-LLOQ (high analyte)
for (j in 4:12) {
  plate_450[, j] <- sample_means[j - 3] + rnorm(8, 0, 0.015)
}

# 630 nm is a reference wavelength: 0.92 x the 450 nm response plus tiny
# per-well offset. Realistic agreement so Lin's CCC > 0.99.
plate_630 <- 0.92 * plate_450 + rnorm(length(plate_450), 0, 0.006)

# Round to 4 decimal places for a readable fixture file.
plate_450 <- round(plate_450, 4)
plate_630 <- round(plate_630, 4)

# ---- Assemble CSV in the "Raw Data (XXX)" format expected by the importer ----
# Each block: blank row, "Raw Data (XXX)" marker in col 2, column-number
# header row (1..12 in cols 2..13), then A-H rows with row letter in col 1
# and numeric values in cols 2..13.

out_path <- "tests/testthat/fixtures/multiwave_synthetic.csv"

format_block <- function(mat, wavelength) {
  header <- paste(c("", as.character(1:12)), collapse = ",")
  rows <- vapply(seq_len(8), function(i) {
    paste(c(LETTERS[i], format(mat[i, ], trim = TRUE, scientific = FALSE)),
          collapse = ",")
  }, character(1))
  c(paste0(",Raw Data (", wavelength, ")"),
    header,
    rows)
}

lines <- c(
  ",Plate Reader Export (synthetic fixture for shinytest2)",
  "",
  format_block(plate_450, 450),
  "",
  format_block(plate_630, 630),
  ""
)

writeLines(lines, out_path)
message(sprintf("Wrote %s (%d lines)", out_path, length(lines)))

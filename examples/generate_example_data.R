# Generate Example Datasets for Bioassay Analysis App
# Run once from the repo root, then commit the output files

set.seed(2026)

PLATE_NROW <- 8
PLATE_NCOL <- 12
ROW_NAMES <- LETTERS[1:8]

# --- RBA Saxitoxin Example ---
rba_concs <- c(1e-6, 1e-7, 3e-8, 1e-8, 3e-9, 1e-9, 1e-10, 3e-11)
plate_rba <- matrix(NA_real_, nrow = 8, ncol = 12)

# Standards: 4PL curve with realistic noise
for (s in 1:8) {
  true_resp <- 200 + (8500 - 200) / (1 + (rba_concs[s] / 5e-9)^(-1.05))
  plate_rba[s, 1:3] <- round(rnorm(3, true_resp, true_resp * 0.06), 0)
}

# Samples: various concentrations
sample_concs <- c(2e-7, 5e-8, 1e-8, 7e-9, 2e-9, 8e-10, 3e-10, 5e-11,
                  4e-8, 6e-9, 1e-9, 2e-10, 9e-8, 3e-8, 5e-9, 4e-10,
                  1e-7, 2e-8, 8e-9, 1e-10, 7e-8, 4e-9, 6e-10, 2e-11)
idx <- 1
for (col in 4:12) {
  for (row in 1:8) {
    if (idx <= length(sample_concs)) {
      conc <- sample_concs[idx]
      true_resp <- 200 + (8500 - 200) / (1 + (conc / 5e-9)^(-1.05))
      plate_rba[row, col] <- round(rnorm(1, true_resp, true_resp * 0.06), 0)
      idx <- idx + 1
    }
  }
}

# Write with row labels
lines_rba <- sapply(1:8, function(r) {
  paste(c(LETTERS[r], plate_rba[r, ]), collapse = "\t")
})
dir.create("examples", showWarnings = FALSE)
writeLines(lines_rba, "examples/rba_stx_example.csv")


# --- ELISA Cortisol Example ---
elisa_concs <- c(4000, 1600, 640, 256, 102.4, 41.0, 16.4, 6.6)
plate_elisa <- matrix(NA_real_, nrow = 8, ncol = 12)

blank_abs <- 0.045; nsb_abs <- 0.085; b0_abs <- 1.35

# Column 1: Controls
plate_elisa[1:2, 1] <- round(rnorm(2, blank_abs, 0.005), 4)
plate_elisa[3:4, 1] <- round(rnorm(2, nsb_abs, 0.008), 4)
plate_elisa[5:7, 1] <- round(rnorm(3, b0_abs, 0.04), 4)
plate_elisa[8, 1]   <- round(rnorm(1, 2.10, 0.05), 4)

# Columns 2-3: Standards
for (s in 1:8) {
  bb0 <- 100 / (1 + (elisa_concs[s] / 200)^1.1)
  nsb_corr <- bb0 / 100 * (b0_abs - blank_abs - (nsb_abs - blank_abs))
  abs_val <- nsb_corr + (nsb_abs - blank_abs) + blank_abs
  plate_elisa[s, 2:3] <- round(rnorm(2, abs_val, abs_val * 0.04), 4)
}

# Columns 4-11: Samples
sample_elisa <- c(50, 150, 300, 800, 25, 500, 100, 1200,
                  75, 200, 400, 60, 350, 900, 180, 2500)
idx <- 1
for (cs in seq(4, 10, by = 2)) {
  for (row in 1:8) {
    if (idx <= length(sample_elisa)) {
      conc <- sample_elisa[idx]
      bb0 <- 100 / (1 + (conc / 200)^1.1)
      nsb_corr <- bb0 / 100 * (b0_abs - blank_abs - (nsb_abs - blank_abs))
      abs_val <- nsb_corr + (nsb_abs - blank_abs) + blank_abs
      plate_elisa[row, cs]     <- round(rnorm(1, abs_val, abs_val * 0.04), 4)
      plate_elisa[row, cs + 1] <- round(rnorm(1, abs_val, abs_val * 0.04), 4)
      idx <- idx + 1
    }
  }
}

lines_elisa <- sapply(1:8, function(r) {
  paste(c(LETTERS[r], plate_elisa[r, ]), collapse = "\t")
})
writeLines(lines_elisa, "examples/elisa_cortisol_example.csv")

cat("Example datasets generated:\n")
cat("  examples/rba_stx_example.csv\n")
cat("  examples/elisa_cortisol_example.csv\n")

# scripts/init_renv.R
#
# AUDIT-004 (dependency pinning): bootstrap renv for reproducibility.
# Run ONCE on the maintainer's machine to generate renv.lock + renv/:
#
#   Rscript scripts/init_renv.R
#
# After that:
#   - Commit `renv.lock`, `renv/activate.R`, and `.Rprofile`.
#   - Contributors restore the locked env with: `Rscript -e 'renv::restore()'`.
#   - Refresh the lockfile after a dependency change:
#       `Rscript -e 'renv::snapshot()'`.
#
# DESCRIPTION already declares minimum-version pins for the sensitive
# packages (drc, ggplot2, dplyr, rmarkdown — see test-audit-004.R) and
# Depends: R (>= 4.4). renv.lock layers exact-version reproducibility on
# top of that so the reference numbers in tests/testthat/test-report-numbers.R
# and the shipped example reports are reproducible byte-for-byte.

if (!requireNamespace("renv", quietly = TRUE)) {
  install.packages("renv", repos = "https://cloud.r-project.org")
}

# Bare init: skip renv's static dependency discovery (which can miss packages
# loaded dynamically inside Rmd chunks). Snapshot the current library so
# what's actually installed gets recorded.
renv::init(bare = TRUE, force = TRUE)
renv::snapshot(type = "all", prompt = FALSE)

cat(
  "\n[init_renv] renv.lock + renv/activate.R + .Rprofile generated.\n",
  "[init_renv] Commit all three files to seal AUDIT-004 (dependency pinning).\n",
  sep = ""
)

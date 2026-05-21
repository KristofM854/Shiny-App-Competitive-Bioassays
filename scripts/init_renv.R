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

# Standard renv init: scans DESCRIPTION + all source/Rmd files for library()
# and :: usage, installs discovered packages into the project library, and
# writes renv.lock in one pass.  Do NOT use bare = TRUE here — that creates an
# empty project library so the subsequent snapshot records only the bootstrap
# set, missing the full analysis/reporting stack.
renv::init(force = TRUE)

# Explicit snapshot to capture any packages that renv's static scanner missed
# (e.g. packages loaded conditionally inside Rmd chunks).  Call this again
# after installing any manually-added packages with renv::install().
renv::snapshot(type = "all", prompt = FALSE)

cat(
  "\n[init_renv] renv.lock + renv/activate.R + .Rprofile generated.\n",
  "[init_renv] Commit all three files to seal AUDIT-004 (dependency pinning).\n",
  sep = ""
)

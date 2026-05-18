#!/usr/bin/env Rscript
# Replay a report from an existing output directory without re-running the Shiny app.
#
# Usage:
#   Rscript scripts/replay_report.R /path/to/output_dir
#
# Or via environment variable:
#   RBA_OUTPUT_DIR=/path/to/output_dir Rscript scripts/replay_report.R

args       <- commandArgs(trailingOnly = TRUE)
output_dir <- if (length(args) >= 1L) args[[1L]] else Sys.getenv("RBA_OUTPUT_DIR", unset = "")

if (!nzchar(output_dir)) {
  stop(
    "Usage: Rscript scripts/replay_report.R <output_dir>\n",
    "  or set environment variable RBA_OUTPUT_DIR"
  )
}

if (!dir.exists(output_dir)) {
  stop("Output directory does not exist: ", output_dir)
}

required <- c("long_data_output.csv", "model_stats.json")
missing  <- required[!file.exists(file.path(output_dir, required))]
if (length(missing) > 0L) {
  stop("Required files missing from output_dir: ", paste(missing, collapse = ", "))
}

output_dir <- normalizePath(output_dir, mustWork = TRUE)

# Locate repo root relative to this script's location.
# sys.frame(1)$ofile is set when the script is sourced; when run via Rscript
# the path comes from the command-line argument to Rscript itself.
repo_root <- tryCatch({
  script_path <- normalizePath(sys.frame(1L)$ofile, mustWork = FALSE)
  normalizePath(file.path(dirname(script_path), ".."), mustWork = FALSE)
}, error = function(e) {
  # Fallback: derive from the first Rscript argument (the script path)
  all_args    <- commandArgs(trailingOnly = FALSE)
  script_flag <- grep("--file=", all_args, value = TRUE)
  if (length(script_flag) > 0L) {
    script_path <- sub("^--file=", "", script_flag[[1L]])
    normalizePath(file.path(dirname(script_path), ".."), mustWork = FALSE)
  } else {
    normalizePath(".", mustWork = FALSE)
  }
})

template <- file.path(repo_root, "reports", "unified_analysis_template.Rmd")
if (!file.exists(template)) {
  stop("Template not found: ", template)
}

cat("Replaying report from:", output_dir, "\n")
cat("Template:             ", template, "\n")

out_file <- rmarkdown::render(
  input      = template,
  output_dir = output_dir,
  params     = list(output_dir = output_dir),
  envir      = new.env(parent = globalenv()),
  quiet      = FALSE
)

cat("Report written to:", out_file, "\n")

# Generate a Word reference document for consistent report styling.
# Run once from the repository root, then commit reports/reference.docx.
#
# Usage:
#   Rscript reports/create_reference_doc.R
#
# After running, open reports/reference.docx in Microsoft Word, modify the
# heading styles (font, size, colour, spacing), and save. The Word templates
# in reports/ will then pick up those styles automatically via
#   output: word_document: reference_doc: reference.docx

library(rmarkdown)

# Resolve repo root from this script's location so the script works whether
# called as `Rscript reports/create_reference_doc.R` or sourced interactively.
all_args    <- commandArgs(trailingOnly = FALSE)
script_flag <- grep("^--file=", all_args, value = TRUE)
if (length(script_flag) > 0L) {
  script_dir <- normalizePath(dirname(sub("^--file=", "", script_flag[[1L]])),
                              mustWork = FALSE)
} else {
  script_dir <- normalizePath("reports", mustWork = FALSE)
}
out_dir <- script_dir

# A minimal Rmd that exercises the heading levels and a table so Word generates
# matching paragraph/table styles.
ref_rmd <- tempfile(fileext = ".Rmd")
writeLines(c(
  "---",
  "title: Reference Document",
  "output: word_document",
  "---",
  "",
  "# Heading 1",
  "",
  "## Heading 2",
  "",
  "### Heading 3",
  "",
  "Body text paragraph.",
  "",
  "| Column A | Column B |",
  "|----------|----------|",
  "| Data 1   | Data 2   |"
), ref_rmd)

rmarkdown::render(ref_rmd,
                  output_file = "reference.docx",
                  output_dir  = out_dir,
                  quiet       = TRUE)
unlink(ref_rmd)

message("Reference doc created at: ", file.path(out_dir, "reference.docx"))
message("")
message("Next steps:")
message("  1. Open reports/reference.docx in Microsoft Word.")
message("  2. Modify heading styles: font, size, colour, paragraph spacing.")
message("  3. Save the file (keep the filename reference.docx).")
message("  4. Commit: git add reports/reference.docx && git commit")
message("")
message("Once committed, add `reference_doc: reference.docx` under")
message("`output: word_document:` in any Rmd to use these styles.")

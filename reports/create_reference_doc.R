# Generate a Word reference document for consistent report styling
# Run once, then commit the output file

library(rmarkdown)

# Create a minimal Rmd that defines heading styles
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

rmarkdown::render(ref_rmd, output_file = "reference.docx",
                  output_dir = "reports")
unlink(ref_rmd)

message("Reference doc created at reports/reference.docx")
message("Open it in Word, modify heading styles (fonts, colors, spacing),")
message("then save. The Rmd will use these styles automatically.")

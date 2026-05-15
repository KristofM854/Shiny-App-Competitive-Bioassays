# Tests for AUDIT-004: DESCRIPTION declares correct R version and minimum package pins

test_that("AUDIT-004: DESCRIPTION declares R >= 4.2", {
  desc_path <- file.path(.repo_root, "DESCRIPTION")
  skip_if(!file.exists(desc_path))
  desc_lines <- readLines(desc_path)
  depends_line <- grep("R \\(>= 4\\.2\\)", desc_lines, value = TRUE)
  expect_length(depends_line, 1,
                info = "DESCRIPTION should declare Depends: R (>= 4.2)")
})

test_that("AUDIT-004: DESCRIPTION has minimum version pins for sensitive packages", {
  desc_path <- file.path(.repo_root, "DESCRIPTION")
  skip_if(!file.exists(desc_path))
  desc_lines <- readLines(desc_path)
  desc_text <- paste(desc_lines, collapse = "\n")
  expect_true(grepl("drc.*>=.*3\\.0", desc_text),
              info = "DESCRIPTION should pin drc >= 3.0-1")
  expect_true(grepl("ggplot2.*>=.*3\\.4", desc_text),
              info = "DESCRIPTION should pin ggplot2 >= 3.4.0")
  expect_true(grepl("dplyr.*>=.*1\\.1", desc_text),
              info = "DESCRIPTION should pin dplyr >= 1.1.0")
  expect_true(grepl("rmarkdown.*>=.*2\\.20", desc_text),
              info = "DESCRIPTION should pin rmarkdown >= 2.20")
})

test_that("AUDIT-026: DESCRIPTION imports remotes and withr", {
  desc_path <- file.path(.repo_root, "DESCRIPTION")
  skip_if(!file.exists(desc_path))
  desc_lines <- readLines(desc_path)
  desc_text <- paste(desc_lines, collapse = "\n")
  expect_true(grepl("remotes", desc_text),
              info = "DESCRIPTION should import remotes (AUDIT-026)")
  expect_true(grepl("withr", desc_text),
              info = "DESCRIPTION should import withr")
})

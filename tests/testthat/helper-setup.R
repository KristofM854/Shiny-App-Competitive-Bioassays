# Load app dependencies for testing
# Run tests from the repo root: testthat::test_dir("tests/testthat")

library(dplyr)
library(tidyr)
library(tibble)
library(stringr)
library(jsonlite)

# Source app modules (assumes working directory is repo root)
source("global.R")
source("utils_plate.R")
source("utils_import_v3.R")
source("utils_normalization.R")
source("reports/report_constants.R")
source("reports/report_functions.R")

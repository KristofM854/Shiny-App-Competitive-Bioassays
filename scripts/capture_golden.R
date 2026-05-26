#!/usr/bin/env Rscript
# scripts/capture_golden.R
#
# Run once to populate tests/testthat/golden/{rba,elisa}/ with reference
# artifact files. Then commit those files. The test-golden-*.R tests enforce
# stability against future changes.
#
# Usage (from repo root):
#   Rscript scripts/capture_golden.R
#
# Output artifacts written to tests/testthat/golden/:
#   rba/model_stats.json
#   rba/unknown_results.csv
#   rba/unknown_results_summary.csv
#   elisa/model_stats.json
#   elisa/unknown_results.csv
#   elisa/unknown_results_summary.csv
#
# After running, inspect the JSON files to verify the numbers look reasonable,
# then: git add tests/testthat/golden/ && git commit -m "chore: capture golden artifacts"
#
# ELISA IC50 NOTE: AUDIT-005 removed the upperl=c(NA,NA,100,NA) constraint
# from the ELISA DRC fit. The captured IC50 may differ from the pre-fix
# baseline value of 205.2275 in examples/reference_outputs.json. That is
# expected — the golden files track the CURRENT code, not the pre-fix snapshot.

# --- Resolve repo root -------------------------------------------------------
all_args    <- commandArgs(trailingOnly = FALSE)
script_flag <- grep("^--file=", all_args, value = TRUE)
if (length(script_flag) > 0L) {
  repo_root <- normalizePath(
    file.path(dirname(sub("^--file=", "", script_flag[[1L]])), ".."),
    mustWork = FALSE
  )
} else {
  repo_root <- normalizePath(getwd(), mustWork = FALSE)
}
setwd(repo_root)
message("Repo root: ", repo_root)

source(file.path(repo_root, "scripts", "_bootstrap_analysis.R"))

golden_root <- file.path(repo_root, "tests", "testthat", "golden")
dir.create(file.path(golden_root, "rba"),   recursive = TRUE, showWarnings = FALSE)
dir.create(file.path(golden_root, "elisa"), recursive = TRUE, showWarnings = FALSE)

git_sha <- tryCatch(
  trimws(system("git rev-parse --short HEAD", intern = TRUE)),
  error = function(e) "unknown"
)

# --- Helpers -----------------------------------------------------------------

write_sidecars_rba <- function(out_dir) {
  jsonlite::write_json(list("html"),
    file.path(out_dir, "selected_formats.json"), auto_unbox = TRUE)
  jsonlite::write_json(list(notes = ""),
    file.path(out_dir, "notes.json"), auto_unbox = TRUE)
  jsonlite::write_json(list(
    qc_concentration = "3e-9", expected_hill = "1",
    assay_type = "rba", detection_method = "radioligand", analyte = "Saxitoxin"
  ), file.path(out_dir, "qc_params.json"), auto_unbox = TRUE)
  jsonlite::write_json(list(
    assay_type = "rba", toxin_class = "Saxitoxin",
    toxin_standard_label = "Saxitoxin",
    molecular_weight_g_mol = unname(MW_LOOKUP[["Saxitoxin"]]),
    detection_method = "radioligand", units = "mol/L"
  ), file.path(out_dir, "assay_config.json"), auto_unbox = TRUE)
  jsonlite::write_json(list(
    regression_weight = "none", quant_range_min = 20, quant_range_max = 80,
    ci_method = "t_dist", enable_outlier_detection = FALSE,
    outlier_min_n = 3, normality_assumption = "assume", cv_limit = 30
  ), file.path(out_dir, "analysis_config.json"), auto_unbox = TRUE)
  jsonlite::write_json(list(lang = "en", langs = list("en")),
    file.path(out_dir, "report_language.json"), auto_unbox = TRUE)
}

write_sidecars_elisa <- function(out_dir) {
  jsonlite::write_json(list("html"),
    file.path(out_dir, "selected_formats.json"), auto_unbox = TRUE)
  jsonlite::write_json(list(notes = ""),
    file.path(out_dir, "notes.json"), auto_unbox = TRUE)
  jsonlite::write_json(list(
    qc_concentration = "200", expected_hill = "1",
    assay_type = "elisa", detection_method = "absorbance", analyte = "Cortisol"
  ), file.path(out_dir, "qc_params.json"), auto_unbox = TRUE)
  jsonlite::write_json(list(
    assay_type = "elisa", analyte = "Cortisol",
    detection_method = "absorbance", units = "pg/mL"
  ), file.path(out_dir, "assay_config.json"), auto_unbox = TRUE)
  jsonlite::write_json(list(
    regression_weight = "none", quant_range_min = 20, quant_range_max = 80,
    ci_method = "t_dist", enable_outlier_detection = FALSE,
    outlier_min_n = 3, normality_assumption = "assume", cv_limit = 30
  ), file.path(out_dir, "analysis_config.json"), auto_unbox = TRUE)
  jsonlite::write_json(list(lang = "en", langs = list("en")),
    file.path(out_dir, "report_language.json"), auto_unbox = TRUE)
}

render_single <- function(out_dir) {
  template <- file.path(repo_root, "reports", "unified_analysis_template.Rmd")
  csv_path <- file.path(out_dir, "long_data_output.csv")
  withr::with_envvar(
    list(
      RBA_OUTPUT_DIR = out_dir,
      RBA_CSV_PATH   = csv_path,
      RBA_FMT_JSON   = file.path(out_dir, "selected_formats.json"),
      RBA_NOTES_FILE = file.path(out_dir, "notes.json")
    ),
    rmarkdown::render(
      input        = template,
      output_file  = "analysis_report.html",
      output_dir   = out_dir,
      params       = list(output_dir = out_dir, lang = "en"),
      knit_root_dir = dirname(template),
      envir        = new.env(parent = globalenv()),
      quiet        = TRUE
    )
  )
}

copy_artifacts <- function(src_dir, dst_dir) {
  artifacts <- c("model_stats.json", "unknown_results.csv",
                 "unknown_results_summary.csv", "replicate_stats.csv")
  for (f in artifacts) {
    src <- file.path(src_dir, f)
    if (file.exists(src)) file.copy(src, file.path(dst_dir, f), overwrite = TRUE)
  }
}

# --- RBA Saxitoxin -----------------------------------------------------------
message("\n[1/2] Capturing RBA golden artifacts...")
rba_tmp <- file.path(tempdir(), paste0("golden-rba-", format(Sys.time(), "%Y%m%d%H%M%S")))
dir.create(rba_tmp, recursive = TRUE, showWarnings = FALSE)

n_std    <- 8L
raw      <- read.table(file.path(repo_root, "examples", "rba_stx_example.csv"),
                       header = FALSE, sep = "\t", stringsAsFactors = FALSE)
meas_mat <- as.data.frame(apply(raw[, 2:13], 2, as.numeric))
df_long  <- matrix_to_long(
  create_type_matrix("rba", n_std),
  create_id_matrix("rba", n_std),
  create_dilution_matrix(),
  create_replicate_matrix("rba"),
  meas_mat,
  std_conc = DEFAULT_STX_CONC[1:n_std]
)
rba_long_norm <- normalize_data(df_long, "rba", "radioligand")
write.csv(rba_long_norm, file.path(rba_tmp, "long_data_output.csv"), row.names = FALSE)
write_sidecars_rba(rba_tmp)

render_single(rba_tmp)
copy_artifacts(rba_tmp, file.path(golden_root, "rba"))
message("    Done -> ", file.path(golden_root, "rba"))

# --- ELISA Cortisol ----------------------------------------------------------
message("\n[2/2] Capturing ELISA golden artifacts...")
elisa_tmp <- file.path(tempdir(), paste0("golden-elisa-", format(Sys.time(), "%Y%m%d%H%M%S")))
dir.create(elisa_tmp, recursive = TRUE, showWarnings = FALSE)

raw       <- read.table(file.path(repo_root, "examples", "elisa_cortisol_example.csv"),
                        header = FALSE, sep = "\t", stringsAsFactors = FALSE)
meas_mat  <- as.data.frame(apply(raw[, 2:13], 2, as.numeric))
df_long   <- matrix_to_long(
  create_type_matrix("elisa", n_std),
  create_id_matrix("elisa", n_std),
  create_dilution_matrix(),
  create_replicate_matrix("elisa"),
  meas_mat,
  std_conc = DEFAULT_CORTISOL_CONC[1:n_std]
)
df_long$NormalizedValue <- df_long$MeasurementValue
df_long$ResponseUnit    <- "Raw Absorbance"
write.csv(df_long, file.path(elisa_tmp, "long_data_output.csv"), row.names = FALSE)
write_sidecars_elisa(elisa_tmp)

render_single(elisa_tmp)
copy_artifacts(elisa_tmp, file.path(golden_root, "elisa"))
message("    Done -> ", file.path(golden_root, "elisa"))

# --- Summary -----------------------------------------------------------------
message("\n==============================================================")
message("Golden capture complete.")
message("Git SHA:     ", git_sha)
message("Output root: ", golden_root)
message("==============================================================")
message("")
message("Inspect captured values:")
message("  cat tests/testthat/golden/rba/model_stats.json")
message("  cat tests/testthat/golden/elisa/model_stats.json")
message("")
message("Then commit:")
message("  git add tests/testthat/golden/")
message("  git commit -m 'chore: capture golden artifacts'")

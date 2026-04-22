# =============================================================================
# reports/analysis_pipeline.R
#
# Compute-layer for the analysis Rmd templates. This file is intentionally
# empty at the M1.7 scaffolding step: source paths are wired up and the file
# is shipped so subsequent M1.1-M1.5 refactors can drop extracted helpers in
# without re-touching the Rmd template's source chain.
#
# The refactor plan (IMPLEMENTATION_PLAN.md, §M1) moves pure-compute logic
# out of reports/unified_analysis_template.Rmd into functions here:
#
#   M1.1 quantify_samples(data_long, model_fit, analysis_config, is_elisa,
#                         response_var, classify_range, flag_range)
#   M1.2 fit_all_models(standards_for_model, response_var, selected_weights,
#                       is_elisa, analysis_config, data_long)
#   M1.3 determine_lloq_uloq(standards_for_model, model_fit, response_var,
#                            is_elisa)
#   M1.4 compute_standard_recovery(data_long, model_fit, response_var,
#                                  is_elisa)
#   M1.5 assess_plate_positional(data_long)
#
# Each extraction is expected to preserve numeric output byte-for-byte
# against the B6 golden-number test (tests/testthat/test-report-numbers.R).
# =============================================================================

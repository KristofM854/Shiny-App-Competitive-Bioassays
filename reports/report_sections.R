# =============================================================================
# reports/report_sections.R
#
# Presentation-layer helpers for the analysis Rmd templates. Empty at the
# M1.7 scaffolding step. M1.6 will move cat-heavy rendering blocks
# (executive summary, QC traffic light, exclusion audit, tissue
# normalisation traceability, ...) into small functions here so the Rmd
# shrinks from ~2000 lines toward the target of 700-900 lines.
#
# Functions added here should accept already-computed data frames and
# produce markdown via cat() / render_table(), never do numeric work.
# =============================================================================

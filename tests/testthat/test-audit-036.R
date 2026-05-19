# AUDIT-036: A second copy of the default RBA standard series lived in
# reports/report_constants.R (DEFAULT_STANDARDS$rba_saxitoxin) and diverged
# from the canonical global.R::DEFAULT_STX_CONC (different S2..S8 values), so
# a user on the standalone Rmd path could get different standards. global.R
# is the single source of truth; the unused divergent duplicate was removed.
# Lock it so a future edit cannot reintroduce a second, drifting copy.
# global.R is sourced by helper-setup.R.

test_that("AUDIT-036: no divergent duplicate of default RBA standards", {
  skip_if_not(exists("DEFAULT_STX_CONC"),
              "DEFAULT_STX_CONC not loaded (global.R)")

  # Canonical 8-point saxitoxin series (mol/L), defined once in global.R.
  expect_equal(
    DEFAULT_STX_CONC,
    c(1e-6, 1e-7, 3e-8, 1e-8, 3e-9, 1e-9, 1e-10, 3e-11))

  # The divergent duplicate must not be reintroduced. If a future maintainer
  # re-adds DEFAULT_STANDARDS, it must at least agree with the canonical
  # series rather than silently drifting again.
  if (exists("DEFAULT_STANDARDS")) {
    expect_equal(
      DEFAULT_STANDARDS$rba_saxitoxin, DEFAULT_STX_CONC,
      info = "DEFAULT_STANDARDS$rba_saxitoxin must match global.R::DEFAULT_STX_CONC")
  } else {
    succeed("DEFAULT_STANDARDS removed; global.R is the single source of truth")
  }
})

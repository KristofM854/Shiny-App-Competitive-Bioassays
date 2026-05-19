# AUDIT-028: JOSS requires community guidelines and a contribution process.
# CONTRIBUTING.md and CODE_OF_CONDUCT.md must exist at the repo root so the
# requirement cannot silently regress (e.g. an accidental rename/delete).
# .repo_root is resolved by helper-setup.R.

test_that("AUDIT-028: CONTRIBUTING.md and CODE_OF_CONDUCT.md exist at repo root", {
  skip_if_not(exists(".repo_root"), ".repo_root not set by helper-setup.R")

  contributing <- file.path(.repo_root, "CONTRIBUTING.md")
  coc          <- file.path(.repo_root, "CODE_OF_CONDUCT.md")

  expect_true(file.exists(contributing),
              info = "CONTRIBUTING.md must exist at the repository root")
  expect_true(file.exists(coc),
              info = "CODE_OF_CONDUCT.md must exist at the repository root")

  # Non-empty: a placeholder/zero-byte file would pass a bare existence check
  # but still fail the JOSS community-guidelines expectation.
  expect_gt(file.info(contributing)$size, 0L)
  expect_gt(file.info(coc)$size, 0L)
})

test_that("parse_dilution_cell handles numeric input", {
  expect_equal(parse_dilution_cell("1")$value, 1)
  expect_true(parse_dilution_cell("1")$valid)
  expect_equal(parse_dilution_cell("0.5")$value, 0.5)
  expect_true(parse_dilution_cell("0.5")$valid)
})

test_that("parse_dilution_cell handles ratio input", {
  expect_equal(parse_dilution_cell("1:2")$value, 0.5)
  expect_true(parse_dilution_cell("1:2")$valid)
  expect_equal(parse_dilution_cell("1:10")$value, 0.1)
  expect_true(parse_dilution_cell("1:10")$valid)
})

test_that("parse_dilution_cell handles scientific notation", {
  expect_equal(parse_dilution_cell("1e-3")$value, 0.001)
  expect_true(parse_dilution_cell("1e-3")$valid)
})

test_that("parse_dilution_cell rejects invalid input", {
  expect_false(parse_dilution_cell("")$valid)
  expect_false(parse_dilution_cell(NULL)$valid)
  expect_false(parse_dilution_cell("abc")$valid)
  expect_false(parse_dilution_cell("0")$valid)
  expect_false(parse_dilution_cell("-1")$valid)
  expect_false(parse_dilution_cell("1:0")$valid)
})

test_that("parse_dilution_cell handles whitespace", {
  expect_equal(parse_dilution_cell("  0.5  ")$value, 0.5)
  expect_equal(parse_dilution_cell("  1:2  ")$value, 0.5)
})

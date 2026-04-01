test_that("create_plate_matrix returns 8x12 data.frame of NAs", {
  mat <- create_plate_matrix()
  expect_equal(nrow(mat), 8)
  expect_equal(ncol(mat), 12)
  expect_true(all(is.na(mat)))
  expect_equal(rownames(mat), LETTERS[1:8])
  expect_equal(colnames(mat), as.character(1:12))
})

test_that("create_type_matrix RBA places standards correctly", {
  mat <- create_type_matrix("rba", 8)
  for (s in 1:8) {
    expect_equal(as.character(mat[s, 1]), "Standard")
    expect_equal(as.character(mat[s, 2]), "Standard")
    expect_equal(as.character(mat[s, 3]), "Standard")
  }
  expect_equal(as.character(mat[1, 4]), "Sample")
})

test_that("create_type_matrix ELISA places controls and standards", {
  mat <- create_type_matrix("elisa", 8)
  expect_equal(as.character(mat[1, 1]), "Blank")
  expect_equal(as.character(mat[3, 1]), "NSB")
  expect_equal(as.character(mat[5, 1]), "B0")
  expect_equal(as.character(mat[8, 1]), "TotalActivity")
  for (s in 1:8) {
    expect_equal(as.character(mat[s, 2]), "Standard")
    expect_equal(as.character(mat[s, 3]), "Standard")
  }
})

test_that("create_type_matrix handles 0 standards", {
  mat <- create_type_matrix("rba", 0)
  expect_true(all(mat == "Sample"))
})

test_that("create_type_matrix handles >8 standards for RBA", {
  mat <- create_type_matrix("rba", 10)
  expect_equal(as.character(mat[1, 4]), "Standard")
  expect_equal(as.character(mat[2, 4]), "Standard")
  expect_equal(as.character(mat[3, 4]), "Sample")
})

test_that("create_id_matrix groups standard IDs by row for RBA", {
  mat <- create_id_matrix("rba", 8)
  # All triplicates in row A should be S1
  expect_equal(as.character(mat[1, 1]), "S1")
  expect_equal(as.character(mat[1, 2]), "S1")
  expect_equal(as.character(mat[1, 3]), "S1")
  # Row B = S2
  expect_equal(as.character(mat[2, 1]), "S2")
  expect_equal(as.character(mat[2, 2]), "S2")
  expect_equal(as.character(mat[2, 3]), "S2")
  # Row H = S8
  expect_equal(as.character(mat[8, 1]), "S8")
})

test_that("create_id_matrix groups standard IDs by row for ELISA", {
  mat <- create_id_matrix("elisa", 8)
  # Duplicates in columns 2-3 should share ID
  expect_equal(as.character(mat[1, 2]), "S1")
  expect_equal(as.character(mat[1, 3]), "S1")
  expect_equal(as.character(mat[4, 2]), "S4")
  expect_equal(as.character(mat[4, 3]), "S4")
})

test_that("enforce_plate_shape pads small data frames", {
  small <- data.frame(a = 1:4, b = 5:8)
  result <- enforce_plate_shape(small)
  expect_equal(nrow(result), 8)
  expect_equal(ncol(result), 12)
  expect_equal(colnames(result), as.character(1:12))
  expect_equal(rownames(result), LETTERS[1:8])
})

test_that("enforce_plate_shape trims large data frames", {
  large <- as.data.frame(matrix(1, nrow = 10, ncol = 15))
  result <- enforce_plate_shape(large)
  expect_equal(nrow(result), 8)
  expect_equal(ncol(result), 12)
})

test_that("create_replicate_matrix ELISA uses Ctrl_ prefix for controls", {
  mat <- create_replicate_matrix("elisa")
  expect_true(grepl("^Ctrl_", as.character(mat[1, 1])))
  expect_true(grepl("^Ctrl_", as.character(mat[3, 1])))
  expect_true(grepl("^Ctrl_", as.character(mat[5, 1])))
  expect_true(grepl("^Ctrl_", as.character(mat[8, 1])))
})

test_that("matrix_to_long produces correct long format", {
  type_mat <- create_type_matrix("rba", 2)
  id_mat <- create_id_matrix("rba", 2)
  dil_mat <- create_dilution_matrix()
  rep_mat <- create_replicate_matrix("rba")
  meas_mat <- create_plate_matrix(fill = 100)
  std_conc <- c(1e-6, 1e-7)

  result <- matrix_to_long(type_mat, id_mat, dil_mat, rep_mat, meas_mat, std_conc)

  expect_equal(nrow(result), 96)
  expect_true(all(c("Well", "SampleType", "MeasurementValue", "StandardConc") %in% names(result)))

  # S1 wells should all have concentration 1e-6
  s1_rows <- result[result$SampleID == "S1", ]
  expect_true(all(s1_rows$StandardConc == 1e-6))

  # Sample wells should have NA concentration
  sample_rows <- result[result$SampleType == "Sample", ]
  expect_true(all(is.na(sample_rows$StandardConc)))
})

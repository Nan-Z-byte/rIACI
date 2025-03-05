library(testthat)
library(rIACI)

test_that("process_data", {
  input_dir <- "/Users/nnnn/Phd/IACI_v1/cds_data"
  output_dir <- tempfile("processed_data_")
  dir.create(output_dir)

  process_data(input_dir = input_dir, output_dir = output_dir, save_merged = FALSE)

  output_files <- list.files(output_dir)

  expect_gt(length(output_files), 0)
  # expected_file <- file.path(output_dir, "processed_data.nc")
  # expect_true(file.exists(expected_file))
})

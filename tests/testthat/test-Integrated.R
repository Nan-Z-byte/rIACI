library(testthat)
library(rIACI)

test_that("Integrated test: export_data_to_csv, output_all, and csv_to_netcdf", {
  ## --- Step 3: Export Data to CSV ---
  netcdf_file <- "/Users/nnnn/Downloads/subset.nc"
  expect_true(file.exists(netcdf_file), info = "Processed NetCDF file does not exist. Check the path.")

  # Create a temporary directory to store CSV files exported by export_data_to_csv
  csv_output_dir <- tempfile("csv_output")
  dir.create(csv_output_dir)

  # Run export_data_to_csv to convert the NetCDF file into CSV files.
  # Your Python script (data_processing.py) should process the file and output actual data.
  expect_error(
    export_data_to_csv(nc_file = netcdf_file, output_dir = csv_output_dir),
    NA
  )

  # Check that CSV files were generated in csv_output_dir
  csv_files <- list.files(csv_output_dir, pattern = "\\.csv$", full.names = TRUE)
  expect_true(length(csv_files) > 0, info = "No CSV files were generated by export_data_to_csv.")

  # Verify that each exported CSV file contains the required columns: TMAX, TMIN, PRCP, WP, time
  required_cols <- c("TMAX", "TMIN", "PRCP", "WP", "time")
  for (file in csv_files) {
    df <- read.csv(file)
    missing_cols <- setdiff(required_cols, names(df))
    expect_true(length(missing_cols) == 0,
                info = paste("File", basename(file), "is missing columns:", paste(missing_cols, collapse = ", ")))
  }

  ## --- Step 4: Calculate IACI and Output Results ---
  # Use the CSV files from export_data_to_csv as the input for output_all.
  input_dir <- csv_output_dir

  # Construct sea level data.
  # Option 2: Generate a blank series for the full time range.
  sea_dates <- seq(as.Date("1960-01-01"), as.Date("2023-12-01"), by = "month")
  sea_dates <- format(sea_dates, "%Y-%m")
  sea_values <- rep(NA, length(sea_dates))
  si <- sea_input(Date = sea_dates, Value = sea_values)

  # Create a temporary directory for output_all results.
  output_dir <- tempfile("iaci_results")

  # Run output_all to calculate IACI (using monthly frequency in this example).
  # Adjust base.range and time.span as needed.
  expect_error(
    output_all(si = si,
               input_dir = input_dir,
               output_dir = output_dir,
               freq = "monthly",
               base.range = c(1961, 1990),
               time.span = c(1961, 2023)),
    NA
  )

  # Verify that the "monthly" subdirectory was created and contains CSV files.
  monthly_output_dir <- file.path(output_dir, "monthly")
  expect_true(dir.exists(monthly_output_dir), info = "output_all did not create a 'monthly' directory.")
  out_files <- list.files(monthly_output_dir, pattern = "\\.csv$")
  expect_true(length(out_files) > 0, info = "No CSV files generated in the monthly output directory.")

  # Load one of the output files and check that it contains an IACI column.
  sample_out <- read.csv(file.path(monthly_output_dir, out_files[1]))
  expect_true("IACI" %in% names(sample_out), info = "Output CSV file does not contain the IACI column.")

  ## --- Step 5: Merge CSVs into NetCDF ---
  # Use the CSV files from the output_all results (e.g., monthly output) as the input for csv_to_netcdf.
  merged_netcdf <- tempfile(fileext = ".nc")
  expect_error(
    csv_to_netcdf(csv_dir = monthly_output_dir, output_file = merged_netcdf, freq = "monthly"),
    NA
  )

  # Check that the merged NetCDF file exists.
  expect_true(file.exists(merged_netcdf), info = "csv_to_netcdf did not generate the merged NetCDF file.")
})

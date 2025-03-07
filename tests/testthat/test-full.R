library(testthat)
library(rIACI)
library(reticulate)


test_that("Set up Python virtual environment", {
  skip_on_cran()
  skip_if_not_installed("reticulate")

  if (!py_available(initialize = FALSE)) {
    skip("No Python available in this environment, skipping Python tests.")
  }

  env_dir <- file.path(tempdir(), "test_py_env")

  if (!dir.exists(env_dir)) {
    message("Creating Python virtual environment at: ", env_dir)
    virtualenv_create(env_dir)
    message("Installing Python packages...")
    py_install(
      packages = c("xarray", "pandas", "numpy", "netCDF4", "dask"),
      envname  = env_dir
    )
  }

  use_virtualenv(env_dir, required = TRUE)

  xarray <- import("xarray", convert = FALSE)
  expect_true(!is.null(xarray), "xarray should be available after installation.")
})

test_that("download_data", {
  skip_on_cran()
  start_year <- 2020
  end_year <- 2020
  start_month <- 1
  end_month <- 1
  variables <- c("2m_temperature", "total_precipitation")
  dataset <- "reanalysis-era5-land"
  area <- c(44, -10, 36, 4)
  output_dir <- tempfile("cds_data_")
  dir.create(output_dir)
  user_id <- Sys.getenv("ECMWF_USER_ID")
  user_key <- Sys.getenv("ECMWF_USER_KEY")
  if (user_id == "" || user_key == "") {
    skip("ECMWF_USER_ID or ECMWF_USER_KEY not set, skipping download test")
  }
  download_data(
    start_year = start_year,
    end_year = end_year,
    start_month = start_month,
    end_month = end_month,
    variables = variables,
    dataset = dataset,
    area = area,
    output_dir = output_dir,
    user_id = user_id,
    user_key = user_key,
    max_retries = 1,
    retry_delay = 0,
    timeout = 7200
  )
  expected_nc  <- file.path(output_dir, "2020_01.nc")
  expect_true(file.exists(expected_nc))
})

test_that("process_data", {
  skip_on_cran()
  input_dir <- system.file("extdata", "testdata", package = "rIACI")
  expect_true(nchar(input_dir) > 0, info = "Cannot locate the 'processtest' directory in extdata")
  output_dir <- tempfile("processed_data_")
  dir.create(output_dir)
  process_data(input_dir = input_dir, output_dir = output_dir, save_merged = FALSE)
  output_files <- list.files(output_dir)
  expect_gt(length(output_files), 0)
})

test_that("Check NetCDF subset file existence", {
  netcdf_file <- system.file("extdata", "subset.nc", package = "rIACI")
  expect_true(file.exists(netcdf_file))
})

test_that("export_data_to_csv test with Python", {
  skip_on_cran()
  skip_if_not_installed("reticulate")
  if (!reticulate::py_available(initialize = FALSE)) {
    skip("Python not available")
  }
  netcdf_file <- system.file("extdata", "subset.nc", package = "rIACI")
  expect_true(file.exists(netcdf_file))
  csv_output_dir <- tempfile("csv_output")
  dir.create(csv_output_dir)
  expect_error(
    export_data_to_csv(nc_file = netcdf_file, output_dir = csv_output_dir),
    NA
  )
  csv_files <- list.files(csv_output_dir, pattern = "\\.csv$", full.names = TRUE)
  expect_true(length(csv_files) > 0)
  required_cols <- c("TMAX", "TMIN", "PRCP", "WP", "time")
  for (file in csv_files) {
    df <- read.csv(file)
    missing_cols <- setdiff(required_cols, names(df))
    expect_true(length(missing_cols) == 0)
  }
})

test_that("output_all (monthly) test with Python", {
  skip_on_cran()
  skip_if_not_installed("reticulate")
  if (!reticulate::py_available(initialize = FALSE)) {
    skip("Python not available")
  }
  csv_output_dir <- tempfile("csv_output")
  dir.create(csv_output_dir)
  netcdf_file <- system.file("extdata", "subset.nc", package = "rIACI")
  export_data_to_csv(nc_file = netcdf_file, output_dir = csv_output_dir)
  sea_dates <- seq(as.Date("1960-01-01"), as.Date("2023-12-01"), by = "month")
  sea_dates <- format(sea_dates, "%Y-%m")
  sea_values <- rep(NA, length(sea_dates))
  si <- sea_input(Date = sea_dates, Value = sea_values)
  monthly_output_dir_parent <- tempfile("iaci_monthly_results")
  expect_error(
    output_all(
      si = si,
      input_dir = csv_output_dir,
      output_dir = monthly_output_dir_parent,
      freq = "monthly",
      base.range = c(1961, 1990),
      time.span = c(1961, 2023)
    ),
    NA
  )
  monthly_output_dir <- file.path(monthly_output_dir_parent, "monthly")
  expect_true(dir.exists(monthly_output_dir))
  monthly_files <- list.files(monthly_output_dir, pattern = "\\.csv$")
  expect_true(length(monthly_files) > 0)
  sample_monthly <- read.csv(file.path(monthly_output_dir, monthly_files[1]))
  expect_true("IACI" %in% names(sample_monthly))
})

test_that("output_all (seasonal) test with Python", {
  skip_on_cran()
  skip_if_not_installed("reticulate")
  if (!reticulate::py_available(initialize = FALSE)) {
    skip("Python not available")
  }
  csv_output_dir <- tempfile("csv_output")
  dir.create(csv_output_dir)
  netcdf_file <- system.file("extdata", "subset.nc", package = "rIACI")
  export_data_to_csv(nc_file = netcdf_file, output_dir = csv_output_dir)
  sea_dates <- seq(as.Date("1960-01-01"), as.Date("2023-12-01"), by = "month")
  sea_dates <- format(sea_dates, "%Y-%m")
  sea_values <- rep(NA, length(sea_dates))
  si <- sea_input(Date = sea_dates, Value = sea_values)
  seasonal_output_dir_parent <- tempfile("iaci_seasonal_results")
  expect_error(
    output_all(
      si = si,
      input_dir = csv_output_dir,
      output_dir = seasonal_output_dir_parent,
      freq = "seasonal",
      base.range = c(1961, 1990),
      time.span = c(1961, 2023)
    ),
    NA
  )
  seasonal_output_dir <- file.path(seasonal_output_dir_parent, "seasonal")
  expect_true(dir.exists(seasonal_output_dir))
  seasonal_files <- list.files(seasonal_output_dir, pattern = "\\.csv$")
  expect_true(length(seasonal_files) > 0)
  sample_seasonal <- read.csv(file.path(seasonal_output_dir, seasonal_files[1]))
  expect_true("IACI" %in% names(sample_seasonal))
})

test_that("csv_to_netcdf merges CSV into NetCDF with Python", {
  skip_on_cran()
  skip_if_not_installed("reticulate")
  if (!reticulate::py_available(initialize = FALSE)) {
    skip("Python not available")
  }
  csv_output_dir <- tempfile("csv_output")
  dir.create(csv_output_dir)
  netcdf_file <- system.file("extdata", "subset.nc", package = "rIACI")
  export_data_to_csv(nc_file = netcdf_file, output_dir = csv_output_dir)
  monthly_output_dir_parent <- tempfile("iaci_monthly_results")
  sea_dates <- seq(as.Date("1960-01-01"), as.Date("2023-12-01"), by = "month")
  sea_dates <- format(sea_dates, "%Y-%m")
  sea_values <- rep(NA, length(sea_dates))
  si <- sea_input(Date = sea_dates, Value = sea_values)
  output_all(
    si = si,
    input_dir = csv_output_dir,
    output_dir = monthly_output_dir_parent,
    freq = "monthly",
    base.range = c(1961, 1990),
    time.span = c(1961, 2023)
  )
  monthly_output_dir <- file.path(monthly_output_dir_parent, "monthly")
  merged_netcdf <- tempfile(fileext = ".nc")
  expect_error(
    csv_to_netcdf(csv_dir = monthly_output_dir, output_file = merged_netcdf, freq = "monthly"),
    NA
  )
  expect_true(file.exists(merged_netcdf))
})

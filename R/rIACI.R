#' @useDynLib rIACI
#' @importFrom Rcpp sourceCpp

# Package-level imports and global variables
#' @importFrom stats sd qnorm setNames approx filter
#' @importFrom utils read.csv write.csv globalVariables
#' @importFrom dplyr case_when mutate select summarise group_by ungroup left_join full_join
#' @importFrom tidyr separate
#' @importFrom lubridate ymd year month yday day
#' @importFrom magrittr %>%
NULL

utils::globalVariables(c(
  "Date", "Value", "Value.x", "Value.y", "tx90p", "tn90p",
  "t90p", "t10p", "month", "year", "season", "ref_mean", "ref_sd",
  "tx90_ref_sd", "tn90_ref_sd", "t90_ref_mean", "t90_ref_sd",
  "tx10_ref_sd", "tn10_ref_sd", "t10_ref_mean", "t10_ref_sd",
  "std", "month_day", "wind", "ci", "season_year", "value",
  "ref_df", "interpolated_values", "x", "y", "t90p", "t10p", "mean",
  "threshold", "adjusted_day_of_year", "dates_in_base", "data_in_base",
  "data_df", "all_true", "bools", "runmean", "lengths", "values",
  "rle_bools", "spell_lengths_at_ends", "success", "attempts",
  "temp_thresholds", "temp", "condition_met", "max_spell", "result",
  "prec_runsum", "start_date", "end_date", "date_sequence", "base.range",
  "base_range", "T90p", "T10p", "Rx5day", "CDD", "W90p", "Sea"
))

#' Download ERA5-Land Data
#'
#' Downloads ERA5-Land data from the ECMWF Climate Data Store for the specified time range and variables.
#' Implements a retry mechanism to handle transient errors during data download.
#'
#' @param start_year Integer. The starting year for data download.
#' @param end_year Integer. The ending year for data download.
#' @param start_month Integer. The starting month (default is 1).
#' @param end_month Integer. The ending month (default is 12).
#' @param variables Character vector. Variables to download.
#'   Default includes common variables: \code{c("10m_u_component_of_wind", "10m_v_component_of_wind",
#'   "2m_temperature", "total_precipitation")}.
#' @param dataset Character. Dataset short name (default is "reanalysis-era5-land").
#' @param area Numeric vector. Geographical area specified as \code{c(North, West, South, East)}.
#' @param output_dir Character. Directory to save the downloaded data (default is "cds_data").
#' @param user_id Character. Your ECMWF user ID.
#' @param user_key Character. Your ECMWF API key.
#' @param max_retries Integer. Maximum number of retry attempts in case of download failure (default is 3).
#' @param retry_delay Numeric. Delay between retry attempts in seconds (default is 5).
#' @param timeout Numeric. Timeout duration for each request in seconds (default is 7200, i.e., 2 hours).
#' @return None. Data is downloaded to the specified output directory.
#' @export
#' @importFrom ecmwfr wf_set_key wf_request
#' @examples
#' \dontrun{
#' # Set your ECMWF user ID and key
#' user_id <- "your_user_id"
#' user_key <- "your_api_key"
#'
#' # Define the geographical area (North, West, South, East)
#' area <- c(90, -180, -90, 180) # Global
#'
#' # Download data for 2020
#' download_data(
#'   start_year = 2020,
#'   end_year = 2020,
#'   variables = c("2m_temperature", "total_precipitation"),
#'   area = area,
#'   user_id = user_id,
#'   user_key = user_key
#' )
#' }
download_data <- function(start_year, end_year,
                          start_month = 1,
                          end_month = 12,
                          variables = c("10m_u_component_of_wind", "10m_v_component_of_wind", "2m_temperature", "total_precipitation"),
                          dataset = "reanalysis-era5-land",
                          area = c(-90, -180, 90, 180),
                          output_dir = "cds_data",
                          user_id, user_key,
                          max_retries = 3,
                          retry_delay = 5,
                          timeout = 7200) {

  # Validate input parameters
  if (start_year > end_year || start_month < 1 || start_month > 12 || end_month < 1 || end_month > 12) {
    stop("Invalid year or month range.")
  }

  # Set the ECMWF API key
  ecmwfr::wf_set_key(user = user_id, key = user_key)

  # Ensure the output directory exists
  if (!dir.exists(output_dir)) {
    dir.create(output_dir, recursive = TRUE)
  }

  # Define fixed time range for every hour of each day
  times <- sprintf("%02d:00", 0:23)

  # Loop through years and months
  for (year in as.character(seq(start_year, end_year))) {
    for (month in as.character(seq(start_month, end_month))) {
      # Define the request payload
      request <- list(
        variable = variables,
        year = year,
        month = month,
        day = as.character(seq(1, 31)),
        time = times,
        area = area,
        data_format = "netcdf",
        download_format = "unarchived",
        dataset_short_name = dataset,
        target = paste0(year, "_", sprintf("%02s", month), ".nc")
      )

      # Initialize retry mechanism
      success <- FALSE
      attempts <- 0

      while (!success && attempts < max_retries) {
        tryCatch({
          # Attempt to make the request
          ecmwfr::wf_request(user = user_id, request = request, path = output_dir, transfer = TRUE, time_out = timeout)
          success <- TRUE # Exit loop on successful download
        }, error = function(e) {
          attempts <- attempts + 1
          if (attempts < max_retries) {
            message(paste("Error encountered during download:", e$message))
            message(paste("Retrying attempt", attempts, "after delay of", retry_delay, "seconds..."))
            Sys.sleep(retry_delay) # Wait before retrying
          } else {
            stop("Maximum retry attempts reached. Download failed.")
          }
        })
      }
    }
  }
}


#' Process Data Function
#'
#' Processes NetCDF files in the input directory and saves merged and processed data to the output directory.
#'
#' @param input_dir Character. Directory containing input NetCDF files.
#' @param output_dir Character. Directory to save output files.
#' @param save_merged Logical. If TRUE, saves the merged NetCDF file. Default is FALSE.
#' @return None. Outputs are saved to the specified directory.
#' @export
#' @examples
#' \dontrun{
#' # Example usage of process_data
#' input_directory <- "/path/to/input/netcdf_files"
#' output_directory <- "/path/to/output"
#' process_data(input_dir = input_directory, output_dir = output_directory, save_merged = TRUE)
#' }
process_data <- function(input_dir, output_dir, save_merged = FALSE) {

  python_script <- system.file("python", "data_processing.py", package = "rIACI")
  reticulate::source_python(python_script)

  reticulate::py$process_data(input_dir = input_dir, output_dir = output_dir, save_merged = save_merged)
}


#' Export Data to CSV Function
#'
#' Exports data from a NetCDF file to CSV files, one for each latitude and longitude point, including only points where data is present.
#' This function utilizes a Python script to perform the data processing.
#'
#' @param nc_file Character. Path to the NetCDF file.
#' @param output_dir Character. Output directory where CSV files will be saved.
#' @return None. CSV files are saved to the specified output directory.
#' @details
#' The function calls a Python script using the `reticulate` package to process the NetCDF file.
#' The Python script `data_processing.py` should be located in the `python` directory of the `rIACI` package.
#' Only grid points with available data are exported to CSV files.
#' Each CSV file corresponds to a specific latitude and longitude point.
#'
#' @examples
#' \dontrun{
#' # Example usage of export_data_to_csv
#' netcdf_file <- "/path/to/processed_data.nc"
#' csv_output_directory <- "/path/to/csv_output"
#' export_data_to_csv(nc_file = netcdf_file, output_dir = csv_output_directory)
#' }
#' @export
export_data_to_csv <- function(nc_file, output_dir) {

  python_script <- system.file("python", "data_processing.py", package = "rIACI")
  reticulate::source_python(python_script)

  reticulate::py$export_data_to_csv(nc_file = nc_file, output_dir = output_dir)
}

#' CSV to NetCDF Function
#'
#' Merges CSV files in a specified directory into a single NetCDF file, completing the grid by filling missing values.
#'
#' @param csv_dir Character. Directory containing CSV files, each file representing a single latitude-longitude point.
#'   The filename format should be 'lat_lon.csv'.
#' @param output_file Character. Path to the output NetCDF file.
#' @param freq Character. Frequency of the data, either `'monthly'` or `'seasonal'`.
#'   - `'monthly'` data uses date format `'YYYY-MM'`.
#'   - `'seasonal'` data uses date format like `'YYYY-SSS'` (e.g., `'1961-DJF'`).
#' @return None. The NetCDF file is saved to the specified location.
#' @export
#' @examples
#' \dontrun{
#' # Example usage of csv_to_netcdf
#' csv_directory <- "/path/to/csv_files"
#' output_netcdf_file <- "/path/to/output_file.nc"
#' csv_to_netcdf(csv_dir = csv_directory, output_file = output_netcdf_file, freq = "monthly")
#' }
csv_to_netcdf <- function(csv_dir, output_file, freq) {

  python_script <- system.file("python", "data_processing.py", package = "rIACI")
  reticulate::source_python(python_script)

  reticulate::py$csv_to_netcdf(csv_dir = csv_dir, output_file = output_file, freq = freq)
}

#' Check Validity of Basic Arguments
#'
#' Validates the input parameters to ensure they are valid and consistent.
#'
#' @param tmax Numeric vector. Maximum temperature data.
#' @param tmin Numeric vector. Minimum temperature data.
#' @param prec Numeric vector. Precipitation data.
#' @param wind Numeric vector. Wind data.
#' @param dates Date vector. Dates corresponding to the data.
#' @param base.range Numeric vector of length 2. The base range years.
#' @param n Numeric. A parameter used in calculations.
#' @return None. Stops execution if validation fails.
#' @keywords internal
check_basic_argument_validity <- function(tmax, tmin, prec, wind, dates, base.range, n) {
  if (is.null(dates)) {
    stop("dates vector must be provided and cannot be NULL.")
  }

  if (!inherits(dates, "Date")) {
    stop("dates vector must be of class Date.")
  }

  if (length(tmax) != length(dates) && !is.null(tmax)) {
    stop("Length of tmax and dates do not match.")
  }

  if (length(tmin) != length(dates) && !is.null(tmin)) {
    stop("Length of tmin and dates do not match.")
  }

  if (length(prec) != length(dates) && !is.null(prec)) {
    stop("Length of prec and dates do not match.")
  }

  if (length(wind) != length(dates) && !is.null(wind)) {
    stop("Length of wind and dates do not match.")
  }

  if (all(sapply(list(tmax, tmin, prec, wind), is.null))) {
    stop("At least one of tmax, tmin, prec, or wind must be provided.")
  }

  if (!(length(base.range) == 2 && is.numeric(base.range))) {
    stop("Invalid base.range; expecting a numeric vector of length 2.")
  }

  if (!is.numeric(n) || length(n) != 1) {
    stop("n must be a numeric value of length 1.")
  }
}

#' Create Date Sequence
#'
#' Generates a complete sequence of dates from the earliest to the latest date provided.
#'
#' @param dates Date vector. Original dates.
#' @return Date vector. A complete sequence of dates.
#' @keywords internal
create_date_sequence <- function(dates) {
  start_date <- min(dates)
  end_date <- max(dates)
  date_sequence <- seq.Date(from = start_date, to = end_date, by = "day")
  return(date_sequence)
}

#' Fill Data Series
#'
#' Aligns the input data with the complete date sequence, filling missing dates with NA.
#'
#' @param data Numeric vector. Original data series.
#' @param dates Date vector. Dates corresponding to the data.
#' @param date_sequence Date vector. Complete date sequence.
#' @return Numeric vector. Data series aligned with the date sequence.
#' @keywords internal
fill_data_series <- function(data, dates, date_sequence) {
  data_series <- rep(NA, length(date_sequence))
  date_indices <- match(dates, date_sequence)
  data_series[date_indices] <- data
  return(data_series)
}

#' Calculate Temperature Quantiles
#'
#' Computes temperature quantiles based on data within a specified base range.
#'
#' @param data_series Numeric vector. Data series aligned with date sequence.
#' @param date_sequence Date vector. Complete date sequence.
#' @param base.range Numeric vector. Base range years.
#' @param n Integer. Window size for running averages.
#' @param temp.qtiles Numeric vector. Quantiles to calculate.
#' @param min.base.data.fraction.present Numeric. Minimum fraction of data required in base range.
#' @return Data frame. Calculated quantiles.
#' @keywords internal
#' @importFrom Rcpp sourceCpp

calculate_quantiles <- function(data_series, date_sequence, base.range, n, temp.qtiles, min.base.data.fraction.present) {
  years <- year(date_sequence)
  in_base <- years >= base.range[1] & years <= base.range[2]

  if (sum(in_base) == 0) {
    stop("No data in the specified base range.")
  }

  data_in_base <- data_series[in_base]
  dates_in_base <- date_sequence[in_base]

  # Adjust day of the year to handle leap years
  adjusted_day_of_year <- sapply(dates_in_base, function(date) {
    doy <- yday(date)
    if (month(date) == 2 && day(date) == 29) {
      return(59.5)
    } else {
      return(doy)
    }
  })

  # Call the C++ function to compute running quantiles
  quantiles_df <- running_quantile_cpp(
    data = data_in_base,
    adjusted_day_of_year = adjusted_day_of_year,
    q = temp.qtiles,
    min_fraction = min.base.data.fraction.present
  )

  return(quantiles_df)
}

#' Calculate Wind Quantiles
#'
#' Computes the 90th percentile of wind speed for index calculations.
#'
#' @param wind_data_series Numeric vector. Wind data series aligned with date sequence.
#' @param date_sequence Date vector. Complete date sequence.
#' @param base_range Numeric vector. Base range years.
#' @param wind_qtile Numeric. Wind quantile to calculate (default is 0.90).
#' @return Data frame. Calculated wind thresholds.
#' @keywords internal
#' @importFrom dplyr mutate select summarise group_by ungroup

calculate_wind_quantiles <- function(wind_data_series, date_sequence, base_range, wind_qtile = 0.90) {
  data_df <- data.frame(date = date_sequence, wind = wind_data_series)

  data_df$month_day <- format(data_df$date, '%m-%d')

  ref_period <- data_df$date >= as.Date(paste0(base_range[1], '-01-01')) & data_df$date <= as.Date(paste0(base_range[2], '-12-31'))
  wind_ref_df <- data_df[ref_period, ]

  wind_ref_stats <- wind_ref_df %>%
    group_by(month_day) %>%
    summarise(mean = mean(wind, na.rm = TRUE),
              std = sd(wind, na.rm = TRUE)) %>%
    ungroup() %>%
    mutate(threshold = mean + qnorm(wind_qtile) * std)

  threshold <- wind_ref_stats %>% select(month_day, threshold)

  return(threshold)
}

#' Generate NA Masks
#'
#' Creates masks for annual and monthly data based on maximum allowed missing days.
#'
#' @param data_series_list List. List of data series.
#' @param date_sequence Date vector. Complete date sequence.
#' @param max.missing.days Named numeric vector. Maximum allowed missing days.
#' @return List. NA masks for annual and monthly data.
#' @keywords internal
generate_na_masks <- function(data_series_list, date_sequence, max.missing.days) {
  years <- year(date_sequence)
  months <- month(date_sequence)

  date_factors <- list(
    annual = factor(format(date_sequence, '%Y')),
    monthly = factor(format(date_sequence, '%Y-%m'))
  )

  na_masks <- list(annual = list(), monthly = list())

  for (var_name in names(data_series_list)) {
    data_series <- data_series_list[[var_name]]

    annual_na_counts <- tapply(is.na(data_series), date_factors$annual, sum)
    annual_na_masks <- ifelse(annual_na_counts <= max.missing.days["annual"], 1, NA)
    na_masks$annual[[var_name]] <- annual_na_masks

    monthly_na_counts <- tapply(is.na(data_series), date_factors$monthly, sum)
    monthly_na_masks <- ifelse(monthly_na_counts <= max.missing.days["monthly"], 1, NA)
    na_masks$monthly[[var_name]] <- monthly_na_masks
  }

  return(na_masks)
}

#' Climate Input Function
#'
#' Processes climate data and calculates necessary statistics for climate index calculations.
#'
#' @param tmax Numeric vector. Maximum temperature data.
#' @param tmin Numeric vector. Minimum temperature data.
#' @param prec Numeric vector. Precipitation data.
#' @param wind Numeric vector. Wind speed data.
#' @param dates Date vector. Dates corresponding to the data.
#' @param base.range Numeric vector of length 2. Base range years for calculations (default is c(1961, 1990)).
#' @param n Integer. Window size for running averages (default is 5).
#' @param quantiles List. Pre-calculated quantiles (optional).
#' @param temp.qtiles Numeric vector. Temperature quantiles to calculate (default is c(0.10, 0.90)).
#' @param wind.qtile Numeric. Wind quantile to calculate (default is 0.90).
#' @param max.missing.days Named numeric vector. Maximum allowed missing days for annual and monthly data (default is c(annual = 15, monthly = 3)).
#' @param min.base.data.fraction.present Numeric. Minimum fraction of data required in base range (default is 0.1).
#' @return A list containing processed data and related information for climate index calculations.
#' @export
#' @importFrom lubridate ymd year month yday day
#' @importFrom dplyr mutate select group_by summarise ungroup left_join
#' @importFrom tidyr separate
#' @importFrom Rcpp sourceCpp
#' @useDynLib rIACI
#' @examples
#' \dontrun{
#' # Create climate input object
#' ci <- climate_input(
#'   tmax = tmax,
#'   tmin = tmin,
#'   prec = prec,
#'   wind = wind,
#'   dates = dates
#' )
#' }
climate_input <- function(tmax = NULL, tmin = NULL, prec = NULL, wind = NULL, dates = NULL,
                          base.range = c(1961, 1990), n = 5, quantiles = NULL,
                          temp.qtiles = c(0.10, 0.90), wind.qtile = 0.90,
                          max.missing.days = c(annual = 15, monthly = 3),
                          min.base.data.fraction.present = 0.1) {
  dates <- ymd(dates)

  # Check basic argument validity
  check_basic_argument_validity(tmax, tmin, prec, wind, dates, base.range, n)
  # Create complete date sequence
  date_sequence <- create_date_sequence(dates)
  # Fill data series
  data_series_list <- list()
  if (!is.null(tmax)) {
    data_series_list$tmax <- fill_data_series(tmax, dates, date_sequence)
  }
  if (!is.null(tmin)) {
    data_series_list$tmin <- fill_data_series(tmin, dates, date_sequence)
  }
  if (!is.null(prec)) {
    data_series_list$prec <- fill_data_series(prec, dates, date_sequence)
  }
  if (!is.null(wind)) {
    data_series_list$wind <- fill_data_series(wind, dates, date_sequence)
  }
  # Calculate quantiles
  if (is.null(quantiles)) {
    quantiles <- list()
    if (!is.null(tmax)) {
      quantiles$tmax <- calculate_quantiles(data_series_list$tmax, date_sequence, base.range, n, temp.qtiles, min.base.data.fraction.present)
    }
    if (!is.null(tmin)) {
      quantiles$tmin <- calculate_quantiles(data_series_list$tmin, date_sequence, base.range, n, temp.qtiles, min.base.data.fraction.present)
    }
    if (!is.null(wind)) {
      quantiles$wind <- calculate_wind_quantiles(data_series_list$wind, date_sequence, base.range, wind.qtile)
    }
  }
  # Generate NA masks
  na_masks <- generate_na_masks(data_series_list, date_sequence, max.missing.days)

  years <- year(date_sequence)
  months <- month(date_sequence)
  date_factors <- list(
    annual = factor(years),
    monthly = factor(format(date_sequence, '%Y-%m'))
  )
  month_day <- format(date_sequence, '%m-%d')
  # Create climate input object
  climate_input <- list(
    data = data_series_list,
    quantiles = quantiles,
    na_masks = na_masks,
    dates = date_sequence,
    month_day = month_day,
    date_factors = date_factors,
    base_range = base.range,
    max_missing_days = max.missing.days
  )

  return(climate_input)
}



#' Fast tapply Function
#'
#' Provides a faster implementation of tapply for factor-type indices.
#'
#' @param X Numeric vector. Data to be applied.
#' @param INDEX Factor. Indexing factor.
#' @param FUN Function. Function to apply.
#' @param simplify Logical. Whether to simplify the result.
#' @return List or vector. Result of applying FUN to X grouped by INDEX.
#' @keywords internal
tapply.fast <- function (X, INDEX, FUN = NULL, ..., simplify = TRUE) {
  FUN <- if (!is.null(FUN))
    match.fun(FUN)

  if(!is.factor(INDEX))
    stop("INDEX must be a factor.")

  if (length(INDEX) != length(X))
    stop("arguments must have same length")

  if (is.null(FUN))
    return(INDEX)

  namelist <- levels(INDEX)
  ans <- lapply(split(X, INDEX), FUN, ...)

  ans <- unlist(ans, recursive = FALSE)
  names(ans) <- levels(INDEX)
  return(ans)
}

#' Percent of Days Above or Below Threshold
#'
#' Calculates the percentage of days exceeding or below a threshold.
#'
#' @param temp Numeric vector. Temperature data.
#' @param dates Date vector. Dates corresponding to the data.
#' @param month_day Character vector. Month-day strings.
#' @param date_factor Factor. Date grouping factor (e.g., monthly or annual).
#' @param quantiles_df Data frame. Quantiles data.
#' @param qtile Numeric. Quantile to use.
#' @param op Character. Comparison operator, e.g., '<' or '>'.
#' @param max_missing_days Named numeric vector. Maximum allowed missing days.
#' @return Numeric vector or data frame with calculated percentages.
#' @keywords internal
#' @importFrom stats na.omit

percent_days_op_threshold <- function(temp, dates, month_day, date_factor,
                                      quantiles_df, qtile, op = '<', max_missing_days) {
  compare_fun <- match.fun(op)

  # Construct the column name for the desired quantile
  col_name <- paste0("Q", sprintf("%.1f", qtile * 100))

  # Ensure the quantile column exists
  if (!(col_name %in% names(quantiles_df))) {
    stop(paste("Column", col_name, "not found in quantiles_df"))
  }

  # Extract thresholds and assign names based on month_day
  thresholds <- setNames(quantiles_df[[col_name]], quantiles_df$month_day)

  # Match thresholds to the data's month_day
  temp_thresholds <- thresholds[month_day]

  # Compare temperatures to thresholds
  condition_met <- compare_fun(temp, temp_thresholds)

  # Replace NaN with NA
  condition_met[is.nan(condition_met)] <- NA

  # If date_factor is missing or NULL, return the condition_met vector
  if (missing(date_factor) || is.null(date_factor)) {
    return(condition_met)
  }

  # Ensure date_factor is a factor (required for tapply.fast)
  if (!is.factor(date_factor)) {
    date_factor <- factor(date_factor)
  }

  # Calculate the percentage of days meeting the condition per date_factor using tapply.fast
  result <- tapply.fast(condition_met, date_factor, function(x) {
    mean(x, na.rm = TRUE) * 100
  })

  # Replace NaN with NA
  result[is.nan(result)] <- NA

  return(result)
}

#' Maximum Length of Consecutive Dry Days
#'
#' Calculates the maximum length of consecutive dry days.
#'
#' @param daily_prec Numeric vector. Daily precipitation data.
#' @param date_factor Factor. Date grouping factor.
#' @param threshold Numeric. Precipitation threshold.
#' @param op Character. Comparison operator.
#' @param spells_can_span_years Logical. Whether spells can span across years.
#' @return Numeric vector. Maximum spell lengths.
#' @keywords internal
spell_length_max <- function(daily_prec, date_factor, threshold, op, spells_can_span_years) {
  compare_fun <- match.fun(op)
  bools <- compare_fun(daily_prec, threshold)

  if (spells_can_span_years) {
    # Calculate whether all values in each group are TRUE
    all_true <- tapply.fast(bools, date_factor, all)

    # Calculate maximum spell length at the ends
    spell_lengths_at_ends <- get.series.lengths.at.ends(bools)
    max_spell <- tapply.fast(spell_lengths_at_ends, date_factor, max, na.rm = TRUE)

    # Mask out values which are in the middle of a spell with NA
    na_mask <- ifelse((max_spell == 0) & all_true, NA, 1)

    result <- max_spell * na_mask
  } else {
    # Calculate maximum spell length within each group
    result <- tapply.fast(bools, date_factor, function(x) {
      max(get_series_lengths(x), na.rm = TRUE)
    })
  }

  return(result)
}


#' Maximum Precipitation Over Consecutive Days
#'
#' Calculates maximum precipitation over consecutive n days.
#'
#' @param daily_prec Numeric vector. Daily precipitation data.
#' @param date_factor Factor. Date grouping factor.
#' @param ndays Integer. Number of consecutive days.
#' @param center_mean_on_last_day Logical. Whether to center the mean on the last day.
#' @return Numeric vector. Maximum precipitation amounts.
#' @keywords internal
#' @importFrom stats filter
nday_consec_prec_max <- function(daily_prec, date_factor, ndays, center_mean_on_last_day = FALSE) {
  if (ndays == 1) {
    return(tapply.fast(daily_prec, date_factor, max, na.rm = TRUE))
  }

  # Replace NAs with 0
  daily_prec[is.na(daily_prec)] <- 0

  # Calculate running sum (since we want the sum over ndays)
  prec_runsum <- running_mean(daily_prec, ndays) * ndays

  if (center_mean_on_last_day) {
    k2 <- ndays %/% 2
    prec_runsum <- c(rep(0, k2), prec_runsum[1:(length(prec_runsum) - k2)])
  }

  # Aggregate by date factor using tapply.fast
  result <- tapply.fast(prec_runsum, date_factor, max, na.rm = TRUE)

  return(result)
}

#' Running Mean
#'
#' Calculates the running mean of a vector over a specified window size.
#'
#' @param vec Numeric vector. Input data.
#' @param bin Integer. Window size.
#' @return Numeric vector. Running mean values.
#' @keywords internal
running_mean <- function(vec, bin) {
  vec <- as.vector(vec)
  len <- length(vec)

  if (bin <= 1) {
    return(vec)
  }
  if (bin > len) {
    bin <- len
  }

  left_bin <- bin %/% 2
  right_bin <- bin - left_bin - 1

  # Calculate the running sum
  runsum <- stats::filter(vec, rep(1, bin), sides = 1)

  # Adjust for initial NA values
  runmean <- runsum / bin
  runmean[1:(bin - 1)] <- NA

  return(runmean)
}

#' Get Series Lengths at Ends
#'
#' Calculates the lengths of runs at the ends of a logical vector.
#'
#' @param x Logical vector.
#' @param na.value Logical. Value to assign to NA elements.
#' @return Numeric vector. Lengths of runs at the ends.
#' @keywords internal
get.series.lengths.at.ends <- function(x, na.value = FALSE) {
  stopifnot(is.logical(x) && is.logical(na.value))
  n <- length(x)
  if (n == 1)
    return(as.numeric(x))

  res <- rep(0, n)
  x[is.na(x)] <- na.value

  # Identify starts and ends of TRUE runs
  start <- which(x & !c(FALSE, x[-n]))
  end <- which(x & !c(x[-1], FALSE))
  res[end] <- end - start + 1

  return(res)
}

#' Get Series Lengths
#'
#' Calculates the lengths of consecutive TRUE values in a logical vector.
#'
#' @param bools Logical vector.
#' @return Numeric vector. Lengths of consecutive TRUE runs.
#' @keywords internal
get_series_lengths <- function(bools) {
  rle_bools <- rle(bools)
  lengths <- rle_bools$lengths
  values <- rle_bools$values

  # Return lengths only for TRUE runs
  true_lengths <- lengths[values]

  return(true_lengths)
}

# Functions to calculate various climate indices
#' Calculate TX90p Index
#'
#' Calculates the percentage of days when maximum temperature is above the 90th percentile.
#'
#' @param ci List. Climate input object created by \code{\link{climate_input}}.
#' @param freq Character. Frequency of calculation, either "monthly" or "annual".
#' @return Data frame with dates and calculated TX90p values.
#' @export
#' @importFrom dplyr mutate select rename
#' @examples
#' \dontrun{
#' # Calculate monthly TX90p index
#' tx90p_values <- tx90p(ci, freq = "monthly")
#' }


tx90p <- function(ci, freq = c("monthly", "annual")) {
  freq <- match.arg(freq)

  result <- percent_days_op_threshold(
    temp = ci$data$tmax,
    dates = ci$dates,
    month_day = ci$month_day,
    date_factor = ci$date_factors[[freq]],
    quantiles_df = ci$quantiles$tmax,
    qtile = 0.90,
    op = ">"
  )

  # Apply NA mask if available
  na_mask <- ci$na_masks[[freq]]$tmax
  if (!is.null(na_mask)) {
    # Ensure names align
    names(result) <- names(na_mask)
    result <- result * na_mask
  }

  # Convert result to a data frame with date labels
  if (!is.null(names(result))) {
    result_df <- data.frame(Date = names(result), Value = as.numeric(result))
  } else {
    # If result has no names, this block can create a sequence based on the input data or handle the case appropriately
    result_df <- data.frame(Date = seq_along(result), Value = as.numeric(result))
  }

  return(result_df)
}
#' Calculate TX10p Index
#'
#' Calculates the percentage of days when maximum temperature is below the 10th percentile.
#'
#' @param ci List. Climate input object created by \code{\link{climate_input}}.
#' @param freq Character. Frequency of calculation, either "monthly" or "annual".
#' @return Data frame with dates and calculated TX10p values.
#' @export
#' @importFrom dplyr mutate select rename
#' @examples
#' \dontrun{
#' # Calculate monthly TX10p index
#' tx10p_values <- tx10p(ci, freq = "monthly")
#' }

tx10p <- function(ci, freq = c("monthly", "annual")) {
  freq <- match.arg(freq)

  result <- percent_days_op_threshold(
    temp = ci$data$tmax,
    dates = ci$dates,
    month_day = ci$month_day,
    date_factor = ci$date_factors[[freq]],
    quantiles_df = ci$quantiles$tmax,
    qtile = 0.10,
    op = "<"
  )

  # Apply NA mask if available
  na_mask <- ci$na_masks[[freq]]$tmax
  if (!is.null(na_mask)) {
    # Ensure names align
    names(result) <- names(na_mask)
    result <- result * na_mask
  }

  # Convert result to a data frame with date labels
  if (!is.null(names(result))) {
    result_df <- data.frame(Date = names(result), Value = as.numeric(result))
  } else {
    # If result has no names, this block can create a sequence based on the input data or handle the case appropriately
    result_df <- data.frame(Date = seq_along(result), Value = as.numeric(result))
  }

  return(result_df)
}

#' Calculate TN90p Index
#'
#' Calculates the percentage of days when minimum temperature is above the 90th percentile.
#'
#' @param ci List. Climate input object created by \code{\link{climate_input}}.
#' @param freq Character. Frequency of calculation, either "monthly" or "annual".
#' @return Data frame with dates and calculated TN90p values.
#' @export
#' @importFrom dplyr mutate select rename
#' @examples
#' \dontrun{
#' # Calculate monthly TN90p index
#' tn90p_values <- tn90p(ci, freq = "monthly")
#' }


tn90p <- function(ci, freq = c("monthly", "annual")) {
  freq <- match.arg(freq)

  result <- percent_days_op_threshold(
    temp = ci$data$tmin,
    dates = ci$dates,
    month_day = ci$month_day,
    date_factor = ci$date_factors[[freq]],
    quantiles_df = ci$quantiles$tmin,
    qtile = 0.90,
    op = ">"
  )

  # Apply NA mask if available
  na_mask <- ci$na_masks[[freq]]$tmin
  if (!is.null(na_mask)) {
    # Ensure names align
    names(result) <- names(na_mask)
    result <- result * na_mask
  }

  # Convert result to a data frame with date labels
  if (!is.null(names(result))) {
    result_df <- data.frame(Date = names(result), Value = as.numeric(result))
  } else {
    # If result has no names, this block can create a sequence based on the input data or handle the case appropriately
    result_df <- data.frame(Date = seq_along(result), Value = as.numeric(result))
  }

  return(result_df)
}
#' Calculate TN10p Index
#'
#' Calculates the percentage of days when minimum temperature is below the 10th percentile.
#'
#' @param ci List. Climate input object created by \code{\link{climate_input}}.
#' @param freq Character. Frequency of calculation, either "monthly" or "annual".
#' @return Data frame with dates and calculated TN10p values.
#' @export
#' @importFrom dplyr mutate select rename
#' @examples
#' \dontrun{
#' # Calculate monthly TN10p index
#' tn10p_values <- tn10p(ci, freq = "monthly")
#' }


tn10p <- function(ci, freq = c("monthly", "annual")) {
  freq <- match.arg(freq)

  result <- percent_days_op_threshold(
    temp = ci$data$tmin,
    dates = ci$dates,
    month_day = ci$month_day,
    date_factor = ci$date_factors[[freq]],
    quantiles_df = ci$quantiles$tmin,
    qtile = 0.10,
    op = "<"
  )

  # Apply NA mask if available
  na_mask <- ci$na_masks[[freq]]$tmin
  if (!is.null(na_mask)) {
    # Ensure names align
    names(result) <- names(na_mask)
    result <- result * na_mask
  }
  # Convert result to a data frame with date labels
  if (!is.null(names(result))) {
    result_df <- data.frame(Date = names(result), Value = as.numeric(result))
  } else {
    # If result has no names, this block can create a sequence based on the input data or handle the case appropriately
    result_df <- data.frame(Date = seq_along(result), Value = as.numeric(result))
  }

  return(result_df)
}
#' Calculate Rx5day Index
#'
#' Calculates the maximum consecutive 5-day precipitation amount.
#'
#' @param ci List. Climate input object created by \code{\link{climate_input}}.
#' @param freq Character. Frequency of calculation, either "monthly" or "annual".
#' @param center_mean_on_last_day Logical. Whether to center the mean on the last day (default is FALSE).
#' @return Data frame with dates and calculated Rx5day values.
#' @export
#' @importFrom dplyr mutate select rename
#' @examples
#' \dontrun{
#' # Calculate monthly Rx5day index
#' rx5day_values <- rx5day(ci, freq = "monthly")
#' }


rx5day <- function(ci, freq = c("monthly", "annual"), center_mean_on_last_day = FALSE) {
  freq <- match.arg(freq)

  if (is.null(ci$data$prec)) {
    stop("Precipitation data (prec) is missing in the climate input object.")
  }

  result <- nday_consec_prec_max(
    daily_prec = ci$data$prec,
    date_factor = ci$date_factors[[freq]],
    ndays = 5,
    center_mean_on_last_day = center_mean_on_last_day
  )

  # Apply NA mask if available
  na_mask <- ci$na_masks[[freq]]$prec
  if (!is.null(na_mask)) {
    # Ensure names align
    names(result) <- names(na_mask)
    result <- result * na_mask
  }

  # Convert result to a data frame with date labels
  if (!is.null(names(result))) {
    result_df <- data.frame(Date = names(result), Value = as.numeric(result))
  } else {
    # If result has no names, this block can create a sequence based on the input data or handle the case appropriately
    result_df <- data.frame(Date = seq_along(result), Value = as.numeric(result))
  }

  return(result_df)
}
#' Calculate Consecutive Dry Days (CDD)
#'
#' Calculates the maximum length of consecutive dry days.
#'
#' @param ci List. Climate input object created by \code{\link{climate_input}}.
#' @param spells_can_span_years Logical. Whether spells can span across years (default is TRUE).
#' @param monthly Logical. Whether to interpolate annual data to monthly data (default is TRUE).
#' @return Data frame with dates and calculated CDD values.
#' @export
#' @importFrom dplyr mutate select full_join
#' @importFrom tidyr separate
#' @examples
#' \dontrun{
#' # Calculate annual CDD index
#' cdd_values <- cdd(ci, monthly = FALSE)
#' }

cdd <- function(ci, spells_can_span_years = TRUE, monthly = TRUE) {
  if (is.null(ci$data$prec)) {
    stop("Precipitation data (prec) is missing in the climate input object.")
  }

  result <- spell_length_max(
    daily_prec = ci$data$prec,
    date_factor = ci$date_factors$annual,
    threshold = 1,
    op = "<",
    spells_can_span_years = spells_can_span_years
  )

  # Apply NA mask if available
  na_mask <- ci$na_masks$annual$prec
  if (!is.null(na_mask)) {
    # Ensure names align
    names(result) <- names(na_mask)
    result <- result * na_mask
  }
  if (monthly) {
    # Linearly interpolate annual data to monthly data
    # Retrieve years and corresponding CDD values
    years <- as.numeric(names(result))
    values <- as.numeric(result)

    # Create time series of annual data
    x <- years + 11 / 12
    y <- values

    # Create monthly time series (as decimal years)
    start_year <- min(years)
    end_year <- max(years)
    months_seq <- seq.Date(from = as.Date(paste0(start_year, "-01-01")),
                           to = as.Date(paste0(end_year, "-12-01")),
                           by = "month")
    months_year <- as.numeric(format(months_seq, "%Y"))
    months_month <- as.numeric(format(months_seq, "%m"))
    months_decimal <- months_year + (months_month - 1) / 12
    months_labels <- format(months_seq, "%Y-%m")

    # Perform linear interpolation
    interpolated_values <- approx(x = x, y = y, xout = months_decimal, rule = 2)$y

    # Return monthly data as a data frame
    return(data.frame(Date = months_labels, Value = interpolated_values))
  } else {
    # Return original annual data as a data frame
    return(data.frame(Date = names(result), Value = as.numeric(result)))
  }
}

#' Calculate W90p Index
#'
#' Calculates the percentage of days when wind speed is above the 90th percentile.
#'
#' @param ci List. Climate input object created by \code{\link{climate_input}}.
#' @param freq Character. Frequency of calculation, either "monthly" or "annual".
#' @return Data frame with dates and calculated W90p values.
#' @export
#' @importFrom dplyr mutate select
#' @examples
#' \dontrun{
#' # Calculate monthly W90p index
#' w90p_values <- w90p(ci, freq = "monthly")
#' }

w90p <- function(ci, freq = c("monthly", "annual")) {
  freq <- match.arg(freq)

  if (is.null(ci$data$wind)) {
    stop("Wind data (wind) is missing in the climate input object.")
  }

  # Extract wind thresholds and assign names based on month_day
  quantiles_df <- ci$quantiles$wind
  thresholds <- setNames(quantiles_df$threshold, quantiles_df$month_day)

  # Match thresholds to the data's month_day
  temp_thresholds <- thresholds[ci$month_day]

  # Compare wind speeds to thresholds
  condition_met <- ci$data$wind > temp_thresholds

  # Replace NaN with NA
  condition_met[is.nan(condition_met)] <- NA

  # Get the date factor based on the specified frequency
  date_factor <- ci$date_factors[[freq]]

  # Ensure date_factor is a factor (required for tapply.fast)
  if (!is.factor(date_factor)) {
    date_factor <- factor(date_factor)
  }

  # Calculate the percentage of days meeting the condition per date_factor using tapply.fast
  result <- tapply.fast(condition_met, date_factor, function(x) {
    mean(x, na.rm = TRUE) * 100
  })

  # Replace NaN with NA
  result[is.nan(result)] <- NA

  # Apply NA mask if available
  na_mask <- ci$na_masks[[freq]]$wind
  if (!is.null(na_mask)) {
    # Ensure names align
    names(result) <- names(na_mask)
    result <- result * na_mask
  }

  # Convert the result to a data frame with proper date labels
  result_df <- data.frame(Date = names(result), Value = as.numeric(result))

  return(result_df)
}
#' Calculate T90p Index
#'
#' Calculates the combined percentage of days when temperature is above the 90th percentile.
#'
#' @param ci List. Climate input object.
#' @param freq Character. Frequency of calculation, either "monthly" or "annual".
#' @return Data frame with dates and calculated T90p values.
#' @export
#' @importFrom dplyr full_join mutate select rename
#' @examples
#' \dontrun{
#' # Calculate monthly T90p index
#' t90p_values <- t90p(ci, freq = "monthly")
#' }

t90p <- function(ci, freq = c("monthly", "annual")) {
  freq <- match.arg(freq)

  # Calculate tx90p and tn90p values using the specified frequency
  tx90p_df <- tx90p(ci, freq = "monthly")%>% rename(tx90p = Value)
  tn90p_df <- tn90p(ci, freq = "monthly")%>% rename(tn90p = Value)

  # Join the data frames to ensure dates match using dplyr's full_join
  combined_df <- full_join(tx90p_df, tn90p_df, by = "Date") %>%
    # Compute the average of tx90p and tn90p values
    mutate(Value = 0.5 * (tx90p + tn90p)) %>%
    # Select only the necessary columns
    select(Date, Value)

  return(combined_df)
}
#' Calculate T10p Index
#'
#' Calculates the combined percentage of days when temperature is below the 10th percentile.
#'
#' @param ci List. Climate input object.
#' @param freq Character. Frequency of calculation, either "monthly" or "annual".
#' @return Data frame with dates and calculated T10p values.
#' @export
#' @importFrom dplyr full_join mutate select rename
#' @examples
#' \dontrun{
#' # Calculate monthly T10p index
#' t10p_values <- t10p(ci, freq = "monthly")
#' }

t10p <- function(ci, freq = c("monthly", "annual")) {
  freq <- match.arg(freq)

  # Calculate tx10p and tn10p values using the specified frequency
  tx10p_df <- tx10p(ci, freq = freq)
  tn10p_df <- tn10p(ci, freq = freq)

  # Join the data frames to ensure dates match using dplyr's inner_join
  combined_df <- full_join(tx10p_df, tn10p_df, by = "Date") %>%
    # Compute the average of tx10p and tn10p values
    mutate(Value = 0.5 * (Value.x + Value.y)) %>%
    # Select only the necessary columns
    select(Date, Value)

  return(combined_df)
}

#' Convert Monthly Data to Seasonal Data
#'
#' Aggregates monthly data into seasonal averages.
#'
#' @param data Data frame. Monthly data with Date and Value columns.
#' @return Data frame with seasonal data.
#' @export
#' @importFrom dplyr mutate group_by summarise ungroup
#' @importFrom tidyr separate
#' @examples
#' \dontrun{
#' # Assuming you have monthly data in a data frame 'monthly_data'
#' # with columns 'Date' (in 'YYYY-MM' format) and 'Value'
#' seasonal_data <- monthly_to_seasonal(monthly_data)
#' }

monthly_to_seasonal <- function(data) {
  data <- data %>%
    separate(Date, into = c("year", "month"), sep = "-") %>%
    mutate(
      year = as.integer(year),
      month = as.integer(month),
      value = as.numeric(Value)
    )

  # Defining seasons and grouping data for seasonal averaging
  seasonal_data <- data %>%
    mutate(
      season = case_when(
        month %in% c(12, 1, 2) ~ "DJF",
        month %in% c(3, 4, 5) ~ "MAM",
        month %in% c(6, 7, 8) ~ "JJA",
        month %in% c(9, 10, 11) ~ "SON"
      ),
      season_year = ifelse(month == 12, year + 1, year),
      season = factor(season, levels = c("DJF", "MAM", "JJA", "SON"))
    ) %>%
    group_by(season_year, season) %>%
    summarise(Value = mean(value, na.rm = TRUE), .groups = 'drop') %>%
    mutate(
      Date = paste0(season_year, "-", season)
    ) %>%
    select(Date, Value)

  return(seasonal_data)
}


#' Calculate Standardized CDD Index
#'
#' Calculates the standardized consecutive dry days index.
#'
#' @param ci List. Climate input object.
#' @param freq Character. Frequency of calculation, either "monthly" or "seasonal".
#' @return Data frame with dates and standardized CDD values.
#' @export
#' @importFrom dplyr mutate select left_join group_by summarise
#' @importFrom tidyr separate
#' @examples
#' \dontrun{
#' # Calculate standardized CDD index on a monthly basis
#' cdd_std_values <- cdd_std(ci, freq = "monthly")
#' }

cdd_std <- function(ci, freq = c("monthly", "seasonal")){
  freq <- match.arg(freq)
  cdd_df <- cdd(ci, monthly = TRUE)


  if (freq == 'monthly'){

    cdd_df <- cdd_df%>%
      separate(Date, into = c("year", "month"), sep = "-", remove = FALSE) %>%
      mutate(
        year = as.integer(year),
        month = as.integer(month)
      )

    ref_df <- cdd_df%>%
      dplyr::filter(year >= ci$base_range[1],year<= ci$base_range[2])

    ref_mean <- mean(ref_df$Value, na.rm = TRUE)

    ref_sd <- sd(ref_df$Value, na.rm = TRUE)

    result <- cdd_df %>%
      mutate(Value = (Value - ref_mean) / ref_sd) %>%
      select(Date, Value)
  } else {
    cdd_seasonal <- monthly_to_seasonal(cdd_df)%>%
      separate(Date, into = c("year", "season"), sep = "-", remove = FALSE) %>%
      mutate(
        year = as.integer(year),
        season = factor(season, levels = c("DJF", "MAM", "JJA", "SON"))
      )


    ref_df <- cdd_seasonal%>%
      dplyr::filter(year >= ci$base_range[1],year<= ci$base_range[2])

    ref_mean <- mean(ref_df$Value, na.rm = TRUE)

    ref_sd <- sd(ref_df$Value, na.rm = TRUE)

    result <- cdd_seasonal %>%
      mutate(Value = (Value - ref_mean) / ref_sd) %>%
      select(Date, Value)
  }

  return(result)
}
#' Calculate Standardized T90p Index
#'
#' Calculates the standardized T90p index.
#'
#' @param ci List. Climate input object.
#' @param freq Character. Frequency of calculation, either "monthly" or "seasonal".
#' @return Data frame with dates and standardized T90p values.
#' @export
#' @importFrom dplyr mutate select left_join group_by summarise rename full_join
#' @importFrom tidyr separate
#' @examples
#' \dontrun{
#' # Calculate standardized T90p index on a monthly basis
#' t90p_std_values <- t90p_std(ci, freq = "monthly")
#' }

t90p_std <- function(ci, freq = c("monthly", "seasonal")){
  freq <- match.arg(freq)

  tx90p_df <- tx90p(ci, freq = "monthly")
  tn90p_df <- tn90p(ci, freq = "monthly")
  if (freq == 'monthly'){
    tx90p_df <- tx90p_df %>% rename(tx90p = Value)
    tn90p_df <- tn90p_df %>% rename(tn90p = Value)
    t90p_df <- full_join(tx90p_df, tn90p_df, by = "Date") %>%
      mutate(t90p = 0.5 * (tx90p + tn90p)) %>%
      separate(Date, into = c("year", "month"), sep = "-", remove = FALSE) %>%
      mutate(
        year = as.integer(year),
        month = as.integer(month)
      )

    ref_df <- t90p_df%>%
      dplyr::filter(year >= ci$base_range[1],year<= ci$base_range[2])%>%
      group_by(month)%>%
      summarise(t90_ref_mean=mean(t90p, na.rm = TRUE),
                tx90_ref_sd=sd(tx90p, na.rm = TRUE),
                tn90_ref_sd=sd(tn90p, na.rm = TRUE),
                t90_ref_sd= 0.5*(tx90_ref_sd+tn90_ref_sd)
      )%>%
      select(month,t90_ref_mean,t90_ref_sd)


    t90p_df <- t90p_df%>%
      left_join(ref_df,by = 'month')

    result <- t90p_df%>%
      mutate(Value=(t90p - t90_ref_mean)/t90_ref_sd)%>%
      select(Date, Value)
  }
  else if (freq == 'seasonal') {
    tx90p_seasonal <- monthly_to_seasonal(tx90p_df) %>% rename(tx90p = Value)
    tn90p_seasonal <- monthly_to_seasonal(tx90p_df) %>% rename(tn90p = Value)
    t90p_seasonal <- full_join(tx90p_seasonal, tn90p_seasonal, by = "Date") %>%
      mutate(t90p = 0.5 * (tx90p + tn90p))%>%
      separate(Date, into = c("year", "season"), sep = "-", remove = FALSE) %>%
      mutate(
        year = as.integer(year),
        season = factor(season, levels = c("DJF", "MAM", "JJA", "SON"))
      )

    ref_seasonal <- t90p_seasonal%>%
      dplyr::filter(year >= ci$base_range[1],year<= ci$base_range[2])%>%
      group_by(season)%>%
      summarise(t90_ref_mean=mean(t90p, na.rm = TRUE),
                tx90_ref_sd=sd(tx90p, na.rm = TRUE),
                tn90_ref_sd=sd(tn90p, na.rm = TRUE),
                t90_ref_sd= 0.5*(tx90_ref_sd+tn90_ref_sd)
      )%>%
      select(season,t90_ref_mean,t90_ref_sd)


    t90p_seasonal <- t90p_seasonal%>%
      left_join(ref_seasonal,by = 'season')

    result <- t90p_seasonal%>%
      mutate(Value=(t90p - t90_ref_mean)/t90_ref_sd)%>%
      select(Date, Value)
  }

  return(result)

}
#' Calculate Standardized T10p Index
#'
#' Calculates the standardized T10p index.
#'
#' @param ci List. Climate input object.
#' @param freq Character. Frequency of calculation, either "monthly" or "seasonal".
#' @return Data frame with dates and standardized T10p values.
#' @export
#' @importFrom dplyr mutate select left_join group_by summarise rename full_join
#' @importFrom tidyr separate
#' @examples
#' \dontrun{
#' Calculate standardized T10p index on a monthly basis
#' t10p_std_values <- t10p_std(ci, freq = "monthly")
#' }

t10p_std <- function(ci, freq = c("monthly", "seasonal")){
  freq <- match.arg(freq)

  tx10p_df <- tx10p(ci, freq = "monthly")
  tn10p_df <- tn10p(ci, freq = "monthly")
  if (freq == 'monthly'){
    tx10p_df <- tx10p_df %>% rename(tx10p = Value)
    tn10p_df <- tn10p_df %>% rename(tn10p = Value)
    t10p_df <- full_join(tx10p_df, tn10p_df, by = "Date") %>%
      mutate(t10p = 0.5 * (tx10p + tn10p)) %>%
      separate(Date, into = c("year", "month"), sep = "-", remove = FALSE) %>%
      mutate(
        year = as.integer(year),
        month = as.integer(month)
      )

    ref_df <- t10p_df%>%
      dplyr::filter(year >= ci$base_range[1],year<= ci$base_range[2])%>%
      group_by(month)%>%
      summarise(t10_ref_mean=mean(t10p, na.rm = TRUE),
                tx10_ref_sd=sd(tx10p, na.rm = TRUE),
                tn10_ref_sd=sd(tn10p, na.rm = TRUE),
                t10_ref_sd= 0.5*(tx10_ref_sd+tn10_ref_sd)
      )%>%
      select(month,t10_ref_mean,t10_ref_sd)


    t10p_df <- t10p_df%>%
      left_join(ref_df,by = 'month')

    result <- t10p_df%>%
      mutate(Value=(t10p - t10_ref_mean)/t10_ref_sd)%>%
      select(Date, Value)
  }
  else if (freq == 'seasonal') {
    tx10p_seasonal <- monthly_to_seasonal(tx10p_df) %>% rename(tx10p = Value)
    tn10p_seasonal <- monthly_to_seasonal(tx10p_df) %>% rename(tn10p = Value)
    t10p_seasonal <- full_join(tx10p_seasonal, tn10p_seasonal, by = "Date") %>%
      mutate(t10p = 0.5 * (tx10p + tn10p))%>%
      separate(Date, into = c("year", "season"), sep = "-", remove = FALSE) %>%
      mutate(
        year = as.integer(year),
        season = factor(season, levels = c("DJF", "MAM", "JJA", "SON"))
      )

    ref_seasonal <- t10p_seasonal%>%
      dplyr::filter(year >= ci$base_range[1],year<= ci$base_range[2])%>%
      group_by(season)%>%
      summarise(t10_ref_mean=mean(t10p, na.rm = TRUE),
                tx10_ref_sd=sd(tx10p, na.rm = TRUE),
                tn10_ref_sd=sd(tn10p, na.rm = TRUE),
                t10_ref_sd= 0.5*(tx10_ref_sd+tn10_ref_sd)
      )%>%
      select(season,t10_ref_mean,t10_ref_sd)


    t10p_seasonal <- t10p_seasonal%>%
      left_join(ref_seasonal,by = 'season')

    result <- t10p_seasonal%>%
      mutate(Value=(t10p - t10_ref_mean)/t10_ref_sd)%>%
      select(Date, Value)
  }

  return(result)

}
#' Calculate Standardized Rx5day Index
#'
#' Calculates the standardized Rx5day index.
#'
#' @param ci List. Climate input object.
#' @param freq Character. Frequency of calculation, either "monthly" or "seasonal".
#' @return Data frame with dates and standardized Rx5day values.
#' @export
#' @importFrom dplyr mutate select left_join group_by summarise
#' @importFrom tidyr separate
#' @examples
#' \dontrun{
#' # Calculate standardized Rx5day index on a monthly basis
#' rx5day_std_values <- rx5day_std(ci, freq = "monthly")
#' }
rx5day_std <- function(ci, freq = c("monthly", "seasonal")){
  freq <- match.arg(freq)
  df <- rx5day(ci, freq = 'monthly')
  result <- calculate_standardized(df,freq, ci$base_range)
  return(result)
}
#' Calculate Standardized W90p Index
#'
#' Calculates the standardized W90p index.
#'
#' @param ci List. Climate input object.
#' @param freq Character. Frequency of calculation, either "monthly", "seasonal".
#' @return Data frame with dates and standardized W90p values.
#' @export
#' @importFrom dplyr mutate select left_join group_by summarise
#' @importFrom tidyr separate
#' @examples
#' \dontrun{
#' # Calculate standardized W90p index on a monthly basis
#' w90p_std_values <- w90p_std(ci, freq = "monthly")
#' }

w90p_std <- function(ci, freq = c("monthly", "seasonal")){
  freq <- match.arg(freq)
  df <- w90p(ci, freq = 'monthly')
  result <- calculate_standardized(df,freq, ci$base_range)
  return(result)
}

#' Calculate Standardized Indices
#'
#' Helper function to calculate standardized indices based on a reference period.
#'
#' @param df Data frame. Input data with Date and Value columns.
#' @param freq Character. Frequency of calculation, either "monthly" or "seasonal".
#' @param base_range Numeric vector. Base range years.
#' @return Data frame with standardized values.
#' @keywords internal
#' @importFrom dplyr mutate select left_join group_by summarise
#' @importFrom tidyr separate

calculate_standardized <- function(df, freq, base_range){
  if (freq == 'monthly'){

    df <- df%>%
      separate(Date, into = c("year", "month"), sep = "-", remove = FALSE) %>%
      mutate(
        year = as.integer(year),
        month = as.integer(month)
      )

    ref_df <- df%>%
      dplyr::filter(year >= base_range[1],year<= base_range[2])%>%
      group_by(month)%>%
      summarise(ref_mean=mean(Value, na.rm = TRUE),
                ref_sd=sd(Value, na.rm = TRUE))%>%
      select(month,ref_mean,ref_sd)


    result <- df%>%
      left_join(ref_df, by = 'month')%>%
      mutate(Value=(Value - ref_mean)/ref_sd)%>%
      select(Date, Value)

  } else {
    df_seasonal <- monthly_to_seasonal(df)%>%
      separate(Date, into = c("year", "season"), sep = "-", remove = FALSE) %>%
      mutate(
        year = as.integer(year),
        season = factor(season, levels = c("DJF", "MAM", "JJA", "SON"))
      )


    ref_df <- df_seasonal%>%
      dplyr::filter(year >= base_range[1],year<= base_range[2])%>%
      group_by(season)%>%
      summarise(ref_mean=mean(Value, na.rm = TRUE),
                ref_sd=sd(Value, na.rm = TRUE))%>%
      select(season,ref_mean,ref_sd)


    result <- df_seasonal%>%
      left_join(ref_df,by = 'season') %>%
      mutate(Value = (Value - ref_mean) / ref_sd) %>%
      select(Date, Value)
  }

  return(result)
}


#' Sea Level Input Function
#'
#' Creates a data frame for sea level data input.
#'
#' @param Date Character vector. Dates in "YYYY-MM" format.
#' @param Value Numeric vector. Sea level values (default is NA).
#' @return Data frame with Date and Value columns.
#' @export
#' @examples
#' \dontrun{
#' # Create a sea level input data frame
#' dates <- c("1980-01", "1980-02", "1980-03")
#' values <- c(1.2, 1.3, 1.4)
#' sea_data <- sea_input(Date = dates, Value = values)
#' # If Value is not provided, it defaults to NA
#' sea_data_na <- sea_input(Date = dates)
#' }
sea_input <- function(Date = levels(ci$date_factors$monthly), Value = NA) {
  # Check if Date is in 'YYYY-MM' format using a regular expression
  Date
  if (!all(grepl("^\\d{4}-\\d{2}$", Date))) {
    stop("All Date entries must be in 'YYYY-MM' format.")
  }

  # Create a data frame
  df <- data.frame(Date = Date, Value = Value)
  return(df)
}

#' Calculate Standardized Sea Level Index
#'
#' Calculates the standardized sea level index.
#'
#' @param si Data frame. Sea level input data created by \code{\link{sea_input}}.
#' @param ci List. Climate input object containing the base range (e.g., created by \code{\link{climate_input}}).
#' @param freq Character. Frequency of calculation, either "monthly" or "seasonal".
#' @return Data frame with dates and standardized sea level values.
#' @export
#' @importFrom dplyr mutate select left_join group_by summarise
#' @importFrom tidyr separate
#' @examples
#' \dontrun{
#' # Assume you have a climate input object 'ci' and sea level data 'si'
#' # ci should be created using 'climate_input' function
#' ci <- climate_input(...)
#' si <- sea_input(Date = c("1980-01", "1980-02", "1980-03"), Value = c(1.2, 1.3, 1.4))
#' # Calculate standardized sea level index with monthly frequency
#' sea_std_values <- sea_std(si, ci, freq = "monthly")
#' }

sea_std <- function(si, ci, freq = c("monthly", "seasonal")){
  freq <- match.arg(freq)
  df <- si
  base_range <- ci$base_range
  result <- calculate_standardized(df, freq, base_range)
  return(result)
}


#' Calculate Integrated Iberian Actuarial Climate Index (IACI)
#'
#' Integrates various standardized indices to compute the IACI.
#'
#' @param ci List. Climate input object.
#' @param si Data frame. Sea level input data.
#' @param freq Character. Frequency of calculation, either "monthly" or "seasonal".
#' @return Data frame with dates and IACI values.
#' @export
#' @importFrom dplyr mutate select rename full_join
#' @examples
#' \dontrun{
#' # Assume we have a climate input object 'ci' and sea level data 'si'
#' # ci should be created using 'climate_input' function (not provided here)
#' ci <- list(base_range = c(1980, 1990))
#' si <- sea_input(
#'   Date = c("1980-01", "1980-02", "1980-03"),
#'   Value = c(1.2, 1.3, 1.4)
#' )
#' # Calculate the IACI with monthly frequency
#' result <- iaci_output(ci, si, freq = "monthly")
#' }
iaci_output <- function(ci,si,freq = c("monthly", "seasonal")){
  freq <- match.arg(freq)
  df1 <- t90p_std(ci, freq) %>% rename(T90p = Value)
  df2 <- t10p_std(ci, freq) %>% rename(T10p = Value)
  df3 <- rx5day_std(ci, freq) %>% rename(Rx5day = Value)
  df4 <- cdd_std(ci, freq) %>% rename(CDD = Value)
  df5 <- w90p_std(ci, freq)%>% rename(W90p = Value)
  df6 <- sea_std(si,ci, freq) %>% rename(Sea = Value)

  df_list <- list(df1, df2, df3, df4, df5, df6)
  aligned_dfs <- lapply(df_list, function(df) df[order(df$Date),])
  combined_df <- do.call(cbind.data.frame, aligned_dfs)
  combined_df <- combined_df[!duplicated(colnames(combined_df))]

  combined_df <- combined_df %>%
    mutate(IACI = (T90p - T10p + Rx5day + CDD + W90p + Sea) / 6)

  return(combined_df)
}



#' Output IACI of all grids
#'
#' Processes all CSV files in the input directory and outputs the results to the output directory.
#'
#' @param si Data frame. Sea level input data.
#' @param input_dir Character. Directory containing input CSV files.
#' @param output_dir Character. Directory to save output files.
#' @param freq Character. Frequency of calculation, either "monthly" or "seasonal".
#' @param base.range Numeric vector. Base range years (default is c(1961, 1990)).
#' @param time.span Numeric vector. Time span for output data (default is c(1961, 2022)).
#' @return None. Results are saved to the output directory.
#' @export
#' @importFrom dplyr mutate select left_join group_by summarise rename full_join
#' @importFrom tidyr separate
#' @importFrom readr read_csv write_csv
#' @examples
#' \dontrun{
#' # Assume we have sea level data 'si' and input/output directories
#' si <- sea_input(
#'   Date = c("1980-01", "1980-02", "1980-03"),
#'   Value = c(1.2, 1.3, 1.4)
#' )
#' input_dir <- "path/to/input/csv/files"
#' output_dir <- "path/to/save/output/files"
#' # Run the output_all function with monthly frequency
#' output_all(si, input_dir, output_dir, freq = "monthly")
#' }
output_all <- function(si, input_dir, output_dir, freq = c("monthly", "seasonal"), base.range = c(1961, 1990), time.span = c(1961, 2022)) {
  filelist <- list.files(path = input_dir, pattern = '\\.csv$')

  for (i in seq_along(filelist)) {
    data <- read.csv(file = file.path(input_dir, filelist[i]))

    ci <- climate_input(tmax = data$TMAX, tmin = data$TMIN, prec = data$PRCP, wind = data$WP,
                        dates = data$time, base.range = base.range, n = 5, quantiles = NULL,
                        temp.qtiles = c(0.10, 0.90), wind.qtile = 0.90,
                        max.missing.days = c(annual = 15, monthly = 3),
                        min.base.data.fraction.present = 0.1)

    iaci <- iaci_output(ci, si, freq)
    iaci <- iaci %>%
      separate(Date, into = c("year", "date"), sep = "-", remove = FALSE) %>%
      mutate(year = as.integer(year)) %>%
      dplyr::filter(year >= time.span[1], year <= time.span[2])%>%
      select(-year,-date)

    if (freq == "monthly") {
      out_dir <- paste(output_dir, '/monthly/', filelist[i], sep = '')
      if (!dir.exists(paste(output_dir, '/monthly/', sep = ''))) {
        dir.create(paste(output_dir, '/monthly/', sep = ''), recursive = TRUE)
        message("Created directory: ", paste(output_dir, '/monthly/', sep = ''))
      }
    } else if (freq == "seasonal") {
      out_dir <- paste(output_dir, '/seasonal/', filelist[i], sep = '')
      if (!dir.exists(paste(output_dir, '/seasonal/', sep = ''))) {
        dir.create(paste(output_dir, '/seasonal/', sep = ''), recursive = TRUE)
        message("Created directory: ", paste(output_dir, '/seasonal/', sep = ''))
      }
    } else {
      stop("Invalid frequency specified")
    }

    write.csv(iaci, out_dir, row.names = FALSE)
  }
}

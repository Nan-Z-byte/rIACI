% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/rIACI.R
\name{csv_to_netcdf}
\alias{csv_to_netcdf}
\title{CSV to NetCDF Function}
\usage{
csv_to_netcdf(csv_dir, output_file, freq)
}
\arguments{
\item{csv_dir}{Character. Directory containing CSV files, each file representing a single latitude-longitude point.
The filename format should be 'lat_lon.csv'.}

\item{output_file}{Character. Path to the output NetCDF file.}

\item{freq}{Character. Frequency of the data, either `'monthly'` or `'seasonal'`.
- `'monthly'` data uses date format `'YYYY-MM'`.
- `'seasonal'` data uses date format like `'YYYY-SSS'` (e.g., `'1961-DJF'`).}
}
\value{
None. The NetCDF file is saved to the specified location.
}
\description{
Merges CSV files in a specified directory into a single NetCDF file, completing the grid by filling missing values.
}
\examples{
\dontrun{
# Example usage of csv_to_netcdf
csv_directory <- "/path/to/csv_files"
output_netcdf_file <- "/path/to/output_file.nc"
csv_to_netcdf(csv_dir = csv_directory, output_file = output_netcdf_file, freq = "monthly")
}
}

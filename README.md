
`rIACI` is a package designed to calculate the Iberian Actuarial Climate Index (IACI). This package integrates multiple standardized climate indices, including temperature, precipitation, wind power, and sea level data, to support climate change analysis and risk assessment.

**Table of contents**
- [1. Installation](#1-installation)
- [2. Dependencies](#2-dependencies)
  - [2.1. R Dependencies](#21-r-dependencies)
  - [2.2. Python Dependencies](#22-python-dependencies)
- [3. Quick Start](#3-quick-start)
- [4. Features](#4-features)
  - [4.1. Data Download](#41-data-download)
  - [4.2. Data Processing](#42-data-processing)
  - [4.3. Climate Index Calculation](#43-climate-index-calculation)
  - [4.4. IACI Calculation](#44-iaci-calculation)
- [5. Examples](#5-examples)
- [6. Notes](#6-notes)
- [7. Contributing](#7-contributing)
- [8. License](#8-license)


## 1. Installation
You can install the latest version of the `rIACI` package from GitHub:

```R
R

# Install devtools if not already installed
install.packages("devtools")

# Install rIACI package from GitHub
devtools::install_github("your_username/rIACI")
````
Please ensure that you have the latest version of R and the necessary dependency packages installed.

## 2. Dependencies
### 2.1. R Dependencies
The `rIACI` package depends on the following R packages:

- `Rcpp`
- `ecmwfr`
- `reticulate`
- `lubridate`
- `dplyr`
- `tidyr`
- `magrittr`
- `stats`
- `utils`

### 2.2. Python Dependencies
Some data processing functions in `rIACI` rely on Python scripts. To ensure smooth operation, please install Python (version 3.6 or higher) and the following Python libraries:

- `xarray`
- `pandas`
- `numpy`
- `dask`

**Installing Python and Required Libraries**:
**1. Python Installation**:

- **Windows**: Download and install Python from the [official website](https://www.python.org/downloads/windows/.
- **macOS**: Python 2.x is pre-installed. It is recommended to install Python 3.x using [Homebrew](https://brew.sh/) or download from the [official website](https://www.python.org/downloads/windows/.
- **Linux**: Use your distribution's package manager (e.g., `apt`, `yum`) to install Python.

**2. Installing Libraries via `pip`** :

Open a terminal or command prompt and run：

```bash
bash

pip install xarray pandas numpy
```
**3. Ensuring `reticulate` Can Find Python**:

The R package `reticulate` needs to interface with Python. You may need to set the Python path in R:
```R
R

reticulate::use_python("/path/to/your/python", required = TRUE)
```
Replace `"/path/to/your/python"` with the actual path to your Python executable.

## 3. Quick Start

Below are the basic steps to calculate the IACI using the `rIACI` package:

**1. Download ERA5-Land Data**: Use the `download_data()` function to download necessary meteorological data from the ECMWF Climate Data Store.

**2. Process Data**: Use the `process_data()` function to process the downloaded NetCDF files and extract required variables.

**3. Export Data to CSV**: Use the `export_data_to_csv()` function to export the processed data to CSV format for further analysis.

**4. Calculate Climate Indices**: Use the `climate_input()` function to create a climate input object and then calculate various standardized climate indices.

**5. Calculate IACI**: Use the `iaci_output()` function to integrate all standardized indices and calculate the IACI.

**6. Output Results**: Use the `output_all()` function to batch process data for all grid points and save results to a specified directory.

## 4. Features

### 4.1. Data Download
```R
download_data(
  start_year,        # Starting year
  end_year,          # Ending year
  variables,         # List of variables to download
  area,              # Geographical area (North, West, South, East)
  output_dir,        # Directory to save data
  user_id,           # ECMWF user ID
  user_key           # ECMWF API key
)
```
This function downloads ERA5-Land data from the ECMWF Climate Data Store, supporting specific time ranges, variables, and geographical areas.

### 4.2. Data Processing
```R
process_data(
  input_dir,         # Input directory containing downloaded NetCDF files
  output_dir,        # Output directory to save processed data
  save_merged = FALSE  # Whether to save the merged NetCDF file
)
```
This function processes the downloaded NetCDF files, extracts required variables, and prepares data for further analysis.

### 4.3. Climate Index Calculation
```R
climate_input(
  tmax,              # Maximum temperature data
  tmin,              # Minimum temperature data
  prec,              # Precipitation data
  wind,              # Wind speed data
  dates,             # Corresponding dates
  base.range = c(1961, 1990)  # Base year range
)
```

This function creates a climate input object containing all necessary data and parameters for calculating various climate indices.

You can use the following functions to calculate different climate indices:

- `tx90p()`: Calculates the percentage of days when maximum temperature exceeds the 90th percentile.
- `tn90p()`: Calculates the percentage of days when minimum temperature exceeds the 90th percentile.
- `tx10p()`: Calculates the percentage of days when maximum temperature is below the 10th percentile.
- `tn10p()`: Calculates the percentage of days when minimum temperature is below the 10th percentile.
- `rx5day()`: Calculates the maximum consecutive 5-day precipitation amount.
- `cdd()`: Calculates the maximum length of consecutive dry days (precipitation less than 1mm).
- `w90p()`: Calculates the percentage of days when wind power exceeds the 90th percentile.

### 4.4. IACI Calculation
```R
iaci_output(
  ci,                # Climate input object
  si,                # Sea level input data
  freq = "monthly"   # Frequency: "monthly" or "seasonal"
)
````

This function integrates all standardized climate indices and sea level data to calculate the IACI.

## 5. Examples
Below is a complete example of calculating the IACI using the `rIACI` package.

**1. Download Data**
```R
# Set ECMWF user ID and API key
user_id <- "your_user_id"   # yourmail@mail.com
user_key <- "your_api_key"

# Define geographical area (North, West, South, East)
# Example: Iberian Peninsula roughly bounded by 44N, -10W, 35N, 5E
area <- c(44, -10, 36, 4)  

# Download data from the year 1960
download_data(
  start_year = 1960,
  end_year = 2023,
  area = area,
  output_dir = "cds_data",
  user_id = user_id,
  user_key = user_key
)
```
**2. Process Data**
```R
# Define input and output directories
input_directory <- "path/to/downloaded/cds_data"
output_directory <- "path/to/processed_data"

# Process data
process_data(
  input_dir = input_directory,
  output_dir = output_directory,
  save_merged = TRUE
)
```

**3. Export Data to CSV**
```R
# Define the processed NetCDF file and output directory
netcdf_file <- "path/to/processed_data.nc"
csv_output_directory <- "path/to/csv_output"

# Export data to CSV
export_data_to_csv(
  nc_file = netcdf_file,
  output_dir = csv_output_directory
)
```
**4. Calculate and output IACI for all grids**

**Integrating Sea Level Data**

- **Option 1: Load Your Sea Level Data**

If you have an existing sea level data file, load it and prepare it for IACI calculations:

```R
# Load sea level data from a CSV file
sea_data <- read.csv('path/to/sea_level_data')
sea_data$Date <- format.Date(sea_data$Date,'%Y-%m')

# Prepare the sea level input for IACI calculations
si <- sea_input(sea_data$Date,sea_data$Sea)
```
- **Option 2: Generate a Blank Series**

If you don't have sea level data, you can generate a blank series for the full time range

```R
# Generate monthly dates from 1960-01 to 2023-12
sea_dates <- seq(as.Date("1960-01-01"), as.Date("2023-12-01"), by = "month")
sea_dates <- format(sea_dates, "%Y-%m")  # Format as "YYYY-MM"

# Generate NA values for each date
sea_values <- rep(NA, length(sea_dates))  

# Combine dates and values into a data frame
si <- sea_input(Date = sea_dates, Value = sea_values)
````

**Calculate IACI for All CSV Files and Output Results**

```R
# Define input and output directories
input_dir <- "csv_output"
output_dir <- "iaci_results"

# Run the output_all function with monthly frequency
output_all(
  si = si,
  input_dir = input_dir,
  output_dir = output_dir,
  freq = "monthly",
  base.range = c(1961, 1990),
  time.span = c(1961, 2023)
)
```
**5. Merge CSVs into NetCDF (optional)**
```R
# Define output file
merged_netcdf <- "iaci.nc"

#Run the CSV to NetCDF Function
csv_to_netcdf(csv_dir = iaci_results_directory, output_file = merged_netcdf)
```

## 6. Notes
- **Data Integrity**: Ensure that the input data is complete and correctly formatted, especially date formats and handling of missing values.
- **Base Year Range**: The default base year range is 1961-1990. Adjust this as needed for your analysis.
- **Python Dependencies**: Some data processing functions rely on Python. Ensure that Python and the necessary libraries are installed and correctly configured with the `reticulate` package.
- **ECMWF API**: Before downloading data, register an account with ECMWF and obtain a valid API key.



##
=======
## 7. Contributing
Contributions to the `rIACI` package are welcome! You can participate in the following ways:
- Submit issues or feature requests
- Submit pull requests to improve the code
- Share your experiences and suggestions
  
Please read the project's contribution guidelines before contributing.

## 8. License
The `rIACI` package is distributed under the MIT License. For more details, please refer to the [LICENSE](LICENSE) file.

## Acknowledgements

"With gratitude to **Jos\'e Luis Vilar-Zan\'on**, **Jos\'e Garrido**, and **Antonio \'e Heras Mart\'inez** for their essential support."



Thank you for using the rIACI package! If you have any questions or suggestions, please feel free to contact us.

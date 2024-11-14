// [[Rcpp::depends(Rcpp)]]
#include <Rcpp.h>
#include <algorithm>
#include <cmath>
#include <map>
#include <string>
#include <iomanip>
#include <sstream>
using namespace Rcpp;

// Function to compute quantile using R's type = 8 algorithm
double compute_quantile(std::vector<double>& x, double p) {
  size_t n = x.size();
  if (n == 0) return NA_REAL;

  // Data should be sorted
  // std::sort(x.begin(), x.end()); // No need to sort here as it's already sorted

  double h = (n + 1.0 / 3.0) * p + 1.0 / 3.0;
  double j = floor(h);
  double g = h - j;

  if (j <= 0) {
    return x[0];
  } else if (j >= n) {
    return x[n - 1];
  } else {
    // Adjust indices for zero-based indexing
    return x[j - 1] + g * (x[j] - x[j - 1]);
  }
}

// Function to compute window days
std::vector<double> compute_window_days(double x) {
  std::vector<double> window_days;

  if (x >= 58 && x <= 61) {
    if (x == 58) {
      window_days = {56, 57, 58, 59, 59.5};
    } else if (x == 59) {
      window_days = {57, 58, 59, 59.5, 60};
    } else if (x == 59.5) {
      window_days = {58, 59, 59.5, 60, 61};
    } else if (x == 60) {
      window_days = {59, 59.5, 60, 61, 62};
    } else if (x == 61) {
      window_days = {59.5, 60, 61, 62, 63};
    }
  } else {
    // Standard window days
    double w1 = std::ceil(x - 2);
    double w2 = std::ceil(x - 1);
    double w3 = x;
    double w4 = std::floor(x + 1);
    double w5 = std::floor(x + 2);

    window_days = {w1, w2, w3, w4, w5};
  }

  // Remove duplicates and ensure window days are within valid range
  std::sort(window_days.begin(), window_days.end());
  window_days.erase(std::unique(window_days.begin(), window_days.end()), window_days.end());

  return window_days;
}

// [[Rcpp::export]]
DataFrame running_quantile_cpp(NumericVector data,
                               NumericVector adjusted_day_of_year,
                               NumericVector q,
                               double min_fraction) {
  int data_length = data.size();
  int num_quantiles = q.size();

  // Create day_vector with 366 days, including 59.5 for February 29th
  std::vector<double> day_vector;
  for (int doy = 1; doy <= 365; ++doy) {
    day_vector.push_back(static_cast<double>(doy));
    if (doy == 59) {
      day_vector.push_back(59.5); // Insert 59.5 to represent February 29th
    }
  }
  int num_days = day_vector.size(); // Should be 366

  // Initialize result vectors
  std::vector<NumericVector> quantile_columns(num_quantiles);
  for (int k = 0; k < num_quantiles; ++k) {
    quantile_columns[k] = NumericVector(num_days, NA_REAL);
  }

  // Create a map from adjusted_day_of_year to data values
  std::map<double, std::vector<double>> day_data_map;
  for (int i = 0; i < data_length; ++i) {
    double doy = adjusted_day_of_year[i];
    if (!NumericVector::is_na(data[i])) {
      day_data_map[doy].push_back(data[i]);
    }
  }

  // Main loop to compute quantiles for each day
  for (int i = 0; i < num_days; ++i) {
    double x = day_vector[i];

    // Compute window days
    std::vector<double> window_days = compute_window_days(x);

    // Collect data for these window days
    std::vector<double> window_data;
    for (double wd : window_days) {
      auto it = day_data_map.find(wd);
      if (it != day_data_map.end()) {
        window_data.insert(window_data.end(), it->second.begin(), it->second.end());
      }
    }

    // Check for sufficient data
    double valid_fraction = static_cast<double>(window_data.size()) / window_days.size();
    if (valid_fraction >= min_fraction && !window_data.empty()) {
      // Data should be sorted for compute_quantile
      std::sort(window_data.begin(), window_data.end());
      for (int k = 0; k < num_quantiles; ++k) {
        double q_value = compute_quantile(window_data, q[k]);
        quantile_columns[k][i] = q_value;
      }
    }
  }

    // Create the month_day vector
    std::vector<std::string> month_day_vector(num_days);
  for (int i = 0; i < num_days; ++i) {
    double doy = day_vector[i];
    Date date_i;
    if (doy == 59.5) {
      date_i = Date(2000, 2, 29); // Map doy=59.5 to February 29th
    } else {
      int day_offset = static_cast<int>(doy) - 1;
      date_i = Date(2001, 1, 1) + day_offset; // Use a non-leap year as base
    }
    int month = date_i.getMonth(); // getMonth() returns 1-12
    int day = date_i.getDay();

    std::stringstream ss;
    ss << std::setw(2) << std::setfill('0') << month << "-"
       << std::setw(2) << std::setfill('0') << day;

    month_day_vector[i] = ss.str();
  }


  // Prepare the result DataFrame
  DataFrame result;
  result["month_day"] = month_day_vector;
  for (int k = 0; k < num_quantiles; ++k) {
    std::ostringstream ss;
    ss << "Q" << std::fixed << std::setprecision(1) << q[k] * 100;
    std::string colname = ss.str();
    result[colname] = quantile_columns[k];
  }

  return result;
}

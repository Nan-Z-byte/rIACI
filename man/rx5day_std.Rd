% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/rIACI.R
\name{rx5day_std}
\alias{rx5day_std}
\title{Calculate Standardized Rx5day Index}
\usage{
rx5day_std(ci, freq = c("monthly", "seasonal"))
}
\arguments{
\item{ci}{List. Climate input object.}

\item{freq}{Character. Frequency of calculation, either "monthly" or "seasonal".}
}
\value{
Data frame with dates and standardized Rx5day values.
}
\description{
Calculates the standardized Rx5day index.
}
\examples{
\dontrun{
# Calculate standardized Rx5day index on a monthly basis
rx5day_std_values <- rx5day_std(ci, freq = "monthly")
}
}

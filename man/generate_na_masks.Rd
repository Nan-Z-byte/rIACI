% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/rIACI.R
\name{generate_na_masks}
\alias{generate_na_masks}
\title{Generate NA Masks}
\usage{
generate_na_masks(data_series_list, date_sequence, max.missing.days)
}
\arguments{
\item{data_series_list}{List. List of data series.}

\item{date_sequence}{Date vector. Complete date sequence.}

\item{max.missing.days}{Named numeric vector. Maximum allowed missing days.}
}
\value{
List. NA masks for annual and monthly data.
}
\description{
Creates masks for annual and monthly data based on maximum allowed missing days.
}
\keyword{internal}

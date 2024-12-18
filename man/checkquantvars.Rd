% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/checkquantvars.R
\name{checkquantvars}
\alias{checkquantvars}
\title{checkquantvars() Function}
\usage{
checkquantvars(data, name)
}
\arguments{
\item{data}{A data frame containing measurement values and dates. Column names
should be formatted as "ID", "Measurement name 1", "Date Measurement name 1",
..., "Measurement name k", "Date Measurement name k".}

\item{name}{Common name of columns that should contain quantitative variables.
Should be "Outcome" or "Body Weight"}
}
\value{
This function returns a dataset with numeric values in appropriate columns.
}
\description{
This function makes sure that the outcome or body weight measurements get
converted to numeric. If it finds any strings, it prompts user to input
a numeric value or NA.
}

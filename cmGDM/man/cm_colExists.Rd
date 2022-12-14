% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/cm_colExists.R
\name{cm_colExists}
\alias{cm_colExists}
\title{Utility function to check if column exists}
\usage{
cm_colExists(colLabel, colNames, UIname)
}
\arguments{
\item{colLabel}{Character string or integer. Name or index number of the column (or variable, covariate, etc) whose existence is being checked}

\item{colNames}{Character vector. The column names or variable set to be used to check the existence of the value passed in colLabel}

\item{UIname}{Character. Name of variable to be tested}
}
\value{
An named list:
\describe{
 \item{isOK}{Logical flag; TRUE when colLabel is found; FALSE otherwise}
 \item{msg}{Character string with an error message; "" if isOK == TRUE}
}
}
\description{
A simple function to check for the existence of a specified name in a vector of names, returning the results in a standardised structure which includes error messages corresponding to regular error conditions
}

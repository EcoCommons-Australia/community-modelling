% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/cm_load_community_data.R
\name{cm_load_community_data}
\alias{cm_load_community_data}
\title{Load community data into a cm_experiment object}
\usage{
cm_load_community_data(
  thisExperiment = NULL,
  bioFilename = NULL,
  sheet = 1,
  dataType = "",
  sppFilter = 0,
  presenceMarker = "1",
  absenceMarker = "0",
  siteCol = NULL,
  dissimMeasure = "",
  outFolder = "~/cmGDM/",
  trace = FALSE
)
}
\arguments{
\item{thisExperiment}{cm_experiment. Object to which biological/community data will be added}

\item{bioFilename}{Character. Full path to a file containing an accepted form of biological or community data}

\item{sheet}{Character or integer. Name or index number of a sheet if \emph{bioFilename} points to a file containing a spreadsheet/workbook}

\item{dataType}{Character. One of the accepted data types in entries in the file named in \emph{bioFilename}. One of: "Presence_absence", "Abundance", "Dissimilarity"}

\item{sppFilter}{Integer. For dataType == "Abundance" or dataType == "Presence_absence", filter sites with richnes < sppFilter. Default of 0 causes no filtering.}

\item{presenceMarker}{Character. Denotes "presence" when in the file named in \emph{bioFilename} has \emph{dataType} of "Presence-Absence"}

\item{absenceMarker}{Character. Denotes "absence" when in the file named in \emph{bioFilename} has \emph{dataType} of "Presence-Absence"}

\item{siteCol}{Character or integer. Name or numeric index to the column holding the site names or identifiers}

\item{dissimMeasure}{Character. The type of dissimilarity measure stored in the file named in \emph{bioFilename} when \emph{dataType} is "Dissimilarity". For \emph{Presence-absence} and \emph{Abundance} data, this will be the dissimilarity measure computed from the data table. Currently recognised measures include: "Bray-Curtis", "Jaccard", "Fst"}

\item{outFolder}{Character. Path to user's experiment folder}

\item{trace}{Logical. Produce helpful diagnostic messages? Default is FALSE, therefore radio silence is maintained until told otherwise}
}
\value{
Returns an updated copy of the cm_experiment object passed in parameter \emph{thisExperiment} and also writes it to the user's project folder for the experiment folder
}
\description{
The data table loaded by this function represents the response or dependent variable to be modelled as a function of one or more environmental/ecological covariates.
}
\details{
Data may be in the form of a presence-absence table, abundance data, or a pre-computed dissimilarity matrix.

Terminology used in the literature is variable: these data may be called the 'biological' data, 'ecological' data, or 'community' data to be modelled.

A wide range of file formats is accepted including text files (csv or txt) using any common delimiter, and spreadsheets/workbooks (ods, xls or xlsx).
}
\examples{
\dontrun{
thisExperiment <- cm_load_community_data(thisExperiment, bioFilename = "my_new_data.csv",
                                         dataType = "Presence_absence",
                                         dissimMeasure = "Bray-Curtis")
}
}

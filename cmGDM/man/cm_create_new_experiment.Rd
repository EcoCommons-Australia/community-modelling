% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/cm_create_new_experiment.R
\name{cm_create_new_experiment}
\alias{cm_create_new_experiment}
\title{Create a new cm experiment}
\usage{
cm_create_new_experiment(
  userID = "",
  userName = "",
  experimentName = "",
  description = ""
)
}
\arguments{
\item{userID}{Character. User ID supplied by the system passed from the UI when a new experiment is created}

\item{userName}{Character. User name supplied by the system passed from the UI when a new experiment is created}

\item{experimentName}{Character. Name of the experiment assigned by the user passed from the UI when a new experiment is created}

\item{description}{Character. User-supplied description of the experiment passed from the UI when a new experiment is created}
}
\value{
An initialised cm_experiment object with side-effect of saving the new cm_experiment object to the user's experiment folder
}
\description{
Create a new cm experiment
}
\examples{
\dontrun{
thisExperiment <- cm_create_new_experiment("user123", "Fred Fernackerpan",
                                           "Species assemblages model 1")
}
}

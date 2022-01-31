
#' Compose an error message
#' 
#' Prepare a multi-part message informing the user of reasons for failure to allow a requested action such as fitting a GDM using a cm_experiment object
#'
#' @param thisExperiment A cm_experiment object created by a call to \link{cm_create_new_experiment} and updated by each step in the workflow
#'
#' @return A character string representing a composite error message detailing all errors found by examining the \emph{status} field of the cm_experiment object
#' @export
#'

cm_bad_experiment_msg <- function(thisExperiment)
{
  msg <- NULL
  #c("siteData_OK", "sampleData_OK", "biologicalData_OK", "covarData_OK", "predictionData_OK"
  if (!thisExperiment$status["siteData_OK"])
    msg <- c(msg, "     Valid site data has not been added to this experiment")
  
  # if (!thisExperiment$status["sampleData_OK"])
  #   msg <- c(msg, "     Valid sample data has not been added to this experiment")
  
  if (!thisExperiment$status["biologicalData_OK"])
    msg <- c(msg, "     Valid biological (response) data has not been added to this experiment")
  
  if (!thisExperiment$status["covarData_OK"])
    msg <- c(msg, "     Valid covariate data has not been added to this experiment")
  
  # if (!thisExperiment$status["predictionData_OK"])
  #   msg <- c(msg, "Valid sample data has not been added to this expermient")
  
  return(paste(msg, collapse = "\n "))
}

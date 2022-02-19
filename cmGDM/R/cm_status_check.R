#' EcoCommons Community Model Object Status Summary
#'
#' A short dump of status information for a cm_experiment object
#'
#' @param thisExperiment cm_experiment. The experiment object whose status is to be summarised
#'
#' @return Nothing
#' @export
#'
cm_status_check <- function(thisExperiment)
{
  if (!("cm_experiment" %in% class(thisExperiment)))
    stop("Object passed in 'thisExperiment' must be of class 'cm_experiment'")

  statusLines <- c("-----------------------------------------------",
                   "EcoCommons Community Modelling Module",
                   "GDM Model Status Check Report",
                   "-----------------------------------------------",
                   "",
                   paste("Experiment name:", thisExperiment$experimentName),
                   paste("Decription:", thisExperiment$description),
                   paste("Date created:", thisExperiment$dateCreated),
                   paste("Date updated:", thisExperiment$dateDataUpdated),
                   paste("Model run date:", thisExperiment$dateLastModelRun),
                   "",
                   "Status:",
                   paste("  Element                  Loaded OK?"),
                   paste("  -------------            ---------"),
                   paste("  Site data               ", thisExperiment$status["siteData_OK"]),
                   paste("  Biological data         ", thisExperiment$status["biologicalData_OK"]),
                   paste("  Env. Covariate data     ", thisExperiment$status["covarData_OK"]),
                   paste("  Predicition covar. data ", thisExperiment$status["predictionData_OK"]),
                   paste("  Model fitted            ", thisExperiment$status["modelFit_OK"]),
                   "",
                   "-----------------------------------------------")
  
  return(cat(paste(statusLines, collapse = "\n")))
}
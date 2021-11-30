

#' Create a new cm experiment
#'
#' @param userID Character. User ID
#' @param experimentName Character. Name of the experiment assigned by the user
#'
#' @return An initialised cm_experiment object
#' @export
#'
#' @examples
#' \dontrun{
#' thisExperiment <- cm_create_new_experiment("user123", "Species assemblages model 1")
#' }
cm_create_new_experiment <- function(userID = "", experimentName = "")
{
  if (userID == "")
    stop("'userID' must be given a value")
  
  
  if (experimentName == "")
    stop("'experimentName' must be given a value")
  
  # Create (instantiate) a community modelling S3 object
  statusVec <- vector(length = 5) # type is "logical" by default
  names(statusVec) <- c("siteData_OK", "biologicalData_OK",
                        "covarData_OK", "predictionData_OK", "modelFit_OK")
  
  thisExperiment <- list(userID = userID,
                         experimentName = experimentName,
                         dateCreated = as.character(Sys.Date()),
                         dateDataUpdated = "",
                         dateLastModelRun = "",
                         status = statusVec,
                         includeGeo = TRUE,
                         data = list(siteData = list(srcFile = "",
                                                     siteCol = "",
                                                     longitudeCol = "",
                                                     latitudeCol = "",
                                                     dataTable = data.frame()),
                                     biologicalData = list(srcFile = "",
                                                           fileType = "",
                                                           sheet = "",
                                                           siteCol = "",
                                                           dataType = "",
                                                           presenceMarker = "1",
                                                           absenceMarker = "0",
                                                           dissimMeasure = "",
                                                           dataTable = data.frame(),
                                                           dissimMatrix = matrix()),
                                     covarData = list(srcFolder = "",
                                                      filenames = "",
                                                      label = "",
                                                      covarNames = "",
                                                      covarSiteMin = numeric(),
                                                      covarSiteMax = numeric(),
                                                      covarExtentMin = numeric(),
                                                      covarExtentMax = numeric(),
                                                      dataTable = matrix()),
                                     predictionData = list(srcFolder = "",
                                                           label = "",
                                                           covarNames = "",
                                                           covarSiteMin = numeric(),
                                                           covarSiteMax = numeric(),
                                                           covarExtentMin = numeric(),
                                                           covarExtentMax = numeric())),
                         model = list(gdm = list(),
                                      varImp = list(),
                                      perfSummary = list()))
  
  class(thisExperiment) <- c("cm_experiment", class(thisExperiment))
  
  return(thisExperiment)
}

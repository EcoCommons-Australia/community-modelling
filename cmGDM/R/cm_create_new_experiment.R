

#' Create a new cm experiment
#'
#' @param userID Character. User ID supplied by the system passed from the UI when a new experiment is created
#' @param userName Character. User name supplied by the system passed from the UI when a new experiment is created
#' @param experimentName Character. Name of the experiment assigned by the user passed from the UI when a new experiment is created
#' @param description Character. User-supplied description of the experiment passed from the UI when a new experiment is created
#'
#' @return An initialised cm_experiment object with side-effect of saving the new cm_experiment object to the user's experiment folder
#' @export
#'
#' @examples
#' \dontrun{
#' thisExperiment <- cm_create_new_experiment("user123", "Fred Fernackerpan",
#'                                            "Species assemblages model 1")
#' }
cm_create_new_experiment <- function(userID = "", userName = "", experimentName = "", description = "")
{
  if (userID == "")
    stop("'userID' must be given a value")
  
  if (experimentName == "")
    stop("'experimentName' must be given a value")
  
  # Make/check path for output for this experiment
  outFolder <- paste0(path.expand("~/cmGDM/"), experimentName)
  if (!dir.exists(outFolder)) dir.create(outFolder, recursive = TRUE)
  
  # Create (instantiate) a community modelling S3 object
  statusVec <- vector(length = 5) # type is "logical" by default
  names(statusVec) <- c("siteData_OK", "biologicalData_OK",
                        "covarData_OK", "predictionData_OK", "modelFit_OK")
  
  thisExperiment <- list(userID = userID,
                         userName = userName,
                         experimentName = experimentName,
                         dateCreated = as.character(Sys.Date()),
                         dateDataUpdated = "",
                         dateLastModelRun = "",
                         description = description,
                         status = statusVec,
                         includeGeo = FALSE,
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
                                                           filenames = "",
                                                           label = "",
                                                           covarNames = "",
                                                           covarSiteMin = numeric(),
                                                           covarSiteMax = numeric(),
                                                           covarExtentMin = numeric(),
                                                           covarExtentMax = numeric(),
                                                           dataTable = matrix()),
                                     sitepair = data.frame()),
                         model = list(gdm = list(),
                                      varImp = list(),
                                      perfSummary = list()))
  
  class(thisExperiment) <- c("cm_experiment", class(thisExperiment))
  
  saveRDS(thisExperiment, paste0(outFolder, "/cmGDM_", experimentName, ".rds"))
  
  return(thisExperiment)
}

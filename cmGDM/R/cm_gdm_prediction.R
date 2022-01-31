
#' Make predictions of change in composition
#'
#' @param thisExperiment cm_experiment. Experiment object to be used in making predictions
#' @param outFolder String. Path of the output folder
#' @param trace Logical. Should informative progress messages be output
#'
#' @return Returns an updated cm_experiment object and saves it to the experiment folder
#' @export
#'
#' @examples \dontrun{
#'   cm_predict_gdm(myExperiment, outFolder = "path/to/output")
#' }
cm_predict_gdm <- function(thisExperiment,
                           #covarSrc = NULL,
                           outFolder = "~/cmGDM/",
                           trace = FALSE)
{
  if (!("cm_experiment" %in% class(thisExperiment)))
    stop("Object passed in 'thisExperiment' must be of class 'cm_experiment'")
  
  if (!thisExperiment$status["modelFit_OK"])
    stop(paste("No GDM model has been successfully fitted for experiment", thisExperiment$experimentName))
  
  if (!thisExperiment$status["predictionData_OK"])
    stop("A prediction data set has not been successfully added to this experiment")
  
  # if (!covarSrc %in% c("train", "prediction"))
  #   stop("'dataSrc' must one of 'train' or 'prediction'")
  
  # Make/check path for output for this experiment
  outFolder <- paste0(path.expand(outFolder), thisExperiment$experimentName)
  if (!dir.exists(outFolder)) dir.create(outFolder, recursive = TRUE)
  
  if (trace) cat("Loading covariate layers and transforming using fitted GDM\n")
  #studyStack <- terra::rast(paste0(thisExperiment$data$covarData$srcFolder, "/", thisExperiment$data$covarData$filenames))
  #if (covarSrc == "train")
  #{
  trainStack <- raster::stack(paste0(thisExperiment$data$covarData$srcFolder, "/", thisExperiment$data$covarData$filenames))
  #}
  #else
  #{
  if (thisExperiment$status["predictionData_OK"])
  {
    predStack <- raster::stack(paste0(thisExperiment$data$predictionData$srcFolder, "/", thisExperiment$data$predictionData$filenames))
  }
  else
    stop("Cannot generate GDM predictions because a prediction covariate dataset has not yet been added to this experiment")
  #}
  
  predictionOutput <- predict(thisExperiment$model$gdm,
                              data = trainStack,
                              time = TRUE,
                              predData = predStack)
  
  raster::writeRaster(predictionOutput, paste0(outFolder, "/", thisExperiment$experimentName , "_GDM_prediction.tif"), overwrite = TRUE, options = "COMPRESS=DEFLATE")
  
  # Extract predicted compositional (i.e. dissimilarity) change at the sites/sample locations and store them in theExperiment object:
  siteCol <- thisExperiment$data$siteData$siteCol
  longCol <- thisExperiment$data$siteData$longitudeCol
  latCol <- thisExperiment$data$siteData$latitudeCol
  thisExperiment$model$sitePredictions <- raster::extract(predictionOutput, as.matrix(thisExperiment$data$siteData$dataTable[, c(longCol, latCol)]))
  
  saveRDS(thisExperiment, paste0(outFolder, "/cmGDM_", thisExperiment$experimentName, ".rds"))
  
  return(thisExperiment)
}

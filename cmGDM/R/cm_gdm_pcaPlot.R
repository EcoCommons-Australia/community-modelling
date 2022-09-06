#' GDM PCA plot of scaled covariates
#'
#' Produce a raster map colour-coded using the first three components of a Principal Component Analysis (PCA) of scaled covariates. Grid cells with the similar colours represent cells with the same combination of scaled environmental variables and therefore similar predicted community composition.  
#'
#' Output is (1) a PNG-formatted image and a companion 'wld' world coordinate file which allows the PNG image to be loaded into a GIS program as a raster layer, and (2) a 3-band geoTIFF raster file for use GIS programs and map production using R (e.g. tmap package).
#'
#' @param thisExperiment Object of class 'cm_experiment'
#' @param covarSrc Character string. Which covariate data set should be used to test for extrapolation? Default is "train', i.e. the data set used to fit or 'train' the GDM. Alternative is 'predict', which uses the prediction data set after first check that it has been uploaded by the user
#' @param outFolder Character string. Path of a folder into which the output GIS layer and PNG image will be written
#' @param trace Logical. Produce console messages? Default is FALSE, so we typically go quietly into the digital night
#'
#' @return Nothing but has side-effect of saving image and GIS raster files to the user's project folder for the experiment
#' @export
#'
#' @examples
#' \dontrun{
#' 
#' ## Produce plots:
#' cm_gdm_pcaPlot(myExperiment)
#' 
#' ## A simple map using package tmap:
#' library(raster)
#' library(tmap)
#' ## Path to geoTIFF in EcoCommons project folder - change as needed
#' ## if the tif file was downloaded and stored in a local folder:
#' tiffFile <- paste0("cmGDM_", thisExperiment$experimentName, "_GDM_transformed_PCA.tif")
#' rr <- raster::stack(paste0(path.expand(outFolder), thisExperiment$experimentName, "/", tiffFile)
#' theMap <- tmap::tm_shape(rr) +
#' tm_graticules(lines = FALSE, ticks = TRUE) +
#'   tm_xlab("Longitude") +
#'   tm_ylab("Latitude") +
#'   tm_rgb(r = 1, g = 2, b = 3, interpolate = FALSE)
#' 
#' print(theMap)
#' }
cm_gdm_pcaPlot <- function(thisExperiment,
                           covarSrc = "train",
                           outFolder = "~/cmGDM/",
                           trace = FALSE)
  
{
  if (!("cm_experiment" %in% class(thisExperiment)))
    stop("Object passed in 'thisExperiment' must be of class 'cm_experiment'")
  
  if (!thisExperiment$status["modelFit_OK"])
    stop(paste("No GDM model has been successfully fitted for experiment", thisExperiment$experimentName))
  
  # Make/check path for output for this experiment
  outFolder <- paste0(path.expand(outFolder), thisExperiment$experimentName)
  if (!dir.exists(outFolder)) dir.create(outFolder, recursive = TRUE)
  
  ### Test for available covar rasters...
  
  ########
  if (trace) cat("Loading covariate layers and transforming using fitted GDM\n")
  #studyStack <- terra::rast(paste0(thisExperiment$data$covarData$srcFolder, "/", thisExperiment$data$covarData$filenames))
  if (covarSrc == "train")
  {
    studyStack <- raster::stack(paste0(thisExperiment$data$covarData$srcFolder, "/", thisExperiment$data$covarData$filenames))
  }
  else
  {
    if (thisExperiment$status["predictionData_OK"])
    {
      studyStack <- raster::stack(paste0(thisExperiment$data$predictionData$srcFolder, "/", thisExperiment$data$predictionData$filenames))
    }
    else
      stop("Cannot generate PCA plot using prediction covariate dataset because it has not yet been added to this experiment")
  }
  
  #transStuff <- gdm::gdm.transform(thisExperiment$model$gdm, raster::stack(studyStack))
  transStuff <- gdm::gdm.transform(thisExperiment$model$gdm, studyStack)
  
  if (trace) cat("Computing PCA and running prediction\n")
  #transStuff_trimmed <- na.omit(terra::values(transStuff))
  transStuff_trimmed <- na.omit(raster::values(transStuff))
  
  pcaResult <- stats::prcomp(transStuff_trimmed)
  
  pcaRast <- raster::predict(transStuff, pcaResult, index = 1:3)
  if (trace) cat("Computing colour mapping\n")
  pcaRast[[1]] <- (pcaRast[[1]] - pcaRast[[1]]@data@min)/ (pcaRast[[1]]@data@max - pcaRast[[1]]@data@min)
  
  if (sum(range(pcaRast[[2]][], na.rm = TRUE)) != 0) # There has got be a smarter way than this...
  {
    pcaRast[[2]] <- (pcaRast[[2]] - pcaRast[[2]]@data@min)/ (pcaRast[[2]]@data@max - pcaRast[[2]]@data@min)
    pcaRast[[3]] <- (pcaRast[[3]] - pcaRast[[3]]@data@min)/ (pcaRast[[3]]@data@max - pcaRast[[3]]@data@min)
  }
  
  if (trace) cat("Preparing PNG image and world coordinate file\n")
  redMat <- matrix(pcaRast[[1]][], nrow = raster::nrow(pcaRast[[1]]), ncol = raster::ncol(pcaRast[[1]]), byrow = TRUE)
  blueMat <- matrix(pcaRast[[2]][], nrow = raster::nrow(pcaRast[[2]]), ncol = raster::ncol(pcaRast[[2]]), byrow = TRUE)
  greenMat <- matrix(pcaRast[[3]][], nrow = raster::nrow(pcaRast[[3]]), ncol = raster::ncol(pcaRast[[3]]), byrow = TRUE)
  
  pixMat <- array(NA, dim = c(raster::nrow(pcaRast[[1]]), raster::ncol(pcaRast[[1]]), 3))
  pixMat[ , , 1] <- redMat
  pixMat[ , , 2] <- blueMat
  pixMat[ , , 3] <- greenMat
  
  png::writePNG(pixMat, paste0(outFolder, "/cmGDM_", thisExperiment$experimentName , "_GDM_transformed_PCA.png"))
  
  worldData <- c(raster::res(pcaRast[[1]])[2],
                 0,
                 0,
                 -raster::res(pcaRast[[1]])[1],
                 raster::extent(pcaRast[[1]])@xmin + raster::res(pcaRast[[1]])[2]/2,
                 raster::extent(pcaRast[[1]])@ymax - raster::res(pcaRast[[1]])[1]/2)
  
  # Could output the world file with a pgw extension...
  # writeLines(as.character(worldData), paste0(outFolder, "/cmGDM_", thisExperiment$experimentName , "_GDM_transformed_PCA.pgw"))
  # but seems best to go with the ESRI version i.e. wld extension which seems to fit the GDLA docs
  writeLines(as.character(worldData), paste0(outFolder, "/cmGDM_", thisExperiment$experimentName , "_GDM_transformed_PCA.wld"))
  
  # Now perform translation to geoTIFF 3-band file...
  gdalUtilities::gdal_translate(src_dataset = paste0(path.expand(outFolder), "/cmGDM_", thisExperiment$experimentName , "_GDM_transformed_PCA.png"),
                                dst_dataset = paste0(path.expand(outFolder), "/cmGDM_", thisExperiment$experimentName , "_GDM_transformed_PCA.tif"),
                                b = 1:3,
                                a_srs = "EPSG:4326")
}

#' GDM PCA plot of scaled covariates
#'
#' Produce a raster map colour-coded using the first three components of a Principal Component Analysis (PCA) of scaled covariates. Grid cells with the similar colours represent cells with the same combination of scaled environmental variables and therefore similar predicted community composition.  
#'
#' Both graphics output (as a PNG-formatted image) and a GeoTIFF raster are output.
#'
#' @param thisExperiment Object of class 'cm_experiment'
#' @param outFolder Character (string). Path of a folder into which the output GIS layer and PNG image will be written
#' @param trace Logical. Produce console messages? Default is FALSE
#'
#' @return Nothing
#' @export
#'
#' @examples
#' \dontrun{
#' cm_gdm_pcaPlot(myExperiemnt, outFolder = "exprimentResults")
#' }
cm_gdm_pcaPlot <- function(thisExperiment,
                           outFolder = "",
                           trace = FALSE)
  
{
  if (!("cm_experiment" %in% class(thisExperiment)))
    stop("Object passed in 'thisExperiment' must be of class 'cm_experiment'")
  
  if (!thisExperiment$status["modelFit_OK"])
    stop(paste("No GDM model has been successfully fitted for experiment", thisExperiment$experimentName))
  
  ### Test for available covar rasters...
  
  
  
  ########
  if (trace) cat("Loading covariate layers and transforming using fitted GDM")
  studyStack <- terra::rast(paste0(thisExperiment$data$covarData$srcFolder, "/", thisExperiment$data$covarData$filenames))
  transStuff <- gdm::gdm.transform(thisExperiment$model$gdm, raster::stack(studyStack))
  
  if (trace) cat("Computing PCA and running prediction")
  transStuff_trimmed <- na.omit(terra::values(transStuff))
  
  pcaResult <- stats::prcomp(transStuff_trimmed)
  
  pcaRast <- stats::predict(transStuff, pcaResult, index = 1:3)
  if (trace) cat("Computing colour mapping")
  pcaRast[[1]] <- (pcaRast[[1]] - pcaRast[[1]]@data@min)/ (pcaRast[[1]]@data@max - pcaRast[[1]]@data@min) * 255
  
  if (sum(range(pcaRast[[2]][], na.rm = TRUE)) != 0) # There has got be a smarter way than this...
  {
    pcaRast[[2]] <- (pcaRast[[2]] - pcaRast[[2]]@data@min)/ (pcaRast[[2]]@data@max - pcaRast[[2]]@data@min) * 255
    pcaRast[[3]] <- (pcaRast[[3]] - pcaRast[[3]]@data@min)/ (pcaRast[[3]]@data@max - pcaRast[[3]]@data@min) * 255
  }
  
  newRas <- pcaRast[[1]]
  goodCells <- which(!is.na(newRas))
  
  redPart <- trunc(na.omit(pcaRast[[1]][]) + 0.5)/255
  greenPart <- trunc(na.omit(pcaRast[[2]][]) + 0.5)/255
  bluePart <- trunc(na.omit(pcaRast[[3]][]) + 0.5)/255
  
  #colVals <- rgb(redPart, greenPart, bluePart)
  
  newRas[goodCells]  <- grDevices::rgb(redPart, greenPart, bluePart)
  
  # Save raster
  if (trace) cat("Saving GIS raster")
  terra::writeRaster(terra::rast(newRas),
                     filename = paste0(outFolder, "/", thisExperiment$experimentName , "_GDM_transformed_PCA.tif"),
                     overwrite = TRUE,
                     gdal = "COMPRESS=DEFLATE" )
  
  if (trace) cat("Saving PNG image")
  # render as png and save
  grDevices::png(paste0(outFolder, "/", thisExperiment$experimentName , "_GDM_transformed_PCA.png"),
      width = 1024, height = 1024)
  raster::plotRGB(pcaRast, r = 1, g = 2, b = 3, main = thisExperiment$experimentName)
  grDevices::dev.off()
}
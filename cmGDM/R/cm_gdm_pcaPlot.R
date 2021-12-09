



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
  studyStack <- terra::rast(paste0(thisExperiment$data$covarData$srcFolder, "/", thisExperiment$data$covarData$filenames))
  transStuff <- gdm::gdm.transform(thisExperiment$model$gdm, raster::stack(studyStack))
  
  cat("done\nComputing PCA...")
  transStuff_trimmed <- na.omit(terra::values(transStuff))
  
  pcaResult <- prcomp(transStuff_trimmed)
  
  pcaRast <- predict(transStuff, pcaResult, index = 1:3)
  cat("done\nComputing colour mapping...")
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
  
  newRas[goodCells]  <- rgb(redPart, greenPart, bluePart)
  
  # Save raster
  terra::writeRaster(terra::rast(newRas),
                     filename = paste0(outFolder, "/", thisExperiment$experimentName , "_GDM_transformed_PCA.tif"),
                     overwrite = TRUE,
                     gdal = "COMPRESS=DEFLATE" )
  
  # render as png and save
  png(paste0(outFolder, "/", thisExperiment$experimentName , "_GDM_transformed_PCA.png"),
      width = 1024, height = 1024)
  raster::plotRGB(pcaRast, r = 1, g = 2, b = 3, main = thisExperiment$experimentName)
  dev.off()
}
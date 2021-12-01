
#' Show extrapolation
#'
#' Models applied to covariate values outside the range used to fit the model cannot be trusted. EXTRREME caution is required when interpreting results in areas where extrapolation has occurred. This function identifies the regions where extrapolation has occurred, and outputs PNG and geoTIFF files showing grid cells within which extrapolation has NOT occurred (i.e. "good" cells)
#'
#' @param thisExperiment cm_experiment object. The experiment with a fitted GDM to be processed
#' @param extrapType Character string. Show extrapolation for all covariates presented to model fitting ("all"), or just those with variable importance > 0 ("important") 
#' @param allPlots Logical. Show extrapolation plots for each covariate plus the overall extrapolation plot (TRUE) or just the overall plot (default, FALSE)
#' @param outFolder Character string. Full path to a folder to be used for output geoTIFF and PNG files
#'
#' @return
#' @export
#'
#' @examples
cm_show_extrapolation <- function(thisExperiment,
                                  extrapType = c("all", "important"),
                                  allPlots = FALSE,
                                  outFolder = "")
  
{
  if (!("cm_experiment" %in% class(thisExperiment)))
    stop("Object passed in 'thisExperiment' must be class cm_experiment")
  
  if (!thisExperiment$status["modelFit_OK"])
    stop("Cannot generate performance plots: no successfully fitted GDM found in 'thisExperiment'")
  
  if (!(toupper(extrapType) %in% c("ALL", "IMPORTANT")))
    stop("'showVarImp' must be one of 'all' or 'important'")
  
  if (!dir.exists(outFolder))
    dir.create(outFolder, recursive = TRUE)
  
  covarFiles <- paste0(thisExperiment$data$covarData$srcFolder, "/",
                       thisExperiment$data$covarData$filenames)
  
  covarStack <- terra::rast(covarFiles)
  
  if (extrapType == "all")
  {
    ### Type 1: Strict extrapolation for all covariates presented during model fitting
    outStack <- covarStack
    
    covarNames <- gsub(".tif", "", thisExperiment$data$covarData$filenames, fixed = TRUE)
    
    for (thisVarInd in 1:19)
    {
      covarRange_site <- c(thisExperiment$data$covarData$covarSiteMin[thisVarInd],
                           thisExperiment$data$covarData$covarSiteMax[thisVarInd])
      
      reclassMat <- matrix(c(covarRange_site, 1), nrow = 1)
      
      extrapRas <- terra::classify(covarStack[[thisVarInd]], reclassMat, othersNA = TRUE)
      
      if (allPlots)
      {
        png(file.path(outFolder, paste0("cm_gdm_extrapolation_", covarNames[thisVarInd]), ".png"),
            width = 1024, height = 1024)
        plot(extrapRas, main = paste0("Good cells: ", covarNames[thisVarInd]))
        dev.off()
        
        terra::writeRaster(extrapRas, paste0("cm_gdm_extrapolation_", covarNames[thisVarInd], ".tif"), gdal = "COMPRESS=DEFLATE")
      }
      
      outStack[[thisVarInd]] <- extrapRas
    }
    
    png(file.path(outFolder, paste0("cm_gdm_extrapolation_ALL_covariates.png")),
        width = 1024, height = 1024)
    plot(sum(outStack), main = "Type 1: Good cells (no extrap.)")
    dev.off()
    
    terra::writeRaster(extrapRas, "cm_gdm_extrapolation_ALL_covariates.tif", gdal = "COMPRESS=DEFLATE")
  }
  else
  {
    #### Type 2: Strict extrapolation for only those covariates which have var imp > 0
    varImp <- 100*(thisExperiment$model$varImp[[2]][, 1]/sum(thisExperiment$model$varImp[[2]][, 1]))
    
    usedCovars <- names(varImp[varImp > 0])
    
    theseVarInd <- which(covarNames %in% usedCovars)
    outStack <- covarStack
    
    for (thisVarInd in theseVarInd)
    {
      covarRange_site <- c(thisExperiment$data$covarData$covarSiteMin[thisVarInd],
                           thisExperiment$data$covarData$covarSiteMax[thisVarInd])
      
      reclassMat <- matrix(c(covarRange_site, 1), nrow = 1)
      
      testRas <- terra::classify(covarStack[[thisVarInd]], reclassMat, othersNA = TRUE)
      
      png(file.path(outFolder, paste0("cm_gdm_extrapolation_", covarNames[thisVarInd]), ".png"),
          width = 1024, height = 1024)
      plot(extrapRas, main = paste0("Good cells: ", covarNames[thisVarInd]))
      dev.off()
      
      outStack[[thisVarInd]] <- testRas
    }
    
    png(file.path(outFolder, paste0("cm_gdm_extrapolation_IMPORTANT_covariates.png")),
        width = 1024, height = 1024)
    plot(sum(outStack[[theseVarInd]]), main = "Type 2: Good cells (no extrap.)")
    dev.off()
    
    terra::writeRaster(extrapRas, "cm_gdm_extrapolation_IMPORTANT_covariates.tif", gdal = "COMPRESS=DEFLATE")
  }
}
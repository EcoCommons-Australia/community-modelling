
#' Show extrapolation
#'
#' Models applied to covariate values outside the range used to fit the model cannot be trusted.
#' 
#' EXTREME caution is required when interpreting results in areas where extrapolation has occurred.
#' 
#' This function identifies the regions where extrapolation has occurred, and outputs PNG and geoTIFF files showing grid cells within which extrapolation has NOT occurred (i.e. "good" cells)
#'
#' @param thisExperiment cm_experiment object. The experiment with a fitted GDM to be processed
#' @param covarSrc Character string. Which covariate data set should be used to test for extrapolation? Default is "train', i.e. the data set used to fit or 'train' the GDM. Alternative is 'predict', which uses the prediction data set after first check that it has been uploaded by the user
#' @param showVarImp Character string. Show extrapolation for all covariates presented to model fitting ("all"), or just those with variable importance > 0 ("important") 
#' @param allPlots Logical. Show extrapolation plots for each covariate plus the overall extrapolation plot (TRUE) or just the overall plot (default, FALSE)
#' @param outFolder Character string. Full path to a folder to be used for output geoTIFF and PNG files
#'
#' @return Nothing
#' @export
#'
#' @examples
#' \dontrun{
#' # Show areas with NO extrapolation for all variables
#' cm_show_extrapolation(myExperiment, showVarImp = "all")
#' 
#' # Show areas with No extrapolation but only for IMPORTANT variables
#' cm_show_extrapolation(myExperiment, showVarImp = "important")
#' 
#' # Show extrapolation for important varaible using hte prediction data set
#' cm_show_extrapolation(myExperiment, covarSrc = "predict", showVarImp = "important")
#' }
cm_show_extrapolation <- function(thisExperiment,
                                  covarSrc = "train",
                                  showVarImp = c("all", "important"),
                                  allPlots = FALSE,
                                  outFolder = "~/cmGDM/")
  
{
  if (!("cm_experiment" %in% class(thisExperiment)))
    stop("Object passed in 'thisExperiment' must be class cm_experiment")
  
  if (!thisExperiment$status["modelFit_OK"])
    stop("Cannot generate performance plots: no successfully fitted GDM found in 'thisExperiment'")
  
  if (!(toupper(showVarImp) %in% c("ALL", "IMPORTANT")))
    stop("'showVarImp' must be one of 'all' or 'important'")

  # Make/check path for output for this experiment
  outFolder <- paste0(outFolder, thisExperiment$experimentName)
  if (!dir.exists(outFolder)) dir.create(outFolder, recursive = TRUE)
  
  if (covarSrc == "train")
  {
    # We can assume that there is covariate data is available because we checked
    # earlier that a successfully fitted model is present i.e.
    # thisExperiment$status["modelFit_OK"] == TRUE
  covarFiles <- paste0(thisExperiment$data$covarData$srcFolder, "/",
                       thisExperiment$data$covarData$filenames)
  
  covarStack <- terra::rast(covarFiles)
  }
  else
  {
    # Unlike the "train" branch, we must check that prediction data has been
    # successfully loaded because a model fit is not dependent on prior upload
    # of prediction covariate data
    if (covarSrc == "predict")
    {
      if (thisExperiment$status["predictionData_OK"])
      {
        covarFiles <- paste0(thisExperiment$data$predictionData$srcFolder, "/",
                             thisExperiment$data$predictionData$filenames)
        
        covarStack <- terra::rast(covarFiles)
        
      }
      else
        stop("Cannot test for extrapolation using prediction covariate dataset beacuase it has not yet been added to this experiment")
    }
  }
  
  if (showVarImp == "all")
  {
    ### Type 1: Strict extrapolation for all covariates presented during model fitting
    outStack <- covarStack
    
    covarNames <- gsub(".tif", "", thisExperiment$data$covarData$filenames, fixed = TRUE)
    
    for (thisVarInd in 1:length(thisExperiment$data$covarData$covarSiteMin))
    {
      covarRange_site <- c(thisExperiment$data$covarData$covarSiteMin[thisVarInd],
                           thisExperiment$data$covarData$covarSiteMax[thisVarInd])
      
      reclassMat <- matrix(c(covarRange_site, 1), nrow = 1)
      
      extrapRas <- terra::classify(covarStack[[thisVarInd]], reclassMat, othersNA = TRUE)
      
      if (allPlots)
      {
        grDevices::png(file.path(outFolder, paste0("cm_gdm_extrapolation_", covarNames[thisVarInd]), ".png"),
            width = 1024, height = 1024)
        plot(extrapRas, main = paste0("Good cells: ", covarNames[thisVarInd]))
        grDevices::dev.off()
        
        terra::writeRaster(extrapRas, paste0(outFolder, "/cmGDM_extrapolation_", covarNames[thisVarInd], "_no_extrapolation.tif"), gdal = "COMPRESS=DEFLATE")
      }
      
      outStack[[thisVarInd]] <- extrapRas
    }
    
    grDevices::png(file.path(outFolder, paste0("cm_gdm_extrapolation_ALL_covariates.png")),
        width = 1024, height = 1024)
    plot(sum(outStack), main = "Type 1: Good cells (no extrap.)")
    grDevices::dev.off()
    
    terra::writeRaster(extrapRas, paste0(outFolder, "/cmGDM_extrapolation_ALL_covariates_no_extrapolation.tif"), gdal = "COMPRESS=DEFLATE")
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
      
      grDevices::png(file.path(outFolder, paste0("cmGDM_extrapolation_", covarNames[thisVarInd]), "_no_extrapolation.png"),
          width = 1024, height = 1024)
      plot(extrapRas, main = paste0("Good cells: ", covarNames[thisVarInd]))
      grDevices::dev.off()
      
      outStack[[thisVarInd]] <- testRas
    }
    
    grDevices::png(file.path(outFolder, paste0("cm_gdm_extrapolation_IMPORTANT_covariates.png")),
        width = 1024, height = 1024)
    plot(sum(outStack[[theseVarInd]]), main = "Type 2: Good cells (no extrap.)")
    grDevices::dev.off()
    
    terra::writeRaster(extrapRas, "cm_gdm_extrapolation_IMPORTANT_covariates_no_extrapolation.tif", gdal = "COMPRESS=DEFLATE")
  }
}

#' Run Community Modelling GDM experiment
#'
#' Experiments can only be run when all preceding steps in the workflow have been successfully completed.
#' 
#' @param thisExperiment cm_experiment. Object for the current experiment
#' @param outFolder String. Path to the experiment output folder
#' @param includeGeo Logical. Should geographical distance between sites/samples be added as a covariate?
#' @param calc_varImp Logical. Should variable importance calculations be run?
#' @param trace Logical. Produce helpful diagnostic messages? Default is FALSE, therefore radio silence is maintained until told otherwise
#'
#' @return cm_experiment object with updated fields with side-effect of saving the updated cm_experiment object to the user's experiment folder
#' @export
#'
#' @examples
#' \dontrun{
#' 
#' anotherExperiment <- cm_run_gdm_experiment(anotherExperiment)
#' 
#' }
cm_run_gdm_experiment <- function(thisExperiment,
                                  outFolder = "~/cmGDM/",
                                  includeGeo = FALSE,
                                  calc_varImp = FALSE,
                                  trace = FALSE)
{
  numCores <- 10
  
  if (!("cm_experiment" %in% class(thisExperiment)))
    stop("Object passed in 'thisExperiment' must be class cm_experiment")
  
  # Test for minimum data ensemble to fit a GDM
  if (!all(thisExperiment$status[c("siteData_OK", "biologicalData_OK",
                                   "covarData_OK")]))
    stop(paste("Sorry human, I cannot run your experiment. You must fix these issues:\n",
               cm_bad_experiment_msg(thisExperiment)))
  
  # Make/check path for output for this experiment
  outFolder <- paste0(path.expand(outFolder), thisExperiment$experimentName)
  if (!dir.exists(outFolder)) dir.create(outFolder, recursive = TRUE)
  
  # Bravely attempt to prepare data set for gdm fitting...this always ends in
  # tears before bedtime before we have a clean data build!
  thisSiteCol <- thisExperiment$data$siteData$siteCol
  thisLongitudeCol <- thisExperiment$data$siteData$longitudeCol
  thisLatitudeCol <- thisExperiment$data$siteData$latitudeCol
  
  # Now assemble the environmental predictors table...
  envTable <- data.frame(site = thisExperiment$data$siteData$dataTable[, thisSiteCol],
                         X = thisExperiment$data$siteData$dataTable[, thisLongitudeCol],
                         Y = thisExperiment$data$siteData$dataTable[, thisLatitudeCol],
                         thisExperiment$data$covarData$dataTable,
                         stringsAsFactors = FALSE)
  #colnames(envTable)[2:3] <- c("X", "Y")
  
  if (trace)
  {
    cat("Covariate data table:\n=================================\n")
    print(thisExperiment$data$covarData$dataTable)
    
    cat("\nAssembled environmental data table:\n========================================\n")
    print(envTable)
  }
  
  # Add site column to bioData matrix as required for GDM data preparation
  bio_table <- data.frame(site = thisExperiment$data$siteData$dataTable[, thisSiteCol],
                          thisExperiment$data$biologicalData$dissimMatrix,
                          stringsAsFactors = FALSE)
  
  dataStuff <- gdm::formatsitepair(bioData = bio_table,
                                   bioFormat = 3,
                                   siteColumn = "site",
                                   XColumn = "X",
                                   YColumn = "Y",
                                   predData = envTable)
  
  if (trace)
  {
    cat("Output from gdm::formatsitepair():\n========================================\n")
    print(dataStuff)
  }
  
  # Store sitepair table:
  thisExperiment$data$sitepair <- dataStuff
  
  # Record the state of the param includeGeo in the cm_experiment object:
  thisExperiment$includeGeo <- includeGeo
  
  # Fit a gdm:
  gdmModel <- gdm::gdm(dataStuff, geo = thisExperiment$includeGeo)
  
  if (is.null(gdmModel)) # The model failed
  {
    cat("Model fit for experiment '", thisExperiment$experimentName, "' has failed\n", sep = "")
  }
  else
  {
    cat("Model fit for experiment '", thisExperiment$experimentName, "' was successful\n", sep = "")
    thisExperiment$dateLastModelRun <- as.character(Sys.Date())
    thisExperiment$status["modelFit_OK"] <- TRUE
    thisExperiment$model$gdm <- gdmModel
    
    
    # Variable importance computations
    if (calc_varImp)
    {
    #if (trace) print(dataStuff)
    ans <- gdm::gdm.varImp(dataStuff, geo = thisExperiment$includeGeo, parallel = TRUE, fullModelOnly = FALSE,
                           cores = numCores) #,
    #outFile = paste0(outFolder, "/", this_Taxon, "_variable_importance_results.csv"))
    #varImportance <- data.frame(importance = round(100*ans[[2]][, 1]/sum(ans[[2]][, 1]),2))
    thisExperiment$model$varImp <- ans #100*ans[[2]][, 1]/sum(ans[[2]][, 1])
    }
  }
  
  if (trace)
  {
    cat("\nResults of variable importance computation:\n==================================================\n")
    print(ans)
    cat("\n Ready to return\n")
  }
  
  saveRDS(thisExperiment, paste0(outFolder, "/cmGDM_", thisExperiment$experimentName, ".rds"))
  
  return(thisExperiment)
}

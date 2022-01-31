
#' Load Prediction Covariate Data
#'
#' Load covariate data from GIS layers to be used to predict dissimilarities between sites/samples under different environmental conditions. Checks are made to ensure that the prediction data matches the covariates used to fit a GDM
#'
#' @param thisExperiment cm_experiment object. Object for the current experiment. NOTE: Require site data to have been successfully added to the experiment before this method is called 
#' @param src_folder Character. Path to the folder storing raster layers 
#' @param pred_filenames Character vector. Vector giving the names of the files in src-folder to be used as prediction covariates in this experiment
#' @param label Character. An optional human-friendly label for the project's environmental data
#' @param outFolder Character. Path to user's experiment folder
#' @param trace Logical. Produce helpful diagnostic messages? Default is FALSE, therefore radio silence is maintained until told otherwise
#' 
#' @return Returns an updated copy of the cm_experiment object passed in parameter \emph{thisExperiment} and saves a copy to the user's project folder for the experiment
#' @export
#'
#' @examples
#' \dontrun{
#' # Assume a previously created cm_experiment object 'Butterfly_effect'
#' 
#' dataFolder <- "/path/to/GIS_layers"
#' 
#' # Make character vector of ALL GIS layer files in the source folder
#' #### NOTE: File names MUST be the same as loaded by cm_load_covars
#' #### even though they may cover different regions and/or times
#' theFiles <- list.files(dataFolder, '*.*')
#' Butterfly_effect <- cm_load_pred_data(Butterfly_effect,
#'                                       dataFolder,
#'                                       theFiles)
#' 
#' # Load only selected subset of files:
#' theFiles <- c("bio01.tif", "bio05.tif", "TPI.tif")
#' Butterfly_effect <- cm_load_pred_data(Butterfly_effect,
#'                                       dataFolder,
#'                                       theFiles)
#' }
cm_load_prediction_data <- function(thisExperiment,
                                    src_folder = "",
                                    pred_filenames = "",
                                    label = "",
                                    outFolder = "~/cmGDM/",
                                    trace = FALSE)
{
  if (!("cm_experiment" %in% class(thisExperiment)))
    stop("Object passed in 'thisExperiment' must be of class 'cm_experiment'")
  
  # Make/check path for output for this experiment
  outFolder <- paste0(path.expand(outFolder), thisExperiment$experimentName)
  if (!dir.exists(outFolder)) dir.create(outFolder, recursive = TRUE)
  
  missingLabels <- c("Site/sample data", "Biological data", "Covariate data")
  
  if (!all(thisExperiment$status[c("siteData_OK", "biologicalData_OK", "covarData_OK")]))
  {
    missingInd <- which(thisExperiment$status[c("siteData_OK", "biologicalData_OK",
                                                "covarData_OK")])
    stop(paste("Please added:", paste(missingLabels[missingInd], collapse = ", "), "before adding prediction covariate data"))
  }
  
  if (!dir.exists(src_folder)) stop("Folder given in 'src_folder' cannot be found")
  
  pred_file_paths <- paste0(src_folder, "/", pred_filenames)
  if (!all(file.exists(pred_file_paths)))
    stop(paste0("The following covariate file(s) could not be found: ",
                paste(pred_file_paths[which(!file.exists(pred_file_paths))], collapse = ", ")))

  if (length(pred_filenames) < length(thisExperiment$data$covarData$covarNames))
    stop("Number of prediction covariates is less then the number of covariates for fitting GDM: they must be the same")
  
  if (length(pred_filenames) > length(thisExperiment$data$covarData$covarNames))
    stop("Number of prediction covariates is greater then the number of covariates for fitting GDM: they must be the same")
  
  predCovarNames <- gsub(pattern = "(.*)\\..*$", replacement = "\\1", pred_filenames)
  ii <- order(predCovarNames)
  predCovarNames <- predCovarNames[ii]
  pred_filenames <- pred_filenames[ii]
  
  if (!all(sort(predCovarNames)) == sort(thisExperiment$data$covarData$covarNames))
    stop("Names of prediction covariates must match names of covariates used to fit the model")
    
  # ?? try-except or try-catch
  covarStack <- terra::rast(pred_file_paths)
  
  if (trace)
  {
    print(class(covarStack))
  }
  
  siteEnvData <- terra::extract(covarStack,
                                 thisExperiment$data$siteData$dataTable[, c(thisExperiment$data$siteData$longitudeCol,
                                                                            thisExperiment$data$siteData$latitudeCol)])
  
  if (trace)
  {
    print(siteEnvData)
  }
  
  ############# How to deal with NAs in extracted data? GDM cannot be run on
  ############# such data, so must raise "exception" and stop I suppose
  siteNA <- which(is.na(rowSums(siteEnvData)))
  
  if (length(siteNA) > 0)
    stop(paste("Some sites have missing environmental covariate data; they are:",
               paste(thisExperiment$data$siteData$dataTable[siteNA, thisExperiment$data$siteData$siteCol], collapse = ", ")))
  
  siteEnvRange <- Rfast::colMinsMaxs(siteEnvData)
  
  extentEnvRange <- Rfast::colMinsMaxs(as.matrix(covarStack))
  
  thisExperiment$dateDataUpdated <- as.character(Sys.Date())
  thisExperiment$status["predictionData_OK"] <- TRUE
  thisExperiment$data$predictionData$srcFolder <- src_folder
  thisExperiment$data$predictionData$filenames <- pred_filenames
  thisExperiment$data$predictionData$label <- label
  thisExperiment$data$predictionData$covarNames <- predCovarNames
  thisExperiment$data$predictionData$covarSiteMin <- siteEnvRange["min", ]
  thisExperiment$data$predictionData$covarSiteMax <- siteEnvRange["max", ]
  thisExperiment$data$predictionData$covarExtentMin <- extentEnvRange["min", ]
  thisExperiment$data$predictionData$covarExtentMax <- extentEnvRange["max", ]
  thisExperiment$data$predictionData$dataTable <- siteEnvData
  
  saveRDS(thisExperiment, paste0(outFolder, "/cmGDM_", thisExperiment$experimentName, ".rds"))
  
  return(thisExperiment)
}

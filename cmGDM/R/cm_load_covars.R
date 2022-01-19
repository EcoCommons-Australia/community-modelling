
#' Load Covariate Data
#'
#' @param thisExperiment cm_experiment object. Object for the current experiment. NOTE: Require site data to have been successfully added to the experiment before this method is called 
#' @param src_folder Character. Path to the folder storing raster layers 
#' @param covar_filenames Character vector. Vector giving the names of the files in src-folder to be used as covariates in this experiment
#' @param label Character. An optional human-friendly label for the project's environmental data
#' @param trace Logical. Produce helpful diagnostic messages? Default is FALSE, therefore radio silence is maintained until told otherwise
#' 
#' @return Returns an updated copy of the cm_experiment object passed in parameter \emph{thisExperiment}
#' @export
#'
#' @examples
#' \dontrun{
#' # Assume a previously created cm_experiment object 'Butterfly_effect'
#' 
#' dataFolder <- "/path/to/GIS_layers"
#' 
#' # Make character vector of ALL GIS layer files in the source folder
#' theFiles <- list.files(dataFolder, '*.*')
#' Butterfly_effect <- cm_load_covar_data(Butterfly_effect,
#'                                        dataFolder,
#'                                        theFiles)
#' 
#' # Load only selected subset of files:
#' theFiles <- c("bio01.tif", "bio05.tif", "TPI.tif")
#' Butterfly_effect <- cm_load_covar_data(Butterfly_effect,
#'                                        dataFolder,
#'                                        theFiles)
#' }
#' 
cm_load_covar_data <- function(thisExperiment,
                               src_folder = "",
                               covar_filenames = "",
                               label = "",
                               trace = FALSE)
{
  # Gather details about the user's selection of covariate rasters and update cm_experiment object passed in thisExperiment
  
  # Sanity checks...
  if (!thisExperiment$status["siteData_OK"]) stop("Please added site/sample location information to this experiment before adding covariate data")
  
  if (!dir.exists(src_folder)) stop("Folder given in 'src_folder' cannot be found")
  
  covar_file_paths <- paste0(src_folder, "/", covar_filenames)
  if (!all(file.exists(covar_file_paths)))
    stop(paste0("The following covariate file(s) could not be found: ",
                paste(covar_file_paths[which(!file.exists(covar_file_paths))], collapse = ", ")))
  
  # ?? try-except or try-catch
  covarStack <- terra::rast(covar_file_paths)
  
  if (trace)
  {
    cat("covarStack was loaded\n")
  }
  
  # Extract data forcing output to be a matrix - a data.frame is produced contrary to docs for the function!
  siteEnvData <- as.matrix(terra::extract(covarStack,
                                thisExperiment$data$siteData$dataTable[, c(thisExperiment$data$siteData$longitudeCol,
                                                                           thisExperiment$data$siteData$latitudeCol)]))
  
  # Drop ID column inserted by terra::extract. WHY DOES terra::extract DO THIS????
  siteEnvData <- siteEnvData[, -1]
  
  if (trace)
  {
    cat("Dump of siteEnvData:\n=========================\n")
    print(siteEnvData)
  }
  
  ############# How to deal with NAs in extracted data? GDM cannot be run on
  ############# such data, so must raise "exception" and stop I suppose
  siteNA <- which(is.na(rowSums(siteEnvData)))
  
  if (length(siteNA) > 0)
    stop(paste("Some sites have missing environmental covariate data; they are:",
               paste(thisExperiment$data$siteData$dataTable[siteNA, thisExperiment$data$siteData$siteCol], collapse = ", ")))
  
  siteEnvRange <- Rfast::colMinsMaxs(siteEnvData)
  
  if (trace)
  {
    cat("Dump of siteEnvRange:\n==========================\n")
    print(siteEnvRange)
  }
  
  extentEnvRange <- Rfast::colMinsMaxs(as.matrix(terra::extract(covarStack, terra::ext(covarStack))))
  
  if (trace)
  {
    cat("Dump of extentEnvRange:\n=========================\n")
    print(extentEnvRange)
  }
  
  thisExperiment$dateDataUpdated <- as.character(Sys.Date())
  thisExperiment$status["covarData_OK"] <- TRUE
  thisExperiment$data$covarData$srcFolder <- src_folder
  thisExperiment$data$covarData$filenames <- covar_filenames
  thisExperiment$data$covarData$covarNames <- gsub(pattern = "(.*)\\..*$", replacement = "\\1", covar_filenames)
  thisExperiment$data$covarData$label <- label
  thisExperiment$data$covarData$dataTable <- siteEnvData
  thisExperiment$data$covarData$covarSiteMin <- siteEnvRange["min", ]
  thisExperiment$data$covarData$covarSiteMax <- siteEnvRange["max", ]
  thisExperiment$data$covarData$covarExtentMin <- extentEnvRange["min", ]
  thisExperiment$data$covarData$covarExtentMax <- extentEnvRange["max", ]
  thisExperiment$data$covarData$dataTable <- siteEnvData
  
  if (trace)
  {
    cat("'thisExperiment' object updated and ready to return\n")
  }
  
  return(thisExperiment)
}

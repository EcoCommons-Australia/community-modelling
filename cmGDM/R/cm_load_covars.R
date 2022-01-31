
#' Load Covariate Data
#' 
#' Covariate (environmental or ecological) data required to fit the GDM is loaded by the user supplying either (a) a path to a folder of GIS raster layers, or (b) a table of pre-assembled environmental values at the site/sample locations.
#' 
#' For option (a), the function will load and store the data for the full geographical extent of the rasters, and extract and store a table of environmental data at the site/sample locations present in the site data section of the cm_experiment object passed in \emph{thisExperiment}. The raster data may be used for predicting across the full geographical extent when a model is fitted (e.g. using \link{cm_load_prediction_data}) or for other analyses (e,g, using \link{cm_gdm_pcaPlot} or \link{cm_show_extrapolation}).
#' 
#' For option (b), the table is stored in the covariate data section of the cm_experiment object passed in \emph{thisExperiment}. Modelling fitting may proceed by a call to \link{cm_run_gdm_experiment}, but options for further analyses are extremely limited until rasters are loaded, or a prediction data set is loaded by calling \link{cm_load_prediction_data}.
#'
#' @param thisExperiment cm_experiment object. Object for the current experiment. NOTE: Require site data to have been successfully added to the experiment before this method is called 
#' @param src_folder Character. Path to the folder storing raster layers 
#' @param covar_filenames Character vector. Vector giving the names of the files in src-folder to be used as covariates in this experiment
#' @param covar_filename string. Name of a single data file containing tabulated covariate data
#' @param siteCol Integer or string. Column in table when a single table is being loaded which stores site/sample names
#' @param sheet Integer or string. Name or index to sheet within a spreadsheet/workbook
#' @param label Character. An optional human-friendly label for the project's environmental data
#' @param outFolder Character. Path to user's experiment folder
#' @param trace Logical. Produce helpful diagnostic messages? Default is FALSE, therefore radio silence is maintained until told otherwise
#' 
#' @return Returns an updated copy of the cm_experiment object passed in parameter \emph{thisExperiment} with side-effect of saving the updated object to the user's experiment folder
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
                               covar_filename = "",
                               siteCol = 1,
                               sheet = 1,
                               label = "",
                               outFolder = "~/cmGDM/",
                               trace = FALSE)
{
  # Gather details about the user's selection of covariate rasters and update cm_experiment object passed in thisExperiment
  
  if (!("cm_experiment" %in% class(thisExperiment)))
    stop("Object passed in 'thisExperiment' must be of class 'cm_experiment'")
  
  # Make/check path for output for this experiment
  outFolder <- paste0(path.expand(outFolder), thisExperiment$experimentName)
  if (!dir.exists(outFolder)) dir.create(outFolder, recursive = TRUE)
  
  # Sanity checks...
  if (!thisExperiment$status["siteData_OK"]) stop("Please added site/sample location information to this experiment before adding covariate data")
  
  if (!thisExperiment$status["biologicalData_OK"]) stop("Please added biological/community information to this experiment before adding covariate data")
  
  if (covar_filename != "") # Env covar data is in a pre-assembled file
  {
    if (!file.exists(covar_filename)) stop("File provided in 'fileName' cannot be found")
    
    fileType <- tools::file_ext(covar_filename)
    
    if (!(fileType %in% c("csv", "txt", "xls", "xlsx", "ods")))
    {
      stop("No value was given for 'fileType';  valid values are 'csv', 'txt', 'xls', 'xlsx', 'ods'")
    }
    else
    {
      if (fileType %in% c("csv", "txt"))
      {
        # Some form of text file. First, attempt to identify the delimiter...
        #thisDelim <- reader::get.delim(siteFilename)
        # reader::get_delim() sometimes fails spectacularly and at the most inopportune time
        # So, here is a simple but more reliable method:
        stuff <- readLines(covar_filename, n = 10)
        
        delimFound <- FALSE
        thisDelim <- NULL
        delimSet <- c(" ", ",", ";", "\t")
        
        for (testDelim in delimSet)
        {
          split_size <- unlist(lapply(strsplit(stuff, testDelim, fixed = TRUE), function(el){length(el)}))
          delimFound <- all(split_size == split_size[1]) & (split_size[1] != 1)
          
          if (delimFound) thisDelim <- testDelim
        }
        
        if (is.null(thisDelim)) stop(paste("fileType =", fileType, "but a sensible delimiter could not be found"))
        
        if (trace) cat("csv or txt: Delim found was '", thisDelim, "'\n", sep = "")
        
        # if (thisDelim %in% c(" ", ",", "\t"))
        # {
        if (trace) cat("csv or txt: Calling base::read.table\n")
        tryCatch({siteEnvData <- read.table(covar_filename, header = TRUE, sep = thisDelim, stringsAsFactors = FALSE)})
        if (trace)
        {
          cat("Dump after loading csv or txt file:\n")
          print(siteEnvData)
          
        }
        # }
      }
      else
      {
        # Some kind of spreadsheet is to be loaded...
        if (fileType == "ods")
        {
          if (trace) cat("ods: Calling readODS::read_ods\n")
          tryCatch({siteEnvData <- readODS::read_ods(covar_filename, sheet = sheet, na = c("NA", ""))})
          if (trace)
          {
            cat("Dump after loading ods file:\n")
            print(siteEnvData)
          }
        }
        else
        {
          if (fileType %in% c("xls", "xlsx"))
          {
            if (trace) cat("lxs or xlsx: calling gdata::read.xls\n")
            tryCatch({siteEnvData <- gdata::read.xls(covar_filename, sheet = sheet)})
            if (trace)
            {
              cat("Dump after loading xls or xlsx file:\n")
              print(siteEnvData)
            }
          }
        }
      }
      
      # Order rows by site, set rownames and delete site col
      ii <- order(siteEnvData[, siteCol])
      siteEnvData <- siteEnvData[ii, ]
      rownames(siteEnvData) <- gsub(" ", ".", trimws(as.character(siteEnvData[, siteCol])), fixed = TRUE)
      
      if (is.numeric(siteCol))
        siteEnvData <- siteEnvData[, -siteCol]
      else
        siteEnvData <- siteEnvData[, -which(colnames(siteEnvData) ==  siteCol)]
      
      covarStack <- NULL
      theseCovarNames <- colnames(siteEnvData)
      
      if(trace)
      {
        cat("Covariate names after load from table:\n")
        print(theseCovarNames)
      }
    }
  }
  else # Env data is to be extracted from a stack of rasters
  {
    if (!dir.exists(src_folder)) stop("Folder given in 'src_folder' cannot be found")
    
    covar_file_paths <- paste0(src_folder, "/", covar_filenames)
    if (!all(file.exists(covar_file_paths)))
      stop(paste0("The following covariate file(s) could not be found: ",
                  paste(covar_file_paths[which(!file.exists(covar_file_paths))], collapse = ", ")))
    
    # OK to proceed; first, extract covaraite names, sort them and use this order
    # to re-order filesnames so that the extraction into a stack and data.table
    # will align. In addition, this ordering will be used to align prediction data
    # when cm_load_pred_covars() is called.
    theseCovarNames <- gsub(pattern = "(.*)\\..*$", replacement = "\\1", covar_filenames)
    ii <- order(theseCovarNames)
    theseCovarNames <- theseCovarNames[ii]
    covar_filenames <- covar_filenames[ii]
    
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
    
    # Add rownames; these will be used to ensure data alignment. We can assume that the order of site names
    # has been reconciled between site data and biological data when cm_load_community_data was run:
    rownames(siteEnvData) <- thisExperiment$data$siteData$dataTable[, thisExperiment$data$siteData$siteCol]
  }
  
  if (trace)
  {
    cat("Dump of siteEnvData after load:\n=========================\n")
    print(siteEnvData)
  }
  
  ############# How to deal with NAs in extracted data? GDM cannot be run on
  ############# such data, so must raise "exception" and stop I suppose
  siteNA <- which(is.na(rowSums(siteEnvData)))
  
  if (length(siteNA) > 0)
    stop(paste("Some sites have missing environmental covariate data; they are:",
               paste(thisExperiment$data$siteData$dataTable[siteNA, thisExperiment$data$siteData$siteCol], collapse = ", ")))
  
  siteEnvRange <- Rfast::colMinsMaxs(as.matrix(siteEnvData))
  colnames(siteEnvRange) <- theseCovarNames
  
  if (trace)
  {
    cat("Dump of siteEnvRange after site range calcs:\n==========================\n")
    print(siteEnvRange)
  }
  
  if (!is.null(covarStack))
    extentEnvRange <- Rfast::colMinsMaxs(as.matrix(terra::extract(covarStack, terra::ext(covarStack))))
  else
  {
    extentEnvRange <- matrix(NA, 2, length(theseCovarNames))
    rownames(extentEnvRange) <- c("min", "max")
    colnames(extentEnvRange) <- theseCovarNames
  }
  
  if (trace)
  {
    cat("Dump of extentEnvRange:\n=========================\n")
    print(extentEnvRange)
  }
  
  thisExperiment$dateDataUpdated <- as.character(Sys.Date())
  thisExperiment$status["covarData_OK"] <- TRUE
  thisExperiment$data$covarData$srcFolder <- src_folder
  thisExperiment$data$covarData$filenames <- covar_filenames
  thisExperiment$data$covarData$covarNames <- theseCovarNames #gsub(pattern = "(.*)\\..*$", replacement = "\\1", covar_filenames)
  thisExperiment$data$covarData$label <- label
  thisExperiment$data$covarData$covarSiteMin <- siteEnvRange["min", ]
  thisExperiment$data$covarData$covarSiteMax <- siteEnvRange["max", ]
  thisExperiment$data$covarData$covarExtentMin <- extentEnvRange["min", ]
  thisExperiment$data$covarData$covarExtentMax <- extentEnvRange["max", ]
  thisExperiment$data$covarData$dataTable <- siteEnvData
  
  if (trace)
  {
    cat("'thisExperiment' object updated and ready to return\n")
  }
  
  saveRDS(thisExperiment, paste0(outFolder, "/cmGDM_", thisExperiment$experimentName, ".rds"))
  
  return(thisExperiment)
}


#' Load a site table and perform checks
#' 
#' @param thisExperiment cm_experiment. Object to which the site data will be added when checks are passed
#' @param siteFilename Character. Full path to the site file to be added to the cm_experiment object
#' @param siteCol Character or integer. Name or numeric index to the column holding the site names or identifiers
#' @param sheet Character or integer. Name or numeric index to the sheet holding the site information when the file is a spreadsheet/workbook
#' @param longitudeCol Character or integer. Name or numeric index to the column holding longitude values
#' @param latitudeCol Character or integer. Name or numeric index to the column holding latitude values
#' @param outFolder String. Path of the output folder
#' @param trace Logical. Produce helpful diagnostic messages? Default is FALSE, therefore radio silence is maintained until told otherwise
#'
#' @return cm_experiment object with updated fields and stores a copy in the user's project folder for this experiment
#' @export
#'
#' @examples
#' \dontrun{
#' # Create a cm_experiment object:
#' birdTurnover1 <- cm_create_new_experiment("user456", "Bird turnover GDM")
#' 
#' # CSV-delimited text file:
#' birdTurnover1 <- cm_load_site_table(birdTurnover1,
#'                                     "/path/to/siteData.csv",
#'                                     siteCol = 1,
#'                                     longitudeCol = 2,
#'                                     latitudeCol = 3)
#'                                     
#' # Excel spreadsheet (sheet must be given as sheet number or sheet name):
#' birdTurnover1 <- cm_load_site_table(birdTurnover1,
#'                                     "/path/to/siteData.xlsx",
#'                                     sheet = "Sites",
#'                                     siteCol = 1,
#'                                     longitudeCol = 2,
#'                                     latitudeCol = 3)
#' }
cm_load_site_table <- function(thisExperiment = NULL,
                               siteFilename = NULL,
                               sheet = NULL,                               
                               siteCol = NULL,
                               longitudeCol = NULL,
                               latitudeCol = NULL,
                               outFolder = "~/cmGDM/",
                               trace = FALSE)
{
  if (is.null(thisExperiment))
    stop("'thisExperiment' must be given a value. Please call cm_create_new_experiment() first")
  
  if (is.null(siteFilename))
  {
    stop("'siteFilename' parameter must be given a value")
  }
  
  if (!file.exists(siteFilename))
  {
    stop("File referenced in 'siteFilename' cannot be found")
  }
  
  fileType <- tools::file_ext(siteFilename)
  
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
      stuff <- readLines(siteFilename, n = 10)
      
      #delimFound <- FALSE
      thisDelim <- NULL
      delimSet <- c(" ", ",", ";", "\t")
      delimCount <- 0
      
      for (testDelim in delimSet)
      {
        split_size <- unlist(lapply(strsplit(stuff, testDelim, fixed = TRUE), function(el){length(el)}))
        # delimFound <- all(split_size == split_size[1]) & (split_size[1] != 1)
        # 
        # if (delimFound) thisDelim <- testDelim
        
        if (sum(split_size) > delimCount)
        {
          delimCount <- sum(split_size)
          thisDelim <- testDelim
        }
      }
      
      if (is.null(thisDelim)) stop(paste("fileType =", fileType, "but a sensible delimiter could not be found"))
      
      if (trace) cat("csv or txt: Delim found was '", thisDelim, "'\n", sep = "")
      
      # if (thisDelim %in% c(" ", ",", "\t"))
      # {
        if (trace) cat("csv or txt: Calling base::read.table\n")
        tryCatch({siteData <- read.table(siteFilename, header = TRUE, sep = thisDelim, stringsAsFactors = FALSE)})
        if (trace) print(siteData)
      # }
    }
    else
    {
      # Some kind of spreadsheet is to be loaded...
      if (fileType == "ods")
      {
        if (trace) cat("ods: Calling readODS::read_ods\n")
        # If no value (character or integer) was supplied in the function call, then assume sheet == 1
        if (is.null(sheet)) sheet <- 1
        
        tryCatch({siteData <- readODS::read_ods(siteFilename, sheet = sheet, na = c("NA", ""))})
        if (trace) print(siteData)
      }
      else
      {
        if (fileType %in% c("xls", "xlsx"))
        {
          if (trace) cat("xls or xlsx: calling gdata::read.xls\n")
          # If no value (character or integer) was supplied in the function call, then assume sheet == 1
          if (is.null(sheet)) sheet <- 1
          
          tryCatch({siteData <- gdata::read.xls(siteFilename, sheet = sheet)})
          if (trace) print(siteData)
        }
      }
    }
  }
  
  # Test for a valid value passed in parameter 'siteCol'
  if (is.null(siteCol))
  {
    stop("siteCol was not given a value")
  }
  else
  {
    testResult <- cm_colExists(siteCol, colnames(siteData), "Site/sample name column")
    if (!testResult$isOK) stop(testResult$reason)
  }
  
  # Test for sensible values in the siteCol column of siteData
  # No missing values
  if ((any(is.na(siteData[, siteCol]))) | (any(siteData[, siteCol] == "")))
  {
    stop("Values found in site/sample column of site data file contain one or more missing values; site labels cannot have missing values")
  }
  
  # No duplicates
  if (any(duplicated(siteData[, siteCol])))
  {
    stop("Values found in siteCol column of site data file contain one or more duplicated values; site labels must be unique")
  }
  
  # Ok, we got this far, now coerce site labels to type character and replace
  # spaces with periods for consistent cross-checks with column names in bioData
  # tables
  siteData[, siteCol] <- gsub(" ", ".", as.character(siteData[, siteCol]), fixed = TRUE)
  
  #print("longitude checks")
  
  # We now move on to coordinate checks
  if (is.null(longitudeCol) | is.null(latitudeCol))
  {
    stop("longitudeCol and/or latitudeCol have not been given a value/values")
  }
  
  # Are the numeric values OK?
  # Longitude checks...
  testResult <- cm_colExists(longitudeCol, colnames(siteData), "Longitude column")
  
  if (!testResult$isOK)
    stop(testResult$reason)
  else
  {
    siteData[, longitudeCol] <- as.numeric(siteData[, longitudeCol])
    
    if (any(is.na(siteData[, longitudeCol])))
    {
      stop("One of more values in longitude column of data file are missing or not numeric; all sites must have valid coordinates")
    }
    
    if (any(siteData[, longitudeCol] == 0))
    {
      stop("One of more values in longitude column of data file are zero; all sites must have valid coordinates")
    }
    
    if (any(siteData[, longitudeCol] < 0))
    {
      stop("One of more values in longitude column of data file are less than zero; all sites must have positive longitudes in Australia")
    }
  }
  
  #print("latitude checks")
  # Latitude checks...
  testResult <- cm_colExists(latitudeCol, colnames(siteData), "Latitude column")
  
  if (!testResult$isOK)
    stop(testResult$reason)
  else
  {
    siteData[, latitudeCol] <- as.numeric(siteData[, latitudeCol])
    
    if (any(is.na(siteData[, latitudeCol])))
    {
      stop("One of more values in latitude column of data file are missing or not numeric; all sites must have valid coordinates")
    }
    
    if (any(siteData[, latitudeCol] == 0))
    {
      stop("One of more values in latitude column of data file are zero; all sites must have valid coordinates")
    }
    
    if (any(siteData[, latitudeCol] > 0))
    {
      stop("One of more values in latitude column of data file are greater than zero; all sites must have negative latitudes in Australia")
    }
  }
  
  # Check for coords in the geographical extent of covariate rasters
  
  # Make/check path for output for this experiment
  outFolder <- paste0(path.expand(outFolder), thisExperiment$experimentName)
  if (!dir.exists(outFolder)) dir.create(outFolder, recursive = TRUE)
  
  if (trace) cat("Updating cm_experiment object\n")
  # Lucky user has just won the siteData check lottery!
  
  # To match the way formatsitepair function organises row-order, sort site
  # data. This will be used to order biological data and environmental data when
  # they are loaded:
  siteData <- siteData[order(siteData[, siteCol]), ]
  rownames(siteData) <- siteData[, siteCol]
  
  if (trace) print(siteData)
  
  thisExperiment$status["siteData_OK"] <- TRUE
  thisExperiment$dateDataUpdated <- as.character(Sys.Date())
  thisExperiment$data$siteData$srcFile <- siteFilename
  thisExperiment$data$siteData$siteCol <- siteCol
  thisExperiment$data$siteData$longitudeCol <- longitudeCol
  thisExperiment$data$siteData$latitudeCol <- latitudeCol
  thisExperiment$data$siteData$dataTable <- siteData
  
  saveRDS(thisExperiment, paste0(outFolder, "/cmGDM_", thisExperiment$experimentName, ".rds"))
  
  return(thisExperiment)
}

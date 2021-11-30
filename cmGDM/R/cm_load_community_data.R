

#' Load community data into a cm_experiment object
#' 
#' The data table loaded by this function represents to response or dependent variable to be modelled as a function of one or more ecological covariates.
#' 
#' Data may be in the form of a presence-absence table, abundance data, or a pre-computed dissimilarity matrix.
#' 
#' Terminology used in the literature is variable: these data may be called the 'biological' data, 'ecological' data, or 'community' data to be modelled.
#' 
#' @param thisExperiment cm_experiment. Object to which community be added
#' @param bioFilename Character. Full path to a file containing an accepted form of biological or community data
#' @param sheet Character or integer. Name or index number of a sheet if \emph{bioFilename} points to a file containing a spreadsheet/workbook
#' @param dataType Character. One of the accepted data types in entries in the file named in \emph{bioFilename}
#' @param presenceMarker Character. Denotes "presence" when in the file named in \emph{bioFilename} has \emph{dataType} of "Presence" or "Presence-Absence"
#' @param absenceMarker Character. Denotes "absence" when in the file named in \emph{bioFilename} has \emph{dataType} of "Presence-Absence"
#' @param siteCol Character or integer. Name or numeric index to the column holding the site names or identifiers
#' @param dissimMeasure Character. The type of dissimilarity measure stored in the file named in \emph{bioFilename} when \emph{dataType} is "Dissimilarity"
#' @param trace Logical. Produce helpful diagnostic messages? Default is FALSE, therefore radio silence is maintained until told otherwise
#'
#' @return cm_experiment object with updated fields
#' @export
#'
#' @examples
#' \dontrun{
#' 
#' }
cm_load_community_data <- function(thisExperiment = NULL,
                                   bioFilename = NULL,
                                   sheet = 1,
                                   dataType = "", #c("Presence_absence", "Abundance", "Dissimilarity"),
                                   presenceMarker = "1",
                                   absenceMarker = "0",
                                   siteCol = NULL,
                                   dissimMeasure = "", #c("Bray-Curtis", "Jaccard", "Fst"),
                                   trace = FALSE)
{
  small_Fst <- -0.02
  
  if (is.null(thisExperiment))
    stop("'thisExperiment' must be given a value. Please call cm_create_new_experiment() first")
  
  if (!thisExperiment$status["siteData_OK"])
    stop("'thisExperience' must have valid stie data stroed in it. Please run 'cm_load_site_table() first'")
  
  if (is.null(bioFilename))
    stop("'bioFilename' has not been given a value")
  
  if (!file.exists(bioFilename))
    stop("Cannot find file referred to in 'bioFilename'")
  
  if (!(dataType %in% c("Presence_absence", "Abundance", "Dissimilarity")))
    stop("'dataType' must be one of 'Presence_absence', 'Abundance', or 'Dissimilarity'")
  
  fileType <- tools::file_ext(bioFilename)
  
  if (!(fileType %in% c("csv", "txt", "xls", "xlsx", "ods"))) stop("File format must be one of 'csv', 'txt', 'xls', 'xlsx', or 'ods'")
  
  if ((dataType == "Dissimilarity") & !is.null(siteCol))
  {
    stop("'dataType' is set to 'Dissimilarity' but 'siteCol' is not NULL; is the dataType really 'Dissimilarity'?")
  }
  
  if (!is(thisExperiment, "cm_experiment")) stop("thisExperiment must be class 'cm_experiment'; Please run cm_create_new_experiment()") 
  
  if (is.null(siteCol) & !(dataType == "Dissimilarity")) stop("siteCol must be given a value: either the name or column number of site labels")
  
  if ((dataType == "Dissimilarity") & is.null(dissimMeasure)) stop("Because dataType = 'Dissimilarity', 'dissimMeasure' cannot be NULL")
  
  if (!(dissimMeasure %in% c("Bray-Curtis", "Jaccard", "Fst"))) stop("'dissimMeasure' must be one of the following: 'Bray-Curtis', 'Jaccard', 'Fst'")
  
  if ((dataType %in% c('Presence_absence', 'Abundance')) & (dissimMeasure == "Fst")) stop(paste("'dissimMeasure' cannot be 'Fst' when dataType =", dataType))
  
  if (fileType %in% c("csv", "txt"))
  {
    # Some form of text file. First, attempt to identify the delimiter...
    #thisDelim <- reader::get.delim(bioFilename)
    
    # reader::get_delim() sometimes fails spectacularly and at the most inopportune time
    # So, here is a simple but more reliable method:
    stuff <- readLines(bioFilename, n = 10)
    
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
    
    tryCatch({bioData <- read.table(bioFilename, header = TRUE, sep = thisDelim, stringsAsFactors = FALSE)})
  }
  else
  {
    # Some kind of spreadsheet is to be loaded...
    if (fileType == "ods")
    {
      tryCatch({bioData <- readODS::read_ods(bioFilename, sheet = sheet, na = c("NA", ""))})
    }
    else
    {
      if (fileType %in% c("xls", "xlsx"))
      {
        tryCatch({bioData <- gdata::read.xls(bioFilename, sheet = sheet, stringsAsFactors = FALSE)})
      }
    }
  }
  
  # Test that number of sites specified in siteData is same as number in bioData
  if (nrow(bioData) < nrow(thisExperiment$data$siteData$dataTable))
    stop("Fewer sites in biological data table than in site data table: they must be the same")
  
  if (nrow(bioData) > nrow(thisExperiment$data$siteData$dataTable))
    stop("More sites in biological data table than in site data table: they must be the same")
  
  ################ P/A or Abundance
  if (dataType %in% c("Presence_absence", "Abundance"))
  {
    
    # Test for a valid value passed in parameter 'siteCol' if the data is NOT dissimilarity
    if (is.null(siteCol))
    {
      stop("'siteCol' must be given a value for Presence-absence or Abundance data")
    }
    else
    {
      #print(colnames(bioData))
      testResult <- cm_colExists(siteCol, colnames(bioData))
      if (!testResult$isOK) stop(testResult$reason)
    }
    
    # Test for sensible values in the siteCol column of biologicalData
    # 1. No missing values
    if ((any(is.na(bioData[, siteCol]))) | (any(bioData[, siteCol] == "")))
    {
      stop("Values found in siteCol column of site data file contain one or more missing values; site labels cannot have missing values")
    }
    
    # 2. No duplicates
    if (any(duplicated(bioData[, siteCol])))
    {
      stop("Values found in siteCol column of biological data file contain one or more duplicated values; site labels must be unique")
    }
    
    # Ok, we got this far, now coerce site labels to type character
    #bioData[, siteCol] <- as.character(bioData[, siteCol])
    
    # 3. Do the site labels in bioData match those given in previously loaded site data?
    siteData_siteCol <- thisExperiment$data$siteData$siteCol
    
    if (!all(bioData[, siteCol] %in% thisExperiment$data$siteData$dataTable[, siteData_siteCol]))
      stop("Site labels in biological data file do not match site labels loaded from site data file")
    
    # Perform quality checks on bioData
    # if (dataType %in% c("Presence_absence", "Abundance"))
    # {
    #   if (any(bioData < 0))
    #     stop(paste("Negative values found in biological data table. For 'dataType' =", dataType,"all values must be >= 0"))
    #   
    #   else
    #     bioData <- as.matrix(as.numeric(bioData))
    # }
    
    # All good so far, so now give bioData rownames using the specified column, then trim the data.frame by removing siteCol
    rownames(bioData) <- gsub(" ", ".", as.character(bioData[, siteCol]), fixed = TRUE)
    if (is.numeric(siteCol))
      bioData <- bioData[, -siteCol]
    else
      bioData <- bioData[, -which(colnames(bioData) ==  siteCol)]
    
    #if (trace) print(bioData)
    
    # Missing values are BAD
    if (any(is.na(bioData)))
      stop("Missing values found in biological data table: cannot proceed with missing values present")
    
    if (dataType == "Presence_absence")
    {
      bioData <- matrix(as.character(as.matrix(bioData)), nrow(bioData), ncol(bioData),
                        dimnames = list(rownames(bioData), colnames(bioData)))
      
      if (trace)
      {
        cat("\nRe-cast as character:\n")
        print(bioData)
      }
      
      numCodes <- length(unique(as.vector(bioData)))
      if (numCodes > 2)
        stop("'bioData' could not be uniquely coded with presence-absence values: there seem to be more values in the table than provided by values given in 'presenceMarker' and 'absenceMarker'")
      
      if (numCodes < 2)
        stop("'bioData' could not be uniquely coded with presence-absence values: there seem to be fewer values in the table than provided by values given in 'presenceMarker' and 'absenceMarker'")
      
      # bioData should be clean; Recode bioData to "0" for absence and "1" for presence
      if (presenceMarker != "1")
        bioData[which(bioData == presenceMarker, arr.ind = TRUE)] <- "1"
      
      if (absenceMarker != "0")
        bioData[which(bioData == absenceMarker, arr.ind = TRUE)] <- "0"
      
      if (trace)
      {
        cat("\nCoding reassigned:\n")
        print(bioData)
      }
      
      # Convert bioData to a numeric table and test for 0, 1-only coding
      bioData <- matrix(as.numeric(bioData), nrow(bioData), ncol(bioData),
                        dimnames = list(rownames(bioData), colnames(bioData)))
      
      if (trace)
      {
        cat("\nRe-cast as numeric:\n")
        print(bioData)
      }
      
      # Trap coding unexpected errors when 
      if (any(is.na(bioData)))
        stop("'bioData' could not be uniquely coded with presence-absence values: please check the data table and try again")
      
      # We should able to compute a dissimilarity matrix from recoded P/A table using the specified
      # dissimilarity measure
      if (dissimMeasure == "Bray-Curtis")
        dissimMat <- as.matrix(vegan::vegdist(bioData))
      
      if (dissimMeasure == "Jaccard")
        dissimMat <- as.matrix(vegan::vegdist(bioData, "jaccard"))
    }
    
    if (dataType == "Abundance")
    {
      if (trace)
      {
        cat("\nAbundance: step 1:\n")
        print(class(bioData))
        print(bioData)
      }
      
      # Stuff will happen...
      # All must be numeric
      # All good so far, so now give bioData rownames using the specified column, then trim the data.frame by removing siteCol
      # rownames(bioData) <- as.character(bioData[, siteCol])
      # if (is.numeric(siteCol))
      #   bioData <- bioData[, -siteCol]
      # else
      #   bioData <- bioData[, -which(colnames(bioData) ==  siteCol)]
      
      # Convert bioData to a numeric matrix
      # bioData <- matrix(bioData, nrow(bioData), ncol(bioData),
      #                   dimnames = list(rownames(bioData), colnames(bioData)))
      bioData <- as.matrix(bioData)
      
      if (trace)
      {
        cat("\nAbundance: step 2:\n")
        print(class(bioData))
        print(bioData)
      }
      
      if (!all(is.numeric(bioData)))
        stop(paste("'bioData' contains non-numeric values; cannot be so for 'dataType' =", dataType))
      
      if (any(bioData < 0))
        stop(paste("Negative values found in biological data table. For 'dataType' =", dataType,"all values must be >= 0"))
      
      if (dissimMeasure == "Bray-Curtis")
        dissimMat <- as.matrix(vegan::vegdist(bioData))
      
    }
    
    
  }
  else  # Dissimilarity
  {
    if (trace)
    {
      cat("\nDissimilarity step 1:\n")
      print(bioData)
    }
    
    # Give bioData rownames ASSUMING column 1 holds site names, then trim the data.frame by removing siteCol
    rownames(bioData) <- gsub(" ", ".", as.character(bioData[, 1]), fixed = TRUE)
    bioData <- bioData[, -1]
    
    # Re-cast as matrix
    bioData <- as.matrix(bioData)
    
    if (trace)
    {
      cat("\nDissimilarity step 2:\n")
      print(bioData)
      print(class(bioData))
      print(is.numeric(bioData))
      cat("\nbioData rownames:\n")
      print(rownames(bioData))
      cat("\nbioData colnames:\n")
      print(colnames(bioData))
      
      
    }
    
    
    if (!all(is.numeric(bioData)))
      stop(paste("'bioData' contains non-numeric values; cannot be so for 'dataType' =", dataType))
    
    # Is it square?
    if (!(nrow(bioData) ==  ncol(bioData)))
      stop("For 'dataType' = 'Dissimilarity', number of rows in biological data table must be the same as the number of columns")
    
    # Is matrix symmetric?
    if ((dissimMeasure == "") & !isSymmetric(bioData))
      stop(paste("'dataType' = 'Dissimilarity' and 'dissimMeasure' =", dissimMeasure, "but the biological data matrix is not symmetrical"))
    
    # Row and column labels are same?
    if (!all(rownames(bioData) == colnames(bioData)))
      stop(paste("'dataType' = 'Dissimilarity' but row and column names in 'bioData' are not the same"))
    
    # Row names in bioData are all present in siteTable (which MUST have been loaded before attempting to load bioData)
    if (!all(rownames(bioData) %in% thisExperiment$data$siteData$dataTable[, thisExperiment$data$siteData$siteCol]))
      stop(paste("Row names in bioData do not match site labels in siteData"))
    
    # if (!all(is.numeric(bioData)))
    #   stop("'bioData' contains non-numeric values; cannot be so for 'dataType' = 'Abundance' or 'Dissimilarity'")
    
    # Test for negative values
    negCells <- which(bioData < 0, arr.ind = TRUE)
    
    if (dissimMeasure == "Fst")
    {
      # Adjust tiny negative values which may occasionally appear in Fst data
      if (nrow(negCells) > 0)
      {
        # Are any of the negative values non-trivial?
        if (any(bioData[negCells]) > small_Fst)
          stop("'bioData' with 'dissimMeasure' = 'Fst' has large negative values")
        else
        {
          # Set trivially negative values to 0
          cat("'bioData' with 'dissimMeasure' = 'Fst' has negative values less than small_Fst; these values have been set to 0")
          bioData[negCells] <- 0
        }
      }
    }
    else
    {
      if (trace)
      {
        cat("\nDissimilarity != Fst: step 1:\n")
        print(bioData)
      }
      
      
      # Non-Fst dissimilarity with negative values is strictly forbidden...
      if (nrow(negCells) > 0)
        stop(paste0("'bioData' with 'dissimMeasure' = '", dissimMeasure, "' has negative values: cannot proceed"))
      
      # Is matrix symmetric?
      if (!isSymmetric(bioData))
        stop(paste("'dataType' = 'Dissimilarity' and 'dissimMeasure' =", dissimMeasure, "but the biological data matrix is not symmetrical"))
      
      # Row and column labels are same?
      if (!all(rownames(bioData) == colnames(bioData)))
        stop(paste("'dataType' = 'Dissimilarity' but row and column names in 'bioData' are not the same"))
      
      # Row names in bioData are all present in siteTable (which MUST have been loaded before attempting to load bioData)
      if (!all(rownames(bioData) %in% thisExperiment$data$siteData$dataTable[, thisExperiment$data$siteData$siteCol]))
        stop(paste("Row names in bioData do not match site labels in siteData"))
    }
    
    # Passed all tests for a dissimilarity matrix
    dissimMat <- bioData
  }
  
  # Update and save revised cm_experiment object
  thisExperiment$dateDataUpdated <- as.character(Sys.Date())
  thisExperiment$status["biologicalData_OK"] <- TRUE
  thisExperiment$data$biologicalData$srcFile <- bioFilename
  thisExperiment$data$biologicalData$fileType <- fileType
  thisExperiment$data$biologicalData$sheet <- sheet
  thisExperiment$data$biologicalData$siteCol <- siteCol
  thisExperiment$data$biologicalData$dataType <- dataType
  thisExperiment$data$biologicalData$presenceMarker <- presenceMarker
  thisExperiment$data$biologicalData$absenceMarker <- absenceMarker
  thisExperiment$data$biologicalData$dissimMeasure <- dissimMeasure
  thisExperiment$data$biologicalData$dataTable <- bioData
  thisExperiment$data$biologicalData$dissimMatrix <- dissimMat
  
  return(thisExperiment)
}

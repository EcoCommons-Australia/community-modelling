
#' Utility function to check if column exists
#'
#' A simple function to check for the existence of a specified name in a vector of names, returning the results in a standardised structure which includes error messages corresponding to regular error conditions
#'
#' @param colLabel Character string or integer. Name or index number of the column (or variable, covariate, etc) whose existence is being checked
#' @param colNames Character vector. The column names or variable set to be used to check the existence of the value passed in colLabel
#' @param UIname Character. Name of variable to be tested
#'
#' @return An named list:
#' \describe{
#'  \item{isOK}{Logical flag; TRUE when colLabel is found; FALSE otherwise}
#'  \item{msg}{Character string with an error message; "" if isOK == TRUE}
#' } 
#' @export
#'
cm_colExists <- function(colLabel, colNames, UIname)
{
  isOK <- TRUE
  msg = ""
  #paramName <- deparse(substitute(colLabel))
  
  if (is.numeric(colLabel))
  {
    colLabel <- as.integer(colLabel)
    
    if (colLabel <= 0)
    {
      isOK <- FALSE
      msg <- paste(UIname, "given as column number but value is less than 1; column numbers start at 1")
    }
    
    if (colLabel > length(colNames))
    {
      isOK <- FALSE
      msg <- paste(UIname, "given as column number but value is greater than the number of columns in the data file")
    }
  }
  else
  {
    if (!colLabel %in% colNames)
    {
      isOK <- FALSE
      msg <- paste(UIname, "given as a column name, name was not found in column names of the data file")
    }
  }
  
  return(list(isOK = isOK, reason = msg))
}

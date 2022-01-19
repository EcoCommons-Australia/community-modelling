

#' GDM report
#'
#' @param thisExperiment cmGDM object
#' @param outFile Character. Name of the file (including a fully specified path) to which outpout will be written.
#' @param format Character. Report format to be used. Default is "pdf". Other options are "word" and "html".
#'
#' @return Nothing
#' @export
#'
#' @examples
#' \dontrun{
#' 
#' cm_gdm_report(myExperiment)
#' 
#' }
cm_gdm_report <- function(thisExperiment,
                          outFile = "",
                          format = "pdf")
  
{
  if (!("cm_experiment" %in% class(thisExperiment)))
    stop("Object passed in 'thisExperiment' must be of class 'cm_experiment'")
  
  
  ######### ????????????? Make a report even if no experiment has been run ??????????????????
  if (!thisExperiment$status["modelFit_OK"])
    stop(paste("No GDM model has been successfully fitted for experiment", thisExperiment$experimentName))
  
  if (outFile == "")
    stop("Please provide a file name in parameter 'outFile'")
  
  if (!(tolower(format) %in% c("word", "pdf", "html")))
    stop("'format' must be one of 'word', 'pdf' or 'html'")
  
  
  
  
  
}

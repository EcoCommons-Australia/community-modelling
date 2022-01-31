

#' GDM report
#' 
#' A pdf file is produced documenting the outcomes of the model fitted using the cm_experiment passed in \emph{thisExperiment}.
#' 
#' Note that all graphical elements included in the report are also separately stored in the user's project folder for the experiment, and are therefore \emph{potentially} downloadable so that the user may incorporate them in other documents such as reports or theses.
#'
#' @param thisExperiment cm_experiment. A cm_experiment object to be used to generate the report
#' @param outFolder String. Base path to which output will be written.
#' @param format String. Report format to be used. Default is "pdf" which is the only formatted implement at present. Other options will include "word" and "html".
#' @param trace Logical. Produce hopefully diagnostic messages. Default is to remain silent.
#'
#' @return Nothing but with a side-effect of saving the report pdf to the project folder
#' @export
#'
#' @examples
#' \dontrun{
#' 
#' cm_gdm_report(myExperiment)
#' 
#' }
cm_gdm_report <- function(thisExperiment,
                          outFolder = "~/cmGDM/",
                          format = "pdf",
                          trace = FALSE)
  
{
  if (!("cm_experiment" %in% class(thisExperiment)))
    stop("Object passed in 'thisExperiment' must be of class 'cm_experiment'")
  
  ######### ????????????? Make a report even if no experiment has been run ??????????????????
  if (!thisExperiment$status["modelFit_OK"])
    stop(paste("No GDM model has been successfully fitted for experiment", thisExperiment$experimentName))
  
  # Make/check path for output for this experiment
  outFolder <- paste0(path.expand(outFolder), thisExperiment$experimentName)
  if (!dir.exists(outFolder)) dir.create(outFolder, recursive = TRUE)

  if (!(tolower(format) %in% c("word", "pdf", "html")))
    stop("'format' must be one of 'word', 'pdf' or 'html'")
  
  # Check for presence of performance plots and create them if they are not found
  obsPredFile <- paste0(outFolder, "/cmGDM_", thisExperiment$experimentName , "_GDM_observed_predicted_dissimilarity.png")
  ecolDistFile <- paste0(outFolder, "/cmGDM_", thisExperiment$experimentName ,"_GDM_ecological_distance.png")
  
  if (!(file.exists(obsPredFile) & file.exists(ecolDistFile)))
  {
    if (trace) cat("cm_gdm_report: GDM performance plots are being generated\n")
    cmGDM::cm_performance_plots(thisExperiment)
  }
  
  if (trace) cat("cm_gdm_report: About to start\n")
  
  rmarkdown::render(system.file("cmGDM_report.Rmd", package = "cmGDM"),
                    params = list(thisExperiment = thisExperiment),
                    output_file = paste0(outFolder, "/cmGDM_report_", thisExperiment$experimentName,"_", as.character(Sys.Date()), ".pdf"))
  
  if (trace) cat("cm_gdm_report: End of report generation\n")
}

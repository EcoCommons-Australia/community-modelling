

#' GDM summary
#'
#' Generate a summary report on a fitted GDM. Output is always written to the console and, optionally, also written to a text file.
#'
#' @param thisExperiment cm_experiment object. Experiment object storing a successfully fitted GDM
#' @param outFile Character. File name (with full path) into which the summary will be written. Default of "" means no file will be written
#'
#' @return A character object of formatted text with side-effect of a text file if \emph{outfile} is not empty
#' @export
#'
#' @examples
#' \dontrun{
#' # Console-only output
#' cm_gdm_summary(anExperiment)
#' 
#' # Console and file output
#' cm_gdm_summary(anotherExperiment, file = "thisFile.txt")
#' }
#' 
cm_gdm_summary <- function(thisExperiment,
                           outFile = "")
{
  if (!("cm_experiment" %in% class(thisExperiment)))
    stop("Object passed in 'thisExperiment' must be of class 'cm_experiment'")
  
  if (thisExperiment$status["modelFit_OK"])
    #stop(paste("No GDM model has been successfully fitted for experiment", thisExperiment$experimentName))
  {
    if (length(thisExperiment$model$varImp) > 0)
    {
      impScores <- rep(0, length(thisExperiment$model$gdm$predictors))
      names(impScores) <- thisExperiment$model$gdm$predictors
      
      # Insert non-zero importance scores
      impScores[rownames(thisExperiment$model$varImp[[2]])] <- round(thisExperiment$model$varImp[[2]][, 1], 3)
      
      maxLen <- max(unlist(lapply(names(impScores), stringr::str_length)))
      paddedNames <- stringr::str_pad(names(impScores), side = "left", width = maxLen + 4, pad = " ")
      paddedNums <- paste0("   ", format(impScores))
      impTable <- matrix(c(paddedNames, paddedNums), ncol = 2, dimnames = list(NULL, c("Covariate", "Contribution")))
      
      impTable_str <- c("    Covariate       Contrib.", "    ------------    -----------", paste(impTable[, 1], impTable[, 2]))
    }
    else
    {
      impTable_str <- "  Covariate importance permutation test has not been run.\n  To do this re-run the experiment with option 'Compute Var Imp' selected."
    }
    
    summaryLines <- NULL
    
    summaryLines <- c(summaryLines,
                      "-----------------------------------------------",
                      "EcoCommons Community Modelling Module",
                      "GDM Model Summary",
                      "-----------------------------------------------",
                      "",
                      paste("Experiment name:", thisExperiment$experimentName),
                      paste("Decription:", thisExperiment$description),
                      paste("Model run date:", thisExperiment$dateLastModelRun),
                      "",
                      paste("Number of site/sample pairs:", thisExperiment$model$gdm$sample),
                      paste("Site weight type:", thisExperiment$data$siteData$weightType),
                      paste("Filter sites with species richness < ", thisExperiment$data$biologicalData$sppFilter),
                      "",
                      "Covariates:",
                      paste("  Geographical distance included:", ifelse(thisExperiment$model$gdm$geo, "Yes", "No")),
                      paste("  Covariates: Number used =", length(thisExperiment$model$gdm$predictors)),
                      paste0("    ", sort(thisExperiment$model$gdm$predictors)),
                      "",
                      "Model performance:",
                      paste0("      Model deviance: ", round(thisExperiment$model$gdm$gdmdeviance, 2)),
                      paste0("  Explained deviance: ", round(thisExperiment$model$gdm$explained, 2), "%"),
                      paste0("       NULL deviance: ", round(thisExperiment$model$gdm$nulldeviance, 2)),
                      paste0("          Interecept: ", round(thisExperiment$model$gdm$intercept, 3)),
                      "",
                      "Covariate importance:",
                      impTable_str
    )
    
    #cat(paste(summaryLines, collapse = "\n"))
  }
  else
    summaryLines <- paste0("No GDM model has been successfully fitted for experiment '", thisExperiment$experimentName, "'")
  
  if (outFile != "")
  {
    writeLines(summaryLines, outFile)
  }
  
  return(cat(paste(summaryLines, collapse = "\n")))
}

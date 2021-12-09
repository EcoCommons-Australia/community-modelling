
#' GDM performance plots
#' 
#' Create three types of performance evaluation plot for a fitted GDM: observed v predicted dissimilarity, ecological distance v observed dissimilarity, and variable contribution plots
#'
#' @param thisExperiment cm_experiment object. An experiment object storing a successfully fitted GDM
#' @param outFolder Character. Full path to a folder into which image files will be written
#' @param showVarImp Character. Control which set of variable contribution plots are created: "all" generates all plots; "nonzero" generates only plots for variables with a variable importance score greater than zero
#'
#' @return
#' @export
#'
#' @examples
#' \dontrun{
#' # Default plots: show ALL variable contribution plots
#' cm_performance_plots(anExperiment,
#'                      "/path/to/my/stuff")
#'         
#' # Show only var. contrib.plots when varaible importance is > 0
#' cm_performance_plots(anExperiment,
#'                      "/path/to/my/stuff",
#'                      "nonzero")
#' }
cm_performance_plots <- function(thisExperiment,
                                 outFolder = "",
                                 showVarImp = "all")
{
  if (!("cm_experiment" %in% class(thisExperiment)))
    stop("Object passed in 'thisExperiment' must be class cm_experiment")
  
  if (!thisExperiment$status["modelFit_OK"])
    stop("Cannot generate performance plots: no successfully fitted GDM found in 'thisExperiment'")
  
  if (!(toupper(showVarImp) %in% c("ALL", "NONZERO")))
    stop("'showVarImp' must be one of 'all' or 'nonzero'")
  
  # Make life easier...
  gdmModel <- thisExperiment$model$gdm
  
  plotyStuff <- gdm::isplineExtract(gdmModel)
  
  # Model performance plots: Plot of Observed Compositional Dissimilarity v
  # Predicted Ecological Distance: How well does the model do at linking a
  # predicted distance between samples/sites to the observed dissimilarity between
  # to site/sample pair?
  png(paste0(outFolder, "/", thisExperiment$experimentName , "_GDM_observed_predicted_dissimilarity.png"))
  compDissimPlot <- ggplot2::ggplot(data.frame(observed = gdmModel$observed,
                                               predicted = gdmModel$predicted),
                                    aes(x = predicted, y = observed)) +
    geom_point(colour = "blue") +
    ylim(c(0, 1)) +
    geom_abline(slope = 1, intercept = 0) +
    xlab("Predicted Compositional Dissimilarity") +
    ylab("Observed Compositional Dissimilarity") #+
  plot(compDissimPlot)
  dev.off()
  
  # Observed v Predicted Compositional Dissimilarity: This is the GDM equivalent
  # of a calibration plot
  
  # Make a data.frame holding expected curve data; the basic design of this
  # code-chunk is borrowed from the gdm package source code.
  expected_x <- seq(min(gdmModel$ecological),
                    max(gdmModel$ecological),
                    length = 200)
  expected_y <- 1 - exp(-expected_x)
  expected_curve <- data.frame(ecological = expected_x,
                               observed = expected_y)

  png(paste0(outFolder, "/", thisExperiment$experimentName ,"_GDM_ecological_distance.png"))
  compDissim_ecoDist <- ggplot2::ggplot(data.frame(ecological = gdmModel$ecological,
                                                   observed = gdmModel$observed),
                                        aes(x = ecological, y = observed)) +
    geom_line(data = expected_curve, colour = "dodgerblue", size = 1) +
    geom_point(colour = "blue") +
    ylim(c(0, 1)) +
    xlab("Predicted Ecological Distance") +
    ylab("Observed Compositional Dissimilarity")
  plot(compDissim_ecoDist)
  dev.off()
  
  # Variable contribution plots
  # Which vars are in the final fit? We will only produce
  # spline plots for them ;)
  varInd <- which(colSums(plotyStuff$y) != 0)
  varNames <- colnames(plotyStuff$y)[varInd]
  varNames[which(varNames == "matrix_1")] <- "floristics"
  
  # Set up a container for the ggplot objects
  plotyBits <- vector("list", length(varNames))
  names(plotyBits) <- varNames
  
  # Trim variables to be plotted?
  if (toupper(showVarImp) == "NONZERO")
  {
    varImpScore <- sort(100*thisExperiment$model$varImp[[2]][, 1]/sum(thisExperiment$model$varImp[[2]][, 1]), decreasing = TRUE)
    varNames <- names(varImpScore[varImpScore > 0])
  }
  
  # Step through the list of variables to be plotted
  for (thisVar in varNames)
  {
    plotData <- data.frame(x = plotyStuff$x[, thisVar],
                           y = plotyStuff$y[, thisVar])
    
    plotyBits[[thisVar]] <- ggplot2::ggplot(plotData, aes(x = x, y = y)) +
      geom_line(colour = "blue", size = 1) +
      ylab(paste0("f(", thisVar, ")")) +
      xlab(thisVar) +
      ylim(c(0,1)) +
      ggtitle(paste0("Contribution: ", round(varImpScore[thisVar], 2), "%")) +
      theme(axis.title.x = element_text(size = 10),
            axis.title.y = element_text(size = 10),
            axis.text.x = element_text(size = 8),
            axis.text.y = element_text(size = 8))
  }
  
  # Save the multipanel plot
  ggpubr::ggarrange(plotlist = plotyBits, ncol = 3, nrow = 4) %>%
    ggpubr::ggexport(filename = paste0(outFolder, "/", thisExperiment$experimentName ,"_GDM_variable_splines_transformed.png"),
                     width = 768, height = 1024)
}


#' GDM performance plots
#' 
#' Create three types of performance evaluation plot for a fitted GDM: observed v predicted dissimilarity, ecological distance v observed dissimilarity, and variable contribution plots
#'
#' @param thisExperiment cm_experiment object. An experiment object storing a successfully fitted GDM
#' @param outFolder Character. Path to user's experiment folder
#' @param showVarImp Character. Control which set of variable contribution plots are created: "all" generates all plots; "nonzero" generates only plots for variables with a variable importance score greater than zero. If variable importance permutation has not been run, this parameter will be ignored
#'
#' @return Nothing
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
                                 outFolder = "~/cmGDM/",
                                 showVarImp = "all")
{
  if (!("cm_experiment" %in% class(thisExperiment)))
    stop("Object passed in 'thisExperiment' must be class cm_experiment")
  
  if (!thisExperiment$status["modelFit_OK"])
    stop("Cannot generate performance plots: no successfully fitted GDM found in 'thisExperiment'")
  
  if (!(toupper(showVarImp) %in% c("ALL", "NONZERO")))
    stop("'showVarImp' must be one of 'all' or 'nonzero'")
  
  # Make/check path for output for this experiment
  outFolder <- paste0(path.expand(outFolder), thisExperiment$experimentName)
  if (!dir.exists(outFolder)) dir.create(outFolder, recursive = TRUE)
  
  # Model performance plots: Plot of Observed Compositional Dissimilarity v
  # Predicted Ecological Distance: How well does the model do at linking a
  # predicted distance between samples/sites to the observed dissimilarity between
  # to site/sample pair?
  plotData <- data.frame(observed = thisExperiment$model$gdm$observed,
                         predicted = thisExperiment$model$gdm$predicted)
  
  grDevices::png(paste0(outFolder, "/cmGDM_", thisExperiment$experimentName , "_GDM_observed_predicted_dissimilarity.png"), width = 12, height = 12, units = "cm", res = 150)
  compDissimPlot <- ggplot2::ggplot(plotData,
                                    ggplot2::aes(x = .data$predicted, y = .data$observed)) +
    ggplot2::ylim(c(0, 1)) +
    ggplot2::xlim(c(0, 1)) +
    ggplot2::geom_abline(slope = 1, intercept = 0, colour = "dodgerblue", size = 1) +
    ggplot2::geom_point(colour = "blue") +
    ggplot2::xlab("Predicted Compositional Dissimilarity") +
    ggplot2::ylab("Observed Compositional Dissimilarity") +
    ggplot2::theme(axis.text = ggplot2::element_text(size = 12),
                   axis.title = ggplot2::element_text(size = 14))
  print(compDissimPlot)
  grDevices::dev.off()
  
  # Observed v Predicted Compositional Dissimilarity: This is the GDM equivalent
  # of a calibration plot
  
  # Make a data.frame holding expected curve data; the basic design of this
  # code-chunk is borrowed from the gdm package source code.
  expected_x <- seq(min(thisExperiment$model$gdm$ecological),
                    max(thisExperiment$model$gdm$ecological),
                    length = 200)
  expected_y <- 1 - exp(-expected_x)
  expected_curve <- data.frame(ecological = expected_x,
                               observed = expected_y)
  
  plotData <- data.frame(ecological = thisExperiment$model$gdm$ecological,
                         observed = thisExperiment$model$gdm$observed)
  
  grDevices::png(paste0(outFolder, "/cmGDM_", thisExperiment$experimentName ,"_GDM_ecological_distance.png"), width = 12, height = 12, units = "cm", res = 150)
  compDissim_ecoDist <- ggplot2::ggplot(data = plotData,
                                        ggplot2::aes(x = .data$ecological, y = .data$observed)) +
    ggplot2::geom_line(data = expected_curve, colour = "dodgerblue", size = 1) +
    ggplot2::geom_point(colour = "blue") +
    ggplot2::ylim(c(0, 1)) +
    ggplot2::xlab("Predicted Ecological Distance") +
    ggplot2::ylab("Observed Compositional Dissimilarity")
  plot(compDissim_ecoDist)
  grDevices::dev.off()
  
  # Variable contribution plots
  # Collate spline plotting coordinates
  plotyStuff <- gdm::isplineExtract(thisExperiment$model$gdm)
  
  if (length(thisExperiment$model$varImp) > 0)
  {
    # Extract scores: Latest version of gdm only retains non-zero values of variable importance
    varImpScore <- rep(0, length(thisExperiment$model$gdm$predictors))
    names(varImpScore) <- thisExperiment$model$gdm$predictors
    
    # Insert non-zero importance scores
    varImpScore[rownames(thisExperiment$model$varImp[[2]])] <- round(thisExperiment$model$varImp[[2]][, 1], 3)
    
    #ii <- order(varImpScore, decreasing = TRUE)
    varImpScore <- sort(varImpScore, decreasing = TRUE)
    
    # Trim variables to be plotted?
    if (toupper(showVarImp) == "NONZERO")
    {
      ii_zero <- which(varImpScore == 0)
      if (length(ii_zero) > 0)
        varImpScore <- varImpScore[-ii_zero]
    }
    
    # These are the variable names to be plotted...
    varNames <- names(varImpScore)
    #}
    
    # Set up a container for the ggplot objects
    plotyBits <- vector("list", length(varNames))
    names(plotyBits) <- varNames
    
    # Step through the list of variables to be plotted
    for (thisVar in varNames)
    {
      plotData <- data.frame(x = plotyStuff$x[, thisVar],
                             y = plotyStuff$y[, thisVar])
      
      #if (length(thisExperiment$model$varImp) > 0)
      #{
        plotyBits[[thisVar]] <- ggplot2::ggplot(plotData, aes(x = .data$x, y = .data$y)) +
          ggplot2::geom_line(colour = "blue", size = 1) +
          ggplot2::ylab(paste0("f(", thisVar, ")")) +
          ggplot2::xlab(thisVar) +
          ggplot2::ylim(c(0, ceiling(max(plotyStuff$y)))) +
          ggplot2::ggtitle(paste0("Contribution: ", round(varImpScore[thisVar], 2))) +
          ggplot2::theme(title = ggplot2::element_text(size = 9),
                         axis.title.x = ggplot2::element_text(size = 9),
                         axis.title.y = ggplot2::element_text(size = 9),
                         axis.text.x = ggplot2::element_text(size = 8),
                         axis.text.y = ggplot2::element_text(size = 8))
      #}
      # else
      # {
      #   plotyBits[[thisVar]] <- ggplot2::ggplot(plotData, aes(x = .data$x, y = .data$y)) +
      #     ggplot2::geom_line(colour = "blue", size = 1) +
      #     ggplot2::ylab(paste0("f(", thisVar, ")")) +
      #     ggplot2::xlab(thisVar) +
      #     ggplot2::ylim(c(0, ceiling(max(plotyStuff$y)))) +
      #     ggplot2::theme(title = ggplot2::element_text(size = 9),
      #                    axis.title.x = ggplot2::element_text(size = 9),
      #                    axis.title.y = ggplot2::element_text(size = 9),
      #                    axis.text.x = ggplot2::element_text(size = 8),
      #                    axis.text.y = ggplot2::element_text(size = 8))
      # }
    }
    
    # Save the multipanel plot
    ggpubr::ggarrange(plotlist = plotyBits, ncol = 3, nrow = 4) %>%
      ggpubr::ggexport(filename = paste0(outFolder, "/cmGDM_", thisExperiment$experimentName ,"_GDM_variable_splines_transformed.png"),
                       width = 1024, height = 1400, res = 150)
  }
}

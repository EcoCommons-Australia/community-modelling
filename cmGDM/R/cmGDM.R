
#' cmGDM: Community Modelling using Generalised Dissimilarity Modelling
#'
#' EcoCommons Community Modelling experiments run using Generalised Dissimilarity Modelling
#' 
#' Fitting a GDM using this package implements the following simple workflow:
#' 
#' \describe{
#'  \item{1.}{Create a new experiment by calling \link{cm_create_new_experiment}}
#'  \item{2.}{Load a table giving site/sample labels and coordinates with a call to \link{cm_load_site_table}}
#'  \item{3.}{Load covariate information to be used to fit the GDM using \link{cm_load_covar_data}}
#'  \item{4.}{Load data representing the response or predicted variable by calling \link{cm_load_community_data}}
#'  \item{5.}{Fit a GDM using a call to \link{cm_run_gdm_experiment}}
#'  \item{6.}{Perform optional follow-up steps including calling \link{cm_gdm_summary}, \link{cm_gdm_report}, \link{cm_performance_plots}, \link{cm_load_prediction_data} followed by a call to \link{cm_predict_gdm}, and \link{cm_gdm_pcaPlot}}
#' }
#' 
#' The workflow ensures that necessary cross-checks between data elements are made in a logical sequence. Loading data at any step requires each step before it to have been successfully completed.
#' 
#' Therefore, Step 5 can only be completed once the sequence of preceding steps has been successfully executed. That is, steps 1 to 4 load the minimum or essential data required to fit a GDM. 
#' 
#' Output for all functions is stored in a system-generated experiment folder within the user's storage area, and are theoretically available for the user to download and incorporate in various documents.
#' 
#' The function \link{cm_gdm_report} assembles graphical output (stored in the experiment folder) and other information into a pdf which documents the fitted model.
#' 
#' @references
#' Ferrier, S., G. Manion, J. Elith, and K. Richardson. 2007. Using generalized dissimilarity modelling to analyse and predict patterns of beta diversity in regional biodiversity assessment. \emph{Diversity and Distributions} 13:252-264.
#' 
#' Fitzpatrick, M. C., and S. R. Keller. 2015. Ecological genomics meets community-level modelling of biodiversity: mapping the genomic landscape of current and future environmental adaptation. \emph{Ecology Letters} 18:1-16.
#' 
#' Mokany, K. and others. In review. A working guide to harnessing generalized dissimilarity modelling for biodiversity analysis and conservation assessment. \emph{Global Ecology and Biogeography}
#' 
#' @import gdm
#' @importFrom ggplot2 aes xlab ylab geom_line geom_abline geom_point theme ggtitle
#' @importFrom magrittr %>%
#' @importFrom terra classify ext extract rast values writeRaster
#' @importFrom ggpubr ggarrange
#' @importFrom stringr str_pad
#' @importFrom grDevices dev.off png
#' @importFrom utils read.table
#' @importFrom raster raster stack nrow ncol predict res
#' @importFrom methods is
#' @importFrom stats na.omit
#' @importFrom png writePNG
#' @importFrom gdalUtils gdal_translate
#' @importFrom rmarkdown render
#' @importFrom rlang .data
#' @docType package
#' @name cmGDM
NULL

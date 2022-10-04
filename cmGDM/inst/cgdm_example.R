############################################################
###        EcoCommons: Community workflow (cmGDM)        ###
############################################################
##
## Author details:   EcoCommons Platform 
## Contact details:  comms@ecocommons.org.au
## Date:             September 2022
## Copyright phrase: This script is the product of EcoCommons platform.   
##                   Please refer to the EcoCommons website for more details:   
##                   <https://www.ecocommons.org.au/> 
##
#### Script and data info: 
#
# This notebook provides 4 examples on how to run a Community Modelling module 
# using the R-package `cmGDM`, created by Peter D. Wilson.
#
# Generalised Dissimilarity Modelling (GDM) has become a widely-used method to 
# link differences in composition between samples and explanatory environmental 
# variables (“covariates”). The dependent or predicted variable in a GDM may be 
# any form of distance or dissimilarity measure scaled to range between 0 
# (exactly matching pairs)and 1 (totally dissimilar pairs). GDMs may be fitted 
# using any assemblage of covariates thought to be important in explaining the 
# differences in composition between pairs of samples. The prototype R-package, 
# `cmGDM` is designed to implement GDM modelling as the first method within a 
# new EcoCommons Community Modelling module. Fitting a GDM is performed by the 
# R-package `gdm` available from the CRAN repository.`cmGDM` has been designed 
# to implement a simple, robust workflow for basic fitting, review and reporting 
# of GDMs.
#
# A. Species composition example: Presence-absence data
# B. Species composition example: Presence-absence data and environmental data as GIS layers
# C. Species composition example: Abundance data, richness site weights and environmental data as GIS layers
# D. Genetic diversity example

# Install the package from GitLab
remotes::install_gitlab("ecocommons-australia/community-modelling-workflow",
                        subdir = "cmGDM",
                        auth_token = "glpat-ZkrNPPksV74B1VnRKR2h",
                        quiet = TRUE,
                        force = TRUE)

library(cmGDM)

#     
### A. Species composition example: Presence-absence data

# 1. Create an experiment:
# Calling *cm_create_experiment()* generates an *blank* R S3 object in memory 
# ready for following steps, and saves it to the user’s work area.
# Note that the status for the data will be *FALSE* for all at this moment.
myExperiment <- cmGDM::cm_create_new_experiment(userID = "ID123",
                                                userName = "Your name here",
                                                experimentName = "Example GDM fit gdm pkg data",
                                                description = "Fit GDM using cmGDM applied to example data from the gdm package")
myExperiment$status

# 2. Load the site table:
myExperiment <- cmGDM::cm_load_site_table(myExperiment,
                                          siteFilename = "/Users/fen087/Documents/community modelling/Examples/gdm_pkg_data/gdm_pkg_site_table.csv",
                                          siteCol      = "site",
                                          longitudeCol = "Long",
                                          latitudeCol  = "Lat")
myExperiment$status

# 3. Load biological data:
# In this example we are loading a community table storing presence-absence coded data.
# `myExperiment$status` should be *TRUE* for both 'siteData' and 'biologicalData'
myExperiment <- cmGDM::cm_load_community_data(thisExperiment = myExperiment,
                                              bioFilename    = "/Users/fen087/Documents/community modelling/Examples/gdm_pkg_data/gdm_pkg_PA_table.csv",
                                              dataType       = "Presence_absence",
                                              siteCol        = "site",
                                              dissimMeasure  = "Bray-Curtis")
myExperiment$status

# 4. Load environmental covariate data:
# The environmental covariate data is loaded as a pre-assembled data table.
myExperiment <- cmGDM::cm_load_covar_data(myExperiment,
                                          src_folder     = "/Users/fen087/Documents/community modelling/Examples/gdm_pkg_data/",
                                          covar_filename = "gdm_pkg_env_table.csv")
myExperiment$status


# 5.Fit a GDM:
# After loading of site, biological and environmental data, we can now run the experiment.
# In this example, geographic distance is used as a covariate
myExperiment <- cmGDM::cm_run_gdm_experiment(myExperiment,
                                             includeGeo = TRUE)
myExperiment

# ALTERNATIVE: optional calculation of variable importance information. 
# Running this code will OVERWRITE the previous results stored in the object ‘myExperiment’.

myExperiment <- cmGDM::cm_run_gdm_experiment(myExperiment,
                                             includeGeo  = TRUE,
                                             calc_varImp = TRUE)
myExperiment

# 6: Post-fitting performance review and graphical output:
# Show summary  
cmGDM::cm_gdm_summary(myExperiment,
                      outFile = "/Users/fen087/Documents/community modelling/Examples/gdm_pkg_data/Experiment_cmGDM_summary.txt")

# Performance plots
cmGDM::cm_performance_plots(myExperiment)

cmGDM::cm_performance_plots(myExperiment,
                            showVarImp = "nonZero")
# Experiment report
cmGDM::cm_gdm_report(myExperiment)


#_______________________________________________________________________________

# B. Species composition example: Presence-absence data and environmental data as GIS layers

# 1. Create an experiment
# Calling *cm_create_experiment()* generates an *blank* R S3 object in memory 
# ready for following steps, and saves it to the user’s work area.
#Note that the status for the data will be *FALSE* for all at this moment.
myExperiment2 <- cmGDM::cm_create_new_experiment(userID = "ID123",
                                                 userName = "Peter D. Wilson",
                                                 experimentName = "Example GDM fit gdm pkg data Climate Only",
                                                 description = "Fit GDM using cmGDM applied to example data from the gdm package CLIMATE ONLY")

# 2. Load the site table:
myExperiment2 <- cmGDM::cm_load_site_table(myExperiment2,
                                           siteFilename = "/Users/fen087/Documents/community modelling/Examples/gdm_pkg_data/gdm_pkg_site_table.csv",
                                           siteCol      = "site",
                                           longitudeCol = "Long",
                                           latitudeCol  = "Lat")
myExperiment2$status

# 3. Load biological data:
# In this example we are loading a community table storing presence-absence coded data.
# `myExperiment$status` should be *TRUE* for both 'siteData' and 'biologicalData'
myExperiment2 <- cmGDM::cm_load_community_data(thisExperiment = myExperiment2,
                                               bioFilename   = "/Users/fen087/Documents/community modelling/Examples/gdm_pkg_data/gdm_pkg_PA_table.csv",
                                               dataType      = "Presence_absence",
                                               siteCol       = "site",
                                               dissimMeasure = "Bray-Curtis")
myExperiment2$status

# 4. Load environmental covariate data:
# For this example, we will load the environmental data as a set of GIS raster layers
myExperiment2 <- cmGDM::cm_load_covar_data(myExperiment2,
                                           src_folder = "/Users/fen087/Documents/community modelling/Examples/gdm_pkg_data/env_data/westOZ/",
                                           covar_filenames = c("westOZ_bio5.tif", "westOZ_bio6.tif", 
                                                               "westOZ_bio15.tif", "westOZ_bio18.tif",
                                                               "westOZ_bio19.tif"))
myExperiment2$status

# 5.Fit a GDM:
myExperiment2 <- cmGDM::cm_run_gdm_experiment(myExperiment2,
                                              includeGeo = TRUE,
                                              calc_varImp = TRUE)
myExperiment2

# 6: Post-fitting performance review and graphical output:
# Show summary  
cmGDM::cm_gdm_summary(myExperiment2)

# Performance plots
cmGDM::cm_performance_plots(myExperiment2)

# PCA plots
cmGDM::cm_gdm_pcaPlot(myExperiment2)

# Experiment report
cmGDM::cm_gdm_report(myExperiment2)


#_______________________________________________________________________________

# C. Species composition example: Abundance data, richness site weights and environmental data as GIS layers

# 1. Create an experiment:
# Calling *cm_create_experiment()* generates an *blank* R S3 object in memory 
# ready for following steps, and saves it to the user’s work area.
#Note that the status for the data will be *FALSE* for all at this moment.
myExperiment3 <- cmGDM::cm_create_new_experiment(userID        = "ID123",
                                                 userName       = "Peter D. Wilson",
                                                 experimentName = "Example GDM fit to abundance table",
                                                 description    = "Fit GDM to simulated abundance-type community table and apply a richness weighting")
myExperiment3$status

# 2. Load the site table:
# As previous, but now specify 'weightType'
myExperiment3 <- cmGDM::cm_load_site_table(myExperiment3,
                                           siteFilename = "/Users/fen087/Documents/community modelling/Examples/gdm_pkg_data/siteData.csv",
                                           siteCol = "site",
                                           longitudeCol = "longitude",
                                           latitudeCol = "latitude",
                                           weightType = "richness")
myExperiment3$status

# 3. Load biological data:
myExperiment3 <- cmGDM::cm_load_community_data(thisExperiment = myExperiment3,
                                               bioFilename = "/Users/fen087/Documents/community modelling/Examples/gdm_pkg_data/abundance_matrix.csv",
                                               siteCol = 1,
                                               dataType = "Abundance",
                                               dissimMeasure = "Bray-Curtis")

myExperiment3$status

# 4. Load environmental covariate data:
# For this example, the dataset consists in 19 Bioclim variables; this step can be slow
myExperiment3 <- cmGDM::cm_load_covar_data(thisExperiment = myExperiment3,
                                           src_folder = "/Users/fen087/Documents/community modelling/Examples/gdm_pkg_data/env_data/eastOZ",
                                           covar_filenames = list.files("/Users/fen087/Documents/community modelling/Examples/gdm_pkg_data/env_data/eastOZ",
                                                                        "*.tif"))
myExperiment3$status

# 5.Fit a GDM:
myExperiment3 <- cmGDM::cm_run_gdm_experiment(thisExperiment = myExperiment3,
                                              includeGeo = TRUE)

# 6: Post-fitting performance review and graphical output:
# Show summary  
cmGDM::cm_gdm_summary(myExperiment3)

# Performance plots
cmGDM::cm_performance_plots(myExperiment3)


#_______________________________________________________________________________

# D. Genetic diversity example

# 1. Create an experiment:
# Calling *cm_create_experiment()* generates an *blank* R S3 object in memory 
# ready for following steps, and saves it to the user’s work area.
# Note that the status for the data will be *FALSE* for all at this moment
myExperiment4 <- cmGDM::cm_create_new_experiment(userID = "ID123",
                                                 userName = "Peter D. Wilson",
                                                 experimentName = "Example GDM fit Fst",
                                                 description = "Use Acacia purpureopetala data as example")

# 2. Load the site table:
myExperiment4 <- cmGDM::cm_load_site_table(myExperiment4,
                                           siteFilename = "/Users/fen087/Documents/community modelling/Examples/gdm_pkg_data/siteLocation.csv",
                                           siteCol = "site",
                                           longitudeCol = "long",
                                           latitudeCol = "lat")
myExperiment4$status

# 3. Load biological data:
# It should be a pre-computed dissimilarity matrix storing FST values between 
#pairs of samples from each location recorded in the site date loaded in Step 2
myExperiment4 <- cmGDM::cm_load_community_data(thisExperiment = myExperiment4,
                                               bioFilename = "/Users/fen087/Documents/community modelling/Examples/gdm_pkg_data/Fstonlynewsitenamesonly_PDW.csv",
                                               dataType = "Dissimilarity",
                                               dissimMeasure = "Fst")
myExperiment4$status

# 4. Load environmental covariate data:
# For this example, we will be loading as a set of GIS layers
myExperiment4 <- cmGDM::cm_load_covar_data(myExperiment4,
                                           src_folder = "/Users/fen087/Documents/community modelling/Examples/gdm_pkg_data/env_data/acacia_purp",
                                           covar_filenames = paste0("AP_bio", stringr::str_pad(as.character(1:19),
                                                                                          side = "left", width = 2, pad = "0"), ".tif"))
myExperiment4$status

# 5.Fit a GDM:
# In this example, calc_varImp = TRUE
myExperiment4 <- cmGDM::cm_run_gdm_experiment(myExperiment4,
                                              includeGeo  = TRUE,
                                              calc_varImp = TRUE)
myExperiment4

# 6: Post-fitting performance review and graphical output:
# Show summary  
cmGDM::cm_gdm_summary(myExperiment4)

# Performance plots
cmGDM::cm_performance_plots(myExperiment4)
cmGDM::cm_performance_plots(myExperiment4,
                            showVarImp = "nonzero")
# PCA plots
cmGDM::cm_gdm_pcaPlot(myExperiment4)

# Experiment report
cmGDM::cm_gdm_report(myExperiment4)

#______________________________
# Publication plot
# Use of the GeoTIFF GIS raster file to generate a geo-referenced plot suitable 
#for publication 
library(raster)
library(tmap)
library(sf)

# Make sf point object for the sites in the experiment
siteData <- read.csv("/Users/fen087/Documents/community modelling/Examples/gdm_pkg_data/siteLocation.csv",
                     stringsAsFactors = FALSE)

siteData_sf <- sf::st_as_sf(siteData,
                            coords = c("long", "lat"),
                            crs = 4326)
siteLabels <- siteData
siteLabels$long <- siteLabels$long + c(0, 0, 0, 0, 0, 0, -0.09, +0.08, 0, 0, 0, 0, 0)
siteLabels$lat <- siteLabels$lat + c(0.015, -0.015, 0.015, 0.015, -0.015, 0.015, 0, 0, 0.015, -0.015, 0.015, 0.015, 0.015)
siteLabels$site <- gsub(".", " ", siteLabels$site,
                        fixed = TRUE)
siteLabels_sf <- sf::st_as_sf(siteLabels,
                              coords = c("long", "lat"),
                              crs = 4326)


# Load 3-band GeoTIFF as a raster stack as indicated in the stackexchange post
rr <- raster::stack("/Users/fen087/cmGDM/Example GDM fit Fst_GDM_transformed_PCA")

# Make nice map using the lovely tmap package:
theMap <- tmap::tm_shape(rr) +
  tm_graticules(lines = FALSE, ticks = TRUE) + 
  tm_xlab("Longitude") + tm_ylab("Latitude") +
  tm_rgb(r = 1, g = 2, b = 3, interpolate = FALSE) +
  tm_shape(siteData_sf) + tm_symbols(col = "black", size = 0.5) +
  tm_shape(siteLabels_sf) + tm_text("site", fontface = "bold")
print(theMap)

# Done!
# EcoCommons Community Modelling Module
#
# Test suite driver for prototype code development
#
####################################################
#         TEST ENVIRONMENTAL COVARIATES            #
####################################################
#
# Peter D. Wilson
# Adjunct Fellow
# Dept. of Biological Sciences
# Faculty of Science and Engineering
# Macquarie University, Sydney, Australia
#
# 2021-08-15: Version of the first kind
# 2021-11-24: Changed to use prototype package 'cmGDM', and
# generalised access to test data files via a global parameter which points to
# the data file folder.
#

library(cmGDM)


###########################################################
#               ENVIRONMENTAL COVARIATE DATA              #
###########################################################

base_folder <- "/home/peterw/Nyctimene/EcoCommons/R-scripts/protoype_dev/test_data/"

dataTestExp <- cmGDM::cm_create_new_experiment("peterw", "bioData tests")

dataTestExp <- cmGDM::cm_load_site_table(thisExperiment = dataTestExp,
                                         siteFilename = paste0(base_folder, "siteData_NSW_OK.csv"),
                                         siteCol = 1,
                                         longitudeCol = 2,
                                         latitudeCol = 3)

dataTestExp <- cmGDM::cm_load_community_data(thisExperiment = dataTestExp,
                                             siteCol = "site",
                                             bioFilename = paste0(base_folder, "community_PA_01_OK.csv"),
                                             dataType = "Presence_absence",
                                             dissimMeasure = "Bray-Curtis")

dataTestExp <- cmGDM::cm_load_covar_data(thisExperiment = dataTestExp,
                                         src_folder = "/home/peterw/Nyctimene/ENM_env_data/NSW-dataset/Current_climate/CHELSA_short",
                                         covar_filenames = c("CHELSA_bio01.tif", "CHELSA_bio12.tif"),
                                         label = "NSW CHELSA",
                                         trace = TRUE)



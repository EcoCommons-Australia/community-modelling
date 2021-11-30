# EcoCommons Community Modeling: GDM
#
# Test workflow using an Fst dissimilarity matrix
#
#
# Peter D. Wilson
# Adjunct Fellow
# Dept. of Biological Sciences
# Faculty of Science and Engineering
# Macquarie University, Sydney, Australia
#
# 2021-11-25 
#

library(cmGDM)


baseInputFolder <- "/home/peterw/Nyctimene/EcoCommons/R-scripts/protoype_dev/test_data"
baseOutputFolder <- "/home/peterw/Nyctimene/EcoCommons/R-scripts/protoype_dev/test_output"

dataTestExp2 <- cmGDM::cm_create_new_experiment("peterw", "Test with RF refugia data")

dataTestExp2 <- cmGDM::cm_load_site_table(thisExperiment = dataTestExp2,
                                         siteFilename = file.path(baseInputFolder, "Syncarpia_glomulifera_site_coordinates.csv"),
                                         siteCol = 1,
                                         longitudeCol = 2,
                                         latitudeCol = 3,
                                         trace = FALSE)

dataTestExp2 <- cmGDM::cm_load_community_data(thisExperiment = dataTestExp2,
                                             bioFilename = file.path(baseInputFolder, "SyncGlom_Fst_matrix.csv"),
                                             dataType = "Dissimilarity",
                                             dissimMeasure = "Fst",
                                             trace = FALSE)

dataTestExp2 <- cmGDM::cm_load_covar_data(thisExperiment = dataTestExp2,
                                         src_folder = "/home/peterw/Nyctimene/ENM_env_data/NSW-dataset/Current_climate/CHELSA_short",
                                         covar_filenames = list.files("/home/peterw/Nyctimene/ENM_env_data/NSW-dataset/Current_climate/CHELSA_short", "*.tif"),
                                         label = "NSW CHELSA",
                                         trace = FALSE)

dataTestExp2 <- cmGDM::cm_run_gdm_experiment(thisExperiment = dataTestExp2)

cmGDM::cm_gdm_summary(dataTestExp2, file.path(baseOutputFolder, "Fst_workflow_summary.txt"))
cm_performance_plots(dataTestExp2, outFolder = "/home/peterw/Nyctimene/EcoCommons/R-scripts/protoype_dev/test_output")
saveRDS(dataTestExp2, file = file.path(baseOutputFolder, "Fst_workflow_dataTestExp2.rds"))

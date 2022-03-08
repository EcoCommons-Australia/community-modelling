# EcoCommons Community Modelling Module
#
# Test suite driver for prototype code development
#
####################################################
#                 TEST SITE LOAD                   #
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
# 2022-02-19: Some code tidy-ups to adapt to modifications to cmGDM functions
#


library(cmGDM)

###########################################################
#             BIOLOGICAL/COMMUNITY SITE DATA              #
###########################################################

##### Presence-absence data

### CHANGE THE PATH TO SHOW THE FOLDER IN WHICH YOU HAVE STORED THE DOWNLOADED DATA FILES ####
base_folder <- "/home/peterw/Data_and_Projects/EcoCommons/community-modelling-workflow/test_data/"
##############################################################################################

dataTestExp <- cmGDM::cm_create_new_experiment(userID = "user123",
                                               userName = "peterw",
                                               experimentName = "bioData tests",
                                               description = "Run test to validate loading biological data")

dataTestExp <- cmGDM::cm_load_site_table(thisExperiment = dataTestExp,
                                         siteFilename = paste0(base_folder, "siteData_OK.csv"),
                                         siteCol = 1,
                                         longitudeCol = 2,
                                         latitudeCol = 3)

# Test 0: Call without any cm_experiment object or file name passed
ans <- cmGDM::cm_load_community_data()

# Test 1: Pass valid cm_experiment object but no filename
ans <- cmGDM::cm_load_community_data(thisExperiment = dataTestExp)

# Test 2: siteCol specified with integer: Expected outcome = error about unspecified data type
ans <- cmGDM::cm_load_community_data(thisExperiment = dataTestExp,
                                     siteCol = 1,
                                     bioFilename = paste0(base_folder, "community_PA_01_OK.csv"))

# Test 3: siteCol specified with name: Expected outcome = error about unspecified data type
ans <- cmGDM::cm_load_community_data(thisExperiment = dataTestExp,
                                     siteCol = "site",
                                     bioFilename = paste0(base_folder, "community_PA_01_OK.csv"))

# Test 2: siteCol specified with integer; dataType = "Presence_absence";
# Expected outcome = error about unspecified dissimMeasure
ans <- cmGDM::cm_load_community_data(thisExperiment = dataTestExp,
                                     siteCol = 1,
                                     bioFilename = paste0(base_folder, "community_PA_01_OK.csv"),
                                     dataType = "Presence_absence")

# Test 3: siteCol specified with name; dataType = "Presence_absence";
# Expected outcome = error about unspecified dissimMeasure
ans <- cmGDM::cm_load_community_data(thisExperiment = dataTestExp,
                                     siteCol = "site",
                                     bioFilename = paste0(base_folder, "community_PA_01_OK.csv"),
                                     dataType = "Presence_absence")

# Test 4: siteCol specified with number; dataType = "Presence_absence"; dissimMeasure = "Bray-Curtis";
# Expected outcome = PASS
ans <- cmGDM::cm_load_community_data(thisExperiment = dataTestExp,
                                     siteCol = 1,
                                     bioFilename = paste0(base_folder, "community_PA_01_OK.csv"),
                                     dataType = "Presence_absence",
                                     dissimMeasure = "Bray-Curtis")

# Test 5: siteCol specified with name; dataType = "Presence_absence"; dissimMeasure = "Bray-Curtis";
# Expected outcome = PASS
ans <- cmGDM::cm_load_community_data(thisExperiment = dataTestExp,
                                     siteCol = "site",
                                     bioFilename = paste0(base_folder, "community_PA_01_OK.csv"),
                                     dataType = "Presence_absence",
                                     dissimMeasure = "Bray-Curtis")

# Test 6: siteCol specified with bad name
ans <- cmGDM::cm_load_community_data(thisExperiment = dataTestExp,
                                     siteCol = "teddybear",
                                     bioFilename = paste0(base_folder, "community_PA_01_OK.csv"),
                                     dataType = "Presence_absence",
                                     dissimMeasure = "Bray-Curtis")

# Test 7: siteCol specified with bad number
ans <- cmGDM::cm_load_community_data(thisExperiment = dataTestExp,
                                     siteCol = -1,
                                     bioFilename = paste0(base_folder, "community_PA_01_OK.csv"),
                                     dataType = "Presence_absence",
                                     dissimMeasure = "Bray-Curtis")

# Test 8: siteCol specified with bad number
ans <- cmGDM::cm_load_community_data(thisExperiment = dataTestExp,
                                     siteCol = 6,
                                     bioFilename = paste0(base_folder, "community_PA_01_OK.csv"),
                                     dataType = "Presence_absence",
                                     dissimMeasure = "Bray-Curtis")

# Test 9: siteCol specified with name; dataType = "Presence_absence"; dissimMeasure = "Fst";
# Expected result: FAILURE with message about bad dissimMeasure for presence-absence data
ans <- cmGDM::cm_load_community_data(thisExperiment = dataTestExp,
                                     siteCol = "site",
                                     bioFilename = paste0(base_folder, "community_PA_01_OK.csv"),
                                     dataType = "Presence_absence",
                                     dissimMeasure = "Fst")

# Test 11: File with missing site (i.e. row)
ans <- cmGDM::cm_load_community_data(thisExperiment = dataTestExp,
                                     siteCol = 1,
                                     bioFilename = paste0(base_folder, "community_PA_missingSite.csv"),
                                     dataType = "Presence_absence",
                                     dissimMeasure = "Bray-Curtis")

# Test 12: File with missing data as an empty cell
ans <- cmGDM::cm_load_community_data(thisExperiment = dataTestExp,
                                     siteCol = 1,
                                     bioFilename = paste0(base_folder, "community_PA_blankCell_present.csv"),
                                     dataType = "Presence_absence",
                                     dissimMeasure = "Bray-Curtis")

# Test 13: File with missing data as an NA cell
ans <- cmGDM::cm_load_community_data(thisExperiment = dataTestExp,
                                     siteCol = 1,
                                     bioFilename = paste0(base_folder, "community_PA_NA_present.csv"),
                                     dataType = "Presence_absence",
                                     dissimMeasure = "Bray-Curtis")

# Test 14: Presence-absence file encoded with "pa"; Expected result: PASS
ans <- cmGDM::cm_load_community_data(thisExperiment = dataTestExp,
                                     siteCol = "Site",
                                     bioFilename = paste0(base_folder, "community_PA_pa_OK.csv"),
                                     presenceMarker = "p",
                                     absenceMarker = "a",
                                     dataType = "Presence_absence",
                                     dissimMeasure = "Bray-Curtis")

# Test 15: Presence-absence file encoded with "012" i.e. a data entry typo!; Expected result: FAIL
ans <- cmGDM::cm_load_community_data(thisExperiment = dataTestExp,
                                     siteCol = "Site",
                                     bioFilename = paste0(base_folder, "community_PA_012.csv"),
                                     presenceMarker = "1",
                                     absenceMarker = "0",
                                     dataType = "Presence_absence",
                                     dissimMeasure = "Bray-Curtis")

# Test 16: Presence-absence file encoded with "pas" i.e. a data entry typo!; Expected result: FAIL
ans <- cmGDM::cm_load_community_data(thisExperiment = dataTestExp,
                                     siteCol = "Site",
                                     bioFilename = paste0(base_folder, "community_PA_pas.csv"),
                                     presenceMarker = "p",
                                     absenceMarker = "a",
                                     dataType = "Presence_absence",
                                     dissimMeasure = "Bray-Curtis")

##### Dissimilarity

base_folder <- "/home/peterw/Nyctimene/EcoCommons/R-scripts/protoype_dev/test_data/"

dataTestExp <- cmGDM::cm_create_new_experiment(userID = "user123",
                                               userName = "peterw",
                                               experimentName = "bioData tests Fst",
                                               description = "Run test to validate loading biological data as an Fst")

dataTestExp <- cmGDM::cm_load_site_table(thisExperiment = dataTestExp,
                                         siteFilename = paste0(base_folder, "siteData_OK.csv"),
                                         siteCol = 1,
                                         longitudeCol = 2,
                                         latitudeCol = 3)

# Test 1: Fst matrix: Name mismatch between rownames and colnames: i.e. hand-assembled table with a typo
ans <- cmGDM::cm_load_community_data(thisExperiment = dataTestExp,
                                     bioFilename = paste0(base_folder, "community_Fst_matrix_nameMismatch.csv"),
                                     dataType = "Dissimilarity",
                                     dissimMeasure = "Fst")

# Test 2: Fst matrix: Correction of very small negative values
ans <- cmGDM::cm_load_community_data(thisExperiment = dataTestExp,
                                     bioFilename = paste0(base_folder, "community_Fst_matrix_smallNegVal.csv"),
                                     dataType = "Dissimilarity",
                                     dissimMeasure = "Fst")

# Test 3: Fst matrix: Trap large negative values
ans <- cmGDM::cm_load_community_data(thisExperiment = dataTestExp,
                                     bioFilename = paste0(base_folder, "community_Fst_matrix_bigNegVal.csv"),
                                     dataType = "Dissimilarity",
                                     dissimMeasure = "Fst")

# Test 5: Fst matrix: Error-free load
ans <- cmGDM::cm_load_community_data(thisExperiment = dataTestExp,
                                     bioFilename = paste0(base_folder, "community_Fst_matrix_OK.csv"),
                                     dataType = "Dissimilarity",
                                     dissimMeasure = "Fst")

# Test 6: Other dissimilarity matrix: Negative value
ans <- cmGDM::cm_load_community_data(thisExperiment = dataTestExp,
                                     bioFilename = paste0(base_folder, "community_Dissim_matrix_negVal.csv"),
                                     dataType = "Dissimilarity",
                                     dissimMeasure = "Bray-Curtis")

# Test 7: Other dissimilarity matrix: Non-symmetric
ans <- cmGDM::cm_load_community_data(thisExperiment = dataTestExp,
                                     bioFilename = paste0(base_folder, "community_Dissim_matrix_nonSym.csv"),
                                     dataType = "Dissimilarity",
                                     dissimMeasure = "Bray-Curtis")

# Test 8: Other dissimilarity matrix: Non-symmetric
ans <- cmGDM::cm_load_community_data(thisExperiment = dataTestExp,
                                     bioFilename = paste0(base_folder, "community_Dissim_matrix_badName.csv"),
                                     dataType = "Dissimilarity",
                                     dissimMeasure = "Bray-Curtis")

# Test 8: Other dissimilarity matrix: Extra name in dissim matrix
ans <- cmGDM::cm_load_community_data(thisExperiment = dataTestExp,
                                     bioFilename = paste0(base_folder, "community_Dissim_matrix_extraName.csv"),
                                     dataType = "Dissimilarity",
                                     dissimMeasure = "Bray-Curtis")

# Test 8: Other dissimilarity matrix: Missing name in dissim matrix
ans <- cmGDM::cm_load_community_data(thisExperiment = dataTestExp,
                                     bioFilename = paste0(base_folder, "community_Dissim_matrix_missingName.csv"),
                                     dataType = "Dissimilarity",
                                     dissimMeasure = "Bray-Curtis")

# Test 9: Other dissimilarity matrix: Error-free load
ans <- cmGDM::cm_load_community_data(thisExperiment = dataTestExp,
                                     bioFilename = paste0(base_folder, "community_Dissim_matrix_OK.csv"),
                                     dataType = "Dissimilarity",
                                     dissimMeasure = "Bray-Curtis")

##### Abundance (aka Count data)

# Test 1: Other dissimilarity matrix: Negative value
ans <- cmGDM::cm_load_community_data(thisExperiment = dataTestExp,
                                     siteCol = 1,
                                     bioFilename = paste0(base_folder, "community_Count_negVal.csv"),
                                     dataType = "Abundance",
                                     dissimMeasure = "Bray-Curtis")

# Test 2: Other dissimilarity matrix: Missing site
ans <- cmGDM::cm_load_community_data(thisExperiment = dataTestExp,
                                     siteCol = 1,
                                     bioFilename = paste0(base_folder, "community_Count_missingSite.csv"),
                                     dataType = "Abundance",
                                     dissimMeasure = "Bray-Curtis")

# Test 3: Other dissimilarity matrix: Extra site
ans <- cmGDM::cm_load_community_data(thisExperiment = dataTestExp,
                                     siteCol = 1,
                                     bioFilename = paste0(base_folder, "community_Count_extraSite.csv"),
                                     dataType = "Abundance",
                                     dissimMeasure = "Bray-Curtis")

# Test XX: Other dissimilarity matrix: Error-free load
ans <- cmGDM::cm_load_community_data(thisExperiment = dataTestExp,
                                     siteCol = 1,
                                     bioFilename = paste0(base_folder, "community_Count_OK.csv"),
                                     dataType = "Abundance",
                                     dissimMeasure = "Bray-Curtis")


##### Abundance with weights loaded by load_site_table(). Therefore, each test
##### is preceded by a call to this function to instantiate an object with site
##### data correctly loaded

# Test 1: Abundance (count) data with weightType == 'equal': Expect error-free load
dataTestExp <- cmGDM::cm_load_site_table(thisExperiment = dataTestExp,
                                         siteFilename = paste0(base_folder, "siteData_OK.csv"),
                                         siteCol = 1,
                                         longitudeCol = 2,
                                         latitudeCol = 3)

ans <- cmGDM::cm_load_community_data(thisExperiment = dataTestExp,
                                     siteCol = 1,
                                     bioFilename = paste0(base_folder, "community_Count_OK.csv"),
                                     dataType = "Abundance",
                                     dissimMeasure = "Bray-Curtis")

# Test 2: Abundance (count) data with weightType == 'richness': Expect error-free load
dataTestExp <- cmGDM::cm_load_site_table(thisExperiment = dataTestExp,
                                         siteFilename = paste0(base_folder, "siteData_OK.csv"),
                                         siteCol = 1,
                                         longitudeCol = 2,
                                         latitudeCol = 3,
                                         weightType = "richness")

ans <- cmGDM::cm_load_community_data(thisExperiment = dataTestExp,
                                     siteCol = 1,
                                     bioFilename = paste0(base_folder, "community_Count_OK.csv"),
                                     dataType = "Abundance",
                                     dissimMeasure = "Bray-Curtis")

# Check that the site data table has been updated with richness values as
# weights: expect to see a "weights" column with values 1, 3, 3, 3, 3
print(ans$data$siteData$dataTable)

# Test 3: Abundance (count) data with weightType == 'custom': Expect error-free load
dataTestExp <- cmGDM::cm_load_site_table(thisExperiment = dataTestExp,
                                         siteFilename = paste0(base_folder, "siteData_weights_custom_user_colname_OK.csv"),
                                         siteCol = 1,
                                         longitudeCol = 2,
                                         latitudeCol = 3,
                                         weightType = "custom",
                                         weightsCol = "site_weights")

ans <- cmGDM::cm_load_community_data(thisExperiment = dataTestExp,
                                     siteCol = 1,
                                     bioFilename = paste0(base_folder, "community_Count_OK.csv"),
                                     dataType = "Abundance",
                                     dissimMeasure = "Bray-Curtis")

# Check that the site data table has been updated with custom values as
# weights: expect to see a "weights" column with values 1, 1, 1, 2, 2 in column
# named "site_weights"
cat("Site data table:\n")
print(ans$data$siteData$dataTable)
cat("-----------------------------------------\n\n")

# and expect to see weightType and weightsCol values recorded in site data
# information
cat("Site weight parameters:\n")
cat("  Weight type:", ans$data$siteData$weightType, "\n")
cat("  Weights column:", ans$data$siteData$weightsCol, "\n")
cat("-----------------------------------------\n\n")



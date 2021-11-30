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
#

library(cmGDM)

base_folder <- "/home/peterw/Nyctimene/EcoCommons/R-scripts/protoype_dev/test_data/"

dataTestExp <- cmGDM::cm_create_new_experiment("peterw", "Site data tests")

###########################################################
#                      SITE DATA                          #
###########################################################

# Test 0: Call with experimentObj = NULL
ans <- cm_load_site_table()

# Test 1: siteFilename parameter not given a value and therefore has default
# value of NULL
ans <- cm_load_site_table(thisExperiment = dataTestExp)

# Test 2: File referenced in siteFilename does not exist
ans <- cm_load_site_table(thisExperiment = dataTestExp,
                          siteFilename = "nowhere_to_be_found")

# Test 3: Valid siteFilename and valid fileType, but siteCol parameter not given
# a value and therefore has default value of NULL
ans <- cm_load_site_table(thisExperiment = dataTestExp,
                          siteFilename = paste0(base_folder, "siteData_OK.csv"))

# Test 4: siteCol as an out of bounds integer: below lower bound
ans <- cm_load_site_table(thisExperiment = dataTestExp,
                          siteFilename = paste0(base_folder, "siteData_OK.csv"),
                          siteCol = -1) 

# Test 5: siteCol as an out of bounds integer: above upper bound
ans <- cm_load_site_table(thisExperiment = dataTestExp,
                          siteFilename = paste0(base_folder, "siteData_OK.csv"),
                          siteCol = 11)

# Test 6: siteCol as a column name not found in colnames()
ans <- cm_load_site_table(thisExperiment = dataTestExp,
                          siteFilename = paste0(base_folder, "siteData_OK.csv"),
                          siteCol = "notThere")

# Test 7: Bad siteCol: duplicated site labels/names
ans <- cm_load_site_table(thisExperiment = dataTestExp,
                          siteFilename = paste0(base_folder, "siteData_duplicatedSiteLabel.csv"),
                          siteCol = "site")

# Test 8: Bad siteCol: missing as "" site labels/names
ans <- cm_load_site_table(thisExperiment = dataTestExp,
                          siteFilename = paste0(base_folder, "siteData_missingSiteLabel.csv"),
                          siteCol = "site")

# Test 9: Bad siteCol: missing as "NA" site labels/names
ans <- cm_load_site_table(thisExperiment = dataTestExp,
                          siteFilename = paste0(base_folder, "siteData_missingNA_SiteLabel.csv"),
                          siteCol = "site")

# Test 10a: longitudeCol not been given a value
ans <- cm_load_site_table(thisExperiment = dataTestExp,
                          siteFilename = paste0(base_folder, "siteData_OK.csv"),
                          siteCol = "site")

# Test 10b: latitudeCol not been given a value
ans <- cm_load_site_table(thisExperiment = dataTestExp,
                          siteFilename = paste0(base_folder, "siteData_OK.csv"),
                          siteCol = "site",
                          longitudeCol = 2)

# Test 11a: at least one longitude value is missing
ans <- cm_load_site_table(thisExperiment = dataTestExp,
                          siteFilename = paste0(base_folder, "siteData_missingLongitude.csv"),
                          siteCol = "site",
                          longitudeCol = 2,
                          latitudeCol = 3)

# Test 11a: at least one longitude value has a bad value i.e. negative value
ans <- cm_load_site_table(thisExperiment = dataTestExp,
                          siteFilename = paste0(base_folder, "siteData_negLongitude.csv"),
                          siteCol = "site",
                          longitudeCol = 2,
                          latitudeCol = 3)

# Test 12a: at least one latitude value is missing
ans <- cm_load_site_table(thisExperiment = dataTestExp,
                          siteFilename = paste0(base_folder, "siteData_missingLatitude.csv"),
                          siteCol = "site",
                          longitudeCol = 2,
                          latitudeCol = 3)

# Test 12a: at least one latitude value has a bad value i.e. positive value
ans <- cm_load_site_table(thisExperiment = dataTestExp,
                          siteFilename = paste0(base_folder, "siteData_posLatitude.csv"),
                          siteCol = "site",
                          longitudeCol = 2,
                          latitudeCol = 3)

# Test 13: load valid file in xls format
ans <- cm_load_site_table(thisExperiment = dataTestExp,
                          siteFilename = paste0(base_folder, "siteData_OK.xls"),
                          siteCol = "site",
                          longitudeCol = 2,
                          latitudeCol = 3)

# Test 14: load valid file in xlsx format
ans <- cm_load_site_table(thisExperiment = dataTestExp,
                          siteFilename = paste0(base_folder, "siteData_OK.xlsx"),
                          siteCol = "site",
                          longitudeCol = 2,
                          latitudeCol = 3)

# Test 15: load valid file in xlsx format
ans <- cm_load_site_table(thisExperiment = dataTestExp,
                          siteFilename = paste0(base_folder, "siteData_OK.ods"),
                          siteCol = "site",
                          longitudeCol = 2,
                          latitudeCol = 3)

# Test 16: load valid file in txt format with a TAB delimiter
ans <- cm_load_site_table(thisExperiment = dataTestExp,
                          siteFilename = paste0(base_folder, "siteData_OK_TABdelim.txt"),
                          siteCol = "site",
                          longitudeCol = 2,
                          latitudeCol = 3)

# Test 17: load valid file in txt format with a SPACE delimiter
ans <- cm_load_site_table(thisExperiment = dataTestExp,
                          siteFilename = paste0(base_folder, "siteData_OK_SPACEdelim.txt"),
                          siteCol = "site",
                          longitudeCol = 2,
                          latitudeCol = 3)

# Test 18: load valid file in txt format with a COMMA delimiter
ans <- cm_load_site_table(thisExperiment = dataTestExp,
                          siteFilename = paste0(base_folder, "siteData_OK_COMMAdelim.txt"),
                          siteCol = "site",
                          longitudeCol = 2,
                          latitudeCol = 3)



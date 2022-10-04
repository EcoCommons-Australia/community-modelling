---
'output:
  pdf_document: default
  html_document: default'
---
<center>

[![EcoCommons GDM Workflow](attachment: 3eb9a10e-f24b-49a1-a356-645195f2a11b.jpg)](https: //www.ecocommons.org.au/)
    
</center>

# Community modelling workflow
### Generalised Dissimilarity Modelling (GDM)

---
Author details:  EcoCommons Platform   
Contact details: comms@ecocommons.org.au  
Copyright statement: This script is the product of EcoCommons platform.   
                     Please refer to the EcoCommons website for more details:   
                     <https://www.ecocommons.org.au/>  
Date: October 2022  

---

#### Script and data info: 

This notebook provides an example on how to run a Community Modelling module using the R-package `cmGDM`, created by Peter D. Wilson.

Generalised Dissimilarity Modelling (GDM) has become a widely-used method to link differences in composition
between samples and explanatory environmental variables (“covariates”). The dependent or predicted variable
in a GDM may be any form of distance or dissimilarity measure scaled to range between 0 (exactly matching pairs)and 1 (totally dissimilar pairs). GDMs may be fitted using any assemblage of covariates thought to be important in explaining the differences in composition between pairs of samples. The prototype R-package, `cmGDM` is designed to implement GDM
modelling as the first method within a new EcoCommons Community Modelling module. Fitting a GDM is performed by the R-package `gdm` available from the [CRAN repository](https://cran.r-project.org/web/packages/gdm/index.html) . `cmGDM` has been designed to implement a simple, robust workflow for basic fitting, review and reporting of GDMs.

In the near future, this material may form part of comprehensive support materials available to EcoCommons users. If you have any corrections or suggestions, please [contact the EcoCommons](mailto:comms@ecocommons.org.au) support and communications team.

---
### Input data type 

The fundamental data elements required to fit a GDM are:
- site/sample ID and location data
- a suite of environmental predictors or covariates associated with each site/sample
- data on the composition of entities at each site/sample, or a pre-computed dissimilarity matrix giving a dissimilarity measure between each pair of sites or samples

The package `cmGDM` accepts three basic table formats, where users can generate pre-processed
or derived tables by asking users to supply necessary information. For example, users will be asked to load
community data as either:

- presence-absence table
- abundance table
- dissimilarity table

In the first two table types, they are standard tables which may be assembled by hand (in spreadsheets for example) or
output by functions in other R-packages. For dissimilarity tables, many functions in other R-packages can
produce dissimilarity tables including the R-package `vegan`, and several packages for analysing phylogenetic
and population genetic data. The path to data importation chosen during the development of `cmGDM` is to follow procedures referred to as “bioformat = 3” in the `gdm` package.

The workflow embodied in the `cmGDM` package is a simple, but strict, linear sequence designed to ensure maximum
data integrity whilst providing a reasonable level of flexibility regarding data format options for each data element.

<center>

![Figure 1. Steps in the cmGDM workflow for fitting GDMs](attachment:d97cb1bc-6e94-4a3a-9d4e-5c3194371cfc.jpg)

</center>


## Examples

You will find below four examples to run 

- [A. Species composition example: Presence-absence data](#A. Species composition example: Presence-absence data)  
- [B. Species composition example: Presence-absence data and environmental data as GIS layers] (#B. Species composition example: Presence-absence data and environmental data as GIS layers)  
- [C. Species composition example: Abundance data, richness site weights and environmental data as GIS layers](#C. Species composition example: Abundance data, richness site weights and environmental data as GIS layers)  
- [D. Genetic diversity example](#D. Genetic diversity example)


---
<center>
    
# A. Species composition example: Presence-absence data
    
</center>

The original development of GDM was focused on modelling dissimilarity in species composition between sites or
samples as a function of geographical distance between them, and differences in environments (Ferrier et al. 2007;
Fitzpatrick and Keller 2015). Community composition data used in community ecology is expected to be a widely-used
standard format in ecology: sites/samples as rows and species as columns. Data in a community table may be the
abundance of each species at each site, or a record of presence or absence of each species at each site. This worked
example shows the fitting of a GDM to a presence-absence community table.

Data for this example is adapted from the example data set supplied by the `gdm` R-package. These data were originally
from a study examining species composition in samples of plant communities in south-west Australia modelled in
relation to a number of environmental covariates (Fitzpatrick et al. 2013). The following tables were extracted from
the `gdm` package data set in formats suitable for the cmGDM workflow, namely:
- A site table;
- A community table recoded as a presence-absence table; and,
- Environmental covariate data, both GIS layers and as a table of environmental data at each site.

#### Step 1 - Create an experiment:
This is the essential first step in fitting a GDM using `cmGDM`. Calling *cm_create_experiment()* generates an *blank* R S3 object of class *cm_experiment* in memory ready for following steps, and saves it to the user’s work area. After successful completion of each step in the workflow, the stored _cm_experiment_ object for that experiment is updated and saved to the user’s work area. Note that the status for the data will be *FALSE* for all at this moment.


```R
# install the package from GitLab
#remotes::install_gitlab("ecocommons-australia/community-modelling-workflow",
#                        subdir = "cmGDM",
#                        auth_token = "glpat-ZkrNPPksV74B1VnRKR2h",
#                        quiet = TRUE)
library(cmGDM)

myExperiment <- cmGDM::cm_create_new_experiment(userID = "ID123",
                                                userName = "Your name here",
                                                experimentName = "Example GDM fit gdm pkg data",
                                                description = "Fit GDM using cmGDM applied to example data from the gdm package")
myExperiment$status
```


    Error: package or namespace load failed for ‘cmGDM’ in loadNamespace(i, c(lib.loc, .libPaths()), versionCheck = vI[[i]]):
     namespace ‘rlang’ 0.4.12 is already loaded, but >= 1.0.0 is required
    Traceback:


    1. library(cmGDM)

    2. tryCatch({
     .     attr(package, "LibPath") <- which.lib.loc
     .     ns <- loadNamespace(package, lib.loc)
     .     env <- attachNamespace(ns, pos = pos, deps, exclude, include.only)
     . }, error = function(e) {
     .     P <- if (!is.null(cc <- conditionCall(e))) 
     .         paste(" in", deparse(cc)[1L])
     .     else ""
     .     msg <- gettextf("package or namespace load failed for %s%s:\n %s", 
     .         sQuote(package), P, conditionMessage(e))
     .     if (logical.return && !quietly) 
     .         message(paste("Error:", msg), domain = NA)
     .     else stop(msg, call. = FALSE, domain = NA)
     . })

    3. tryCatchList(expr, classes, parentenv, handlers)

    4. tryCatchOne(expr, names, parentenv, handlers[[1L]])

    5. value[[3L]](cond)

    6. stop(msg, call. = FALSE, domain = NA)


#### Step 2 - Load the site table:
A site table must be loaded next. It is the basis for checking the completeness of subsequent data elements
and, in the case of loading environmental covariate data as GIS layers, essential for extracting data from them to create an environmental data table. Note that now the `myExperiment$status` should be *TRUE* for siteData and you are able to visualise a dataTable under `myExperiment$data$siteData`. 


```R
myExperiment <- cmGDM::cm_load_site_table(thisExperiment = myExperiment,
                                          siteFilename = "/home/jovyan/gdm_pkg_data/gdm_pkg_site_table.csv",
                                          siteCol      = "site",
                                          longitudeCol = "Long",
                                          latitudeCol  = "Lat")
myExperiment$status
```


    Error in loadNamespace(i, c(lib.loc, .libPaths()), versionCheck = vI[[i]]): namespace ‘rlang’ 0.4.12 is already loaded, but >= 1.0.0 is required
    Traceback:


    1. loadNamespace(x)

    2. namespaceImportFrom(ns, loadNamespace(j <- i[[1L]], c(lib.loc, 
     .     .libPaths()), versionCheck = vI[[j]]), i[[2L]], from = package)

    3. asNamespace(ns)

    4. loadNamespace(j <- i[[1L]], c(lib.loc, .libPaths()), versionCheck = vI[[j]])

    5. namespaceImport(ns, loadNamespace(i, c(lib.loc, .libPaths()), 
     .     versionCheck = vI[[i]]), from = package)

    6. loadNamespace(i, c(lib.loc, .libPaths()), versionCheck = vI[[i]])

    7. namespaceImport(ns, loadNamespace(i, c(lib.loc, .libPaths()), 
     .     versionCheck = vI[[i]]), from = package)

    8. loadNamespace(i, c(lib.loc, .libPaths()), versionCheck = vI[[i]])

    9. stop(gettextf("namespace %s %s is already loaded, but %s %s is required", 
     .     sQuote(package), current, zop, zversion), domain = NA)


#### Step 3 - Load biological data:
For this experiment, we are loading a community table storing presence-absence coded data. The path to the local
copy of the data file is passed in the parameter ‘bioFilename’ when *cm_load_community_data()*
is called. The dissimilarity measure which will be computed on the data table is the Bray-Curtis dissimilarity index.

Note that now the `myExperiment$status` should be *TRUE* for both 'siteData' and 'biologicalData', and you are able to visualise your biological data.


```R
myExperiment <- cmGDM::cm_load_community_data(thisExperiment = myExperiment,
                                              bioFilename    = "/home/jovyan/gdm_pkg_data/gdm_pkg_PA_table.csv",
                                              dataType       = "Presence_absence",
                                              siteCol        = "site",
                                              dissimMeasure  = "Bray-Curtis")
myExperiment$status
```


<style>
.dl-inline {width: auto; margin:0; padding: 0}
.dl-inline>dt, .dl-inline>dd {float: none; width: auto; display: inline-block}
.dl-inline>dt::after {content: ":\0020"; padding-right: .5ex}
.dl-inline>dt:not(:first-of-type) {padding-left: .5ex}
</style><dl class=dl-inline><dt>siteData_OK</dt><dd>TRUE</dd><dt>biologicalData_OK</dt><dd>TRUE</dd><dt>covarData_OK</dt><dd>FALSE</dd><dt>predictionData_OK</dt><dd>FALSE</dd><dt>modelFit_OK</dt><dd>FALSE</dd></dl>



#### Step 4 - Load environmental covariate data:
Assuming that you have downloaded a local copy of this file, you must provide the path to your local folder in
“src_folder” to reference this location on your computer. In this code chunk, the environmental covariate data is loaded as a pre-assembled data table. This will allow a GDM to be fitted, but means that it is not possible to create some forms of output (e.g. raster map of predicted compositional similarity - see worked examples B and C below) as this requires the environmental data to be presented to the workflow as GIS layers.

Note that now the `myExperiment$status` should be *TRUE* for 'siteData', 'biologicalData' and 'covarData', and you are able to visualise your covariate data.


```R
myExperiment <- cmGDM::cm_load_covar_data(myExperiment,
                                          src_folder     = "/home/jovyan/gdm_pkg_data/",
                                          covar_filename = "gdm_pkg_env_table.csv")
myExperiment$status
```


<style>
.dl-inline {width: auto; margin:0; padding: 0}
.dl-inline>dt, .dl-inline>dd {float: none; width: auto; display: inline-block}
.dl-inline>dt::after {content: ":\0020"; padding-right: .5ex}
.dl-inline>dt:not(:first-of-type) {padding-left: .5ex}
</style><dl class=dl-inline><dt>siteData_OK</dt><dd>TRUE</dd><dt>biologicalData_OK</dt><dd>TRUE</dd><dt>covarData_OK</dt><dd>TRUE</dd><dt>predictionData_OK</dt><dd>FALSE</dd><dt>modelFit_OK</dt><dd>FALSE</dd></dl>



#### Step 5 - Fit a GDM:
After the successful loading of site, biological and environmental data, we can now run the experiment.

The default parameter settings for fitting a basic GDM in `cmGDM` is with geographic distance excluded and variable importance not calculated. This performs a basic model fit and is an extremely fast computational task. Generating variable importance information is a very computationally expensive process. If you wish to perform this step, you can re-run the experiment by setting the parameter `calc_varImp = TRUE` in the call to *cm_run_experiment()* as shown in the alternate form of Step 5 below.

In this call, geographic distance is used as a covariate. Including geographic distance has a trivial impact on the speed of model fitting.


```R
myExperiment <- cmGDM::cm_run_gdm_experiment(myExperiment,
                                             includeGeo = TRUE)
myExperiment
```


    Error in gdm::formatsitepair(bioData = bio_table, bioFormat = 1, dist = thisDissimMeasure, : object 'siteColName' not found
    Traceback:


    1. cmGDM::cm_run_gdm_experiment(myExperiment, includeGeo = TRUE)

    2. gdm::formatsitepair(bioData = bio_table, bioFormat = 1, dist = thisDissimMeasure, 
     .     abundance = ifelse(thisExperiment$data$biologicalData$dataType == 
     .         "Abundance", TRUE, FALSE), siteColumn = siteColName, 
     .     XColumn = "X", YColumn = "Y", predData = envTable, sppFilter = thisExperiment$data$biologicalData$sppFilter, 
     .     weightType = thisExperiment$data$siteData$weightType, custWeights = customWeights)


**ALTERNATIVE Step 5:** Run experiment with optional calculation of variable importance information. NOTE:
Running this code will OVERWRITE the previous results stored in the object ‘myExperiment’. If you wish to keep
these alternate runs completely separate, you should set up a new experiment for each one.


```R
myExperiment <- cmGDM::cm_run_gdm_experiment(myExperiment,
                                             includeGeo  = TRUE,
                                             calc_varImp = TRUE)
myExperiment
```


    Error in gdm::formatsitepair(bioData = bio_table, bioFormat = 1, dist = thisDissimMeasure, : object 'siteColName' not found
    Traceback:


    1. cmGDM::cm_run_gdm_experiment(myExperiment, includeGeo = TRUE, 
     .     calc_varImp = TRUE)

    2. gdm::formatsitepair(bioData = bio_table, bioFormat = 1, dist = thisDissimMeasure, 
     .     abundance = ifelse(thisExperiment$data$biologicalData$dataType == 
     .         "Abundance", TRUE, FALSE), siteColumn = siteColName, 
     .     XColumn = "X", YColumn = "Y", predData = envTable, sppFilter = thisExperiment$data$biologicalData$sppFilter, 
     .     weightType = thisExperiment$data$siteData$weightType, custWeights = customWeights)


#### Step 6: Post-fitting performance review and graphical output:
The following actions allow you to review the quality of the fitted model, and to produce a variety of informative plots.

##### Show summary  
This allows you to see a summary of useful performance data on the screen and optionally
save this information to a text file so you can use it in other documents. The default is a screen-dump only.
To save to a file, give a value to the parameter ‘outFile’.


```R
cmGDM::cm_gdm_summary(myExperiment,
                      outFile = "/home/jovyan/gdm_pkg_data/Experiment_cmGDM_summary.txt")
```


    Error in loadNamespace(i, c(lib.loc, .libPaths()), versionCheck = vI[[i]]): namespace ‘rlang’ 0.4.12 is already loaded, but >= 1.0.0 is required
    Traceback:


    1. loadNamespace(x)

    2. namespaceImportFrom(ns, loadNamespace(j <- i[[1L]], c(lib.loc, 
     .     .libPaths()), versionCheck = vI[[j]]), i[[2L]], from = package)

    3. asNamespace(ns)

    4. loadNamespace(j <- i[[1L]], c(lib.loc, .libPaths()), versionCheck = vI[[j]])

    5. namespaceImport(ns, loadNamespace(i, c(lib.loc, .libPaths()), 
     .     versionCheck = vI[[i]]), from = package)

    6. loadNamespace(i, c(lib.loc, .libPaths()), versionCheck = vI[[i]])

    7. namespaceImport(ns, loadNamespace(i, c(lib.loc, .libPaths()), 
     .     versionCheck = vI[[i]]), from = package)

    8. loadNamespace(i, c(lib.loc, .libPaths()), versionCheck = vI[[i]])

    9. stop(gettextf("namespace %s %s is already loaded, but %s %s is required", 
     .     sQuote(package), current, zop, zversion), domain = NA)


The output from this call is shown below. The model appears to be a very good, with the fraction of deviance explained
just over 80%, and reasonably small intercept value. If variable importance calculation was requested, the summary output will include a table of variable importance values in place of the advisory text. See, for example, worked examples B and C below.


##### Performance plots

A call to the `cmGDM` function *cm_performance_plots()* will generate three plots:
- A plot of observed compositional dissimilarity versus predicted ecological distance.
- A plot of observed compositional dissimilarity versus predicted compositional dissimilarity.
- Plots of the contribution of covariates to the model across the range of each covariate (i.e. the shape of the relationship modelled using spline functions).

This set of informative plots is provided in the basic `gdm` R-package. Here, however, they are plotted using the
graphical tools in the `ggplot2` R-package. Note that this function places these image files in the default experiment
folder within the user’s work area where they can be viewed or downloaded to be included in documents.


```R
cmGDM::cm_performance_plots(myExperiment)
```

The variable contribution plot output by the function includes all variables by default, even those which make zero contribution to the model. When variable importance computations were requested in the call to *cm_run_gdm_experiment()*,
this plot can be ‘de-cluttered’ with the following call:


```R
cmGDM::cm_performance_plots(myExperiment,
                            showVarImp = "nonZero")
```

##### Experiment report

The `cmGDM` package includes a report template which can be used to generate a PDF report of the experiment. Two templates are in fact available to cater for experiments which include variable importance information and those without. The function *cm_gdm_report()* automatically determines which template to use based on information present in the experiment object. For your convenience, this function will generate the performance plots if they are not found in the default experiment folder.

The PDF is saved to the default experiment folder. Reports are not included in this document due to their size and
the fact that they would duplicate the example output already shown here. The reader can easily generate a report for
an experiment by this simple function call:


```R
cmGDM::cm_gdm_report(myExperiment)
```

---
<center>

# B. Species composition example: Presence-absence data and environmental data as GIS layers

</center>

#### Step 1: Create experiment object:
We begin by creating a new experiment. In this example, we will also fit a GDM using the example data set supplied
with the `gdm` R-package, but using only climate data as these are available as GIS raster layers in the `gdm` package.
This will demonstrate additional outputs only possible when GIS data is supplied for environmental covariates. The
name given to this experiment, as well as the description, highlight our use of this restricted set of covariates.


```R
myExperiment2 <- cmGDM::cm_create_new_experiment(userID = "ID123",
                                                userName = "Peter D. Wilson",
                                                experimentName = "Example GDM fit gdm pkg data Climate Only",
                                                description = "Fit GDM using cmGDM applied to example data from the gdm package CLIMATE ONLY")
```


    Error in loadNamespace(i, c(lib.loc, .libPaths()), versionCheck = vI[[i]]): namespace ‘rlang’ 0.4.12 is already loaded, but >= 1.0.0 is required
    Traceback:


    1. loadNamespace(x)

    2. namespaceImportFrom(ns, loadNamespace(j <- i[[1L]], c(lib.loc, 
     .     .libPaths()), versionCheck = vI[[j]]), i[[2L]], from = package)

    3. asNamespace(ns)

    4. loadNamespace(j <- i[[1L]], c(lib.loc, .libPaths()), versionCheck = vI[[j]])

    5. namespaceImport(ns, loadNamespace(i, c(lib.loc, .libPaths()), 
     .     versionCheck = vI[[i]]), from = package)

    6. loadNamespace(i, c(lib.loc, .libPaths()), versionCheck = vI[[i]])

    7. namespaceImport(ns, loadNamespace(i, c(lib.loc, .libPaths()), 
     .     versionCheck = vI[[i]]), from = package)

    8. loadNamespace(i, c(lib.loc, .libPaths()), versionCheck = vI[[i]])

    9. stop(gettextf("namespace %s %s is already loaded, but %s %s is required", 
     .     sQuote(package), current, zop, zversion), domain = NA)


#### Step 2: Load site details:
The same site data table we used in Worked Example A is used here.


```R
myExperiment2 <- cmGDM::cm_load_site_table(myExperiment2,
                                          siteFilename = "/home/jovyan/gdm_pkg_data/gdm_pkg_site_table.csv",
                                          siteCol      = "site",
                                          longitudeCol = "Long",
                                          latitudeCol  = "Lat")
myExperiment2$status
```

#### Step 3: Load biological data:
The same presence-absence coded matrix seen in Worked Example A is loaded for this experiment. As before this file
must be downloaded to a local location and this path must be passed in the parameter ‘bioFilename’.


```R
myExperiment2 <- cmGDM::cm_load_community_data(thisExperiment = myExperiment2,
                                              bioFilename   = "/home/jovyan/gdm_pkg_data/gdm_pkg_PA_table.csv",
                                              dataType      = "Presence_absence",
                                              siteCol       = "site",
                                              dissimMeasure = "Bray-Curtis")
myExperiment2$status
```

#### Step 4: Load environmental covariate data:
For this worked example, we will load the environmental data as a set of GIS raster layers. This will allow us to produce a raster map showing areas with similar predicted community composition using scores from a PCA of transformed
environmental covariates.


```R
myExperiment2 <- cmGDM::cm_load_covar_data(myExperiment2,
                                          src_folder = "/home/jovyan/gdm_pkg_data/env_data/westOZ/",
                                          covar_filenames = c("westOZ_bio5.tif", "westOZ_bio6.tif", 
                                                              "westOZ_bio15.tif", "westOZ_bio18.tif",
                                                              "westOZ_bio19.tif"))
myExperiment2$status
```

#### Step 5: Fit a GDM:
The default fitting of a GDM using the function *cm_run_gdm_experiment()* is to exclude geographical distance and to
not perform variable importance computations. For this example, we will include geographical distance and perform
importance calculations. Computing variable importance information will take considerable computing resources and
run for a long time. If you wish to avoid this burden, you can easily call *cm_run_gdm_experiment()* by removing the
'calc_varImp' parameter from the function call.


```R
myExperiment2 <- cmGDM::cm_run_gdm_experiment(myExperiment2,
                                              includeGeo  = TRUE,
                                              calc_varImp = TRUE)
myExperiment2
```

We can now look at the summary report to see how the model performs with this call:


```R
cmGDM::cm_gdm_summary(myExperiment2)
```

Recall that you can save the summary function output to a text file by passing a file name (with full path) to the
parameter 'outFile' (see example A). The screen output form the summary function looks like this:

Note that the model doesn’t perform quite as well as the the version fitted in Worked Example A: Explained deviance
drops from 80.21% to 70.35%. Bioclim variable 19 (Precipitation of the coldest quarter) makes a huge contribution to
the model output, followed by much weaker contributions from Bioclim variable 5 (Maximum monthly temperature)
and geographical distance between sites.

Performance plots can reveal aspects of model performance not apparent from summary statistics. The three plots can
be generated with this call:


```R
cmGDM::cm_performance_plots(myExperiment2)
```


    Error in loadNamespace(i, c(lib.loc, .libPaths()), versionCheck = vI[[i]]): namespace ‘rlang’ 0.4.12 is already loaded, but >= 1.0.0 is required
    Traceback:


    1. loadNamespace(x)

    2. namespaceImportFrom(ns, loadNamespace(j <- i[[1L]], c(lib.loc, 
     .     .libPaths()), versionCheck = vI[[j]]), i[[2L]], from = package)

    3. asNamespace(ns)

    4. loadNamespace(j <- i[[1L]], c(lib.loc, .libPaths()), versionCheck = vI[[j]])

    5. namespaceImport(ns, loadNamespace(i, c(lib.loc, .libPaths()), 
     .     versionCheck = vI[[i]]), from = package)

    6. loadNamespace(i, c(lib.loc, .libPaths()), versionCheck = vI[[i]])

    7. namespaceImport(ns, loadNamespace(i, c(lib.loc, .libPaths()), 
     .     versionCheck = vI[[i]]), from = package)

    8. loadNamespace(i, c(lib.loc, .libPaths()), versionCheck = vI[[i]])

    9. stop(gettextf("namespace %s %s is already loaded, but %s %s is required", 
     .     sQuote(package), current, zop, zversion), domain = NA)


These outputs are shown in figures below.  
The basic `gdm` package includes a function to produce a map (in image form) showing grid cells which are predicted to
have similar composition on the basis of the combination of environmental covariates in each cell. Grid cells with similar
predicted composition have similar colours. The function *cm_gdm_pcaPlot()* in the `cmGDM` package reproduces this
image output but provides it in two forms:
1. A PNG image which can be used in documents, reports, etc. as any other image. However, `cmGDM` also adds an ancillary “World coordinates” file which allows you to directly import the image as a GIS layer into R (packages `raster` and `terra`, for example) or GIS programs such as QGIS or ArcGIS. This allows you to produce multi-layered GIS outputs.

2. A geoTIFF file which is a stand-alone, industry standard GIS file format. Again, this can be used in R or GIS programs to produce multi-layered maps, and shared with others. NOTE: Files are saved to the default experiment folder.


```R
cmGDM::cm_gdm_pcaPlot(myExperiment2)
```

The basic PNG image output produced by this call is shown below. As mentioned, this file is accompanied by a
“World coordinate” file allowing it to be treated as a GIS layer, and there is a version stored as a GeoTIFF GIS file. An example of downstream use of this GIS resource is provided in Worked Example C.

Finally, a report can be generated with:


```R
cmGDM::cm_gdm_report(myExperiment2)
```

---
<center>
    
# C. Species composition example: Abundance data, richness site weights and environmental data as GIS layers

</center>

#### Step 1: Create experiment object:
For this example, the data is a simulated community table covering 10 sites and simulated samples of 16 virtual species.
The data type represents simulated counts at each site for each species. Four species are given a strong North-South
abundance gradient, four are simulated with a weak North-South gradient and the remainder are random, relatively
low-abundance species.
Sites are located in New South Wales and are spread from the far north coast to the south coast.
We begin, creating a new experiment.


```R
library(cmGDM)
myExperiment3 <- cmGDM::cm_create_new_experiment(userID        = "ID123",
                                                userName       = "Peter D. Wilson",
                                                experimentName = "Example GDM fit to abundance table",
                                                description    = "Fit GDM to simulated abundance-type community table and apply a richness weighting")
```

#### Step 2: Load site details:
Loading site details flows the well-established process but we now specify 'weightType'.


```R
myExperiment3 <- cmGDM::cm_load_site_table(myExperiment3,
                                           siteFilename = "/home/jovyan/gdm_pkg_data/siteData.csv",
                                           siteCol      = "site",
                                           longitudeCol = "longitude",
                                           latitudeCol  = "latitude",
                                           weightType   = "richness")
myExperiment3$status
```

#### Step 3: Load biological data:
Loading the biological data set is routine.


```R
myExperiment3 <- cmGDM::cm_load_community_data(thisExperiment = myExperiment3,
                                              bioFilename   = "/home/jovyan/gdm_pkg_data/abundance_matrix.csv",
                                              siteCol       = 1,
                                              dataType      = "Abundance",
                                              dissimMeasure = "Bray-Curtis")
myExperiment3$status
```

#### Step 4: Load environmental covariate data:
Environmental covariates for the example model fit are from the “eastOZ” data set representing the 19 basic Bioclim
variables covering roughly the eastern third of the Australian continent. This step, as mentioned before, can be a little slow.


```R
myExperiment3 <- cmGDM::cm_load_covar_data(thisExperiment = myExperiment3,
                                          src_folder = "/home/jovyan/gdm_pkg_data/env_data/eastOZ",
                                          covar_filenames = list.files("/home/jovyan/gdm_pkg_data/env_data/eastOZ",
                                                                       "*.tif"))
myExperiment3$status
```

#### Step 5: Fit a GDM:
We can now fit a GDM. In this demonstration, we will keep things simple and fit a basic GDM, forgoing the often
very informative but computationally expensive estimation of variable importance. In comparison, fitting a basic GDM
is exceptionally fast.


```R
myExperiment3 <- cmGDM::cm_run_gdm_experiment(thisExperiment = myExperiment3,
                                             includeGeo = TRUE)
myExperiment3
```

Note that geographical distance was specified for inclusion in the covariates presented to the modelling process.
How good is the model? Let’s begin our examination of model performance by calling the *function cm_gdm_summary()*:


```R
cmGDM::cm_gdm_summary(myExperiment3)
```

We can also generate the three diagnostic plots which are part of the GDM standard outputs:


```R
cmGDM::cm_performance_plots(thisExperiment = myExperiment3)
```

---
<center>
    
# D. Genetic diversity example
    
</center>

This worked example is based on a study of genetic diversity in populations of the Purple Acacia (*Acacia purpureopetala*)
by van der Merwe et al. (2021) who applied GDMs as one approach to understanding the population genetics of a
narrow-range endemic species.  
A growing number of published studies apply GDMs to model the relationship between genetic diversity and environmental
factors. GDMs are well-suited to modelling genetic diversity because they:  
- Are able to flexibly model relationships between diversity and environmental factors;
- Can highlight the relative importance of geographical distance versus environmental factors and therefore provide insight into the most important population genetic processes giving rise to observed diversity. Geographic distance is associated with Isolation By Distance (IBD), while a significant role for environmental covariates suggests some combination of local adaptation and Isolation by Environment (IBE) may be an influence on observed dissimilarity; and,
- Can be used with accepted measures of inter-population and inter-sample genetic diversity or dissimilarity measures.  

As suggested, biological data for these applications of GDMs is in the form of dissimilarity matrices produced by
standardised methods widely used in populations genetics. The most commonly encountered measure is FST which
is naturally scaled between 0 and 1 (recall that this is a key requirement for GDMs). Other potential measures are
described by Jost et al. (2018).  
We will fit a GDM to the FST matrix from van der Merwe et al. (2021) using a diverse ensemble of covariates, but
spanning a narrow geographical extent.

#### Step 1: Create experiment object:
As in the other examples, we begin by creating a new experiment.


```R
myExperiment4 <- cmGDM::cm_create_new_experiment(userID = "ID123",
                                                 userName = "Peter D. Wilson",
                                                 experimentName = "Example GDM fit Fst",
                                                 description = "Use Acacia purpureopetala data as example")
```

#### Step 2: Load site details:
Loading site details follows the well-established process.


```R
myExperiment4 <- cmGDM::cm_load_site_table(myExperiment4,
                                          siteFilename = "/home/jovyan/gdm_pkg_data/siteLocation.csv",
                                          siteCol      = "site",
                                          longitudeCol = "long",
                                          latitudeCol  = "lat")
myExperiment4$status
```

#### Step 3: Load biological data:
Our biological data is a pre-computed dissimilarity matrix storing FST values between pairs of samples from each
location recorded in the site data loaded in Step 2. As before this file must be downloaded to a local location and this
path must be passed in the parameter ‘bioFilename’.


```R
myExperiment4 <- cmGDM::cm_load_community_data(thisExperiment = myExperiment4,
                                              bioFilename     = "/home/jovyan/gdm_pkg_data/Fstonlynewsitenamesonly_PDW.csv",
                                              dataType        = "Dissimilarity",
                                              dissimMeasure   = "Fst")
myExperiment4$status
```

#### Step 4: Load environmental covariate data:
A broad set of environmental covariates will be used in this experiment, and they will be loaded as a set of GIS layers.
When this option is used, `cmGDM` will automatically extract a table of environmental covariate values at each location
found in the previously loaded site table (Step 2 above). As before you should download a local copy of these files and
edit the value passed in 'src_folder' to reference this location on your computer.
This step may take some time to complete when large GIS layers must be manipulated, or there are a large number of
sites/samples.


```R
myExperiment4 <- cmGDM::cm_load_covar_data(myExperiment4,
                                          src_folder      = "/home/jovyan/gdm_pkg_data/env_data/acacia_purp",
                                          covar_filenames = paste0("Ap_bio", stingr::str_pad(as.character(1:19),
                                                                                         side = "left", width = 2, pad = "0"), ".tif"))
myExperiment4$status
```

#### Step 5: Fit a GDM:
With the successful addition of site, biological and environmental data, we now call *cm_run_experiment()* with
'includeGeo = TRUE' and 'calc_varImp = TRUE'. NOTE: Computing variable importance will take a considerable
amount of time.


```R
myExperiment4 <- cmGDM::cm_run_gdm_experiment(myExperiment4,
                                              includeGeo  = TRUE,
                                              calc_varImp = TRUE)
myExperiment4
```

The following steps are OPTIONAL. They provide a range of summary information and plots for a completed experiment
which are useful in evaluating model quality and determining future modelling tasks.
First, we will produce a quick summary by calling the function *cm_gdm_summary()*. This allows you to see a
summary of useful performance data on the screen and optionally save to a text file so you can use the information
in other documents. The default is a screen-dump only. To save to a file, give a value to the parameter 'outFile'.
e.g. *cm_gdm_experiment(myExperiment, outFile = “/follow/this/path/to/Experiment_4_summary.txt”)*


```R
cmGDM::cm_gdm_summary(myExperiment4)
```

The output below indicates that the model is quite good as it has explained a little over 79% of the deviance. Deviance
is a measure of variability in the response variable in a Generalised Linear Model (in this case, FST values between
pairs of population samples.) Site aspect dominates the fitted relationship with reasonable contributions from a subset
of variables including sand content of soil, and a number of rainfall variables. Based on this summary, it would be
helpful to create a new experiment using only those variables making non-zero contributions in the original model.
An alternate approach to model simplification would be to remove covariates showing a high level of correlation with
one or more covariates. This was the approach used by Fitzpatrick et al. (2013) whose data was used in worked
examples A and B.

The `cmGDM` function *cm_performance_plots()* provides additional insight into the quality of the fitted model. This
can be run with a very simple call which plots response curves for all covariates:


```R
cmGDM::cm_performance_plots(myExperiment4)
```

Alternatively, when variable importance calculations have been performed, we can trim the response plots to show only
covariates with importance scores greater than 0. That is, we can call:


```R
cmGDM::cm_performance_plots(myExperiment4,
                            showVarImp = "nonzero")
```

As shown in Worked Example B (above), when covariate data are in the form of GIS layers we can produce a raster
map showing areas with similar predicted compositional dissimilarity. This in achieved by a simple call as follows:


```R
cmGDM::cm_gdm_pcaPlot(myExperiment4)
```

Generating a PDF report for this experiment is very simple:


```R
cmGDM::cm_gdm_report(myExperiment4)
```


### 6. Publication plot:

The use of the GeoTIFF GIS raster file to generate a geo-referenced plot suitable for publication is shown next.
Using GIS functions in R or a standalone GIS program, many enhancements can be made. For example, geographic coordinates can be accurately represented, as well as useful layers such as labelled symbols for sample locations.


```R
library(raster)
library(tmap)
library(sf)

# Make sf point object for the sites in the experiment
siteData <- read.csv("/home/jovyan/gdm_pkg_data/siteLocation.csv",
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
rr <- raster::stack("/home/jovyan/gdm_pkg_data/cmGDM_Example GDM fit Fst_GDM_transformed_PCA")

# Make nice map using the lovely tmap package:
theMap <- tmap::tm_shape(rr) +
          tm_graticules(lines = FALSE, ticks = TRUE) + 
          tm_xlab("Longitude") + tm_ylab("Latitude") +
          tm_rgb(r = 1, g = 2, b = 3, interpolate = FALSE) +
          tm_shape(siteData_sf) + tm_symbols(col = "black", size = 0.5) +
          tm_shape(siteLabels_sf) + tm_text("site", fontface = "bold")
print(theMap)
```

---
# References  

Faraway, J. 2014. Regression with Distance Matrices. Journal of Applied Statistics 41:2342–2357.  

Ferrier, S., G. Manion, J. Elith, and K. Richardson. 2007. Using generalized dissimilarity modelling to analyse and
predict patterns of beta diversity in regional biodiversity assessment. Diversity and Distributions 13:252–264.  

Fitzpatrick, M. C., and S. R. Keller. 2015. Ecological genomics meets community-level modelling of biodiversity:
mapping the genomic landscape of current and future environmental adaptation. Ecology Letters 18:1–16.  

Fitzpatrick, M. C., N. J. Sanders, S. Normand, J.-C. Svenning, S. Ferrier, A. D. Gove, and R. R. Dunn. 2013.  
Environmental and historical imprints on beta diversity: insights from variation in rates of species turnover along
gradients. Proceedings of the Royal Society B: Biological Sciences 280:20131201.  

Fitzpatrick, M. C., and S. R. Keller. 2015. Ecological genomics meets community-level modelling of biodiversity:
mapping the genomic landscape of current and future environmental adaptation. Ecology Letters 18:1–16.  

Jost, L., F. Archer, S. Flanagan, O. Gaggiotti, S. Hoban, and E. Latch. 2018. Differentiation measures for conservation
genetics. Evolutionary Applications 11:1139–1148.  

Lichstein, J. W. 2006. Multiple regression on distance matrices: a multivariate spatial analysis tool. Plant Ecology
188:117–131.  

Mokany, K., C.Ware, S. N. C.Woolley, S. Ferrier, and M. C. Fitzpatrick. 2022. A working guide to harnessing generalized
dissimilarity modelling for biodiversity analysis and conservation assessment. Global Ecology and Biogeography In
press.  

Smouse, P. E., J. C. Long, and R. R. Sokal. 1986. Multiple regression and correlation extensions of the Mantel Test of
matrix correspondence. Systematic Zoology 35:627–632.
---

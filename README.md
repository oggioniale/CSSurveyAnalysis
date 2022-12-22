<!-- [![](https://img.shields.io/badge/doi-10.5281/zenodo.5576813-yellow.svg)](https://doi.org/10.5281/zenodo.5576813)-->
[![License: GPL v3](https://img.shields.io/badge/License-GPL%20v3-blue.svg)](http://www.gnu.org/licenses/gpl-3.0)

Introduction
==================

This repository provides a [document](https://oggioniale.github.io/CSSurveyAnalysis/) about the statistical analysis, performed in R, used in the papers L’Astorina et al., in preparation and Bergami et al., in preparation for exploring environmental Citizen Science practices and scientists' attitudes at ILTER, starting from the results of a global survey. 

In the spirit of open science, open data and reproducible science we share [the dataset](https://doi.org/10.5281/zenodo.7148597) and the statistical analysis.


Credits
-------
The R script is being developed by Alessandro Oggioni ([IREA-CNR](http://www.irea.cnr.it)) and Caterina Bergami ([ISMAR-CNR](http://www.ismar.cnr.it)). It is released under the [GNU General Public License version 3](https://www.gnu.org/licenses/gpl-3.0.html) (GPL‑3).

Please cite this document:

``` bibtex
@misc{oggioniCSI2022,
  author = {Alessandro Oggioni and Caterina Bergami},
  title = {Statistical analysis for exploring environmental Citizen Science practices at ILTER},
  url = {https://oggioniale.github.io/CSSurveyAnalysis/},
  year = {2022},
  doi = {xx.xxxxx/xx-xxx-xxx}
}
```


Use
-------
To reproduce the analysis please download the excel file of [dataset](https://doi.org/10.5281/zenodo.7148597), execute this code for load the dataset
```
dataset <- readxl::read_excel("ILTER_PublicEngagement_forPapers.xlsx")
dataset$age <- as.numeric(format(Sys.Date(), "%Y")) - as.numeric(dataset$Q33)
```
and finally run the different code.


Support contact
---------------
For support, suggestions or comments you can use the [GitHub Issue Tracker](https://github.com/oggioniale/CSSurveyAnalysis/issues).


# Omics BioAnalytics

<p align="center">

<img src="https://github.com/singha53/omics-central-viz/blob/master/public/logo.png" width={400} alt="Omics BioAnalytics" />

</p>

Omics BioAnalytics is a Shiny app that perform common bioinformatics
analyses such as descriptive analyses of demographics variables,
exploratory data analysis, differential expression analysis and
biomarker discovery analysis. The can either use the provided sample
data to run through the various analyses of the app or upload their own
data (demographics variables, omics data).

## Installing

  - assuming R is installed.

<!-- end list -->

``` r
install.packages("devtools")
devtools::install_github("singha53/omicsBioAnalytics")
```

## Get started

``` r
library(omicsBioAnalytics);
omicsBioAnalytics::startApp()
```

## Programming/Scripting Languages

### Web-app

  - RShiny
  - RStudio

### Multi-model app

  - Alexa Skill Kit
  - JavaScript (Node.js, v8.11.2)
  - npm (v6.4.0)

## Sample datasets used for this app

### Heart Failure case study

  - data was download from my previously pubilshe study [Can J
    Cardiol. 2019 Apr;35(4):471-479
    (PMID: 30935638)](https://amritsingh.shinyapps.io/multiomics_HFhospitalizations/)

### Pathway datasets

### How to use?

``` r
library(omicsBioAnalytics);
data(pathwayDB)
```

### How to generate?

[steps to reproduce
pathwayDB](https://github.com/singha53/omicsCentralDatasets/blob/master/inst/extdata/pathwayDB/pathways.md)

## Features

  - analyze multiple omics data obtained on the same set of subjects
    (samples/observations)
  - interactive visualizations (heatmaps, networks, statistical graphs)
  - RShiny app as an R-package

## Room for improvements

  - use of Shiny modules to improve server-side code
  - use of R6Class to improve code

## Contributing

If you’d like to contribute, please fork the repository and use a
feature branch. Pull requests are warmly welcome.

## Links

  - Repository:
    <https://github.com/singha53/https://github.com/singha53/omicsBioAnalytics/>

## Copyright and license

Copyright 2020 AMRITPAL SINGH Inc.

The code in this project is licensed under MIT license.

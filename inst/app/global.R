options(repos=structure(BiocManager::repositories())) ## repository configuration of bioconductor packages

# load libraries
## pakcages for shiny app
suppressPackageStartupMessages(library("shiny"));
suppressPackageStartupMessages(library("shinydashboard"));
suppressPackageStartupMessages(library("shinyBS"));
suppressPackageStartupMessages(library("plotly"));
suppressPackageStartupMessages(library("omicsBioAnalytics")); # devtools::install_github("singha53/omicsBioAnalytics", force = TRUE)
suppressPackageStartupMessages(library("googleVis"));
suppressPackageStartupMessages(library("limma"))
suppressPackageStartupMessages(library("lattice"))
suppressPackageStartupMessages(library("aws.s3"))

# set env vars
Sys.setenv("AWS_ACCESS_KEY_ID" = Sys.getenv("AWS_ACCESS_KEY_ID"),
  "AWS_SECRET_ACCESS_KEY" = Sys.getenv("AWS_SECRET_ACCESS_KEY"),
  "AWS_DEFAULT_REGION" = Sys.getenv("AWS_DEFAULT_REGION"))

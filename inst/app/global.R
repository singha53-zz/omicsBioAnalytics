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
source("makeEnvVars.R")
Sys.setenv("S3BUCKET" = readRDS("S3BUCKET.rds"),
  "AWS_ACCESS_KEY_ID" = readRDS("AWS_ACCESS_KEY_ID.rds"),
  "AWS_SECRET_ACCESS_KEY" = readRDS("AWS_SECRET_ACCESS_KEY.rds"),
  "AWS_DEFAULT_REGION" = readRDS("AWS_DEFAULT_REGION.rds"))

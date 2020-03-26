options(repos=structure(BiocManager::repositories())) ## repository configuration of bioconductor packages

# load libraries
## pakcages for shiny app
suppressPackageStartupMessages(library("shiny"));
suppressPackageStartupMessages(library("shinydashboard"));
suppressPackageStartupMessages(library("shinyBS"));
suppressPackageStartupMessages(library("plotly"));
suppressPackageStartupMessages(library("omicsBioAnalytics")); # devtools::install_github("singha53/omicsBioAnalytics", force = TRUE)
suppressPackageStartupMessages(library("googleVis"));
suppressPackageStartupMessages(library("limma"));
suppressPackageStartupMessages(library("lattice"));
suppressPackageStartupMessages(library("aws.s3"));
suppressPackageStartupMessages(library("canvasXpress"));
suppressPackageStartupMessages(library("enrichR"));

## Import data
data("pathwayDB")

## pathways to use
kegg <- subset(pathwayDB, DB == "KEGG_2019_Human")
kegg <- split(kegg$Genes, as.character(kegg$Pathways))
wikipathways <- subset(pathwayDB, DB == "WikiPathways_2019_Human")
wikipathways <- split(wikipathways$Genes, as.character(wikipathways$Pathways))

# set env vars
source("makeEnvVars.R")
Sys.setenv("S3BUCKET" = readRDS("S3BUCKET.rds"),
  "AWS_ACCESS_KEY_ID" = readRDS("AWS_ACCESS_KEY_ID.rds"),
  "AWS_SECRET_ACCESS_KEY" = readRDS("AWS_SECRET_ACCESS_KEY.rds"),
  "AWS_DEFAULT_REGION" = readRDS("AWS_DEFAULT_REGION.rds"),
  "TABLE_NAME" = readRDS("TABLE_NAME.rds"))

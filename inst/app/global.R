options(repos=structure(BiocManager::repositories())) ## repository configuration of bioconductor packages

# load libraries
## pakcages for shiny app
suppressPackageStartupMessages(library("shiny"));
suppressPackageStartupMessages(library("shinydashboard"));
suppressPackageStartupMessages(library("shinyBS"));
suppressPackageStartupMessages(library("dqshiny"));
suppressPackageStartupMessages(library("plotly"));
suppressPackageStartupMessages(library("omicsBioAnalytics")); # devtools::install_github("singha53/omicsBioAnalytics", force = TRUE)
suppressPackageStartupMessages(library("googleVis"));
suppressPackageStartupMessages(library("limma"));
suppressPackageStartupMessages(library("lattice"));
suppressPackageStartupMessages(library("aws.s3"));
suppressPackageStartupMessages(library("canvasXpress"));
suppressPackageStartupMessages(library("enrichR"));
suppressPackageStartupMessages(library("visNetwork"));
suppressPackageStartupMessages(library("caret"));
suppressPackageStartupMessages(library("glmnet"));
suppressPackageStartupMessages(library("ggrepel"));

## Import data
data("heartFailure")
hf_datasets <- heartFailure$omicsData
hf_datasets$demo <- heartFailure$demo

data("covid19")
data("pathwayDB")

## pathways to use
kegg <- subset(pathwayDB, DB == "KEGG_2019_Human")
kegg <- split(kegg$Genes, as.character(kegg$Pathways))
wikipathways <- subset(pathwayDB, DB == "WikiPathways_2019_Human")
wikipathways <- split(wikipathways$Genes, as.character(wikipathways$Pathways))

# set env vars
if(file.exists("makeEnvVars.R")){
  source("makeEnvVars.R")
  Sys.setenv("S3BUCKET" = readRDS("S3BUCKET.rds"),
  "AWS_ACCESS_KEY_ID" = readRDS("AWS_ACCESS_KEY_ID.rds"),
  "AWS_SECRET_ACCESS_KEY" = readRDS("AWS_SECRET_ACCESS_KEY.rds"),
  "AWS_DEFAULT_REGION" = readRDS("AWS_DEFAULT_REGION.rds"),
  "TABLE_NAME" = readRDS("TABLE_NAME.rds"))
}

## color paletter for groups
group_colors <- c("#999999", "#E69F00", "#56B4E9", "#009E73", "#F0E442", "#0072B2", "#D55E00", "#CC79A7")

# If Alexa Skill is setup already then change the flag below to TRUE
alexa_skill_exists <- FALSE
if(alexa_skill_exists){
  dynamodb_table_name <- Sys.getenv("TABLE_NAME")
  s3_bucket <- Sys.getenv("S3BUCKET")
  previous_workloads <- sapply(get_bucket(bucket = s3_bucket), function(i){
    strsplit(i$Key, "-")[[1]][1]
  })
  user_id <- paste(sample(0:9, 7), collapse = "")
  if(!(user_id %in% previous_workloads)){
    flag <- FALSE
  }
}

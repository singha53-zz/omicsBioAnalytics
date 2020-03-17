#' heartFailure
#'
#' Heart failure datasets: demographics, cells, Holter variables, mRNA transcripts, proteins
#'
#' @docType data
#'
#' @usage data(heartFailure)
#'
#' @format An object of class \code{"list"};
#' \describe{
#' \item{demo}{58 subjects x 29 demographic variables}
#' \item{cells}{58 subjects x 14 cell-types}
#' \item{holter}{58 subjects x 29 Holter variables}
#' \item{mrna}{58 subjects x 5000 mRNA transcripts}
#' \item{proteins}{58 subjects x 65 proteins}
#' }
#'
#' @keywords heart failure, blood, multi-omics, biomarkers
#'
#' @references Singh \emphasis{et al.} Ensembling Electrical and Proteogenomics Biomarkers for Improved Prediction of Cardiac-Related 3-Month Hospitalizations: A Pilot Study. Can J Cardiol. 2019 Apr;35(4):471-479.
#' (\href{https://amritsingh.shinyapps.io/multiomics_HFhospitalizations/}{heart failure datasets})
#'
#'
#' @examples
#' library(omicsBioAnalytics)
#' data(heartFailure)
"heartFailure"

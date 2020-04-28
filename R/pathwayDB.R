#' pathwayDB
#'
#' Various pathway datasets (BioCarta, KEGG, Reactome, WikiPathways)
#'  download from Enrichr and converted to tidy data format.
#'
#' @docType data
#'
#' @usage data(pathwayDB)
#'
#' @format An object of class \code{"data.frame"};
#' \describe{
#' \item{Pathways}{Name of pathway/geneset}
#' \item{Genes}{Gene symbol}
#' \item{DB}{Name of database/annotation}
#' }
#'
#' @keywords datasets
#'
#' @references Chen EY et al. Enrichr: interactive and collaborative HTML5
#'  gene list enrichment analysis tool. BMC Bioinformatics. 2013;128(14).
#' (\href{https://github.com/singha53/omicsBioAnalytics/blob/master/
#' inst/extdata/pathwayDB/pathways.md}{Data preprocessing})
#'
#'
#' @examples
#' library(omicsBioAnalytics)
#' data(pathwayDB)
"pathwayDB"

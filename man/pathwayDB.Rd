% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/pathwayDB.R
\docType{data}
\name{pathwayDB}
\alias{pathwayDB}
\title{pathwayDB}
\format{An object of class \code{"data.frame"};
\describe{
\item{Pathways}{Name of pathway/geneset}
\item{Genes}{Gene symbol}
\item{DB}{Name of database/annotation}
}}
\usage{
data(pathwayDB)
}
\description{
Various pathway datasets (BioCarta, KEGG, Reactome, WikiPathways) download from Enrichr and converted to tidy data format.
}
\examples{
library(omicsBioAnalytics)
data(pathwayDB)
}
\references{
Chen EY et al. Enrichr: interactive and collaborative HTML5 gene list enrichment analysis tool. BMC Bioinformatics. 2013;128(14).
(\href{https://github.com/singha53/omicsBioAnalytics/blob/master/inst/extdata/pathwayDB/pathways.md}{Data preprocessing})
}
\keyword{datasets}

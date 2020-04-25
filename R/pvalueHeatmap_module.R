#' UI of pvalue heatmap for pairwise associations between datasets
#'
#' @param id, character used to specify namespace,
#' see \code{shiny::\link[shiny]{NS}}
#'
#' @return a \code{shiny::\link[shiny]{tagList}} containing UI element
#' @export
pvalue_heatmap_ui <- function(id) {
  ns <- shiny::NS(id)
  plotly::plotlyOutput(ns("pca_clinvar"), width = "100%")
}

#' pvalueHeatmap module server-side processings
#'
#' This module produces the pvalue heatmap
#'
#' @param input,output,session standard \code{shiny} boilerplate
#' @param demo data frame with metadata
#' @param pcs data frame containing principal components
#' @export
pvalue_heatmap_server <- function(input, output, session, demo, pcs) {
  output$pca_clinvar <- plotly::renderPlotly({
    ggplotly(pcaHeatmap(pcs = pcs, demo = demo))
  })
}

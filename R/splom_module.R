#' UI of Scatterplot matrix of Principal components
#'
#' @param id, character used to specify namespace,
#' see \code{shiny::\link[shiny]{NS}}
#'
#' @return a \code{shiny::\link[shiny]{tagList}} containing UI element
#' @export
splom_ui <- function(id) {
  ns <- shiny::NS(id)
  shiny::plotOutput(ns("pca_plot"), width = "100%")
}

#' Scatterplot matrix module server-side processings
#'
#' This module produces the pvalue heatmap
#'
#' @param input,output,session standard \code{shiny} boilerplate
#' @param demo data frame with metadata
#' @param pcs data frame containing principal components
#' @export
splom_server <- function(input, output, session, pcs, response, group_colors) {
  output$pca_plot <- shiny::renderPlot({
    omicsBioAnalytics::pcaPairs(pcs = pcs,
      y = response,
      col = group_colors[1:nlevels(response)]
  )})
}

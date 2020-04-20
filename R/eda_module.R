#' UI of Exploratory Data Analysis (EDA) page
#'
#' @param id, character used to specify namespace, see \code{shiny::\link[shiny]{NS}}
#'
#' @return a \code{shiny::\link[shiny]{tagList}} containing UI element
#' @export
eda_ui <- function(id) {
  ns <- NS(id)

  fluidPage(
  fluidRow(column(
    4,
    sliderInput(
      ns("ncomp"),
      h3("Number of components"),
      min = 2,
      max = 5,
      value = 1
    ),
    h3("PCA component plots", align = "center")
  ), column(8,
    h3("Percentage variation explained", align = "center"),
    verbatimTextOutput(ns("var_exp"))
  )),
  fluidRow(column(4, omicsBioAnalytics::splom_ui(ns("pca_splom"))),
    column(8,
      h3("Which metadata variables are associated with major
      sources of variation in the expression data?", align = "center"),
      omicsBioAnalytics::pvalue_heatmap_ui(ns("pca_pvalue_heatmap"))
    ))
  )
}

#' Reactive values for eda_server module server-side processing
#'
#' @param input, output, session standard \code{shiny}
#'
#' @return list with following components
#' \describe{
#'   \item{ncomp}{reactive number indicating number of PCs}
#' }
#' @export
pca_mod_server <- function(input, output, session) {

  return(
    list(
      ncomp = reactive({ input$ncomp })
    )
  )
}


#' EDA module server-side processings
#'
#' This module produces the EDA panel for a given dataset
#'
#' @param input,output,session standard \code{shiny} boilerplate
#' @param demo data frame with metadata
#' @param dataset data frame containing omic variables
#' @param response factor containing cateogories for the response variable
#' @param vars list of one element ncomp(containing number of PCs)
#' @export
eda_server <- function(input, output, session, demo, dataset, response, group_colors, vars) {
  shiny::observeEvent(vars$ncomp(), {
    pcs <- prcomp(
      x = dataset,
      scale. = TRUE,
      center = TRUE,
      rank. = vars$ncomp()
    )

    output$var_exp <- shiny::renderPrint({
      summary(pcs)})

    shiny::callModule(module = omicsBioAnalytics::splom_server, id = "pca_splom", pcs = pcs$x, response = response, group_colors = group_colors)
    shiny::callModule(module = omicsBioAnalytics::pvalue_heatmap_server, id = "pca_pvalue_heatmap", demo = demo, pcs = pcs$x)
  })
}

#' UI of Exploratory Data Analysis page
#' @export
#' @rdname eda
eda <- function() {
  shiny::fluidRow(shiny::uiOutput("eda"))
}

#' UI of Exploratory Data Analysis (EDA) page
#'
#' @param id, character used to specify namespace,
#' see \code{shiny::\link[shiny]{NS}}
#'
#' @return a \code{shiny::\link[shiny]{tagList}} containing UI element
#' @export
eda_ui <- function(id) {
  ns <- shiny::NS(id)

  shiny::fluidPage(
    shiny::fluidRow(shiny::column(
    4,
      shiny::sliderInput(
      ns("ncomp"),
        shiny::h3("Number of components"),
      min = 2,
      max = 5,
      value = 1
    ),
      shiny::h3("PCA component plots", align = "center")
  ), shiny::column(8,
    shiny::h3("Percentage variation explained", align = "center"),
    shiny::verbatimTextOutput(ns("var_exp"))
  )),
    shiny::fluidRow(shiny::column(4,
      omicsBioAnalytics::splom_ui(ns("pca_splom"))),
      shiny::column(8,
        shiny::h3("Which metadata variables are associated with major
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
eda_ui_vars <- function(input, output, session) {

  return(
    list(
      ncomp = shiny::reactive({
        input$ncomp})
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
#' @param eda_ui_vars list of one element ncomp(containing number of PCs)
#' @export
eda_server <- function(input, output, session, demo, dataset,
  response, group_colors, eda_ui_vars) {
  shiny::observeEvent(eda_ui_vars$ncomp(), {
    pcs <- prcomp(
      x = dataset,
      scale. = TRUE,
      center = TRUE,
      rank. = eda_ui_vars$ncomp()
    )

    output$var_exp <- shiny::renderPrint({
      summary(pcs)})

    shiny::callModule(module = omicsBioAnalytics::splom_server,
      id = "pca_splom", pcs = pcs$x, response = response,
      group_colors = group_colors)
    shiny::callModule(module = omicsBioAnalytics::pvalue_heatmap_server,
      id = "pca_pvalue_heatmap", demo = demo, pcs = pcs$x)
  })
}

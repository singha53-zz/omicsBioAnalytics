#' UI of Dot plot
#'
#' @param id, character used to specify namespace,
#' see \code{shiny::\link[shiny]{NS}}
#'
#' @return a \code{shiny::\link[shiny]{tagList}} containing UI element
#' @export
dotplot_ui <- function(id, panel_names) {
  ns <- shiny::NS(id)
  shiny::fluidRow(align = 'center',
    shiny::column(12,
      shiny::radioButtons(
        inputId = ns("panel_name"),
        label = "Select panel:",
        choices = panel_names,
        selected = panel_names[1],
        inline = TRUE
      ),
      shiny::plotOutput(ns("dotplot"),
        width = "100%",
        click = shiny::hoverOpts(id = ns("dotplot_click"))),
      shiny::actionButton(ns("dotplot_options_button"), "Plot options"),
      shinyBS::bsModal(ns("dotplot_options"), "Plot options", ns("dotplot_options_button"), size = "large",
        shiny::sliderInput(ns("hjust"), "horizontal justification:", min = 0, max = 1, value = 0.5),
        shiny::sliderInput(ns("vjust"), "vertical justification:", min = 0, max = 1, value = 0.5),
        shiny::sliderInput(ns("xAngle"), "x-axis text angle:", min = 0, max = 180, value = 0),
        shiny::sliderInput(ns("xSize"), "x-axis text size:", min = 5, max = 20, value = 7),
        shiny::sliderInput(ns("ySize"), "y-axis text size:", min = 5, max = 20, value = 7)
      )
    )
  )
}


#' Reactive values for dotplot_server module server-side processing
#'
#' @param input, output, session standard \code{shiny}
#'
#' @return list with following components
#' \describe{
#'   \item{xAngle}{reactive number indicating the angle to rotate the x-axis}
#'   \item{hjust}{reactive number between 0 and 1 indicating horizontal justification}
#'   \item{vjust}{reactive number between 0 and 1 indicating vertical justification}
#'   \item{xSize}{reactive number indicating the size of the x-axis label}
#'   \item{ySize}{reactive number indicating the size of the x-axis label}
#' }
#' @export
dotplot_ui_vars <- function(input, output, session) {
  return(
    list(
      panel_name = shiny::reactive({
        input$panel_name
      }),
      xAngle = shiny::reactive({
        input$xAngle
      }),
      hjust = shiny::reactive({
        input$hjust
      }),
      vjust = shiny::reactive({
        input$vjust
      }),
      xSize = shiny::reactive({
        input$xSize
      }),
      ySize = shiny::reactive({
        input$ySize
      }),
      dotplot_click = shiny::reactive({
        input$dotplot_click
      })
    )
  )
}

#' Dot plot module server-side processings
#'
#' This module produces the pvalue heatmap
#'
#' @param input,output,session standard \code{shiny} boilerplate
#' @param demo data frame with metadata
#' @param pcs data frame containing principal components
#' @export
dotplot_server <- function(input, output, session, data, dotplot_ui_vars) {

  output$dotplot <- shiny::renderPlot({
    filter(data, panel == dotplot_ui_vars$panel_name()) %>%
    ggplot2::ggplot(aes(x = features, y = coef, color = panel)) + geom_point(size = 5) +
    ggplot2::geom_segment(aes(xend = features, color = panel), yend = 0, size = 1) +
    ggplot2::ylab("Variable importance") +
    ggplot2::theme_classic() +
    ggplot2::theme(legend.position = "none") +
    ggplot2::theme(axis.text.x = ggplot2::element_text(angle = dotplot_ui_vars$xAngle(),
      hjust = dotplot_ui_vars$hjust(),
      vjust = dotplot_ui_vars$vjust(),
      size = dotplot_ui_vars$xSize()),
      axis.text.y = ggplot2::element_text(size = dotplot_ui_vars$ySize()))
  })

}

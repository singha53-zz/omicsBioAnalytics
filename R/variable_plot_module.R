#' UI of Variable plot
#'
#' @param id, character used to specify namespace,
#' see \code{shiny::\link[shiny]{NS}}
#'
#' @return a \code{shiny::\link[shiny]{tagList}} containing UI element
#' @export
variable_plot_ui <- function(id) {
  ns <- shiny::NS(id)
  shiny::fluidRow(align = 'center',
    shiny::column(12,
      plotly::plotlyOutput(ns("variable_plot"), width = "100%"),
      shiny::actionButton(ns("variable_options_button"), "Plot options"),
      shinyBS::bsModal(ns("variable_options"), "Plot options", ns("variable_options_button"), size = "large",
        shiny::sliderInput(ns("hjust"), "horizontal justification:", min = 0, max = 1, value = 0.5),
        shiny::sliderInput(ns("vjust"), "vertical justification:", min = 0, max = 1, value = 0.5),
        shiny::sliderInput(ns("xAngle"), "x-axis text angle:", min = 0, max = 180, value = 0),
        shiny::sliderInput(ns("xSize"), "x-axis text size:", min = 5, max = 20, value = 7),
        shiny::sliderInput(ns("ySize"), "y-axis text size:", min = 5, max = 20, value = 7)
      )
    )
  )
}


#' Reactive values for variable_plot_server module server-side processing
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
variable_plot_ui_vars <- function(input, output, session) {
  return(
    list(
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
      })
    )
  )
}

#' Variable plot module server-side processings
#'
#' This module produces the pvalue heatmap
#'
#' @param input,output,session standard \code{shiny} boilerplate
#' @param response vector of response
#' @param response_var data frame containing principal components
#' @export
variable_plot_server <- function(input, output, session, response, response_var, datasets, selected_variable, group_colors, variable_plot_ui_vars) {

  print("inside variable_plot_server function")
  print(names(datasets))
  print(selected_variable$panel)
  print(selected_variable$feature)
  exp <- datasets[[selected_variable$panel]][, selected_variable$feature]
  print(exp)

  output$variable_plot <- plotly::renderPlotly({
    if (length(exp) > 0 & length(selected_variable$feature) > 0) {
      print("exp")
      print(exp)
      print(selected_variable$feature)
      ggplotly(data.frame(x = response, y = exp) %>%
          ggplot(aes(x = x, y = y, fill = x)) +
          geom_violin(trim = FALSE) +
          xlab(response_var) +
          ylab(selected_variable$feature) +
          geom_jitter(shape = 16, position = position_jitter(0.2)) +
          ggtitle(paste(selected_variable$feature, " vs. ", response_var)) +
          theme_classic() +
          theme(legend.position = "none") +
          scale_fill_manual(values = group_colors[1:length(unique(response))]) +
          theme(axis.text.x = element_text(angle = variable_plot_ui_vars$xAngle(),
            hjust = variable_plot_ui_vars$hjust(),
            vjust = variable_plot_ui_vars$vjust(),
            size = variable_plot_ui_vars$xSize()),
            axis.text.y = element_text(size = variable_plot_ui_vars$ySize()))
      )
    } else {
      omicsBioAnalytics::empty_plot("Please select a point from the dot plots below.")
    }
  })

}

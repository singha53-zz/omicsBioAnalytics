#' @export
#' @rdname reportUI
report_ui <- function() {
  shiny::sidebarLayout(
    shiny::sidebarPanel(
      shiny::textInput("title", "Title", value = "",
        placeholder = "Title of report..."),
      shiny::textInput("author", "Author", value = "",
        placeholder = "Author..."),
      shiny::textInput("aff", "Affiliation", value = "",
        placeholder = "Affiliation..."),
      shiny::selectInput(
              inputId = "figs",
              label = "Figures:",
              choices = c("None", c("pca", "volcano_mrna")),
              size = 10, selectize = FALSE,
              selected = "None"
            ),
      shiny::fileInput("cFig", "Upload custom figure"),
      shiny::downloadButton("report", "Generate report")
    ),
    shiny::mainPanel(
      shiny::h1("Add section"),
      shiny::imageOutput("myImage"),
      shiny::div(
        shiny::h4("Text"),
        shiny::tags$textarea(
          "**Markdown** syntax is allowed.",
          id = "markdowninput",
          rows = 3,
          style = "width:80%;")),
      shiny::actionButton("h1Btn", "Insert section",
        color = "primary", style = "height: 20%"),
      shiny::uiOutput("dragAndDrop")
    )
  )
}

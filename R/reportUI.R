#' @export
#' @rdname reportUI
reportUI <- function() {
  sidebarLayout(
    sidebarPanel(
      textInput("title", "Title", value = "", placeholder = "Title of report..."),
            textInput("author", "Author", value = "", placeholder = "Author..."),
            textInput("aff", "Affiliation", value = "", placeholder = "Affiliation..."),
            selectInput(
              inputId = "figs",
              label = "Figures:",
              choices = c("None", c("pca", "volcano_mrna")),
              size = 10,selectize = FALSE,
              selected = "None"
            ),
            fileInput("cFig", "Upload custom figure"),
            downloadButton("report", "Generate report")
    ),
    mainPanel(
      h1("Add section"),
      imageOutput("myImage"),
      div(
        h4("Text"),
        tags$textarea(
          "**Markdown** syntax is allowed.",
          id    = 'markdowninput',
          rows  = 3,
          style = 'width:80%;')),
      actionButton("h1Btn", "Insert section", color = "primary", style = "height: 20%"),
      uiOutput("dragAndDrop")
    )
  )
}

#
# fluidRow(sidebarLayout(
#   sidebarPanel(
#     fluidRow(
#       textInput("title", "Title", value = "", placeholder = "Title of manuscript..."),
#       textInput("author", "Author", value = "", placeholder = "first author..."),
#       textInput("aff", "Affiliation", value = "", placeholder = "first affiliation..."),
#       selectInput(
#         inputId = "figs",
#         label = "Figures:",
#         choices = c("None", c("pca", "volcano_mrna")),
#         size = 10,selectize = FALSE,
#         selected = "None"
#       ),
#       fileInput("cFig", "Upload custom figure"),
#       downloadButton("report", "Generate report"),
#       imageOutput("myImage")
#     )
#   ),
#   mainPanel(
#     fluidRow(
#       h1("Add section"),
#       div(
#         h4("Text"),
#         tags$textarea(
#           "Please using any **markdown** syntax!",
#           id    = 'markdowninput',
#           rows  = 3,
#           style = 'width:80%;')),
#       actionButton("h1Btn", "Insert section", color = "primary", style = "height: 20%")),
#     uiOutput("tbl")
#   )
# ))

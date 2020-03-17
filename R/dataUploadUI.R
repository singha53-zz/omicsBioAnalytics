#' @export
#' @rdname uiElements
dataUpload <- function() {
  fluidRow(
    fluidRow(
      column(6,
        h4("File specifications")),
      column(6, radioButtons("sep", "Separator",
        choices = c(Comma = ",", Tab = "\t"), selected = ","))),
    fluidRow(
      column(6, fileInput(inputId = "demo", label = "Demographics data")),
        column(6, uiOutput('responseVar'))),
    fluidRow(
      column(6, fileInput("omicsData",
        label="Omics data (1 or more csv files)",
        multiple = TRUE)),
      column(6,
        uiOutput("dataGenSym")
        )
      ),
    actionButton("run", "Run Analysis", icon = icon("play"))
  )
}

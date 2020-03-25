#' @export
#' @rdname uiElements
dataUpload <- function() {
  fluidRow(
    fluidRow(
      column(12,
        h2("Web-based analytics"),
        radioButtons("sep", "file type:",
          choices = c(Comma = ",", Tab = "\t"), selected = ",")),
      column(6, fileInput(inputId = "demo", label = "Demographics data")),
      column(6, uiOutput('responseVar'))),
    fluidRow(
      column(6, fileInput("omicsData",
        label="Omics data (1 or more csv files)",
        multiple = TRUE),
        actionButton("run", "Run Analysis", icon = icon("play"),
          style="color: #fff; background-color: #337ab7; border-color: #2e6da4"))),
    fluidRow(
      column(12,
        h2("Voice-enabled analytics"),
        actionButton("alexa", "Alexa, analyze my data!", icon = icon("user")),
        bsModal("modal", "Data Table", "alexa", size = "large", textOutput("msg"))))
    # fluidRow(
    #   column(12,
    #     h2("Voice-enabled analytics"),
    #     textInput("s3workload", "Enter a 5-7 digit number for Alexa to remember this analysis...", "e.g. 12345"),
    #     tags$style(type='text/css', '#msg {color: red;}'),
    #     textOutput("msg"),
    #     actionButton("alexa", "Alexa, analyze my data!", icon = icon("upload"),
    #       style="color: #fff; background-color: #337ab7; border-color: #2e6da4")
    #     ))
  )
}

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
      column(6, uiOutput('responseVar')),
      column(6, uiOutput('refVar'))
      ),
    fluidRow(
      column(6, fileInput("omicsData",
        label="Omics data (1 or more csv files)",
        multiple = TRUE)),
      column(6,
        ""
        )),
     fluidRow(
         "OR Try these examples datasets:",
         column(6,
           actionButton("run", "Run Analysis", icon = icon("play"),
             style="color: #fff; background-color: #337ab7; border-color: #2e6da4")
         ),
         column(6,
           customDownloadButton("heartFailure", label = "Heart Failure", icon = icon("heart")),
           customDownloadButton("covid19", label = "COVID-19", icon = icon("chart-line"))
         )),
    fluidRow(
      column(12,
        h2("Voice-enabled analytics"),
        actionButton("alexa", "Alexa, analyze my data!", icon = icon("user"),
          style="color: #fff; background-color: #337ab7; border-color: #2e6da4"),
        bsModal("modal", "Omics BioAnalytics Alexa Skill", "alexa", size = "large", textOutput("msg"))),
      uiOutput("errMsgAlexa")
      )
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


#' https://stackoverflow.com/questions/49350509/adding-removing-icon-in-downloadbutton-and-fileinput
customDownloadButton <- function(outputId, label = "Download", icon = icon("download")){
  tags$a(id = outputId, class = "btn btn-default shiny-download-link", href = "",
    target = "_blank", download = NA, icon, label)
}

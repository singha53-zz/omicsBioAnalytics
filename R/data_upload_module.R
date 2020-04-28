#' UI of Data Upload page
#'
#' @param id, character used to specify namespace,
#' see \code{shiny::\link[shiny]{NS}}
#'
#' @return a \code{shiny::\link[shiny]{tagList}} containing UI element
#' @export
data_upload_ui <- function(id) {
  ns <- shiny::NS(id)

  shiny::fluidPage(
    shiny::fluidRow(
      shiny::column(12,
        shiny::h2("Web-based analytics"),
        shiny::radioButtons(ns("sep"), "file type:",
          choices = c(Comma = ",", Tab = "\t"), selected = ",")),
      shiny::column(6, shiny::fileInput(inputId = ns("demo"),
        label = "Metadata")),
      shiny::column(6, shiny::uiOutput(ns("response_var"))),
      shiny::column(6, shiny::uiOutput(ns("ref_var")))
    ),
    shiny::fluidRow(
      shiny::column(6, shiny::fileInput(ns("omics_data"),
        label = "Omics data (1 or more csv files)",
        multiple = TRUE))),
    shiny::fluidRow(
      shiny::column(6,
        shiny::actionButton(ns("run"),
          "Run Analysis",
          icon = shiny::icon("play"),
          style = "color: #fff; background-color:
          #337ab7; border-color: #2e6da4"),
        shiny::uiOutput(ns("uploadErrorMsg"))
      ),
      shiny::column(6,
        shiny::h3("Or try these example datasets:", align = "left"),
        shiny::fluidRow(shiny::column(6,
          customDownloadButton(ns("heart_failure"),
            label = "Heart Failure", icon = shiny::icon("heart"))),
          shiny::column(6, customDownloadButton(ns("covid19"),
            label = "COVID-19", icon = shiny::icon("chart-line")))),
        shiny::fluidRow(shiny::column(6, "read paper: ",
          shiny::a("CJC 2019",
            href = "https://www.ncbi.nlm.nih.gov/pubmed/30935638")),
          shiny::column(6, "read paper: ",
            shiny::a("bioRxiv",
              href = "https://www.biorxiv.org/content/10.1101/
              2020.03.24.004655v1"))),
        shiny::fluidRow(shiny::column(6,
          shiny::a("watch demo",
            href = "https://www.youtube.com/watch?v=u1zLL4uXZi8")),
          shiny::column(6, shiny::a("watch demo",
            href = "https://www.youtube.com/watch?v=oglZDscpbAU"))))
    ),
    shiny::fluidRow(
      shiny::column(12,
        shiny::h2("Voice-enabled analytics"),
        shiny::actionButton(ns("alexa"),
          "Alexa, analyze my data!",
          icon = shiny::icon("user"),
          style = "color: #fff; background-color:
          #337ab7; border-color: #2e6da4"),
        shinyBS::bsModal(ns("modal"),
          "Omics BioAnalytics Alexa Skill",
          ns("alexa"), size = "large",
          shiny::textOutput("msg"))),
      shiny::uiOutput(ns("errMsgAlexa"))
    )
  )
}

#' Reactive values for data_upload_server module server-side processing
#'
#' @param input, output, session standard \code{shiny}
#'
#' @return list with following components
#' \describe{
#'   \item{response_var}{reactive vector of categories
#'   for the response variable}
#' }
#' @export
data_upload_ui_vars <- function(input, output, session) {

  return(
    list(
      sep = shiny::reactive({
        input$sep}),
      demo = shiny::reactive({
        input$demo}),
      response_var = shiny::reactive({
        input$response_var}),
      ref_var = shiny::reactive({
        input$ref_var}),
      omics_data = shiny::reactive({
        input$omics_data}),
      run = shiny::reactive({
        input$run}),
      heart_failure = shiny::reactive({
        input$heart_failure}),
      covid19 = shiny::reactive({
        input$covid19}),
      alexa = shiny::reactive({
        input$alexa}),
      modal = shiny::reactive({
        input$modal})
    )
  )
}

#' Data Upload module server-side processings
#'
#' This module produces the Data Upload panel for a given dataset
#'
#' @param input,output,session standard \code{shiny} boilerplate
#' @param demo data frame with metadata
#' @param dataset data frame containing omic variables
#' @param response factor containing cateogories for the response variable
#' @param metadata_vars list of reactive vars (cont_var, transform)
#' @param cont_var character selected continuous variable
#' @param transform boolean TRUE: log2-transformation
#' @export
data_upload_server <- function(input, output, session,
  heart_failure_data, covid19_data, data_upload_ui_vars) {
  ns <- session$ns

  output$uploadErrorMsg = renderUI({
    validate(
      need(data_upload_ui_vars$demo(), "Metadata is required with at least 1 categorical variable!"),
      need(data_upload_ui_vars$omics_data(), "At least one omics data is required!"),
      need(data_upload_ui_vars$response_var(), "A response variable is required!")
      # need(length(unique(data_upload_server_vars$get_demo_data()[, data_upload_ui_vars$response_var()])) > 1, "At least 2 categories required!")
    )
  })

  # Demographics data upload
  get_demo_data <- shiny::reactive({
    req(data_upload_ui_vars$demo())
    demo_data <- read.table(data_upload_ui_vars$demo()$datapath,
      header = TRUE, sep = data_upload_ui_vars$sep())
    demo_data
  })

  # omics data upload
  get_omics_data <- shiny::reactive({
    req(data_upload_ui_vars$omics_data())
    omics_data <- lapply(data_upload_ui_vars$omics_data()$datapath, read.table,
      header = TRUE,
      sep = data_upload_ui_vars$sep())
    names(omics_data) <- gsub(".csv|.tsv|.txt", "",
      data_upload_ui_vars$omics_data()$name)
    omics_data
  })


  # show column names of demo dataset
  output$response_var <- shiny::renderUI({
    shiny::req(get_demo_data())
    keep_cols <- apply(get_demo_data(), 2, function(i) {
      ifelse(length(table(as.character(i))) < 9 &
          min(table(as.character(i))) > 1, TRUE, FALSE)
    })
    shiny::selectInput(ns("response_var"),
      "Select response variable",
      colnames(get_demo_data()[, keep_cols]))
  })

  output$ref_var <- shiny::renderUI({
    shiny::selectInput(ns("ref_var"),
      "Select reference level",
      unique(get_demo_data()[, data_upload_ui_vars$response_var()]))
  })

  response <- shiny::reactive({
    relevel(factor(as.character(get_demo_data()[,
      data_upload_ui_vars$response_var()])),
    ref = data_upload_ui_vars$ref_var())})

  # determine which datasets to perform gene set enrichment analysis on?
  perform_pathway_analysis <- shiny::reactive({
    dataset_names <- sapply(names(get_omics_data()), function(i){
      length(intersect(colnames(get_omics_data()[[i]]), unlist(kegg))) > 5
    })
    names(dataset_names)[dataset_names]
  })

  # if user wants to analyze the example heart failure data
  output$heart_failure <- shiny::downloadHandler(
    filename = "heartFailureDatasets_omicsBioAnalytics.zip",
    content = function(file) {
      files <- NULL;

      # loop through the sheets
      for (i in 1:length(heart_failure_data)) {
        #write each sheet to a csv file, save the name
        file_name <- paste0(names(heart_failure_data)[i], ".txt")
        write.table(heart_failure_data[[i]],
          file_name, sep = "\t",
          row.names = FALSE)
        files <- c(file_name, files)
      }
      # create the zip file
      zip(file, files)
    }
  )

  # if user wants to analyze the example COVID-19 data
  output$covid19 <- shiny::downloadHandler(
    filename = "COVID19Datasets_omicsBioAnalytics.zip",
    content = function(file) {
      files <- NULL;

      #loop through the sheets
      for (i in 1:length(covid19_data)) {
        #write each sheet to a txt file, save the name
        file_name <- paste0(names(covid19_data)[i], ".txt")
        write.table(covid19_data[[i]], file_name, sep = "\t", row.names = FALSE)
        files <- c(file_name, files)
      }
      #create the zip file
      zip(file, files)
    }
  )

  return(list(get_demo_data = get_demo_data,
    response = response,
    get_omics_data = get_omics_data,
    perform_pathway_analysis = perform_pathway_analysis))
}

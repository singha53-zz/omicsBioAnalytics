#' UI of Generate Report page
#'
#' @param id, character used to specify namespace,
#' see \code{shiny::\link[shiny]{NS}}
#'
#' @return a \code{shiny::\link[shiny]{tagList}} containing UI element
#' @export
report_ui <- function(id) {
  ns <- shiny::NS(id)

  shiny::fluidPage(
    shiny::sidebarLayout(
      shiny::sidebarPanel(
        shiny::textInput(ns("title"), "Title", value = "",
          placeholder = "Title of report..."),
        shiny::textInput(ns("author"), "Author", value = "",
          placeholder = "Author..."),
        shiny::textInput(ns("aff"), "Affiliation", value = "",
          placeholder = "Affiliation..."),
        shiny::selectInput(
          inputId = ns("figs"),
          label = "Figures:",
          choices = c("None", c("pca", "volcano_mrna")),
          size = 10, selectize = FALSE,
          selected = "None"
        ),
        shiny::fileInput(ns("cFig"), "Upload custom figure"),
        shiny::downloadButton(ns("report"), "Generate report")
      ),
      shiny::mainPanel(
        shiny::h1("Add section"),
        shiny::imageOutput("myImage"),
        shiny::div(
          shiny::h4("Text"),
          shiny::tags$textarea(
            "**Markdown** syntax is allowed.",
            id = ns("markdowninput"),
            rows = 3,
            style = "width:80%;")),
        shiny::actionButton(ns("h1Btn"), "Insert section",
          color = "primary", style = "height: 20%"),
        shiny::uiOutput(ns("dragAndDrop"))
      )
    )
  )
}

#' Reactive values for report_server module server-side processing
#'
#' @param input, output, session standard \code{shiny}
#'
#' @return list with following components
#' \describe{
#'   \item{response_var}{reactive vector of categories
#'   for the response variable}
#' }
#' @export
report_ui_vars <- function(input, output, session) {

  return(
    list(
      title = shiny::reactive({
        input$title
      }),
      author = shiny::reactive({
        input$author
      }),
      aff = shiny::reactive({
        input$aff
      }),
      figs = shiny::reactive({
        input$figs
      }),
      cFig = shiny::reactive({
        input$cFig
      }),
      report = shiny::reactive({
        input$report
      }),
      markdowninput = shiny::reactive({
        input$markdowninput
      }),
      h1Btn = shiny::reactive({
        input$h1Btn
      })
    )
  )
}

#' Data Upload module server-side processings
#'
#' This module produces the Data Upload panel for a given dataset
#'
#' @param input,output,session standard \code{shiny} boilerplate
#' @param report_ui_vars list of reactive vars ()
#' @export
report_server <- function(input, output, session, report_ui_vars) {
  ns <- session$ns

  shiny::observe({
    shiny::updateSelectInput(session,
      inputId = ns("figs"),
      label = "Figures:",
      choices = c("None", gsub(".png", "", grep(".png", list.files(tempdir()), value = TRUE))),
      selected = "None"
    )
  })

  shiny::observeEvent(report_ui_vars$cFig(), {
    inFile <- report_ui_vars$cFig()
    if (is.null(inFile))
      return()
    file.copy(inFile$datapath, file.path(tempdir(), inFile$name) )
    shiny::updateSelectInput(session,
      inputId = ns("figs"),
      label = "Figures:",
      choices = c("None", gsub(".png", "", grep(".png", list.files(tempdir()), value = TRUE))),
      selected = "None"
    )
  })

  list_of_elements <- list()
  tracker <- shiny::reactiveValues(section = list(), fig = list())

  shiny::observeEvent(report_ui_vars$h1Btn(), {
    nr <- report_ui_vars$h1Btn()
    print(paste0("add item: ", nr))
    id <- ns(paste0("input", report_ui_vars$h1Btn()))

    tracker$section[[id]]$txt <- report_ui_vars$markdowninput()
    tracker$section[[id]]$fig <- report_ui_vars$figs()
    element <- shiny::div(style = "display: flex; justify-content: space-between; align-items: center; width: 100%",
      id = ns(paste0("newInput", nr)),
      report_ui_vars$markdowninput(),
      shiny::br(),
      ifelse(report_ui_vars$figs() == "None", "", paste("Attached Fig: ", report_ui_vars$figs())),
      shiny::actionButton(ns(paste0('removeBtn',nr)), 'Remove')
    )
    list_of_elements[[id]] <<- element

    # Render manuscript section on UI
    output$dragAndDrop = shiny::renderUI({
      shiny::fluidRow(
        shiny::column(
          width = 12,
          sortable::rank_list(
            text = "Drag the items in any desired order",
            labels = list_of_elements,
            input_id = ns("rank_list_1")
          ),
          shiny::verbatimTextOutput(ns("results"))
        )
      )
    })

    shiny::observeEvent(input[[paste0('removeBtn', nr)]],{
      print(paste0("delete item: ", nr))
      shiny::removeUI(
        selector = paste0("#", ns(paste0("newInput", nr)))
      )
      list_of_elements <<- list_of_elements[names(list_of_elements) != ns(paste0("input", nr))]

      output$tbl = shiny::renderUI({
        shiny::fluidRow(
          shiny::column(
            width = 12,
            sortable::rank_list(
              text = "Drag the items in any desired order",
              labels = list_of_elements,
              input_id = ns("rank_list_1")
            )
          )
        )
      })

      print(list_of_elements)
    })

    print(list_of_elements)
    print("---------------")

  })

  ## drag and drop is fixed
  shiny::observe({
    list_of_elements <<- list_of_elements[input$rank_list_1]
  })

  output$report <- shiny::downloadHandler(
    # For PDF output, change this to "report.pdf"
    filename = "report.doc",
    content = function(file) {
      # Copy the report file to a temporary directory before processing it, in
      # case we don't have write permissions to the current working dir (which
      # can happen when deployed).
      tempReport <- file.path(tempdir(), "report.Rmd")
      file.copy("report.Rmd", tempReport, overwrite = TRUE)
      file.copy("HFdiagnosis.bib", file.path(tempdir(), "HFdiagnosis.bib"), overwrite = TRUE)

      # Set up parameters to pass to Rmd document
      params <- list(title = report_ui_vars$title(),
        author = report_ui_vars$author(),
        affiliation = report_ui_vars$aff(),
        section = tracker$section,
        ord = names(list_of_elements))

      # Knit the document, passing in the `params` list, and eval it in a
      # child of the global environment (this isolates the code in the document
      # from the code in this app).
      shiny::withProgress(message = 'Download in progress',
        detail = 'This may take a while...', value = 0, {
          rmarkdown::render(tempReport, output_file = file,
            params = params,
            envir = new.env(parent = globalenv())
          )
        })
    }
  )

  # A temp file to save the output.
  # This file will be removed later by renderImage
  # outfile <- tempfile(fileext = '.png')

  output$myImage <- shiny::renderImage({

    # Return a list containing the filename
    list(src = paste0(tempdir(), "/", report_ui_vars$figs(), ".png"),
      contentType = 'image/png',
      width = 400,
      height = 300,
      alt = "No image was selected from side panel.")
  }, deleteFile = FALSE)
}

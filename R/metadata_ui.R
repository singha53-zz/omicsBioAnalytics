#' @export
#' @rdname uiElements
metadata <- function() {
  shiny::fluidRow(shiny::tabsetPanel(
    shiny::tabPanel(
      "Continuous variables",
      shiny::radioButtons(
        "vars",
        "Choose from one of the following variables:",
        "",
        inline = TRUE
      ),
      shiny::fluidRow(shiny::column(6, shiny::radioButtons(
        "transform",
        "Should the variable be log transformed?",
        c("No" = "no", "Yes" = "yes"),
        inline = TRUE
      )), shiny::column(6, shiny::radioButtons(
        "test",
        "Which test should be applied?",
        c("Linear regression" = "lr", "Kruskal-Wallis Test" = "ks"),
        inline = TRUE
      ))),
      shiny::fluidRow(
        shiny::column(6,
          shiny::h3(""),
          plotly::plotlyOutput('plot')
        ),
        shiny::column(
          6,
          shiny::h3("Descriptive statistics"),
          shiny::tableOutput("descriptiveStat"),
          shiny::h3(shiny::textOutput("testTitle")),
          shiny::verbatimTextOutput("test"),
          shiny::fluidRow(shiny::column(6, shiny::h6(shiny::textOutput("lmAssumptions"))),
            shiny::column(6, shiny::actionButton("lr",
              "linear regression assumptions", icon = shiny::icon("table")),
              shinyBS::bsModal("hypothesisTests",
                "Assessment of linear regression assumptions.", "lr", size = "large",
                DT::dataTableOutput("tbl")))),
          shiny::h3("Conclusion"),
          shiny::tags$style(type = "text/css", "#conclusion {color: red;}"),
          shiny::textOutput("conclusion")
        ))
    ),
    shiny::tabPanel("Categorical variables",
      shiny::fluidPage(
        shiny::column(6,
          shiny::radioButtons(
            "catVar",
            "Choose from one of the following variables:",
            "",
            inline = TRUE
          )),
        shiny::column(6,
          shiny::h3(shiny::textOutput("chisqTitle")),
          shiny::br(),
          shiny::fluidRow(shiny::column(6, shiny::h4("Observed counts"),
            shiny::uiOutput("obsCounts")),
            shiny::column(6, shiny::h4("Observed frequency"),
              shiny::uiOutput("obsFreq"))),
          shiny::br(),
          shiny::h4("Chi-square test"),
          shiny::verbatimTextOutput("chisqTest"),
          shiny::br(),
          shiny::tags$style(type = "text/css",
            "#chisqConclusion {color: red;}"),
          shiny::textOutput("chisqConclusion"))
      )
    )
  ))
}

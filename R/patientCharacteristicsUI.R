#' @export
#' @rdname uiElements
patientCharacteristics <- function() {
  fluidRow(tabsetPanel(
    tabPanel(
      "Continuous variables",
      radioButtons(
        "vars",
        "Demographic variable:",
        "",
        inline = TRUE
      ),
      fluidRow(column(6, radioButtons(
        "transform",
        "Should the clinical variable be log transformed?",
        c("No" = "no", "Yes" = "yes"),
        inline = TRUE
      )), column(6, radioButtons(
        "test",
        "Which test should be applied?",
        c("Linear regression" = "lr", "Kruskal-Wallis Test" = "ks"),
        inline = TRUE
      ))),
      fluidRow(
        column(6,
          h3(""),
          plotlyOutput('plot')
        ),
        column(
          6,
          h3("Descriptive statistics"),
          tableOutput("descriptiveStat"),
          h3(textOutput("testTitle")),
          verbatimTextOutput("test"),
          fluidRow(column(6, h6(textOutput("lmAssumptions"))),
            column(6, actionButton("lr", "linear regression assumptions", icon = icon("table")),
              bsModal("hypothesisTests", "Assessment of linear regression assumptions.", "lr", size = "large",
                DT::dataTableOutput("tbl")))),
          h3("Conclusion"),
          tags$style(type='text/css', '#conclusion {color: red;}'),
          textOutput("conclusion")
        ))
    ),
    tabPanel("Categorical variables",
      fluidPage(
        column(6,
          radioButtons(
            "catVar",
            "categorical variable:",
            "",
            inline = TRUE
          )),
        column(6,
          h3(textOutput("chisqTitle")),
          br(),
          fluidRow(column(6, h4("Observed counts"), uiOutput("obsCounts")),
            column(6, h4("Observed frequency"), uiOutput("obsFreq"))),
          br(),
          h4("Chi-square test"),
          verbatimTextOutput("chisqTest"),
          br(),
          tags$style(type='text/css', '#chisqConclusion {color: red;}'),
          textOutput("chisqConclusion"))
      )
    )
  ))
}

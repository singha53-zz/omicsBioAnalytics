
# Header
header <- shinydashboard::dashboardHeader(
  title = "Omics BioAnalytics"
)

# Sidebar
sidebar <- shinydashboard::dashboardSidebar(
  conditionalPanel(
    condition = "output.analysisRan == false",
    shinydashboard::sidebarMenu(
      id = "tabs",
      shinydashboard::menuItem("Overview",
        tabName = "overview", icon = icon("dashboard")),
      shinydashboard::menuItem(
        "Data upload",
        tabName = "data",
        icon = icon("database")
      ),
      shinydashboard::menuItem(
        "Methods",
        tabName = "methods",
        icon = icon("flask")
      ),
      shinydashboard::menuItem(
        "Analysis",
        tabName = "analysis",
        icon = icon("bar-chart-o")
      ),
      shinydashboard::menuItem(
        "Generate Report",
        tabName = "report",
        icon = icon("clipboard-list")
      )
    )
  ),
  conditionalPanel(
    condition = "output.analysisRan == true",
    shinydashboard::sidebarMenu(
      id = "tabs",
      shinydashboard::menuItem("Overview",
        tabName = "overview", icon = icon("dashboard")),
      shinydashboard::menuItem(
        "Data upload",
        tabName = "data",
        icon = icon("database")
      ),
      shinydashboard::menuItem(
        "Methods",
        tabName = "methods",
        icon = icon("flask")
      ),
      shinydashboard::menuItem(
        "Analysis",
        tabName = "analysis",
        icon = icon("bar-chart-o"),
        shinydashboard::menuSubItem("Metadata Analysis",
          tabName = "subitem1"),
        shinydashboard::menuSubItem("Exploratory Data Analysis",
          tabName = "subitem2"),
        shinydashboard::menuSubItem("Differential Expression",
          tabName = "subitem3"),
        shinydashboard::menuSubItem("Biomarker Panels",
          tabName = "subitem4")
      ),
      shinydashboard::menuItem(
        "Generate Report",
        tabName = "report",
        icon = icon("clipboard-list")
      )
    )
  )
)

## Body
body <- shinydashboard::dashboardBody(
    # tags$style(".content {background-color: white;}"),
    # tags$style(type="text/css", ".modelInput label{ display: table-cell; }
    #             .modelInput .form-group { display: table-row;}"),
  tags$head(
    tags$link(
      rel = "stylesheet",
      type = "text/css",
      href = "styles.css"
    )),
    fluidPage(
      shinydashboard::tabItems(
        shinydashboard::tabItem(
          tabName = "overview",
          fluidRow(HTML("<img src='app-architecture.png'
            id='app-architecture'/>")),
          fluidRow(align = "center",
            column(12, h4("Author"), a("Amrit Singh",
              href = "https://www.amritsingh.ca")),
            column(12, h4("Affiliations"), a("PROOF Centre of Excellence",
              href = "http://www.proofcentre.ca"), "|", a("University of British Columbia", href = "https://www.ubc.ca/")),
            column(12, h4("Source code"), a("web app",
              href = "https://github.com/singha53/omicsBioAnalytics"), " | ", a("Multimodal Alexa Skill",
                href = "https://github.com/singha53/omics-bioanalytics-alexa-skill")),
            column(12, h4("Demo"), a("Application to heart failure data",
              href = "https://www.youtube.com/watch?v=u1zLL4uXZi8"), " | ", a("Application to COVID-19 data", href = "https://www.youtube.com/watch?v=oglZDscpbAU"), " | ",  a("Alexa Skill",
                href = "https://www.youtube.com/watch?v=MEDLiO4CL7o")),
            column(12, h4("Feedback"), a("Web app",
              href = "https://github.com/singha53/omicsBioAnalytics/issues"), " | ",  a("Alexa Skill",
                href = "https://github.com/singha53/omics-bioanalytics-alexa-skill/blob/master/CONTRIBUTING.md")),
            column(12, h4("Contest Submission"), a("Shiny Contest 2020",
              href = "https://community.rstudio.com/t/omics-bioanalytics-reproducible-research-using-r-shiny-and-alexa-2020-shiny-contest-submission/59770"))
          )
        ),
        shinydashboard::tabItem(
          tabName = "data",
          omicsBioAnalytics::data_upload_ui("data_upload")
        ),
        shinydashboard::tabItem("subitem1",
          omicsBioAnalytics::metadata_ui("metadata")
        ),
        shinydashboard::tabItem("subitem2",
          omicsBioAnalytics::eda()
        ),
        shinydashboard::tabItem("subitem3",
          omicsBioAnalytics::dea()
        ),
        shinydashboard::tabItem("subitem4",
          omicsBioAnalytics::biomarker_discovery_analysis()
        ),
        shinydashboard::tabItem(
          tabName = "methods",
          h5("R version 3.6.1"),
          h5("RStudio version 1.2.5019."),
          fluidRow(column(6, h3("Analyses")),
            column(6, h3("R-packages"))),
          fluidRow(column(6, h5("Development of web application")),
            column(6, h5("shiny (v1.1), shinydashboard (v0.7.1),
              shinyBS (0.61)"))),
          fluidRow(column(6, h5("Data wrangling")),
            column(6, h5("tidyverse (v1.2.1)"))),
          fluidRow(column(6, h5("Exploratory Data Analysis")),
            column(6, h5("stats (v3.5.1)"))),
          fluidRow(column(6, h5("Differential expression analysis")),
            column(6, h5("limma (v3.40.6)"))),
          fluidRow(column(6, h5("Biomarker analysis")),
            column(6, h5("caret (v6.0-84), glmnet (v2.0-18), pROC (v1.15.3)"))),
          fluidRow(column(6, h5("Pathway enrichment analysis")),
            column(6, h5("enrichR (v2.1)"))),
          fluidRow(column(6, h5("graphical visualizations")),
            column(6, h5("plotly (v4.9.1), d3heatmap (v0.6.1.2),
              RColorBrewer (v1.1-2), visNetwork (v2.0.9), googleVis (v0.6.4),
              igraph (v1.2.4.1), canvasXpress (v1.26.5), DT (v0.11), UpSetR (v1.4.0)")))
        ),
        shinydashboard::tabItem(
          tabName = "report",
          omicsBioAnalytics::report_ui("report")
        )
      )
    )
)


shinydashboard::dashboardPage(
  skin = "blue",
  header,
  sidebar,
  body
)

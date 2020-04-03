
# Header
header <- dashboardHeader(
  title = "Omics BioAnalytics"
)

# Sidebar
sidebar <- dashboardSidebar(
  conditionalPanel(
    condition = "output.analysisRan == false",
    sidebarMenu(
      id = "tabs",
      menuItem("Overview", tabName = "overview", icon = icon("dashboard")),
      menuItem(
        "Data upload",
        tabName = "data",
        icon = icon("database")
      ),
      menuItem(
        "Methods",
        tabName = "methods",
        icon = icon("flask")
      ),
      menuItem(
        "Generate Report",
        tabName = "report",
        icon = icon("clipboard-list")
      )
    )
  ),
  conditionalPanel(
    condition = "output.analysisRan == true",
    sidebarMenu(
      id = "tabs",
      menuItem("Overview", tabName = "overview", icon = icon("dashboard")),
      menuItem(
        "Data upload",
        tabName = "data",
        icon = icon("database")
      ),
      menuItem(
        "Methods",
        tabName = "methods",
        icon = icon("flask")
      ),
      menuItem(
        "Analysis",
        tabName = "analysis",
        icon = icon("bar-chart-o"),
        menuSubItem("Metadata", tabName = "subitem1"),
        menuSubItem("Exploratory Data Analysis", tabName = "subitem2"),
        menuSubItem("Differential Expression", tabName = "subitem3"),
        menuSubItem("Biomarker Panels", tabName = "subitem4")
      ),
      menuItem(
        "Generate Report",
        tabName = "report",
        icon = icon("clipboard-list")
      )
    )
  )
)

## Body
body <- dashboardBody(
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
      tabItems(
        tabItem(
          tabName = "overview",
          fluidRow(HTML("<img src='app-architecture.png' id='app-architecture'/>"))
        ),
        tabItem(
          tabName = "data",
          omicsBioAnalytics::dataUpload()
        ),
        tabItem("subitem1",
          omicsBioAnalytics::metadata()
        ),
        tabItem("subitem2",
          omicsBioAnalytics::eda()
        ),
        tabItem("subitem3",
          omicsBioAnalytics::dea()
        ),
        tabItem("subitem4",
          omicsBioAnalytics::biomarkerDiscoveryUI()
        ),
        tabItem(
          tabName = "methods",
          h5("R version 3.6.1"),
          h5("RStudio version 1.2.5019."),
          fluidRow(column(6, h3("Analyses")),
            column(6, h3("R-packages"))),
          fluidRow(column(6, h5("Development of web application")),
            column(6, h5("shiny (v1.1), shinydashboard (v0.7.1), shinyBS (0.61)"))),
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
            column(6, h5("plotly (v4.9.1), d3heatmap (v0.6.1.2), RColorBrewer (v1.1-2), visNetwork (v2.0.9), googleVis (v0.6.4), igraph (v1.2.4.1), canvasXpress (v1.26.5), DT (v0.11), UpSetR (v1.4.0)")))
        ),
        tabItem(
          tabName = "report",
          # omicsBioAnalytics::dataUpload()
          omicsBioAnalytics::reportUI()
        )
      )
    )
)


dashboardPage(
  skin = "blue",
  header,
  sidebar,
  body
)

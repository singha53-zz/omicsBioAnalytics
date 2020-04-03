
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
          "methods"
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

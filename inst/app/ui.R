
# Header
header <- dashboardHeader(
  title = "Omics BioAnalytics",

  # Dropdown menu for messages
  dropdownMenu(type = "messages", badgeStatus = "success",
    messageItem("Support Team",
      "This is the content of a message.",
      time = "5 mins"
    ),
    messageItem("Support Team",
      "This is the content of another message.",
      time = "2 hours"
    ),
    messageItem("New User",
      "Can I get some help?",
      time = "Today"
    )
  ),

  # Dropdown menu for notifications
  dropdownMenu(type = "notifications", badgeStatus = "warning",
    notificationItem(icon = icon("users"), status = "info",
      "5 new members joined today"
    ),
    notificationItem(icon = icon("warning"), status = "danger",
      "Resource usage near limit."
    ),
    notificationItem(icon = icon("shopping-cart", lib = "glyphicon"),
      status = "success", "25 sales made"
    ),
    notificationItem(icon = icon("user", lib = "glyphicon"),
      status = "danger", "You changed your username"
    )
  ),

  # Dropdown menu for tasks, with progress bar
  dropdownMenu(type = "tasks", badgeStatus = "danger",
    taskItem(value = 20, color = "aqua",
      "Refactor code"
    ),
    taskItem(value = 40, color = "green",
      "Design new layout"
    ),
    taskItem(value = 60, color = "yellow",
      "Another task"
    ),
    taskItem(value = 80, color = "red",
      "Write documentation"
    )
  )
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
        menuSubItem("Patient Characteristics", tabName = "subitem1"),
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
          omicsBioAnalytics::patientCharacteristics()
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

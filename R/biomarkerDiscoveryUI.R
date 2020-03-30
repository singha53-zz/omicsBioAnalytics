#' @export
#' @rdname uiElements
biomarkerDiscoveryUI <- function() {
  fluidRow(
    tabsetPanel(
      tabPanel("Build biomarker panels",
        sidebarLayout(
          sidebarPanel(

            tags$style(type="text/css", ".modelInput label{ display: table-cell; } .modelInput .form-group { display: table-row;}"),

            # Input: Slider for the number of observations to generate ----
            h4("Setting:"),
            checkboxGroupInput("selectedGroups", "Select two groups to compare (required):", "", "", inline = TRUE),
            sliderInput("alpha",
              "Variable selection (alpha values)",
              min = 0, max = 1, value = c(0.7, 1)),

            tags$div(class = "modelInput", numericInput(inputId="alphaGrid", h5("Number of alpha values:", style = 'padding:0px 6px 0px 0px;'), value = 5, width = "50%")),

            checkboxGroupInput("checkGroup_single",
              h5("Build biomarker panel(s) using dataset(s):"),
              choices = list("Clinical" = "clinical",
                "Cells" = "cells",
                "Holter"="holter",
                "Transcripts" = "transcripts",
                "Proteins" = "proteins"),
              selected = c("clinical", "cells", "holter", "transcripts", "proteins"), inline = TRUE),

            checkboxGroupInput("checkGroup_ensemble",
              h5("Build ensemble of biomarker panels using dataset(s):"),
              choices = list("Clinical" = "clinical",
                "Cells" = "cells",
                "Holter"="holter",
                "Transcripts" = "transcripts",
                "Proteins" = "proteins"),
              selected = c("cells", "holter", "transcripts", "proteins"), inline = TRUE),

            h4("Classification performance"),
            # Input: cross-validation scheme
            radioButtons(
              "cvScheme",
              "Cross-validation (CV) scheme:",
              c("5-fold"="fiveFold",
                "10-fold"="tenFold"
              ),
              inline = TRUE
            ),

            # Input: Number of iterations
            tags$div(class = "modelInput", numericInput(inputId="n_repeats", h5("Number of times to repeat CV:", style = 'padding:0px 6px 0px 0px;'), value = 5, width = "50%")),

            actionButton("build", "Build Penalized Regression Models!", icon = icon("power-off"), style="color: #fff; background-color: #337ab7; border-color: #2e6da4", width = "100%")
            ),

          mainPanel(

            tabsetPanel(type = "tabs",
              tabPanel("Performance",
                uiOutput("errMsg"),
                fluidRow(
                  column(6,
                    tags$div(plotlyOutput("rocPlot"),
                      style = 'padding:100px 0px 0px 0px;')
                  ),
                  column(6,
                    tags$div(
                      h4("Optimal biomarker panels", align = "center"),
                      DT::dataTableOutput("aucs"), style = 'padding:100px 100px 0px 0px;')
                  )
                )),
              tabPanel("Panels",
                fluidRow(column(2, ""), column(8, h4("Overlap between single and ensemble biomarker panels"), plotOutput("panelN"),
                  downloadButton("biomarkerPanels", label = HTML("<span style='font-size:1em;'>Download<br/>Biomarkers</span>"), style="color: #fff; background-color: #FF7F00; border-color: #2e6da4")
                ), column(2, "")),
                fluidRow(column(6, h3("Individual biomarker panels"), plotlyOutput("singlePanel", height = "1000px")),
                  column(6, h3("Ensemble biomarker panel"), plotlyOutput("ensemblePanel", height = "1000px")))
              ),
              tabPanel("PCA plots",
                fluidRow(column(6,
                  h3("Base classifier", align = "center"),
                  canvasXpress::canvasXpressOutput("pcaBasePanel"),
                  radioButtons("pcaBasePanelRadioButtons", "Select dataset", c(" "), inline = TRUE)
                ),
                  column(6,
                    h3("Ensemble classifier", align = "center"),
                    canvasXpress::canvasXpressOutput("pcaEnsemblePanel")
                  ))),
              tabPanel("Heatmaps",
                fluidRow(column(6,
                  h3("Base classifier", align = "center"),
                  canvasXpress::canvasXpressOutput("heatmapBasePanel"),
                  radioButtons("heatmapBasePanelRadioButtons", "Select dataset", c(" "), inline = TRUE)
                ),
                  column(6,
                    h3("Ensemble classifier", align = "center"),
                    canvasXpress::canvasXpressOutput("heatmapEnsemblePanel")
                  ))),
              tabPanel("Biological signficance of ensemble panel",
                fluidRow(
                  column(6, sliderInput("corCutoff", h5("Correlation cutoff", align = "center"),
                    min = 0.5, max = 1, value = 0.5)),
                  column(6, sliderInput("bioFDR", h5("FDR cutoff for pathway enrichment", align = "center"),
                    min = 0.05, max = 1, value = 0.05))
                ),
                visNetworkOutput("biomarkerSig", height = "600px", width = "100%")
              )
            )

          )
        )
      )
    )
  )
}

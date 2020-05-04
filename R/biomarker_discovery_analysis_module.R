#' UI of Biomarker Discovery Analysis page
#' @export
#' @rdname biomarker_discovery_analysis
biomarker_discovery_analysis <- function() {
  shiny::fluidRow(shiny::uiOutput("biomarker_discovery_analysis"))
}

#' UI of Biomarker Discovery Analysis page
#'
#' @param id, character used to specify namespace,
#' see \code{shiny::\link[shiny]{NS}}
#'
#' @return a \code{shiny::\link[shiny]{tagList}} containing UI element
#' @export
biomarker_discovery_analysis_ui <- function(id, dataset_names, response, response_var) {
  ns <- shiny::NS(id)

  shiny::fluidPage(
    shiny::fluidRow(
      shiny::tabsetPanel(
        shiny::tabPanel("Build biomarker panels",
          shiny::sidebarLayout(
            shiny::sidebarPanel(

              shiny::tags$style(type = "text/css", ".modelInput label{ display: table-cell; } .modelInput .form-group { display: table-row;}"),

              # Input: Slider for the number of observations to generate ----
              shiny::h4("Setting:"),
              shiny::checkboxGroupInput(ns("selectedGroups"),
                label = paste0("Select two groups from the ", response_var, " response variable to compare (required):"),
                choices = levels(response),
                selected = levels(response)[1:2],
                inline = TRUE),
              shiny::sliderInput(ns("alpha"),
                "Variable selection (alpha values)",
                min = 0, max = 1, value = c(0.7, 1)),

              shiny::tags$div(class = "modelInput", shiny::numericInput(inputId = ns("alphaGrid"), shiny::h5("Number of alpha values:", style = 'padding:0px 6px 0px 0px;'), value = 5, width = "50%")),

              shiny::checkboxGroupInput(ns("checkGroup_single"),
                shiny::h5("Build biomarker panel(s) using dataset(s):"),
                choices = dataset_names, inline = TRUE),

              shiny::checkboxGroupInput(ns("checkGroup_ensemble"),
                shiny::h5("Build ensemble of biomarker panels using dataset(s):"),
                choices = dataset_names, inline = TRUE),

              shiny::h4("Classification performance"),
              # Input: cross-validation scheme
              shiny::radioButtons(
                ns("cvScheme"),
                "Cross-validation (CV) scheme:",
                c("5-fold" = "fiveFold",
                  "10-fold" = "tenFold"
                ),
                inline = TRUE
              ),

              # Input: Number of iterations
              shiny::tags$div(class = "modelInput", shiny::numericInput(inputId = ns("n_repeats"), shiny::h5("Number of times to repeat CV:", style = 'padding:0px 6px 0px 0px;'), value = 5, width = "50%")),

              shiny::actionButton(ns("build"), "Build Penalized Regression Models!", icon = shiny::icon("power-off"), style = "color: #fff; background-color: #337ab7; border-color: #2e6da4", width = "100%"),
              shiny::uiOutput(ns("errMsg"))
            ),

            shiny::mainPanel(
              shiny::tabsetPanel(type = "tabs",
                shiny::tabPanel("Performance",
                  shiny::fluidRow(
                    shiny::column(6,
                      shiny::tags$div(plotly::plotlyOutput(ns("rocPlot")),
                        style = 'padding:100px 0px 0px 0px;')
                    ),
                    shiny::column(6,
                      shiny::tags$div(
                        shiny::h4("Optimal biomarker panels", align = "center"),
                        DT::dataTableOutput(ns("aucs")), style = 'padding:100px 100px 0px 0px;')
                    )
                  )),
                shiny::tabPanel("Panels",
                  shiny::fluidRow(
                    shiny::column(12, omicsBioAnalytics::variable_plot_ui(ns("biomarker_plot")))
                  ),
                  shiny::fluidRow(
                    shiny::column(12,
                      h2("Single Omic biomarker panels"),
                      shiny::uiOutput(ns("biomarker_panels")))
                  ),
                  shiny::fluidRow(
                    shiny::column(12,
                      h2("Ensemble biomarker panel"),
                      shiny::uiOutput(ns("ensemble_panel")))
                  ),
                  shiny::fluidRow(
                    shiny::column(12, shiny::h4("Overlap between single and ensemble biomarker panels"),
                    shiny::plotOutput(ns("panelN")),
                    shiny::downloadButton(ns("biomarkerPanels"), label = shiny::HTML("<span style='font-size:1em;'>Download<br/>Biomarkers</span>"), style = "color: #fff; background-color: #FF7F00; border-color: #2e6da4")
                  ))
                ),
                shiny::tabPanel("PCA plots",
                  shiny::fluidRow(shiny::column(6,
                    shiny::h3("Base classifier", align = "center"),
                    canvasXpress::canvasXpressOutput(ns("pcaBasePanel")),
                    shiny::radioButtons(ns("pcaBasePanelRadioButtons"), "Select dataset", c(" "), inline = TRUE)
                  ),
                    shiny::column(6,
                      shiny::h3("Ensemble classifier", align = "center"),
                      canvasXpress::canvasXpressOutput(ns("pcaEnsemblePanel"))
                    ))),
                shiny::tabPanel("Heatmaps",
                  shiny::fluidRow(shiny::column(6,
                    shiny::h3("Base classifier", align = "center"),
                    canvasXpress::canvasXpressOutput(ns("heatmapBasePanel")),
                    shiny::radioButtons(ns("heatmapBasePanelRadioButtons"), "Select dataset", c(" "), inline = TRUE)
                  ),
                    shiny::column(6,
                      shiny::h3("Ensemble classifier", align = "center"),
                      canvasXpress::canvasXpressOutput(ns("heatmapEnsemblePanel"))
                    ))),
                shiny::tabPanel("Biological signficance of ensemble panel",
                  shiny::fluidRow(
                    shiny::column(6, shiny::sliderInput(ns("corCutoff"), shiny::h5("Correlation cutoff", align = "center"),
                      min = 0.5, max = 1, value = 0.5)),
                    shiny::column(6, shiny::sliderInput(ns("bioFDR"), shiny::h5("FDR cutoff for pathway enrichment", align = "center"),
                      min = 0.05, max = 1, value = 0.05))
                  ),
                  visNetwork::visNetworkOutput(ns("biomarkerSig"), height = "600px", width = "100%")
                )
              )

            )
          )
        )
      )
    )
  )
}

#' Reactive values for biomarker_discovery_analysis_server module server-side processing
#'
#' @param input, output, session standard \code{shiny}
#'
#' @return list with following components
#' \describe{
#'   \item{cont_var}{reactive character continuous variable to analyze}
#'   \item{cat_var}{reactive character categorical variable to analyze}
#'   \item{transform}{reactive boolean whether to apply a
#'   log2 transformation of continuous variable}
#' }
#' @export
biomarker_discovery_analysis_ui_vars <- function(input, output, session) {

  return(
    list(
      selectedGroups = shiny::reactive({
        input$selectedGroups
      }),
      alpha = shiny::reactive({
        input$alpha
      }),
      alphaGrid = shiny::reactive({
         input$alphaGrid
       }),
      checkGroup_single = shiny::reactive({
         input$checkGroup_single
       }),
      checkGroup_ensemble = shiny::reactive({
         input$checkGroup_ensemble
       }),
      cvScheme = shiny::reactive({
         input$cvScheme
       }),
      n_repeats = shiny::reactive({
         input$n_repeats
       }),
       build = shiny::reactive({
         input$build
       }),
      pcaBasePanelRadioButtons = shiny::reactive({
         input$pcaBasePanelRadioButtons
       }),
      heatmapBasePanelRadioButtons = shiny::reactive({
         input$heatmapBasePanelRadioButtons
       }),
      corCutoff = shiny::reactive({
         input$corCutoff
       }),
      bioFDR = shiny::reactive({
        input$bioFDR
       })
    )
  )
}

#' Biomarker Discovery Analysis module server-side processings
#'
#' This module produces the Biomarker Discovery Analysis panel for a given dataset
#'
#' @param input,output,session standard \code{shiny} boilerplate
#' @param biomarker_discovery_analysis_ui_vars list of reactive vars ()
#' @export
biomarker_discovery_analysis_server <- function(input, output, session, datasets, response, response_var, group_colors, biomarker_discovery_analysis_ui_vars) {
  ns <- session$ns

  ## Build GLMNET models with button is clicked.
  shiny::observeEvent(biomarker_discovery_analysis_ui_vars$build(), {
    # Validate conditioning need to build biomarker panels
    errMsg <- shiny::reactive({shiny::validate(
      need(length(biomarker_discovery_analysis_ui_vars$selectedGroups()) > 1, "Please only select two groups."),
      need(length(biomarker_discovery_analysis_ui_vars$selectedGroups()) < 3, "Please only select two groups."),
      need(length(biomarker_discovery_analysis_ui_vars$checkGroup_single()) > 0, "Please select at least one dataset to build a classifier."),
      need(length(biomarker_discovery_analysis_ui_vars$checkGroup_ensemble()) > 0, "Please select at least one dataset to build a ensemble classifier.")
    )})
    output$errMsg <- shiny::renderUI({
      errMsg()
    })
    shiny::req(length(biomarker_discovery_analysis_ui_vars$selectedGroups()) == 2)
    shiny::req(length(biomarker_discovery_analysis_ui_vars$checkGroup_single()) > 0)
    shiny::req(length(biomarker_discovery_analysis_ui_vars$checkGroup_ensemble()) > 0)
    shiny::req(length(response) > 0 )
    shiny::req(sum(biomarker_discovery_analysis_ui_vars$selectedGroups() %in% response) > 0)

    ## only consider data for selected two groups
    if (nlevels(response) > 2) {
      subset_response <- shiny::isolate({droplevels(response[response %in% input$selectedGroups])})
      subset_eset <- shiny::isolate({lapply(datasets, function(i){
        i[response %in% input$selectedGroups, ]
      })})
    } else {
      subset_response <- shiny::isolate({response})
      subset_eset <- shiny::isolate({
        datasets})
    }

    # if response is coded with numbers only, change it to a valid R variable
    subset_response <- factor(make.names(subset_response))

    # parameters
    alphaMin <- shiny::isolate(biomarker_discovery_analysis_ui_vars$alpha()[1])
    alphaMax <- shiny::isolate(biomarker_discovery_analysis_ui_vars$alpha()[2])
    alphalength <- shiny::isolate(biomarker_discovery_analysis_ui_vars$alphaGrid())
    kfolds <- shiny::isolate(biomarker_discovery_analysis_ui_vars$cvScheme())
    n_repeats <- shiny::isolate(biomarker_discovery_analysis_ui_vars$n_repeats())
    single <- shiny::isolate(as.character(biomarker_discovery_analysis_ui_vars$checkGroup_single()))
    ensem <- shiny::isolate(as.character(biomarker_discovery_analysis_ui_vars$checkGroup_ensemble()))
    dataset_names <- shiny::isolate(unique(c(single, ensem)))

    ## set control parameters
    if (kfolds == "fiveFold") {
      ctrl <- caret::trainControl(method = "repeatedcv",
        number = 5,
        repeats = n_repeats,
        summaryFunction = twoClassSummary,
        classProbs = TRUE,
        savePredictions = TRUE)
      set.seed(123)
      ctrl$index <- caret::createMultiFolds(subset_response, 5, n_repeats)
    } else {
      ctrl <- caret::trainControl(method = "repeatedcv",
        number = 10,
        repeats = n_repeats,
        summaryFunction = twoClassSummary,
        classProbs = TRUE,
        savePredictions = TRUE)
      set.seed(456)
      ctrl$index <- caret::createMultiFolds(subset_response, 10, n_repeats)
    }

    enetGrid = expand.grid(alpha = seq(alphaMin, alphaMax, length.out = alphalength),
      lambda = seq(0.001, 0.1, by = 0.01))

    shiny::withProgress(message = 'Constructing models.',
      detail = 'This may take a while...', value = 0, {
        mods <- vector("list", length(dataset_names))
        names(mods) <- dataset_names

        for (dat in dataset_names) {
          # Increment the progress bar, and update the detail text.
          incProgress(1/length(dataset_names), detail = paste("Building ", dat, " model..."))

          mods[[dat]] <- caret::train(x = subset_eset[[dat]], y = subset_response,
            preProc = c("center", "scale"),
            method = "glmnet",
            metric = "ROC",
            tuneGrid = enetGrid,
            trControl = ctrl)
        }

        ## Add ensemble panel
        pred <- lapply(mods, function(i){
          i$pred
        }) %>%
          do.call(rbind, .) %>%
          as.data.frame() %>%
          mutate(panel = rep(names(mods), each = nrow(mods[[1]]$pred)))
        pred <- pred[, -c(1, 4)]
        colnames(pred)[3] <- "Yes"
        pred <- pred %>% dplyr::select(obs, rowIndex, Yes:panel)
        pred$Resample <-  sapply(strsplit(pred$Resample, "\\."), function(i) i[2])
        pred <- pred %>%
          filter(panel %in% ensem) %>%
          group_by(obs, rowIndex, alpha, lambda, Resample) %>%
          dplyr::summarise(Yes = mean(Yes)) %>%
          mutate(panel = "Ensemble") %>%
          dplyr::full_join(pred, .)

        ## Compute classification performance
        perf <- pred %>%
          group_by(panel, alpha, lambda, Resample) %>%
          dplyr::summarise(auc = pROC::roc(obs~Yes, direction = "<")$auc) %>%
          ungroup %>%
          group_by(panel, alpha, lambda) %>%
          dplyr::summarise(Mean = mean(auc),
            SD = sd(auc)) %>%
          ungroup() %>%
          group_by(panel) %>%
          filter(Mean == max(Mean)) %>%
          filter(SD == max(SD)) %>%
          arrange(desc(lambda)) %>%
          dplyr::slice(1) %>%
          ungroup() %>%
          arrange(desc(Mean))

        ## Compuate roc curves
        rocTable <- pred %>%
          group_by(panel, alpha, lambda, Resample) %>%
          tidyr::nest() %>%
          mutate(roc = purrr::map(data, ~{
            data.frame(tpr = pROC::roc(.$obs,.$Yes, direction = "<")$sensitivities,
              fpr = (1-pROC::roc(.$obs,.$Yes, direction = "<")$specificities))
          })) %>%
          tidyr::unnest(roc) %>%
          group_by(panel, alpha, lambda, fpr) %>%
          dplyr::summarise(mean_tpr = mean(tpr), sd_fpr = sd(tpr)) %>%
          dplyr::inner_join(x = perf, y = ., by = c("panel", "alpha", "lambda"))
      })


    colors <- c("#2ca02c", "#1f77b4", "black", "#ff7f0e","#9467bd", "#d62728")
    output$rocPlot <- plotly::renderPlotly({
      options(htmlwidgets.TOJSON_ARGS = NULL) ## import in order to run canvasXpress
      plot_ly(rocTable, x = ~fpr, y = ~mean_tpr, type = 'scatter', mode = 'lines',
        linetype = ~panel, color = ~panel, source = "auc", key = ~panel, colors = colors) %>%
        layout(title = 'ROC curves',
          xaxis = list(title = 'False Positive Rate (FPR)'),
          yaxis = list(title = 'Average True Positive Rate (TPR)')) %>%
        layout(legend = list(x = 0.6, y = 0.2))
    })

    output$aucs <- DT::renderDataTable({
      DT::datatable(
        perf %>%
          mutate(alpha = signif(alpha, 2), lambda = signif(lambda, 2),
            Mean = signif(Mean, 2), SD = signif(SD, 2)),
        selection = list(target = "row+column"),
        options = list(pageLength = nrow(perf), dom = "ft", digits = 4))
    }, width = "50%")
    proxy = DT::dataTableProxy('aucs')
    # highlight rows that are selected on plotly output
    shiny::observe({
      event.data = plotly::event_data("plotly_hover", source = "auc")

      if (is.null(event.data)) {
        rowNums <- NULL
      } else {
        rowNums <- row.names(as.data.frame(perf)[perf$panel %in% as.character(event.data$key[[1]]),])
      }

      proxy %>% DT::selectRows(as.numeric(rowNums))
    })

    ################################################################################
    #
    # Panels tab
    #
    ################################################################################
    ## Variables in each Single Omics Biomarker Panel
    ## Fit single dataset models
    singlePanelMods <- lapply(1:length(single), function(i){
      dataset <- single[i]
      alpha <- subset(perf, panel == dataset)$alpha
      lambda <- subset(perf, panel == dataset)$lambda
      fit <- glmnet::glmnet(x = as.matrix(subset_eset[[dataset]]), y = subset_response, alpha = alpha, lambda = lambda, family = "binomial")
      Coefficients <- coef(fit, s = lambda)
      Active.Index <- which(Coefficients[, 1] != 0)
      data.frame(coef = abs(Coefficients[Active.Index, ])) %>%
        dplyr::mutate(features = rownames(.)) %>%
        dplyr::slice(-1) %>%
        dplyr::arrange(coef) %>%
        dplyr::mutate(features = factor(as.character(features), as.character(features))) %>%
        dplyr::mutate(panel = dataset)
    })
    names(singlePanelMods) <- single
    ### single panel features
    singlePanel <- lapply(singlePanelMods, function(i){
      as.character(i$features)
    })
    output$biomarker_panels = shiny::renderUI({
      omicsBioAnalytics::dotplot_ui(ns("biomarker_panels"),
        panel_names = names(singlePanelMods))
    })

    single_panel_ui_vars <- shiny::callModule(module = omicsBioAnalytics::dotplot_ui_vars,
      "biomarker_panels")
    shiny::callModule(module = omicsBioAnalytics::dotplot_server,
      id = "biomarker_panels",
      data = do.call(rbind, singlePanelMods),
      dotplot_ui_vars = single_panel_ui_vars)

    ## Variables in the Ensemble Biomarker Panel
    ## Fit ensemble dataset models
    ensemblePanelMods <- lapply(1:length(ensem), function(i){
      dataset <- ensem[i]
      alpha <- subset(perf, panel == "Ensemble")$alpha
      lambda <- subset(perf, panel == "Ensemble")$lambda
      fit <- glmnet::glmnet(x = as.matrix(subset_eset[[dataset]]), y = subset_response, alpha = alpha, lambda = lambda, family = "binomial")
      Coefficients <- coef(fit, s = lambda)
      Active.Index <- which(Coefficients[, 1] != 0)
      data.frame(coef = abs(Coefficients[Active.Index, ])) %>%
        dplyr::mutate(features = rownames(.)) %>%
        dplyr::slice(-1) %>%
        dplyr::arrange(coef) %>%
        dplyr::mutate(features = factor(as.character(features), as.character(features))) %>%
        dplyr::mutate(panel = dataset)
    })
    names(ensemblePanelMods) <- ensem
    ### ensemble panel features
    ensemblePanel <- lapply(ensemblePanelMods, function(i){
      as.character(i$features)
    })
    output$ensemble_panel = shiny::renderUI({
      omicsBioAnalytics::dotplot_ui(ns("ensemble_panel"),
        panel_names = names(ensemblePanelMods))
    })
    ensemble_panel_ui_vars <- shiny::callModule(module = omicsBioAnalytics::dotplot_ui_vars,
      "ensemble_panel")
    shiny::callModule(module = omicsBioAnalytics::dotplot_server,
      id = "ensemble_panel",
      data = do.call(rbind, ensemblePanelMods),
      dotplot_ui_vars = ensemble_panel_ui_vars)

    output$biomarkerPanels <- shiny::downloadHandler(
      filename = function() {
        paste("BiomarkerPanels_multiomics_HFhospitalizations_", Sys.Date(), ".txt", sep = "")
      },
      content = function(file) {
        write.table(rbind(data.frame(dataset = rep(names(singlePanel), sapply(singlePanel, length)),
          biomarkers = unlist(singlePanel),
          Panel = "Individual Panels"),
          data.frame(dataset = rep(names(ensemblePanel), sapply(ensemblePanel, length)),
            biomarkers = unlist(ensemblePanel),
            Panel = "Ensemble Panel")),
          file,
          sep = "\t",
          row.names = FALSE)
      }
    )

    ## plot selected biomarker based on selection from dot plots above
    selected_variable <- shiny::reactiveValues(feature = "", panel = "")
    shiny::observeEvent(single_panel_ui_vars$dotplot_click(), {
      if (!is.null(single_panel_ui_vars$dotplot_click)) {
        selection <- shiny::nearPoints(do.call(rbind, singlePanelMods),
          single_panel_ui_vars$dotplot_click(),
          threshold = 5,
          maxpoints = 1)
        selected_variable$feature <- as.character(selection$features)
        selected_variable$panel <- selection$panel
      }
    })
    shiny::observeEvent(ensemble_panel_ui_vars$dotplot_click(), {
      if (!is.null(ensemble_panel_ui_vars$dotplot_click)) {
        selection <- shiny::nearPoints(do.call(rbind, ensemblePanelMods),
          ensemble_panel_ui_vars$dotplot_click(),
          threshold = 5,
          maxpoints = 1)
        selected_variable$feature <- as.character(selection$features)
        selected_variable$panel <- selection$panel
      }
    })

    shiny::observeEvent(selected_variable$feature, {
      something_is_there <- ifelse(length(selected_variable$feature) > 0, TRUE, FALSE)
      print(selected_variable$feature)
      print(selected_variable$panel)
      print(length(selected_variable$feature))
      if (something_is_there) {
        if (selected_variable$panel %in% names(datasets)){
          print("make plot")
          biomarker_ui_vars <- shiny::callModule(module = omicsBioAnalytics::variable_plot_ui_vars,
            "biomarker_plot")
          shiny::callModule(module = omicsBioAnalytics::variable_plot_server,
            id = "biomarker_plot",
            response = response,
            response_var = response_var,
            datasets = datasets,
            selected_variable = selected_variable,
            group_colors = group_colors,
            variable_plot_ui_vars = biomarker_ui_vars)
        }
      } else {
        omicsBioAnalytics::empty_plot("Please select a point from the dot plots below.")
      }
    })

    barColors <- c("#1f77b4", "#ff7f0e", "#2ca02c", "#d62728","#9467bd")
    names(barColors) <- names(subset_eset)

    overlap <- lapply(intersect(single, ensem), function(i){
      intersect(singlePanel[[i]], ensemblePanel[[i]])
    })
    names(overlap) <- intersect(single, ensem)

    output$panelN <- shiny::renderPlot({
      panels <-  singlePanel
      panels$Ensemble <- as.character(unlist(ensemblePanel))

      Input <- UpSetR::fromList(panels)
      UpSetR::upset(Input, sets = colnames(Input))
    })

    ################################################################################
    #
    # PCA plots
    #
    ################################################################################
    shiny::observe({
      shiny::updateRadioButtons(session, "pcaBasePanelRadioButtons",
        label = "Select panel",
        choices = single,
        inline = TRUE)
    })
    ### Base classifier
    output$pcaBasePanel <- canvasXpress::renderCanvasXpress({
      print(group_colors[1:nlevels(subset_response)])
      dataset <- subset_eset[[biomarker_discovery_analysis_ui_vars$pcaBasePanelRadioButtons()]]
      variables <- singlePanel[[biomarker_discovery_analysis_ui_vars$pcaBasePanelRadioButtons()]]
      grouping <- data.frame(Group = subset_response)
      rownames(dataset) <- rownames(grouping) <- paste0("subj", 1:nrow(grouping))

      if (length(variables) > 2) {
        pc <- prcomp(dataset[, variables, drop = FALSE], scale. = TRUE, center = TRUE)

        canvasXpress(data = pc$x[, 1:3], digits = 50,
          varAnnot  = grouping,
          colorBy   = "Group",
          ellipseBy = "Group",
          graphType = "Scatter3D",
          colors = group_colors[1:nlevels(subset_response)],
          xAxisTitle = paste0("PC1 (", round(100*summary(pc)$importance["Proportion of Variance","PC1"], 0), "%)"),
          yAxisTitle = paste0("PC2 (", round(100*summary(pc)$importance["Proportion of Variance","PC2"], 0), "%)"),
          zAxisTitle = paste0("PC3 (", round(100*summary(pc)$importance["Proportion of Variance","PC3"], 0), "%)"))
      } else if (length(variables) == 2) {
        canvasXpress(
          data = dataset[, variables, drop = FALSE],
          varAnnot  = grouping,
          colorBy   = "Group",
          # colorScheme = "Set2",
          colors = group_colors[1:nlevels(subset_response)],
          graphType = "ScatterBubble2D",
          size = list(1)
        )
      } else {
        y = t(as.data.frame(dataset[, variables, drop = FALSE]))
        x = data.frame(Group = grouping)
        colnames(y) <- rownames(x) <- paste0("subj", 1:ncol(y))

        canvasXpress(
          data = y,
          smpAnnot = x,
          colorBy = "Group",
          axisTitleFontStyle = "italic",
          graphOrientation = "vertical",
          graphType = "Boxplot",
          jitter = TRUE,
          colors = group_colors[1:nlevels(subset_response)],
          legendBox = FALSE,
          plotByVariable = TRUE,
          showBoxplotOriginalData = TRUE,
          smpLabelRotate = 90,
          smpTitle = "Response",
          smpTitleFontStyle = "italic",
          title = variables,
          #height = 300,
          afterRender = list(list("groupSamples", list("Group")))
        )
      }
    })

    ### Ensemble classifier
    output$pcaEnsemblePanel <- canvasXpress::renderCanvasXpress({
      dataset <- mapply(function(x, y){
        x[, y]
      }, x = subset_eset[ensem], y = ensemblePanel) %>%
        do.call(cbind, .)
      grouping <- data.frame(Group = subset_response)
      rownames(dataset) <- rownames(grouping) <- paste0("subj", 1:nrow(grouping))

      pc <- prcomp(dataset, scale. = TRUE, center = TRUE)
      canvasXpress(data = pc$x[, 1:3], digits = 50,
        varAnnot  = grouping,
        colorBy   = "Group",
        ellipseBy = "Group",
        colors = group_colors[1:nlevels(subset_response)],
        graphType = "Scatter3D",
        xAxisTitle = paste0("PC1 (", round(100*summary(pc)$importance["Proportion of Variance","PC1"], 0), "%)"),
        yAxisTitle = paste0("PC2 (", round(100*summary(pc)$importance["Proportion of Variance","PC2"], 0), "%)"),
        zAxisTitle = paste0("PC3 (", round(100*summary(pc)$importance["Proportion of Variance","PC3"], 0), "%)"))
    })

    ################################################################################
    #
    # Heatmap of selected variables
    #
    ################################################################################
    shiny::observe({
      shiny::updateRadioButtons(session, "heatmapBasePanelRadioButtons",
        label = "Select panel",
        choices = single,
        inline = TRUE)
    })
    ### Base classifier
    output$heatmapBasePanel <- canvasXpress::renderCanvasXpress({
      dataset <- subset_eset[[biomarker_discovery_analysis_ui_vars$heatmapBasePanelRadioButtons()]]
      variables <- singlePanel[[biomarker_discovery_analysis_ui_vars$heatmapBasePanelRadioButtons()]]
      y <- t(scale(dataset[, variables, drop = FALSE]))
      y[y < -2] <- -2
      y[y > 2] <- 2
      x = data.frame(Group = subset_response)
      rownames(x) <- colnames(y) <- paste0("subj", 1:nrow(x))
      z = data.frame(dataset = rep(biomarker_discovery_analysis_ui_vars$heatmapBasePanelRadioButtons(), length(variables)))
      rownames(z) <- rownames(y)

      if (length(variables) > 1) {
        canvasXpress::canvasXpress(
          data = y,
          smpAnnot = x,
          varAnnot = z,
          colors = group_colors[1:nlevels(subset_response)],
          colorSpectrum = list("magenta", "blue", "black", "red", "gold"),
          colorSpectrumZeroValue = 0,
          graphType = "Heatmap",
          smpOverlays = list("Group"),
          heatmapIndicatorHeight = 50,
          heatmapIndicatorHistogram = TRUE,
          heatmapIndicatorPosition = "topLeft",
          heatmapIndicatorWidth = 60,
          samplesClustered = TRUE,
          showTransition = TRUE,
          variablesClustered = TRUE)
      } else {
        y = t(as.data.frame(dataset[, variables, drop = FALSE]))
        x = data.frame(Group = subset_response)
        colnames(y) <- rownames(x) <- paste0("subj", 1:ncol(y))

        canvasXpress::canvasXpress(
          data = y,
          smpAnnot = x,
          colorBy = "Group",
          axisTitleFontStyle = "italic",
          graphOrientation = "vertical",
          graphType = "Boxplot",
          jitter = TRUE,
          colorScheme = "Set2",
          colors = c('blue', 'red'),
          legendBox = FALSE,
          plotByVariable = TRUE,
          showBoxplotOriginalData = TRUE,
          smpLabelRotate = 90,
          smpTitle = "Response",
          smpTitleFontStyle = "italic",
          title = variables,
          #height = 300,
          afterRender = list(list("groupSamples", list("Group"))))
      }
    })

    ### Ensemble classifier
    output$heatmapEnsemblePanel <- canvasXpress::renderCanvasXpress({
      y = mapply(function(x, y){
        x[, y, drop = FALSE]
      }, x = subset_eset[names(ensemblePanel)], y = ensemblePanel, SIMPLIFY = FALSE) %>%
        do.call(cbind, .) %>%
        scale(.) %>%
        t
      y[y < -2] <- -2
      y[y > 2] <- 2
      print(rownames(y))
      print(colnames(y))
      rownames(y) <- sapply(strsplit(rownames(y), "\\."), function(i) i[2])
      x = data.frame(Group = subset_response)
      rownames(x) <- colnames(y) <- paste0("subj", 1:nrow(x))
      z = data.frame(dataset = rep(names(ensemblePanel), sapply(ensemblePanel, length)))
      rownames(z) <- rownames(y)

      canvasXpress::canvasXpress(
        data = y,
        smpAnnot = x,
        varAnnot = z,
        colors = group_colors[1:nlevels(subset_response)],
        colorKey = list(dataset = barColors[names(ensemblePanel)], Group = group_colors[1:nlevels(subset_response)]),
        colorSpectrum = list("magenta", "blue", "black", "red", "gold"),
        colorSpectrumZeroValue = 0,
        graphType = "Heatmap",
        smpOverlays = list("Group"),
        varOverlayProperties = list(dataset = list(position = "top")),
        varOverlays = list("dataset"),
        heatmapIndicatorHeight = 50,
        heatmapIndicatorHistogram = TRUE,
        heatmapIndicatorPosition = "topLeft",
        heatmapIndicatorWidth = 60,
        samplesClustered = TRUE,
        showTransition = TRUE,
        variablesClustered = TRUE)
    })

    print(names(ensemblePanel))
    print(dim(subset_eset))
    print(ensemblePanel)
    # Enrichment analysis
    dat <- mapply(function(x, y) {
      x[, y, drop = FALSE]
    }, x = subset_eset[names(ensemblePanel)], y = ensemblePanel, SIMPLIFY = FALSE) %>%
      do.call(cbind, .)
    colnames(dat) <- sapply(strsplit(colnames(dat), "\\."), function(i) paste(i[-1], collapse = "_"))
    pairs <- split(t(combn(colnames(dat), 2)), 1:nrow(t(combn(colnames(dat), 2))))
    print(dim(pairs))

    output$biomarkerSig <- visNetwork::renderVisNetwork({
      withProgress(message = 'Constructing network.',
        detail = 'This may take a while...', value = 0, {

          edgesCor <- lapply(pairs, function(i){
            data = as.data.frame(dat[, i])
            c(colnames(data), cor(data[,1], data[,2]))
          }) %>%
            do.call(rbind, .) %>%
            as.data.frame() %>%
            dplyr::rename(from = V1, to = V2, cor = V3) %>%
            mutate(cor = as.numeric(as.character(cor))) %>%
            filter(abs(cor) > biomarker_discovery_analysis_ui_vars$corCutoff()) %>%
            mutate(color = ifelse(cor > 0, "salmon", "blue"))
          print(summary(edgesCor$cor))

          ## gene set enrichment analysis
          # dbs <- listEnrichrDbs()
          dbs <- c("Jensen_DISEASES", "KEGG_2019_Human", "WikiPathways_2019_Human")
          enriched <- enrichr(unlist(ensemblePanel), dbs)

          edgesGset <- do.call(rbind, enriched) %>%
            mutate(database = rep(names(enriched), sapply(enriched, nrow)))
          if (sum(edgesGset$Adjusted.P.value < biomarker_discovery_analysis_ui_vars$bioFDR()) < 1) {
            edges <- edgesCor
          } else {
            sig_pathways <- do.call(rbind, enriched) %>%
              mutate(database = rep(names(enriched), sapply(enriched, nrow))) %>%
              filter(Adjusted.P.value < biomarker_discovery_analysis_ui_vars$bioFDR()) %>%
              dplyr::select(Term, Genes)
            edgesGset <- data.frame(from = rep(sig_pathways$Term, sapply(strsplit(sig_pathways$Genes, ";"), length)),
              to = unlist(strsplit(sig_pathways$Genes, ";")) ) %>%
              mutate(cor = 1, color = "black")
            edges <- rbind(edgesCor, edgesGset)
          }

          # nodes
          nodes <- data.frame(id = unique(c(as.character(edges$from), unlist(ensemblePanel))))
          group <- lapply(as.character(nodes$id), function(i){
            mtch <- paste(na.omit(sapply(names(ensemblePanel), function(j){
              if (i %in% ensemblePanel[[j]]) {
                j
              } else {
                return(NA)
              }
            })), collapse = "_")
          }) %>% unlist()
          group[group == ""] <- "pathway"

          nodes$group <- group
          nodes$label <- nodes$id
          # nodes$shape <- "pathway.png"

          shapes <- c("square", "triangle", "circle", "dot", "star",
            "ellipse", "database", "text", "diamond")
          if (length(unique(group)) < length(shapes)) {
            nodeShapes <- c(shapes[1:length(unique(group))], "box")
            names(nodeShapes) <- c(names(setdiff(unique(group), "pathway")), "pathway")
          } else {
            nodeShapes <- rep('circle', length(unique(group)))
            names(nodeShapes) <- unique(group)
          }

          if (length(unique(group)) < length(shapes)) {
            nodeColors <- RColorBrewer::brewer.pal(12, "Set3")[1:length(unique(group))]
            names(nodeColors) <- unique(group)
          } else {
            nodeColors <- colors()[1:length(unique(group))]  # 657 possibilites
            names(nodeColors) <- unique(group)
          }
          nodes$shape <- nodeShapes[group]
          nodes$color <- nodeColors[group]

          nodeLegend <- lapply(unique(group), function(i){
            list(label = i, shape = as.character(nodeShapes[i]), color = as.character(nodeColors[i]))
          })

          ledges <- data.frame(color = c("salmon", "blue", "black"),
            label = c("positive correlation", "negative correlation", "curated link"),
            font.align = "top")

          visNetwork(nodes, edges) %>%
            visNodes(shapeProperties = list(useBorderWithImage = TRUE)) %>%
            visLayout(randomSeed = 2) %>%
            visLegend(width = 1, position = "left", main = "Group") %>%
            visLegend(addNodes = nodeLegend,
              addEdges = ledges,
              useGroups = FALSE) %>%
            visEvents(click = "function(nodes){
        Shiny.onInputChange('click', nodes.nodes[0]);
        ;}"
            )

        })
    })

  })
}

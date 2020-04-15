hfDatasets <- heartFailure$omicsData
hfDatasets$demo <- heartFailure$demo

function(input, output, session) {

  # Demographics data upload
  getDemoData <- reactive({
    req(input$demo)
    demoData <- read.csv(input$demo$datapath, header = TRUE, sep = input$sep)
    demoData
  })

  # show column names of demo dataset
  output$responseVar <- renderUI({
    keepCols <- apply(getDemoData(), 2, function(i){
      keepvar <- as.character(i)
      ifelse(length(table(as.character(i))) < 9 & min(table(as.character(i))) > 1, TRUE, FALSE)
    })
    selectInput('responseVar', 'Select response variable', colnames(getDemoData()[, keepCols]))
  })

  output$refVar <- renderUI({
    selectInput('refVar', 'Select reference level', unique(getDemoData()[, input$responseVar]))
  })

  # omics data upload
  getOmicsData <- reactive({
    req(input$omicsData)
    omicsData <- lapply(input$omicsData$datapath, read.csv, header = TRUE, sep = input$sep)
    names(omicsData) <- gsub(".csv|.tsv", "", input$omicsData$name)
    omicsData
  })

  # determine which datasets to perform gene set enrichment analysis on?
  output$performPathwayAnalysis <- reactive({
    datasetNames <- sapply(names(getOmicsData()), function(i){
      length(intersect(colnames(getOmicsData()[[i]]), unlist(kegg))) > 5
    })
    names(datasetNames)[datasetNames]
  })
  outputOptions(output, "performPathwayAnalysis", suspendWhenHidden = FALSE)
  performPathwayAnalysis <- reactive({
    datasetNames <- sapply(names(getOmicsData()), function(i){
      length(intersect(colnames(getOmicsData()[[i]]), unlist(kegg))) > 5
    })
    names(datasetNames)[datasetNames]
  })

  # Do not show analysis sidemenu at startup!!
  output$analysisRan <- reactive({
    returnedValue = FALSE
    return(returnedValue)
  })
  outputOptions(output, "analysisRan", suspendWhenHidden = FALSE)

  # if user wants to analyze the example heart failure data
  output$heartFailure <- downloadHandler(
    filename = "heartFailureDatasets_omicsBioAnalytics.zip",
    content = function(file){
      files <- NULL;

      #loop through the sheets
      for (i in 1:length(hfDatasets)){
        #write each sheet to a csv file, save the name
        fileName <- paste0(names(hfDatasets)[i], ".csv")
        write.table(hfDatasets[[i]], fileName, sep = ',', row.names = FALSE)
        files <- c(fileName,files)
      }
      #create the zip file
      zip(file,files)
    }
  )

  # if user wants to analyze the example COVID-19 data
  output$covid19 <- downloadHandler(
    filename = "COVID19Datasets_omicsBioAnalytics.zip",
    content = function(file){
      files <- NULL;

      #loop through the sheets
      for (i in 1:length(covid19)){
        #write each sheet to a csv file, save the name
        fileName <- paste0(names(covid19)[i], ".csv")
        write.table(covid19[[i]], fileName, sep = ',', row.names = FALSE)
        files <- c(fileName,files)
      }
      #create the zip file
      zip(file,files)
    }
  )

  # Run analysis
  observeEvent(input$run, {
    print("number of levels")
    print(getDemoData()[, input$responseVar])
    print(length(unique(getDemoData()[, input$responseVar])))
    output$uploadErrorMsg = renderUI({
      validate(
        need(input$demo, "Metadata is required with at least 1 categorical variable!"),
        need(input$omicsData, "At least one omics data is required!"),
        need(input$responseVar, "A response variable is required!"),
        need(length(unique(getDemoData()[, input$responseVar])) > 1, "At least 2 categories required!")
      )
    })

    ################################################################################
    #
    # Patient Characteristics
    #
    ################################################################################
    ## split demo data into cat and cont vars
    demo <- getDemoData()
    response <- relevel(factor(as.character(demo[, input$responseVar])), ref = input$refVar)
    demoSplit <- omicsBioAnalytics::splitData(demo, group = input$responseVar, trim = 0.8)

    ## @@@@@@@@@@@@@@@@@@@@@@@ Continuous variable panel @@@@@@@@@@@@@@@@@@@@@@@ ##
    updateRadioButtons(session, "vars",
      label = "Choose from one of the following variables:",
      choices = colnames(demoSplit$data.cont),
      selected = colnames(demoSplit$data.cont)[1], inline = TRUE
    )

    observe({
      req(input$vars != "")
      DF <- reactive({
        df <- data.frame(x = response,
          y = if (input$transform == "no") {
            demo[, input$vars]
          } else {
            log2(demo[, input$vars])
          }) %>%
          na.omit()
      })

      fit <- reactive({
        if(input$test == "lr"){
          lm(y~x, data = DF())
        } else {
          kruskal.test(y~x, data = DF())
        }
      })

      yaxis <- reactive({
        if(input$transform == "no") {
          input$vars
        } else {
          paste(input$vars, "(log2)")
        }
      })
      title <- reactive({
        paste(input$vars, " vs. ", input$responseVar)
      })
      output$plot <- renderPlotly({
        options(htmlwidgets.TOJSON_ARGS = NULL) ## import in order to run canvasXpress
        ggplotly(DF() %>%
        ggplot(aes(x = x, y = y, fill = x)) +
          geom_violin(trim=FALSE) +
          geom_point(position = position_dodge(width = 0.1)) +
          # geom_dotplot(binaxis='y', stackdir='center', dotsize=1) +
          xlab(input$responseVar) +
          ylab(yaxis()) +
          ggtitle(title()) +
          theme_classic() +
          theme(legend.position = "none") +
          scale_fill_manual(values=groupColors[1:nlevels(response)])
          )
      })

      output$testTitle <- renderText({
        if(input$test == "lr"){
          "Linear Regression"
        } else {
          "Kruskal-Wallis Test"
        }
      })

      assumptions <- reactive({
        if(input$test == "lr"){
          gvlma::gvlma(fit())$GlobalTest[2:6] %>%
            do.call(rbind, .) %>%
            as.data.frame %>%
            mutate(
              Test = c(
                "Global Stat",
                "Skewness",
                "Kurtosis",
                "Link Function",
                "Heteroscedasticity"
              ),
              Decision = ifelse(
                pvalue < 0.05,
                "Assumptions NOT satisfied!",
                "Assumptions acceptable."
              )
            ) %>%
            mutate(Slope = Value) %>%
            dplyr::select(Test, pvalue, Decision) %>%
            mutate(pvalue = signif(as.numeric(pvalue), 2))
        } else {
          return(NULL)
        }
      })
      output$tbl = DT::renderDataTable( assumptions(), options = list(lengthChange = FALSE))

      output$lmAssumptions <- renderText({
        if(input$test == "lr"){
          if(assumptions()$Decision[1] == "Assumptions acceptable."){
            "Assumptions acceptable. Linear regression is recommended."
          } else {
            "Try a log2-transformation. If that doesn't work, the Kruskal-Wallis Test is recommended."
          }
        } else {
          return(NULL)
        }
      })

      output$test <- renderPrint({
        if(input$test == "lr"){
          coef(summary(fit()))
        } else {
          fit()
        }
      })

      output$descriptiveStat <- renderTable({
        summaryStat <- DF() %>%
          mutate(x = factor(x)) %>%
          group_by(x) %>%
          dplyr::summarise(Min = min(y, na.rm = TRUE),
            Median = median(y, na.rm = TRUE),
            Max = max(y, na.rm = TRUE),
            Mean = mean(y, na.rm = TRUE),
            SD  = sd(y, na.rm = TRUE)) %>%
          dplyr::rename(Group = x)

        if(input$test == "lr"){
          parametricDescStat <- signif(rbind(summaryStat$Mean, summaryStat$SD), 3)
          rownames(parametricDescStat) <- c("Mean", "SD")
          colnames(parametricDescStat) <- summaryStat$Group
          parametricDescStat %>%
            as.data.frame() %>%
            mutate(Measure = rownames(.))
        } else {
          nonparametricDescStat <- signif(rbind(summaryStat$Min, summaryStat$Median, summaryStat$Max), 3)
          rownames(nonparametricDescStat) <- c("Min", "Median", "Max")
          colnames(nonparametricDescStat) <- summaryStat$Group
          nonparametricDescStat %>%
            as.data.frame() %>%
            mutate(Measure = rownames(.))
        }
      })

      output$conclusion <- renderText({
        if(input$test == "lr"){
          ifelse(summary(aov(fit()))[[1]][1, "Pr(>F)"] < 0.05,
            paste0("There is a statistically significant difference (at p<0.05) ",
              input$var, " between the groups based an Analysis of Variance (see pairwise comparisons above)."),
            paste0("There is no statistically significant difference (at p<0.05) ",
              input$var, " between the groups based an Analysis of Variance (see pairwise comparisons above)."))
        } else {
          ifelse(fit()$p.value < 0.05,
            paste0("There is a statistically significant difference (at p<0.05) in ",
              input$var, " distributions between the groups."),
            paste0("There is no statistically significant difference (at p<0.05) in ",
              input$var, " distributions between the groups."))
        }
      })
    })
    ## @@@@@@@@@@@@@@@@@@@@@@@ Categorical variable panel @@@@@@@@@@@@@@@@@@@@@@@ ##
    updateRadioButtons(session, "catVar",
      label = "Choose from one of the following variables:",
      choices = setdiff(colnames(demoSplit$data.cat), input$responseVar),
      selected = setdiff(colnames(demoSplit$data.cat), input$responseVar)[1], inline = TRUE
    )
    observe({
      output$obsCounts <- renderGvis({
        d <- as.data.frame(as.data.frame.matrix(addmargins(table(response, demo[, input$catVar]))))
        gvisTable(cbind(' '=rownames(d), d))
      })
      output$obsFreq <- renderGvis({
        d <- as.data.frame(as.data.frame.matrix(apply(table(response, demo[, input$catVar]), 1, function(i){
          round(100*i/sum(i), 0)
        }) %>% t))
        gvisTable(cbind(' '=rownames(d), d))
      })
      output$chisqTest <- renderPrint({
        x <- input$catVar
        chisq.test(demo[, x], response)
      })
      output$chisqConclusion <- renderText({
        pval <- chisq.test(demo[, input$catVar], response)$p.value
        ifelse(pval < 0.05,
          paste0("There is a statistically significant association (at p<0.05) between ",
            input$catVar, " and ", input$responseVar, " (p-value = ", signif(pval, 3), ")."),
          paste0("There is no statistically significant association (at p<0.05) between ",
            input$catVar, " and ", input$responseVar, " (p-value = ", signif(pval, 3), ")."))
      })
      output$chisqTitle <- renderText({input$catVar})
    })


    ################################################################################
    #
    # Exploratory Data Analysis
    #
    ################################################################################
    observe({
    ## User interface
    output$eda = renderUI({
      myTabs <- lapply(names(getOmicsData()), function(i){
        tabPanel(i,
          fluidRow(column(
            4,
            sliderInput(
              paste("ncomp", i, sep="_"),
              h3("Number of components"),
              min = 2,
              max = 5,
              value = 1
            ),
            h3("PCA component plots", align = "center")
          ), column(
            8,
            h3("Percentage variation explained", align = "center"),
            verbatimTextOutput(paste("varExp", i, sep="_"))
          )),
          fluidRow(column(
            4,
            plotOutput(paste("pcaPlot", i, sep="_"), width = "100%")
          ),
            column(
              8,
              h3("Which metadata variables are associated with major sources of variation in the expression data?", align = "center"),
              plotly::plotlyOutput(paste("pcClinVarPlot", i, sep="_"), width = "100%")
            ))
        )
      })

      do.call(tabsetPanel, myTabs)
    })

    ## Backend
    lapply(names(getOmicsData()),
      function(i) {
        observeEvent(input[[paste("ncomp", i, sep="_")]], {
          pcs = prcomp(
              getOmicsData()[[i]],
              scale. = TRUE,
              center = TRUE,
              rank. = input[[paste("ncomp", i, sep="_")]]
            )
          output[[paste("varExp", i, sep="_")]] <- renderPrint({summary(pcs)})
          output[[paste("pcaPlot", i, sep="_")]] <- renderPlot({
            omicsBioAnalytics::pcaPairs(pcs = pcs, y = response, col=groupColors[1:nlevels(response)])
          })
          output[[paste("pcClinVarPlot", i, sep="_")]] <- renderPlotly({
            ggplotly(pcaHeatmap(pcs = pcs$x, demo = demo)) %>%
              layout(legend = list(
                orientation = "h",
                x = 0.1,
                y = -1
              ))
          })
        })
      }
    )
    })

    ################################################################################
    #
    # Differential Expression Analysis
    #
    ################################################################################
    observe({
      ## User interface
      output$dea = renderUI({
        myTabs <- lapply(names(getOmicsData()), function(i){
          tabPanel(i,
            fluidRow(
              column(6,
                radioButtons(paste("comparison", i, sep="_"),
                  label = "Comparison:",
                  choices = paste(levels(response)[1], setdiff(levels(response), levels(response)[1]), sep = " vs. "),
                  selected = paste(levels(response)[1], setdiff(levels(response), levels(response)[1]), sep = " vs. ")[1],
                  inline = TRUE)
              ),
              column(4,
                radioButtons(paste("deTest", i, sep="_"), "Test:",
                  c("OLS" = "ols", "LIMMA" = "limma", "LIMMA voom" = "vlimma"),
                  "limma",
                  inline = TRUE)),
              column(2,
                br(),
                bsButton(paste("search_button", i, sep="_"), label = "", icon = icon("question"), style = "color:gray", size = "extra-small"),
                bsPopover(id = paste("search_button", i, sep="_"), title = "Tests",
                  content = "OLS: Ordinary Least Squares, LIMMA: OLS with eBayes variance correction factor",
                  placement = "right",
                  trigger = "click",  options = NULL)
              ),
              column(6, verbatimTextOutput(paste("selection", i, sep="_")), style = 'padding: 15px 10px 0px 10px;'),
              column(6, align = "center",
                sliderInput(paste("fdr", i, sep="_"), h3("Select FDR threshold", align = "center"),
                  min = 0.05, max = 0.5, value = 0.15))),
            fluidRow(align = "center",
              column(6, plotly::plotlyOutput(paste("volcanoPlot", i, sep="_"))),
              column(6,
                div(style="display:inline-block;vertical-align:top;",
                  fluidRow(
                    column(8,
                      autocomplete_input(paste("variable_name", i, sep="_"), "Type variable name:",
                        colnames(getOmicsData()[[i]]), max_options = ncol(getOmicsData()[[i]]))),
                    column(4,
                      actionButton(paste("dePlotOps_button", i, sep="_"), "Plot options"),
                      bsModal(paste("dePlotOps", i, sep="_"), "Plot options", paste("dePlotOps_button", i, sep="_"), size = "large",
                        sliderInput(paste("dePlotOps_hjust", i, sep="_"), "horizontal justification:", min = 0, max = 1, value = 0.5),
                        sliderInput(paste("dePlotOps_vjust", i, sep="_"), "vertical justification:", min = 0, max = 1, value = 0.5),
                        sliderInput(paste("dePlotOps_xAngle", i, sep="_"), "x-axis text angle:", min = 0, max = 180, value = 0),
                        sliderInput(paste("dePlotOps_xSize", i, sep="_"), "x-axis text size:", min = 5, max = 20, value = 7),
                        sliderInput(paste("dePlotOps_ySize", i, sep="_"), "y-axis text size:", min = 5, max = 20, value = 7)
                        )
                      )
                  )),
                plotly::plotlyOutput(paste("boxplot", i, sep="_"))
              )),
            fluidRow(column(8, h4(textOutput(paste("statement", i, sep="_")))),
              column(4,
                actionButton(paste("button", i, sep="_"), "Significant variables", icon = icon("table")),
                bsModal(paste("modal", i, sep="_"), "Differentially expressed variables.", paste("button", i, sep="_"), size = "large",
                  DT::dataTableOutput(paste("sig", i, sep="_"))),
                downloadButton(paste("topTable", i, sep="_"), label = HTML("<span style='font-size:1em;'>Download</span>"), style="color: #fff; background-color: #619CFF; border-color: #2e6da4")

                )),
            hr(),
            conditionalPanel(
              condition = paste0("output.performPathwayAnalysis.indexOf('", i, "') != -1"),
            fluidRow(align = 'center',
              h1("Geneset Enrichment Analysis"),
              column(8, sliderInput(paste("enrichmentSlider", i, sep="_"), "Select number of pathways:", min=0, max=10, value=5, step=2)),
              column(4,
                actionButton(paste("pathwayEnrichmentOps_button", i, sep="_"), "Plot options"),
                bsModal(paste("pathwayEnrichmentOps", i, sep="_"), "Plot options", paste("pathwayEnrichmentOps_button", i, sep="_"), size = "large",
                  sliderInput(paste("pathwayEnrichmentOps_hjust", i, sep="_"), "horizontal justification:", min = 0, max = 1, value = 0.5),
                  sliderInput(paste("pathwayEnrichmentOps_vjust", i, sep="_"), "vertical justification:", min = 0, max = 1, value = 0.5),
                  sliderInput(paste("pathwayEnrichmentOps_xAngle", i, sep="_"), "x-axis text angle:", min = 0, max = 180, value = 0),
                  sliderInput(paste("pathwayEnrichmentOps_xSize", i, sep="_"), "x-axis text size:", min = 5, max = 20, value = 7),
                  sliderInput(paste("pathwayEnrichmentOps_ySize", i, sep="_"), "y-axis text size:", min = 5, max = 20, value = 7)
                )
              ),
              column(12,
                # sliderInput(paste("enrichmentSlider", i, sep="_"), "Select number of pathways:", min=0, max=10, value=5, step=2),
                # plotly::plotlyOutput(paste("pathwayEnrichment", i, sep="_")),
                plotOutput(paste("pathwayEnrichment", i, sep="_"), click = hoverOpts(id = paste("pathwayEnrichment_plot_click", i, sep="_"))),
                verbatimTextOutput(paste("pathwayEnrichment_info", i, sep="_")),
                downloadButton(paste("edgesGsetAll", i, sep="_"), label = HTML("<span style='font-size:1em;'>Download<br/>Enriched Pathways</span>")))),
              fluidRow(align = 'center',
                column(6, h1(paste0("Drugs that reverse expression of up-regulated ", i)),
                  sliderInput(paste("drugEnrichmentUpSlider", i, sep="_"), "Select number of compounds:", min=0, max=10, value=5, step=2),
                  plotly::plotlyOutput(paste("drugEnrichmentUp", i, sep="_"))),
                column(6, h1(paste0("Drugs that reverse expression of down-regulated ", i)),
                  sliderInput(paste("drugEnrichmentDownSlider", i, sep="_"), "Select number of compounds:", min=0, max=10, value=5, step=2),
                  plotly::plotlyOutput(paste("drugEnrichmentDown", i, sep="_"))),
                column(6,
                  downloadButton(paste("edgesPertUp", i, sep="_"), label = HTML("<span style='font-size:1em;'>Download compounds </span>"), style="color: #fff; background-color: #F8766D; border-color: #2e6da4")),
                column(6,
                  downloadButton(paste("edgesPertDown", i, sep="_"), label = HTML("<span style='font-size:1em;'>Download compounds </span>"), style="color: #fff; background-color: #619CFF; border-color: #2e6da4"))
              )
            )
            )
        })

        do.call(tabsetPanel, myTabs)
      })


      ## Backend
      lapply(names(getOmicsData()),
        function(i) {
          observeEvent(input[[paste("fdr", i, sep="_")]], {
          observeEvent(input[[paste("comparison", i, sep="_")]], {
          observeEvent(input[[paste("deTest", i, sep="_")]], {
            print(input[[paste("fdr", i, sep="_")]])
            print(input[[paste("comparison", i, sep="_")]])
            print(input[[paste("deTest", i, sep="_")]])
            req(input[[paste("fdr", i, sep="_")]])
            req(input[[paste("comparison", i, sep="_")]])
            req(input[[paste("deTest", i, sep="_")]])

            eset <- getOmicsData()[[i]]
            if(any(log2(eset) < 0)){
              updateRadioButtons(session, paste("deTest", i, sep="_"),
                label = "Test:",
                choices = c("OLS" = "ols", "LIMMA" = "limma"),
                selected = "limma",
                inline = TRUE
              )
            }

            selectedCoef <- which(levels(response) == sapply(strsplit(input[[paste("comparison", i, sep="_")]], " vs. "), function(i){ i[[2]]}))
            req(length(selectedCoef) == 1)

            design <- model.matrix(~response)
            top <- generateTopTable(eset, design, coefNumber = selectedCoef, test = input[[paste("deTest", i, sep="_")]])

            subsetTop <- reactive({
              top = top %>%
                mutate(Significant=ifelse(adj.P.Val < input[[paste("fdr", i, sep="_")]],
                  paste("FDR < ", input[[paste("fdr", i, sep="_")]]), "Not Sig"))
            })

            output[[paste("topTable", i, sep="_")]] <- downloadHandler(
              filename = function() {
                paste("Significant_variables_OmicsBioAnalytics_", i, "_FDR",
                  input[[paste("fdr", i, sep="_")]], "_", Sys.Date(), ".csv", sep="")
              },
              content = function(file) {
                write.csv(subsetTop(), file, row.names = FALSE)
              }
            )

            # volcano plot
            output[[paste("volcanoPlot", i, sep="_")]] <- renderPlotly({
              options(htmlwidgets.TOJSON_ARGS = NULL) ## import in order to run canvasXpress
              plot_ly(subsetTop(), x = ~logFC, y = ~sig,
                key = ~FeatureName, color=~Significant,
                colors=c("#F8766D", "grey"), marker = list(size=10),
                type = "scatter", source = paste("volcanoPlot", i, sep="_")) %>%
                layout(legend = list(orientation = 'h', xanchor="center", x=0.5, y=1.1),
                  xaxis = list(title = "log<sub>2</sub>FC"),
                  yaxis = list(title = "-log<sub>10</sub>(P-value)"))
            })

            variable_name <- reactiveValues(selected = colnames(eset)[1])
            observe({
              s <- unlist(event_data("plotly_click", source = paste("volcanoPlot", i, sep="_"))$key)
              if(length(s) >0){
                variable_name$selected <- s
              }
            })
            observeEvent(input[[paste("variable_name", i, sep="_")]], {
              variable_name$selected <- input[[paste("variable_name", i, sep="_")]]
            })


            # feature plot
            output[[paste("boxplot", i, sep="_")]] <- renderPlotly({
              if(variable_name$selected %in% colnames(eset)){
                ggplotly(data.frame(x = response, y = eset[, variable_name$selected]) %>%
                    ggplot(aes(x = x, y = y, fill = x)) +
                    geom_violin(trim=FALSE) +
                    xlab(input$responseVar) +
                    ylab(variable_name$selected) +
                    geom_jitter(shape=16, position=position_jitter(0.2)) +
                    ggtitle(paste(variable_name$selected, " vs. ", input$responseVar)) +
                    theme_classic() +
                    theme(legend.position = "none") +
                    scale_fill_manual(values=groupColors[1:length(unique(response))]) +
                    theme(axis.text.x = element_text(angle = input[[paste("dePlotOps_xAngle", i, sep="_")]],
                      hjust = input[[paste("dePlotOps_hjust", i, sep="_")]],
                      vjust = input[[paste("dePlotOps_vjust", i, sep="_")]],
                      size = input[[paste("dePlotOps_xSize", i, sep="_")]]),
                      axis.text.y = element_text(size = input[[paste("dePlotOps_ySize", i, sep="_")]]))
                )
              } else {
                omicsBioAnalytics::empty_plot("Select point or type variable name.")
              }
            })

            # selected varibales
            observeEvent(input[[paste("variable_name", i, sep="_")]],{
              output[[paste("selection", i, sep="_")]] <- renderPrint({
                # detect triggers
                if (!(variable_name$selected %in% colnames(eset))) {
                  "Click on a point on the volcano plot"
                } else {
                  # cat(paste("You selected:", s$key, "\n\n"));
                  # cat(paste("Fold-change = ", signif(s$x, 3), "\n P-value = ", signif(10^-s$y, 3)))
                  filter(top, FeatureName == variable_name$selected)[, c("FeatureName", "logFC", "AveExpr", "P.Value", "adj.P.Val")]
                }
              })
            })

            # statement
            output[[paste("statement", i, sep="_")]] <- renderText({
              paste("There are ", sum(top$adj.P.Val < input[[paste("fdr", i, sep="_")]]), "significant ", i, ". ",
                sum(top$logFC > 0 & top$adj.P.Val < input[[paste("fdr", i, sep="_")]]), i, " were up-regulated in subjects whereas",
                sum(top$logFC < 0 & top$adj.P.Val < input[[paste("fdr", i, sep="_")]]), i, " were down-regulated.")
            })

            # table of significant featuers
            output[[paste("sig", i, sep="_")]] <-  DT::renderDataTable( dplyr::filter(subsetTop(), Significant != "Not Sig") %>%
                mutate(logFC = signif(logFC, 2),
                  P.Value = signif(P.Value, 2),
                  adj.P.Val = signif(adj.P.Val, 2),
                  sig = signif(sig, 2)) %>%
                dplyr::select(FeatureName, logFC, P.Value, adj.P.Val))

            ## Differential pathway analysis
            if(i %in% performPathwayAnalysis()){
              pathwaydbs <- c("Jensen_DISEASES", "KEGG_2019_Human", "WikiPathways_2019_Human")
              sigTable <- dplyr::filter(subsetTop(), Significant != "Not Sig")
              up <- sigTable$logFC[sigTable$logFC > 0 ]
              names(up) <- sigTable$FeatureName[sigTable$logFC > 0 ]
              down <- sigTable$logFC[sigTable$logFC < 0 ]
              names(down) <- sigTable$FeatureName[sigTable$logFC < 0 ]
              all <- c(up, down)
              # dbs <- listEnrichrDbs()

              # Run Pathway Analysis using EnrichR
              if(length(all) > 1){
                # enrichment analysis for all genes/proteins
                enrichedAll <- enrichr(names(all), pathwaydbs)
                edgesGsetAll <- do.call(rbind, enrichedAll) %>%
                  dplyr::mutate(database = rep(names(enrichedAll), sapply(enrichedAll, nrow))) %>%
                  dplyr::filter(Adjusted.P.value < input[[paste("fdr", i, sep="_")]])
                edgesGsetAll$int <- as.numeric(sapply(strsplit(as.character(edgesGsetAll$Overlap), "/"), function(i) i[1]))

                edgesGsetAll_list <- strsplit(edgesGsetAll$Genes, ";")
                names(edgesGsetAll_list) <- edgesGsetAll$Term

                updateSliderInput(session, paste("enrichmentSlider", i, sep="_"), min = 1, max = length(edgesGsetAll_list), value = min(5, round(length(edgesGsetAll_list)/2, 0)))

              } else {
                edgesGsetAll <- data.frame(msg = "No pathways were identified")
              }
              # plot heatmap of enriched pathway
              # output[[paste("pathwayEnrichment", i, sep="_")]] <- renderPlotly({
              #   if(nrow(edgesGsetAll) > 1 & i %in% performPathwayAnalysis()){
              #     options(htmlwidgets.TOJSON_ARGS = NULL) ## import in order to run canvasXpress
              #     ggplotly(
              #       drugHeatmap(fc = all,
              #         genesetList = edgesGsetAll_list[1:input[[paste("enrichmentSlider", i, sep="_")]]],
              #         col = c("#F8766D", "#619CFF"), datasetName = i, GeneSetName = "Pathways")
              #     )
              #   } else {
              #     omicsBioAnalytics::empty_plot(paste0("No enriched pathways at an FDR = ", input[[paste("fdr", i, sep="_")]]))
              #   }
              # })
              print(edgesGsetAll)
              output[[paste("pathwayEnrichment", i, sep="_")]] <- renderPlot({
                if(nrow(edgesGsetAll) > 1 & i %in% performPathwayAnalysis()){
                  options(htmlwidgets.TOJSON_ARGS = NULL) ## import in order to run canvasXpress
                  edgesGsetAll[1:input[[paste("enrichmentSlider", i, sep="_")]], ] %>%
                    mutate(Term = factor(as.character(Term), as.character(Term))) %>%
                  ggplot(aes(x = Term, y = int, color = Term)) + geom_point(size = 5) +
                    geom_segment(aes(xend = Term, color = Term), yend = 0, size = 1) +
                    scale_y_log10() +
                    ylab("Overlap") +
                    theme_classic() +
                    theme(legend.position = "none") +
                    scale_fill_manual(values=groupColors[1:length(unique(response))]) +
                    theme(axis.text.x = element_text(angle = input[[paste("pathwayEnrichmentOps_xAngle", i, sep="_")]],
                      hjust = input[[paste("pathwayEnrichmentOps_hjust", i, sep="_")]],
                      vjust = input[[paste("pathwayEnrichmentOps_vjust", i, sep="_")]],
                      size = input[[paste("pathwayEnrichmentOps_xSize", i, sep="_")]]),
                      axis.text.y = element_text(size = input[[paste("pathwayEnrichmentOps_ySize", i, sep="_")]]))
                } else {
                  omicsBioAnalytics::empty_plot(paste0("No enriched pathways at an FDR = ", input[[paste("fdr", i, sep="_")]]))
                }
              })
              output[[paste("pathwayEnrichment_info", i, sep="_")]] <- renderPrint({
                if(!is.null(input[[paste("pathwayEnrichment_plot_click", i, sep="_")]])){
                  hover=input[[paste("pathwayEnrichment_plot_click", i, sep="_")]]
                  print(nearPoints(edgesGsetAll, hover, threshold = 5, maxpoints = 1)[,c("Term", "Overlap", "P.value", 'Adjusted.P.value', "Genes", "database")])
                }
              })


              # Run EnrichR for drug enrichment analysis
              if(length(up) > 1){
                # enrichment analysis for up-regulated genes/proteins
                enrichedUp <- enrichr(names(up), "LINCS_L1000_Chem_Pert_down")
                edgesPertUp <- do.call(rbind, enrichedUp) %>%
                  dplyr::mutate(database = rep(names(enrichedUp), sapply(enrichedUp, nrow))) %>%
                  dplyr::filter(Adjusted.P.value < input[[paste("fdr", i, sep="_")]])
                print("edgesPertUp")
                print(edgesPertUp)
                edgesPertUp_list <- strsplit(edgesPertUp$Genes, ";")
                names(edgesPertUp_list) <- edgesPertUp$Term

                updateSliderInput(session, paste("drugEnrichmentUpSlider", i, sep="_"), min = 1, max = length(edgesPertUp_list), value = min(5, round(length(edgesPertUp_list)/2, 0)))

              } else {
                edgesPertUp <- data.frame(msg = "No pathways were identified")
              }
              if(length(down) > 1){
                # enrichment analysis for down-regulated genes/proteins
                enrichedDown <- enrichr(names(down), "LINCS_L1000_Chem_Pert_up")
                edgesPertDown <- do.call(rbind, enrichedDown) %>%
                  dplyr::mutate(database = rep(names(enrichedDown), sapply(enrichedDown, nrow))) %>%
                  dplyr::filter(Adjusted.P.value < input[[paste("fdr", i, sep="_")]])

                edgesPertDown_list <- strsplit(edgesPertDown$Genes, ";")
                names(edgesPertDown_list) <- edgesPertDown$Term

                updateSliderInput(session, paste("drugEnrichmentDownSlider", i, sep="_"), min = 1, max = length(edgesPertDown_list), value = min(5, round(length(edgesPertDown_list)/2, 0)))

              } else {
                edgesPertDown <- data.frame(msg = "No pathways were identified")
              }

              # plot heatmaps for enriched drugs
              output[[paste("drugEnrichmentUp", i, sep="_")]] <- renderPlotly({
                if(nrow(edgesPertUp) > 1 & i %in% performPathwayAnalysis()){
                  options(htmlwidgets.TOJSON_ARGS = NULL) ## import in order to run canvasXpress
                  ggplotly(
                    drugHeatmap(fc = up,
                      genesetList = edgesPertUp_list[1:input[[paste("drugEnrichmentUpSlider", i, sep="_")]]],
                      col = "#F8766D", datasetName = i, GeneSetName = "LINCS L1000 Chemical Perturbations Down")
                  )
                } else {
                  omicsBioAnalytics::empty_plot(paste0("No enriched compounds at an FDR = ", input[[paste("fdr", i, sep="_")]]))
                }
              })

              output[[paste("drugEnrichmentDown", i, sep="_")]] <- renderPlotly({
                if(nrow(edgesPertDown) > 1 & i %in% performPathwayAnalysis()){
                  options(htmlwidgets.TOJSON_ARGS = NULL) ## import in order to run canvasXpress
                  ggplotly(
                    drugHeatmap(fc = down,
                      genesetList = edgesPertDown_list[1:input[[paste("drugEnrichmentDownSlider", i, sep="_")]]],
                      col = "#619CFF", datasetName = i, GeneSetName = "LINCS L1000 Chemical Perturbations Up")
                  )
                } else {
                  omicsBioAnalytics::empty_plot(paste0("No enriched compounds at an FDR = ", input[[paste("fdr", i, sep="_")]]))
                }
              })

              # Download buttons
              output[[paste("edgesGsetAll", i, sep="_")]] <- downloadHandler(
                filename = function() {
                  paste("EnrichmentAnalysis_OmicsBioAnalytics_", i, "_FDR",
                    input[[paste("fdr", i, sep="_")]], "_", Sys.Date(), ".csv", sep="")
                },
                content = function(file) {
                  write.csv(edgesGsetAll, file, row.names = FALSE)
                }
              )
              output[[paste("edgesPertUp", i, sep="_")]] <- downloadHandler(
                filename = function() {
                  paste("Compounds_that_reverse_expression_of_upregulated_variables_OmicsBioAnalytics_", i, "_FDR",
                    input[[paste("fdr", i, sep="_")]], "_", Sys.Date(), ".csv", sep="")
                },
                content = function(file) {
                  write.csv(edgesPertUp, file, row.names = FALSE)
                }
              )
              output[[paste("edgesPertDown", i, sep="_")]] <- downloadHandler(
                filename = function() {
                  paste("Compounds_that_reverse_expression_of_downregulated_variables_OmicsBioAnalytics_", i, "_FDR",
                    input[[paste("fdr", i, sep="_")]], "_", Sys.Date(), ".csv", sep="")
                },
                content = function(file) {
                  write.csv(edgesPertDown, file, row.names = FALSE)
                }
              )

              }

          })
          })
          })
        }
      )})

    ################################################################################
    #
    # Biomarker Discovery Analysis
    #
    ################################################################################
    # update sidebar: let user choose which datasets to use to build models
    updateCheckboxGroupInput(session, "checkGroup_single",
      label = "Build biomarker panel(s) using dataset(s):",
      choices = names(getOmicsData()),
      selected = names(getOmicsData())
    )
    updateCheckboxGroupInput(session, "checkGroup_ensemble",
      label = "Build ensemble of biomarker panels using dataset(s):",
      choices = names(getOmicsData()),
      selected = names(getOmicsData())
    )
    updateCheckboxGroupInput(session, "selectedGroups",
      label = paste0("Select two groups from the ", input$responseVar, " response variable to compare (required):"),
      choices = levels(response),
      selected = levels(response)[1:2],
      inline = TRUE
    )

    ## Classification performances
    observeEvent(input$build, {
      errMsg <- reactive({validate(
        need(length(input$selectedGroups) > 1, "Please only select two groups."),
        need(length(input$selectedGroups) < 3, "Please only select two groups."),
        need(length(input$checkGroup_single) > 0, "Please select at least one dataset to build a classifier."),
        need(length(input$checkGroup_ensemble) > 0, "Please select at least one dataset to build a ensemble classifier.")
      )})
      output$errMsg <- renderUI({
        errMsg()
      })
      req(length(input$selectedGroups) == 2)
      req(length(input$checkGroup_single) > 0)
      req(length(input$checkGroup_ensemble) > 0)
      req(length(response) > 0 )
      req(sum(input$selectedGroups %in% response)>0)
      print("response")
      print(response)
      print(input$selectedGroups)
      print(response[response %in% input$selectedGroups])

      ## reduce data to two groups
      if(nlevels(response) > 2){
        subset_response <- isolate({droplevels(response[response %in% input$selectedGroups])})
        subset_eset <- isolate({lapply(getOmicsData(), function(i){
          i[response %in% input$selectedGroups, ]
        })})
      } else {
        subset_response <- isolate({response})
        subset_eset <- isolate({getOmicsData()})
      }

      # if response is coded with numbers only, change it to a valid R variable
      subset_response <- factor(make.names(subset_response))
      isolate(alphaMin <- input$alpha[1])
      isolate(alphaMax <- input$alpha[2])
      isolate(alphalength <- input$alphaGrid)
      isolate(kfolds <- input$cvScheme)
      isolate(n_repeats <- input$n_repeats)
      isolate(single <- as.character(input$checkGroup_single))
      isolate(ensem <- as.character(input$checkGroup_ensemble))
      isolate(datasets <- unique(c(single, ensem)))

      ## set control parameters
      if(kfolds == "fiveFold"){
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

      withProgress(message = 'Constructing models.',
        detail = 'This may take a while...', value = 0, {
          mods <- vector("list", length(datasets))
          names(mods) <- datasets

          for(dat in datasets){
            # Increment the progress bar, and update the detail text.
            incProgress(1/length(datasets), detail = paste("Building ", dat, " model..."))

            mods[[dat]] <- caret::train(x=subset_eset[[dat]], y=subset_response,
              preProc=c("center", "scale"),
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
            mutate(panel = rep(names(mods), each=nrow(mods[[1]]$pred)))
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
            dplyr::summarise(mean_tpr = mean(tpr), sd_fpr=sd(tpr)) %>%
            dplyr::inner_join(x = perf, y = ., by = c("panel", "alpha", "lambda"))
        })


      colors <- c("#2ca02c", "#1f77b4", "black", "#ff7f0e","#9467bd", "#d62728")
      output$rocPlot <- renderPlotly({
        options(htmlwidgets.TOJSON_ARGS = NULL) ## import in order to run canvasXpress
        plot_ly(rocTable, x = ~fpr, y = ~mean_tpr, type = 'scatter', mode = 'lines',
          linetype = ~panel, color = ~panel, source = "auc", key = ~panel, colors = colors) %>%
          layout(title = 'ROC curves',
            xaxis = list(title = 'False Positive Rate (FPR)'),
            yaxis = list (title = 'Average True Positive Rate (TPR)')) %>% layout(legend = list(x = 0.6, y = 0.2))
      })

      output$aucs <- DT::renderDataTable({
        DT::datatable(
          perf %>%
            mutate(alpha = signif(alpha, 2), lambda = signif(lambda, 2),
              Mean = signif(Mean, 2), SD = signif(SD, 2)),
          selection = list(target = "row+column"),
          options = list(pageLength = nrow(perf), dom = "ft", digits=4))
      }, width = "50%")
      proxy = DT::dataTableProxy('aucs')
      # highlight rows that are selected on plotly output
      observe({

        event.data = plotly::event_data("plotly_hover", source = "auc")

        if(is.null(event.data)) {
          rowNums <- NULL
        } else {
          rowNums <- row.names(as.data.frame(perf)[perf$panel %in% as.character(event.data$key[[1]]),])
        }

        proxy %>% DT::selectRows(as.numeric(rowNums))
      })

      ## Fit single dataset models
      singlePanelMods <- lapply(1 : length(single), function(i){
        dataset <- single[i]
        alpha <- subset(perf, panel == dataset)$alpha
        lambda <- subset(perf, panel == dataset)$lambda
        fit <- glmnet(x=as.matrix(subset_eset[[dataset]]), y=subset_response, alpha=alpha, lambda=lambda, family = "binomial")
        Coefficients <- coef(fit, s = lambda)
        Active.Index <- which(Coefficients[, 1] != 0)
        data.frame(coef = abs(Coefficients[Active.Index, ])) %>%
          mutate(features = rownames(.)) %>%
          dplyr::slice(-1) %>%
          arrange(coef) %>%
          mutate(features = factor(as.character(features), as.character(features))) %>%
          mutate(panel = dataset)
      })
      names(singlePanelMods) <- single
      ### single panel features
      singlePanel <- lapply(singlePanelMods, function(i){
        as.character(i$features)
      })

      barColors <- c("#1f77b4", "#ff7f0e", "#2ca02c", "#d62728","#9467bd")
      names(barColors) <- names(subset_eset)
      output$singlePanel <- renderPlotly({
        options(htmlwidgets.TOJSON_ARGS = NULL)
        f1 <- list(
          family = "Arial, sans-serif",
          size = 8,
          color = "lightgrey"
        )
        f2 <- list(
          family = "Old Standard TT, serif",
          size = 8,
          color = "black"
        )
        a <- list(
          title = "AXIS TITLE",
          titlefont = f1,
          showticklabels = TRUE,
          tickangle = 45,
          tickfont = f2,
          exponentformat = "E"
        )
        do.call(rbind, singlePanelMods) %>%
          group_by(panel) %>%
          do(
            p = plot_ly(., x = ~features, y = ~coef, marker = list(color = barColors[unique(.$panel)])) %>%
              layout(xaxis = a, yaxis = a, annotations = list(
                x = 0.5, y = 1.05, text = ~unique(panel), showarrow = F, xref='paper', yref='paper'
              ))
          ) %>%
          subplot(nrows = nrow(.), margin = 0.05) %>%
          layout(showlegend=FALSE,
            margin = list(l = 50, r = 100, b = 150, t = 50, pad = 4))
      })

      ## Fit ensemble dataset models
      ensemblePanelMods <- lapply(1 : length(ensem), function(i){
        dataset <- ensem[i]
        alpha <- subset(perf, panel == "Ensemble")$alpha
        lambda <- subset(perf, panel == "Ensemble")$lambda
        fit <- glmnet(x=as.matrix(subset_eset[[dataset]]), y=subset_response, alpha=alpha, lambda=lambda, family = "binomial")
        Coefficients <- coef(fit, s = lambda)
        Active.Index <- which(Coefficients[, 1] != 0)
        data.frame(coef = abs(Coefficients[Active.Index, ])) %>%
          mutate(features = rownames(.)) %>%
          dplyr::slice(-1) %>%
          arrange(coef) %>%
          mutate(features = factor(as.character(features), as.character(features))) %>%
          mutate(panel = dataset)
      })
      names(ensemblePanelMods) <- ensem
      ### ensemble panel features
      ensemblePanel <- lapply(ensemblePanelMods, function(i){
        as.character(i$features)
      })

      output$biomarkerPanels <- downloadHandler(
        filename = function() {
          paste("BiomarkerPanels_multiomics_HFhospitalizations_", Sys.Date(), ".tsv", sep="")
        },
        content = function(file) {
          write_tsv(rbind(data.frame(dataset = rep(names(singlePanel), sapply(singlePanel, length)),
            biomarkers = unlist(singlePanel),
            Panel = "Individual Panels"),
            data.frame(dataset = rep(names(ensemblePanel), sapply(ensemblePanel, length)),
              biomarkers = unlist(ensemblePanel),
              Panel = "Ensemble Panel")), file)
        }
      )

      output$ensemblePanel <- renderPlotly({
        options(htmlwidgets.TOJSON_ARGS = NULL)
        f1 <- list(
          family = "Arial, sans-serif",
          size = 8,
          color = "lightgrey"
        )
        f2 <- list(
          family = "Old Standard TT, serif",
          size = 8,
          color = "black"
        )
        a <- list(
          title = "AXIS TITLE",
          titlefont = f1,
          showticklabels = TRUE,
          tickangle = 45,
          tickfont = f2,
          exponentformat = "E"
        )
        do.call(rbind, ensemblePanelMods) %>%
          group_by(panel) %>%
          do(
            p = plot_ly(., x = ~features, y = ~coef, marker = list(color = barColors[unique(.$panel)])) %>%
              layout(xaxis = a, yaxis = a, annotations = list(
                x = 0.5, y = 1.05, text = ~unique(panel), showarrow = F, xref='paper', yref='paper'
              ))
          ) %>%
          subplot(nrows = nrow(.), margin = 0.05) %>%
          layout(showlegend=FALSE,
            margin = list(l = 50, r = 100, b = 150, t = 50, pad = 4))
      })

      overlap <- lapply(intersect(single, ensem), function(i){
        intersect(singlePanel[[i]], ensemblePanel[[i]])
      })
      names(overlap) <- intersect(single, ensem)

      output$panelN <- renderPlot({
        panels <-  singlePanel
        panels$Ensemble <- as.character(unlist(ensemblePanel))

        Input <- UpSetR::fromList(panels)
        UpSetR::upset(Input, sets = colnames(Input))
      })

      ## PCA plot
      observe({
        updateRadioButtons(session, "pcaBasePanelRadioButtons",
          label = "Select panel",
          choices = single,
          inline = TRUE)
      })
      ### Base classifier
      output$pcaBasePanel <- renderCanvasXpress({
        dataset <- subset_eset[[input$pcaBasePanelRadioButtons]]
        variables <- singlePanel[[input$pcaBasePanelRadioButtons]]
        grouping <- data.frame(Group = subset_response)
        rownames(dataset) <- rownames(grouping) <- paste0("subj", 1:nrow(grouping))

        if(length(variables) > 2){
          pc <- prcomp(dataset[, variables, drop = FALSE], scale. = TRUE, center = TRUE)

          canvasXpress(data = pc$x[, 1:3], digits = 50,
            varAnnot  = grouping,
            colorBy   = "Group",
            ellipseBy = "Group",
            graphType = "Scatter3D",
            colorScheme = "Set2",
            colors = groupColors[1:nlevels(subset_response)],
            xAxisTitle = paste0("PC1 (", round(100*summary(pc)$importance["Proportion of Variance","PC1"], 0), "%)"),
            yAxisTitle = paste0("PC2 (", round(100*summary(pc)$importance["Proportion of Variance","PC2"], 0), "%)"),
            zAxisTitle = paste0("PC3 (", round(100*summary(pc)$importance["Proportion of Variance","PC3"], 0), "%)"))
        } else if(length(variables) == 2){
          canvasXpress(
            data=dataset[, variables, drop = FALSE],
            varAnnot  = grouping,
            colorBy   = "Group",
            colorScheme = "Set2",
            colors = groupColors[1:nlevels(subset_response)],
            graphType="ScatterBubble2D",
            size=list(1)
          )
        } else {
          y=t(as.data.frame(dataset[, variables, drop=FALSE]))
          x = data.frame(Group = grouping)
          colnames(y) <- rownames(x) <- paste0("subj", 1:ncol(y))

          canvasXpress(
            data=y,
            smpAnnot=x,
            colorBy   = "Group",
            axisTitleFontStyle="italic",
            graphOrientation="vertical",
            graphType="Boxplot",
            jitter=TRUE,
            colorScheme = "Set2",
            colors = groupColors[1:nlevels(subset_response)],
            legendBox=FALSE,
            plotByVariable=TRUE,
            showBoxplotOriginalData=TRUE,
            smpLabelRotate=90,
            smpTitle="Response",
            smpTitleFontStyle="italic",
            title=variables,
            #height = 300,
            afterRender=list(list("groupSamples", list("Group")))
          )
        }
      })

      ### Ensemble classifier
      output$pcaEnsemblePanel <- renderCanvasXpress({
        dataset <- mapply(function(x, y){
          x[, y]
        }, x = subset_eset[ensem], y = ensemblePanel) %>%
          do.call(cbind, .)
        grouping <- data.frame(Group = subset_response)
        rownames(dataset) <- rownames(grouping) <- paste0("subj", 1:nrow(grouping))

        pc <- prcomp(dataset, scale. = TRUE, center = TRUE)
        canvasXpress(data      = pc$x[, 1:3], digits = 50,
          varAnnot  = grouping,
          colorBy   = "Group",
          ellipseBy = "Group",
          colorScheme = "Set2",
          colors = groupColors[1:nlevels(subset_response)],
          graphType = "Scatter3D",
          xAxisTitle = paste0("PC1 (", round(100*summary(pc)$importance["Proportion of Variance","PC1"], 0), "%)"),
          yAxisTitle = paste0("PC2 (", round(100*summary(pc)$importance["Proportion of Variance","PC2"], 0), "%)"),
          zAxisTitle = paste0("PC3 (", round(100*summary(pc)$importance["Proportion of Variance","PC3"], 0), "%)"))
      })

      ## Heatmap of selected variables
      observe({
        updateRadioButtons(session, "heatmapBasePanelRadioButtons",
          label = "Select panel",
          choices = single,
          inline = TRUE)
      })
      ### Base classifier
      output$heatmapBasePanel <- renderCanvasXpress({
        dataset <- subset_eset[[input$heatmapBasePanelRadioButtons]]
        variables <- singlePanel[[input$heatmapBasePanelRadioButtons]]
        y <- t(scale(dataset[, variables, drop=FALSE]))
        y[y < -2] <- -2
        y[y > 2] <- 2
        x = data.frame(Group = subset_response)
        rownames(x) <- colnames(y) <- paste0("subj", 1:nrow(x))
        z = data.frame(dataset = rep(input$heatmapBasePanelRadioButtons, length(variables)))
        rownames(z) <- rownames(y)

        if(length(variables) > 1){
          canvasXpress(
            data=y,
            smpAnnot=x,
            varAnnot=z,
            colors = c('blue', 'red'),
            colorSpectrum=list("magenta", "blue", "black", "red", "gold"),
            colorSpectrumZeroValue=0,
            graphType="Heatmap",
            smpOverlays=list("Group"),
            heatmapIndicatorHeight=50,
            heatmapIndicatorHistogram=TRUE,
            heatmapIndicatorPosition="topLeft",
            heatmapIndicatorWidth=60,
            samplesClustered=TRUE,
            showTransition=TRUE,
            variablesClustered=TRUE)
        } else {
          y=t(as.data.frame(dataset[, variables, drop=FALSE]))
          x = data.frame(Group = subset_response)
          colnames(y) <- rownames(x) <- paste0("subj", 1:ncol(y))

          canvasXpress(
            data=y,
            smpAnnot=x,
            colorBy   = "Group",
            axisTitleFontStyle="italic",
            graphOrientation="vertical",
            graphType="Boxplot",
            jitter=TRUE,
            colorScheme = "Set2",
            colors = c('blue', 'red'),
            legendBox=FALSE,
            plotByVariable=TRUE,
            showBoxplotOriginalData=TRUE,
            smpLabelRotate=90,
            smpTitle="Response",
            smpTitleFontStyle="italic",
            title=variables,
            #height = 300,
            afterRender=list(list("groupSamples", list("Group"))))
        }
      })

      ### Ensemble classifier
      output$heatmapEnsemblePanel <- renderCanvasXpress({
        y = mapply(function(x, y){
          x[, y, drop=FALSE]
        }, x = subset_eset[names(ensemblePanel)], y = ensemblePanel, SIMPLIFY = FALSE) %>%
          do.call(cbind, .) %>%
          scale(.) %>%
          t
        y[y < -2] <- -2
        y[y > 2] <- 2
        x = data.frame(Group = subset_response)
        rownames(x) <- colnames(y) <- paste0("subj", 1:nrow(x))
        z = data.frame(dataset = rep(names(ensemblePanel), sapply(ensemblePanel, length)))
        rownames(z) <- rownames(y)

        canvasXpress(
          data=y,
          smpAnnot=x,
          varAnnot=z,
          colors = groupColors[1:nlevels(subset_response)],
          colorKey=list(dataset=barColors[names(ensemblePanel)], Group=groupColors[1:nlevels(subset_response)]),
          colorSpectrum=list("magenta", "blue", "black", "red", "gold"),
          colorSpectrumZeroValue=0,
          graphType="Heatmap",
          smpOverlays=list("Group"),
          varOverlayProperties=list(dataset=list(position="top")),
          varOverlays=list("dataset"),
          heatmapIndicatorHeight=50,
          heatmapIndicatorHistogram=TRUE,
          heatmapIndicatorPosition="topLeft",
          heatmapIndicatorWidth=60,
          samplesClustered=TRUE,
          showTransition=TRUE,
          variablesClustered=TRUE)
      })

      # Enrichment analysis
      dat <- mapply(function(x, y){
        x[, y, drop=FALSE]
      }, x = subset_eset[names(ensemblePanel)], y = ensemblePanel, SIMPLIFY = FALSE) %>%
        do.call(cbind, .)
      colnames(dat) <- sapply(strsplit(colnames(dat), "\\."), function(i) paste(i[-1], collapse = "_"))
      pairs <- split(t(combn(colnames(dat), 2)), 1:nrow(t(combn(colnames(dat), 2))))

      output$biomarkerSig <- renderVisNetwork({
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
              filter(abs(cor) > input$corCutoff) %>%
              mutate(color = ifelse(cor > 0, "salmon", "blue"))
            print(summary(edgesCor$cor))

            ## gene set enrichment analysis
            # dbs <- listEnrichrDbs()
            dbs <- c("Jensen_DISEASES", "KEGG_2019_Human", "WikiPathways_2019_Human")
            enriched <- enrichr(unlist(ensemblePanel), dbs)

            edgesGset <- do.call(rbind, enriched) %>%
              mutate(database = rep(names(enriched), sapply(enriched, nrow)))
            if(sum(edgesGset$Adjusted.P.value < input$bioFDR) < 1){
              edges <- edgesCor
            } else {
              sig_pathways <- do.call(rbind, enriched) %>%
                mutate(database = rep(names(enriched), sapply(enriched, nrow))) %>%
                filter(Adjusted.P.value < input$bioFDR) %>%
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
                if(i %in% ensemblePanel[[j]]){
                  j
                } else {
                  return(NA)
                }
              })), collapse="_")
            }) %>% unlist()
            group[group == ""] <- "pathway"

            nodes$group <- group
            nodes$label <- nodes$id
            # nodes$shape <- "pathway.png"

            shapes <- c("square", "triangle", "circle", "dot", "star",
              "ellipse", "database", "text", "diamond")
            if(length(unique(group)) < length(shapes)){
              nodeShapes <- c(shapes[1:length(unique(group))], "box")
              names(nodeShapes) <- c(names(setdiff(unique(group), "pathway")), "pathway")
            } else {
              nodeShapes <- rep('circle', length(unique(group)))
              names(nodeShapes) <- unique(group)
            }

            if(length(unique(group)) < length(shapes)){
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



    # show analysis sidemenu when run analysis button is pressed
    output$analysisRan <- reactive({
      returnedValue = TRUE
      return(returnedValue)
    })
    outputOptions(output, "analysisRan", suspendWhenHidden = FALSE)
  })

  ################################################################################
  #
  # Generate a report
  #
  ################################################################################
  # Generate the PNG
  # png(paste0(tempdir(), "/", "pca.png"), width = 400, height = 300)
  # hist(rnorm(50), main = "PCA plot")
  # dev.off()
  # png(paste0(tempdir(), "/", "volcano_mrna.png"), width = 400, height = 300)
  # hist(rnorm(50), main = "Volcano plot")
  # dev.off()
  #
  # x <- reactiveValues(imgs = list.files(tempdir()))
  observe({
    updateSelectInput(session,
      inputId = "figs",
      label = "Figures:",
      choices = c("None", gsub(".png", "", grep(".png", list.files(tempdir()), value = TRUE))),
      selected = "None"
    )
  })

  observeEvent(input$cFig, {
    inFile <- input$cFig
    if (is.null(inFile))
      return()
    file.copy(inFile$datapath, file.path(tempdir(), inFile$name) )
    updateSelectInput(session,
      inputId = "figs",
      label = "Figures:",
      choices = c("None", gsub(".png", "", grep(".png", list.files(tempdir()), value = TRUE))),
      selected = "None"
    )
  })

  list_of_elements <- list()
  tracker <- reactiveValues(section = list(), fig = list())

  observeEvent(input$h1Btn, {
    nr <- input$h1Btn
    print(paste0("add item: ", nr))
    id <- paste0("input",input$h1Btn)

    tracker$section[[id]]$txt <- input$markdowninput
    tracker$section[[id]]$fig <- input$figs

    element <- div(style="display: flex; justify-content: space-between; align-items: center; width: 100%",
      id = paste0("newInput",nr),
      input$markdowninput,
      br(),
      ifelse(input$figs == "None", "", paste("Attached Fig: ", input$figs)),
      actionButton(paste0('removeBtn',nr), 'Remove')
    )
    list_of_elements[[id]] <<- element

    observeEvent(input[[paste0('removeBtn',nr)]],{
      print(paste0("delete item: ", nr))
      shiny::removeUI(
        selector = paste0("#newInput",nr)
      )
      list_of_elements <<- list_of_elements[names(list_of_elements) != paste0("input",nr)]

      output$tbl = renderUI({
        fluidRow(
          column(
            width = 12,
            sortable::rank_list(
              text = "Drag the items in any desired order",
              labels = list_of_elements,
              input_id = "rank_list_1"
            )
          )
        )
      })
    })


    output$dragAndDrop = renderUI({
      fluidRow(
        column(
          width = 12,
          sortable::rank_list(
            text = "Drag the items in any desired order",
            labels = list_of_elements,
            input_id = "rank_list_1"
          ),
          verbatimTextOutput("results")
        )
      )
    })

  })

  ## drag and drop is fixed
  observe({
    list_of_elements <<- list_of_elements[input$rank_list_1]
  })

  output$report <- downloadHandler(
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
      params <- list(title = input$title,
        author = input$author,
        affiliation = input$aff,
        section = tracker$section,
        ord = names(list_of_elements))

      # Knit the document, passing in the `params` list, and eval it in a
      # child of the global environment (this isolates the code in the document
      # from the code in this app).
      withProgress(message = 'Download in progress',
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

  output$myImage <- renderImage({

    # Return a list containing the filename
    list(src = paste0(tempdir(), "/", input$figs, ".png"),
      contentType = 'image/png',
      width = 400,
      height = 300,
      alt = "No image was selected from side panel.")
  }, deleteFile = FALSE)

  ################################################################################
  #
  # Voice-enabled analytics
  #
  ################################################################################
  if(alexaSkillExists){
    observeEvent(input$alexa, {
      # # req(input$demo); req(input$omicsData);
      # errMsgAlexa <- reactive({validate(
      #   need(is.null(input$demo), "Metadata must be provided."),
      #   need(is.null(input$omicsData), "Please upload at least one omics data file.")
      # )})
      # output$errMsgAlexa <- renderUI({
      #   errMsgAlexa()
      # })

      # output$msg <- renderText({
      #   "Alexa is taking a look at your data, please wait..."
      # })

      withProgress(message = 'Alexa is taking a look.',
        detail = 'This may take a while...', value = 0, {

          demo <- isolate({getDemoData()})
          responseColumnName <- isolate({input$responseVar})
          responseRefLevel <- isolate({input$refVar})
          response <- isolate({relevel(factor(as.character(demo[, responseColumnName])), ref = input$refVar)})
          omicsData <- isolate({getOmicsData()})

          dynamodbAttr <- list()
          # analyze demo data and save images to S3
          dynamodbAttr$ds <- omicsBioAnalytics::alexaMetadata(demo, group = responseColumnName, trim = 0.5, format = "APL")

          # demo <- heartFailure$demo
          # omicsData <- heartFailure$omicsData[c("cells", "holter", "proteins")]
          # group <- "hospitalizations"
          # EDA and save images to S3
          dynamodbAttr$eda <- omicsBioAnalytics::alexaEda(demo, group = responseColumnName, omicsData)

          # Perform Differential Expression Analysis and save images to S3
          dynamodbAttr$dexp <- omicsBioAnalytics::alexaDexp(demo, group = responseColumnName, omicsData)

          # save dynamodb attributes to dynamodbTableName (set in global.R) for userID (set in global.R)
          omicsBioAnalytics::put_item(dynamodbTableName, list(id = userID, phoneNumber= jsonlite::toJSON(dynamodbAttr)))
        })
        output$msg <- renderText({
          paste0("If you have an Alexa device please say, 'Alexa, start omics bioanalytics' to begin. \n Please use the following id to access your analysis when prompted by Alexa: ", userID)
        })
    })
  } else {
    observeEvent(input$alexa, {
      output$msg <- renderText({
        "This functionality has been suspended due to cost considerations of S3 and DynamoDB. Please see the Overview tab for a link to the source code (github repo) and step-by-step setup instructions for the complementary Alexa Skill. Sorry for the inconvenience."
      })
    })
  }

  # delete temp files
  # session$onSessionEnded(function() {
  #   sapply(list.files(tempdir(), full.names = TRUE), file.remove)
  # })
}

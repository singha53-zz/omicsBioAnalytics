options(repos=structure(BiocManager::repositories())) ## repository configuration of bioconductor packages

# load libraries
library("shiny");
library("shinyBS");
library("plotly");
library("omicsBioAnalytics");
library("googleVis");
library("limma")

## global variables
## set colors for binary groups
lvl1Color <- "#66C2A5"
lvl2Color <- "#FC8D62"

function(input, output, session) {

  # Demographics data upload
  getDemoData <- reactive({
    req(input$demo)
    # demoData <- read.csv(input$demo$datapath,
    #   header = input$header,
    #   sep = input$sep)
    demoData <- read.csv(input$demo$datapath, header = TRUE, sep = input$sep)
    demoData
  })

  # show column names of demo dataset
  output$responseVar <- renderUI({
    keepCols <- apply(getDemoData(), 2, function(i){
      ifelse(nlevels(factor(as.character(i))) == 2, TRUE, FALSE)
    })
    selectInput('responseVar', 'Select response variable', colnames(getDemoData()[, keepCols]))
  })

  # omics data upload
  getOmicsData <-reactive({
    req(input$omicsData)
    omicsData <- lapply(input$omicsData$datapath, read.csv, header = TRUE, sep = input$sep)
    names(omicsData) <- gsub(".csv|.tsv", "", input$omicsData$name)
    lapply(omicsData, head)
    omicsData
  })

  # select dataset
  output$dataGenSym <- renderUI({
    checkboxGroupInput("dataGenSym", "Which datasets are labelled with gene symbols?",
      choiceNames =
        as.list(names(getOmicsData())),
      choiceValues =
        as.list(names(getOmicsData()))
    )
  })

  # Do not show analysis sidemenu at startup!!
  output$analysisRan <- reactive({
    returnedValue = FALSE
    return(returnedValue)
  })
  outputOptions(output, "analysisRan", suspendWhenHidden = FALSE)

  # Run analysis
  observeEvent(input$run, {
    ################################################################################
    #
    # Patient Characteristics
    #
    ################################################################################
    ## split demo data into cat and cont vars
    demo <- getDemoData()
    demoSplit <- omicsBioAnalytics::splitData(demo, group = input$responseVar, trim = 0.8)
    ## @@@@@@@@@@@@@@@@@@@@@@@ Continuous variable panel @@@@@@@@@@@@@@@@@@@@@@@ ##
    updateRadioButtons(session, "vars",
      label = "Demographic variable:",
      choices = colnames(demoSplit$data.cont),
      selected = colnames(demoSplit$data.cont)[1], inline = TRUE
    )
    observe({
      DF <- reactive({
        df <- data.frame(x = demo[, input$responseVar],
          y = if (input$transform == "no") {
            demo[, input$vars]
          } else {
            log2(demo[, input$vars])
          }) %>%
          na.omit()
      })

      fit <- reactive({
        if(input$test == "ttest"){
          lm(y~x, data = DF())
        } else {
          wilcox.test(y~x, data = DF())
        }
      })

      f <- list(family = "Courier New, monospace",
        size = 15,
        color = "#7f7f7f")
      xaxis <- list(title = input$responseVar,
        titlefont = f)
      yaxis <- reactive({
        list(title = if (input$transform == "no") {
          input$vars
        } else {
          paste(input$vars, "(log2)")
        },
          titlefont = f)
      })
      title <- reactive({
        title = paste(input$vars, " vs. ", input$responseVar)
      })
      output$plot <- renderPlotly({
        options(htmlwidgets.TOJSON_ARGS = NULL) ## import in order to run canvasXpress
        plot_ly(data = DF(), y = ~y, color = ~x, type = "box") %>%
            layout(showlegend = FALSE) %>%
            layout(xaxis = xaxis,
              yaxis = yaxis(),
              title = title())
      })

      output$testTitle <- renderText({
        if(input$test == "ttest"){
          "Student's t-Test"
        } else {
          "Wilcoxon Rank Sum test"
        }
      })

      assumptions <- reactive({
        if(input$test == "ttest"){
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
      output$tbl = renderDataTable( assumptions(), options = list(lengthChange = FALSE))

      output$lmAssumptions <- renderText({
        if(input$test == "ttest"){
          if(assumptions()$Decision[1] == "Assumptions acceptable."){
            "Assumptions acceptable. Student's t-Test is recommended."
          } else {
            "Try a log2-transformation. If that doesn't work, the Wilcoxon Rank Sum test is recommended."
          }
        } else {
          return(NULL)
        }
      })

      output$test <- renderPrint({
        if(input$test == "ttest"){
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

        if(input$test == "ttest"){
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
        if(input$test == "ttest"){
          ifelse(coef(summary(fit()))[2, "Pr(>|t|)"] < 0.05,
            paste0("There is a statistically significant difference (at p<0.05) in mean ",
              input$var, " between the groups."),
            paste0("There is no statistically significant difference (at p<0.05) in mean ",
              input$var, " between the groups."))
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
      label = "Demographic variable:",
      choices = setdiff(colnames(demoSplit$data.cat), input$responseVar),
      selected = setdiff(colnames(demoSplit$data.cat), input$responseVar)[1], inline = TRUE
    )
    observe({
      output$obsCounts <- renderGvis({
        d <- as.data.frame(as.data.frame.matrix(addmargins(table(demo[, input$responseVar], demo[, input$catVar]))))
        gvisTable(cbind(' '=rownames(d), d))
      })
      output$obsFreq <- renderGvis({
        d <- as.data.frame(as.data.frame.matrix(apply(table(demo[, input$responseVar], demo[, input$catVar]), 1, function(i){
          round(100*i/sum(i), 0)
        }) %>% t))
        gvisTable(cbind(' '=rownames(d), d))
      })
      output$chisqTest <- renderPrint({
        x <- input$catVar
        chisq.test(demo[, x], demo[, input$responseVar])
      })
      output$chisqConclusion <- renderText({
        pval <- chisq.test(demo[, input$catVar], demo[, input$responseVar])$p.value
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
              h3("Which clinical variables are associated with major sources of variation in the gene expression data?", align = "center"),
              plotlyOutput(paste("pcClinVarPlot", i, sep="_"), width = "100%")
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
            omicsBioAnalytics::pcaPairs(pcs = pcs, y = demo[, input$responseVar], col=c(lvl1Color, lvl2Color))
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
              column(6, verbatimTextOutput(paste("selection", i, sep="_")), style = 'padding: 15px 10px 0px 10px;'),
              column(6, align = "center",
                sliderInput(paste("fdr", i, sep="_"), h3("Select FDR threshold", align = "center"),
                  min = 0.05, max = 0.5, value = 0.05))),
            fluidRow(
              column(6, plotlyOutput(paste("volcanoPlot", i, sep="_"))),
              column(6,
                plotlyOutput(paste("boxplot", i, sep="_"))
              )),
            fluidRow(column(8, h4(textOutput(paste("statement", i, sep="_")))),
              column(4, actionButton(paste("button", i, sep="_"), "Significant variables", icon = icon("table")),
                bsModal(paste("modal", i, sep="_"), "Data Table", paste("button", i, sep="_"), size = "large",
                  DT::dataTableOutput(paste("sig", i, sep="_")))))
          )
        })

        do.call(tabsetPanel, myTabs)
      })

      ## Backend
      lapply(names(getOmicsData()),
        function(i) {
          observeEvent(input[[paste("fdr", i, sep="_")]], {
            design <- model.matrix(~demo[, input$responseVar])
            fit <- eBayes(lmFit(t(getOmicsData()[[i]]), design))
            top <- topTable(fit, coef = 2, adjust.method = "BH", n = nrow(fit), sort.by="none")
            top <- top %>% mutate(FeatureName = colnames(getOmicsData()[[i]]),
              sig = -log10(P.Value)) %>%
              arrange(P.Value)

            subsetTop <- reactive({
              top = top %>%
                mutate(Significant=ifelse(adj.P.Val < input[[paste("fdr", i, sep="_")]],
                  paste("FDR < ", input[[paste("fdr", i, sep="_")]]), "Not Sig"))
            })

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

            # selected feature on volcano plot
            output[[paste("selection", i, sep="_")]] <- renderPrint({
              s <- event_data("plotly_click", source = paste("volcanoPlot", i, sep="_"))
              if (length(s) == 0) {
                "Click on a point on the volcano plot"
              } else {
                cat(paste("You selected:", s$key, "\n\n"));
                cat(paste("Fold-change = ", signif(s$x, 3), "\n P-value = ", signif(10^-s$y, 3)))
              }
            })

            # feature plot
            output[[paste("boxplot", i, sep="_")]] <- renderPlotly({
              s <- event_data("plotly_click", source = paste("volcanoPlot", i, sep="_"))
              if (length(s)) {
                var <- unlist(s[["key"]])
                lvls <- levels(demo[, input$responseVar])
                lvl1 <- as.numeric(getOmicsData()[[i]][demo[, input$responseVar] == lvls[1], var])
                lvl2 <- as.numeric(getOmicsData()[[i]][demo[, input$responseVar] == lvls[2], var])

                f <- list(family = "Courier New, monospace",
                  size = 15, color = "#7f7f7f")
                xaxis <- list(title = input$responseVar, titlefont = f)
                yaxis <- list(title = var, titlefont = f)

                options(htmlwidgets.TOJSON_ARGS = NULL) ## import in order to run canvasXpress
                plot_ly(type = 'box') %>%
                  add_boxplot(y = lvl1, jitter = 0.3, pointpos = -1.8, boxpoints = 'all',
                    marker = list(color = lvl1Color),
                    line = list(color = lvl1Color),
                    name = paste0(lvls[1], " (n=", length(lvl1), ")")) %>%
                  add_boxplot(y = lvl2, jitter = 0.3, pointpos = -1.8, boxpoints = 'all',
                    marker = list(color = lvl2Color),
                    line = list(color = lvl2Color),
                    name = paste0(lvls[2], "( n=", length(lvl2), ")")) %>%
                  layout(showlegend = FALSE) %>%
                  layout(xaxis = xaxis,
                    yaxis = yaxis,
                    title = paste(var, "vs.", input$responseVar))
              } else {
                plotly_empty()
              }
            })

            # statement
            output[[paste("statement", i, sep="_")]] <- renderText({
              paste("There are ", sum(top$adj.P.Val < input[[paste("fdr", i, sep="_")]]), "significant ", i, ". ",
                sum(top$logFC > 0 & top$adj.P.Val < input[[paste("fdr", i, sep="_")]]), i, " were up-regulated in subjects whereas",
                sum(top$logFC < 0 & top$adj.P.Val < input[[paste("fdr", i, sep="_")]]), i, " were down-regulated.")
            })

            print(dplyr::filter(subsetTop(), Significant != "Not Sig") %>%
                mutate(logFC = signif(logFC, 2),
                  P.Value = signif(P.Value, 2),
                  adj.P.Val = signif(adj.P.Val, 2),
                  sig = signif(sig, 2)) %>%
                dplyr::select(FeatureName, logFC, P.Value, adj.P.Val), options = list(lengthChange = FALSE))
            # table of significant featuers
            output[[paste("sig", i, sep="_")]] <-  DT::renderDataTable( dplyr::filter(subsetTop(), Significant != "Not Sig") %>%
                mutate(logFC = signif(logFC, 2),
                  P.Value = signif(P.Value, 2),
                  adj.P.Val = signif(adj.P.Val, 2),
                  sig = signif(sig, 2)) %>%
                dplyr::select(FeatureName, logFC, P.Value, adj.P.Val))




          })
        }
      )})

    # show analysis sidemenu when run analysis button is pressed
    output$analysisRan <- reactive({
      returnedValue = TRUE
      return(returnedValue)
    })
    outputOptions(output, "analysisRan", suspendWhenHidden = FALSE)
  })
}

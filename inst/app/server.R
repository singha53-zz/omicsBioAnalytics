# load libraries
library("shiny");
library("plotly");
library("omicsBioAnalytics");
library("googleVis");

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
    # print(getDemoData()[1:2, 1:2])
    # print(input$responseVar)
    # print(lapply(getOmicsData()$data, head))
    # print(getOmicsData()$name)

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
      xaxis <- list(title = "hospitalizations",
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
        title = paste(input$vars, "vs. hospitalizations")
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
            input$catVar, " and hospitalizations", " (p-value = ", signif(pval, 3), ")."),
          paste0("There is no statistically significant association (at p<0.05) between ",
            input$catVar, " and hospitalizations", " (p-value = ", signif(pval, 3), ")."))
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
        print(i)
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
    )})


    # show analysis sidemenu when run analysis button is pressed
    output$analysisRan <- reactive({
      returnedValue = TRUE
      return(returnedValue)
    })
    outputOptions(output, "analysisRan", suspendWhenHidden = FALSE)
  })
}

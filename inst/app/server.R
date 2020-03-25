## set colors for binary groups
lvl1Color <- "#66C2A5"
lvl2Color <- "#FC8D62"

dynamodbTableName <- Sys.getenv("TABLE_NAME")
S3Bucket <- Sys.getenv("S3BUCKET")
previousWorkloads <- sapply(get_bucket(bucket = S3Bucket), function(i){
  strsplit(i$Key, "-")[[1]][1]
})
flag <- TRUE
while(flag){
  userID <- paste(sample(0:9, 7), collapse = "")
  if(!(userID %in% previousWorkloads)){
    flag <- FALSE
  }
}

# test upload to dynamoDB
# omicsBioAnalytics::put_item(dynamodbTableName, list(id = userID, phoneNumber= jsonlite::toJSON("50 subjects")))

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

  # performPathwayAnalysis <- reactive({lapply(names(getOmicsData()), function(i){
  #   names(i)
  # })})
  # print(performPathwayAnalysis())

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
      output$tbl = DT::renderDataTable( assumptions(), options = list(lengthChange = FALSE))

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
              h3("Which clinical variables are associated with major sources of variation in the expression data?", align = "center"),
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
              column(6, plotly::plotlyOutput(paste("volcanoPlot", i, sep="_"))),
              column(6,
                plotly::plotlyOutput(paste("boxplot", i, sep="_"))
              )),
            fluidRow(column(8, h4(textOutput(paste("statement", i, sep="_")))),
              column(4, actionButton(paste("button", i, sep="_"), "Significant variables", icon = icon("table")),
                bsModal(paste("modal", i, sep="_"), "Differentially expressed variables.", paste("button", i, sep="_"), size = "large",
                  DT::dataTableOutput(paste("sig", i, sep="_"))))),
            hr(),
            conditionalPanel(
              condition = paste0("output.performPathwayAnalysis.indexOf('", i, "') != -1"),
            fluidRow(
              column(12, plotOutput(paste("gsetPlot", i, sep="_"), height = "250px")),
              column(2, ""),
              column(2, actionButton(paste("keggButton", i, sep="_"), "Significant KEGG pathways", icon = icon("table")),
                bsModal(paste("keggModal", i, sep="_"), "Significant KEGG pathways",
                  paste("keggButton", i, sep="_"), size = "large",
                  DT::dataTableOutput(paste("sigKegg", i, sep="_")))),
              column(2, ""),
              column(2, ""),
              column(2, actionButton(paste("wikiButton", i, sep="_"), "Significant Wiki pathways", icon = icon("table")),
                bsModal(paste("wikiModal", i, sep="_"), "Significant Wiki pathways",
                  paste("wikiButton", i, sep="_"), size = "large",
                  DT::dataTableOutput(paste("sigWiki", i, sep="_")))),
              column(2, "")
              ),
              fluidRow(column(6, style = 'padding: 0px 0px 0px 0px;',
                h4("Network of all up-regulated pathways", align = "center"),
                visNetwork::visNetworkOutput(paste("upregulated", i, sep="_"), height = "400px", width = "100%")
              ),
                column(6, style = 'padding: 0px 0px 0px 0px;',
                  h4("Network of all down-regulated pathways", align = "center"),
                  visNetwork::visNetworkOutput(paste("downregulated", i, sep="_"), height = "400px", width = "100%")
                )),
              h6("Zoom in to see pathway labels")
            )
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

            ## Differential pathway analysis
            if(length(intersect(colnames(getOmicsData()[[i]]), unlist(kegg)) > 5) &
                length(intersect(colnames(getOmicsData()[[i]]), unlist(wikipathways)) > 5)){
              ## KEGG pathways
              indKegg <- lapply(kegg, function(pathway){
                which(colnames(getOmicsData()[[i]]) %in% pathway)
              })
              indKegg <- indKegg[sapply(indKegg, length) > 5]
              gsetKegg <- camera(t(getOmicsData()[[i]]), indKegg, design, contrast = 2)
              gsetKegg$adj.P.Val <- p.adjust(gsetKegg$PValue, "BH")
              ## Wikipathways
              indWiki <- lapply(wikipathways, function(pathway){
                which(colnames(getOmicsData()[[i]]) %in% pathway)
              })
              indWiki <- indWiki[sapply(indWiki, length) > 5]
              gsetWiki <- camera(t(getOmicsData()[[i]]), indWiki, design, contrast = 2)
              gsetWiki$adj.P.Val <- p.adjust(gsetWiki$PValue, "BH")
            } else {
              gsetKegg <- gsetWiki <- NULL
            }


            output[[paste("gsetPlot", i, sep="_")]] <- renderPlot({
              if(all(c(!is.null(gsetKegg), !is.null(gsetWiki)))){
                par(mar=c(4,4,1,1))
                par(mfrow = c(1, 2))
                col <- rep("grey", length(gsetKegg$adj.P.Val))
                col[gsetKegg$adj.P.Val < input[[paste("fdr", i, sep="_")]] & gsetKegg$Direction == "Up"] <- "tomato"
                col[gsetKegg$adj.P.Val < input[[paste("fdr", i, sep="_")]] & gsetKegg$Direction == "Down"] <- "skyblue"
                plot(gsetKegg$adj.P.Val, log="x", col = col, pch = 19,
                  ylim = c(0,1), main = "KEGG pathways",
                  ylab="FDR", xlab = "Number of pathways")
                legend("topleft", c("up-regulated", "down-regulated", "not signficant"),
                  col = c("tomato","skyblue","grey"), pch = 19, bty = "n")
                col <- rep("grey", length(gsetWiki$adj.P.Val))
                col[gsetWiki$adj.P.Val < input[[paste("fdr", i, sep="_")]] & gsetWiki$Direction == "Up"] <- "tomato"
                col[gsetWiki$adj.P.Val < input[[paste("fdr", i, sep="_")]] & gsetWiki$Direction == "Down"] <- "skyblue"
                plot(gsetWiki$adj.P.Val, log="x", col = col, pch = 19,
                  ylim = c(0,1), main = "WikiPathways",
                  ylab="FDR", xlab = "Number of pathways")
                legend("topleft", c("up-regulated", "down-regulated", "not signficant"),
                  col = c("tomato","skyblue","grey"), pch = 19, bty = "n")
              } else {
                ""
              }
            })

            output[[paste("sigKegg", i, sep="_")]] = DT::renderDataTable(
              if(!is.null(gsetKegg)){
                gsetKegg %>% mutate(Pathway = rownames(.)) %>%
                  filter(adj.P.Val < input[[paste("fdr", i, sep="_")]]) %>%
                  mutate(PValue = signif(PValue, 3), adj.P.Val = signif(adj.P.Val, 3)) %>%
                  dplyr::select(Pathway, NGenes, Direction, PValue, adj.P.Val)
              } else {
                ""
              }, options = list(lengthChange = FALSE))
            output[[paste("sigWiki", i, sep="_")]] = DT::renderDataTable(
              if(!is.null(gsetWiki)){
                gsetWiki %>% mutate(Pathway = rownames(.)) %>%
                  filter(adj.P.Val < input[[paste("fdr", i, sep="_")]]) %>%
                  mutate(PValue = signif(PValue, 3), adj.P.Val = signif(adj.P.Val, 3)) %>%
                  dplyr::select(Pathway, NGenes, Direction, PValue, adj.P.Val)
              } else {
                ""
              }, options = list(lengthChange = FALSE))

            int = function (s1, s2) {
              length(intersect(s1, s2))/length(union(s1, s2))
            }
            output[[paste("upregulated", i, sep="_")]] <- visNetwork::renderVisNetwork({
              if(all(c(!is.null(gsetKegg), !is.null(gsetWiki)))){
                gset <- rbind(gsetKegg, gsetWiki)
                pathway.genes <- lapply(append(indKegg, indWiki), function(ind){
                  colnames(getOmicsData()[[i]])[ind]
                })
                ## up-regulated
                up.pathways <- gset[gset$adj.P.Val < input[[paste("fdr", i, sep="_")]] & gset$Direction == "Up", , drop=FALSE]
                if(nrow(up.pathways) > 1){
                  ref <- pathway.genes[rownames(up.pathways)]
                  links <- t(combn(names(ref), 2)) %>% as.data.frame(.) %>%
                    dplyr::tbl_df(.) %>% dplyr::rename(from = V1, to = V2) %>%
                    dplyr::mutate(int = unlist(purrr::map2(ref[as.character(from)],
                      ref[as.character(to)], omicsBioAnalytics::jaccard)))
                  edges <- links[links$int > 0.1, ]
                  nodes <- data.frame(id=unique(as.character(as.matrix(links[, 1:2]))))
                  nodes$label <- nodes$id
                  nodes$color <- "salmon"
                  print(dim(edges))
                  print(dim(nodes))
                  visNetwork::visNetwork(nodes, edges)
                } else {
                  return(NULL)
                }
              } else {
                return(NULL)
              }
            })
            output[[paste("downregulated", i, sep="_")]] <- visNetwork::renderVisNetwork({
              if(all(c(!is.null(gsetKegg), !is.null(gsetWiki)))){
                gset <- rbind(gsetKegg, gsetWiki)
                pathway.genes <- lapply(append(indKegg, indWiki), function(ind){
                  colnames(getOmicsData()[[i]])[ind]
                })
                ## down-regulated
                down.pathways <- gset[gset$adj.P.Val < input[[paste("fdr", i, sep="_")]] & gset$Direction == "Down", , drop=FALSE]
                if(nrow(down.pathways) > 1){
                  ref <- pathway.genes[rownames(down.pathways)]
                  links <- t(combn(names(ref), 2)) %>% as.data.frame(.) %>%
                    dplyr::tbl_df(.) %>% dplyr::rename(from = V1, to = V2) %>%
                    dplyr::mutate(int = unlist(purrr::map2(ref[as.character(from)],
                      ref[as.character(to)], omicsBioAnalytics::jaccard)))
                  edges <- links[links$int > 0.1, ]
                  nodes <- data.frame(id=unique(as.character(as.matrix(links[, 1:2]))))
                  nodes$label <- nodes$id
                  nodes$color <- "skyblue"
                  visNetwork::visNetwork(nodes, edges)
                } else {
                  return(NULL)
                }
              } else {
                return(NULL)
              }
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


    observe({
      print(input$dataGenSym)
    })

    ################################################################################
    #
    # Biomarker Discovery Analysis
    #
    ################################################################################




  })


  # Voice-enabled analytics
  observeEvent(input$alexa, {

    output$msg <- renderText({
      "Alexa is taking a look at your data, please wait..."
    })

    if(any(c(is.null(input$demo), is.null(input$omicsData), is.null(input$responseVar)))) {
      output$msg <- renderText({
        "Missing the required data files and response selection!"
      })
    } else {

      diffexp = function(datasets, response){
        design <- model.matrix(~response)
        lapply(datasets, function(i){
          fit <- eBayes(lmFit(t(i), design))
          top <- topTable(fit, coef = 2, adjust.method = "BH", n = nrow(fit), sort.by="none")
          top %>% mutate(FeatureName = colnames(i),
            sig = -log10(P.Value)) %>%
            arrange(P.Value)
        })
      }
      dexpResults <- diffexp(heartFailure$omicsData, heartFailure$demo[, "hospitalizations"])

      dexpResults <- lapply(names(dexpResults), function(name){
        lapply(c(0.01, 0.05, 0.1, 0.2), function(fdr){
          volcanoPlot <- dexpResults[[name]] %>%
            mutate(Significant=ifelse(adj.P.Val < fdr, paste("FDR < ", fdr), "Not Sig")) %>%
            ggplot(aes(x = logFC, y = sig, color = Significant)) +
            geom_point() +
            ylab("-log10(p-value)") +
            xlab(expression("log2 fold-change")) +
            theme_bw()
          ggsave(paste0(tempdir(), "/", paste(userID, name, fdr, sep="-"), ".png"), volcanoPlot, device = "png")
        })
      })
      s3sync(grep(userID, list.files(tempdir()), value = TRUE), bucket = S3Bucket, direction = "upload")

      # Upon completion of analysis
      output$msg <- renderText({
        paste0("If you have an Alexa device please say, 'Alexa, start omics bioanalytics' to begin. \n Please use the following id to access your analysis when prompted by Alexa: ", userID)
      })
    }
  })

  # delete temp files
  session$onSessionEnded(function() {
    sapply(grep(userID, list.files(tempdir(), full.names = TRUE), value = TRUE), file.remove)
  })
}

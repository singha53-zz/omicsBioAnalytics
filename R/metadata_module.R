#' UI of Metadata Analysis page
#'
#' @param id, character used to specify namespace,
#' see \code{shiny::\link[shiny]{NS}}
#'
#' @return a \code{shiny::\link[shiny]{tagList}} containing UI element
#' @export
metadata_ui <- function(id) {
  ns <- shiny::NS(id)

  shiny::fluidPage(
    shiny::fluidRow(shiny::tabsetPanel(
      shiny::tabPanel(
        "Continuous variables",
        shiny::radioButtons(
          ns("cont_var"),
          "Choose from one of the following variables:",
          "",
          inline = TRUE
        ),
        shiny::fluidRow(shiny::column(6, shiny::radioButtons(
          ns("transform"),
          "Should the variable be log transformed?",
          c("No" = "no", "Yes" = "yes"),
          inline = TRUE
        )), shiny::column(6, shiny::radioButtons(
          ns("test"),
          "Which test should be applied?",
          c("Linear regression" = "lr", "Kruskal-Wallis Test" = "ks"),
          inline = TRUE
        ))),
        shiny::fluidRow(
          shiny::column(6,
            shiny::h3(""),
            plotly::plotlyOutput(ns("plot"))
          ),
          shiny::column(
            6,
            shiny::h3("Descriptive statistics"),
            shiny::tableOutput(ns("descriptive_stat")),
            shiny::h3(shiny::textOutput(ns("test_title"))),
            shiny::verbatimTextOutput(ns("test")),
            shiny::uiOutput(ns("show_assumptions_button")),
            shiny::h3("Conclusion"),
            shiny::textOutput(ns("conclusion"))
          ))
      ),
      shiny::tabPanel("Categorical variables",
        shiny::fluidPage(
          shiny::column(4,
            shiny::radioButtons(
              ns("cat_var"),
              "Choose from one of the following variables:",
              "",
              inline = TRUE
            )),
          shiny::column(8,
            shiny::h3(shiny::textOutput(ns("chisq_title"))),
            shiny::br(),
            shiny::fluidRow(shiny::column(6, shiny::h4("Observed counts"),
              shiny::uiOutput(ns("obs_counts"))),
              shiny::column(6, shiny::h4("Observed frequency"),
                shiny::uiOutput(ns("obs_freq")))),
            shiny::br(),
            shiny::h4("Chi-square test"),
            shiny::verbatimTextOutput(ns("chisq_test")),
            shiny::br(),
            shiny::tags$style(type = "text/css",
              "#chisq_conclusion {color: red;}"),
            shiny::textOutput(ns("chisq_conclusion")))
        )
      )
    ))
  )
}

#' Reactive values for metadata_server module server-side processing
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
metadata_ui_vars <- function(input, output, session) {

  return(
    list(
      cont_var = shiny::reactive({
        input$cont_var}),
      cat_var = shiny::reactive({
        input$cat_var}),
      transform = shiny::reactive({
        input$transform}),
      test = shiny::reactive({
        input$test
      })
    )
  )
}

#' Metadata module server-side processings
#'
#' This module produces the Metadata panel for a given dataset
#'
#' @param input,output,session standard \code{shiny} boilerplate
#' @param demo data frame with metadata
#' @param dataset data frame containing omic variables
#' @param response factor containing cateogories for the response variable
#' @param metadata_ui_vars list of reactive vars (cont_var, transform)
#' @param cont_var character selected continuous variable
#' @param transform boolean TRUE: log2-transformation
#' @export
metadata_server <- function(input, output, session,
  data_upload_ui_vars,
  data_upload_server_vars,
  metadata_ui_vars) {

  response_var <- data_upload_ui_vars$response_var()
  demo <- data_upload_server_vars$get_demo_data()
  response <- data_upload_server_vars$response()
  demo_split <- omicsBioAnalytics::splitData(demo,
    group = response_var, trim = 0.8)

  if (ncol(demo_split$data.cont) > 1){
    #@@@@@@@@@@@@@@@@@@@@@@@ Continuous variable panel @@@@@@@@@@@@@@@@@@@@@@@#
    shiny::updateRadioButtons(session, "cont_var",
      label = "Choose from one of the following variables:",
      choices = colnames(demo_split$data.cont),
      selected = colnames(demo_split$data.cont)[1], inline = TRUE
    )

    shiny::observe({
      shiny::req(metadata_ui_vars$cont_var() != "")
      cont_dat <- shiny::reactive({
        data.frame(x = response,
          y = if (metadata_ui_vars$transform() == "no") {
            demo[, metadata_ui_vars$cont_var()]
          } else {
            log2(demo[, metadata_ui_vars$cont_var()])
          }) %>%
          na.omit()
      })

      fit <- shiny::reactive({
        if (metadata_ui_vars$test() == "lr") {
          lm(y~x, data = cont_dat())
        } else {
          kruskal.test(y~x, data = cont_dat())
        }
      })

      yaxis <- shiny::reactive({
        if (metadata_ui_vars$transform() == "no") {
          metadata_ui_vars$cont_var()
        } else {
          paste(metadata_ui_vars$cont_var(), "(log2)")
        }
      })
      title <- shiny::reactive({
        paste(metadata_ui_vars$cont_var(), " vs. ", response_var)
      })
      output$plot <- plotly::renderPlotly({
        options(htmlwidgets.TOJSON_ARGS = NULL) ## for canvasXpress
        ggplotly(cont_dat() %>%
            ggplot(aes(x = x, y = y, fill = x)) +
            geom_violin(trim = FALSE) +
            geom_point(position = position_dodge(width = 0.1)) +
            # geom_dotplot(binaxis='y', stackdir='center', dotsize=1) +
            # xlab(data_upload_vars$response_var()) +
            xlab(response_var) +
            ylab(yaxis()) +
            ggtitle(title()) +
            theme_classic() +
            theme(legend.position = "none") +
            scale_fill_manual(values = group_colors[1:nlevels(response)])
        )
      })

      output$test_title <- shiny::renderText({
        if (metadata_ui_vars$test() == "lr") {
          "Linear Regression"
        } else {
          "Kruskal-Wallis Test"
        }
      })

      assumptions <- shiny::reactive({
        if (metadata_ui_vars$test() == "lr") {
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
      output$tbl <- DT::renderDataTable(assumptions(),
        options = list(lengthChange = FALSE))

      output$lm_assumptions <- shiny::renderText({
        if (metadata_ui_vars$test() == "lr") {
          if (assumptions()$Decision[1] == "Assumptions acceptable.") {
            "Assumptions acceptable."
          } else {
            "Try a log2-transformation. If that doesn't work,
          the Kruskal-Wallis Test is recommended."
          }
        } else {
          return(NULL)
        }
      })

      output$test <- shiny::renderPrint({
        if (metadata_ui_vars$test() == "lr") {
          coef(summary(fit()))
        } else {
          fit()
        }
      })

      ## to show lr assumptions button or not
      shiny::observeEvent(metadata_ui_vars$test(), {
            output$show_assumptions_button <- shiny::renderUI({
              if (metadata_ui_vars$test() == "lr") {
                ns <- session$ns
                shiny::fluidRow(shiny::column(6,
                  shiny::h6(shiny::textOutput(ns("lm_assumptions")))),
                  shiny::column(6, shiny::actionButton(ns("lr"),
                    "linear regression assumptions",
                    icon = shiny::icon("table")),
                    shinyBS::bsModal(ns("hypothesisTests"),
                      "Assessment of linear regression assumptions.",
                      ns("lr"), size = "large",
                      DT::dataTableOutput(ns("tbl")))))
              } else {
                return(NULL)
              }
            })
      })

      output$descriptive_stat <- shiny::renderTable({
        summary_stat <- cont_dat() %>%
          dplyr::mutate(x = factor(x)) %>%
          dplyr::group_by(x) %>%
          dplyr::summarise(Min = min(y, na.rm = TRUE),
            Median = median(y, na.rm = TRUE),
            Max = max(y, na.rm = TRUE),
            Mean = mean(y, na.rm = TRUE),
            SD  = sd(y, na.rm = TRUE)) %>%
          dplyr::rename(Group = x)

        if (metadata_ui_vars$test() == "lr") {
          parametric_desc_stat <- signif(rbind(summary_stat$Mean,
            summary_stat$SD), 3)
          rownames(parametric_desc_stat) <- c("Mean", "SD")
          colnames(parametric_desc_stat) <- summary_stat$Group
          parametric_desc_stat %>%
            as.data.frame() %>%
            dplyr::mutate(Measure = rownames(.))
        } else {
          nonparametric_desc_stat <- signif(rbind(summary_stat$Min,
            summary_stat$Median, summary_stat$Max), 3)
          rownames(nonparametric_desc_stat) <- c("Min", "Median", "Max")
          colnames(nonparametric_desc_stat) <- summary_stat$Group
          nonparametric_desc_stat %>%
            as.data.frame() %>%
            dplyr::mutate(Measure = rownames(.))
        }
      })

      output$conclusion <- shiny::renderText({
        if (metadata_ui_vars$test() == "lr") {
          ifelse(summary(aov(fit()))[[1]][1, "Pr(>F)"] < 0.05,
            paste0("There is a statistically significant difference
            (at p<0.05) ",
              metadata_ui_vars$cont_var(), " between the groups based on an Analysis of Variance
            (see pairwise comparisons above)."),
            paste0("There is no statistically significant difference
            (at p<0.05) ",
              metadata_ui_vars$cont_var(), " between the groups based on an Analysis of Variance
            (see pairwise comparisons above)."))
        } else {
          ifelse(fit()$p.value < 0.05,
            paste0("There is a statistically significant difference
            (at p<0.05) in ",
              metadata_ui_vars$cont_var(), " between the mean ranks of the groups."),
            paste0("There is no statistically significant difference
            (at p<0.05) in ",
              metadata_ui_vars$cont_var(), " between the mean ranks of the groups."))
        }
      })
    })
  }

  if (ncol(demo_split$data.cat) > 1){
    #@@@@@@@@@@@@@@@@@@@@@@@ Categorical variable panel @@@@@@@@@@@@@@@@@@@@@@@#
    shiny::updateRadioButtons(session, "cat_var",
      label = "Choose from one of the following variables:",
      choices = setdiff(colnames(demo_split$data.cat), response_var),
      selected = setdiff(colnames(demo_split$data.cat), response_var)[1],
      inline = TRUE)
    shiny::observe({
      output$obs_counts <- googleVis::renderGvis({
        d <- as.data.frame(as.data.frame.matrix(addmargins(table(response,
          demo[, metadata_ui_vars$cat_var()]))))
        googleVis::gvisTable(cbind(" " = rownames(d), d))
      })
      output$obs_freq <- googleVis::renderGvis({
        d <- as.data.frame(as.data.frame.matrix(apply(table(response,
          demo[, metadata_ui_vars$cat_var()]), 1, function(i) {
            round(100 * i / sum(i), 0)
          }) %>% t))
        googleVis::gvisTable(cbind(" " = rownames(d), d))
      })
      output$chisq_test <- shiny::renderPrint({
        chisq.test(demo[, metadata_ui_vars$cat_var()], response)
      })
      output$chisq_conclusion <- shiny::renderText({
        pval <- chisq.test(demo[, metadata_ui_vars$cat_var()], response)$p.value
        ifelse(pval < 0.05,
          paste0("There is a statistically significant
          association (at p<0.05) between ",
            metadata_ui_vars$cat_var(), " and ",
            response_var, " (p-value = ", signif(pval, 3), ")."),
          paste0("There is no statistically significant
          association (at p<0.05) between ",
            metadata_ui_vars$cat_var(), " and ",
            response_var, " (p-value = ", signif(pval, 3), ")."))
      })
      output$chisq_title <- shiny::renderText({
        metadata_ui_vars$cat_var()})
    })
  }
}

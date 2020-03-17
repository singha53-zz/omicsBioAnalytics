#' @export
#' @rdname uiElements
eda <- function() {
  fluidRow(uiOutput("eda"))
}

#' @export
#' @rdname uiElements
edaTabs = function(datasetNames){
  lapply(datasetNames, function(i){
    tabPanel(i, "hi")
  })
}


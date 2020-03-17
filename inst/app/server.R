# load libraries
library("shiny")
library("asthmaMetabolomics")

function(input, output, session) {
  output$analysisRan <- reactive({
    # result <- glycoPipe(inFileName)
    returnedValue = TRUE
    # returnedText <- result$params
    return(returnedValue)
  })
  outputOptions(output, "analysisRan", suspendWhenHidden = FALSE)
}

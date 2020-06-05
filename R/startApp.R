#' Start App
#'
#' @param ... passed to \link[shiny]{runApp}
#' @export
#' @rdname startApplication
#' from https://github.com/sk-sahu/sig-bio-shiny/blob/master/R/run.R
start_app <- function() {
  appDir <- system.file("inst/app", package = "omicsBioAnalytics")
  shiny::runApp(appDir, launch.browser = TRUE)
}

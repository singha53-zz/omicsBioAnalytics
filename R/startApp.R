#' Start App
#'
#' @param ... passed to \link[shiny]{runApp}
#' @export
#' @rdname startApplication
startApp <- function() {
  shiny::runApp("inst/app")
}


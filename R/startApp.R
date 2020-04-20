#' Start App
#'
#' @param ... passed to \link[shiny]{runApp}
#' @export
#' @rdname startApplication
start_app <- function() {
  shiny::runApp("inst/app")
}

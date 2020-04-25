#' Download button with custom icon
#' https://stackoverflow.com/questions/49350509/adding-removing-icon-in-downloadbutton-and-fileinput
#' @export
customDownloadButton <- function(outputId, label = "Download", icon = icon("download")){
  shiny::tags$a(id = outputId,
    class = "btn btn-default shiny-download-link",
    href = "",
    target = "_blank",
    download = NA,
    icon, label)
}

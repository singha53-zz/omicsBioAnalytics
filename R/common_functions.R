#' @export
#' @rdname empty_plot
empty_plot = function(label){
  data.frame(x = 0.5, y = 0.5, label = label) %>%
    ggplot(aes(x = x, y = y)) +
    geom_text(aes(label = label)) +
    xlab("") +
    ylab("") +
    theme(axis.line=element_blank(),
      axis.text.x=element_blank(),
      axis.text.y=element_blank(),
      axis.ticks=element_blank(),
      axis.title.x=element_blank(),
      axis.title.y=element_blank(),
      legend.position="none",
      panel.background=element_blank(),
      panel.border=element_blank(),
      panel.grid.major=element_blank(),
      panel.grid.minor=element_blank(),
      plot.background=element_blank())
}

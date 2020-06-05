#' @export
#' @rdname pcaPairs
pcaPairs = function(pcs,y,col){
  trellis.par.set(superpose.symbol = list(pch = c(19,19),
    cex = c(1,1), col = col), superpose.line = list(col=col))
  pch_vector <- c(0,6,12)
  my_settings <- list(superpose.symbol=list(alpha = rep(1, 9)), col=col,
    cex=rep(0.8, 9), fill= col, font = rep(1, 9), pch=pch_vector)
  trellis.par.set(my_settings)
  caret::featurePlot(x = pcs,
    y = y,
    plot = "ellipse",
    auto.key = list(columns = 2))
}

#' @export
#' @rdname pcaHeatmap
pcaHeatmap = function(pcs, demo){
  pvalheatmap <- matrix(0, ncol = ncol(demo), nrow = ncol(pcs))
  rownames(pvalheatmap) <- colnames(pcs)
  colnames(pvalheatmap) <- colnames(demo)
  for(i in 1:ncol(pcs)){
    for(j in 1:ncol(demo)){
      pvalheatmap[i,j] <- summary(aov(lm(pcs[,i]~demo[,j])))[[1]][1, "Pr(>F)"]
    }
  }

  pvalheatmap[pvalheatmap < 0.01] <- 0.01
  pvalheatmap[pvalheatmap > 0.1] <- 1
  pvalheatmap[pvalheatmap > 0.01 & pvalheatmap < 0.05] <- 0.05
  pvalheatmap[pvalheatmap > 0.05 & pvalheatmap < 0.1] <- 0.1
  pvalheatmap[pvalheatmap == "0.01"] <- "p < 0.01"
  pvalheatmap[pvalheatmap == "0.05"] <- "0.01 < p < 0.05"
  pvalheatmap[pvalheatmap == "0.1"] <- "0.05 < p < 0.10"
  pvalheatmap[pvalheatmap == "1"] <- "p > 0.10"
  pvalheatmap %>% as.data.frame %>% mutate(Variable = rownames(.)) %>%
    tidyr::gather(Threshold, Value, -Variable) %>% mutate(Threshold = factor(Threshold,
      levels = unique(Threshold))) %>%
    mutate(Value = factor(Value, levels = c("p < 0.01", "0.01 < p < 0.05", "0.05 < p < 0.10", "p > 0.10"))) %>%
    ggplot(aes(Threshold, Variable)) +
    geom_tile(aes(fill = Value), colour = "white") + scale_fill_manual(values = rev(RColorBrewer::brewer.pal(n = 8,
      name = "Blues")[c(2, 4, 6, 8)])) + customTheme(sizeStripFont = 10,
        xAngle = 40, hjust = 1, vjust = 1, xSize = 10, ySize = 10,
        xAxisSize = 10, yAxisSize = 10) + xlab("") + ylab("")
}

#' @export
#' @rdname customTheme
customTheme = function(sizeStripFont, xAngle, hjust, vjust, xSize,
  ySize, xAxisSize, yAxisSize) {
  theme(strip.background = element_rect(colour = "black", fill = "white",
    size = 1), strip.text.x = element_text(size = sizeStripFont),
    strip.text.y = element_text(size = sizeStripFont), axis.text.x = element_text(angle = xAngle,
      hjust = hjust, vjust = vjust, size = xSize, color = "black"),
    axis.text.y = element_text(size = ySize, color = "black"),
    axis.title.x = element_text(size = xAxisSize, color = "black"),
    axis.title.y = element_text(size = yAxisSize, color = "black"),
    panel.background = element_rect(fill = "white", color = "black"))
}

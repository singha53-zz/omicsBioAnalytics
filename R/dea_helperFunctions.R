#' @export
#' @rdname jaccard
jaccard <- function (s1, s2) {
  length(intersect(s1, s2))/length(union(s1, s2))
}

#' @export
#' @rdname generateTopTable
generateTopTable = function(eset, design, coefNumber, test){
  if (test == "limma"){
    fit <- limma::eBayes(limma::lmFit(t(eset), design))
    top <- limma::topTable(fit, coef = coefNumber, adjust.method = "BH", n = nrow(fit), sort.by="none")
    top %>% dplyr::mutate(FeatureName = colnames(eset),
      sig = -log10(P.Value)) %>%
      dplyr::arrange(P.Value)
  } else if (test == "vlimma") {
    v <- limma::voom(t(eset), design, plot=FALSE)
    fit <- limma::lmFit(v, design)
    fit <- limma::eBayes(fit)
    top <- limma::topTable(fit, coef = coefNumber, n = nrow(fit), adjust.method = "BH", sort.by="none")
    top %>% dplyr::mutate(FeatureName = colnames(eset),
      sig = -log10(P.Value)) %>%
      dplyr::arrange(P.Value)
  } else {
    # assume OLS
    fit <- limma::lmFit(t(eset), design)
    fit$t <- fit$coef/fit$stdev.unscaled/fit$sigma
    fit$p.value <- 2 * pt(-abs(fit$t), df = fit$df.residual)
    top <- limma::topTable(fit, coef = coefNumber, adjust.method = "BH", n = nrow(fit), sort.by="none")
    top %>% dplyr::mutate(FeatureName = colnames(eset),
      sig = -log10(P.Value)) %>%
      dplyr::arrange(P.Value)
  }
}


#' @export
#' @rdname drugHeatmap
drugHeatmap = function(fc, genesetList, col, datasetName, GeneSetName){
  ## make data frame for ggplot
  Dat <- data.frame(Gene = unlist(genesetList),
    Pathway = rep(names(genesetList), unlist(lapply(genesetList, length))),
    FC = fc[unlist(genesetList)])

  Dat$FC[Dat$FC > 0] <- "Up";
  Dat$FC[Dat$FC == 0] <- "NoChange";
  Dat$FC[Dat$FC < 0] <- "Down"
  Dat$FC <- factor(Dat$FC, levels = c("Up", "NoChange", "Down"))
  ## order genes and pathways based on there popularity (number of times they repeat)
  Dat$Gene <- factor(as.character(Dat$Gene), levels = names(table(as.character(Dat$Gene))[order(table(as.character(Dat$Gene)))]))
  Dat$Pathway <- factor(as.character(Dat$Pathway), levels = names(table(as.character(Dat$Pathway))[order(table(as.character(Dat$Pathway)))]))
  Dat$Pathway <- unlist(lapply(strsplit(as.character(Dat$Pathway), " "), function(i){
    lb <- paste(c(i[1:3], "\n", i[4:6], "\n", i[7:10]), collapse = " ")
    gsub("NA", "", lb)
  }))


  if(length(table(Dat$FC)) < 2){
    stop(paste("Please specify", length(table(Dat$FC)), "colors", sep = "_"))
  }

  ## Signficant Pathway Map
  ggplot(Dat, aes(x = Gene, y = Pathway, fill = FC)) + geom_tile() +
    geom_rect(aes(xmin = -Inf,
      xmax = Inf, ymin = -Inf, ymax = Inf), fill = "black",
      alpha = 0.03) +
    xlab(datasetName) + scale_fill_manual(values=col) +
    geom_tile() +
    customTheme(sizeStripFont = 10, xAngle = 90, hjust = 1, vjust = 1, xSize = 5,
      ySize = 5, xAxisSize = 8, yAxisSize = 8) +
    theme(panel.background = element_rect(fill = "black")) +
    ylab(GeneSetName)
}

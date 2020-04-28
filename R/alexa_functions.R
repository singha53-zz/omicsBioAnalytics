#' @export
#' @rdname alexaMetadata
alexa_metadata <- function(demo, group, trim, format) {
  result <- omicsBioAnalytics::computeDescriptiveStats(demo,
    group, trim, format)
  # save figures to tempdir()
  mapply(function(var, var_type) {
    if(var_type == "continuous") {
      metadataPlot <- data.frame(x = factor(demo[, group]), y = demo[, var]) %>%
        ggplot(aes(x = x, y = y, color = x)) +
        geom_violin(trim=FALSE) +
        geom_dotplot(binaxis='y', stackdir='center', dotsize=1) +
        ylab(var) +
        xlab(paste0(group, "\n ", paste(paste0(levels(demo[, group]), " (n=", table(demo[, group]), ") "), collapse = ', '))) +
        theme(legend.position = "none") +
        theme_bw()
      ggsave(paste0(tempdir(), "/", paste(userID, "ds", var, sep="-"), ".png"), metadataPlot, device = "png", width = 4, height = 4)
      put_object(paste0(tempdir(), "/", paste(userID, "ds", var, sep="-"), ".png"), bucket = S3Bucket)
    } else if (var_type == "categorical") {
      df <- table(demo[, var], demo[, group])
      tg = gridExtra::tableGrob(df)
      h = grid::convertHeight(sum(tg$heights), "in", TRUE)
      w = grid::convertWidth(sum(tg$widths), "in", TRUE)
      ggplot2::ggsave(paste0(tempdir(), "/", paste(userID, "ds", var, sep="-"), ".png"), tg, width=w, height=h)
      put_object(paste0(tempdir(), "/", paste(userID, "ds", var, sep="-"), ".png"), bucket = S3Bucket)
    } else {
      NA
    }
  }, var = names(result$apl), var_type = result$var_type)
  return(result$apl)
}

#' @export
#' @rdname alexaEda
alexaEda = function(demo, group, omicsData){
  result <- lapply(names(omicsData), function(i){
    pcs = prcomp(
      omicsData[[i]],
      scale. = TRUE,
      center = TRUE,
      rank. = 2
    )
    pcaPlot <- pcs$x %>%
      as.data.frame() %>%
      mutate(group = demo[, group]) %>%
      ggplot(aes(x = PC1, y = PC2, color = group)) +
      geom_point(size = 3) +
      stat_ellipse(size = 2) +
      theme_classic() +
      customTheme(sizeStripFont = 15, xAngle = 0, hjust = 0.5, vjust = 0.5,
        xSize = 15, ySize = 15, xAxisSize = 15, yAxisSize = 15) +
      ylab(paste0("PC2 (", 100*signif(summary(pcs)$importance["Proportion of Variance", "PC2"], 2), "%)")) +
      xlab(paste0("PC1 (", 100*signif(summary(pcs)$importance["Proportion of Variance", "PC1"], 2), "%)"))
    ggsave(paste0(tempdir(), "/", paste(userID, "eda", i, sep="-"), ".png"), pcaPlot, device = "png", width = 4, height = 4)
    put_object(paste0(tempdir(), "/", paste(userID, "eda", i, sep="-"), ".png"), bucket = S3Bucket)

    list(primary = i,
      secondary = paste0("The first two PCS explain ", 100*signif(summary(pcs)$importance["Cumulative Proportion", "PC2"], 2), "% of the variation in the ", i, " dataset."),
      tertiary = paste0("PC1 ~ ", group, ", P = ", signif(summary(aov(lm(pcs$x[,"PC1"]~demo[, group])))[[1]][1, "Pr(>F)"], 2)),
      quaternary = paste0("PC2 ~ ", group, ", P = ", signif(summary(aov(lm(pcs$x[,"PC2"]~demo[, group])))[[1]][1, "Pr(>F)"], 2))
    )
  })
  names(result) <- names(omicsData)
  return(result)
}

#' @export
#' @rdname alexaDexp
alexaDexp = function(demo, group, omicsData){
  diffexp = function(datasets, response){
    design <- model.matrix(~response)
    lapply(datasets, function(i){
      fit <- eBayes(lmFit(t(i), design))
      top <- topTable(fit, coef = 2, adjust.method = "BH", n = nrow(fit), sort.by="none")
      top %>% mutate(FeatureName = colnames(i),
        sig = -log10(P.Value)) %>%
        arrange(P.Value)
    })
  }
  dexpResults <- diffexp(omicsData, demo[, group])

  dexpResults <- lapply(names(dexpResults), function(name){
    fdrs <- c(0.01, 0.05, 0.1, 0.2)
    fdrResults <- lapply(fdrs, function(fdr){
      top <- dexpResults[[name]] %>%
        mutate(Significant=factor(ifelse(adj.P.Val < fdr, paste("FDR < ", fdr), "Not Sig"), c(paste("FDR < ", fdr), "Not Sig")))

      volcanoPlot <- ggplot(top, aes(x = logFC, y = sig, color = Significant)) +
        geom_point() +
        theme_classic() +
        customTheme(sizeStripFont = 15, xAngle = 0, hjust = 0.5, vjust = 0.5,
          xSize = 15, ySize = 15, xAxisSize = 15, yAxisSize = 15) +
        ylab("-log10(p-value)") +
        xlab(expression("log2 fold-change")) +
        ggrepel::geom_text_repel(data = filter(top, adj.P.Val < fdr), aes(label = FeatureName))

      ggsave(paste0(tempdir(), "/", paste(userID, "dexp", name, fdr, sep="-"), ".png"), volcanoPlot, device = "png", width = 4, height = 3.7)
      put_object(paste0(tempdir(), "/", paste(userID, "dexp", name, fdr, sep="-"), ".png"), bucket = S3Bucket)

      # ggsave(paste0(tempdir(), "/", paste(userID, name, fdr, sep="-"), ".png"), volcanoPlot, device = "png")
      up <- sum(top$logFC > 0 & top$adj.P.Val < fdr)
      down <- sum(top$logFC < 0 & top$adj.P.Val < fdr)
      list(primary = name,
        secondary = paste0("There are ", sum(up, down), " significant (", up, " up- and ", down, " down-regulated) variables in the ", name, " dataset."),
        tertiary = "",
        quaternary = paste0("FDR = ", 100*fdr, "%."))
    })
    names(fdrResults) <- fdrs
    fdrResults
  })
  names(dexpResults) <- names(omicsData)
  dexpResults
}

#' @export
#' @rdname jaccard
jaccard <- function (s1, s2) {
  length(intersect(s1, s2))/length(union(s1, s2))
}

#' @export
#' @rdname generateTopTable
generateTopTable = function(eset, design, coefNumber, test){
  if (test == "limma"){
    fit <- eBayes(lmFit(t(eset), design))
    top <- topTable(fit, coef = coefNumber, adjust.method = "BH", n = nrow(fit), sort.by="none")
    top %>% mutate(FeatureName = colnames(eset),
      sig = -log10(P.Value)) %>%
      arrange(P.Value)
  } else if (test == "vlimma") {
    v <- voom(t(eset), design, plot=FALSE)
    fit <- lmFit(v, design)
    fit <- eBayes(fit)
    top <- topTable(fit, coef = coefNumber, n = nrow(fit), adjust.method = "BH")
    top %>% mutate(FeatureName = colnames(eset),
      sig = -log10(P.Value)) %>%
      arrange(P.Value)
  } else {
    # assume OLS
    fit <- lmFit(t(eset), design)
    fit$t <- fit$coef/fit$stdev.unscaled/fit$sigma
    fit$p.value <- 2 * pt(-abs(fit$t), df = fit$df.residual)
    top <- topTable(fit, coef = coefNumber, adjust.method = "BH", n = nrow(fit), sort.by="none")
    top %>% mutate(FeatureName = colnames(eset),
      sig = -log10(P.Value)) %>%
      arrange(P.Value)
  }
}

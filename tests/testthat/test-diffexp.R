test_that("multiplication works", {
  expect_equal(2 * 2, 4)
})


## 24h mock treated vs. 24h RSV treated (ACAP3) should be up in RSV (but isnt)
library(omicsBioAnalytics)
data("covid19")
library(tidyverse)

lib.size <- colSums(covid19$mrna)
y <- t(log2(t(covid19$mrna + 0.5)/(lib.size + 1) * 1e+06))
data.frame(gene = y[, "ACAP3"],
           group = covid19$demo$group) %>%
  ggplot(aes(x = group, y = gene, fill = group)) +
  geom_violin(trim = FALSE) +
  xlab("group") +
  ylab("ACAP3") +
  geom_jitter(shape = 16, position = position_jitter(0.2)) +
  ggtitle(paste("ACAP3", " vs. ", "group")) +
  theme_classic() +
  theme(legend.position = "none") +
  theme(axis.text.x = element_text(angle = 40,
                                   hjust = 1,
                                   vjust = 1,
                                   size = 8),
        axis.text.y = element_text(size = 8)) +
  stat_summary(fun.y=mean, colour="darkred", geom="point",
               shape=18, size=3,show_guide = FALSE)

response <- covid19$demo$group
design <- model.matrix(~relevel(factor(response), ref="24hrs after treatment_Mock treatment"))
top <- generateTopTable(eset=covid19$mrna, design, coefNumber=2, test="vlimma")
dplyr::filter(top, FeatureName == "ACAP3")

v <- limma::voom(t(covid19$mrna), design, plot=FALSE)
fit <- limma::lmFit(v, design)
fit <- limma::eBayes(fit)
top <- limma::topTable(fit, n = nrow(fit), coef = 2, adjust.method = "BH", sort.by="none")
top %>% dplyr::mutate(FeatureName = colnames(covid19$mrna),
                      sig = -log10(P.Value)) %>%
  dplyr::arrange(P.Value) %>%
  dplyr::filter(FeatureName == "ACAP3")


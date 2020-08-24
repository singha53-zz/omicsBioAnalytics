subset_response <- heartFailure$demo$hospitalizations
subset_eset <- heartFailure$omicsData[c(1,2,4)]

# parameters
alphaMin <- 0.5
alphaMax <- 1
alphalength <- 2
kfolds <- "fiveFold"
n_repeats <- 2
single <- setdiff(names(heartFailure$omicsData), "mrna")
ensem <- setdiff(names(heartFailure$omicsData), "mrna")
dataset_names <- unique(c(single, ensem))

ctrl <- caret::trainControl(method = "repeatedcv",
                            number = 5,
                            repeats = n_repeats,
                            summaryFunction = twoClassSummary,
                            classProbs = TRUE,
                            savePredictions = TRUE)
set.seed(123)
ctrl$index <- caret::createMultiFolds(subset_response, 5, n_repeats)

enetGrid = expand.grid(alpha = seq(alphaMin, alphaMax, length.out = alphalength),
                       lambda = seq(0.001, 0.1, by = 0.01))

mods <- vector("list", length(dataset_names))
names(mods) <- dataset_names

for (dat in dataset_names) {
  mods[[dat]] <- caret::train(x = subset_eset[[dat]], y = subset_response,
                              preProc = c("center", "scale"),
                              method = "glmnet",
                              metric = "ROC",
                              tuneGrid = enetGrid,
                              trControl = ctrl)
}

## Add ensemble panel
pred <- lapply(mods, function(i){
  i$pred
}) %>%
  do.call(rbind, .) %>%
  as.data.frame() %>%
  mutate(panel = rep(names(mods), each = nrow(mods[[1]]$pred)))
pred <- pred[, -c(1, 4)]
colnames(pred)[3] <- "Yes"
pred <- pred %>% dplyr::select(obs, rowIndex, Yes:panel)
pred$Resample <-  sapply(strsplit(pred$Resample, "\\."), function(i) i[2])
pred <- pred %>%
  dplyr::filter(panel %in% ensem) %>%
  dplyr::group_by(obs, rowIndex, alpha, lambda, Resample) %>%
  dplyr::summarise(Yes = mean(Yes)) %>%
  dplyr::mutate(panel = "Ensemble") %>%
  dplyr::full_join(pred, .)

perf <- pred %>%
  dplyr::group_by(panel, alpha, lambda, Resample) %>%
  tidyr::nest() %>%
  dplyr::mutate(auc = purrr::map(.x = data, .f = ~{
    as.numeric(pROC::roc(response = .x$obs, predictor = .x$Yes, direction = "<")$auc)
  })) %>%
  tidyr::unnest(auc) %>%
  dplyr::select(-data) %>%
  dplyr::group_by(panel, alpha, lambda) %>%
  dplyr::summarise(Mean = mean(auc),
                   SD = sd(auc)) %>%
  dplyr::ungroup() %>%
  dplyr::group_by(panel) %>%
  dplyr::filter(Mean == max(Mean)) %>%
  dplyr::filter(SD == max(SD)) %>%
  dplyr::arrange(desc(lambda)) %>%
  dplyr::slice(1) %>%
  dplyr::ungroup() %>%
  dplyr::arrange(dplyr::desc(Mean))

#' @export
#' @rdname splitData
splitData = function (demo, group = NULL, trim = 0.8) {
  assertthat::assert_that(!is.null(group), msg = "Please provide a group argument (must be a column name in demo)")
  assertthat::assert_that(group %in% colnames(demo), msg = "group must be present in the demographics dataset.")
  # assertthat::assert_that(is.factor(demo[, group]), msg = "group must be of class factor.")
  # assertthat::assert_that(nlevels(droplevels(demo[, group])) >1, msg = "group should at least 2 levels.")
  print("Number of samples in each group")
  print(table(demo[, group]))
  imbal <- scale(table(demo[, group]), center = FALSE, scale = nrow(demo)) %>%
    drop
  if(sum(imbal < 0.4) > 0) print("Class imbalance exists")
  missing <- colSums(!is.na(demo))/nrow(demo)
  varKeep <- missing[missing >= trim]
  if (length(setdiff(names(missing), names(varKeep))) > 0) {
    print("Missing variables and percentage of avaiable data")
    print(missing[missing < trim][order(missing[missing <
        trim])])
  }
  data <- demo[, names(varKeep)]
  dataList <- lapply(1:ncol(data), function(i) {
    if (nlevels(factor(as.character(data[, i]))) < 10) {
      x <- factor(as.character(data[, i]))
    }
    else {
      x <- as.numeric(data[, i])
    }
    x
  })
  names(dataList) <- colnames(data)
  data.new <- dataList %>% do.call(cbind.data.frame, .)
  data.cat <- data.new[, !sapply(data.new, is.numeric)]
  oneLevel <- which(dataList[colnames(data.cat)] %>% sapply(., function(i) nlevels(i)) == 1)
  rmCatVars <- names(oneLevel)
  if (length(oneLevel) > 0) {
    print(paste0("The categorical variable ", paste(names(which(dataList[colnames(data.cat)] %>%
        sapply(., function(i) nlevels(i)) == 1)), collapse = "/"),
      " only has 1 level; User should remove this variable"))
  } else {
    print("All categorial variables have more than 1 level: Good!")
  }
  data.cont <- data.new[, sapply(data.new, is.numeric), drop = FALSE]
  summary <- data.frame(Total_nVar = ncol(demo),
    missing_nVar = (ncol(demo) - length(varKeep)),
    nonMissing_nVar = length(varKeep),
    nonMissing_cat_nVar = ncol(data.cat),
    nonMissing_cont_nVar = ncol(data.cont))
  if(length(rmCatVars)>0){
    summary$rmCatVars <- rmCatVars
  }
  return(list(summary = summary, data.cat = data.cat, data.cont = data.cont))
}


summariseCatVar = function(demo, group, format){
  if(ncol(demo) < 2){
    return(NA)
  }
  assertthat::assert_that(group %in% colnames(demo), msg = "group must be present in the demographics dataset.")
  nlvls <- sapply(demo[, setdiff(colnames(demo), group)], nlevels)
  pvals <- sapply(setdiff(colnames(demo), group), function(i){
    assertthat::assert_that(!is.numeric(i), msg = "demo must only contain categorical variables.")
    assertthat::assert_that(length(levels(demo[,i])) > 1, msg = paste0(i, " has less than 2 levels."))
    chisq.test(demo[,i], demo[, group])$p.value
  })

  if(format == "dataframe"){
    result <- do.call(rbind, lapply(setdiff(colnames(demo), group), function(i){
      y <- demo[, i]
      tbl <- table(y, demo[, group])
      tbl2 <- paste(tbl, paste0("(", signif(100*tbl/length(y), 1), "%)"))
      dim(tbl2) <- dim(tbl)
      dimnames(tbl2) <- dimnames(tbl)
      df <- tbl2 %>%
        as.data.frame() %>%
        dplyr::mutate(variable = paste0("    ", rownames(.)))
      df[nrow(df) + 1, ] <- ''
      df$variable[nrow(df)] <- i
      df <- df[c(nrow(df), 1:(nrow(df)-1)), c(3, 1, 2)]
      df$`P-value` <- c(signif(pvals[i], 2), rep("", (nrow(df)-1)))
      df$Test <- "Pearson's Chi-squared Test"
      df
    }))
  } else {
    result <- lapply(setdiff(colnames(demo), group), function(i){
      y <- demo[, i]
      tbl <- as.matrix(as.data.frame.matrix(apply(table(demo[, group], demo[, i]), 1, function(i){
        round(100*i/sum(i), 0)
      }) %>% t))
      list(primary = paste0(i, " ~ ", group),
        secondary = paste(unlist(lapply(colnames(tbl), function(j){
          x <- tbl[, j]
          paste0(j, ": ", paste(paste(names(x), x, sep = "="), collapse = ", "))
        })), collapse = " | "),
        tertiary = paste0 ("P =", signif(pvals[i], 2)),
        quaternary = "Test: Pearson's Chi-squared Test")
    })
    names(result) <- setdiff(colnames(demo), group)
  }
  return(result)
}


summariseContVar = function(demo, group, format){
  if (ncol(demo) < 2) {
    return(NA)
  }
  assertthat::assert_that(group %in% colnames(demo), msg = "group must be present in the demographics dataset.")
  result <- t(apply(demo[, setdiff(colnames(demo), group)], 2, function(y){
    assertthat::assert_that(is.numeric(y), msg = "demo must only contain continuous variables.")
    x <- demo[, group]
    fit <- lm(y~x)

    testAssumptions <- gvlma::gvlma(fit)$GlobalTest[2:6] %>%
      do.call(rbind, .) %>%
      as.data.frame %>%
      dplyr::mutate(
        Test = c(
          "Global Stat",
          "Skewness",
          "Kurtosis",
          "Link Function",
          "Heteroscedasticity"
        ),
        Decision = ifelse(
          pvalue < 0.05,
          "Assumptions NOT satisfied!",
          "Assumptions acceptable."
        )
      ) %>%
      dplyr::mutate(Slope = Value) %>%
      dplyr::select(Test, pvalue, Decision) %>%
      dplyr::mutate(pvalue = signif(as.numeric(pvalue), 2))

    if(subset(testAssumptions, Test == "Global Stat")$pvalue < 0.05) {
      # linear model assumtions were not met use a Kruskal Wallis
      c(sapply(levels(x), function(lvl){
        paste0(signif(median(y[x == lvl], na.rm = TRUE), 2),
          " [",
          signif(quantile(y[x == lvl], prob = 0.25, na.rm = TRUE), 2),
          ", ",
          signif(quantile(y[x == lvl], prob = 0.75, na.rm = TRUE), 2),
          "]")
      }), "P-value" = signif(kruskal.test(y~x)$p.value, 2),
        "Test" = "Kruskal-Wallis Rank Sum Test")
    } else {
      # linear model assumtions were not met use a t-test
      c(sapply(levels(x), function(lvl){
        paste0(signif(mean(y[x == lvl], na.rm = TRUE), 2),
          " (",
          signif(sd(y[x == lvl], na.rm = TRUE), 2),
          ")")
      }), "P-value" = signif(summary(aov(fit))[[1]][1, "Pr(>F)"], 2),
        "Test" = "Linear Regression")
    }
  })) %>%
    as.data.frame() %>%
    dplyr::mutate(variable = row.names(.))
  if(format == 'dataframe'){
    result[, c(5, 4, 1, 2, 3)]
  } else {
    result2 <- split(result[, c(4, 1, 2, 3)], result$variable)
    result3 <- lapply(names(result2), function(j){
      lvls <- as.matrix(result2[[j]][, levels(demo[, group])])

      list(primary =  paste0(j, " ~ ", group),
        secondary = paste(paste(colnames(lvls), lvls, sep = "="), collapse = " | "),
        tertiary = paste0("P = ", as.matrix(result2[[j]]["P-value"])),
        quaternary = paste0("Test: ", as.matrix(result2[[j]]["Test"])))
    })
    names(result3) <- names(result2)
    result3
  }
}
#
# library(omicsBioAnalytics)
# data("heartFailure")
# demo <- heartFailure$demo
# group  <-  "hospitalizations"
# trim  <-  0.5
# format = "apl"
# result <- computeDescriptiveStats(demo, group, trim, format)
# result
# library(gridExtra)
# library(grid)
#
# df <- table(demo[, "Sex"], demo[, "hospitalizations"])
# p<-tableGrob(df)
# grid.arrange(p)

#' @export
#' @rdname computeDescriptiveStats
computeDescriptiveStats = function(demo, group = NULL, trim = 0.5, format){
  assertthat::assert_that(!is.null(group), msg = "Please provide a group argument (must be a column name in demo)")
  assertthat::assert_that(group %in% colnames(demo), msg = "group must be present in the demographics dataset.")
  # assertthat::assert_that(is.factor(demo[, group]), msg = "group must be of class factor.")
  # assertthat::assert_that(nlevels(droplevels(demo[, group])) > 1, msg = "group should have at least 2 levels.")
  print(colnames(demo))

  print(paste0("Summarize continous variables.", "\n"))
  ## Split data into categorical and continuous variables
  splitCatContVars <- splitData(demo, group, trim)

  print(paste0("Summarize categorical variables.", "\n"))
  ## categorical variables
  catDat <- splitCatContVars$data.cat[, setdiff(colnames(splitCatContVars$data.cat), splitCatContVars$summary$rmCatVars)]

  ## Chi-square test for categorical variables
  cat <- summariseCatVar(demo = catDat, group = group, format = format)

  ## continuous variables
  contDat <- splitCatContVars$data.cont
  contDat[, group] <- demo[, group]
  cont <- summariseContVar(demo = contDat, group = group, format = format)

  # kable(rbind(cont, cat), row.names = FALSE)
  if (format == "dataframe") {
    if (is.data.frame(cont) & is.data.frame(cat)){
      rbind(cont, cat)
    } else if (is.data.frame(cont) & !is.data.frame(cat)) {
      cont
    } else if (!is.data.frame(cont) & is.data.frame(cat)) {
      cat
    } else {
      NA
    }
  } else {
    if (is.list(cont) & is.list(cat)){
      list(apl = append(cont, cat),
        var_type = c(rep("continuous", length(cont)), rep("categorical", length(cat))))
    } else if (is.list(cont) & !is.list(cat)) {
      list(apl = cont,
        var_type = rep("continuous", length(cont)))
    } else if (!is.list(cont) & is.list(cat)) {
      list(apl = cat,
        var_type = rep("categorical", length(cat)))
    } else {
      NA
    }
  }
}

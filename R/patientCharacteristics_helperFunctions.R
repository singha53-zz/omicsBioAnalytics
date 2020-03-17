#' @export
#' @rdname splitData
splitData = function (demo, group, trim = 0.8)
{
  if (nrow(demo) < 10)
    stop("too few samples")
  print("Number of samples in each group")
  print(table(demo[, group]))
  imbal <- scale(table(demo[, group]), center = FALSE, scale = nrow(demo)) %>%
    drop
  if (sum(imbal < 0.4) > 0)
    print("Class imbalance exists")
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
  oneLevel <- which(dataList[colnames(data.cat)] %>% sapply(.,
    function(i) nlevels(i)) == 1)
  if (length(oneLevel) > 1) {
    print(paste0("The categorical variable ", paste(names(which(dataList[colnames(data.cat)] %>%
        sapply(., function(i) nlevels(i)) == 1)), collapse = "/"),
      " only has 1 level; User should remove this variable"))
  }
  else {
    print("All categorial variables have more than 1 level: Good!")
  }
  data.cont <- data.new[, sapply(data.new, is.numeric), drop = FALSE]
  summary <- data.frame(Total_nVar = ncol(demo), missing_nVar = (ncol(demo) -
      length(varKeep)), nonMissing_nVar = length(varKeep),
    nonMissing_cat_nVar = ncol(data.cat), nonMissing_cont_nVar = ncol(data.cont))
  return(list(summary = summary, data.cat = data.cat, data.cont = data.cont))
}

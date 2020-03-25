#' @export
#' @rdname jaccard
jaccard <- function (s1, s2) {
  length(intersect(s1, s2))/length(union(s1, s2))
}


stack <- function (x, keepNameOrder = FALSE, sorted = FALSE, ...) {
  keep <- vapply(x, is.vector, logical(1))
  if (!any(keep))
    stop("at least one vector element is required")
  if (!all(keep))
    warning("non-vector elements will be ignored")
  x <- x[keep]
  ind <- names(x)
  values <- unlist(x, recursive = TRUE, use.names = FALSE)
  if(length(values) != length(ind)){
    ind <- rep.int(ind, lengths(x))
  }
  ind <- if(keepNameOrder) factor(ind, unique(ind)) else as.factor(ind)
  if(sorted)
    data.frame(values, ind)[order(ind),]
  else
    data.frame(values, ind)
}

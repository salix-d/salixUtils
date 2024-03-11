#' Stack Vectors from a Data Frame or List
#'
# @param x
# @param keepNameOrder
# @param sorted
# @param ...
#
# @return
#' @export
stack <- function(x, keepNameOrder = FALSE, sorted = FALSE, keepFactors = FALSE, select = NULL) {
  x <- as.list(x)

  if (!all(vapply(x, \(x) all(lengths(x) == 1), logical(1))))
    stop("Some elements of `x` contains elements of different lengths. `x` should be a list of 1 level.")

  ind <- names(x)
  if (is.null(ind)) ind <- seq_along(x)

  if (!missing(select)) {
    nl <- as.list(seq_along(x))
    names(nl) <- names(x)
    keep <- ind[eval(substitute(select), nl, parent.frame())]
    x <- x[keep]
  }
  isFactor <- vapply(x, is.factor, logical(1))
  if (any(isFactor)) {
    x[isFactor] <- lapply(x[isFactor], factor2level)
  }
  ind <- if (keepNameOrder) factor(ind, unique(ind)) else as.factor(ind)

  out <- data.frame(
    values = c(x, recursive = TRUE, use.names = FALSE),
    ind = rep(ind, lengths(x))
  )

  if (sorted)
   out[order(ind),]
  else
    out
}

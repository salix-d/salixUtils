#' Remove empty elements from a vector, list, matrix, data.frame
#'
#' @param x       An R object
#' @param na.rm   (logical) If `TRUE`, consider NA values as empty. Default: `FALSE`
#' @param margin  (numeric | integer) For data.frame and matrix, to remove the  empty rows (1L) or empty columns (2L). Default: 2L.
#'
#' @return `x` without the empty elements
#'
#' @examples \dontrun{
#' x <- list(0, NULL, "a", NA, "")
#' rm_empty(x)
#' rm_empty(x, na.rm = TRUE)
#' df <- data.frame(a = c(1L, 2L, NA_integer_, 4L, 5L), b = c("a", "b", "", "d", "e"), c = NA)
#' }
#' @export
methods::setGeneric("rm_empty", signature = "x", function(x, na.rm = FALSE, margin = NULL) {
  if (!length(x)) {
    NULL
  } else {
    if (!missing(margin)) warning("Argument `margin` ignored")
    if (na.rm) x[lengths(x) > 0 & nzchar(x) & !is.na(x)]
    else x[lengths(x) > 0 & nzchar(x)]
  }
})

#' @rdname rm_empty
methods::setMethod(
  f = "rm_empty",
  signature = "matrix",
  definition = function(x, na.rm = FALSE, margin = c(1L, 2L)) {
    if (!length(x)) {
      NULL
    } else {
      margin = match.arg(margin, c(1L, 2L))

      y <- x == ""
      print(y)
      y[is.na(y)] <- if (na.rm) TRUE else FALSE
      print(y)
      switch(margin,
             {
               n <- which(rowSums(y) != ncol(x))
               x[n, , drop = FALSE]
             },
             {
               n <- which(colSums(y) != nrow(x))
               x[, n, drop = FALSE]
             })
    }
  },
  valueClass = "matrix"
)
#' @rdname rm_empty
methods::setMethod(
  f = "rm_empty",
  signature = "data.frame",
  definition = function(x, na.rm = FALSE, margin = 2L) {
    if (!length(x)) {
      NULL
    } else {
      y <- x == ""
      y[is.na(y)] <- if (na.rm) TRUE else FALSE
      switch(margin,
             {
               n <- which(rowSums(y) != ncol(x))
               x[n, , drop = FALSE]
             },
             {
               n <- which(colSums(y) != nrow(x))
               x[, n, drop = FALSE]
             })
    }
  },
  valueClass = "data.frame"
)

#' @rdname rm_empty
methods::setMethod(
  f = "rm_empty",
  signature = "data.table",
  definition = function(x, na.rm = FALSE, margin = 2L) {
    if (!length(x)) {
      NULL
    } else {
      y <- x == ""
      y[is.na(y)] <- if (na.rm) TRUE else FALSE
      switch(margin,
             {
               n <- which(rowSums(y) != ncol(x))
               x[n, , drop = FALSE]
             },
             {
               n <- which(colSums(y) != nrow(x))
               x[, ..n, drop = FALSE]
             })
    }
  },
  valueClass = "data.table"
)

#' @rdname rm_empty
methods::setMethod(
  f = "rm_empty",
  signature = signature("missing"),
  definition = function(x, na.rm = FALSE, margin = 2L) {
    stop("argument 'x' is missing, with no default")
  }
)

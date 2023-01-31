#' Return the First or Last Parts of an Object
#'
#' @param x an object
#' @param n an integer vector of length 1 or up to dim(x). Default is 6L. Negative values subtract n to the length or dimension. If only one value is provided but the object has more than one dimension, it will be used for all dimension (the difference with base::head()). If the object is a function, it will return n lines of the source code.
#'
#' @return A subsetted object.
#' @export
#'
#' @examples
head2 <- function(x, n = 6){
  if ("ftable" %in% class(x)) {
    r <- format(x, quote = FALSE)
    dimnames(r) <- list(rep.int("", nrow(r)), rep.int("", ncol(r)))
    if (length(n) == 1) {
      x <- head2(r, n = c(n + nrow(r) - nrow(x), n + ncol(r) - ncol(x)))
    } else {
      x <- head2(r, n = c(n[1] + nrow(r) - nrow(x), n[2] + ncol(r) - ncol(x)))
    }
    noquote(x)
  }
  else {
    if(is.function(x)) {
      if (n >= 0)
        x <- deparse(x, nlines = n)
      else {
        x <- deparse(x)
        x <- x[seq_len(max(length(x) + n, 0L))]
      }
      x <- as.matrix(x)
      dimnames(x) <- list(rep("", nrow(x)), "")
      noquote(x)
    } else {
      d <- dim(x)
      if (is.null(d)) {
        d <- length(x)
        x[seq_len(if (n < 0) max(d + n, 0L) else min(n, d))]
      } else {
        ld <- length(d)
        ln <- length(n)
        if (ln < ld) n[(ln + 1):ld] <- n[1]
        args <- rep(list(x, c(), drop = FALSE), c(1L, ld, 1L))
        for (i in seq_len(ld)) {
          args[[i + 1]] <- seq_len(if (n[i] < 0) max(d[i] + n[i], 0L) else min(n[i], d[i]))
        }
        do.call(`[`, args)
      }
    }
  }
}

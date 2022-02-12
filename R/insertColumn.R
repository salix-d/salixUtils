#' @param x         data.frame
#' @param ind       Numeric vector. At which index(es) should the new column(s) be inserted.
#' @param col.names Character vector. Name(s) of the new column(s).
#' @param data      Vector, matrix, array or data.frame. Default is NA.
#'
#' @export
insertColumns <- insertCol <- function(x, ind, col.names = ncol(df) + seq_along(colIndex), data = NA){
  out <- x
  out[, col.names] <- data
  out[, order(c(col(x)[1,], ind))]
}

#' @param x         data.frame
#' @param ind       Numeric vector. At which index(es) should the new column(s) be inserted.
#' @param col.names Character vector. Name(s) of the new column(s).
#' @param data      Vector, matrix, array or data.frame. Default is NA.
#'
#' @export
insertCol <- function(x, ind, col.names = NULL, data = NA){
  if(is.null(col.names)) col.names <- ncol(x) + seq_along(ind)
  col.order <- order(c(col(x)[1,], ind))
  x[,col.names] <- data
  x[,col.order]
}

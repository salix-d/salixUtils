#' Pad a vector or list of vector with NAs to a given length.
#'
#' @param x           A list of vector to pad.
#' @param length.out  integer: The length of vector after padding. Optional. If missing, takes the length of the longer list item.
#' @param pad         The value to fill the vector(s) with. Default is NA.
#'
#' @return a list or vector depending on input
#'
#' @examples \dontrun{
#' li <- list(a = 1)
#' }
#' @export
pad_list <- function(x, length.out = NULL, pad = NA){
  if (missing(x)) stop("argument 'x' missing, with no default")
  if (is.list(x)) {
    if (!length(length.out)) {
      length.out <- max(lengths(x))
    }
    out <-  lapply(x, `[`, seq_len(length.out))
    if (is.na(pad)) {
      out
    } else {
      lapply(x, \(y) `[<-`(y, is.na(y), pad))
    }
  } else {
    pad_list(list(x), length.out = length.out, pad = pad)[[1]]
  }
}

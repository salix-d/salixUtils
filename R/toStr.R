#' Convert an R Object to a Character String
#'
#' @description Similar to \link[base]{toString} but with additionnal parameters for more flexibility.
#'
#' @param x            The object to be converted.
#' @param and          (logical)  Whether to use "and" between the two last item. Default TRUE.
#' @param oxford_comma (logical) Whether to use an oxford comma. Default FALSE. Ignored if `and` set to FALSE.
#'
#' @return A character string.
#' @export
#'
#' @examples
#' toStr(letters[1:3])

toStr <- function(x, and = TRUE, oxford_comma = FALSE){
  n <- length(x)
  if (n > 1) {
    if (isTRUE(all.equal(x, suppressWarnings(as.integer(x)))) &&
        all(x == (seq_len(n) + x[[1]] - 1))) {
      x <- paste0(x[[1]], "to", x[[n]])
    } else if (and) {
      if (n == 2) {
        x <- paste0(x[1], " and ", x[2])
      } else {
        x <- paste0(paste(x[-n], collapse = ", "), if (oxford_comma) ", and " else " and ", x[n])
      }
    } else {
        x <- paste0(x, collapse = ", ")
    }
  }
  x
}

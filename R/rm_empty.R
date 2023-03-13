#' Remove empty elements from a vector or list
#'
#' @param x   A vector or list.
#'
#' @return The vector or list without the empty elements (length 0 or empty strings).
#'
#' @examples \dontrun{
#' x <- list(0, NULL, "a", NA, "")
#' rm_empty(x)
#' }
#' @export
rm_empty <- function(x) {
  if (!length(x)) {
    NULL
  } else {
    x[lengths(x) > 0 & nzchar(x)]
  }
}

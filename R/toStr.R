#' Convert an R Object to a Character String for prettier messages
#'
#' @param x The object to be converted.
#'
#' @details The object is first converted to character and then the elements are
#' concatenated together separated by ", ", and " and " for the last item.
#'
#' @return A character vector of length 1.
#' @export
#'
#' @examples
toStr <- function(x){
  if(!is.character(x)) x <- as.character(x)
  n <- length(x)
  paste(paste(x[-n], collapse = ", "), "and", x[n])
}

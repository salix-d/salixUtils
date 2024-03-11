#' Name a vector with the its values
#'
#' @param x vector
#'
#' @return named vector
#' @export
#'
#' @examples
#' nameself(letters)
nameself <- function(x) {
  `names<-`(x, x)
}

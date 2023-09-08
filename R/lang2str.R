#' Convert language to a string
#'
#' @param x language
#'
#' @return a character vector of length 1
#' @export
#'
#' @examples \dontrun{
#' lang2str(list())
#' }
#'
lang2str <- function(x){
  as.character(as.expression(substitute(expr = x, env = parent.frame(1L))))
}

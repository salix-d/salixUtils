#' Add line breaks to a string at a specified width.
#'
#' @param string (character) A character vector of length 1.
#' @param width  (integer|numeric) The number of character per line.
#'
#' @return A character vector of length 1 with the added line breaks.
#' @export
str2lines <- function(string, width = 80){
  if (length(width) != 1 || (!is.numeric(width) && !is.integer(width)) || width <= 0)
    stop("'width' must be a positive number.")
  if (nchar(string, type = "width") > width) {
    pattern <- paste0(".{1,",width,"}")
    strings <- stringi::stri_extract_all_regex(string, pattern = pattern)[[1]]
    string <- paste(trimws(strings), collapse = "\n")
  }
  string
}

#' Concatenate elements of a vector.
#'
#' @param x    (character) A character vector.
#' @param sep  (character) An character string to separate the elements.
#' Default is "".
#'
#' @description Concatenate elements of a vector after converting to character.
#'
#' @return A character string of the concatenated values.
#' @export
#'
concat <- function(x, sep = ""){
  paste(x, collapse = sep)
}

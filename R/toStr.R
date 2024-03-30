#' Convert a vector to a character string
#'
#' @description Similar to [toString][base::toString] but with additional parameters for more flexibility.
#'
#' @param x            A vector to be converted.
#' @param join_word    (character) The word to use between the two last item. Default is 'and'. Set to NULL to omit.
#' @param oxford_comma (logical) Whether to use an oxford comma. Default FALSE. Ignored if `and` set to FALSE.
#' @param quote        (logical) Whether to add single quotes around each word.
#'
#' @return A character string.
#' @export
#'
#' @examples
#' toStr(letters[1:3])
#' toStr(1:5)

toStr <- function(x, join_word = "and", quote = FALSE, oxford_comma = FALSE){
  if (missing(x)) stop("argument 'x' is missing, with no default")
  assert(join_word, what = c("character"), name = "join_word", length_check = 1L)
  if (!missing(oxford_comma)) oxford_comma <- assert_boolean(oxford_comma)
  if (!missing(quote)) quote <- assert_boolean(quote)
  n <- length(x)
  x <- x[nzchar(x)]
  if (quote) {
    x <- paste0("'", x, "'")
  }
  if (n > 1) {
    if (join_word != "and" &&
        join_word != "or" &&
        (is.integer(x) ||
         isTRUE(all.equal(x, suppressWarnings(as.integer(x))))) &&
        all(x == (seq_len(n) + x[1] - 1))
    ) {
      if (!grepl("[A-z]", join_word)) {
        paste0(x[[1]], join_word, x[n])
      } else {
        paste(x[[1]], join_word, x[n])
      }
    } else if (length(join_word)) {
      paste0(paste(x[-n], collapse = ", "), if (oxford_comma) ", " else " ", join_word, " ", x[n])
    } else {
      paste0(x, collapse = ", ")
    }
  } else {
    x
  }
}

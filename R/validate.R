#' Value(s) validation using partial matching
#'
#' @param x           The value(s) to validate
#' @param choices     A vector of candidate values
#' @param name        The name of the value being validated (used in error messages)
#' @param several.ok  (logical) Whether x can have more than one value. Default TRUE.
#'
#' @return NULL
#' @examples \dontrun{
#'  validate(x = "bird", choices = c("dog", "cat"), name = "pet")
#' }
#'
#' @export
validate <- function(x, choices, name, several.ok = TRUE){
  assert(x, what = class(choices)[[1]], name = name,
         check.length = if (several.ok) 0L else 1L)
  m <- pmatch(tolower(x), table = choices, nomatch = 0L)
  wrong <- m == 0L
  if (any(wrong)) {
    stop(
      toStr(x[wrong], quote = TRUE),
      if (sum(wrong) > 1)
        " are not valid `"
      else
        " is not a valid `",
      name,
      "`\n\tChoices are ",
      toStr(choices, join_word = "or", quote = TRUE),
      "(or a distinguishable abbreviation)",
      call. = FALSE
    )
  }
  choices[m]
}

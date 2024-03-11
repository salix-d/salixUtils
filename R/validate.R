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
validate <- function(x, choices, name, several.ok = TRUE) {
  assert(x, what = class(choices)[[1]], name = name,
         check.length = TRUE, x.length = if (several.ok) 0L else 1L)
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

#' Boolean value(s) validation
#'
#' @param x          (character | numeric | integer | logical) A vector.
#' @param name       (character) The name of the vector (used in error messages). Optional.
#' @param vectorize  (logical) Whether to vectorize the function when x has length > 1 (TRUE; Default)
#' @param keepNA     (logical) Whether to keep NA values (TRUE) or coerced them to false (FALSE; Default)
#'
#' @description
#' Check that each element of x is either TRUE or FALSE. Works with vectors containing logical values, and/or 0s and 1s, and/or "true", "t", "false", "f" and "na" strings (regardless of case). Coerce the non logical values to their logical equivalent. If keepMissing is FALSE (default), missing values will be coerced to FALSE.
#'
#'
#' @return A logical vector
#' @export
validate_boolean <- function(x, name = lang2str(x), keepMissing = FALSE) {
  if (!is.atomic(x))
    stop("Argument `x` should be an atomic vector.", call. = FALSE)
  if (!length(x)) {
    if (!keepMissing) out <- FALSE
    else stop("'", name, "' should be one of TRUE or FALSE", call. = FALSE)
  } else if (length(x) == 1) {
    if (keepMissing && !(is_true(x) | is_false(x)))
      stop("'", name, "' should be one of TRUE or FALSE", call. = FALSE)
    else
      out <- is_true(x)
  } else {
    if (keepMissing && !all(is_true(x) | is_false(x))) {
      stop("Every element of '", name, "' should be one of TRUE or FALSE", call. = FALSE)
    } else {
      out <- is_true(x)
    }
  }
  out
}

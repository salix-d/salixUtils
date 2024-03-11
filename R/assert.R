#' Assert the class and length of an object
#'
#' @param x             an R object
#' @param what          (character) The class(es) that the object should be
#' @param name          (character) The name of the R object (used in error messages)
#' @param length_check  (logical) Whether to check if the object has length (Default: TRUE)
#' @param length_req    (numeric) The required length of x.
#' @param stopOnFail    Whether the function stops as soon as one of the check fails, or keep going to gather all error messages at once.
#'
#' @return NULL
#' @export
assert <- function(x,
                   what,
                   name = NULL,
                   length_check = TRUE,
                   length_req = NULL,
                   stopOnFail = NULL) {
  if (is.null(name)) {
    name <- lang2str(x)
  }
  assert(length_check, "logical", "length_check", 1L)
  assert(length_req, "logical", "length_req", 1L)
  msgLen <- NULL
  msgClass <- NULL
  if (length_check) {
    if (is.null(length_req)) {
      length_req <- 0L
    } else {
      assert(x.length, c("numeric", "integer"), "x.length", TRUE, 1L)
      x.length <- as.integer(x.length)
      # assert_positive(x.length)
    }
    msgLen <- assert_length(
      x = x,
      len = check.length,
      name = name,
      stopOnFail = {
        if (is.logical(stopOnFail) && !is.na(stopOnFail))
          stopOnFail
        else
          length(x) > 0
      }
    )
  }
  if (length(x)) {
    msgClass <- assert_class(
      x = x,
      what = what,
      name = name,
      is2nd = length(msgLen),
      stopOnFail = {
        if (is.logical(stopOnFail) && !is.na(stopOnFail))
          stopOnFail
        else
          FALSE
      }
    )
  }
  msg <- c(msgLen, msgClass)
  if (length(msg)) {
    stop(msg, call. = FALSE)
  }
}
#' @export
assert_length <- function(x, len, name, stopOnFail = TRUE) {
  .fun <- if (stopOnFail) stop else paste0
  if (len == 0L && !length(x)) {
    .fun("'", name, "' can't be empty")
  } else if (len > 0 && length(x) != len) {
    .fun("'", name, "' must be length ", len)
  } else {
    NULL
  }
}
#' @export
assert_class <- function(x, what, name, is2nd = FALSE, stopOnFail = TRUE) {
  .fun <- if (stopOnFail) stop else paste0
  if (what == "boolean") {
    assert_boolean(x = x, name = name, is2nd = is2nd, stopOnFail = stopOnFail)
  } else if (!inherits(x = x, what = what)) {
    what <- toStr(x = what, join_word = "or")
    if (!is2nd)
      .fun("'", name, "' must be of class ", what)
    else
      .fun(" and of class ", what)
  } else {
    NULL
  }
}
#' Assert if an object contains only TRUE or FALSE values
#'
#' @param x          (character | numeric | integer | logical) A vector.
#' @param name       (character) The name of the vector (used in error messages). Optional.
#' @param stopOnFail (logical) Whether the function stops as soon as one of the check fails, or gathers all error messages first.
#'
#' @description
#' Check that each element of x is either TRUE or FALSE. Works with vectors containing logical values, and/or 0s and 1s, and/or "true", "t", "false", "f" strings (regardless of case).
#'
#'
#' @return NULL
#' @export
assert_boolean <- function(x, name = lang2str(x), is2nd = FALSE, stopOnFail = TRUE) {
  .fun <- if (stopOnFail) stop else paste0
  if (!all(is_true(x) | is_false(x))) {
    if (length(x) == 1) {
      if (!is2nd) {
        .fun("'", name, "' should be one of TRUE or FALSE")
      } else {
        .fun(" and should be one of TRUE or FALSE")
      }
    } else {
      if (!is2nd) {
        .fun("'", name, "' should only contains TRUE or FALSE values")
      } else {
        .fun(" and should only contains TRUE or FALSE values")
      }
    }
  } else {
    NULL
  }
}

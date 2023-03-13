assert <- function(x, what, name = NULL, check.length = NULL,
                   stopOnFail = NULL) {
  if (missing(name)) {
    name <- deparse(substitute(x))
  }
  msgLen <- if (length(check.length) && !isFALSE(check.length)) {
    assert_length(x = x, len = check.length, name = name,
                    stopOnFail = if (is.logical(stopOnFail) && !is.na(stopOnFail)) stopOnFail else length(x) > 0)
  } else {
    NULL
  }
  msgClass <- if (length(x)) {
    assert_class(x = x, what = what, name = name,
                   is2nd = length(msgLen), stopOnFail = if (is.logical(stopOnFail) && !is.na(stopOnFail)) stopOnFail else FALSE)
  } else {
    NULL
  }
  msg <- c(msgLen, msgClass)
  if (length(msg)) {
    stop(msg, call. = FALSE)
  }
}
assert_class <- function(x, what, name, is2nd = FALSE, stopOnFail = TRUE) {
  .fun <- if (stopOnFail) stop else paste0
  if (!inherits(x = x, what = what)) {
    if (!is2nd)
      .fun("'", name, "' must be of class ", toStr(x = what, join_word = "or"))
    else
      .fun(" and of class ", toStr(x = what, join_word = "or"))
  } else {
    NULL
  }
}
assert_length <- function(x, len, name, stopOnFail = TRUE) {
  len <- as.integer(len)
  if (!is.na(len) && len >= 0) {
    .fun <- if (stopOnFail) stop else paste0
    if (len == 0 && !length(x)) {
      .fun("'", name, "' can't be empty")
    } else if (length(x) != len) {
      .fun("'", name, "' must be length ", len)
    }
  }
}
assert_logical <- function(x, name = deparse(substitute(x))) {
  assert_length(x, len = 1L, name = name)
  x <- tolower(x)
  if (x == "true" || x == "1")
    TRUE
  else if (x == "false" || x == "0" || x == "na")
    FALSE
  else
    stop("'", name, "' should be one of TRUE or FALSE")
}
validate <- function(x, choices, name){
  wrong <- !x %in% choices
  if (any(wrong)) {
    stop(
      ennum(x[wrong], quote = TRUE),
      if (sum(wrong) > 1)
        " are not valid "
      else
        " is not a valid ",
      name,
      "\nChoices are ",
      ennum(choices, join_word = "or", quote = TRUE),
      call. = FALSE
    )
  }
}

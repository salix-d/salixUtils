#' Assert the class and length of an object
#'
#' @param x             an R object
#' @param what          (character) The class(es) that the object should be
#' @param name          (character) The name of the R object (used in error messages)
#' @param check.length  Whether to check if the object has length (logical) or the length to assert (integer)
#' @param stopOnFail    Whether the function stops as soon as one of the check fails, or keep going to gather all error messages at once.
#'
#' @return NULL
#' @export
assert <- function(x, what, name = NULL, check.length = NULL,
                   stopOnFail = NULL) {
  if (is.null(name)) {
    name <- lang2str(x)
  }
  msgLen <- {
    if (length(check.length) == 1L && !isFALSE(check.length)) {
      check.length <- {
        if (isTRUE(check.length))
          0L
        else
          as.integer(check.length)
      }
      if (!is.na(check.length) && check.length >= 0) {
        assert_length(
          x = x, len = check.length, name = name,
          stopOnFail = {
            if (is.logical(stopOnFail) && !is.na(stopOnFail))
              stopOnFail
            else
              length(x) > 0
            }
          )
      }
    } else {
      NULL
    }
  }
  msgClass <- {
    if (length(x)) {
      assert_class(
        x = x, what = what, name = name, is2nd = length(msgLen),
        stopOnFail = {
          if (is.logical(stopOnFail) && !is.na(stopOnFail))
            stopOnFail
          else
            FALSE
        }
      )
    } else {
      NULL
    }
  }
  msg <- c(msgLen, msgClass)
  if (length(msg)) {
    stop(msg, call. = FALSE)
  }
}
assert_length <- function(x, len, name, stopOnFail = TRUE) {
  .fun <- if (stopOnFail) stop else paste0
  if (len == 0 && !length(x)) {
    .fun("'", name, "' can't be empty")
  } else if (len > 0 && length(x) != len) {
    .fun("'", name, "' must be length ", len)
  } else {
    NULL
  }
}
assert_class <- function(x, what, name, is2nd = FALSE, stopOnFail = TRUE) {
  .fun <- if (stopOnFail) stop else paste0
  if (!inherits(x = x, what = what)) {
    what <- toStr(x = what, join_word = "or")
    if (!is2nd)
      .fun("'", name, "' must be of class ", what)
    else
      .fun(" and of class ", what)
  } else {
    NULL
  }
}
is.true <- function(x, vectorize = TRUE){
  if (vectorize && length(x) > 1L) {
    assert_class(x, c("logical", "numeric", "integer", "character"), name = "x", stopOnFail = FALSE)
    switch(
      class(x)[1],
      logical = x,
      numeric = x == 1,
      integer = x == 1,
      character =
        x <- tolower(x)
      if (keepNA) {
        x[x %in%  c("na",
                    "na_character_",
                    "na_real_",
                    "na_integer_")] <- NA_character_
      } else {
        x[is.na(x)] <- "false"
      }
      x == "true" | x == "t" | x == "1"
      # (
      #   (!is.na(as.logical(x)) & as.logical(x)) |
      #     (!is.na(as.integer(x)) & as.integer(x) == 1)
      #   )
    )
  } else {
    if (length(x) == 1L &&
        inherits(x, c("logical", "numeric", "integer", "character")) &&
        !is.na(x)) {
      switch(
        class(x)[1],
        logical = x,
        numeric = x == 1,
        integer = x == 1,
        character = (as.logical(x) || as.integer(x) == 1)
      )
    } else {
      FALSE
    }
  }
}
is.false <- function(x, keepNA = FALSE, keepEmpty = FALSE, vectorize = TRUE){
  if (vectorize && length(x) > 1L) {
    if (is.list(x)) {
      lapply(
        if (keepEmpty)
          x[lengths(x) > 0]
        else
          x,
        is.false,
        keepNA = keepNA,
        keepEmpty = keepEmpty,
        vectorize = vectorize
      )
    } else {
      (if (keepNA) F else is.na(x)) |
        switch(
          class(x)[1],
          logical = !x,
          numeric = x == 0,
          integer = x == 0,
          character = {
            x <- tolower(x)
            x[x %in%  c(NA,
                        "na",
                        "na_character_",
                        "na_real_",
                        "na_integer_")] <- if (keepNA) NA_character_ else "false"
            x == "false" | x == "f" | x == "0"
          },
          rep.int(FALSE, length(x))
        )
    }
  } else {
    length(x) == 1L &&
      switch(
        class(x)[1],
        logical = !x,
        numeric = x == 0,
        integer = x == 0,
        character = {
          x <- tolower(x)
          if (x %in% c("false", "f", "0")) {
            TRUE
          } else if (is.na(x) ||
                     x %in% c("na", "na_character_", "na_real_", "na_integer_")) {
            if (keepNA) {
              NA
            } else {
              TRUE
            }
          } else {
            FALSE
          }
        }
      )
  }
}
is.false(x, keepNA = T)
is.false(x, keepNA = F)
is.false <- function(x, keepNA = FALSE, vectorize = FALSE){
  (if (vectorize) {
    lengths(x) == 1L
    } else {
      lengths(x) == 1L
    }) &
      ((if (keepNA) {
          is.logical(x) & !is.na(x) & !x
        } else {
          is.na(x) | (is.logical(x) & !x)
        }) |
          x == 0 | tolower(x) == "false")
  } else {
    if (keepNA)
      length(x) == 1L && ((is.logical(x) && !is.na(x) && !x) ||
        x == 0 || tolower(x) == "false")
    else
      length(x) == 1L && (is.na(x) || (is.logical(x) && !x) ||
        x == 0 || tolower(x) == "false")
  }
}
#' Assert if an object contains only TRUE or FALSE values
#'
#' @param x          (character | numeric | integer | logical) A vector.
#' @param name       (character) The name of the vector (used in error messages). Optional.
#' @param vectorize  (logical) Whether to vectorize the function when x has length > 1 (TRUE; Default)
#' @param keepNA     (logical) Whether to keep NA values (TRUE) or coerced them to false (FALSE; Default)
#'
#' @description
#' Check that each element of x is either TRUE or FALSE. Works with vectors containing logical values, and/or 0s and 1s, and/or "true", "t", "false", "f" and "na" strings (regardless of case). Coerce the non logical values to their logical equivalent. If keepNA is FALSE (default), NAs will be coerced to FALSE.
#'
#'
#' @return A logical vector
#' @export
assert_TF <- function(x, name = lang2str(x), vectorize = TRUE) {
  if (!length(x)) {
    x <- FALSE
  } else if (vectorize && length(x) > 1) {
    if (!is.atomic(x)) stop("Argument `x` should be an atomic vector.", call. = FALSE)
    i_T <- is.true(x, vectorize = TRUE)
    i_F <- is.false(x, vectorize = TRUE)
    if (all(i_T | i_F)) {
      x[i_T] <- TRUE
      x[i_F] <- FALSE
    } else {
      stop("Every element of '", name, "' should be one of TRUE or FALSE", call. = FALSE)
    }
  } else {
    if (is.true(x)) x <- TRUE
    else if (is.false(x)) x <- FALSE
    else
      stop("'", name, "' should be one of TRUE or FALSE", call. = FALSE)
  }
  x
}

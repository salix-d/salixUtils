is_true <- function(x, recursive = TRUE) {
  if (!length(x)) {
    FALSE
  } else if (is.list(x)) {
    if (recursive) {
      lapply(x, is_true)
    } else {
      FALSE
    }
  } else {
    out <- rep.int(FALSE, length(x))
    if (is.numeric(x) |
        is.integer(x) | is.complex(x)) {
      out[x == 1] <- TRUE
    } else if (is.character(x)) {
      x <- tolower(x)
      out[x == "true" | x == "t" | x == "1"] <- TRUE
    } else if (!is.logical(x)) {
      stop(class(x), " is an unsupported class")
    }
    `names<-`(out, x)
  }
}
is_false <- function(x, recursive = TRUE) {
  if (!length(x)) {
    FALSE
  } else if (is.list(x)) {
    if (recursive) {
      lapply(x, is_false)
    } else {
      FALSE
    }
  } else {
    out <- rep.int(FALSE, length(x))
    if (is.numeric(x) | is.integer(x) | is.complex(x)) {
      out[x == 0] <- TRUE
    } else if (is.character(x)) {
      x <- tolower(x)
      out[x == "false" | x == "f" | x == "0"] <- TRUE
    } else if (!is.logical(x)) {
      stop(class(x), " is an unsupported class")
    }
    `names<-`(out, x)
  }
}
is_na <- function(x, na_values = c("na", "n/a", "null", ""), recursive = TRUE) {
  if (!length(x)) {
    FALSE
  } else if (is.list(x)) {
    if (recursive) {
      lapply(x, is_na, na_values = na_values)
    } else {
      FALSE
    }
  } else {
    if (is.logical(x) | is.numeric(x) | is.integer(x) | is.complex(x)) {
      is.na(x)
    } else if (is.character(x)) {
      x <- tolower(x)
      x %in% na_values | is.na(x)
    } else {
      stop(class(x), " is an unsupported class")
    }
  }
}

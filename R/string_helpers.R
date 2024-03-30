#=== extract ===================================================================
#' @export
str_extract_first <- function(str, pattern, fixed = FALSE, ...) {
  ({
    if (fixed)
      stringi::stri_extract_first_fixed
    else
      stringi::stri_extract_first_regex
  })(str = str,
     pattern = pattern,
     ...)
}
#' @export
str_extract_last <- function(str, pattern, fixed = FALSE, ...) {
  ({
    if (fixed)
      stringi::stri_extract_last_fixed
    else
      stringi::stri_extract_last_regex
  })(str = str,
     pattern = pattern,
     ...)
}
#' @export
str_extract_all <- function(str, pattern, fixed = FALSE, simplify = TRUE, ...) {
  ({
    if (fixed)
      stringi::stri_extract_all_fixed
    else
      stringi::stri_extract_all_regex
  })(str = str,
     pattern = pattern,
     simplify = simplify,
     ...)
}
#' @export
str_extract <- function(str, pattern, mode = "all", fixed = FALSE, ...) {
  validate(mode, c("all", "first", "last"), several.ok = FALSE)
  switch(mode,
         first = str_extract_first,
         last = str_extract_last,
         all = str_extract_all
  )(str = str, pattern = pattern, fixed = fixed, ...)
}
#' @export
str_words <- function(str, mode = "first", ...) {
  validate(mode, c("first", "last", "all"))
  switch(mode,
         first = stringi::stri_extract_first_words,
         last = stringi::stri_extract_last_words,
         all = stringi::stri_extract_all_words,
  )(str = str, ...)
}
#===============================================================================
#=== replace ===================================================================
#' @export
str_replace_all <- function(str, pattern, replacement, fixed = FALSE,
                            vectorize_all = FALSE, ...) {
  ({
    if (fixed)
      stringi::stri_replace_all_fixed
    else
      stringi::stri_replace_all_regex
  })(
    str = str,
    pattern = pattern,
    replacement = replacement,
    vectorize_all = vectorize_all,
    ...
  )
}
#' @export
str_replace_first <- function(str, pattern, replacement, fixed = FALSE, ...) {
  ({
    if (fixed)
      stringi::stri_replace_first_fixed
    else
      stringi::stri_replace_first_regex
  })(str = str,
     pattern = pattern,
     replacement = replacement,
     ...)
}
#' @export
str_replace_last <- function(str, pattern, replacement, fixed = FALSE, ...) {
  ({
    if (fixed)
      stringi::stri_replace_last_fixed
    else
      stringi::stri_replace_last_regex
  })(str = str,
     pattern = pattern,
     replacement = replacement,
     ...)
}
#' @export
str_replace <- function(str, pattern, replacement,
                        mode = "all", fixed = FALSE,
                        vectorize_all = FALSE, ...) {
  switch(mode,
         all = str_replace_all(str = str, pattern = pattern,
                               replacement = replacement, fixed = fixed,
                               vectorize_all = vectorize_all, ...),
         first = str_replace_first(str = str, pattern = pattern,
                                   replacement = replacement, fixed = fixed, ...),
         last = str_replace_last(str = str, pattern = pattern,
                                 replacement = replacement, fixed = fixed, ...),
         {
           warning("Invalide mode. Returning string unchanged")
           str
         }
  )
}
#===============================================================================
#=== detect ====================================================================
#' @export
str_detect <- function(str, pattern, fixed = FALSE, ...) {
  if (!fixed)
    stringi::stri_detect_regex(str = str, pattern = pattern, ...)
  else
    stringi::stri_detect_fixed(str = str, pattern = pattern, ...)
}
#' @export
str_detect_which <- function(str, pattern, fixed = FALSE, ...) {
  which(str_detect(str = str, pattern = pattern, ...))
}
#===============================================================================
#=== split =====================================================================
#' @export
str_split <- function(str, pattern, fixed = FALSE, ...) {
  if (!fixed)
    stringi::stri_split_regex(str = str, pattern = pattern, ...)
  else
    stringi::stri_split_fixed(str = str, pattern = pattern, ...)
}
#' @export
str_lines <- function(str, omit_empty = FALSE, simplify = TRUE, use.names = TRUE) {
  salixUtils::assert(str, "character", "str", length_check = TRUE)
  salixUtils::assert(omit_empty, "logical", "omit_empty", length_check = TRUE)

  out <- {
    if (length(str) == 1 && simplify && !omit_empty) {
      stringi::stri_split_lines1(str = str)
    } else if (simplify) {
      c(stringi::stri_split_lines(str = str, omit_empty = omit_empty),
        recursive = TRUE, use.names = FALSE)
    } else {
      stringi::stri_split_lines(str = str, omit_empty = omit_empty)
    }
  }
  if (use.names && !is.null(names(str))) {
    names(out) <- names(str)
  }
  out
}
#===============================================================================
#=== count =====================================================================
#' @export
str_count <- function(str, pattern, fixed = FALSE, ...) {
  if (!fixed)
    stringi::stri_count_regex(str = str, pattern = pattern, ...)
  else
    stringi::stri_count_fixed(str = str, pattern = pattern, ...)
}
#===============================================================================
#=== drop ======================================================================
#' @export
str_drop <- function(str, pattern, fixed = FALSE) {
  to <- {
    if (!fixed)
      stringi::stri_locate_first_regex(str = str, pattern = pattern)[[1]]
    else
      stringi::stri_locate_first_fixed(str = str, pattern = pattern)[[1]]
  }
  stringi::stri_sub(str = str, to = (to - 1L))
}
#===============================================================================
#=== wrap ======================================================================
#' Add line breaks to a string at a specified width.
#'
#' @param string (character) A character vector of length 1.
#' @param width  (integer|numeric) The number of character per line.
#'
#' @return A character vector of length 1 with the added line breaks.
#' @export
str_wrap <- function(string, width = 80){
  if (length(width) != 1 || (!is.numeric(width) && !is.integer(width)) || width <= 0)
    stop("'width' must be a positive number.")
  if (nchar(string, type = "width") > width) {
    pattern <- paste0(".{1,",width,"}")
    strings <- str_extract_all(string, pattern = pattern, simplify = TRUE)
    string <- paste(trimws(strings), collapse = "\n")
  }
  string
}
#' @export
str_wrap <- function(string, width = 80){
  if (length(width) != 1)
    stop("'width' must be of length 1")
  if (!is.numeric(width) && !is.integer(width))
    stop("'width' must be of type integer or numeric")
  if (width <= 0)
    stop("'width' must be greater than 0")
  if ((n <- nchar(string, type = "w")) > width) {
    lttrs <- strsplit(string, "")[[1]]
    strings <- character(ceiling(n / (width - 1)))
    i <- 1
    for (j in seq_along(lttrs)) {
      l <- lttrs[j]
      n <- nchar(strings[[i]])
      if (n + 1 != width) {
        if (n == 0 && l == " ")
          next
        else
          strings[[i]] <- paste0(strings[[i]], l)
      } else {
        if (l %in% c(LETTERS, letters, 0:9) && lttrs[j + 1] %in% c(LETTERS, letters, 0:9)) {
          if (lttrs[j - 1] %in% c(LETTERS, letters, 0:9))
            strings[[i]] <- paste0(strings[[i]], "-")
          else
            strings[[i]] <- paste0(strings[[i]], " ")
          i <- i + 1
          strings[[i]] <- paste0(strings[[i]], l)
        } else {
          strings[[i]] <- paste0(strings[[i]], l)
          i <- i + 1
        }
      }
    }
    string <- concat(strings[strings != ""], "\n")
  }
  string
}


#' @export
duplicates <- dups <- function(input, incomparables = FALSE, fromLast = FALSE, nmax = NA, ...){
  c(
    integer = salixUtils:::C_duplicates_int,
    double = salixUtils:::C_duplicates_dbl,
    logical = salixUtils:::C_duplicates_log,
    character = salixUtils:::C_duplicates_char
  )[[typeof(input)]](input)
}
duplicates2 <- dups <- function(input, incomparables = FALSE, fromLast = FALSE, nmax = NA, ...){
  unique(input[duplicated(input, incomparables = incomparables, ...)])
}
#' @export
is.duplicates <- is.dups <- function(input, incomparables = FALSE, ...){
  input %in% unique(input[duplicated(input, incomparables = incomparables, ...)])
}

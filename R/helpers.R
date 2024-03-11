# is empty
#' @export
is.empty     <- function(x) if (length(x)) lengths(x) == 0 else TRUE
is.empty.chr <- function(x) identical(x, character(0))
is.empty.num <- function(x) identical(x, numeric(0))
is.empty.int <- function(x) identical(x, integer(0))
is.empty.lst <- function(x) identical(x, list())

# others
#' @export
rowAny <- function(x) rowSums(x) > 0
#' @export
rowAll <- function(x) rowSums(x) == ncol(x)

#' @export
factor2level <- function(x) {
  vapply(x, \(y) which(levels(x) == y), 0)
}

# vapply shortcuts
v.chr     <- function(x, fun, ...) vapply(X = x, FUN = fun, FUN.VALUE = character(1), ...)
v.int     <- function(x, fun, ...) vapply(X = x, FUN = fun, FUN.VALUE = integer(1), ...)
v.dbl     <- function(x, fun, ...) vapply(X = x, FUN = fun, FUN.VALUE = numeric(1), ...)
v.lgl     <- function(x, fun, ...) vapply(X = x, FUN = fun, FUN.VALUE = logical(1), ...)
v.lst     <- function(x, fun, ...) vapply(X = x, FUN = fun, FUN.VALUE = vector("list", 1), ...)

# operators
`%ni%` <- Negate('%in%')

# not empty
is.empty     <- function(x) lengths(x) == 0
is.empty.chr <- function(x) identical(x, character(0))
is.empty.num <- function(x) identical(x, numeric(0))
is.empty.int <- function(x) identical(x, integer(0))
is.empty.lst <- function(x) identical(x, list())

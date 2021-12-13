#' @name extract
#' @title        extractting Vectors and List
#' @description  extract extracts the elements of a vector for which a formula or function gives true. If the object is a list, the formula or function is evaluated using vapply.
#' @param x      a data vector (including list) to be filtered
#' @param f      a function or a formula to use as a function. Return must be of type logical.
#' @return       the filtered data object
#' @usage
#'             extract(1:5, ~x>3)
#'
#'             dat <- list(list(item = 1, data = NULL), list(item = 2, data = data.frame(x = letters[1:5], y = 1:5)))list(list(item = 1, data = NULL), list(item = 2, data = data.frame(x = letters[1:5], y = 1:5)))
#'             extract(dat, ~!is.null(x$data))
#' @export
extract <- function(x, f){
  UseMethod("extract", object = f)
}
#' @exportMethod extract function
#' @method extract function
#' @export
extract.function <- function(x, f){
  UseMethod("extract.function", object = x)
}
#' @method extract.function list
#' @export
extract.function.list <- function(x, f){
  x[vapply(x, f, logical(1))]
}
#' @method extract.function vector
#' @export
extract.function.vector <- function(x, f){
  x[f(x)]
}
#' @method extract.function numeric
#' @export
extract.function.numeric <- extract.function.vector
#' @method extract.function logical
#' @export
extract.function.logical <- extract.function.vector
#' @method extract.function integer
#' @export
extract.function.integer <- extract.function.vector
#' @method extract.function character
#' @export
extract.function.character <- extract.function.vector
#' @method extract formula
#' @export
extract.formula <- function(x, f) UseMethod("extract.formula", object = x)
#' @method extract.formula list
#' @export
extract.formula.list <- function(x, f) x[vapply(x, as.fun(f), logical(1))]
#' @method extract.formula vector
#' @export
extract.formula.vector <- function(x, f) x[as.fun(f)(x)]
#' @method extract.formula logical
#' @export
extract.formula.logical <- extract.formula.vector
#' @method extract.formula numeric
#' @export
extract.formula.numeric <- extract.formula.vector
#' @method extract.formula integer
#' @export
extract.formula.integer <- extract.formula.vector
#' @method extract.formula character
#' @export
extract.formula.character <- extract.formula.vector

#' @name as.fun
#' @title Convert Formula to Function
#' @param    f a right sided formula (i.e ~ x==1)
#' @return   a function which evaluates the given formula
#' @note     the function passes an argument x, so x should be the variable in the formula
#' @usage as.fun(~ x > 3)
as.fun <- function(f) {
  if (length(f) == 3) stop("Only right sided formula allowed.")
  `functionBody<-`(function(x){}, value = f[[2]])
}
# awk '/^\S* <- function/{gsub(/\./," ",$1);print "#\047 @method", $1}' ./R/extract.R
# bench::mark(
#   Filter(function(x) !is.null(x$data), gbif),
#   extract(gbif, ~ !is.null(x$data)),
#   purrr::discard(gbif, ~ is.null(.x$data)),
#   iterations = 100
# )
setGeneric("extract", def = function(x, f) x[vapply(x, f, logical(1))])
con <- file("./R/extract_methods.R", "w")
vapply(list(
  c("list", "function"),
  c("vector", "function"),
  c("numeric", "function"),
  c("logical", "function"),
  c("integer", "function"),
  c("character", "function"),
  c("list", "formula"),
  c("vector", "formula"),
  c("logical", "formula"),
  c("numeric", "formula"),
  c("integer", "formula"),
  c("character", "formula")
), function(x){method.skeleton("extract", x, con);TRUE}, logical(1))
close(con)

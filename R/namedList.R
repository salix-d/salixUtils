#' @name  namedList
#' @title Making a named list
#' @param ... the variables to be include in the list (i.e. \code{a, b, c}) or a call resulting in a list of the variable's name(s) (i.e \code{ls(pattern="^[a-c]$")})
#' @param evalCall whether to eval the call made in the ... argument. Default is FALSE.
#' @param sorted whether to sort the resulting list by it's names
#' @return A named list from the name(s) and value(s) of the given variable(s)
#' @examples
#' a <- 1
#' b <- 2
#' c <- 3
#'
#' # Using the variable names
#' namedList(c, a, b)
#'
#' # Using a function to get the variable names
#' namedList(ls(pattern = "^[a-c]$"), sorted = TRUE)
#'
#' # Subsetting of a vector to get the variable names
#' namedList(letters[1:3])

namedList <- function(..., evalCall = FALSE, sorted = FALSE){
  if(!evalCall){
    vars <- as.list(sys.call())[-1]
    if(!any(is.null(names(vars)))) vars <- vars[names(vars)==""]
    nm <- as.character(vars)
    if(length(nm) == 1 && grep("\\W", nm)) evalCall <- TRUE
  }
  if(evalCall){
    vars <- as.list(match.call())[-1]
    if(!any(is.null(names(vars)))) vars <- vars[names(vars) %in% c("", "...")]
    nm <-  eval.parent(as.expression(vars))
  }
  names(nm) <- nm
  if(sorted) nm <- nm[sort(names(nm))]
  val <- sapply(nm, function(x) get(x, inherits = TRUE, pos = 1L), simplify = FALSE)
  return(val)
}

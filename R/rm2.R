#' Remove Objects from a Specified Environment containing a specified pattern
#'
#' @description remove object(s) matching a pattern from an environment. The arguments pos, envir and inherits are passed to [remove][base::remove]. A prompt ask to confirm the removal of variables in case the pattern matches somethings the user wants to keep.
#'
#' @param pattern    a regular expression. Only names matching pattern will be removed.
#' @param pos        where to do the removal. By default, uses the current environment.
#' @param envir      the environment to use.
#' @param inherits   should the enclosing frames of the environment be inspected?
#'
#' @export
#'
#' @examples
#' x <- letters
#' rm2("^x$")
rm2 <- function(pattern, pos = -1, envir = NULL, inherits = FALSE) {
  if (is.null(envir)) envir <- as.environment(pos)
  if (!is.character(pattern))
    stop("`pattern` must be a character string")
  obj2rm <- ls(pattern = pattern, envir = envir)
  msg <- paste("remove :", toStr(obj2rm), "? (y/N)")
  if (identical(readline(msg), "y"))
    remove(list = obj2rm, envir = envir, inherits = inherits)
}

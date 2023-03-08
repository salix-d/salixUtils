#' @export
`%|%` <- function(lhs, rhs) {
  if (!lhs || lhs < 1 || length(lhs) == 0 || is.null(lhs) || is.na(lhs) || is.nan(lhs)) rhs
  else lhs
}
#' @export
`%+<-%` <- `%+=%` <- function(lhs, rhs){
  assign(paste(substitute(lhs)), value = lhs + rhs, envir = parent.frame())
}
`%&%` <- function(lhs, rhs){
  lhs[is.na(lhs) | is.nan(lhs) | is.null(lhs)] <- ""
  paste0(lhs, rhs)
}
#' @export
`%&<%` <- function(lhs, rhs){
  lhs[is.na(lhs) | is.nan(lhs) | is.null(lhs)] <- ""
  paste0(lhs, collapse = rhs)
}
#' @export
`%==%` <- function(lhs, rhs){
  out <- lhs == rhs
  if (!length(out)) out <- rep(FALSE, length(lhs))
  else out[is.na(out)] <- FALSE
  out
}
#' @export
`%<=%` <- function(lhs, rhs){
  out <- lhs <= rhs
  if (!length(out)) out <- rep(FALSE, length(lhs))
  else out[is.na(out)] <- FALSE
  out
}
#' @export
`%>=%` <- function(lhs, rhs){
  out <- lhs >= rhs
  if (!length(out)) out <- rep(FALSE, length(lhs))
  else out[is.na(out)] <- FALSE
  out
}
#' @export
`%>>%` <- `%over%` <- function(lhs, rhs){
  out <- lhs > rhs
  if (!length(out)) out <- rep(FALSE, length(lhs))
  else out[is.na(out)] <- FALSE
  out
}
#' @export
`%<<%` <- `%under%` <- function(lhs, rhs){
  out <- lhs < rhs
  if (!length(out)) out <- rep(FALSE, length(lhs))
  else out[is.na(out)] <- FALSE
  out
}
#' @export
`%+%` <- function(lhs, rhs) paste0(lhs, rhs)

#' @export
`%++%` <- function(lhs, rhs){
  ll <- length(lhs)
  lr <- length(rhs)
  if (ll != lr && length(lr) > length(ll)) rhs <- rep(rhs, each = ll)
  else lhs <- rep(lhs, each = lr)
  paste0(lhs, rhs)
}
#' @export
`%++=%` <- function(lhs, rhs){
  nm <- as.character(substitute(lhs))
  ll <- length(lhs)
  lr <- length(rhs)
  if (ll != lr && length(lr) > length(ll)) rhs <- rep(rhs, each = ll)
  else lhs <- rep(lhs, each = lr)
  assign(nm, paste0(lhs, rhs), envir = parent.frame())
}
# these are preventing the build...
# TODO: figure out why
# #' @export
# methods::setGeneric("%+=%",def = function(lhs, rhs){}, signature = "lhs")
# methods::setMethod("%+=%",
#           signature = "character",
#           definition = function(lhs, rhs){
#             assign(as.character(substitute(lhs)), paste0(lhs, rhs), envir = parent.frame())
#           })
# methods::setMethod("%+=%",
#           signature = "numeric",
#           definition = function(lhs, rhs){
#             assign(as.character(substitute(lhs)), lhs + rhs, envir = parent.frame())
#           })

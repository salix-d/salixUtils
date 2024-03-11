#' @export
#' @family operators
`%ni%` <- Negate('%in%')

#' @export
#' @family operators
`%|%` <- function(lhs, rhs) {
  if (!lhs || lhs < 1 || length(lhs) == 0 || is.null(lhs) || is.na(lhs) || is.nan(lhs)) rhs
  else lhs
}

#' @export
#' @family operators
`%+=%` <- `%+<-%` <- function(lhs, rhs){
  v <- if (is.character(lhs) || is.character(rhs)) {
    paste0(lhs, rhs)
  } else {
    lhs + rhs
  }
  assign(paste(substitute(lhs)), value = v, envir = parent.frame())
  v
}

#' @export
#' @family operators
`%&%` <- function(lhs, rhs){
  lhs[is.na(lhs) | is.nan(lhs)] <- NULL
  paste0(c(lhs, recursive = TRUE))
}

#' @export
#' @family operators
`%&<%` <- function(lhs, rhs){
  lhs[is.na(lhs) | is.nan(lhs)] <- NULL
  paste0(c(lhs, recursive = TRUE), collapse = rhs)
}

#' @export
#' @family operators
`%==%` <- function(lhs, rhs){
  out <- lhs == rhs
  if (!length(out)) out <- rep(FALSE, length(lhs))
  else out[is.na(out)] <- FALSE
  out
}

#' @export
#' @family operators
`%===%` <- function(lhs, rhs){
  out <- paste(lhs) == paste(rhs)
  if (!length(out)) {
    FALSE
  } else {
    out[is.na(out)] <- FALSE
    all(out)
  }
}

#' @export
#' @family operators
`%<=%` <- `%le%` <- function(lhs, rhs){
  out <- lhs <= rhs
  if (!length(out)) out <- rep(FALSE, length(lhs))
  else out[is.na(out)] <- FALSE
  out
}

#' @export
#' @family operators
`%>=%`  <- `%ge%` <- function(lhs, rhs){
  out <- lhs >= rhs
  if (!length(out)) out <- rep(FALSE, length(lhs))
  else out[is.na(out)] <- FALSE
  out
}

#' @export
#' @family operators
`%>>%` <- `%over%` <- `%gt%` <- function(lhs, rhs){
  out <- lhs > rhs
  if (!length(out)) out <- rep(FALSE, length(lhs))
  else out[is.na(out)] <- FALSE
  out
}

#' @export
#' @family operators
`%<<%` <- `%under%` <- `%lt%` <- function(lhs, rhs){
  assert(lhs, c("numeric", "integer", "logical"), TRUE)
  out <- lhs < rhs
  if (!length(out)) out <- rep(FALSE, length(lhs))
  else out[is.na(out)] <- FALSE
  out
}

#' @export
#' @family operators
`%+%` <- function(lhs, rhs) {
  ll <- length(lhs)
  lr <- length(rhs)
  if (ll != lr) {
    if ((ll > lr && (ll %% lr != 0)) || (ll < lr && (lr %% ll != 0))) {
      warning("longer object length is not a multiple of shorter object length",
              "\n  consider `%++%`")
    }
  }
  paste0(lhs, rhs)
}

#' @export
#' @family operators
`%++%` <- function(lhs, rhs){
  ll <- length(lhs)
  lr <- length(rhs)
  if (ll != lr && length(lr) > length(ll)) rhs <- rep(rhs, each = ll)
  else lhs <- rep(lhs, each = lr)
  paste0(lhs, rhs)
}

#' @export
#' @family operators
`%++=%` <- function(lhs, rhs){
  nm <- as.character(substitute(lhs))
  ll <- length(lhs)
  lr <- length(rhs)
  if (ll != lr && length(lr) > length(ll)) rhs <- rep(rhs, each = ll)
  else lhs <- rep(lhs, each = lr)
  assign(nm, paste0(lhs, rhs), envir = parent.frame())
}

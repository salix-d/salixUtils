`%|%` <- function(lhs, rhs) if(!lhs || lhs < 1 || length(lhs)==0 || is.null(lhs)||is.na(lhs) || is.nan(lhs)) rhs else lhs

`%+<-%` <- `%+=%` <- function(lhs, rhs){
  assign(paste(substitute(lhs)), value = lhs + rhs, envir = parent.frame())
}
`%&%` <- function(lhs, rhs){
  lhs[is.na(lhs) | is.nan(lhs) | is.null(lhs)] <- ""
  paste0(lhs, rhs)
}
# c(1:5, NA, 7:10) %&% 1
`%&<%` <- function(lhs, rhs){
  lhs[is.na(lhs) | is.nan(lhs) | is.null(lhs)] <- ""
  paste0(lhs, collapse = rhs)
}
# cat('c("' %&% ("row" %&% 1:5 %&<% '","') %&% '")')
# cat('c("' %&% (LETTERS[1:5] %&% 1 %&<% '","') %&% '")')
`%==%` <- function(lhs, rhs){
  out <- lhs == rhs
  if(!length(out)) out <- rep(FALSE, length(lhs))
  else out[is.na(out)] <- FALSE
  out
}
`%<=%` <- function(lhs, rhs){
  out <- lhs <= rhs
  if(!length(out)) out <- rep(FALSE, length(lhs))
  else out[is.na(out)] <- FALSE
  out
}
`%>=%` <- function(lhs, rhs){
  out <- lhs >= rhs
  if(!length(out)) out <- rep(FALSE, length(lhs))
  else out[is.na(out)] <- FALSE
  out
}
`%>>%` <- `%over%` <- function(lhs, rhs){
  out <- lhs > rhs
  if(!length(out)) out <- rep(FALSE, length(lhs))
  else out[is.na(out)] <- FALSE
  out
}
`%<<%` <- `%under%` <- function(lhs, rhs){
  out <- lhs < rhs
  if(!length(out)) out <- rep(FALSE, length(lhs))
  else out[is.na(out)] <- FALSE
  out
}
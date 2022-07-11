#' @export
len <- function(...) length(...)
#' @export
cNames <- function(...) colnames(...)
#' @export
rNames <- function(...) rownames(...)
#' @export
cSums <- function(...) colSums(...)
#' @export
rSums <- function(...) rowSums(...)

#' @export
setGeneric("%+=%",def = function(lhs, rhs){}, signature = "lhs")
#' @exportMethod
setMethod("%+=%",
          signature = "character",
          definition = function(lhs, rhs){
            assign(as.character(substitute(lhs)), paste0(lhs, rhs), envir = parent.frame())
          })
#' @exportMethod
setMethod("%+=%",
          signature = "numeric",
          definition = function(lhs, rhs){
            assign(as.character(substitute(lhs)), lhs + rhs, envir = parent.frame())
          })

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
  if(ll!=lr && length(lr) > length(ll)) rhs <- rep(rhs, each = ll)
  else lhs <- rep(lhs, each = lr)
  assign(nm, paste0(lhs, rhs), envir = parent.frame())
}

#' shortcut for `as.data.frame()` allowing to name the columns as well
#' @param ... same arguments as for `as.data.frame()`
#' @param colnames character vector for the names of the data.frame (colnames)
#' @example
#' (mat <- matrix(c(
#' "Plantae", "Tracheophyta", "Magnoliopsida", "Sapindales", "Sapindaceae", "Acer", "Acer saccharum",
#' "Plantae", "Tracheophyta", "Magnoliopsida", "Magnoliales", "Magnoliaceae", "NA", "NA",
#' "Plantae", "Tracheophyta", "Pinopsida", "Pinales", "Pinaceae", "Pinus", "NA"
#' ), ncol = 7, byrow = TRUE, dimnames = list(NULL, c("rank1", "rank2", "rank3", "rank4", "rank5", "rank6", "rank7"))))
#'
#' # given this taxonomy matrix
#'      rank1     rank2          rank3           rank4         rank5          rank6   rank7
#' [1,] "Plantae" "Tracheophyta" "Magnoliopsida" "Sapindales"  "Sapindaceae"  "Acer"  "Acer saccharum"
#' [2,] "Plantae" "Tracheophyta" "Magnoliopsida" "Magnoliales" "Magnoliaceae" "NA"    "NA"
#' [3,] "Plantae" "Tracheophyta" "Pinopsida"     "Pinales"     "Pinaceae"     "Pinus" "NA"
#' # we want to convert into a data.frame
#' # and rename rows to their corresponding to ASV[i] and columns to their corresponding rank
#' tax <- df(mat, row.names = paste0("ASV", seq(nrow(mat))), col.names = c("kingdom", "phylum", "class", "order", "family", "genus", "species"))
#' tax
#' @export
as.df <- function(..., col.names = NULL){
  out <- as.data.frame(...)
  if (length(col.names)) colnames(out) <- col.names
  out
}

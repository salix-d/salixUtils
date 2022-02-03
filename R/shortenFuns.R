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
`%++%` <- function(lhs, rhs){
  ll <- length(lhs)
  lr <- length(rhs)
  if(ll!=lr && length(lr) > length(ll)) rhs <- rep(rhs, each = ll)
  else lhs <- rep(lhs, each = lr)
  paste0(lhs, rhs)
}
`%++=%` <- function(lhs, rhs){
  nm <- as.character(substitute(lhs))
  ll <- length(lhs)
  lr <- length(rhs)
  if(ll!=lr && length(lr) > length(ll)) rhs <- rep(rhs, each = ll)
  else lhs <- rep(lhs, each = lr)
  assign(nm, paste0(lhs, rhs), envir = parent.frame())
}
letters[1:2] %+% 1:2
letters[1:2] %++% 1:2
letters[1:2] %+% 1:3
letters[1:2] %++% 1:3
trm <- letters[1:2]
trm %+*=% 1:2
bench::mark(trm %++% LETTERS[1:10] %+% 1:10,
            trm %+++% LETTERS[1:10] %+% 1:10,
            trm %++% LETTERS[1:10] %+% 1:10,
            trm %+++% LETTERS[1:10] %+% 1:10)
df = function(...) data.frame(...)
#' @export
as.df = function(...) as.data.frame(...)
#' shortcut for `cbind.data.frame()`
#' @export
cbind.df = function(...) cbind.data.frame(...)
#' shortcut for `rbind.data.frame()`
#' @export
rbind.df = function(...) rbind.data.frame(...)
#' shortcut for `data.frame()` allowing to name the columns as well
#' ... same arguments as for `data.frame()`
#' names character vector for the names of the data.frame (colnames)
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
#' tax <- df(mat, row.names = paste0("ASV",seq(nrow(mat))), names = c("kingdom", "phylum", "class", "order", "family", "genus", "species"))
#' tax
#' @export
df = function(..., names = NULL){
  out <- data.frame(...)
  if(!missing(names)) names(out) <- names
  out
}

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

# vapply shortcuts
#' @export
v.chr <- function(x, fun, ...) vapply(X = x, FUN = fun, FUN.VALUE = character(1), ...)
#' @export
v.int <- function(x, fun, ...) vapply(X = x, FUN = fun, FUN.VALUE = integer(1), ...)
#' @export
v.dbl <- function(x, fun, ...) vapply(X = x, FUN = fun, FUN.VALUE = numeric(1), ...)
#' @export
v.lgl <- function(x, fun, ...) vapply(X = x, FUN = fun, FUN.VALUE = logical(1), ...)
#' @export
v.lst <- function(x, fun, ...) vapply(X = x, FUN = fun, FUN.VALUE = vector("list", 1), ...)


#' Shortcut for `as.data.frame()` allowing to name the columns as well
#' @param ... same arguments as for `as.data.frame()`
#' @param col.names character vector for the names of the data.frame (colnames)
#' @examples
#' (mat <- matrix(
#'   data = c("Plantae", "Plantae", "Plantae",
#'            "Tracheophyta", "Tracheophyta", "Tracheophyta",
#'            "Magnoliopsida", "Magnoliopsida", "Pinopsida",
#'            "Sapindales", "Magnoliales", "Pinales",
#'            "Sapindaceae", "Magnoliaceae", "Pinaceae",
#'            "Acer", "NA", "Pinus",
#'            "Acer saccharum", "NA", "NA"),
#'   nrow = 3,
#'   dimnames = list(NULL, c("rank1", "rank2", "rank3", "rank4", "rank5", "rank6", "rank7"))))
#'
#' # given this taxonomy matrix
#' #      rank1     rank2          rank3           rank4         rank5          rank6   rank7
#' # [1,] "Plantae" "Tracheophyta" "Magnoliopsida" "Sapindales"  "Sapindaceae"  "Acer"  "Acer saccharum"
#' # [2,] "Plantae" "Tracheophyta" "Magnoliopsida" "Magnoliales" "Magnoliaceae" "NA"    "NA"
#' # [3,] "Plantae" "Tracheophyta" "Pinopsida"     "Pinales"     "Pinaceae"     "Pinus" "NA"
#' # we want to convert into a data.frame
#' # and rename rows to their corresponding to ASV[i] and columns to their corresponding rank
#' tax <- as.df(mat,
#' row.names = paste0("ASV", seq_len(nrow(mat))),
#' col.names = c("kingdom", "phylum", "class", "order", "family", "genus", "species"))
#' tax
#' @export
as.df <- function(..., col.names = NULL){
  out <- as.data.frame(...)
  if (length(col.names)) colnames(out) <- col.names
  out
}

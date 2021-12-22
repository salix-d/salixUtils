#' @export
insertColumn <- function(dat, colIndex, colData = NA, colName = NULL){
  nCol <- ncol(dat) + length(colIndex)
  out <- matrix(ncol = nCol, nrow = nrow(dat), dimnames = list(NULL, seq(nCol)))
  if(length(colIndex)>1){
    colIndex <- vapply(seq(colIndex), function(i) colIndex[[i]] + (i - 1), double(1))
  }
  out[, -colIndex] <- as.matrix(dat)
  if (!is.null(colnames(dat))) colnames(out)[-colIndex] <- colnames(dat)
  rownames(out) <- rownames(dat)
  if(!missing(colData)) out[, colIndex] <- colData
  if(!missing(colName)) colnames(out)[colIndex] <- colName
  return(out)
}

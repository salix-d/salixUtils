insertColumn <- function(dat, colIndex, colData = NA, colName = NULL){
  out <- matrix(ncol = ncol(dat) + length(colIndex), nrow = nrow(dat))
  colIndex <- sapply(seq(colIndex), function(i) colIndex[[i]] + (i - 1))
  out[, -colIndex] <- dat
  colnames(out)[-colIndex] <- colnames(dat)
  rownames(out) <- rownames(dat)
  if(!missing(colData)) out[, is.na(colnames(out))] <- colData
  if(!missing(colName)) colnames(out)[-colRanges] <- colnames(newDat)
  return(out)
}

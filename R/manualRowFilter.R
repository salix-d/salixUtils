manualRowFilter <- function(df, by, cols = FALSE){
  dfList <- df
  rownames(dfList) <- NULL
  if(!isFALSE(cols)) dfList <- dfList[, cols]
  dfList <- split(dfList, by)

  rowFilter <- !logical(length = nrow(df))
  for(li in dfList[unique(by)]){
    print(data.frame(li, row.names = NULL))
    res <- readline("Select row numbers to be REMOVED (empty line to skip):  ")
    if(res == "") next
    if(nchar(res)>1) res <- strsplit(res, "[^0-9]")[[1]]
    res <- as.integer(res)
    if(!all(res %in% seq(nrow(li)))) warning("error reading row numbers")
    row2rm <- rownames(li)[res]
    print(row2rm)
    rowFilter[as.integer(row2rm)] <- FALSE
    print(rowFilter)
  }

  message(sum(!rowFilter), " rows have been filtered out")
  return(df[rowFilter,])
}

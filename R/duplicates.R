#' @export
duplicates <- function(input, na.rm = FALSE){
  dups <- input[duplicated(input)]
  if(na.rm) dups <- dups[!is.na(dups)]
  return(input %in% dups)
}

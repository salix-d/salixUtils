duplicates <- function(input, na.rm = FALSE) UseMethod("duplicates")
duplicates.vector <- function(input, na.rm = FALSE){
  dups <- input[duplicated(input)]
  if(na.rm) dups <- dups[!is.na(dups)]
  return(input %in% dups)
}

duplicates.character <- function(input, na.rm = FALSE){
  duplicates.vector(input = input, na.rm = na.rm)
}


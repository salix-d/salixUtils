#' Use zip from a directory
#'
#' @param dirpath    (character vector of length 1) The directory path
#' @param zipname    (character vector of length 1) The name to give to the zipped file
#' @param pattern    (character vector of length 1) The pattern to select the files in the directory
#' @param ...        Other parameters to pass to \code{list.files}
#'
#' @return    zipname
#' @export
zip_dir <- function(dirpath, zipname, pattern = NULL, ...) {
  assert(dirpath, "character", "dirpath", 1L)
  assert(zipname, "character", "dirpath", 1L)
  if (!is.null(pattern)) assert(pattern, "character", "dirpath", 1L)
  olddir <- getwd()
  setwd(dirpath)
  utils::zip(zipfile = zipname, files = list.files("./", pattern = pattern), ...)
  setwd(olddir)
  zipname
}

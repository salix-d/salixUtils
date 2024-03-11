#' Fill missing values
#'
#' @param x     vector, list, data.frame or data.table
#' @param type  character, one of "const", "locf" or "nocb". Defaults to "const"
#' @param fill  value to be used to fill when type=="const"
#' @param cols  numeric or character vector specifying columns to be updated. description
#'
#' @return x without missing values
#' @export
empty_fill <-  function(x, type = c("const", "locf", "nocb"), fill = NA) {
    type = match.arg(type)

    if (is.list(x)) {
      if (!missing(fill)) {
        x[] <- lapply(x, empty_fill, type, fill)
      } else {
        x[] <- lapply(x, empty_fill, type)
      }
    } else {
      switch(type,
             "const" = {
               for (i in which(!nzchar(x) | !lengths(x) | is.na(x))) {
                 x[i] <- fill
               }
             },
             "locf" = {
               for (i in which(!nzchar(x) | !lengths(x) | is.na(x))) {
                 if (i > 1) {
                  x[i] <- x[i - 1L]
                 } else {
                  x[i] <- fill
                 }
               }
             },
             "nocb" = {
               for (i in rev(which(!nzchar(x) | !lengths(x) | is.na(x)))) {
                 if (i < NROW(x)) {
                   x[i] <- x[i + 1L]
                 } else {
                   x[i] <- fill
                 }
               }
             })
    }
    x
}

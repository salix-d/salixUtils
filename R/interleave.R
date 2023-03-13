#' Interleave two or more vectors
#'
#' @param x1       a vector or list
#' @param x2       a vector or list
#' @param ...      Optional. More vector(s) or list(s)
#' @param pad      logical: Whether to add NA to shorter object(s). Default TRUE.
#' @param padLast  logical: When pad is set to TRUE, whether to add NA at the end (TRUE) or the beginning (FALSE). When pad is set to FALSE, whether to cut at the end (TRUE) or beginning (FALSE) Default TRUE.
#'
#' @export
#'
#' @return same class as input
#' @examples \dontrun{
# interleave(LETTERS, letters)
# interleave(LETTERS, letters, 1:10)
# interleave(LETTERS, letters, 16:26, pad = TRUE, padLast = FALSE)
# interleave(LETTERS, letters, 1:10, pad = FALSE)
# interleave(LETTERS, letters, 16:26, pad = FALSE, padLast = FALSE)
#' }
interleave <- function(x1, x2, ..., pad = TRUE, padLast = TRUE){
  if (!...length()) {
    .interleave(x1 = x1, x2 = x2, pad = pad, padLast = padLast)
  } else {
    .interleave2(x1 = x1, x2 = x2, ..., pad = pad, padLast = padLast)
  }
}
.interleave <- function(x1, x2, pad = TRUE, padLast = TRUE){
  len1 <- length(x1)
  len2 <- length(x2)
  if (len1 == len2) {
    len <- len1
  } else {
    if (!pad) {
      len <- min(len1, len2)
      if (!padLast) {
        if (len > len2) {
          x2 <- x2[(len2 - len + 1):len2]
        } else {
          x1 <- x1[(len1 - len + 1):len1]
        }
      }
    } else {
      len <- max(len, len2)
      if (!padLast) {
        pad <- rep(NA, abs(len - len2))
        if (len > len2) {
          x2 <- c(pad, x2)
        } else {
          x1 <- c(pad, x1)
        }
      }
    }
  }
  c(x1, x2)[order(rep(seq_len(len), 2))]
}
.interleave2 <- function(x1, x2, ..., pad = TRUE, padLast = TRUE){
  n <- 2 + ...length()
  xs <- list(x1, x2, ...)
  xs.len <- lengths(xs, use.names = FALSE)
  if (all(xs.len == xs.len[1]) || (pad && padLast)) {
    c(x1, x2, ...)[order(rep(seq_len(max(xs.len)), n))]
  } else {
    if (pad) {
      len <- max(xs.len)
      xs <- .mapply(
        FUN = \(x, n) c(rep_len(NA, n), x),
        dots = list(x = xs,  n = len - xs.len),
        MoreArgs = NULL
      )
    } else {
      len <- min(xs.len)
      if (padLast) {
        xs <- lapply(xs, `[`, seq_len(len))
      } else {
        xs <- .mapply(
          FUN = \(x, i, j) x[i:j],
          dots = list(x = xs, i = xs.len - len + 1, j = xs.len),
          MoreArgs = NULL
        )
      }
    }
    c(xs, recursive = TRUE)[order(rep(seq_len(len), n))]
  }
}

seq_split <- function(x, by = NULL, lenOut = NULL, ...){
  if (missing(by) == missing(lenOut)) stop("Please, provide one of 'by' and 'lenOut'")
  l <- if (length(x) > 1) length(x) else as.integer(x)
  by <- if (missing(by)) as.integer(ceiling(l/lenOut)) else as.integer(by)
  if (missing(lenOut)) lenOut <- as.integer(ceiling(l/by))

  out <- vector("list", lenOut)
  b <- i <- 1L
  e <- by
  while (i < lenOut) {
    out[[i]] <- b:e
    b <- e + 1L
    e <- e + by
    i <- i + 1L
  }
  out[[lenOut]] <- b:l
  out
}


subsetArr <- function(arr, margin, rng = NULL, begin = NULL, end = NULL){
  if (!is.array(arr)) stop("`a` needsto be an object of class 'array'")
  d <- dim(arr)
  if (is.null(rng)) {
    if (is.null(begin) && is.null(end))
      stop("provide either rng, begin or end arguments")
    else {
      if (is.null(begin)) begin <- 1
      if (is.null(end)) end <- d[margin]
      rng <- paste0(begin, ":", end)
    }
  }
  rngs <- vector("list", length(d))
  for (i in seq_along(d)) {
    r <- if (length(rng) > 1) rng[[i]] else rng
    rngs[[i]] <- if (i %in% margin) r else 1:d[[i]]
  }
  s <- paste0("arr[", paste(rngs, collapse = ","), "]")
  eval(str2lang(s))
}

seq_ranges <- function(x, by) {
  l <- if (length(x) > 1) length(x) else x
  lenOut <- as.integer(ceiling(l/by))
  out <- vector("list", lenOut)
  b <- i <- 1L
  e <- by
  while (i < lenOut) {
    out[[i]] <- b:e
    b <- e + 1L
    e <- e + by
    i <- i + 1L
  }
  out[[lenOut]] <- b:l
  out
}

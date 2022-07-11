# todo: add methods for non-vectors
seqsplit <- function(x, by = NULL, lenOut = NULL, ...){
  if(missing(by) == missing(lenOut)) stop("Please, provide one of 'by' and 'lenOut'")
  l <- length(x)
  if(l==1) l <- as.integer(x)
  if(missing(by)) by <- as.integer(ceiling(l/lenOut))
  else {
    by <- as.integer(by)
    lenOut <- as.integer(ceiling(l/by))
  }
  out <- vector("list", lenOut)
  b <- i <- 1L
  e <- by
  while(i < lenOut){
    out[[i]] <- b:e
    b <- e+1L
    e <- e+by
    i <- i+1L
  }
  out[[lenOut]] <- b:l
  out
}

grp <- function(x, by = NULL, lenOut = NULL){
  l <- length(x)
  if(l==1) l <- as.integer(x)
  if(missing(by)) by <- as.integer(ceiling(l/lenOut))
  else {
    by <- as.integer(by)
    lenOut <- as.integer(ceiling(l/by))
  }
  out <- vector("list", lenOut)
  b <- i <- 1L
  e <- by
  while(i < lenOut){
    out[[i]] <- x[b:e]
    b <- e+1L
    e <- e+by
    i <- i+1L
  }
  out[[lenOut]] <- x[b:l]
  out
}
grp_by <-function(x, f){
  l <- length(x)
  if(l==1) l <- as.integer(x)
  uf <- unique(f)
  lenOut <- length(uf)
  out <- vector("list", lenOut)
  i <- 1L
  while(i <= lenOut){
    out[[i]] <- x[f==uf[i]]
    i <- i + 1L
  }
  out
}
grp_by.data.frame <-function(x, f){
  l <- length(x)
  if(l==1) l <- as.integer(x)
  uf <- unique(f)
  lenOut <- length(uf)
  out <- vector("list", lenOut)
  i <- 1L
  while(i <= lenOut){
    out[[i]] <- x[f==uf[i],]
    i <- i + 1L
  }
  out
}
methods::setGeneric("seqsplit", signature = "x")
methods::setMethod("seqsplit" ,
                   signature(x = "data.frame"),
                   definition = function(x, lenOut, margin = 1, ...){
                     l <- dim(x)[margin]
                     n <- seqsplit(l, lenOut = lenOut)
                     if(margin == 1){
                       unname(split.data.frame(x, grp))
                     } else {
                       split(seq(ncol(x)), grp)
                       sapply(split(seq(ncol(x)), grp), function(i) x[,i], simplify = FALSE)
                     }
                   })
# methods::setMethod("splitBy",
#                    signature(x = "array"),
#                    definition = function(x, by, margin = 1){
#                      l <- dim(x)[margin]
#                      sapply(seq_ranges(l, by), subsetArr, a = x, m = margin, simplify = FALSE)
                   # })
subsetArr <- function(a, m, r = NULL, b = NULL, e = NULL){
  if(is.null(r)){
    if(is.null(b) && is.null(e)) stop("provide either r or b and e arguments")
    else r <- b:e
  } else if(is.character(r)){
    r <- eval(str2lang(r))
  }
  d <- dim(a)
  d[[m]] <- length(r)
  c <- rep(",", length(d))
  c[[m]] <- "r"
  c <- paste(c, collapse = "")
  s <- paste0("a[", c, "]")
  structure(eval(str2lang(s)), dim = d)
}
seq_ranges <- function(x, by){
  l <- length(x)
  if(l==1) l <- x
  is.odd <- l%%by!=0
  out <- vector("list", floor(l/by)+if(is.odd) 1L else 0L)
  b <- 1L
  e <- by
  i <- 1L
  while(e <= l){
    out[[i]] <- b:e
    e <- e+by
    b <- b+by
    i <- i+1L
  }

  if(is.odd) out[[length(out)]] <- b:l
  out
}

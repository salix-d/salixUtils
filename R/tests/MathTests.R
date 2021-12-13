minus <- function(...){
  x <- c(...)
  sum(x[1L], `-`(x[-1L]))
}
minus(10,5,2)
div <- function(...){
  items <- c(...)
  init <- items[1L]
  items <- items[-1L]
  for (x in items){
    init <- init / x
  }
  init
}
div(3,3:2)
div <- function(..., vectorize = FALSE, recycle = FALSE){
  if(vectorize){
    # l <- max(vapply(x, length, integer(1)))
    # x <- vapply(x, "[", numeric(l), 1:l)
    x <- (function(){
      x <- list(...)
      l <- max(vapply(x, length, integer(1)));
      vapply(x, "[", numeric(l), 1:l)
    })()
    # if(recycle){
    #
    # }
    out <- na.omit(x[,1L])
    # if(length(out)>1) out <- vapply(out, div, numeric(1), )
    for(i in seq(ncol(x))[-1L]){
      y <- x[,i]
      out <- out/y[!is.na(y)]
    }
  }
  else{
    out <- prod(..1, 1/c(...)[-1L])
  }
  out
}
div(5,1:5,vectorize = TRUE)
setClassUnion("allVectors", members = c(character(), integer(), numeric(), logical()))

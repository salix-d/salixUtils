div <- function(..., init = NULL, vectorize = c("none", "all", "initOnly", "notInit"), recycle = FALSE){
  vectorize <- match.arg(vectorize, c("none", "all", "initOnly", "notInit"))
  mis <- if(missing(init)) TRUE else FALSE
  if(vectorize == "none"){
    out <- if(mis) ..1 else init
    out <- prod(out, 1/c(...)[-1L])
  } else {
    args <- list(...)
    x <- (function(){
      l <- max(vapply(args, length, integer(1)));
      print(args)
      vapply(args, "[", numeric(l), 1:l)
    })()
    if(mis){
      out <- x[,1L]
      x <- x[,-1L, drop = FALSE] # remember , drop = FALSE !!
      args <- args[-1L]
    } else {
      out <- init
    }
    l <- max(vapply(args, length, integer(1)))
    print("out")
    print(out)
    print(l)
    if(l==1 && length(out) == 1) out <- div(args, init = out)
    else if(length(out) > 1 && l > 1){
      if(vectorize == "initOnly"){
        out <- vapply(out, div, numeric(1), c(args, recursive = TRUE))
      } else {
        out <- vapply(out, div, numeric(l - if(mis) 1 else 0), args, vectorize = "notInit")
      }
    } else {
      if(vectorize == "notInit") return("here")
      for(i in seq(ncol(x))){
        y <- x[,i]
        out <- out/y[!is.na(y)]
      }
    }
  }
  out
}
div(5,1:3, init=1:2, vectorize = "all")

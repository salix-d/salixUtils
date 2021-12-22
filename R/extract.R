#' @export
methods::setGeneric("extract", def = function(x, f, ...) x[vapply(x, f, logical(1))])
methods::setMethod("extract",
    signature(x = "array", f = "function"),
    function (x, f, margin = 1)
    {
        ndim <- length(dim(x))
        if(ndim == 1){
            x[f(x)]
        } else if(ndim==2){
            if(margin == 1){
                x[f(x),]
            } else {
                x[,f(x)]
            }
        } else if(ndim==3){
            if(margin == 1){
                x[f(x),,]
            } else if(margin == 2){
                x[,f(x),]
            } else if(margin == 3){
                x[f(x),,]
            }
        }
    }
)
methods::setMethod("extract",
          signature(x = "matrix", f = "function"),
          function (x, f, margin = 1)
          {
              if(margin == 1){
                  x[f(x),]
              } else {
                  x[,f(x)]
              }
          }
)
methods::setMethod("extract",
          signature(x = "data.frame", f = "function"),
          function (x, f, margin = 1)
          {
              if(margin == 1){
                  x[f(x),]
              } else {
                  x[,f(x)]
              }
          }
)
methods::setMethod("extract",
    signature(x = "list", f = "function"),
    function (x, f)
    {
        x[vapply(x, f, logical(1))]
    }
)
methods::setMethod("extract",
    signature(x = "logical", f = "function"),
    function (x, f)
    {
        x[f(x)]
    }
)
methods::setMethod("extract",
    signature(x = "numeric", f = "function"),
    function (x, f)
    {
        x[f(x)]
    }
)
methods::setMethod("extract",
    signature(x = "integer", f = "function"),
    function (x, f)
    {
        x[f(x)]
    }
)
methods::setMethod("extract",
          signature(x = "complex", f = "function"),
          function (x, f)
          {
              x[f(x)]
          }
)
methods::setMethod("extract",
    signature(x = "character", f = "function"),
    function (x, f)
    {
        x[f(x)]
    }
)
methods::setMethod("extract",
    signature(x = "ANY", f = "formula"),
    function (x, f)
    {
        extract(x = x, f = as.fun(f))
    }
)
methods::setMethod("extract",
    signature(x = "ANY", f = "character"),
    function (x, f)
    {
        extract(x = x, f = as.fun(f))
    }
)
`%~%` <- `%map%` <- function(lhs, rhs){
    f <- as.fun(rhs)
    vapply(lhs, rhs, logical(1))
}
`%~>%` <- function(lhs, rhs){
    lhs[lhs %~% rhs]
}
# 1:5 %~% ~.>2
# as.map('.[1,]>0')(iris[1,])
# iris[,iris[1,] %~% ~!is.na(as.numeric(.))]

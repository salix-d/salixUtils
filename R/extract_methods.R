setGeneric("extract", def = function(x, f, ...) x[vapply(x, f, logical(1))])
setMethod("extract",
    signature(x = "array", f = "function"),
    function (x, f)
    {
        ndim <- length(dim(x))
        stopifnot(margin <= ndim, "Margin higher than number of  dimension.")
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
setMethod("extract",
          signature(x = "matrix", f = "function"),
          function (x, f)
          {
              ndim <- length(dim(x))
              stopifnot(margin <= ndim, "Margin higher than number of  dimension.")
              if(margin == 1){
                  x[f(x),]
              } else {
                  x[,f(x)]
              }
          }
)
setMethod("extract",
          signature(x = "data.frame", f = "function"),
          function (x, f)
          {
              ndim <- length(dim(x))
              stopifnot(margin <= ndim, "Margin higher than number of  dimension.")
              if(margin == 1){
                  x[f(x),]
              } else {
                  x[,f(x)]
              }
          }
)
setMethod("extract",
    signature(x = "list", f = "function"),
    function (x, f)
    {
        x[vapply(x, f, logical(1))]
    }
)
setMethod("extract",
    signature(x = "logical", f = "function"),
    function (x, f)
    {
        x[f(x)]
    }
)
setMethod("extract",
    signature(x = "numeric", f = "function"),
    function (x, f)
    {
        x[f(x)]
    }
)
setMethod("extract",
    signature(x = "integer", f = "function"),
    function (x, f)
    {
        x[f(x)]
    }
)
setMethod("extract",
          signature(x = "complex", f = "function"),
          function (x, f)
          {
              x[f(x)]
          }
)
setMethod("extract",
    signature(x = "character", f = "function"),
    function (x, f)
    {
        x[f(x)]
    }
)
setMethod("extract",
    signature(x = "ANY", f = "formula"),
    function (x, f, .formals)
    {
        extract(x = x, f = as.fun(f, .formals))
    }
)
setMethod("extract",
    signature(x = "ANY", f = "character"),
    function (x, f, .formals)
    {
        extract(x = x, f = as.fun(f, .formals))
    }
)
`%~%` <- `%map%` <- function(lhs, rhs){
    f <- as.fun(rhs)
    vapply(lhs, rhs, logical(1))
}
`%~>%` <- function(lhs, rhs){
    lhs[lhs %~% rhs]
}
1:5 %~% ~.>2
as.map('.[1,]>0')(iris[1,])
iris[,iris[1,] %~% ~!is.na(as.numeric(.))]

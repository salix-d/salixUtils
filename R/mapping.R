setGeneric("%map%",
           def = function(lhs, rhs){
             rhs(lhs)
           }
)
setMethod("%map%",
          signature(lhs = "list", rhs = "formula"),
           function(lhs, rhs){
             vapply(lhs, as.fun(rhs), logical(1))
           }
)
setMethod("%map%",
          signature(lhs = "list", rhs = "character"),
           function(lhs, rhs){
             vapply(lhs, as.fun(rhs), logical(1))
           }
)
setMethod("%map%",
          signature(lhs = "list", rhs = "function"),
          function(lhs, rhs){
            vapply(lhs, rhs, logical(1))
          }
)
setMethod("%map%",
          signature(lhs = "data.frame", rhs = "function"),
          function(lhs, rhs){
            rhs(lhs)
          }
)
setMethod("%map%",
          signature(lhs = "data.frame", rhs = "formula"),
          function(lhs, rhs){
            as.fun(rhs)(lhs)
          }
)
setMethod("%map%",
          signature(lhs = "data.frame", rhs = "character"),
          function(lhs, rhs){
            as.fun(rhs)(lhs)
          }
)
setGeneric("%~%",
           def = function(lhs, rhs){
             f <- as.fun(rhs)
             f(lhs)
             # vapply(lhs, f, logical(1))
           }
)
setMethod("%~%",
          signature(lhs = "list"),
          function(lhs, rhs){
            f <- as.fun(rhs)
            vapply(lhs, f, logical(1))
          }
)
setMethod("%~%",
          signature(lhs = "data.frame"),
          function(lhs, rhs){
            f <- as.fun(rhs)
            f(. = lhs)
          }
)

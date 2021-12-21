setGeneric("%map%",
           def = function(lhs, rhs){
             rhs(lhs)
           }
)
setMethod("%map%",
          signature(rhs = "formula"),
           function(lhs, rhs){
             lhs %map% as.fun(rhs)
           }
)
setMethod("%map%",
          signature(rhs = "character"),
           function(lhs, rhs){
             lhs %map% as.fun(rhs)
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
setGeneric("%~%",
           def = function(lhs, rhs){
             f <- as.fun(rhs)
             f(lhs)
           }
)
setMethod("%~%",
          signature(lhs = "list"),
          function(lhs, rhs){
            f <- as.fun(rhs)
            sapply(lhs, f)
          }
)
setMethod("%~%",
          signature(lhs = "data.frame"),
          function(lhs, rhs){
            f <- as.fun(rhs)
            f(. = lhs)
          }
)

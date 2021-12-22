methods::setGeneric("%map%",
           def = function(lhs, rhs){
             rhs(lhs)
           }
)
methods::setMethod("%map%",
          signature(rhs = "formula"),
           function(lhs, rhs){
             lhs %map% as.fun(rhs)
           }
)
methods::setMethod("%map%",
          signature(rhs = "character"),
           function(lhs, rhs){
             lhs %map% as.fun(rhs)
           }
)
methods::setMethod("%map%",
          signature(lhs = "list", rhs = "function"),
          function(lhs, rhs){
            vapply(lhs, rhs, logical(1))
          }
)
methods::setMethod("%map%",
          signature(lhs = "data.frame", rhs = "function"),
          function(lhs, rhs){
            rhs(lhs)
          }
)
methods::setGeneric("%~%",
           def = function(lhs, rhs){
             f <- as.fun(rhs)
             f(lhs)
           }
)
methods::setMethod("%~%",
          signature(lhs = "list"),
          function(lhs, rhs){
            f <- as.fun(rhs)
            sapply(lhs, f)
          }
)
methods::setMethod("%~%",
          signature(lhs = "data.frame"),
          function(lhs, rhs){
            f <- as.fun(rhs)
            f(. = lhs)
          }
)

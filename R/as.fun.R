#' @name as.fun
#' @title Convert Formula to Function
#' @param    .body either a right sided formula (i.e ~ x==1) or a string describing the wanted function body
#' @return   a function
#' @examples
#' data("iris")
#'
#' as.fun(~ .[.$Species %in%  c("setosa","virginica") & .$Petal.Width>0.2,])(iris)
#' @export
methods::setGeneric("as.fun",
           def = function(.body) {
             as.function(alist(...=, . = ..1, .x = ..1, .y = ..2, eval(str2lang(.body))))
           }
)
methods::setMethod("as.fun",
          signature(.body = "formula"),
          function (.body)
          {
            if (length(.body) == 3) stop("Only right sided formula allowed.")
            as.function(alist(...=, . = ..1, .x = ..1, .y = ..2, eval(.body[[2]])))
          }
)
methods::setGeneric("%f%",
           def = function(lhs, rhs) {
             as.function(alist(...=, . = ..1, .x = ..1, .y = ..2, eval(str2lang(rhs))))(lhs)
           }
)
methods::setMethod("%f%",
          signature(lhs = "list"),
          function(lhs, rhs)
          {
            lapply(lhs, as.fun(rhs))
          }
)
methods::setMethod("%f%",
          signature(rhs = "formula"),
          function(lhs, rhs)
          {
            if (length(rhs) == 3) stop("Only right sided formula allowed.")
            as.function(alist(...=, . = ..1, .x = ..1, .y = ..2, eval(rhs[[2]])))(lhs)
          }
)

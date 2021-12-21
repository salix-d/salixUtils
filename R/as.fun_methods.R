#' @name as.fun
#' @title Convert Formula to Function
#' @param    f a right sided formula (i.e ~ x==1)
#' @param    .formals a character vector of names OR a named (a)list for the formal arguments to use for the function created from the formula. Default uses . and .x to refer to the first argument passed to the function and .y for the second.
#' @return   a function which evaluates the given formula
#' @example
#' data("iris")
#'
#' as.fun(~ .[.$Species %in%  c("setosa","virginica") & .$Petal.Width>0.2,])(iris)
setGeneric("as.fun",
           def = function(.body) {
             f <- function(..., .x = ..1, .y = ..2, . = ..1){}
             f <- `body<-`(f, value = str2lang(.body))
             f
           }
)

setMethod("as.fun",
          signature(.body = "character"),
          function (.body)
          {
            if (length(.body) == 3) stop("Only right sided formula allowed.")
            f <- function(..., .x = ..1, .y = ..2, . = ..1){}
            f <- `body<-`(f, value = str2lang(.body))
            f
          }
)
setMethod("as.fun",
          signature(.body = "formula"),
          function (.body)
          {
            if (length(.body) == 3) stop("Only right sided formula allowed.")
            f <- function(..., .x = ..1, .y = ..2, . = ..1){}
            f <- `body<-`(f, value = .body[[2]])
            f
          }
)
# '%f%' <- function(.formals, .body) {
#   f <- function(){}
#   f <- `formals<-`(f, value = .formals)
#   f <- `body<-`(f, value = .body[[2]])
#   f
# }
# (function(.){.[.$Species %in%  c("setosa","virginica") & .$Petal.Width>0.2,]})(iris)
# setMethod("as.fun",
#           signature(.body = "formula"),
#           function (.body, .formals = alist(... =, .x = ..1, .y = ..2, . = ..1))
#           {
#             if(is.null(names(.formals))) .formals <- `names<-`(vector("list",length(.formals)), .formals)
#             if (length(.body) == 3) stop("Only right sided formula allowed.")
#             nf <- function(){}
#             nf <- `body<-`(nf, value = .body[[2]])
#             nf <- `formals<-`(nf, value = .formals)
             # nf <- if(vectorize) function(..., .x = ..1, .y = ..2, . = ..1){
             #   if(!missing(.y)) vapply(.x, f, logical(1),.y)
             #   else vapply(.x, f, logical(1))
             # } else f
#             nf
#           }
# )


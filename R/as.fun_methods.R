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
             f <- function(..., .x = ..1, .y = ..2, . = ..1){}
             f <- `body<-`(f, value = str2lang(.body))
             f
           }
)

methods::setMethod("as.fun",
          signature(.body = "character"),
          function (.body)
          {
            if (length(.body) == 3) stop("Only right sided formula allowed.")
            f <- function(..., .x = ..1, .y = ..2, . = ..1){}
            f <- `body<-`(f, value = str2lang(.body))
            f
          }
)
methods::setMethod("as.fun",
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


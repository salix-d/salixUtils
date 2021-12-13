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


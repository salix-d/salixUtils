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
            # f <- `body<-`(f, value = str2lang(.body))
            f <- `body<-`(f, value = .body)
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
bench::mark(
  purrr::as_mapper(~.x>3)(1:5),
  as.fun(~.x>3)(1:5),
  1:5 %f% ~.>3,
  as.fun(".x>3")(1:5),
  1:5 %f% ".>3",
  iterations = 1000)[,c("expression","median", "itr/sec")]

'%f%' <- function(lhs, rhs) {
  as.fun(rhs)(lhs)
}
1:5 %f% ~.>3

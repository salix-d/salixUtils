#' @name as.fun
#' @title Convert Formula to Function
#' @param    f a right sided formula (i.e ~ x==1)
#' @param    .formals a character vector of names OR a named (a)list for the formal arguments to use for the function created from the formula. Default uses . and .x to refer to the first argument passed to the function and .y for the second.
#' @return   a function which evaluates the given formula
#' @example
#' data("iris")
#'
#' iris <- as.fun(~ .$Species ==  "setosa" & .$Petal.Width>0.2)(iris)
iris[iris$Species  == "setosa" & iris$Petal.Width>0.2,]
iris3[iris3[,"Sepal W.",1]>3 & iris3[,"Sepal W.",2]>3 & iris3[,"Sepal W.",3]>3,,]
as.fun(~.[.x[,.y,1]>3&.x[,.y,2]>3&.x[,.y,3]>3,,], vectorize = TRUE)(iris3, "Sepal W.")
# list(iris3, "Sepal W.") %~% ".x[.x[,.y,1]>3&.x[,.y,2]>3&.x[,.y,3]>3,,]"
as.fun <- function(f, .formals = alist(... =, .x = ..1, .y = ..2, . = ..1), vectorize = FALSE) {
  if(is.null(names(.formals))) .formals <- `names<-`(vector("list",length(.formals)), .formals)
  if (length(f) == 3) stop("Only right sided formula allowed.")

  nf <- function(){}
  nf <- `body<-`(nf, value = f[[2]])
  nf <- `formals<-`(nf, value = .formals)
  nf <- `class<-`(nf, c("rlang_lambda_function", "function"))
  nf
}
as.fun.character <- function(f, .formals = alist(... =, .x = ..1, .y = ..2, . = ..1)) {
  if(is.null(names(.formals))) .formals <- `names<-`(vector("list",length(.formals)), .formals)
  nf <- function(){}
  nf <- `body<-`(nf, value = str2lang(f))
  nf <- `formals<-`(nf, value = .formals)
  nf
}
bench::mark(
  as.fun(~x>3, .formals=c("x"))(1:5),
  purrr::as_mapper(~.x>3)(1:5),
  iterations = 100)

`%><%` <- function(x, y){
  if(length(y) != 2) warning("right-hand-side isn't length 2 but it MUST be in the form c(lower_bound, upper_bound)")
  x > y[1] & x < y[2]
}


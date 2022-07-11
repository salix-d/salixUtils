#' @export
class2taxo <- function(obj,
                       ranks = c("kingdom", "phylum", "class", "order", "family", "genus", "species"),
                       USE.NAMES = T){
  l <- length(ranks)
  out <- vapply(obj, function(x){
    r <- match(ranks, x$rank, 0L)
    out <- array(NA_character_, dim = l, dimnames = list(ranks))
    out[x$rank[r]] <- x$name[r]
    out
  }, character(l), USE.NAMES = USE.NAMES)
  if(l==1) as.data.frame(out)
  else {
    as.data.frame(t(out))
  }
}

as.classification <- function(obj){
  if(!is(obj, "classification")){
    check <- function(o) length(d <- dim(o)) && all(d>0) && all(c("rank", "name") %in% colnames(o))
    if("list" %in% class(obj)){
      test <- vapply(obj, function(x){
        if(check(x)) if(!"id" %in% colnames(x)) o[,"id"] <- NA
        o[,c("rank", "name", "id")]
      }, NA)
      test <- any(test)
    } else {
      test <- check(obj)
      if(test){
        if(!"id" %in% colnames(obj)) obj[,"id"] <- NA
        obj <- `names<-`(list(obj[,c("rank", "name", "id")]), obj$name[[length(obj$name)]])

      }
    }
    if(!test) stop("Wrong input class. Object's class must be 'classification' (from taxize) or built similarly")
    class(obj) <- "classification"
  }
  obj
}
is.classification <- function(obj){
  check <- function(o) length(d <- dim(o)) && all(d>0) && all(c("rank", "name") %in% colnames(o))
  test <- is(obj, "classification")
  if(!test){
    if("list" %in% class(obj)){
      test <- any(vapply(obj, check, NA))
    } else {
      test <- check(obj)
    }
  }
  test
}
